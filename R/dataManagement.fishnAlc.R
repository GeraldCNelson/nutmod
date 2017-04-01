# Intro -----------------------
#This script reads in fish data from FAO's Food Balance Sheet data
# and parameters for IMPACT and generates
#scenarios of per capita consumption.

#Copyright (C) 2015 - 2017 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details at http://www.gnu.org/licenses/.
# options(warn=2)
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

# get the years over which the FBS data should be averaged to create starting values
FBSyearsToAverage <- keyVariable("FBSyearsToAverage")

# get the list of years we need to keep
keepYearList <- keyVariable("keepYearList")

#need to include the n-1 year for the elasticity calculations
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X",year0, sep = "")
keepYearList <- c(year0,keepYearList)
IMPACTfish <- fileNameList("IMPACTfish")
IMPACTfish_code <- keyVariable("IMPACTfish_code")
IMPACTalcohol_code <- keyVariable("IMPACTalcohol_code")
scenarioListSSP.pop <- keyVariable("scenarioListSSP.pop")
scenarioListSSP.GDP <- keyVariable("scenarioListSSP.GDP")

#load the SSP GDP data -----
dt.SSPGDP <- getNewestVersion("dt.SSPGDPClean")

# load the SSP population data
dt.SSPPop <- getNewestVersion("dt.SSP.pop.tot")

# get rid of the stuff after SSPx
dt.SSPPop[,scenario := substring(scenario, 1,4)]
dt.SSPGDP[,scenario := substring(scenario, 1,4)]
scenarioListSSP.GDP <- substring(scenarioListSSP.GDP, 1,4)

dt.SSPGDPperCap <- merge(dt.SSPGDP, dt.SSPPop, by.x = c( "scenario", "ISO_code", "year"),
                         by.y = c( "scenario", "region_code.SSP", "year"))
dt.SSPGDPperCap[, value := (value/pop) * 1000] # * 1000 because pop is in millions and value is in billions
deleteListCol <- c( "variable", "unit", "pop")
dt.SSPGDPperCap[, (deleteListCol) := NULL]

# load regions info ----
dt.regions.all <- getNewestVersion("dt.regions.all")

# create list with only countries in both FBS and SSP
# Note SOM is not in FBS
inFBS <- sort(dt.regions.all[!is.na(FAOSTAT_code), ISO_code])
inFBS <- inFBS[!inFBS %in% "SOM"]
# the pop data has a different set of countries than the GDP data. Only keep countries that are in both
inSSP.pop <- sort(dt.regions.all[!is.na(region_code.SSP), ISO_code])
inSSP.GDP <- sort(unique(dt.SSPGDP$ISO_code))
setdiff(inSSP.pop,inSSP.GDP)
inSSP <- sort(intersect(inSSP.pop,inSSP.GDP))
ctyList <- sort(intersect(inFBS,inSSP))
dt.SSPGDPperCap <- dt.SSPGDPperCap[ISO_code %in% ctyList, ]

data.table::setkeyv(dt.SSPGDPperCap, c("scenario", "ISO_code"))
data.table::setorder(dt.SSPGDPperCap, scenario, ISO_code, year)

# lag and difference SSP GDP -----
dt.SSPGDPperCap[,GDP.lag1 := data.table::shift(value,type = "lag"), by = c("ISO_code", "scenario")]
dt.SSPGDPperCap[,delta.GDP := value - GDP.lag1]

# prepare the FBS data -----
dt.FBS <- getNewestVersion("dt.FBS")
# keep only FBS data for countries in ctyList
dt.FBS <- dt.FBS[ISO_code %in% ctyList, ]
# data.table::setnames(dt.FBS, old = "ISO_code", new = "region_code.SSP")# change code for countries from ISO to SSP

#keep only the years to average
dt.FBS <- dt.FBS[year %in% FBSyearsToAverage, ]
# Get the middle of the total number of FBS yearsToAverage. middleYear is what we'll average on
middleYear <-
  FBSyearsToAverage[as.integer(length(FBSyearsToAverage) / 2) + 1]

# choices of FBS data are "foodMT", "perCapKg","perCapKcal","perCapProt"
data.table::setkey(dt.FBS, variable)
dt.FBS.kgPerCap <- dt.FBS["perCapKg"] # choose perCapKg

#to reduce the clutter in the FBS DT
keepListCol <- c("ISO_code", "IMPACT_code", "year", "value")
dt.FBS.kgPerCap <- dt.FBS.kgPerCap[, keepListCol, with = FALSE]

#now average
baseYear <- paste("aveAt", middleYear, sep = "")
dt.FBS.kgPerCap[, (baseYear) := mean(value), by = list(ISO_code, IMPACT_code)]

dt.FBS.kgPerCap <- dt.FBS.kgPerCap[year == middleYear, ]
deleteListCol <- c("value")
dt.FBS.kgPerCap[, (deleteListCol) := NULL]
dt.FBS.kgPerCap[is.na(get(baseYear)), (baseYear) := 0]
data.table::setorder(dt.FBS.kgPerCap, ISO_code, IMPACT_code)

# working on fish elasticities -----
# read in fish data from IMPACT  ----
# # fish supply in 1000 metric tons
# #' @param - fishS - supply of fish
# fishS <- openxlsx::read.xlsx(
#   IMPACTfish,
#   sheet = "QS_FishSys",
#   cols = 1:6, startRow = 3, colNames = FALSE
# )
# colnames(fishS) <-
#   c("fish_type", "region", "freshAquac", "marinAquac", "freshCapt", "marine_capt")

#' @param - fishLookup - fish look up. production and consumption names and IMPACT names
fishLookup <- openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "IMPACT Commodities",
  cols = 6:7, startRow = 2, colNames = TRUE
)

#' @param - fishD - fish demand in 1000 metric tons
fishD <- openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "DemandStkChg",
  cols = 1:11, startRow = 3, colNames = FALSE
)
colnames(fishD) <-
  c(
    "IMPACT_code", "region", "net_trade", "exports", "imports", "tot_demand", "food_demand", "feed_demand",
    "other_demand", "stock_change","crush_demand"
  )

fishD[is.na(fishD)] <- 0
fishD <- fishD[order(fishD$region), ]

# create income elasticities data for the countries common to FBS and SSP ----
#' @param - fishIncElast - fish income elasticity
dt.fishIncElast <- data.table::as.data.table(openxlsx::read.xlsx(
  IMPACTfish, sheet = "IncDmdElas", cols = 1:11, startRow = 1, colNames = TRUE
))

#Column names can't have a "-" in them. This code changes them to underscore
data.table::setnames(dt.fishIncElast, old = colnames(dt.fishIncElast), new = gsub("-", "_", colnames(dt.fishIncElast)))
# in code below, start with item 2 because item 1 is region
data.table::setnames(dt.fishIncElast, old = colnames(dt.fishIncElast)[2:length(dt.fishIncElast)],
                     new = paste(colnames(dt.fishIncElast)[2:length(dt.fishIncElast)], "elas", sep = "."))
data.table::setnames(dt.fishIncElast, old = "region", new = "region_code.IMPACT115")
# add elasticity of zero for aquatic plants and animals (c_aqpl and c_aqan)
dt.fishIncElast[, c("c_aqan.elas", "c_aqpl.elas") := 0]
#if fixFish is TRUE, deal with the missing species - shrimp, tuna, and salmon
# rename shrimp elasticities to crustacean elasticities
# remove salmon and tuna elasticities
#if fixShrimp is TRUE, deal with the missing species - shrimp, tuna, and salmon
# rename shrimp elasticities to crustacean elasticities
# remove salmon and tuna elasticities
switch.fixFish <- keyVariable("switch.fixFish")
if (switch.fixFish == TRUE) {
  dt.fishIncElast[, c_Crust.elas := NULL]
  data.table::setnames(dt.fishIncElast, old = "c_shrimp.elas", new = "c_Crust.elas")
  itemsToRemove <- c("c_Shrimp", "c_Tuna", "c_Salmon")
  IMPACTfish_code <-
    IMPACTfish_code[!(IMPACTfish_code %in% itemsToRemove)]
  itemsToRemove <- c("c_Tuna.elas", "c_Salmon.elas") # because shrimp already removed above
  dt.fishIncElast[, (itemsToRemove) := NULL]
}
fish_code.elast.list <-
  names(dt.fishIncElast)[2:length(dt.fishIncElast)]

# set max income elasticity to 1 if TRUE
switch.changeElasticity <- keyVariable("switch.changeElasticity")
if (switch.changeElasticity == TRUE) {
  temp <- names(dt.fishIncElast)[2:length(names(dt.fishIncElast))]
  for (j in temp)
    data.table::set(
      dt.fishIncElast,
      i = which(dt.fishIncElast[[j]] > 1L),
      j = j,
      value = 1L
    )
}

data.table::setkey(dt.fishIncElast, region_code.IMPACT115)
data.table::setkey(dt.regions.all, region_code.IMPACT115)
dt.fishIncElast.SSP <- dt.fishIncElast[dt.regions.all]
#dt.fishIncElast.SSP <- merge(dt.fishIncElast ,dt.regions.all, by = "region_code.IMPACT115")
keepListCol <- c( "ISO_code","region_code.IMPACT159", fish_code.elast.list)
dt.fishIncElast.SSP <- dt.fishIncElast.SSP[, keepListCol, with = FALSE]
#dt.fishIncElast.SSP <- dt.fishIncElast.SSP[!is.na(region_code.SSP),]

# create a fish elasticities data table with the same income elasticities in all years
dt.years <- data.table::data.table(year = rep(keepYearList, each = nrow(dt.fishIncElast.SSP)))

#' @param - dt.fishIncElast.SSP - fish elasticities for each region in the SSP data and all years
dt.fishIncElast.SSP <- cbind(dt.years, dt.fishIncElast.SSP)
idVars <- c("ISO_code", "region_code.IMPACT159", "year")
dt.fishIncElast <- data.table::melt(
  dt.fishIncElast.SSP,
  id.vars = idVars,
  variable.name = "variable",
  measure.vars = fish_code.elast.list,
  variable.factor = FALSE
)
# dt.fishIncElast <- dt.fishIncElast[!is.na(region_code.SSP), ]
inDT <- dt.fishIncElast
outName <- "dt.fishIncElast"
cleanup(inDT,outName,fileloc("iData"))

# arc elasticity elasInc = [(Qn - Qn-1)/(Qn + Qn-1)/2] / [(Yn - Yn-1) /(Yn + Yn-1)/2)]
# [(Qn - Qn-1)/(Qn + Qn-1)] = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# set x = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# x * (Qn + Qn-1) = (Qn - Qn-1)
# x * Qn + x* Qn-1 + Qn-1 = Qn
# x* Qn - Qn + x* Qn-1 + Qn-1 = 0
# Qn * (x - 1) + Qn-1 * (x + 1) = 0
# Qn * (x - 1) = - Qn-1 * (x + 1)
# Qn = - Qn-1 (1 + x)/(x - 1) = Qn-1 (1+x)/(1-x)

# f.Qn <- function(elasInc, GDPn, GDPn_1, delta.GDP, Qn_1) {
#   x <- elasInc * delta.GDP / (GDPn + GDPn_1)
#   Qn <- (x + 1) * Qn_1 / (1 - x)
#   return(Qn)
# }

# loop over scenarios and countries  -----
# set up a data table to hold the results of the calculations
dt.final <-
  data.table::data.table(scenario = character(0),
                         ISO_code = character(0),
                         year = character(0))
dt.final[, c(IMPACTfish_code) := 0]

# ctyList <- c("AFG", "AGO", "ALB", "ARE", "ARG", "ARM", "AUS", "AUT") # for testing
# scenarioListSSP.GDP <- c("SSP1_v9_130325","SSP2_v9_130325") # for testing
for (scenarioChoice in scenarioListSSP.GDP) {
  for (ctyChoice in ctyList) {
    #print(paste(scenarioChoice,ctyChoice))
    # create a data table with FBS fish perCapKg values for one country
    keylist <- c("ISO_code", "IMPACT_code")
    data.table::setkeyv(dt.FBS.kgPerCap, keylist)
    #' @param dt.FBS.kgPerCap - FBS kgPerCap numbers for years in the keepYearsList
    # get data rows for each fish commodity for country ctyChoice. The row is created if it
    # doesn't exist i dt.FBS.kgPerCap
    dt.FBS.subset <- dt.FBS.kgPerCap[J(ctyChoice, IMPACTfish_code)]

    # some countries don't have fish values for the base year. Next two lines of code deal with this.
    dt.FBS.subset[, year := (middleYear)]
    dt.FBS.subset[is.na(get(baseYear)), (baseYear) := 0]
    data.table::setkeyv(dt.FBS.subset, c("IMPACT_code", baseYear))

    # subset on country and the current scenario
    data.table::setkeyv(dt.SSPGDPperCap, c("scenario", "ISO_code"))
    dt.GDP <-  dt.SSPGDPperCap[scenario %in% scenarioChoice  &  ISO_code %in% ctyChoice,]

    # add columns for the fish commodities
    dt.GDP[,(IMPACTfish_code) := 0]

    dt.temp.fish.elas <- dt.fishIncElast.SSP[ISO_code %in% ctyChoice,]
    dt.GDP <- merge(dt.GDP,dt.temp.fish.elas, by = c("ISO_code", "year"))
    dt.GDP[, GDPRatio := delta.GDP/(value + GDP.lag1)] # to clarify the process
    for (fish in IMPACTfish_code) {
      elas <- paste(fish,"elas", sep = ".")
      dt.GDP[,temp.inc := get(elas) * GDPRatio]
      dt.GDP[,temp.fish := 0]
      dt.GDP[,temp.fish2 := 0]
      # get base year per capita consumption from the FBS data set
      fish.2005 <- dt.FBS.subset[IMPACT_code == fish, get(baseYear)]
      dt.GDP[year %in% "X2005", temp.fish := fish.2005]

      #    for (i in 2:nrow(dt.GDP)) {
      #      dt.GDP$temp.fish[i] <- dt.GDP$temp.fish[i - 1] * (1 + dt.GDP$temp.inc[i])  / (1 - dt.GDP$temp.inc[i])
      #    }

      #faster version of commented out loop above
      dt.GDP[-1,temp.fish := dt.GDP$temp.fish[1] * cumprod((1 + temp.inc) / (1 - temp.inc))]
         dt.GDP[,(fish) := temp.fish]
      }

      # alternative to the for loop is something like the following
      # DT[, res := Price[1] * c(1, cumprod(1 + YHOO.Close[-1]))]
      #   or
      #  DT[-1, cons := DT$cons[1]*cumprod((1+temp.inc)/(1-temp.inc))]

      # the R code is explained at http://stackoverflow.com/questions/37802687/r-data-table-divide-list-of-columns-by-a-second-list-of-columns
      #dt.food_agg[, (nutListReqRatio) := Map(`/`, mget(nutListSum), mget(nutListReq))]

      # Qn = - Qn-1 (1 + x)/(x - 1) = Qn-1 (1+x)/(1-x)

      keepListCol <- c("scenario", "ISO_code", "year", IMPACTfish_code)
      dt.temp <- dt.GDP[,keepListCol, with = FALSE]
      dt.final <- rbind(dt.final, dt.temp)
    }
  }

  #keep only years in keepYearList
  dt.final <- dt.final[year %in% keyVariable("keepYearList"),]

  inDT <- dt.final
  outName <- "dt.fishScenarios"
  cleanup(inDT,outName,fileloc("mData"))

  # alcohol calculations -------

  # Salisu, M.A., and V.N. Balasubramanyam. 1997. “Income and Price Elasticities of Demand for Alcoholic Drinks.”
  # Applied Economics Letters 4 (4): 247–51. doi:10.1080/758518504.
  # has spirits income elasticites of .88 and 1.06 depending on estimation form, wine of 1.42 and 1.55, beer of .7 and .76.
  # data from from UK for 1863 - 1993.
  dt.elas.wide <- data.table::data.table(ISO_code = rep(dt.regions.all$region_code.SSP, each = length(keepYearList)),
                                         year = keepYearList,
                                         c_beer.elas = 0.50,
                                         c_wine.elas = 1.00,
                                         c_spirits.elas = 1.00)

  dt.elas.wide <- dt.elas.wide[ISO_code %in% ctyList]

  idVars <- c("ISO_code", "year")
  alc_code.elast.list <-
    names(dt.elas.wide)[3:length(dt.elas.wide)]
  dt.alcIncElast <- data.table::melt(
    dt.elas.wide,
    id.vars = idVars,
    variable.name = "variable",
    measure.vars = alc_code.elast.list,
    variable.factor = FALSE
  )
  inDT <- dt.elas.wide
  outName <- "dt.alcIncElast"
  cleanup(inDT,outName, fileloc("iData"))

  # loop over scenarios and countries common to FBS and SSP  -----

  # set up dt to hold the alcohol results

  dt.final <-
    data.table::data.table(scenario = character(0),
                           ISO_code = character(0),
                           year = character(0))
  dt.final[, c(IMPACTalcohol_code) := 0]

  for (scenarioChoice in scenarioListSSP.GDP) {
    for (ctyChoice in ctyList) {
      # print(paste(scenarioChoice,ctyChoice))
      # create a data table with FBS alc perCapKg values for one country
      keylist <- c("ISO_code", "IMPACT_code")
      data.table::setkeyv(dt.FBS.kgPerCap, keylist)
      #' @param dt.FBS.kgPerCap - FBS kgPerCap numbers for years in the keepYearsList
      dt.FBS.subset <- dt.FBS.kgPerCap[J(ctyChoice, IMPACTalcohol_code)]
      # some countries don't have values for the base year. Next two lines of code deal with this.
      dt.FBS.subset[, year := (middleYear)]
      #  dt.FBS.subset[is.na(get(baseYear)), (baseYear) := 0, with = FALSE]
      dt.FBS.subset[is.na(get(baseYear)), (baseYear) := 0]
      data.table::setkeyv(dt.FBS.subset, c("IMPACT_code", baseYear))

      # subset on current scenario and country
      data.table::setkeyv(dt.SSPGDPperCap, c("scenario", "ISO_code"))
      dt.GDP <-  dt.SSPGDPperCap[scenario %in% scenarioChoice  &  ISO_code %in% ctyChoice,]
      dt.GDP[,(IMPACTalcohol_code) := 0]

      dt.temp.alc.elas <- dt.elas.wide[ISO_code %in% ctyChoice,]
      dt.GDP <- merge(dt.GDP,dt.temp.alc.elas, by = c("ISO_code","year"))

      # for (alc in IMPACTalcohol_code) {
      #   elas <- paste(alc,"elas", sep = ".")
      #   dt.GDP[,temp := get(elas) * delta.GDP/(value + GDP.lag1)]
      #   dt.GDP[year == "X2005",(alc) := dt.FBS.subset[IMPACT_code == alc,get(baseYear)]]
      #   dt.GDP[, (alc) := get(alc)[1L]]
      #   dt.GDP[-1L, (alc) := get(alc) * (1 + temp)  / (1 - temp)]
      #   # to deal with 0 in year0; hopefully a rare occurance
      #   # this code from http://stackoverflow.com/questions/20535505/replacing-all-missing-values-in-r-data-table-with-a-value
      #   for (i in seq_along(dt.GDP)) data.table::set(dt.GDP, i = which(is.nan(dt.GDP[[i]])), j = i, value = 0)
      # }

      for (alc in IMPACTalcohol_code) {
        elas <- paste(alc, "elas", sep = ".")
        dt.GDP[,temp.inc := eval(parse(text = elas)) * delta.GDP/(value + GDP.lag1)]
        dt.GDP[,temp.alc := 0]

        # get base year per capita consumption from the FBS data set
        dt.GDP[year == "X2005",temp.alc := dt.FBS.subset[IMPACT_code == alc, get(baseYear)]]

        #   for (i in 2:nrow(dt.GDP)) {
        #     dt.GDP$temp.alc[i] <- dt.GDP$temp.alc[i - 1] * (1 + dt.GDP$temp.inc[i])  / (1 - dt.GDP$temp.inc[i])
        #   }
        #faster version of commented out loop above
        dt.GDP[-1,temp.alc := dt.GDP$temp.alc[1] * cumprod((1 + temp.inc) / (1 - temp.inc))]
          dt.GDP[,(alc) := temp.alc]
        }

        keepListCol <- c("scenario", "ISO_code", "year", IMPACTalcohol_code)
        dt.temp <- dt.GDP[,keepListCol, with = FALSE]
        dt.final <- rbind(dt.final,dt.temp)
      }
    }

    # #keep only years in keepYearList
    # needs to be reloaded to get rid of year0 added above
    dt.final <- dt.final[year %in% keyVariable("keepYearList"),]

    inDT <- dt.final
    outName <- "dt.alcScenarios"
    cleanup(inDT,outName,fileloc("mData"))
