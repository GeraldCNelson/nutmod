# Intro -----------------------
#This script reads in fish data and parameters for IMPACT and generates
#scenarios of per capita consumption.

#Copyright (C) 2015 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details at http://www.gnu.org/licenses/.

#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function")) {source("R/nutrientModFunctions.R")}
IMPACTfish_code <- keyVariable("IMPACTfish_code")
FBSyearsToAverage <- keyVariable("FBSyearsToAverage")
keepYearList <- keyVariable("keepYearList")
keepYearList <- keyVariable("keepYearList")
#need to include the n-1 year for the elasticity calculations
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X",year0,sep="")
keepYearList <- c(year0,keepYearList)
IMPACTfish <- fileNameList("IMPACTfish")
scenarioListSSP <- keyVariable("scenarioListSSP")

# load regions info ----
dt.regions.all <- data.table::as.data.table(getNewestVersion("df.regions.all"))

#prepare the SSP GDP data -----
dt.SSPGDP <- getNewestVersion("dt.SSPGDPClean")
data.table::setorder(dt.SSPGDP, scenario, ISO_code, year)
data.table::setkey(dt.SSPGDP, scenario, ISO_code)
# lag and difference SSP GDP -----
dt.SSPGDP[, GDP.lag1 := data.table::shift(value,1,0,"lag"), by = c("ISO_code","scenario")][, delta.GDP := value - GDP.lag1]

# prepare the FBS data -----
dt.FBS <- getNewestVersion("dt.FBS")

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
data.table::set(dt.FBS.kgPerCap, which(is.na(dt.FBS.kgPerCap[[baseYear]])), baseYear, 0)
data.table::setorder(dt.FBS.kgPerCap, ISO_code, IMPACT_code)

# # delete pesky countries
ctyDeleteList <- c("FSM", "GRD", "PRK")
# create list with only countries in both FBS and SSP
inFBS <- sort(dt.regions.all[!is.na(FAOSTAT_code), ISO_code])
inSSP <-
  sort(dt.regions.all[!is.na(region_code.SSP) &
                        !ISO_code %in% (ctyDeleteList), ISO_code])
ctyList <- sort(intersect(inFBS, inSSP))

# load SSP population, note that it only starts at 2010 ----
dt.SSPPopClean <- getNewestVersion("dt.SSPPopClean")
#remove countries that are not in both FBS And SSP
dt.SSPPopClean <- dt.SSPPopClean[ISO_code %in% ctyList, ]

# read in fish data from IMPACT  ----
# fish supply in 1000 metric tons
#' @param - fishS - supply of fish
fishS <- openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "QS_FishSys",
  cols = 1:6,
  startRow = 3,
  colNames = FALSE
)
colnames(fishS) <-
  c("fish_type",
    "region",
    "freshAquac",
    "marinAquac",
    "freshCapt",
    "marine_capt")

#' @param - fishLookup - fish look up. production and consumption names and IMPACT names
fishLookup <- openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "IMPACT Commodities",
  cols = 6:7,
  startRow = 2,
  colNames = TRUE
)

#' @param - fishD - fish demand in 1000 metric tons
fishD <- openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "DemandStkChg",
  cols = 1:11,
  startRow = 3,
  colNames = FALSE
)
colnames(fishD) <-
  c(
    "IMPACT_code",
    "region",
    "net_trade",
    "exports",
    "imports",
    "tot_demand",
    "food_demand",
    "feed_demand",
    "other_demand",
    "stock_change",
    "crush_demand"
  )

fishD[is.na(fishD)] <- 0
fishD <- fishD[order(fishD$region), ]

# create income elasticities data for the regions in IMPACT3 ----
#' @param - fishIncElast - fish income elasticity
dt.fishIncElast <- data.table::as.data.table(openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "IncDmdElas",
  cols = 1:11,
  startRow = 1,
  colNames = TRUE
))

#Column names can't have a "-" in them. This code changes them to underscore
data.table::setnames(
  dt.fishIncElast,
  old = colnames(dt.fishIncElast),
  new = gsub("-", "_", colnames(dt.fishIncElast))
)
# in code below, start with item 2 because item 1 is region
data.table::setnames(
  dt.fishIncElast,
  old = colnames(dt.fishIncElast)[2:length(dt.fishIncElast)],
  new = paste(colnames(dt.fishIncElast)[2:length(dt.fishIncElast)], "elas", sep =
                ".")
)
data.table::setnames(dt.fishIncElast, old = "region", new = "region_code.IMPACT115")
# add elasticity of zero for aquatic plants and animals (c_aqpl and c_aqan)
dt.fishIncElast[, c("c_aqan.elas", "c_aqpl.elas") := 0]
#if fixFish is TRUE, deal with the missing species - shrimp, tuna, and salmon
# rename shrimp elasticities to crustacean elasticities
# remove salmon and tuna elasticities
#if fixShrimp is TRUE, deal with the missing species - shrimp, tuna, and salmon
# rename shrimp elasticities to crustacean elasticities
# remove salmon and tuna elasticities
fixFish <- TRUE
if (fixFish == TRUE) {
  dt.fishIncElast[, c_Crust.elas := NULL]
  data.table::setnames(dt.fishIncElast, old = "c_shrimp.elas", new = "c_Crust.elas")
  itemsToRemove <- c("c_Shrimp", "c_Tuna", "c_Salmon")
  IMPACTfish_code <-
    IMPACTfish_code[!(IMPACTfish_code %in% itemsToRemove)]
}
fish_code.elast.list <-
  names(dt.fishIncElast)[2:length(dt.fishIncElast)]

# set max income elasticity to 1 if TRUE
changeElasticity <- TRUE
if (changeElasticity == TRUE) {
  temp <- names(dt.fishIncElast)[2:length(names(dt.fishIncElast))]
  for (j in temp)
    data.table::set(
      dt.fishIncElast,
      i = which(dt.fishIncElast[[j]] > 1L),
      j = j,
      value = 1L
    )
}
inDT <- temp
outName <- "dt.fishIncElast"
cleanup(inDT,outName, fileloc("iData"))
# merge regions.all and the IMPACT115fishIncElast to assign identical income elasticities
# to all countries in an IMPACT3 region.
data.table::setkey(dt.fishIncElast, region_code.IMPACT115)
data.table::setkey(dt.regions.all, region_code.IMPACT115)
dt.fishIncElast.ISO <- dt.fishIncElast[dt.regions.all]
deleteColList <- c("region_name.IMPACT115")
dt.fishIncElast.ISO <-
  dt.fishIncElast.ISO[, (deleteColList) := NULL]

# create a fish elasticities data table with the same income elasticities in all years
temp <- data.table::data.table(ISO_code = rep(dt.regions.all$ISO_code,
                                              each = length(keepYearList)))
temp <-
  data.table::data.table(ISO_code = rep((dt.fishIncElast.ISO), each = length(keepYearList)))
dt.years <-
  data.table::data.table(year = rep(keepYearList, each = nrow(dt.fishIncElast.ISO)))

#' @param - dt.fishIncElast.ISO - fish elasticities for each region in the SSP data and all years
dt.fishIncElast.ISO <- cbind(dt.years, dt.fishIncElast.ISO)
idVars <- c("region_code.SSP", "year")
dt.fishIncElast <- data.table::melt(
  dt.fishIncElast.ISO,
  id.vars = idVars,
  variable.name = "variable",
  measure.vars = fish_code.elast.list,
  variable.factor = FALSE
)
dt.fishIncElast <- dt.fishIncElast[!is.na(region_code.SSP), ]
data.table::setnames(dt.fishIncElast, old = "region_code.SSP", new = "ISO_code")
inDT <- dt.fishIncElast
outName <- "dt.fishIncElast"
cleanup(inDT,outName,fileloc("iData"))

# loop over scenarios and countries  -----
# set up a data table to hold the results of the calculations
dt.final <-
  data.table::data.table(scenario = character(0),
             ISO_code = character(0),
             year = character(0))
dt.final[, (IMPACTfish_code) := 0]

# arc elasticity elasInc = [(Qn - Qn-1)/(Qn + Qn-1)/2] / [(Yn - Yn-1) /(Yn + Yn-1)/2)]
# [(Qn - Qn-1)/(Qn + Qn-1)] = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# set x = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# x * (Qn + Qn-1) + Qn-1 = x* Qn + x * Qn-1 + Qn-1 = Qn
# Qn -  x * Qn = x * Qn-1 + Qn-1
# Qn = (x * Qn-1 + Qn-1))/1-x

Qn <- function(elasInc, GDPn, GDPn_1, delta.GDP, Qn_1) {
  x <- elasInc * delta.GDP / (GDPn + GDPn_1)
  Qn <- (x + 1) * Qn_1 / (1 - x)
  return(Qn)
}

for (scenarioChoice in scenarioListSSP) {
  for (ctyChoice in ctyList) {
    # subset on current scenario and country
    dt.GDP <-  dt.SSPGDP[.(scenarioChoice, ctyChoice)]

    # the .() code is very useful in subsetting a data table.
    # step 1. setkey for the columns you want to subset on (e.g., variable, ISO_code)
    # step 2. Use this construction (dt.FBS[.("perCapKg",ctyChoice)]) to create a new data table.
    # Note that in this example ctyChoice is a variable that can have multiple items

    # create a data table with FBS fish perCapKg values for one country
    keylist <- c("ISO_code", "IMPACT_code")
    data.table::setkeyv(dt.FBS.kgPerCap, keylist)
    #' @param dt.FBS.kgPerCap - FBS kgPerCap numbers for years in the keepYearsList
    dt.FBS.subset <-
      dt.FBS.kgPerCap[J(ctyChoice, IMPACTfish_code)]
    # some countries don't have values for the base year. Next two lines of code deal with this.
    dt.FBS.subset[, year := (middleYear)]
    dt.FBS.subset[is.na(get(baseYear)), (baseYear) := 0, with = FALSE]
    data.table::setkeyv(dt.FBS.subset, c("IMPACT_code", baseYear))
    keepListCol <- c("scenario", "ISO_code", "year")
    dt.temp <- dt.GDP[, keepListCol, with = FALSE]
    dt.temp[, (IMPACTfish_code) := 0]
    # loop over all fish codes ---
    for (fish in IMPACTfish_code) {
      #fish <- IMPACTfish_code[1]
      # set baseyear value to the average from FBS
      dt.temp[year == middleYear, (fish) :=  dt.FBS.subset[IMPACT_code == fish, get(baseYear)]]
      # get the country elasticities
      keylist <- c("ISO_code", "variable")
      data.table::setkeyv(dt.fishIncElast, keylist)
      dt.elas <-
        dt.fishIncElast[.(ctyChoice, paste(fish, ".elas", sep = ""))]

      #subsequent rows
      for (n in 2:nrow(dt.temp)) {
        #        GDPn_1 <- dt.GDP[n, GDP.lag1]
        #        elastIncn <- dt.elas[n, value]
        n_1 <- n - 1
        Qn_1 <- dt.temp[n_1, get(fish)]
        dt.temp[n, (fish) := Qn(dt.elas[n, value], dt.GDP[n, value], dt.GDP[n, GDP.lag1], dt.GDP[n, delta.GDP], Qn_1)]
      }
    }
    dt.final <- rbind(dt.final, dt.temp)
  }
}

# add SSP population ---
data.table::setkeyv(dt.SSPPopClean, c("scenario", "ISO_code", "year"))
dt.SSPPopTot <-
  dt.SSPPopClean[, sum(value), by = eval(data.table::key(dt.SSPPopClean))]
data.table::setnames(dt.SSPPopTot, "V1", "pop.tot")
data.table::setkeyv(dt.SSPPopTot, c("scenario", "ISO_code", "year"))
data.table::setkeyv(dt.final, c("scenario", "ISO_code", "year"))
dt.final[, pop := dt.SSPPopTot$pop]

#keep only years in keepYearList
dt.final <- dt.final[year %in% keyVariable("keepYearList"),]
inDT <- dt.final
outName <- "dt.fishScenarios"
cleanup(inDT,outName,fileloc("mData"))

# # combine average (initially around 2005) fish data from FBS, the income scenario data to 2050,
# # and the melted (into long form) elasticity data
# # First key the relevant variables in the three data sets FBS, SSP GDP data and IMPACT income elastities
# key.fish <- c("scenario","ISO_code","year","variable","value",paste("aveAt",middleYear,sep=""))
# data.table::setkeyv(dt.FBS.fish,key.fish)
# key.GDP <- c("scenario","ISO_code","year","variable","value")
# data.table::setkeyv(dt.SSPGDP,key.GDP)
# key.elast <- c("ISO_code","year","variable","value")
# data.table::setkeyv(dt.fishIncElast.ISO.melt,key.elast)
# # Now merge all three at once. Need to create the mymerge function to do this
# mymerge = function(x,y) merge(x,y,all=TRUE)
# dt.fishfinal <- Reduce(mymerge,list(dt.SSPGDP,dt.FBS.fish,dt.fishIncElast.ISO.melt))
# setorder(dt.fishfinal,scenario,ISO_code, variable,year,value)
# setcolorder(dt.fishfinal, c("scenario","ISO_code", "variable","unit",
#                             "aveAtX2005","year","value"))
#
# # pull out the elasticities
# dt.test.elas <- dt.test[.(fish_code.elast)]
# dt.test.elas[,c("scenario","unit","aveAtX2005") := NULL]
