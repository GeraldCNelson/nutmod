# Intro -----------------------
#This script reads in alcohol data from FBS.
# Uses income elasticity parameters from
# Nelson, Jon P. 2013. “Meta-Analysis of Alcohol Price and Income Elasticities –
# with Corrections for Publication Bias.” Health Economics Review 3 (1): 17.
# doi:10.1186/2191-1991-3-17. http://www.healtheconomicsreview.com/content/3/1/17.

# beverage,price_elast,income_elast
# beer, -0.30, 0.50
# wine, -0.45, 1.00
# spirits, -0.55, 1.00

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
FBSyearsToAverage <- keyVariable("FBSyearsToAverage")
keepYearList <- keyVariable("keepYearList")
#need to include the n-1 year for the elasticity calculations
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X", year0, sep = "")
keepYearList <- c(year0,keepYearList)
IMPACTalcohol_code <- keyVariable("IMPACTalcohol_code")
scenarioListSSP <- keyVariable("scenarioListSSP")

# load regions info ----
dt.regions.all <- data.table::as.data.table(getNewestVersion("df.regions.all"))

#prepare the SSP GDP data -----
dt.SSPGDP <- getNewestVersion("dt.SSPGDPClean")
data.table::setorder(dt.SSPGDP, scenario, ISO_code, year)
data.table::setkeyv(dt.SSPGDP, c("scenario", "ISO_code"))

# lag and difference SSP GDP -----

#dt.SSPGDP[, GDP.lag1:=c(NA, value[-.N]), by = c("ISO_code","scenario")][, delta.GDP := value - GDP.lag1]
dt.SSPGDP[,GDP.lag1 := data.table::shift(value,type = "lag"), by = c("ISO_code","scenario")]
dt.SSPGDP[,delta.GDP := value - GDP.lag1]

# prepare the FBS data -----
dt.FBS <- getNewestVersion("dt.FBS")
#keep only the years to average
dt.FBS <- dt.FBS[year %in% FBSyearsToAverage,]
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

dt.FBS.kgPerCap <- dt.FBS.kgPerCap[year == middleYear,]
deleteListCol <- c("value")
dt.FBS.kgPerCap[, (deleteListCol) := NULL]
data.table::set(dt.FBS.kgPerCap, which(is.na(dt.FBS.kgPerCap[[baseYear]])), baseYear, 0)
data.table::setorder(dt.FBS.kgPerCap,ISO_code,IMPACT_code)

# delete pesky countries
ctyDeleteList <- c("FSM","GRD","PRK")

# create list with only countries in both FBS and SSP
inFBS <- sort(dt.regions.all[!is.na(FAOSTAT_code),ISO_code])
inSSP <- sort(dt.regions.all[!is.na(region_code.SSP) & !ISO_code %in% (ctyDeleteList),ISO_code])
ctyList <- sort(intersect(inFBS,inSSP))

# load SSP population, note that it only starts at 2010 ----
dt.SSPPopClean <- getNewestVersion("dt.SSPPopClean")
#remove countries that are not in both FBS And SSP
dt.SSPPopClean <- dt.SSPPopClean[ISO_code %in% ctyList,]

# create income elasticities data for the regions in IMPACT3 ----
# create alcohol elasticities data table, all countries have the same income elasticities in all years
temp <- data.table::data.table(ISO_code = rep(dt.regions.all$ISO_code, each = length(keepYearList)),
                             year = keepYearList,
                             c_beer.elas = 0.50,
                             c_wine.elas = 1.00,
                             c_spirits.elas = 1.00)

idVars <- c("ISO_code", "year")
alc_code.elast.list <-
  names(temp)[3:length(temp)]
dt.alcIncElast <- data.table::melt(
    temp,
    id.vars = idVars,
    variable.name = "variable",
    measure.vars = alc_code.elast.list,
    variable.factor = FALSE
  )
inDT <- temp
outName <- "dt.alcIncElast"
cleanup(inDT,outName, fileloc("iData"))

# # merge regions.all and the IMPACT115alcIncElast to assign identical income elasticities
# # to all countries in an IMPACT3 region.
# setkey(dt.alcIncElast, region_code.IMPACT115)
# setkey(dt.regions.all, region_code.IMPACT115)
# dt.alcIncElast.ISO <- dt.alcIncElast[dt.regions.all]
# deleteColList <- c("region_name.IMPACT115")
# dt.alcIncElast.ISO <- dt.alcIncElast.ISO[, (deleteColList) := NULL]

# # create an alc elasticities data table with the same income elasticities in all years
# temp <-
#   data.table(ISO_code = rep((dt.alcIncElast.ISO), each = length(keepYearList)))
# dt.years <-
#   data.table(year = rep(keepYearList, each = nrow(dt.alcIncElast.ISO)))

#' #' @param - dt.alcIncElast.ISO - alc elasticities for each region in the SSP data and all years
#' dt.alcIncElast.ISO <- cbind(dt.years, dt.alcIncElast.ISO)
#' idVars <- c("region_code.SSP", "year")
#' dt.alcIncElast <- melt(
#'   dt.alcIncElast.ISO,
#'   id.vars = idVars,
#'   variable.name = "variable",
#'   measure.vars = alc_code.elast.list,
#'   variable.factor = FALSE
#' )
#' dt.alcIncElast <- dt.alcIncElast[!is.na(region_code.SSP),]
#' setnames(dt.alcIncElast,old = "region_code.SSP", new = "ISO_code")

# loop over scenarios and countries  -----
# this code is designed to make loop implementation easier in the future
# set up a data table to hold the results of the calculations
dt.final <-
  data.table::data.table(scenario = character(0),
             ISO_code = character(0),
             year = character(0))
dt.final[, (IMPACTalcohol_code) := 0]

# arc elasticity elasInc = [(Qn - Qn-1)/(Qn + Qn-1)/2] / [(Yn - Yn-1) /(Yn + Yn-1)/2)]
# [(Qn - Qn-1)/(Qn + Qn-1)] = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# set x = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# x * (Qn + Qn-1) + Qn-1 = x* Qn + x * Qn-1 + Qn-1 = Qn
# Qn -  x * Qn = x * Qn-1 + Qn-1
# Qn = (x * Qn-1 + Qn-1))/1-x


#' Title function to generate nth value of quantity consumed
#'
#' @param elasInc - income elasticity
#' @param GDPn - GDP for year n
#' @param GDPn_1 - GDP for year n-1
#' @param delta.GDP - difference in GDP
#' @param Qn_1 - consumption quantity in year n-1
#'
#' @return Qn
Qn <- function(elasInc, GDPn, GDPn_1, delta.GDP, Qn_1) {
  x <- elasInc * delta.GDP/(GDPn + GDPn_1)
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

    # create a data table with FBS alc perCapKg values for one country
    keylist <- c("ISO_code", "IMPACT_code")
    data.table::setkeyv(dt.FBS.kgPerCap, keylist)
    #' @param dt.FBS.kgPerCap - FBS kgPerCap numbers for years in the keepYearsList
    dt.FBS.subset <-
      dt.FBS.kgPerCap[J(ctyChoice, IMPACTalcohol_code)]
    # some countries don't have values for the base year. Next two lines of code deal with this.
    dt.FBS.subset[, year := (middleYear)]
    dt.FBS.subset[is.na(get(baseYear)), (baseYear) := 0, with = FALSE]
    data.table::setkeyv(dt.FBS.subset,c("IMPACT_code",baseYear))
    keepListCol <- c("scenario", "ISO_code", "year")
    dt.temp <- dt.GDP[, keepListCol, with = FALSE]
    dt.temp[, (IMPACTalcohol_code) := 0]
    # loop over all alc codes ---
    for (alc in IMPACTalcohol_code) {
      #alc <- IMPACTalcohol_code[1]
      # set baseyear value to the average from FBS
      dt.temp[year == middleYear, (alc) :=  dt.FBS.subset[IMPACT_code == alc,get(baseYear)]]
      # get the country elasticities
      keylist <- c("ISO_code", "variable")
      data.table::setkeyv(dt.alcIncElast, keylist)
      dt.elas <-
        dt.alcIncElast[.(ctyChoice, paste(alc, ".elas", sep = ""))]

      #subsequent rows
      for (n in 2:nrow(dt.temp)) {
        #        GDPn_1 <- dt.GDP[n, GDP.lag1]
        #        elastIncn <- dt.elas[n, value]
        n_1 <- n - 1
        Qn_1 <- dt.temp[n_1, get(alc)]
        dt.temp[n, (alc) := Qn(dt.elas[n, value], dt.GDP[n, value], dt.GDP[n, GDP.lag1], dt.GDP[n, delta.GDP],Qn_1)]
      }
    }

    dt.final <- rbind(dt.final,dt.temp)
  }
}

# add SSP population ---
data.table::setkeyv(dt.SSPPopClean, c("scenario", "ISO_code", "year"))
dt.SSPPopTot <- dt.SSPPopClean[, sum(value), by = eval(data.table::key(dt.SSPPopClean))]
data.table::setnames(dt.SSPPopTot,"V1","pop.tot")
data.table::setkeyv(dt.SSPPopTot, c("scenario","ISO_code","year"))
data.table::setkeyv(dt.final, c("scenario","ISO_code","year"))

dt.final[,pop := dt.SSPPopTot$pop]

#keep only years in keepYearList
dt.final <- dt.final[year %in% keyVariable("keepYearList"),]

inDT <- dt.final
outName <- "dt.alcScenarios"
cleanup(inDT,outName,fileloc("mData"))
