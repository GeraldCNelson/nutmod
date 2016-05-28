# Intro -----------------------
#This script reads in alcoholic beverages data from FAO's Food Balance Sheet data.
# It uses income elasticity parameters from
# Nelson, Jon P. 2013. “Meta-Analysis of Alcohol Price and Income Elasticities –
# with Corrections for Publication Bias.” Health Economics Review 3 (1): 17.
# doi:10.1186/2191-1991-3-17. http://www.healtheconomicsreview.com/content/3/1/17.

# beverage,price_elast,income_elast
# beer, -0.30, 0.50
# wine, -0.45, 1.00
# spirits, -0.55, 1.00

# Note that only the income elasticties parameters are used below. These are for the US and are assumed to be smaller
# than for countries with lower per capita income.

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
scenarioListSSP.pop <- keyVariable("scenarioListSSP.pop")
scenarioListSSP.GDP <- keyVariable("scenarioListSSP.GDP")
options(warn=2)
#load the SSP GDP data -----
dt.SSPGDP <- getNewestVersion("dt.SSPGDPClean")

# load regions info ----
dt.regions.all <- data.table::as.data.table(getNewestVersion("df.regions.all"))
# create list with only countries in both FBS and SSP
inFBS <- sort(dt.regions.all[!is.na(FAOSTAT_code), ISO_code])
# the pop data has a different set of countries than the GDP data. Only keep countries that are in both
inSSP.pop <- sort(dt.regions.all[!is.na(region_code.SSP), ISO_code])
inSSP.GDP <- sort(unique(dt.SSPGDP$ISO_code))
setdiff(inSSP.pop,inSSP.GDP)
inSSP <- sort(intersect(inSSP.pop,inSSP.GDP))
ctyList <- sort(intersect(inFBS,inSSP))
# regions.all <- getNewestVersion("df.regions.all")
# region <- keyVariable("region")
# setdiff(regions.all[,region],ctyList)

data.table::setnames(dt.SSPGDP, old = "ISO_code", new = "region_code.SSP")# change code for countries from ISO to SSP
data.table::setkeyv(dt.SSPGDP, c("scenario", "region_code.SSP"))
data.table::setorder(dt.SSPGDP, scenario, region_code.SSP, year)

# lag and difference SSP GDP -----
dt.SSPGDP[,GDP.lag1 := data.table::shift(value,type = "lag"), by = c("region_code.SSP","scenario")]
dt.SSPGDP[,delta.GDP := value - GDP.lag1]

# prepare the FBS data -----
dt.FBS <- getNewestVersion("dt.FBS")
# keep only FBS data for countries in ctyList
dt.FBS <- dt.FBS[ISO_code %in% ctyList, ]
data.table::setnames(dt.FBS, old = "ISO_code", new = "region_code.SSP")# change code for countries from ISO to SSP

#keep only the years to average
dt.FBS <- dt.FBS[year %in% FBSyearsToAverage,]
# Get the middle of the total number of FBS yearsToAverage. middleYear is what we'll average on
middleYear <-
  FBSyearsToAverage[as.integer(length(FBSyearsToAverage) / 2) + 1]

# choices of FBS data are "foodMT", "perCapKg","perCapKcal","perCapProt"
data.table::setkey(dt.FBS, variable)
dt.FBS.kgPerCap <- dt.FBS["perCapKg"] # choose perCapKg
#to reduce the clutter in the FBS DT
keepListCol <- c("region_code.SSP", "IMPACT_code", "year", "value")
dt.FBS.kgPerCap <- dt.FBS.kgPerCap[, keepListCol, with = FALSE]
#now average
baseYear <- paste("aveAt", middleYear, sep = "")
dt.FBS.kgPerCap[, (baseYear) := mean(value), by = list(region_code.SSP, IMPACT_code)]

dt.FBS.kgPerCap <- dt.FBS.kgPerCap[year == middleYear,]
deleteListCol <- c("value")
dt.FBS.kgPerCap[, (deleteListCol) := NULL]
data.table::set(dt.FBS.kgPerCap, which(is.na(dt.FBS.kgPerCap[[baseYear]])), baseYear, 0)
data.table::setorder(dt.FBS.kgPerCap, region_code.SSP, IMPACT_code)

# load SSP population, note that it only starts at 2010 -----
# dt.SSPPopClean <- getNewestVersion("dt.SSPPopClean")
# #keep only countries that are in both FBS And SSP
# dt.SSPPopClean <- dt.SSPPopClean[ISO_code %in% ctyList,]

# create income elasticities data for the regions common to SSP and FSB ----
# create alcohol elasticities data table, all countries have the same income elasticities in all years
dt.elas.wide <- data.table::data.table(region_code.SSP = rep(dt.regions.all$region_code.SSP, each = length(keepYearList)),
                                       year = keepYearList,
                                       c_beer.elas = 0.50,
                                       c_wine.elas = 1.00,
                                       c_spirits.elas = 1.00)

dt.elas.wide <- dt.elas.wide[region_code.SSP %in% ctyList]

idVars <- c("region_code.SSP", "year")
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

# arc elasticity elasInc = [(Qn - Qn-1)/(Qn + Qn-1)/2] / [(Yn - Yn-1) /(Yn + Yn-1)/2)]
# [(Qn - Qn-1)/(Qn + Qn-1)] = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# set x = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# x * (Qn + Qn-1) = (Qn - Qn-1)
# x * Qn + x* Qn-1 + Qn-1 = Qn
# x* Qn - Qn = - Qn-1 - x * Qn-1 = - Qn-1 (1 + x)
# (x - 1)* Qn = - Qn-1 - x * Qn-1 = - Qn-1 (1 + x)
# Qn = - Qn-1 (1 + x)/(x-1) = Qn-1 (1+x)/(1-x)

# loop over scenarios and countries common to FBS and SSP  -----

# set up dt to hold the results
dt.final <- data.table::data.table(scenario = character(0),
            region_code.SSP = character(0), year = character(0))
dt.final[, (IMPACTalcohol_code) := 0]

for (scenarioChoice in scenarioListSSP.GDP) {
  for (ctyChoice in ctyList) {
    print(paste(scenarioChoice,ctyChoice))
    # create a data table with FBS alc perCapKg values for one country
    keylist <- c("region_code.SSP", "IMPACT_code")
    data.table::setkeyv(dt.FBS.kgPerCap, keylist)
    #' @param dt.FBS.kgPerCap - FBS kgPerCap numbers for years in the keepYearsList
    dt.FBS.subset <- dt.FBS.kgPerCap[J(ctyChoice, IMPACTalcohol_code)]
    # some countries don't have values for the base year. Next two lines of code deal with this.
    dt.FBS.subset[, year := (middleYear)]
    dt.FBS.subset[is.na(get(baseYear)), (baseYear) := 0, with = FALSE]
    data.table::setkeyv(dt.FBS.subset, c("IMPACT_code", baseYear))

    # subset on current scenario and country
    data.table::setkeyv(dt.SSPGDP, c("scenario", "region_code.SSP"))
    dt.GDP <-  dt.SSPGDP[scenario %in% scenarioChoice  &  region_code.SSP %in% ctyChoice,]
    dt.GDP[,(IMPACTalcohol_code) := 0]

    dt.temp.alc.elas <- dt.elas.wide[region_code.SSP %in% ctyChoice,]
    dt.GDP <- merge(dt.GDP,dt.temp.alc.elas, by = c("region_code.SSP","year"))
    # calculation to get to Qn
    # x <- elasInc * delta.GDP/(GDPn + GDPn_1)
    # Qn <- Qn_1 * (1 + x) / (1 - x)
    for (alc in IMPACTalcohol_code) {
      elas <- paste(alc,"elas", sep = ".")
      dt.GDP[,temp := get(elas) * delta.GDP/(value + GDP.lag1)]
      dt.GDP[year == "X2005",(alc) := dt.FBS.subset[IMPACT_code == alc,get(baseYear)]]
      dt.GDP[, (alc) := get(alc)[1L]][-1L, (alc) := get(alc) * (1 + temp)  / (1 - temp)]
    }

    keepListCol <- c("scenario", "region_code.SSP", "year", IMPACTalcohol_code)
    dt.temp <- dt.GDP[,keepListCol, with = FALSE]
    dt.final <- rbind(dt.final,dt.temp)
  }
}

#
# # add an SSP population to the final data table ----
# data.table::setkeyv(dt.SSPPopClean, c("scenario", "ISO_code", "year"))
# dt.SSPPopTot <- dt.SSPPopClean[, sum(value), by = eval(data.table::key(dt.SSPPopClean))]
# data.table::setnames(dt.SSPPopTot,"V1","pop.tot")
# data.table::setkeyv(dt.SSPPopTot, c("scenario","ISO_code","year"))
# data.table::setkeyv(dt.final, c("scenario","ISO_code","year"))
#
# dt.final[,pop := dt.SSPPopTot$pop]
#
# #keep only years in keepYearList
# needs to be reloaded to get rid of year0 added above
keepYearList <- keyVariable("keepYearList")
dt.final <- dt.final[year %in% keepYearList,]

inDT <- dt.final
outName <- "dt.alcScenarios"
cleanup(inDT,outName,fileloc("mData"))
