#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords alcoholic beverages, fish data from the book Fish to 2030
#'
# Intro ---------------------------------------------------------------

#Copyright (C) 2015 - 2018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details at http://www.gnu.org/licenses/.

#' @description This script reads in fish and alcoholic beverage data from FAO's Food Balance Sheet data
#' and parameters for IMPACT and generates scenarios of per capita availability for fish composite food items and
#' beer, wine and spirits. The output file
#' The fish food availability data are taken from FAO's FBS data set.

# options(warn=2)
source("R/nutrientModFunctions.R")
sourceFile <- "dataManagement.fishnAlc.R"
createScriptMetaData()

# load data files and key variables
IMPACTfish <- fileNameList("IMPACTfish") # spreadsheet name with data from Fish to 2030
IMPACTfish_code <- keyVariable("IMPACTfish_code") # names of the composites with fish, aquatic mammals and plants that are eaten
IMPACTalcohol_code <- keyVariable("IMPACTalcohol_code")
fishNalcNames <- c(IMPACTfish_code, IMPACTalcohol_code)

# scenarioListSSP.pop <- keyVariable("scenarioListSSP.pop")
# scenarioListSSP.GDP <- keyVariable("scenarioListSSP.GDP")
FBSyearsToAverage.baseyear <- keyVariable("FBSyearsToAverage.baseyear") # years over which the FBS data should be averaged to create base year values for elasticity calculations, currently 2005
FBSyearsToAverage.startyear <- keyVariable("FBSyearsToAverage.startyear") # years over which the FBS data should be averaged to create starting values, currently 2010
keepYearList <- keyVariable("keepYearList") # list of years we need to keep for later use between 2010 and 2050

dt.GDPperCap <- getNewestVersion("dt.pcGDPX0", fileloc("iData"))
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))

# convert FBS data to IMPACT regions
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))

regionList.FBS <- sort(unique(dt.FBS$ISO_code)) # Regions with FBS data
regionList.IMPACT <- sort(unique(dt.regions.all$region_code.IMPACT159)) # IMPACT regions
regionList.ISO <- sort(unique(dt.regions.all$ISO_code)) # ISO regions

regionList.missing.FBS <- sort(regionList.ISO[!regionList.ISO %in% regionList.FBS]) # regions in ISO but not in FBS
# 'big' countries without FBS data are Burundi, Bahrain, Bermuda, Bhutan, Congo (Democratic Republic aka Zaire), Equatorial Guinea,
# Greenland, Libya, Papua New Guinea, Somalia, Syria, Taiwan - as of May 12, 2018
dt.regions.all <- dt.regions.all[!region_code.IMPACT159 %in% keyVariable("dropListCty"),]

dt.FBS <- getNewestVersion("dt.FBS", fileloc("uData")) #dt.FBS is in kgPerCapPerYear
# as of July 24, 2018 FBS also has KcalPerCapPerDay; remove it here for this script
dt.FBS <- dt.FBS[!variable %in% "KcalPerCapPerDay", ]
#keep just the IMPACT fish and alcohol names in the FBS data and the base and start years to average
dt.FBS <- dt.FBS[IMPACT_code %in% fishNalcNames & year %in% c(FBSyearsToAverage.baseyear, FBSyearsToAverage.startyear)]

# the value variable in dt.FBS is in kg per person per year. Convert to per day here. 
# It is converted back to per year below to align with other food items from IMPACT.
dt.FBS[, value := value/keyVariable("DinY")]
dt.FBS[variable %in% "kgPerCapPerYear", variable := "kgPerCapPerDay"]
# Get the middle of the total number of FBS yearsToAverage. 
# middleYear is what we'll average on for the first two observations in calculating future food availability
middleYear.baseyear <- FBSyearsToAverage.baseyear[as.integer(length(FBSyearsToAverage.baseyear) / 2) + 1]
middleYear.startyear <- FBSyearsToAverage.startyear[as.integer(length(FBSyearsToAverage.startyear) / 2) + 1]

# do some data preparation
# remove text after SSPx to make sure the scenario names are only of the SSP scenarios (eg, SSP1, SSP2, ...)
# dt.pop[,scenario := substring(scenario, 1,4)] Not needed because Ag pop data has different scenarios and SSP pop data already only SSP1, etc.

# identify the n-1 year for the elasticity calculations. If the start year is X2010 and the interval is 5 years, the year0 is X2005
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X",year0, sep = "")
keepYearList <- c(year0,keepYearList)

# aggregate to IMPACT regions
merged <- merge(dt.FBS, dt.regions.all, by = "ISO_code")
keepListCol <- c("region_code.IMPACT159", "year", "IMPACT_code", "value")
merged[, setdiff(names(merged), keepListCol) := NULL]
merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]

dt.GDPperCap[, pcGDPX0.lag1 := data.table::shift(pcGDPX0, type = "lag"), by = c("region_code.IMPACT159", "scenario")]
dt.GDPperCap[, delta.GDP := pcGDPX0 - pcGDPX0.lag1]
dt.GDPperCap[,GDPRatio := delta.GDP/(pcGDPX0 - pcGDPX0.lag1)] # is this needed? Seems to be.
keepListCol <- c("region_code.IMPACT159", "scenario", "year","delta.GDP", "GDPRatio")
dt.GDPperCap[, setdiff(names(dt.GDPperCap), keepListCol) := NULL]

