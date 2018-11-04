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
#' beer, wine and spirits. The output file is dt.fishnAlcScenarios
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

FBSyearsToAverage.baseyear <- keyVariable("FBSyearsToAverage.baseyear") # years over which the FBS data should be averaged to create base year values for elasticity calculations, currently 2005
FBSyearsToAverage.startyear <- keyVariable("FBSyearsToAverage.startyear") # years over which the FBS data should be averaged to create starting values, currently 2010
keepYearList <- keyVariable("keepYearList") # list of years we need to keep for later use between 2010 and 2050

# Get the middle of the total number of FBS yearsToAverage. 
# middleYear is what we'll average on for the first two observations in calculating future food availability
middleYear.baseyear <- FBSyearsToAverage.baseyear[as.integer(length(FBSyearsToAverage.baseyear) / 2) + 1]
middleYear.startyear <- FBSyearsToAverage.startyear[as.integer(length(FBSyearsToAverage.startyear) / 2) + 1]

dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
dt.GDPperCap <- getNewestVersion("dt.pcGDPX0", fileloc("iData"))
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData")) # now just used to weight the population share of IMPACT regions that make up multiple ISO codes Oct 13, 2018
dt.FBS <- getNewestVersion("dt.FBS", fileloc("uData")) #dt.FBS is in kgPerCapPerYear
# as of July 24, 2018 FBS also has KcalPerCapPerDay; remove it here for this script
dt.FBS <- dt.FBS[!variable %in% "KcalPerCapPerDay", ]
#keep just the IMPACT fish and alcohol names in the FBS data and the base and start years to average
dt.FBS <- dt.FBS[IMPACT_code %in% fishNalcNames & year %in% c(FBSyearsToAverage.baseyear, FBSyearsToAverage.startyear)]

# average FBS data for three years around baseyear and startyear
formula.wide <- "ISO_code + IMPACT_code ~ year"
dt.FBS.wide <- data.table::dcast(
  data = dt.FBS,
  formula = formula.wide,
  value.var = "value")

dt.FBS.wide[, value.baseyear := rowMeans(.SD), .SDcols = FBSyearsToAverage.baseyear]
dt.FBS.wide[, value.startyear := rowMeans(.SD), .SDcols = FBSyearsToAverage.startyear]
keepListCol <- c("ISO_code", "IMPACT_code", "value.baseyear", "value.startyear")
dt.FBS.wide[, setdiff(names(dt.FBS.wide), keepListCol) := NULL]
# change name of values to align with other data
setnames(dt.FBS.wide, old = c("value.baseyear", "value.startyear"), new = c(middleYear.baseyear, middleYear.startyear))

dt.FBS <- melt(dt.FBS.wide,
               id.vars = c("ISO_code", "IMPACT_code"),
               measure.vars = c(middleYear.baseyear, middleYear.startyear),
               variable.name = "year",
               variable.factor = FALSE)

# convert FBS data to IMPACT regions
# note that FBS doesn't have data for all regions
# regionList.FBS <- sort(unique(dt.FBS$ISO_code)) # Regions with FBS data
# regionList.IMPACT <- sort(unique(dt.regions.all$region_code.IMPACT159)) # IMPACT regions
# regionList.ISO <- sort(unique(dt.regions.all$ISO_code)) # ISO regions
# 
# regionList.missing.FBS <- sort(regionList.ISO[!regionList.ISO %in% regionList.FBS]) # regions in ISO but not in FBS
# # 'big' countries without FBS data are Burundi, Bahrain, Bermuda, Bhutan, Congo (Democratic Republic aka Zaire), Equatorial Guinea,
# # Greenland, Libya, Papua New Guinea, Somalia, Syria, Taiwan - as of May 12, 2018
# dt.regions.all <- dt.regions.all[!region_code.IMPACT159 %in% keyVariable("dropListCty"),]

# prepare the FBS per capita food availability data -----
# FBS data are by ISO regions
# aggregate FBS data to region_code.IMPACT159; mean of per capita availability weighted by population shares in middleYear.startyear
#  keep population data by  ISO code for middleYear.startyear
dt.pop.initialYears <- dt.pop[year %in% c(middleYear.baseyear, middleYear.startyear),]
dt.pop.initialYears[, scenario := NULL]
dt.pop.initialYears <- unique(dt.pop.initialYears)

# aggregate to IMPACT regions
dt.FBS <- merge(dt.FBS, dt.regions.all, by = "ISO_code")
keepListCol <- c("region_code.IMPACT159", "year", "IMPACT_code", "value")
dt.FBS[, setdiff(names(dt.FBS), keepListCol) := NULL]
dt.FBS <- merge(dt.FBS, dt.pop.initialYears, by = c("region_code.IMPACT159", "year"))
dt.FBS <- dt.FBS[, value := weighted.mean(value, PopX0), by = c("IMPACT_code", "region_code.IMPACT159", "year")]
dt.FBS[, PopX0 := NULL] # added Oct 13, 2018
dt.FBS <- unique(dt.FBS)

# the value variable in dt.FBS is in kg per person per year. Convert to per day here. 
# It is converted back to per year below to align with other food items from IMPACT.
dt.FBS[, value := value/keyVariable("DinY")]
#dt.FBS[variable %in% "kgPerCapPerYear", variable := "kgPerCapPerDay"]

# identify the n-1 year for the elasticity calculations. If the start year is X2010 and the interval is 5 years, the year0 is X2005
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X",year0, sep = "")
keepYearList <- c(year0,keepYearList)

dt.GDPperCap[, pcGDPX0.lag1 := data.table::shift(pcGDPX0, type = "lag"), by = c("region_code.IMPACT159", "scenario")]
dt.GDPperCap[, delta.GDP := pcGDPX0 - pcGDPX0.lag1]
dt.GDPperCap[,GDPRatio := delta.GDP/(pcGDPX0 + pcGDPX0.lag1)] # is this needed? Seems to be.Hmmm. it is either 1 or NA. Changed from - to + in the denominator Oct 13, 2018
keepListCol <- c("region_code.IMPACT159", "scenario", "year","delta.GDP", "GDPRatio")
dt.GDPperCap[, setdiff(names(dt.GDPperCap), keepListCol) := NULL]

dt.incElas.alc <- data.table::data.table(region_code.IMPACT159 = rep(dt.regions.all$region_code.IMPACT159, each = length(keepYearList)),
                                        year = keepYearList,
                                        c_beer.elas = 0.50,
                                        c_wine.elas = 1.00,
                                        c_spirits.elas = 1.00)

#' @param - fishIncElast - fish income elasticity note: these use the older IMPACT region names and there are only 115 regions
dt.incElas.fish <- data.table::as.data.table(openxlsx::read.xlsx(
  IMPACTfish, sheet = "IncDmdElas", cols = 1:11, startRow = 1, colNames = TRUE))

#Column names can't have a "-" in them. This code changes them to underscore
data.table::setnames(dt.incElas.fish, old = colnames(dt.incElas.fish), new = gsub("-", "_", colnames(dt.incElas.fish)))
# in code below, start with item 2 because item 1 is region
data.table::setnames(dt.incElas.fish, old = c("region", colnames(dt.incElas.fish)[!colnames(dt.incElas.fish) %in% "region"]),
                     new = c("region", paste(colnames(dt.incElas.fish)[!colnames(dt.incElas.fish) %in% "region"], "elas", sep = ".")))
data.table::setnames(dt.incElas.fish, old = "region", new = "region_code.IMPACT115")

# add elasticity of zero for aquatic plants and animals (c_aqpl and c_aqan)
dt.incElas.fish[, c("c_aqan.elas", "c_aqpl.elas") := 0]

#' if fixFish is TRUE, deal with the missing species - shrimp, tuna, and salmon
#' rename shrimp elasticities to crustacean elasticities
#' remove salmon and tuna elasticities

switch.fixFish <- keyVariable("switch.fixFish")
if (switch.fixFish == TRUE) {
  dt.incElas.fish[, c_Crust.elas := NULL]
  data.table::setnames(dt.incElas.fish, old = "c_shrimp.elas", new = "c_Crust.elas")
  itemsToRemove <- c("c_Shrimp", "c_Tuna", "c_Salmon")
  fishNalcNames <- fishNalcNames[!fishNalcNames %in% itemsToRemove]
  itemsToRemove <- c("c_Tuna.elas", "c_Salmon.elas") # because shrimp already removed above
  dt.incElas.fish[, (itemsToRemove) := NULL]
}
# fish_code.elast.list <-
#   names(dt.incElas.fish)[2:length(dt.incElas.fish)]

# set max income elasticity to 1 if TRUE
switch.changeElasticity <- keyVariable("switch.changeElasticity")
if (switch.changeElasticity == TRUE) {
  #  temp <- names(dt.incElas.fish)[2:length(names(dt.incElas.fish))]
  temp <- names(dt.incElas.fish)[grep("elas", names(dt.incElas.fish))]
  for (j in temp)
    data.table::set(
      dt.incElas.fish,
      i = which(dt.incElas.fish[[j]] > 1L),
      j = j,
      value = 1L
    )
}

# next few lines assign income elasticities to all the region_code.IMPACT159 countries. For non-159 countries, elasticities are set to values for ROW.
dt.incElas.fish <- merge(dt.incElas.fish, dt.regions.all, by = "region_code.IMPACT115")

# Need to get rid of all region_code.IMPACT159 that are in ROW for region_code.IMPACT115. The logic below appears to work. Oct 29, 2018
regions.159 <- unique(dt.regions.all$region_code.IMPACT159)
dt.incElas.fish <- dt.incElas.fish[!(region_code.IMPACT115 %in% "ROW" &  
                                     region_code.IMPACT159 %in% regions.159), ]

keepListCol <- c( "region_code.IMPACT159", paste0(fishNalcNames, ".elas"))
dt.incElas.fish[, setdiff(names(dt.incElas.fish), keepListCol) := NULL]
dt.incElas.fish <- unique(dt.incElas.fish)

# need to weight fish elasticities for individual  countries in regions by their population. oct 29, 2018

# BLT, BLX, CHM, CHP, GSA, ITP, MOR, OBN, RAP, SDP, SPP still duplicated in region_code.IMPACT159 so need to run the code below
dt.incElas.fish <- merge(dt.incElas.fish, dt.pop.initialYears, by = c("region_code.IMPACT159"))
fishElas <- names(dt.incElas.fish)[!names(dt.incElas.fish) %in% c("region_code.IMPACT159", "year")]
for (i in fishElas){
  dt.incElas.fish <- dt.incElas.fish[, (i) := weighted.mean(get(i), PopX0), by = c( "region_code.IMPACT159", "year")]
}
dt.incElas.fish[, c("PopX0", "year") := NULL] 
dt.incElas.fish <- unique(dt.incElas.fish)

# create a fish elasticities data table with the same income elasticities in all years
 dt.years <- data.table::data.table(year = rep(keepYearList, each = nrow(dt.incElas.fish)))
 #'@param - dt.incElas.fish - fish elasticities for each region in the SSP data and all years
dt.incElas.fish <- cbind(dt.years, dt.incElas.fish)

inDT <- dt.incElas.fish
outName <- "dt.incElas.fish" # not used elsewhere
desc <- "Fish income elasticities estimates to 2050; capped at 1.0. Assumed to be identical in all scenarios and all time periods"
cleanup(inDT, outName, fileloc("mData"), desc = desc)

#combine alcohol and fish income elasticities
dt.incElas.fishnalc <- merge(dt.incElas.fish, dt.incElas.alc, by = c("region_code.IMPACT159", "year"))
dt.incElas.fishnalc <- unique(dt.incElas.fishnalc)

#' The goal is to estimate food demand (and set it equal to food availability) based on the food availability values from the FBS, income elasticities from
#' Fish to 2030 and the income levels from the SSPs or other sources. Note that everything is in per capita.
#' In the case of GDP, it is per capita per year.
#' In the case of availability it is per capita (kg) per day. Whether this is per day or per year depends on the /DinY code above.
#' We have all the GDP per cap values and per cap availability for 2005 and 2010. Above we average over 2004-6 for the base year( given by the variable FBSyearsToAverage.baseyear)
#' and 2009-11 for the start year; the year after which the availability values are estimated.
#' So we need to calculate availabiliity going forward, stepwise. Using the arc elasticity formula
# arc income elasticity elasInc = [(Qn - Qn-1)/(Qn + Qn-1)/2] / [(Yn - Yn-1) /(Yn + Yn-1)/2)]
# [(Qn - Qn-1)/(Qn + Qn-1)] = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# let x = elasInc * [(Yn - Yn-1) /(Yn + Yn-1))]
# x * (Qn + Qn-1) = (Qn - Qn-1)
# x * Qn + x* Qn-1 + Qn-1 = Qn
# x* Qn - Qn + x* Qn-1 + Qn-1 = 0
# Qn * (x - 1) + Qn-1 * (x + 1) = 0
# Qn * (x - 1) = - Qn-1 * (x + 1)
#' this is the formula that does the stepwise estimation of availability
# Qn = - Qn-1 (1 + x)/(x - 1) = Qn-1 (1+x)/(1-x)

# variables names equivalent to the variables above
# delta.GDP = Yn - Yn-1
# sum.GDP = Yn + Yn-1
# GDPRatio := delta.GDP/(value.GDP + GDP.lag1) = delta.GDP/sum.GDP = (Yn - Yn-1)/(Yn + Yn-1)
# (x) := get(elasNames) * GDPRatio = x
# (xRatio) := get(1 + x)/(x - 1)

# Combine GDP and FBS data
formula.wide <- "region_code.IMPACT159  + year ~ IMPACT_code"
dt.FBS.wide <- data.table::dcast(
  data = dt.FBS,
  formula = formula.wide,
  value.var = "value")
# remove X2010 data from dt.FBS because only the X2005 data are used for projection. Not currently being done, July 25, 2018
dt.GDPFBS <- merge(dt.GDPperCap, dt.FBS.wide, by = c("region_code.IMPACT159", "year"), allow.cartesian=TRUE, all.x = TRUE) # has NAs for future estimated values

# add income elasticity data to the GDP and FBS data
dt.GDPFBSelas <- merge(dt.GDPFBS, dt.incElas.fishnalc, by = c("region_code.IMPACT159", "year"), allow.cartesian=TRUE)
# after the line above there are many duplicates. This may be because of the allow.cartesian.
# It may have also been because of dups in dt.incElas.fishnalc, removed starting Oct 29, 2019
dt.GDPFBSelas <- unique(dt.GDPFBSelas)

# create columns to hold intermediate results
elasNames <- paste(fishNalcNames,"elas", sep = ".")
temp.inc <- paste0("temp.inc.", fishNalcNames)
x <- paste0("x.", fishNalcNames)
xRatio <- paste0("xRatio.", fishNalcNames)
# xRatio.denom <- paste0("xRatio.denom.", fishNalcNames)
# xRatio.numer <- paste0("xRatio.numer.", fishNalcNames)
Qn <- fishNalcNames
Qn.test <- paste0("test.", fishNalcNames)
# calculate x
dt.GDPFBSelas[, (x) := lapply(.SD, "*", GDPRatio), .SDcols = (elasNames)]

# calculate xRatio
f.xRatio <- function(xIn) {return((1 + xIn)/(1 - xIn))}
dt.GDPFBSelas[, (xRatio) := lapply(.SD, FUN = f.xRatio), .SDcols = (x)]

keepListCol <- c("scenario", "region_code.IMPACT159", "year", Qn, xRatio)
dt.GDPFBSelas[, setdiff(names(dt.GDPFBSelas), (keepListCol)) := NULL]

# DT <- copy(dt.GDPFBSelas)
# DT[is.na(get(xRatio)), (xRatio) := 1]

# see https://stackoverflow.com/questions/50082978/r-how-to-use-mapply-with-a-data-table-and-two-lists-of-column-names?noredirect=1#comment87182401_50082978

f.cumprod <- function(x, y)  cumprod(c(x[1], y[-1]))
dt.GDPFBSelas[, (Qn) := Map(f.cumprod,  mget(Qn), mget(xRatio)), by = .(region_code.IMPACT159, scenario)]

keepListCol <- c("scenario", "region_code.IMPACT159", "year", fishNalcNames)
dt.GDPFBSelas[, setdiff(names(dt.GDPFBSelas), keepListCol) := NULL]

# dt.GDPFBSelas units are kgs per person per day. To align with dt.foodAvailability they need to be per year.
daysinyear <- keyVariable("DinY")
dt.GDPFBSelas[, (fishNalcNames) := lapply(.SD, "*", daysinyear), .SDcols = (fishNalcNames)]
# some countries have no consumption of the fish or alcoholic beverage items
dt.GDPFBSelas[, (names(dt.GDPFBSelas)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.GDPFBSelas)]

idVarsFishnAlc <- c("scenario", "region_code.IMPACT159", "year")
#' #' get the names of the fish and alcoholic beverages that are included in dt.fishScenario
measureVarsFishnAlc <- names(dt.GDPFBSelas)[!names(dt.GDPFBSelas) %in% idVarsFishnAlc]
dt.GDPFBSelas.melt <- data.table::melt(dt.GDPFBSelas,
                                       id.vars = idVarsFishnAlc,
                                       variable.name = "IMPACT_code",
                                       measure.vars = measureVarsFishnAlc,
                                       value.name = "FoodAvailability",
                                       variable.factor = FALSE)

dt.GDPFBSelas.melt <- unique(dt.GDPFBSelas.melt) # added May 15, 2018, not sure why it is needed.
keepYearList <- keyVariable("keepYearList") # get original list
inDT <- dt.GDPFBSelas.melt[year %in% keepYearList]
outName <- "dt.fishnAlcScenarios"
desc <- "Scenarios of fish and alcoholic beverages availability by fish composite and country. Average availability, kgs per person per year"
cleanup(inDT,outName, fileloc("mData"), desc = desc) # changed to mData Oct 30, 2018

# testing of data
# dt.GDPFBSelas.melt[, fishCons := rowSums(.SD), by = c("scenario", "region_code.IMPACT159", "year", "IMPACT_code")]



