#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.SingleScenario.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
library(gdxrrw)
library(ggplot2)
gdxrrw::igdx(gamsSysDir = fileNameList("R_GAMS_SYSDIR"), silent = TRUE)

# Intro -------------------------------------------------------------------

#Copyright (C) 2015 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

#' @description This script reads in the IMPACT data from a gdx file and writes out selected variables to a .rds file.
#' The gdxrrw package is needed to run this. It is available at this url, not from CRAN.
#' @source \url{https://support.gams.com/gdxrrw:interfacing_gams_and_r}
#' Download the relevant file and use the following command to install
#' install.packages("gdxrrw_1.0.0.tgz",repos=NULL). Replace gdxrrw_1.0.0.tgz with the
#' name of the file you downloaded. If you put it in the main directory of your project,
#' the install.packages command will find it.
#' @import gdxrrw

#' Title importIMPACT - Import data from the IMPACT model and write out rds and excel files
#' @description Read IMPACT159 data from a gdx file
#'
#' @return dt.temp
#' @export
#'

# #gdxFileName <- "SSP2-HGEM2-WithGLOBE.gdx"
# gdxFileName <- "SSP2-HGEM-WithoutGLOBE.gdx"
singleScenario <- TRUE

catList <- c("catNames.land", "catNames.commod", "catNames.region", "catNames.world")
vars.land <- c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "landUse", "year", "value")

vars.commods <-
  c("PCX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0", "PerCapKCAL_com", "FoodAvailability")
catNames.commod <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "year", "value")

vars.region <- c("GDPX0", "pcGDPX0", "TotalMalnourished",
                 "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")
catNames.region <- c("scenario", "region_code.IMPACT159", "year", "value")

vars.world <- "PWX0"
catNames.world <- c("scenario", "IMPACT_code", "year", "value")
keepYearList <- keyVariable("keepYearList")

# if (singleScenario == TRUE) {
#   for (i in catList) {
#     temp <- eval(parse(text = i))
#     temp <- temp[!temp %in% "scenario"]
#     assign(i, temp)
#   }
# }

# if (!exists("gdxFileName")) {
#   dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
#   gdxFileName <- dt.metadata[file_description %in% "IMPACT demand data in gdx form", file_name_location]
# }

#' Title generateResults - send a list of variables with common categories to the
#' function to write out the data
#' @param vars - list of variables to process
#' @param catNames - list of categories common to all variables in var
#'
#' @return
#' @export
generateResults <- function(gdxFileName, gdxFileLoc, vars, catNames, singleScenario, keepYearList){
  for (i in vars) {
    catNames <- catNames[!catNames %in% "scenario"]
    processIMPACT159Data(gdxFileName, gdxFileLoc, varName = i, catNames = catNames, singleScenario, keepYearList)
  }
}

#' Title processIMPACT159Data - read in from an IMPACT gdx file and write out rds and excel files for a single param
#' @param gdxFileName - name of the IMPACT gdx file
#' @param varName - name of the IMPACT parameter to write out
#' @param catNames - types of info about the parameter
#' @return null
#' @export
#'
processIMPACT159Data <- function(gdxFileName, gdxFileLoc, varName, catNames, singleScenario, keepYearList) {
  # dt.regions.all <- getNewestVersion("dt.regions.all")
  # IMPACTgdx <- gdxFileName
  dt <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, varName,
                                                     ts = TRUE, names = catNames))
  dt <- data.table::as.data.table(rapply(dt, as.character, classes = "factor", how = "replace"))

  if(!singleScenario == TRUE) {
    dt[scenario %in% c("SSP1-NoCC", "SSP2-GFDL", "SSP2-HGEM","SSP2-HGEM2", "SSP2-IPSL", "SSP2-IPSL2",
                       "SSP2-MIROC", "SSP2-NoCC", "SSP3-NoCC"),
       scenario := paste(scenario, "-REF", sep = "")]
  }

  # dt.temp <- dt.regions.all[,c("region_code.IMPACT159","region_name.IMPACT159"), with = FALSE]
  # data.table::setkey(dt.temp,region_code.IMPACT159)
  # dt.IMPACTregions <- unique(dt.temp)
  # if the data set contains SDN (the old Sudan) data, convert the code to SDP
  if (!varName %in% "PWX0") {
    # this kludge is here because the currently used gdx files have both SDN and SDP
    dt[region_code.IMPACT159 == "SDN", region_code.IMPACT159 := "SDP"]
  }
  dt[,year := paste("X",year, sep = "")]
  dt <- dt[year %in% keepYearList]
  #setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT159, year)
  data.table::setorderv(dt, cols = catNames)
  data.table::setnames(dt, "value", varName)
  # this if statement keeps the region code and name from being added since PW is only for the world
  # if (!varName == "PWX0") {
  # data.table::setkey(dt, region_code.IMPACT159)
  # dt.temp <-
  # merge(dt, dt.IMPACTregions, by = "region_code.IMPACT159", all = TRUE)
  # }
  inDT <- dt
  # this is where dt.FoodAvailability is written out, for example
  outName <- paste("dt", varName, gsub(".gdx", "", gdxFileName), sep = ".")
  if (singleScenario == TRUE) {
    fileDest <- "data/IMPACTData/singleScenario"
    cleanup(inDT,outName,fileDest)
  }else{
    cleanup(inDT,outName,fileloc("iData"))
  }
}

for (i in c("SSP2-HGEM2-WithGLOBE.gdx", "SSP2-HGEM-WithoutGLOBE.gdx")) {

  # comment out lines below to speed up data crunching.
  # generateResults(vars.land,catNames.land)
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),i, sep = "/")
  generateResults(i, gdxFileLoc, vars = vars.commods, catNames = catNames.commod, singleScenario, keepYearList)
  generateResults(i, gdxFileLoc, vars = vars.region,  catNames = catNames.region, singleScenario, keepYearList)
  # generateResults(gdxFileLoc, vars.world, catNames.world)
}

# dt.CSEs <- data.table::as.data.table(
# openxlsx::read.xlsx(CSEs,cols = c(1:3)))
# data.table::setnames(dt.CSEs, old = c("CTY","C","CSE"), new = c("region_code.IMPACT159","IMPACT_code","CSE"))
# data.table::set(dt.CSEs, which(is.na(dt.CSEs[["CSE"]])), "CSE", 0)
# data.table::setorder(dt.CSEs, region_code.IMPACT159, IMPACT_code)
# # add years to the CSE file, because it currently doesn't have any
# dt.years <- data.table::data.table(year = rep(keyVariable("keepYearList"), each = nrow(dt.CSEs)))
# dt.CSEs <- cbind(dt.years, dt.CSEs)
# inDT <- dt.CSEs
# outName <- "dt.CSEs"
# cleanup(inDT,outName,fileloc("iData"))

dt.FoodAvailability.woGlobe <- readRDS(file = "data/IMPACTData/singleScenario/dt.FoodAvailability.SSP2-HGEM-WithoutGLOBE_2017-12-31.rds")
dt.FoodAvailability.wGlobe <- readRDS(file = "data/IMPACTData/singleScenario/dt.FoodAvailability.SSP2-HGEM2-WithGLOBE_2017-12-31.rds")
dt.pcGDPX0.woGlobe <- readRDS(file = "data/IMPACTData/singleScenario/dt.pcGDPX0.SSP2-HGEM-WithoutGLOBE_2017-12-31.rds")
dt.pcGDPX0.wGlobe <- readRDS(file = "data/IMPACTData/singleScenario/dt.pcGDPX0.SSP2-HGEM2-WithGLOBE_2017-12-31.rds")
dt.PCX0.woGlobe <- readRDS(file = "data/IMPACTData/singleScenario/dt.PCX0.SSP2-HGEM-WithoutGLOBE_2017-12-31.rds")
dt.PCX0.wGlobe <- readRDS(file = "data/IMPACTData/singleScenario/dt.PCX0.SSP2-HGEM2-WithGLOBE_2017-12-31.rds")
setkey(dt.FoodAvailability.woGlobe)
setkey(dt.FoodAvailability.wGlobe)
setkey(dt.pcGDPX0.woGlobe)
setkey(dt.pcGDPX0.wGlobe)
setkey(dt.PCX0.woGlobe)
setkey(dt.PCX0.wGlobe)

# first do wo Globe
dt_woGlobe <- merge( dt.FoodAvailability.woGlobe, dt.pcGDPX0.woGlobe, by = c("region_code.IMPACT159", "year"))
dt_woGlobe <- merge(dt_woGlobe, dt.PCX0.woGlobe, by = c("region_code.IMPACT159", "IMPACT_code", "year"))
dt_woGlobe[, budget := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("region_code.IMPACT159", "year")]
dt_woGlobe[, incShare := 100 * budget / pcGDPX0 ]
namesToChange <- c("FoodAvailability", "pcGDPX0", "PCX0", "budget", "incShare")
setnames(dt_woGlobe, old = namesToChange, new = paste0(namesToChange, "_woGlobe"))

# now with Globe
dt_wGlobe <- merge( dt.FoodAvailability.wGlobe, dt.pcGDPX0.wGlobe, by = c("region_code.IMPACT159", "year"))
dt_wGlobe <- merge(dt_wGlobe, dt.PCX0.wGlobe, by = c("region_code.IMPACT159", "IMPACT_code", "year"))
dt_wGlobe[, budget := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("region_code.IMPACT159", "year")]
dt_wGlobe[, incShare := 100 * budget / pcGDPX0 ]
namesToChange <- c("FoodAvailability", "pcGDPX0", "PCX0", "budget", "incShare")
setnames(dt_wGlobe, old = namesToChange, new = paste0(namesToChange, "_wGlobe"))

dt <- merge(dt_woGlobe, dt_wGlobe, by = c("region_code.IMPACT159", "IMPACT_code", "year"))
write.csv(dt, file = "data/IMPACTData/singleScenario/combinedResults.csv")

dt <- as.data.table(read.csv(file = "data/IMPACTData/singleScenario/combinedResults.csv", stringsAsFactors = FALSE))
dt[, X := NULL] # get rid of row numbers
dt <- dt[!region_code.IMPACT159 %in% "SOM",]
dt.50 <- dt[year %in% "X2050",]
dt.50[, c("IMPACT_code", "FoodAvailability_woGlobe", "FoodAvailability_wGlobe", "PCX0_woGlobe", "PCX0_wGlobe","year") := NULL]
dt.50 <- dt.50[!duplicated(region_code.IMPACT159),]
#reorder the cols
setcolorder(dt.50, c("region_code.IMPACT159", "pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe",   "budget_wGlobe",  "incShare_woGlobe",  "incShare_wGlobe"))

dt.50.summary <- as.data.table(summary(dt.50))
dt.50.summary[, V1 := NULL]
dt.50.summary <- dt.50.summary[!V2 %in% "region_code.IMPACT159", ]
sumMeasures <- c("type", "value")
dt.50.summary <- dt.50.summary[, (sumMeasures) := data.table::tstrsplit(N, ":", fixed = TRUE)]
dt.50.summary[, N := NULL]

formula.wide <- "type ~ V2"
dt.50.summary.wide <- data.table::dcast(
  data = dt.50.summary,
  formula = formula.wide,
  value.var = "value")

#reorder the cols
setcolorder(dt.50.summary.wide, c("type", "pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe",   "budget_wGlobe",  "incShare_woGlobe",  "incShare_wGlobe"))

#reorder the rows
dt.50.summary.wide <- dt.50.summary.wide[c(6,1,4,5,2,3), ]

# low-income removal
# noSom <- temp2[!region_code.IMPACT159 %in% c("SOM", "BDI", "LBR", "CAF", "NER", "RWA"),]

# facet maps of deltas due to use of Globe
cat("\n Working on facet maps")
worldMap <- getNewestVersion("worldMap", fileloc("mData"))
DT <- dt.50
DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")

facetColName <- "food_group_code"

# availability in quantity terms 2050, no CC -----
legendText <- "Grams per day, 2050, \nno climate change"
fillLimits <- c(0, 500)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
palette <- myPalette(4)
#' middle two values shift the palette gradient; the code below give a smooth change
fillRange <- fillLimits[2] - fillLimits[1]
breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
fileName <- paste(gdxChoice, l, "facetmap", "FGAvail", "2050", "noCC", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

