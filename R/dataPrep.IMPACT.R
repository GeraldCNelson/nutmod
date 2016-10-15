#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

# this script needs to be separate because shiny can't deal with the gams package.
source("R/gdxrrwSetup.R")
#source("R/gdxrrfunctions.R")
# Intro -------------------------------------------------------------------

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

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

#' Title getGDXmetaData - get gdxmetadata from an IMPACT gdx output file and write out
#' rds and excel versions#'
#' @param gamsDir - path to the gdx library
#' @param IMPACTgdx - name of the gdx file
#' @return null
#' @export

# choose gdx file
print("Choose the IMPACT data gdx file you want to use/")
print("1. for the nutrient modeling paper")
print("2. for the USAID nutrient modeling paper")
print("note the relevant gdx file must be in the data-raw/IMPACTdata directory")

choice <- readline("Choose the number of the gdx file you want to use. ")
if (choice == "1") gdxFileName <- "Micronutrient-Inputs-07252016.gdx" #- gdx with multiple SSP results
if (choice == "2") gdxFileName <- "Micronutrient-Inputs-USAID.gdx"  #-  gdx for the USAID results
#IMPACTgdxfileName <- "Demand Results20150817.gdx" - old gdx
#gdxFileName <- fileNameList("IMPACTgdxfileName")
#gamsSetup() # to load GAMs stuff and create the initial list of IMPACT scenarios
gamsSetup(gdxFileName)
getGDXmetaData <- function(gdxFileName) {
  #  R_GAMS_SYSDIR <-  gamsDir
  #  gdxrrw::igdx(gamsSysDir = R_GAMS_SYSDIR) maybe not needed because done in gamsSetup
  # read in the gdx information to temp
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),gdxFileName, sep = "/")
  temp <- gdxrrw::gdxInfo(
    gdxName = gdxFileLoc, dump = FALSE, returnList = FALSE, returnDF = TRUE)

  # convert to data table and extract just the list of parameters
  dt.gdx.param <- data.table::as.data.table(temp$parameters)
  #  keepListCol <- c("catNames", "text") # remove index, dim, card, doms, and domnames
  deleteListCol <- c("index", "card","doms", "domnames")
  dt.gdx.param <- dt.gdx.param[, (deleteListCol) := NULL]

  data.table::setnames(dt.gdx.param,old = c("name","text"), new = c("catNames", "description"))
  inDT <- dt.gdx.param
  outName <- "dt.IMPACTgdxParams"
  cleanup(inDT,outName,fileloc("iData"))
}
getGDXmetaData(gdxFileName)
#' Title processIMPACT159Data - read in from an IMPACT gdx file and write out rds and excel files for a single param

#' @param gdxFileName - name of the IMPACT gdx file
#' @param varName - name of the IMPACT parameter to write out
#' @param catNames - types of info about the parameter
#' @return null
#' @export
#'
processIMPACT159Data <- function(gdxFileName, varName, catNames) {
  #  dt.regions.all <- getNewestVersion("dt.regions.all")
  # IMPACTgdx <- gdxFileName
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),gdxFileName, sep = "/")
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, varName,
                                                           ts = TRUE, names = catNames))
  keepYearList  <- keyVariable("keepYearList")
  #  dt.temp <- dt.regions.all[,c("region_code.IMPACT159","region_name.IMPACT159"), with = FALSE]
  # data.table::setkey(dt.temp,region_code.IMPACT159)
  # dt.IMPACTregions <- unique(dt.temp)
  # if the data set contains SDN (the old Sudan) data, convert the code to SDP
  if (!varName %in% "PWX0") {
    dt.ptemp[region_code.IMPACT159 == "SDN", region_code.IMPACT159 := "SDP"]
  }
  dt.ptemp[,year := paste("X",year, sep = "")]
  dt.ptemp <- dt.ptemp[year %in% keepYearList]
  dt.ptemp <- data.table::as.data.table(rapply(dt.ptemp, as.character, classes = "factor", how = "replace"))
  #setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT159, year)
  data.table::setorderv(dt.ptemp, cols = catNames)
  data.table::setnames(dt.ptemp,"value",varName)
  # this if statement keeps the region code and name from being added since PW is only for the world
  # if (!varName == "PWX0") {
  #   data.table::setkey(dt.ptemp, region_code.IMPACT159)
  #   dt.temp <-
  #     merge(dt.ptemp, dt.IMPACTregions, by = "region_code.IMPACT159", all = TRUE)
  # }

  #write scenario names
dt.ptemp <- cleanupScenarioNames(dt.ptemp)
  dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
  scenarioListIMPACT <- dt.scenarioListIMPACT$scenario
  scenarioComponents <- c("SSP", "climate_model", "experiment")
  suppressWarnings(
    dt.scenarioListIMPACT[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
  )
  # the code above recyles so you end up with the SSP value in experiment if this is a REF scenario
  # the code below detects this and replaces the SSP value with REF
  dt.scenarioListIMPACT[(SSP == experiment), experiment := "REF"]
  dt.scenarioListIMPACT[, scenarioNew := paste(SSP, climate_model, experiment, sep = "-")]
  dt.ptemp <- merge(dt.ptemp, dt.scenarioListIMPACT, by = "scenario")
  deleteListCol <- c("SSP", "climate_model", "experiment", "scenario")
  dt.ptemp[, (deleteListCol) := NULL]
  data.table::setnames(dt.ptemp, old = c("scenarioNew"), new = c("scenario"))
  leadingCols <- c("scenario")
  lagingCols <-  laggingCols <- names(dt.ptemp)[!names(dt.ptemp) %in% leadingCols]
  data.table::setcolorder(dt.ptemp, c(leadingCols, laggingCols))
  inDT <- dt.ptemp
  # this is where dt.FoodAvailability is written out, for example
  outName <- paste("dt", varName, sep = ".")
  cleanup(inDT,outName,fileloc("iData"))

  # write.csv(scenarioListIMPACT$scenarioNew, file = paste(fileloc("mData"),"scenarioListIMPACT.csv",
  #                                                     sep = "/"), row.names = FALSE)
}

#' Title generateResults - send a list of variable with common categories to the
#' function to write out the data
#'
#' @param vars - list of variables to process
#' @param catNames - list of categories common to all variables
#'
#' @return
#' @export
generateResults <- function(gdxFileName, vars,catNames){
  for (i in vars) {
    processIMPACT159Data(gdxFileName,i, catNames)
  }
}

vars.land <- c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <- c("scenario","IMPACT_code","region_code.IMPACT159","landUse","year","value")
vars.commods <-
  c("PCX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0","PerCapKCAL_com","FoodAvailability")
catNames.commod <- c("scenario","IMPACT_code","region_code.IMPACT159","year","value")
vars.region <- c("GDPX0", "pcGDPX0",  "TotalMalnourished",
                 "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")
catNames.region <- c("scenario","region_code.IMPACT159","year","value")

#' @param worldVars - parameters for data at the world level
vars.world <- "PWX0"
catNames.world <- c("scenario", "IMPACT_code", "year", "value")

# comment out lines below to speed up data crunching.
# generateResults(vars.land,catNames.land)
generateResults(gdxFileName, vars.commods, catNames.commod)
generateResults(gdxFileName, vars.region, catNames.region)
generateResults(gdxFileName, vars.world, catNames.world)

#' @param dt.CSEs - data table with consumer surplus equivalents
CSEs <- fileNameList("CSEs")

dt.CSEs <- data.table::as.data.table(
  openxlsx::read.xlsx(CSEs,cols = c(1:3)))
data.table::setnames(dt.CSEs, old = c("CTY","C","CSE"), new = c("region_code.IMPACT159","IMPACT_code","CSE"))
data.table::set(dt.CSEs, which(is.na(dt.CSEs[["CSE"]])), "CSE", 0)
data.table::setorder(dt.CSEs, region_code.IMPACT159, IMPACT_code)
# add years to the CSE file, because it currently doesn't have any
dt.years <- data.table::data.table(year = rep(keyVariable("keepYearList"), each = nrow(dt.CSEs)))
dt.CSEs <- cbind(dt.years, dt.CSEs)
inDT <- dt.CSEs
outName <- "dt.CSEs"
cleanup(inDT,outName,fileloc("iData"))


