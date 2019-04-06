#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
source("R/nutrientModFunctions.R")
library(readxl)
gdxrrw::igdx(gamsSysDir = fileNameList("R_GAMS_SYSDIR"), silent = TRUE)
sourceFile <- "dataPrep.IMPACT.R"
createScriptMetaData()

# Intro -------------------------------------------------------------------

#Copyright (C) 2015-2018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

#' \description{
#' This script reads in the IMPACT data from a gdx file and writes out selected variables to a .rds file.
#' }
description <- "This script reads in the IMPACT data from a gdx file and writes out selected variables to a .rds file."
# The gdxrrw package is needed to run this. It is available at this url, not from CRAN.
# @source \url{https://support.gams.com/gdxrrw:interfacing_gams_and_r}
# Download the relevant file and use the following command to install
# install.packages("gdxrrw_1.0.4.tgz",repos=NULL). Replace gdxrrw_1.0.4.tgz with the
# name of the file you downloaded. If you put it in the main directory of your project,
# the install.packages command will find it.

#' Title importIMPACT - Import data from the IMPACT model and write out rds and excel files
#' @description Read IMPACT159 data from a gdx file
#'
#' @return dt.temp
#' @export
singleScenario <- FALSE # do the regular IMPACT variables first
gdxFileName <- "Micronutrient-Inputs-07252016.gdx"
gdxFileLoc <- paste(fileloc("IMPACTRawData"), gdxFileName, sep = "/")
gdxChoice <- "SSPs"
keepYearList <- keyVariable("keepYearList")

#' Title generateResults - send a list of variables with common categories to the
#' function to write out the data
#' @param vars - list of variables to process
#' @param catNames - list of categories common to all variables in var
#'
#' @return generate results
#' @export

#' Title processIMPACT159Data - read in from an IMPACT gdx file and write out rds and excel files for a single param
#' @param gdxFileName - name of the IMPACT gdx file
#' @param varName - name of the IMPACT parameter to write out
#' @param catNames - types of info about the parameter
#' @return null
#' @export

processIMPACT159Data <- function(gdxFileLoc, varName, catNames, singleScenario, keepYearList) {
  # IMPACTgdx <- gdxFileName
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, varName,
                                                 ts = TRUE, names = catNames))
  print(gdxFileLoc)
  print(varName)
  print(head(dt.ptemp))
  print(names(dt.ptemp))
  dt.ptemp <-
    data.table::as.data.table(rapply(dt.ptemp, as.character, classes = "factor", how = "replace"))
  
   # if the data set contains SDN (the old Sudan) data, convert the code to SDP
  if (!varName %in% "PWX0") {
    # this kludge is here because the currently used gdx files have both SDN and SDP
    dt.ptemp[region_code.IMPACT159 == "SDN", region_code.IMPACT159 := "SDP"]
  }
  dt.ptemp[, year := paste0("X", year)]
  dt.ptemp <-
    dt.ptemp[year %in% c("X2005", keepYearList)] # adding x2005 here to keep the 2005 data for the fishnalc calculations
  data.table::setorderv(dt.ptemp, cols = catNames)
  data.table::setnames(dt.ptemp, "value", varName)
  # this if statement keeps the region code and name from being added since PW is only for the world
  # if (!varName == "PWX0") {
  # data.table::setkey(dt.ptemp, region_code.IMPACT159)
  # dt.temp <-
  # merge(dt.ptemp, dt.IMPACTregions, by = "region_code.IMPACT159", all = TRUE)
  # }
  inDT <- dt.ptemp
  if (singleScenario == FALSE) inDT <- inDT[, scenario := gsub("-", "_", scenario)]
  
  # this is where dt.FoodAvailability is written out, for example
  outName <- paste("dt", varName, sep = ".")
  #  cleanup(inDT,outName, fileloc("iData"), desc = desc)
  if (singleScenario == TRUE) {
#    outName <- paste("dt", varName, sep = ".")
    outName <- paste("dt", varName, gsub(".gdx", "", gdxFileName), sep = ".")
    
    desc <- paste("Individual IMPACT gdx variable", varName)
    fileName.out <- paste0(getwd(), "/",fileloc("iData"), "CGEPEcompare/", outName )
    print(fileName.out)
    saveRDS(inDT, file = fileName.out) # this won't write out the description
  }
  if (!singleScenario == TRUE) {
    desc <- paste("Individual IMPACT gdx variable", varName)
    fileName.out <- paste0(getwd(), "/", fileloc("iData"), outName )
    saveRDS(inDT, file = fileName.out) # this won't write out the description
  }
}

vars.land <- c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <-
  c("scenario",
    "IMPACT_code",
    "region_code.IMPACT159",
    "landUse",
    "year",
    "value")

vars.commods <-
  c("PCX0",
    # "QSX0",
    # "QSUPX0",
    # "QDX0",
    # "QFX0",
    # "QBFX0",
    # "QLX0",
    # "QINTX0",
    # "QOTHRX0",
    # "QEX0",
    # "QMX0",
    # "PerCapKCAL_com",
    "FoodAvailability"
  )

catNames.commod.CGEresults <- c("IMPACT_code", "region_code.IMPACT159", "year", "value")
catNames.commod <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "year", "value")
vars.region <- c("pcGDPX0", "PopX0")
# not used for now "TotalMalnourished", "PerCapKCAL",  "ShareAtRisk", "PopulationAtRisk", "GDPX0"
catNames.region <- c("scenario", "region_code.IMPACT159", "year", "value")
catNames.region.CGEresults <- c("region_code.IMPACT159", "year", "value")

#' @param worldVars - parameters for data at the world level
vars.world <- "PWX0"
catNames.world <- c("scenario", "IMPACT_code", "year", "value")

generateResults <- function(gdxFileLoc, vars, catNames,  singleScenario, keepYearList) {
  for (i in vars) {
    processIMPACT159Data(gdxFileLoc, varName = i, catNames = catNames, singleScenario, keepYearList)
  }
}

# comment out lines below to speed up data crunching.
#generateResults(gdxFileName, vars = vars.land, catNames = catNames.land)
gdxFileLoc <- paste(fileloc("IMPACTRawData"), gdxFileName, sep = "/")
generateResults(gdxFileLoc, vars = vars.commods, catNames = catNames.commod, singleScenario, keepYearList)
generateResults(gdxFileLoc, vars = vars.region, catNames = catNames.region, singleScenario, keepYearList)
generateResults(gdxFileLoc, vars = vars.world, catNames = catNames.world, singleScenario, keepYearList)

#write out files from the CGE compare process
singleScenario <- TRUE
for (gdxFileName in c("SSP2-HGEM2-WithGLOBE.gdx", "SSP2-HGEM-WithoutGLOBE.gdx")) {
  catNames.commod.CGEresults <- c("IMPACT_code", "region_code.IMPACT159", "year", "value")
  gdxFileLoc <- paste(fileloc("IMPACTRawData"), gdxFileName, sep = "/")
  generateResults(gdxFileLoc, vars = vars.commods, catNames = catNames.commod.CGEresults, singleScenario, keepYearList)
  generateResults(gdxFileLoc, vars = vars.region,  catNames = catNames.region.CGEresults, singleScenario, keepYearList)
  # generateResults(gdxFileLoc, vars.world, catNames.world)
}

finalizeScriptMetadata(metadataDT, sourceFile)
# sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
