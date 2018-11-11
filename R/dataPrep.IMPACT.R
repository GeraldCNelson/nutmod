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

#' @description This script reads in the IMPACT data from a gdx file and writes out selected variables to a .rds file.
#' The gdxrrw package is needed to run this. It is available at this url, not from CRAN.
#' @source \url{https://support.gams.com/gdxrrw:interfacing_gams_and_r}
#' Download the relevant file and use the following command to install
#' install.packages("gdxrrw_1.0.4.tgz",repos=NULL). Replace gdxrrw_1.0.4.tgz with the
#' name of the file you downloaded. If you put it in the main directory of your project,
#' the install.packages command will find it.
#' @import gdxrrw

#' Title importIMPACT - Import data from the IMPACT model and write out rds and excel files
#' @description Read IMPACT159 data from a gdx file
#'
#' @return dt.temp
#' @export

# one approach to ask the user what it is. The other is to read from the metadata output.
# if (!exists("gdxFileName")) source("R/gdxrrwSetup.R")
if (!exists("gdxFileName")) {
  dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
  gdxFileName <- dt.metadata[file_description %in% "IMPACT demand data in gdx form", file_name_location]
}
gdxFileLoc <- paste(fileloc("IMPACTRawData"),gdxFileName, sep = "/")
gdxChoice <- getGdxChoice()
#' Title generateResults - send a list of variables with common categories to the
#' function to write out the data
#' @param vars - list of variables to process
#' @param catNames - list of categories common to all variables in var
#'
#' @return
#' @export

#' Title processIMPACT159Data - read in from an IMPACT gdx file and write out rds and excel files for a single param
#' @param gdxFileName - name of the IMPACT gdx file
#' @param varName - name of the IMPACT parameter to write out
#' @param catNames - types of info about the parameter
#' @return null
#' @export

if (gdxChoice %in% "AfricanAgFutures") { # needed to clean up the scenario names in the gdx
  dt.scenariosLookup  <- as.data.table(read_excel("data-raw/AfricanAgFutures/scenlookupAfrAgFutures.xlsx")) 
  dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
}
keepYearList <- keyVariable("keepYearList")

processIMPACT159Data <- function(gdxFileName, varName, catNames) {
  # dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
  # IMPACTgdx <- gdxFileName
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, varName,
                                                           ts = TRUE, names = catNames))
  dt.ptemp <- data.table::as.data.table(rapply(dt.ptemp, as.character, classes = "factor", how = "replace"))
  
  if (gdxChoice %in% "SSPs") {
    dt.ptemp[scenario %in% c("SSP1-NoCC", "SSP2-GFDL", "SSP2-HGEM","SSP2-HGEM2", "SSP2-IPSL", "SSP2-IPSL2",
                             "SSP2-MIROC", "SSP2-NoCC", "SSP3-NoCC"),
             scenario := paste(scenario, "-REF", sep = "")]
  }
  if (gdxChoice %in% "USAIDPriorities") {
    dt.ptemp[, crop := tstrsplit(scenario, "-", fixed = TRUE, keep = c(3))]
    SSPName <- "SSP2"
    climModel <- "HGEM"
    dt.ptemp[, scenario := paste(SSPName, climModel, paste0("c", crop), sep = "-")]
    dt.ptemp[, crop := NULL]
    # SSPName <- "SSP2"
    # climModel <- "HGEM"
    # dt.ptemp[, scenario := paste(SSPName, climModel, paste0("c", tstrsplit(scenario, "-", fixed = TRUE, keep = c(3))), sep = "-")]
    # dt.ptemp[, crop := NULL]
  }
  
  if (gdxChoice %in% "AfricanAgFutures") { # cleanup the scenario names
    for (i in 1:nrow(dt.scenariosLookup)) {
      dt.ptemp <- dt.ptemp[scenario %in% dt.scenariosLookup$basicNames[i], scenario := dt.scenariosLookup$substantiveNames[i]]
    }
    dt.ptemp <- dt.ptemp[!scenario %in% c("SSP3Afr_base_CC", "SSP1Afr_base_CC")]
  }
  # dt.temp <- dt.regions.all[,c("region_code.IMPACT159","region_name.IMPACT159"), with = FALSE]
  # data.table::setkey(dt.temp,region_code.IMPACT159)
  # dt.IMPACTregions <- unique(dt.temp)
  # if the data set contains SDN (the old Sudan) data, convert the code to SDP
  if (!varName %in% "PWX0") {
    # this kludge is here because the currently used gdx files have both SDN and SDP
    dt.ptemp[region_code.IMPACT159 == "SDN", region_code.IMPACT159 := "SDP"]
  }
  dt.ptemp[,year := paste("X",year, sep = "")]
  dt.ptemp <- dt.ptemp[year %in% c("X2005", keepYearList)] # adding x2005 here to keep the 2005 data for the fishnalc calculations
  #setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT159, year)
  data.table::setorderv(dt.ptemp, cols = catNames)
  data.table::setnames(dt.ptemp, "value", varName)
  # this if statement keeps the region code and name from being added since PW is only for the world
  # if (!varName == "PWX0") {
  # data.table::setkey(dt.ptemp, region_code.IMPACT159)
  # dt.temp <-
  # merge(dt.ptemp, dt.IMPACTregions, by = "region_code.IMPACT159", all = TRUE)
  # }
  inDT <- dt.ptemp
  inDT <- inDT[, scenario := gsub("-", "_", scenario)]
  inDT <- inDT[, scenario := gsub("_REF", "", scenario)]
  
  # this is where dt.FoodAvailability is written out, for example
  outName <- paste("dt", varName, sep = ".")
  desc <- paste("Individual IMPACT gdx variable", varName)
  cleanup(inDT,outName, fileloc("iData"), desc = desc)
}

vars.land <- c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "landUse", "year", "value")
vars.commods <-
  c("PCX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0", "PerCapKCAL_com", "FoodAvailability")
catNames.commod <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "year", "value")
vars.region <- c("GDPX0", "pcGDPX0", "TotalMalnourished",
                 "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")
catNames.region <- c("scenario", "region_code.IMPACT159", "year", "value")

#' @param worldVars - parameters for data at the world level
vars.world <- "PWX0"
catNames.world <- c("scenario", "IMPACT_code", "year", "value")

generateResults <- function(gdxFileName, vars, catNames){
  for (i in vars) {
    processIMPACT159Data(gdxFileName, varName = i, catNames = catNames)
  }
}

# comment out lines below to speed up data crunching.
generateResults(gdxFileName, vars = vars.land, catNames = catNames.land)
generateResults(gdxFileName, vars = vars.commods, catNames = catNames.commod)
generateResults(gdxFileName, vars.region, catNames = catNames.region)
generateResults(gdxFileName, vars.world, catNames = catNames.world)

#' @param dt.CSEs - data table with consumer surplus equivalents
#CSEs <- fileNameList("CSEs")

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


finalizeScriptMetadata(metadataDT, sourceFile)
sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
