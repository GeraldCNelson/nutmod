#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function")) {source("R/nutrientModFunctions.R")}

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

#' @description read in the IMPACT data from a gdx file and prepare for analysis.
#' The gdxrrw package is needed to run this. It is available at this url, not from CRAN.
#' @source \url{https://support.gams.com/gdxrrw:interfacing_gams_and_r}
#' Download the relevant file and use the following command to install
#' install.packages("gdxrrw_1.0.0.tgz",repos=NULL). Replace gdxrrw_1.0.0.tgz with the
#' name of the file you downloaded. If you put it in the main directory of your project,
#' the install.packages command will find it.
#' @import gdxrrw

# needed at the moment because I can't install the gdxrrw file to the nutmod project directory
#packrat::opts$external.packages("gdxrrw")
#' Title importIMPACT - Import data from the IMPACT model and write out rds and excel files
#' @description Read IMPACT3 data from a gdx file
#'
#' @return dt.temp
#' @export

#' Title getGDXmetaData - get gdxmetadata from an IMPACT gdx output file and write out
#' rds and excel versions#'
#' @param gamsDir - path to the gdx library
#' @param IMPACTgdx - name of the gdx file
#' @return null
#' @export
getGDXmetaData <- function(gamsDir,IMPACTgdx) {
  R_GAMS_SYSDIR <-  gamsDir
  gdxrrw::igdx(gamsSysDir = R_GAMS_SYSDIR)
  # read in the gdx information to temp
  temp <-
    gdxrrw::gdxInfo(
      gdxName = IMPACTgdx,
      dump = FALSE,
      returnList = FALSE,
      returnDF = TRUE
    )
  # convert to data table and extract just the list of parameters
  dt.gdx.param <- data.table::as.data.table(temp$parameters)
  keepListCol <-
    c("name", "text") # remove index, dim, card, doms, and domnames
  dt.gdx.param <- dt.gdx.param[, keepListCol, with = FALSE]

  data.table::setnames(dt.gdx.param,old = c("name","text"), new = c("catNames","description"))
  inDT <- dt.gdx.param
  outName <- "dt.IMPACTmetaData"
  cleanup(inDT,outName,fileloc("iData"))
}
getGDXmetaData(fileNameList("R_GAMS_SYSDIR"),fileNameList("IMPACTgdx"))

#' Title processIMPACT3Data - read in from the IMPACT gdx file and write out rds and excel files for a single param

#' @param gdxFileName - name of the IMPACT gdx file
#' @param varName - name of the IMPACT parameter to write out
#' @param catNames - types of info about the parameter
#' @return null
#' @export
#'
processIMPACT3Data <- function(gdxFileName, varName, catNames) {
  df.regions.all <- getNewestVersion("df.regions.all")
  IMPACTgdx <- gdxFileName
#  keepYearList  <- keyVariable("keepYearList")
  dt.temp <- data.table::as.data.table(df.regions.all[,c("region_code.IMPACT3","region_name.IMPACT3")])
  data.table::setkey(dt.temp,region_code.IMPACT3)
  dt.IMPACTregions <- unique(dt.temp)
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(IMPACTgdx, varName,
                                                           ts = TRUE, names = catNames))
  dt.ptemp[, year := paste("X", dt.ptemp$year, sep = "")]
  #dt.ptemp <- dt.ptemp[year %in% keepYearList]
  dt.ptemp <- data.table::as.data.table(rapply(dt.ptemp, as.character, classes = "factor", how = "replace"))
  #setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT3, year)
  data.table::setorderv(dt.ptemp, cols = catNames)
  data.table::setnames(dt.ptemp,"value",varName)
  # this if statement keeps the region code and name from being added since PW is only for the world
  if (!varName == "PWX0") {
    data.table::setkey(dt.ptemp, region_code.IMPACT3)
    dt.temp <-
      merge(dt.ptemp, dt.IMPACTregions, by = "region_code.IMPACT3", all = TRUE)
    # set the region code of South Sudan to the code for Sudan, if it has not already been done
    dt.ptemp[region_code.IMPACT3  == "SDN", region_code.IMPACT3 := "SDP"]
  }
  inDT <- dt.ptemp
  outName <- paste("dt",varName, sep=".")
  cleanup(inDT,outName,fileloc("iData"))
  # return(dt.temp)
}

#' Title generateResults - send a list of variable with common categories to the
#' function to write out the data
#'
#' @param vars - list of variables to process
#' @param catNames - list of categories common to all variables
#'
#' @return
#' @export
generateResults <- function (vars,catNames){
  #dtlist.land <- lapply(vars.land,processIMPACT3Data,catNames = catNames.land)
  for (i in vars){
    processIMPACT3Data(fileNameList("IMPACTgdx"),i, catNames)
  }
}
#' processIMPACT3Data(fileNameList("IMPACTgdx"),)

vars.land <- c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <- c("scenario","IMPACT_code","region_code.IMPACT3","landUse","year","value")

#' @param commodVars - scenario, region_code.IMPACT3, IMPACT_code,year, value
vars.commods <-
  c("PCX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0","PerCapKCAL_com","FoodAvailability")
catNames.commod <- c("scenario","IMPACT_code","region_code.IMPACT3","year","value")

#' @param regionVars - parameters included for data at the region (countries and country-aggregates) level
vars.region <-
  c("GDPX0", "pcGDPX0",  "TotalMalnourished",
    "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")
catNames.region <- c("scenario","region_code.IMPACT3","year","value")

#' @param worldVars - parameters for data at the world level
vars.world <- "PWX0"
catNames.world <- c("scenario","IMPACT_code","year","value")

generateResults(vars.land,catNames.land)
generateResults(vars.commods,catNames.commod)
generateResults(vars.region,catNames.region)
generateResults(vars.world,catNames.world)

#' @param dt.CSEs - data table with consumer surplus equivalents
CSEs <- fileNameList("CSEs")


dt.CSEs <- data.table::as.data.table(
  openxlsx::read.xlsx(CSEs,cols=c(1:3)))
data.table::setnames(dt.CSEs, old=c("CTY","C","CSE"), new=c("region_code.IMPACT3","IMPACT_code","CSE"))
data.table::set(dt.CSEs, which(is.na(dt.CSEs[["CSE"]])), "CSE", 0)
data.table::setorder(dt.CSEs, region_code.IMPACT3, IMPACT_code)
# add years to the CSE file, because it currently doesn't have any
dt.years <- data.table::data.table(year = rep(keyVariable("keepYearList"), each = nrow(dt.CSEs)))
dt.CSEs <- cbind(dt.years, dt.CSEs)
inDT <- dt.CSEs
outName <- "dt.CSEs"
cleanup(inDT,outName,fileloc("iData"))

