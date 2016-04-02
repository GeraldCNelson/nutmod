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

IMPACTDataClean <- fileloc("IMPACTDataClean")

# needed at the moment because I can't install the gdxrrw file to the nutmod project directory
#packrat::opts$external.packages("gdxrrw")
#' Title importIMPACT
#' @description Read IMPACT3 data from a gdx file
#'
#' @param varName - variable name from the gdx file
#' @param catNames - category name from the gdx file
#' @param dt.gdx.param - data frame that holds the gdx parameters
#' @return dt.temp
#' @export
getGDXmetaData <- function() {
  R_GAMS_SYSDIR <-  fileNameList("R_GAMS_SYSDIR")
  IMPACTgdx <- fileNameList("IMPACTgdx")
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
  cleanupIMPACT(inDT,outName)
}
getGDXmetaData()

#' Title processIMPACT3Data
#' @param varName
#' @param catNames
#' @return dt.temp
#' @export
processIMPACT3Data <- function(varName, catNames) {
  df.regions.all <- getNewestVersion("df.regions.all")
  IMPACTgdx <- fileNameList("IMPACTgdx")
  keepYearList  <- keyVariable("keepYearList")
  dt.temp <- data.table::as.data.table(df.regions.all[,c("region_code.IMPACT3","region_name.IMPACT3")])
  data.table::setkey(dt.temp,region_code.IMPACT3)
  dt.IMPACTregions <- unique(dt.temp)
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(IMPACTgdx, varName,
                                                           ts = TRUE, names = catNames))
  dt.ptemp[, year := paste("X", dt.ptemp$year, sep = "")]
  dt.ptemp <- dt.ptemp[year %in% keepYearList]
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
  cleanupIMPACT(inDT,outName)
  # return(dt.temp)
}

#' @param landVars - scenario, region_code.IMPACT3, landUse,IMPACT_code,year, value
generateResults <- function (vars,catNames){
  #dtlist.land <- lapply(vars.land,processIMPACT3Data,catNames = catNames.land)
  for (i in vars){
    processIMPACT3Data(i, catNames)
  }
}

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
inDT <- dt.CSEs
outName <- "dt.CSEs"
cleanupIMPACT(inDT,outName)

#create separate data table just for food items
createFood <- function(fileShortName) {
  IMPACTfoodCommodList <- keyVariable("IMPACTfoodCommodList")
  dt.temp <- getNewestVersionIMPACT(fileShortName)
  dt.temp.food <- dt.temp[IMPACT_code %in% IMPACTfoodCommodList]
  inDT <- dt.temp.food
  outName <- paste(fileShortName,"food",sep=".")
  cleanupIMPACT(inDT,outName)
}

createFood("dt.PWX0")
createFood("dt.PCX0")
createFood("dt.CSEs")

#combine all relevant data tables for analysis
combineIMPACTData <- function() {
  dt.PWX0.food <- getNewestVersionIMPACT("dt.PWX0.food")
  dt.PCX0.food <- getNewestVersionIMPACT("dt.PCX0.food")
  dt.CSEs.food <- getNewestVersionIMPACT("dt.CSEs.food")
  dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
  dt.FoodAvailability <- getNewestVersionIMPACT("dt.FoodAvailability")

  data.table::setkey(dt.PWX0.food,        "scenario",                        "IMPACT_code")
  data.table::setkey(dt.CSEs.food,                    "region_code.IMPACT3", "IMPACT_code")
  data.table::setkey(dt.PCX0.food,        "scenario", "region_code.IMPACT3", "IMPACT_code")
  data.table::setkey(dt.pcGDPX0,          "scenario", "region_code.IMPACT3")
  data.table::setkey(dt.FoodAvailability, "scenario", "region_code.IMPACT3", "IMPACT_code")
  #dtlist <- list(dt.FoodAvailability, dt.PCX0.food,dt.CSEs.food,dt.PWX0.food,dt.pcGDPX0)
  dtlist <- list(dt.FoodAvailability,dt.pcGDPX0,dt.PCX0.food,dt.PWX0.food,dt.CSEs.food)
  #dt.IMPACTfood <- rbindlist(l = dtlist, use.names = TRUE, fill = TRUE)
  dt.IMPACTfood <- plyr::join_all(dtlist)
  #dt.IMPACTfood <-Reduce(mymerge,dtlist)

  # add alcohol
  dt.alcScenarios <- getNewestVersion("dt.alcScenarios")
  dt.alcScenarios[,pop:=NULL]

  idVarsAlc <- c("scenario","ISO_code","year")
  #get the names of the fish that are included in dt.fishScenario
  measureVarsAlc <- names(dt.alcScenarios)[!names(dt.alcScenarios) %in% idVarsAlc]
  measureVarsAlc <- IMPACTalcohol_code <- keyVariable("IMPACTalcohol_code")
  dt.alcScenarios.melt <- data.table::melt(dt.alcScenarios,
                                           id.vars = idVarsAlc,
                                           variable.name = "IMPACT_code",
                                           measure.vars = measureVarsAlc,
                                           value.name = "FoodAvailability",
                                           variable.factor = FALSE)
  dt.alcScenarios.melt<- merge(dt.alcScenarios.melt, df.regions.all, by = "ISO_code", all = TRUE)
  deleteListCol <- c("ISO_code","region_code.SSP","FAOSTAT_code","country_name.ISO", "region_code.IMPACT115",
                     "region_name.IMPACT115","region_code.IMPACTstandard","region_name.IMPACT3",
                     "region_name.IMPACTstandard","Short.name","Official.name",
                     "ISO2_code","UNI_code","UNDP_code","GAUL_code"
  )
  dt.alcScenarios.melt[,(deleteListCol) := NULL]
  #to get rid of the countries that are in IMPACT regions and so don't have separate data
  dt.alcScenarios.melt <- dt.alcScenarios.melt[!is.na(FoodAvailability),]
  data.table::setkey(dt.alcScenarios.melt, "scenario", "region_code.IMPACT3", "IMPACT_code")

  # add fish
  dt.fishScenarios <- getNewestVersion("dt.fishScenarios")
  dt.fishScenarios[,pop:=NULL]
  idVarsFish <- c("scenario","ISO_code","year")
  #get the names of the fish that are included in dt.fishScenario
  measureVarsFish <- names(dt.fishScenarios)[!names(dt.fishScenarios) %in% idVarsFish]
  dt.fishScenarios.melt <- data.table::melt(dt.fishScenarios,
                                            id.vars = idVarsFish,
                                            variable.name = "IMPACT_code",
                                            measure.vars = measureVarsFish,
                                            value.name = "FoodAvailability",
                                            variable.factor = FALSE)
  dt.fishScenarios.melt<- merge(dt.fishScenarios.melt, df.regions.all, by = "ISO_code", all = TRUE)
  deleteListCol <- c("ISO_code","region_code.SSP","FAOSTAT_code","country_name.ISO", "region_code.IMPACT115",
                     "region_name.IMPACT115","region_code.IMPACTstandard","region_name.IMPACT3",
                     "region_name.IMPACTstandard","Short.name","Official.name",
                     "ISO2_code","UNI_code","UNDP_code","GAUL_code"
  )
  dt.fishScenarios.melt[,(deleteListCol) := NULL]
  #to get rid of the countries that are in IMPACT regions and so don't have separate data
  dt.fishScenarios.melt <- dt.fishScenarios.melt[!is.na(FoodAvailability),]
  data.table::setkey(dt.fishScenarios.melt, "scenario", "region_code.IMPACT3", "IMPACT_code")
  dtList <- list(dt.FoodAvailability,dt.fishScenarios.melt,dt.fishScenarios.melt)
  dt.FoodAvailability <- data.table::rbindlist(dtList)

  # dtlist <- list(dt.FoodAvailability,dt.pcGDPX0,dt.PCX0.food,dt.PWX0.food,dt.CSEs.food)
  dtlist <- list(dt.FoodAvailability,dt.pcGDPX0,dt.PCX0.food,dt.PWX0.food,dt.CSEs.food)
  dt.IMPACTfood <- plyr::join_all(dtlist)

  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["CSE"]])), "CSE", 0)

  data.table::setorder(dt.IMPACTfood, scenario, region, IMPACT_code, year)
  data.table::setkeyv(dt.IMPACTfood, c("scenario", "region_code.IMPACT3", "IMPACT_code"))
  inDT <- dt.IMPACTfood
  outName <- "dt.IMPACTfood"
  cleanupIMPACT(inDT,outName)
}


