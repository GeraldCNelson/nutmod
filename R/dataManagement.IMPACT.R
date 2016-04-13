#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")}

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

#' @description read in the IMPACT rds files prepared in dataPrep.IMPACT.R and create the
#' IMPACTfood data table for use in nutrientCalcs.R

#create data table just for food items
createFood <- function(fileShortName) {
  IMPACTfoodCommodList <- keyVariable("IMPACTfoodCommodList")
  keepYearList  <- keyVariable("keepYearList")
  dt.temp <- getNewestVersionIMPACT(fileShortName)
  dt.temp.food <- dt.temp[IMPACT_code %in% IMPACTfoodCommodList]
  dt.temp.food <- dt.temp.food[year %in% keepYearList]
  inDT <- dt.temp.food
  outName <- paste(fileShortName,"food",sep=".")
  cleanup(inDT,outName,fileloc("iData"))
  return(dt.temp.food)
}

#combine all relevant data tables for analysis
combineIMPACTData <- function(region) {
  #  combineIMPACTData <- function(region,scenSSP,climModel,RCP) {
  #    scen <- paste(scenSSP,"-",climModel,sep="")
  keepYearList <- keyVariable("keepYearList")
  dt.FoodAvail <- getNewestVersionIMPACT("dt.FoodAvail")[year %in% keepYearList,]
  # get the list of scenarios in the IMPACT data for use below
  IMPACTscenarioList <- unique(dt.FoodAvail$scenario)
  df.regions.all <- getNewestVersion("df.regions.all")
  data.table::setkey(dt.FoodAvail, "scenario","year", "region_code.IMPACT3", "IMPACT_code")

  # add alcohol
  dt.alcScenarios <- getNewestVersion("dt.alcScenarios")[,pop:=NULL][year %in% keepYearList,]
  idVarsAlc <- c("scenario","ISO_code","year")
  #get the names of the beverages that are included in dt.alcScenarios
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
  data.table::setorderv(dt.alcScenarios.melt, cols = c("scenario", "IMPACT_code",region, "year", "FoodAvailability"))
  data.table::setkeyv(dt.alcScenarios.melt, c("scenario","region_code.IMPACT3", "IMPACT_code",region, "year"))

  # add fish
  dt.fishScenarios <- getNewestVersion("dt.fishScenarios")[,pop:=NULL][year %in% keepYearList,]
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
  data.table::setkeyv(dt.fishScenarios.melt, c("scenario","region_code.IMPACT3", "IMPACT_code",region, "year"))
  #to get rid of the countries that are in IMPACT regions and so don't have separate data
  dt.fishScenarios.melt <- dt.fishScenarios.melt[!is.na(FoodAvailability),]
  data.table::setkeyv(dt.fishScenarios.melt, c("scenario", "region_code.IMPACT3", "IMPACT_code"))

  #create alcohol and fish data sets for all the IMPACT scenarios
  dt.alcScenarios.melt[,scenario:= substr((scenario),1,4)]
  dt.fishScenarios.melt[,scenario:= substr((scenario),1,4)]
  dt.temp <- dt.alcScenarios.melt[FALSE,]
  for (i in unique(dt.FoodAvail$scenario)) {
    climModel <- gsub(substr((i),1,5),"",i)
    SSPNum <- substr((i),1,4)
    print(i)
    temp.fish <- data.table::copy(dt.fishScenarios.melt)
    temp.alc <- data.table::copy(dt.alcScenarios.melt)
    temp.fish[,scenario:= paste(scenario,climModel,sep="-")]
    temp.alc[,scenario:= paste(scenario,climModel,sep="-")]
    dt.temp <- rbind(dt.temp, temp.fish,temp.alc)
  }
  dt.temp <- dt.temp[scenario %in% IMPACTscenarioList,]
  # add fish and alcohol via dt.temp to dt.FoodAvail, which has all the other commodities
  dtList <- list(dt.FoodAvail,dt.temp)
  dt.FoodAvail <- data.table::rbindlist(dtList, use.names = TRUE)

  # now add the other variables
  dt.PWX0.food <- createFood("dt.PWX0")
  #[scenario == scen,]
  dt.PCX0.food <- createFood("dt.PCX0")
  #[scenario == scen,]
  dt.PCX0.food <- dt.PCX0.food
  #[scenario == scen,]
  dt.CSEs.food <- createFood("dt.CSEs")
  dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
  #[scenario == scen,]

  # # rename the scenarios
  #   dt.PWX0.food[, scenario := substring(scenario,1,4)]
  #   dt.PCX0.food[, scenario := substring(scenario,1,4)]
  #   dt.PCX0.food[, scenario := substring(scenario,1,4)]

  data.table::setkeyv(dt.PWX0.food, c("scenario",         "IMPACT_code", "year"))
  data.table::setkeyv(dt.CSEs.food, c(            region, "IMPACT_code"))
  data.table::setkeyv(dt.PCX0.food, c("scenario", region, "IMPACT_code", "year"))
  data.table::setkeyv(dt.pcGDPX0,   c("scenario", region,                "year"))
  data.table::setkeyv(dt.FoodAvail, c("scenario", region, "IMPACT_code", "year"))
  dtlist <- list(dt.FoodAvail,dt.pcGDPX0,dt.PCX0.food,dt.PWX0.food,dt.CSEs.food)
  dt.IMPACTfood <- plyr::join_all(dtlist)
  # set CSEs, PCX, and PWX that are NA to 0
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["CSE"]])),  "CSE", 0)
  # needed because fish and alcohol don't have prices
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["PCX0"]])), "PCX0", 0)
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["PWX0"]])), "PWX0", 0)
  dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList")]
  data.table::setorderv(dt.IMPACTfood, cols = c("scenario", region, "IMPACT_code","year"))
  data.table::setkeyv(dt.IMPACTfood, c("scenario", region, "IMPACT_code"))

  inDT <- dt.IMPACTfood
  outName <- "dt.IMPACTfood"
  cleanup(inDT,outName,fileloc("iData"))
}
region <- keyVariable("region")

combineIMPACTData(region)

