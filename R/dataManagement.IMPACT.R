#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")}

# Intro -------------------------------------------------------------------

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
#   for more details at http://www.gnu.org/licenses/.

#' @description This script reads in the IMPACT rds files prepared in dataPrep.IMPACT.R and
#' creates the IMPACTfood data table for use in nutrientCalcs.R. It reads in dt.FoodAvail,
#' adds the fish and alcohol data, and pcGDPX0, PCX0 PWX0, and CSV and writes out dt.IMPACTfood.

#create data table just for food items
createFood <- function(fileShortName) {
  IMPACTfoodCommodList <- keyVariable("IMPACTfoodCommodList")
  keepYearList  <- keyVariable("keepYearList")
  dt.temp <- getNewestVersionIMPACT(fileShortName)
  dt.temp.food <- dt.temp[IMPACT_code %in% IMPACTfoodCommodList]
  dt.temp.food <- dt.temp.food[year %in% keepYearList]
  inDT <- dt.temp.food
  outName <- paste(fileShortName,"food",sep = ".")
  cleanup(inDT,outName,fileloc("iData"))
  return(dt.temp.food)
}

#combine all relevant data tables for analysis
combineIMPACTData <- function() {
  # combineIMPACTData <- function(region,scenSSP,climModel,RCP) {
  #  scen <- paste(scenSSP,"-",climModel,sep="")
  keepYearList <- keyVariable("keepYearList")
  dt.FoodAvail <- getNewestVersionIMPACT("dt.FoodAvail")
  dt.FoodAvail <- dt.FoodAvail[year %in% keepYearList,]
  dt.regions.all <- data.table::as.data.table(getNewestVersion("df.regions.all"))
  # get the list of scenarios in the IMPACT data for use below
  scenarioListIMPACT <- keyVariable("scenarioListIMPACT")
  # scenarioListIMPACT <- unique(dt.FoodAvail$scenario)
  data.table::setkey(dt.FoodAvail, "scenario","year", "region_code.IMPACT3", "IMPACT_code")

  # add alcohol
  dt.alcScenarios <- getNewestVersion("dt.alcScenarios")
  dt.alcScenarios <- dt.alcScenarios[year %in% keepYearList,]
  IMPACTalcohol_code <- keyVariable("IMPACTalcohol_code")

  #get the names of the beverages that are included in dt.alcScenarios
  idVarsAlc <- c("scenario","region_code.SSP","year")
  measureVarsAlc <- IMPACTalcohol_code

  dt.alcScenarios.melt <- data.table::melt(dt.alcScenarios,
                                           id.vars = idVarsAlc,
                                           variable.name = "IMPACT_code",
                                           measure.vars = measureVarsAlc,
                                           value.name = "FoodAvailability",
                                           variable.factor = FALSE)

   dt.pop <- getNewestVersion("dt.IMPACT3.pop.tot")
  data.table::setnames(dt.pop, old = "value", new = "pop")
  dt.pop[,pop.sum := sum(pop), by = eval(data.table::key(dt.pop))][,pop.share := pop/pop.sum]

  # aggregate to IMPACT3 regions
  dt.alcScenarios.melt <- merge(dt.alcScenarios.melt,dt.regions.all, by = "region_code.SSP")
  keepListCol <- c("scenario", "region_code.SSP", "year", "IMPACT_code", "FoodAvailability", "region_code.IMPACT3")
  dt.alcScenarios.melt <- dt.alcScenarios.melt[,keepListCol, with = FALSE]

  # reduce scenario name to just SSPx
  dt.alcScenarios.melt[,scenario := substring(scenario,1,4)]
  dt.pop[,scenario := substring(scenario,1,4)]

  data.table::setkeyv(dt.alcScenarios.melt,c("scenario", "region_code.IMPACT3", "year"))
  data.table::setkeyv(dt.pop, c("scenario", "region_code.IMPACT3", "year"))
  dt.alcScenarios.melt <- merge(dt.alcScenarios.melt, dt.pop, by = c("scenario", "region_code.IMPACT3", "year"))
 # dt.temp1 <- dt.alcScenarios.melt[dt.pop]
  data.table::setkeyv(dt.alcScenarios.melt, c("scenario", "region_code.IMPACT3", "year", "IMPACT_code"))
  dt.alcScenarios.melt[,foodAvailability.sum := sum(FoodAvailability * pop.share), by = eval(data.table::key(dt.alcScenarios.melt))]

  dt.alcScenarios.melt[,FoodAvailability := sum(FoodAvailability), by = eval(data.table::key(dt.alcScenarios.melt))]
  dt.alcScenarios.melt[,region_code.SSP := NULL]
  dt.alcScenarios.melt <-  unique(dt.alcScenarios.melt)

  # add fish
  dt.fishScenarios <- getNewestVersion("dt.fishScenarios")
  dt.fishScenarios <- dt.fishScenarios[year %in% keepYearList,]
  # use next line because of missing tuna and shrimp
  IMPACTfish_code <- names(dt.fishScenarios)[4:length(dt.fishScenarios)]

  idVarsFish <- c("scenario","region_code.SSP","year")
  #get the names of the fish that are included in dt.fishScenario
  measureVarsFish <- IMPACTfish_code
  dt.fishScenarios.melt <- data.table::melt(dt.fishScenarios,
                                            id.vars = idVarsFish,
                                            variable.name = "IMPACT_code",
                                            measure.vars = measureVarsFish,
                                            value.name = "FoodAvailability",
                                            variable.factor = FALSE)
  # reduce scenario name to just SSPx
  dt.fishScenarios.melt[,scenario := substring(scenario,1,4)]

  # aggregate to IMPACT3 regions
  dt.fishScenarios.melt <- merge(dt.fishScenarios.melt,dt.regions.all, by = "region_code.SSP")
  keepListCol <- c("scenario", "region_code.SSP", "year", "IMPACT_code", "FoodAvailability", "region_code.IMPACT3")
  dt.fishScenarios.melt <- dt.fishScenarios.melt[,keepListCol, with = FALSE]

    data.table::setkeyv(dt.fishScenarios.melt,c("scenario", "region_code.IMPACT3", "year"))
  data.table::setkeyv(dt.pop, c("scenario", "region_code.IMPACT3", "year"))
  dt.fishScenarios.melt <- merge(dt.fishScenarios.melt, dt.pop, by = c("scenario", "region_code.IMPACT3", "year"))
  data.table::setkeyv(dt.fishScenarios.melt, c("scenario", "region_code.IMPACT3", "year", "IMPACT_code"))
  dt.fishScenarios.melt[,foodAvailability.sum := sum(FoodAvailability * pop.share), by = eval(data.table::key(dt.fishScenarios.melt))]

  dt.fishScenarios.melt[,region_code.SSP := NULL]
  dt.fishScenarios.melt <-  unique(dt.fishScenarios.melt)

 #create alcohol and fish data sets for all the IMPACT scenarios
   dt.temp <- dt.alcScenarios.melt[FALSE,]
  for (i in unique(dt.FoodAvail$scenario)) {
    climModel <- gsub(substr((i),1,5),"",i)
    SSPNum <- substr((i),1,4)
    print(i)
    temp.fish <- data.table::copy(dt.fishScenarios.melt)
    temp.alc <- data.table::copy(dt.alcScenarios.melt)
    temp.fish[,scenario := paste(SSPNum,climModel,sep = "-")]
    temp.alc[,scenario := paste(SSPNum,climModel,sep = "-")]
    dt.temp <- rbind(dt.temp, temp.fish,temp.alc)
  }
  dt.temp <- dt.temp[scenario %in% scenarioListIMPACT,]
  deleteListCol <- c("pop", "pop.sum", "pop.share", "foodAvailability.sum")
  dt.temp[,(deleteListCol) := NULL]
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
  #  dt.PWX0.food[, scenario := substring(scenario,1,4)]
  #  dt.PCX0.food[, scenario := substring(scenario,1,4)]
  #  dt.PCX0.food[, scenario := substring(scenario,1,4)]

  data.table::setkeyv(dt.PWX0.food, c("scenario",         "IMPACT_code", "year"))
  data.table::setkeyv(dt.CSEs.food, c(            "region_code.IMPACT3", "IMPACT_code"))
  data.table::setkeyv(dt.PCX0.food, c("scenario", "region_code.IMPACT3", "IMPACT_code", "year"))
  data.table::setkeyv(dt.pcGDPX0,   c("scenario", "region_code.IMPACT3",                "year"))
  data.table::setkeyv(dt.FoodAvail, c("scenario", "region_code.IMPACT3", "IMPACT_code", "year"))
  dtlist <- list(dt.FoodAvail,dt.pcGDPX0,dt.PCX0.food,dt.PWX0.food,dt.CSEs.food)
  dt.IMPACTfood <- plyr::join_all(dtlist)
  # set CSEs, PCX, and PWX that are NA to 0
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["CSE"]])), "CSE", 0)
  # needed because fish and alcohol don't have prices
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["PCX0"]])), "PCX0", 0)
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["PWX0"]])), "PWX0", 0)
  dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList")]
  data.table::setorderv(dt.IMPACTfood, cols = c("scenario",  "region_code.IMPACT3", "IMPACT_code","year"))
  data.table::setkeyv(dt.IMPACTfood, c("scenario",  "region_code.IMPACT3", "IMPACT_code"))

  inDT <- dt.IMPACTfood
  outName <- "dt.IMPACTfood"
  cleanup(inDT,outName,fileloc("iData"))
}

#region <- keyVariable("region")

combineIMPACTData()

