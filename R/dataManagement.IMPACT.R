#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
source("R/nutrientModFunctions.R")

sourceFile <- "dataManagementIMPACT.R"
createScriptMetaData()

library(data.table)

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

#' create data table with just food items
createFood <- function(fileShortName) {
  IMPACTfoodCommodList <- keyVariable("IMPACTfoodCommodList")
  keepYearList  <- keyVariable("keepYearList")
  dt.temp <- getNewestVersionIMPACT(fileShortName)
  dt.temp.food <- dt.temp[IMPACT_code %in% IMPACTfoodCommodList]
  dt.temp.food <- dt.temp.food[year %in% keepYearList]
  # files produced here not used elsewhere so commented out April 8, 2018
  # inDT <- dt.temp.food
  # outName <- paste(fileShortName, "food", sep = "_")
  # desc <- "Seems to be used just for creating dt.PWX0.food and dt.PCX0.food"
  # cleanup(inDT, outName, fileloc("iData"), desc = desc)
  return(dt.temp.food)
}

#' combine all relevant data tables for analysis
combineIMPACTData <- function() {
  keepYearList <- keyVariable("keepYearList")
  dt.FoodAvail <- getNewestVersionIMPACT("dt.FoodAvailability") # created in dataPrep.IMPACT
  dt.FoodAvail <- dt.FoodAvail[year %in% keepYearList, ]
  # next 2 lines are mainly to get rid of ctoml for ZMB
  IMPACTfoodCommodList <- keyVariable("IMPACTfoodCommodList")
  dt.FoodAvail <- dt.FoodAvail[IMPACT_code %in% IMPACTfoodCommodList, ]
  dt.regions.all <- getNewestVersion("dt.regions.all")
  # get the list of scenarios in the IMPACT data for use below
  dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
  scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)
  dt.FoodAvail <- dt.FoodAvail[scenario %in% scenarioListIMPACT,]
  data.table::setkey(dt.FoodAvail, "scenario", "year", "region_code.IMPACT159", "IMPACT_code")

  # aggregate to IMPACT159 regions -----
  dt.pop <- getNewestVersion("dt.SSP.pop.tot")
  dt.pop <- merge(dt.pop, dt.regions.all, by = "region_code.SSP")
  #' population numbers are used here just to aggregate food availability data from individual (small) countries to their
  #' IMPACT159 regions, weighting individual consumption data by their share in region population.
  #' Population data come from SSP. Food availability from FBS. These don't include the same countries.
  #' So for the population weighting need to keep only the countries for which FBS data are available.
  dt.pop <- dt.pop[!is.na(FAOSTAT_code),]
  keepListCol <- c("scenario", "ISO_code", "region_code.IMPACT159", "year", "pop")
  dt.pop <- dt.pop[,(keepListCol), with = FALSE]
  data.table::setkeyv(dt.pop, c("scenario", "year", "region_code.IMPACT159"))
  #' pop.share is the share of an individual country in a region
  #' pop.sum is the sum of the population from all countries in a region.
  #' Note that an IMPACT region can contain a country not in SSP or FSB. The code above removes the countries
  #'   that are not in FSB.
  dt.pop[, pop.sum := sum(value.pop), by = eval(data.table::key(dt.pop))][, pop.share := value.pop/pop.sum]
  #' reduce scenario name to just SSPx
  dt.pop[, scenario := substring(scenario, 1, 4)]
  data.table::setkey(dt.pop, NULL)

  # add alcohol -----
  dt.alcScenarios <- getNewestVersion("dt.alcScenarios")
  dt.alcScenarios <- dt.alcScenarios[year %in% keepYearList, ]

  #' get the names of the beverages that are included in dt.alcScenarios
  IMPACTalcohol_code <- keyVariable("IMPACTalcohol_code")
  idVarsAlc <- c("scenario", "ISO_code", "year")
  measureVarsAlc <- IMPACTalcohol_code

  dt.alcScenarios.melt <- data.table::melt(dt.alcScenarios,
                                           id.vars = idVarsAlc,
                                           variable.name = "IMPACT_code",
                                           measure.vars = measureVarsAlc,
                                           value.name = "FoodAvailability",
                                           variable.factor = FALSE)

  dt.alcScenarios.melt <- merge(dt.alcScenarios.melt, dt.regions.all, by = "ISO_code")
  keepListCol <- c("scenario", "ISO_code", "region_code.IMPACT159", "IMPACT_code", "year", "FoodAvailability")
  dt.alcScenarios.melt <- dt.alcScenarios.melt[, keepListCol, with = FALSE]

  #' reduce scenario name to just SSPx
  dt.alcScenarios.melt[, scenario := substring(scenario, 1, 4)]

  # aggregate alcohol to IMPACT159 regions -------
  #' pop.share is the share of a country's population in the IMPACT region population
  dt.alcScenarios.melt <- merge(dt.alcScenarios.melt, dt.pop, by = c("scenario", "ISO_code", "region_code.IMPACT159", "year"))
  data.table::setkeyv(dt.alcScenarios.melt, c("scenario", "region_code.IMPACT159", "year", "IMPACT_code"))
  dt.alcScenarios.melt[, FoodAvailability := sum(FoodAvailability * pop.share), by = eval(data.table::key(dt.alcScenarios.melt))]
  deleteListCol = c( "pop","pop.sum","pop.share")
  dt.alcScenarios.melt[, (deleteListCol) := NULL]
  data.table::setkey(dt.alcScenarios.melt)
  dt.alcScenarios.melt <-  unique(dt.alcScenarios.melt)

  # add fish -----
  dt.fishScenarios <- getNewestVersion("dt.fishScenarios")
  dt.fishScenarios <- dt.fishScenarios[year %in% keepYearList, ]
  #' get names of fish composites
  IMPACTfish_code <- names(dt.fishScenarios)[4:length(dt.fishScenarios)]

  idVarsFish <- c("scenario", "ISO_code", "year")
  #' get the names of the fish that are included in dt.fishScenario
  measureVarsFish <- IMPACTfish_code
  dt.fishScenarios.melt <- data.table::melt(dt.fishScenarios,
                                            id.vars = idVarsFish,
                                            variable.name = "IMPACT_code",
                                            measure.vars = measureVarsFish,
                                            value.name = "FoodAvailability",
                                            variable.factor = FALSE)

  dt.fishScenarios.melt <- merge(dt.fishScenarios.melt, dt.regions.all, by = "ISO_code")
  keepListCol <- c("scenario", "ISO_code", "region_code.IMPACT159", "IMPACT_code", "year", "FoodAvailability")
  dt.fishScenarios.melt <- dt.fishScenarios.melt[, keepListCol, with = FALSE]

  #' reduce scenario name to just SSPx. This seems to have been done earlier in creation of dt.fishScenarios.melt so commenting next line out. April 8, 2018
  # dt.fishScenarios.melt[, scenario := substring(scenario, 1, 4)]

  # aggregate fish to IMPACT159 regions -------
  #' pop.share is the share of a country's population in the IMPACT region population. Why do I need to adjust for population share??? april 8, 2018

  dt.fishScenarios.melt <- merge(dt.fishScenarios.melt, dt.pop, by = c("scenario", "ISO_code", "region_code.IMPACT159", "year"))
  dt.fishScenarios.melt[, FoodAvailability := sum(FoodAvailability * pop.share), by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year")]
  deleteListCol = c("pop", "pop.sum", "pop.share")
  dt.fishScenarios.melt[, (deleteListCol) := NULL]
  data.table::setkey(dt.fishScenarios.melt, NULL)
  dt.fishScenarios.melt <-  unique(dt.fishScenarios.melt)

  # create alcohol and fish data sets for all the IMPACT scenarios -----
  dt.temp <- dt.alcScenarios.melt[FALSE, ]
  dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
  scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)
  for (i in scenarioListIMPACT) {
    SSPName <- unlist(strsplit(i, "-"))[1] # get SSP abbrev
    climModel <- unlist(strsplit(i, "-"))[2] # get climate model abbrev
    experiment <- unlist(strsplit(i, "-"))[3] # get experiment abbrev
    temp.fish <- data.table::copy(dt.fishScenarios.melt)
    temp.alc <- data.table::copy(dt.alcScenarios.melt)
    temp.fish <- temp.fish[scenario == SSPName, ]
    temp.alc <- temp.alc[scenario == SSPName, ]
    temp.fish[, scenario := paste(SSPName, climModel, experiment, sep = "-")]
    temp.alc[, scenario := paste(SSPName, climModel, experiment, sep = "-")]
    dtList <- list(dt.temp, temp.fish, temp.alc)
    dt.temp <- data.table::rbindlist(dtList, use.names = TRUE)
  }
  dt.temp[,ISO_code := NULL]
  dt.temp <- unique(dt.temp) # added April 8, 2018. Gets rid of duplicate rows for regions with multiple countries (eg., OIO)

  #' add fish and alcohol via dt.temp to dt.FoodAvail, which has all the other commodities
  dtList <- list(dt.FoodAvail, dt.temp)
  dt.FoodAvail <- data.table::rbindlist(dtList, use.names = TRUE)
  dt.FoodAvail <- unique(dt.FoodAvail) # not sure why this is needed. Added April 8, 2018
  #' now add the other variables
  dt.PWX0.food <- createFood("dt.PWX0")
  dt.PCX0.food <- createFood("dt.PCX0")
  dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")

  data.table::setkeyv(dt.PCX0.food, c("scenario", "region_code.IMPACT159", "IMPACT_code", "year"))
  data.table::setkeyv(dt.FoodAvail, c("scenario", "region_code.IMPACT159", "IMPACT_code", "year"))
  data.table::setkeyv(dt.pcGDPX0,   c("scenario", "region_code.IMPACT159",                "year"))
  data.table::setkeyv(dt.PWX0.food, c("scenario",                          "IMPACT_code", "year"))
  dtlist <- list(dt.FoodAvail, dt.pcGDPX0, dt.PCX0.food, dt.PWX0.food)
  dt.IMPACTfood <- suppressMessages(plyr::join_all(dtlist))
  # an alternative to above that might not work. Might work but can't figure it out now.
  # dtlist <- c("dt.FoodAvail", "dt.pcGDPX0", "dt.PCX0.food", "dt.PWX0.food")
  # testList <- mget(dtlist)
  # mergedDT <- Reduce(function(...) merge(...), testList)
  # set CSEs, PCX, and PWX that are NA to 0
  #  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["CSE"]])), "CSE", 0)
  # needed because fish and alcohol don't have prices
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["PCX0"]])), "PCX0", 0)
  data.table::set(dt.IMPACTfood, which(is.na(dt.IMPACTfood[["PWX0"]])), "PWX0", 0)
  dt.IMPACTfood <- dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList")]
  dt.IMPACTfood <- dt.IMPACTfood[!is.na(pcGDPX0),]
  data.table::setorderv(dt.IMPACTfood, cols = c("scenario",  "region_code.IMPACT159", "IMPACT_code", "year"))
  data.table::setkeyv(dt.IMPACTfood, c("scenario",  "region_code.IMPACT159", "IMPACT_code"))
  dt.IMPACTfood <- dt.IMPACTfood[!region_code.IMPACT159 %in% keyVariable("dropListCty")]
  dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")]
  dt.IMPACTfood <- unique(dt.IMPACTfood)
  inDT <- dt.IMPACTfood
  outName <- "dt.IMPACTfood"
  desc <- "Annual and daily availability of each food item, per capita GDP and domestic and world prices."
  cleanup(inDT, outName, fileloc("iData"), desc = desc)
}

#region <- keyVariable("region")

combineIMPACTData()

finalizeScriptMetadata(metadataDT, sourceFile)
