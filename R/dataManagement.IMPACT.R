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

  # add fish and alcoholic beverages
  dt.fishnAlcScenarios <- getNewestVersion("dt.fishnAlcScenarios")
  dt.fishnAlcScenarios <- dt.fishnAlcScenarios[year %in% keepYearList, ]

  idVarsFishnAlc <- c("scenario", "region_code.IMPACT159", "year")
  #' #' get the names of the fish and alcoholic beverages that are included in dt.fishScenario
  measureVarsFishnAlc <- names(dt.fishnAlcScenarios)[!names(dt.fishnAlcScenarios) %in% idVarsFishnAlc]
  dt.fishnAlcScenarios.melt <- data.table::melt(dt.fishnAlcScenarios,
                                                id.vars = idVarsFishnAlc,
                                                variable.name = "IMPACT_code",
                                                measure.vars = measureVarsFishnAlc,
                                                value.name = "FoodAvailability",
                                                variable.factor = FALSE)

  #' # create alcohol and fish data sets for all the IMPACT scenarios -----
  dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
  scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)

  # add the climModel and experiment on to the scenario name

  # some kludging follows - May 12, 2018
  dt.fishnAlcScenarios.melt[, scenario := gsub("SSP1", "SSP1-NoCC-REF", scenario)]  # because the current set of scenarios only has SSP1 and SSP3 with noCC
  dt.fishnAlcScenarios.melt[, scenario := gsub("SSP3", "SSP3-NoCC-REF", scenario)]  # because the current set of scenarios only has SSP1 and SSP3 with noCC
  temp.SSP2 <- dt.fishnAlcScenarios.melt[scenario %in% "SSP2"]
  temp.SSP2.NoCC <- copy(temp.SSP2)
  temp.SSP2.GFDL <- copy(temp.SSP2)
  temp.SSP2.HGEM <- copy(temp.SSP2)
  temp.SSP2.IPSL <- copy(temp.SSP2)
  temp.SSP2.GFDL[ , scenario := gsub("SSP2", "SSP2-GFDL-REF", scenario)]
  temp.SSP2.HGEM[ , scenario := gsub("SSP2", "SSP2-HGEM-REF", scenario)]
  temp.SSP2.IPSL[ , scenario := gsub("SSP2", "SSP2-IPSL-REF", scenario)]
  temp.SSP2.NoCC[ , scenario := gsub("SSP2", "SSP2-NoCC-REF", scenario)]
  dt.fishnAlcScenarios.melt <- dt.fishnAlcScenarios.melt[!scenario %in% c("SSP2", "SSP4", "SSP5")]
  dt.fishnAlcScenarios.melt <- rbind(temp.SSP2.NoCC, temp.SSP2.GFDL, temp.SSP2.HGEM, temp.SSP2.IPSL, dt.fishnAlcScenarios.melt)

  #' add fish and alcohol via dt.fishnAlcScenarios.melt to dt.FoodAvail, which has all the other commodities
  dtList <- list(dt.FoodAvail, dt.fishnAlcScenarios.melt)
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

  # at the moment dt.IMPACTfood has a MIROC scenario. I'm going to get rid of it for now. May 12, 2018
  dt.IMPACTfood <- dt.IMPACTfood[!scenario %in% "SSP2-MIROC-REF"]
  inDT <- dt.IMPACTfood
  outName <- "dt.IMPACTfood"
  desc <- "Annual and daily availability of each food item, per capita GDP and domestic and world prices."
  cleanup(inDT, outName, fileloc("iData"), desc = desc)
}

#region <- keyVariable("region")

combineIMPACTData()

finalizeScriptMetadata(metadataDT, sourceFile)
