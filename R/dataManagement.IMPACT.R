#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
source("R/nutrientModFunctions.R")

sourceFile <- "dataManagementIMPACT.R"
createScriptMetaData()

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
#' The FoodAvailability variable in the data table dt.FoodAvail is in kgs/person/year.

#' create data table with just food items, relevant years and scenarios
IMPACTfoodCommodList <- keyVariable("IMPACTfoodCommodList")
scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))[,scenario]
keepYearList  <- keyVariable("keepYearList")
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))

createFood <- function(fileShortName, fileloc) {
  dt <- getNewestVersion(fileShortName, fileloc)
  dt <- dt[IMPACT_code %in% IMPACTfoodCommodList &
             year %in% keepYearList]
  return(dt)
}

#' combine all relevant data tables for analysis
combineIMPACTData <- function() {
  dt.FoodAvail <- createFood("dt.FoodAvailability", fileloc("iData")) # created in dataPrep.IMPACT
  # data.table::setkey(dt.FoodAvail, "scenario", "year", "region_code.IMPACT159", "IMPACT_code")
  
  # add fish and alcoholic beverages
  dt.fishnAlcScenarios <- createFood("dt.fishnAlcScenarios", fileloc("mData")) # added May 14, 2018 to parallel dt.FoodAvail. Moved to mData because there are gdx specific files Oct 13, 2018
  
  # add correct scenario names for fish and alcohol
  gdxChoice <- getGdxChoice()
  # get the list of scenarios in the IMPACT data for use below
  if (gdxChoice %in% "SSPs"){
    dt.fishnAlcScenarios <- dt.fishnAlcScenarios[scenario %in% c("SSP1", "SSP2", "SSP3")]
    # add the climModel and experiment on to the scenario name
    
    # some kludging follows - May 12, 2018
    dt.fishnAlcScenarios[, scenario := gsub("SSP1", "SSP1-NoCC-REF", scenario)]  # because the current set of scenarios only has SSP1 and SSP3 with noCC
    dt.fishnAlcScenarios[, scenario := gsub("SSP3", "SSP3-NoCC-REF", scenario)]  # because the current set of scenarios only has SSP1 and SSP3 with noCC
    temp.SSP2 <- dt.fishnAlcScenarios[scenario %in% "SSP2"]
    temp.SSP2.NoCC <- copy(temp.SSP2)
    temp.SSP2.GFDL <- copy(temp.SSP2)
    temp.SSP2.HGEM <- copy(temp.SSP2)
    temp.SSP2.IPSL <- copy(temp.SSP2)
    temp.SSP2.GFDL[ , scenario := gsub("SSP2", "SSP2-GFDL-REF", scenario)]
    temp.SSP2.HGEM[ , scenario := gsub("SSP2", "SSP2-HGEM-REF", scenario)]
    temp.SSP2.IPSL[ , scenario := gsub("SSP2", "SSP2-IPSL-REF", scenario)]
    temp.SSP2.NoCC[ , scenario := gsub("SSP2", "SSP2-NoCC-REF", scenario)]
    dt.fishnAlcScenarios <- dt.fishnAlcScenarios[!scenario %in% "SSP2"]
    dt.fishnAlcScenarios <- rbind(temp.SSP2.NoCC, temp.SSP2.GFDL, temp.SSP2.HGEM, temp.SSP2.IPSL, dt.fishnAlcScenarios)
  }
  
  if (gdxChoice %in% "USAIDPriorities"){
    dt.fishnAlcScenarios <- dt.fishnAlcScenarios[scenario %in% c("SSP2")]
    dt.fishnAlcScenarios[, scenario := scenarioListIMPACT[1]]
    dt.temp <- copy(dt.fishnAlcScenarios)
    for (i in 2:length(scenarioListIMPACT)) {
      temp <- dt.temp[, scenario := scenarioListIMPACT[i]]
      dt.fishnAlcScenarios <- rbind(dt.fishnAlcScenarios, temp)
    }
  }
  
  if (gdxChoice %in% "AfricanAgFutures"){
    dt.fishnAlcScenarios <- dt.fishnAlcScenarios[scenario %in% scenarioListIMPACT]}
  
  #' add fish and alcohol via dt.fishnAlcScenarios to dt.FoodAvail, which has all the other commodities
  dtList <- list(dt.FoodAvail, dt.fishnAlcScenarios)
  dt.FoodAvail <- data.table::rbindlist(dtList, use.names = TRUE)
  #' now add the other variables
  dt.PWX0.food <- createFood("dt.PWX0", fileloc("iData"))
  dt.PCX0.food <- createFood("dt.PCX0", fileloc("iData"))
  # dt.pcGDPX0 <- createFood("dt.pcGDPX0") can't do this because dt.pcGDPX0 doesn't include IMPACT_code
  dt.pcGDPX0 <- getNewestVersion("dt.pcGDPX0", fileloc("iData"))
  dt.pcGDPX0 <- dt.pcGDPX0[year %in% keepYearList & scenario %in% scenarioListIMPACT,]
  
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
  
  # at the moment dt.IMPACTfood for the SSPs gdx has a MIROC scenario because the scenario list includes it. I'm going to get rid of it for now. May 12, 2018
  dt.IMPACTfood <- dt.IMPACTfood[!scenario %in% "SSP2-MIROC-REF"]
  inDT <- dt.IMPACTfood
  outName <- "dt.IMPACTfood"
  desc <- "Annual (FoodAvailability) and daily availability (foodAvailpDay) of each food item in kgs, per capita GDP and domestic and world prices."
  cleanup(inDT, outName, fileloc("iData"), desc = desc)
}

#region <- keyVariable("region")

combineIMPACTData()

finalizeScriptMetadata(metadataDT, sourceFile)
