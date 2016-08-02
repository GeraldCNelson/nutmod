# code to produce country specific output for the paper

#Copyright (C) 2016 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
#   for more details at http://www.gnu.org/licenses/.
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

ctyList <- c("NIC", "BRA", "CHM", "ETH", "IND", "GHA","TZA", "FRP", "VNM", "USA", "ZAF")
yearList <- c("X2010", "X2050")
scenarioList <- c("SSP1-NoCC","SSP2-NoCC","SSP3-NoCC", "SSP2-HGEM", "SSP2-IPSL")
dt.regions.all <- getNewestVersion("dt.regions.all")

switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
dt.nutrients <- cookingRetFishCorrect(useCookingRetnValues, fixFish)
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
dt.shannonDiversity <- getNewestVersion("dt.shannonDiversity", fileloc("resultsDir"))
# keep data just for the countries in ctyList, years in yearList and scenarios in scenarioList
dt.IMPACTfood <- dt.IMPACTfood[region_code.IMPACT159 %in% ctyList & scenario %in% scenarioList &
                                 year %in% yearList,]
dt.shannonDiversity <- dt.shannonDiversity[region_code.IMPACT159 %in% ctyList & scenario %in% scenarioList &
                                             year %in% yearList,]

keepListCol <-  c("scenario", "region_code.IMPACT159", "year", "pcGDPX0", "budget.PCX0", "incSharePCX0")
dt.budgetShare <- dt.budgetShare[region_code.IMPACT159 %in% ctyList & scenario %in% scenarioList &
                                   year %in% yearList,keepListCol, with = FALSE]

dt.regions.all <- unique(dt.regions.all[region_code.IMPACT159 %in% ctyList,c("region_code.IMPACT159", "region_name.IMPACT159"), with = FALSE])
dt.regions.all <- dt.regions.all[,region_name.IMPACT159 := gsub(" plus", "", dt.regions.all$region_name.IMPACT159)]
dt.budgetShare <- merge(dt.budgetShare, dt.regions.all, by = "region_code.IMPACT159")
dt.shannonDiversity <- merge(dt.shannonDiversity, dt.regions.all, by = "region_code.IMPACT159")

inDT <- dt.fishIncElast
outName <- "dt.fishIncElast"
cleanup(inDT,outName,fileloc("iData"))
