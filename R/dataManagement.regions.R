#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, region management
#' @title Align data to the current region, identified in nutrentModFunctions.R
#' @name dataManagement.regions.R
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

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

#' @description read in data files with country information, created in the various dataManagement scripts
#' and make sure they all use the same regions

# load regions info ----
dt.regions.all <- getNewestVersion("dt.regions.all")
# region <- keyVariable("region")
region <-  "region_code.IMPACT159"

# list of countries not in SSP data set so no age gender info
missingSSP <- setdiff(dt.regions.all$ISO_code,dt.regions.all$region_code.SSP)
#remove them from list of countries
dt.regions.all <- dt.regions.all[!ISO_code %in% missingSSP,]
data.table::setkey(dt.regions.all)

keepYearList <- keyVariable("keepYearList")
scenarioListIMPACT <- keyVariable("scenarioListIMPACT")

dt.pop <- getNewestVersion("dt.IMPACT.pop3.tot")
dt.pop[,scenario := substr((scenario),1,4)] # keep just the SSP scenario info

# population doesn't vary by climate model so reproduce the pop data for each of the climate model results
dt.pop.complete <- dt.pop[FALSE,]
for (i in scenarioListIMPACT) {
  climModel <- gsub(substr((i),1,5),"",i)
  SSPNum <- substr((i),1,4)
  print(i)
  temp.pop <- data.table::copy(dt.pop)
  temp.pop[,scenario := paste(scenario, climModel, sep = "-")]
  dt.pop.complete <- rbind(dt.pop.complete, temp.pop)
}

# do food data next
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood"); dt.IMPACTfood <- dt.IMPACTfood[year %in% keepYearList,]
# remove list of countries not in SSP data set so no age gender info
dt.IMPACTfood <- dt.IMPACTfood[!region_code.IMPACT159 %in% missingSSP,]


if (!region %in% names(dt.IMPACTfood)) {
  deleteListCol <- c("pcGDPX0", "PCX0", "PWX0")
  dt.temp <- merge(dt.IMPACTfood, dt.regions.all, by = "region_code.IMPACT159") #, allow.cartesian = TRUE)
  keepListCol <- c(names(dt.IMPACTfood),region)
  dt.IMPACTfood <- dt.temp[,keepListCol, with = FALSE][,(deleteListCol) := NULL]
  dt.temp1 <- merge(dt.IMPACTfood,dt.pop.complete, by = c("scenario", "year","region_code.IMPACT159",region))
  data.table::setkeyv(dt.temp1,c("scenario", "year", "IMPACT_code", region))
  dt.temp1[,foodAvailability.sum := sum(FoodAvailability * pop.share), by = eval(data.table::key(dt.temp1))]
  }

data.table::setkeyv(dt.IMPACTfood,c("scenario", "year", "IMPACT_code", region))

dt.IMPACTfood[,value.sum := sum(FoodAvailability), by = eval(data.table::key(dt.IMPACTfood))]
dt.IMPACTfood[,c("FoodAvailability", "pcGDPX0", "PCX0", "PWX0", "CSE") := NULL]
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood.sum <- unique(dt.IMPACTfood)

