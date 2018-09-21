#' @title "Preparation of Population Data"
#' @keywords data management, SSP, UN, population data
#' @name "dataPrep.UNscenarios.R"
#' @description
#' This script reads in the Shared Socioeconomic Profiles population data written by dataPrep.SSP.R,
#' reads in the UN population data and does population trees
#' to align the SSP population data with the nutrient requirements age and gender structure data
#' Creates the following files
#'   req.EAR.ssp - data/req.EAR.percap.rds
#'   req.RDA.vits.ssp - data/req.RDA.vits.percap.rds
#'   req.RDA.minrls.ssp - data/req.RDA.minrls.percap.rds
#'   req.RDA.macro.ssp - data/req.RDA.macro.percap.rds
#'   req.UL.vits.ssp - data/req.UL.vits.percap.rds
#'   req.UL.minrls.ssp - data/req.UL.minrls.percap.rds"
#'   req.MRVs.ssp - data/req.MRVs.percap.rds" # added March 24, 2017

#' @source \url{https://tntcat.ac.at/SspDb/download/common/SspDb_country_data_2013-06-12.csv.zip}
#Copyright (C) 2015 - 2018 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#   GNU General Public License for more details at http://www.gnu.org/licenses/.

source("R/nutrientModFunctions.R")
sourceFile <- "dataPrep.UNscenarios.R"
createScriptMetaData()
library(readxl)
library(stringr)
library(rCharts)

# load needed files
dt.pop.SSPs <- getNewestVersion("dt.PopX0", "data/SSPs/IMPACTData/") # total population by country from the SSPs gdx
dt.SSPPop <- getNewestVersion("dt.SSPPopClean", fileloc("uData")) # age/gender data for all the SSPs.
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))

# convert dt.SSPpop country names to region_code.IMPACT159
dt.SSPPop <- merge(dt.SSPPop, dt.regions.all, by = "ISO_code")
keeplistCol.new <- c("scenario", "region_code.IMPACT159", "year", "ageGenderCode", "year","value")
dt.SSPPop[, setdiff(names(dt.SSPPop), keeplistCol.new) := NULL]

#sum over pop data for regions that combine some countries (e.g., CHM)
dt.SSPPop[, Population.ssp := sum(value), by = c("scenario", "region_code.IMPACT159", "ageGenderCode", "year")]
dt.SSPPop[, value := NULL]
dt.SSPPop <- unique(dt.SSPPop)
dt.SSPPop[, scenario := substring(scenario, 1, 4)]
dt.SSPPop <- dt.SSPPop[scenario %in% "SSP1", scenario := "Low"
                       ][scenario %in% "SSP2", scenario := "Medium"
                         ][scenario %in% "SSP3", scenario := "High"]
dt.SSPPop <- dt.SSPPop[!scenario %in% c("SSP4", "SSP5")]

# read in and manipulate UN pop data
level <- c("LOW VARIANT", "MEDIUM VARIANT", "HIGH VARIANT")
level.varname <- c("low", "medium", "high")
gender <- c("FEMALE", "MALE")
ageList <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100Plus")
ageListNew <- gsub("-", "_", ageList)
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
dt.popUN_male <- data.table( scenario = character(), region_code.IMPACT159 = character(), year = character(), 
                              ageGenderCode = character(), value = numeric())
dt.popUN_female <- data.table( scenario = character(), region_code.IMPACT159 = character(), year = character(), 
                             ageGenderCode = character(), value = numeric())
for (j in gender) {
  fName <- paste0("data/universalData/UNPopulation/WPP2017_POP_F07_3_POPULATION_BY_AGE_", j, ".xlsx")
  if (j %in% "MALE") fName <- paste0("data/universalData/UNPopulation/WPP2017_POP_F07_2_POPULATION_BY_AGE_", j, ".xlsx")
  cat(str(fName))
  for (k in 1:length(level)) { # level is low, medium, high
    dt <- as.data.table(read_excel(fName, skip = 16, sheet = level[k], col_names = TRUE))
    keeplistCol <- c("Variant", "Country code", "Reference date (as of 1 July)", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                     "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
    dt[, setdiff(names(dt), keeplistCol) := NULL]
    
    setnames(dt, old = names(dt), new = c("scenario", "UNI_code", "year", paste0(substr(j, 1, 1), ageListNew)))
    dt[, UNI_code := as.character(UNI_code)]
    dt[, UNI_code := str_pad(UNI_code, 3, side = c("left"), pad = "0")] # add leading 0s to country code
    dt <- merge(dt, dt.regions.all, by = "UNI_code")
    keeplistCol.new <- c("scenario", "region_code.IMPACT159", "year", paste0(substr(j, 1, 1), ageListNew))
    dt[, setdiff(names(dt), keeplistCol.new) := NULL]
    dt[, scenario := gsub(" variant", "", scenario)]
    dt[, year := paste0("X", year)]
    idVars <- c("scenario", "region_code.IMPACT159", "year")
    measureVars <- names(dt)[!names(dt) %in% idVars]
    dt.long  <- data.table::melt(
      data = dt,
      id.vars = idVars,
      measure.vars = measureVars,
      variable.name = "ageGenderCode",
      value.name = "value",
      variable.factor = FALSE
    )
    dt.long[, value := value/1000] #convert from 1000s to millions.
    if (j %in% "MALE") dt.popUN_male <- rbind(dt.popUN_male, dt.long)
    if (j %in% "FEMALE") dt.popUN_female <- rbind(dt.popUN_female, dt.long)
    # newName <- paste("dt.pop",  str_to_lower(j, locale = "en"),  level.varname[k], sep = "_")
    # assign(newName, dt.long)
  }
}

setnames(dt.popUN_female, old = c("ageGenderCode", "value"), new = c("Age", "Population"))
setnames(dt.popUN_male, old = c("ageGenderCode", "value"), new = c("Age", "Population"))
dt.popUN_male[, Age := gsub("M", "", Age)]
dt.popUN_female[, Age := gsub("F", "", Age)]
dt.popUN_female[, Gender := "Female"]
dt.popUN_male[, Gender := "Male"]
# dt.popUN <- merge(dt.popUN_female, dt.popUN_male, by = c("scenario", "region_code.IMPACT159",
#                                                          "year", "Age"), allow.cartesian=TRUE)
dt.popUN <- rbind(dt.popUN_female, dt.popUN_male)
dt.popUN[, Population.UN := sum(Population), by = c("scenario", "region_code.IMPACT159", "Age", "Gender", "year")]
dt.popUN[, Population := NULL]
dt.popUN <- unique(dt.popUN)

# convert dt.SSPPop to format of dt.popUN
dt.SSPPop[substring(ageGenderCode, 1, 1) %in% "F", Gender := "Female"]
dt.SSPPop[substring(ageGenderCode, 1, 1) %in% "M", Gender := "Male"]
dt.SSPPop[, Age := substring(ageGenderCode, 2)]
dt.SSPPop[, ageGenderCode := NULL]

dt.popcombo <- merge(dt.SSPPop, dt.popUN, by = c("scenario", "region_code.IMPACT159", "Age", "Gender", "year"))

dt.popcombo[, Age := factor(Age, levels = ageListNew)]


# code borrowed from https://rpubs.com/walkerke/pyramids_ggplot2
ctyChoice <- "AFG"
yearChoice <- "X2050"
scenarioChoice = "Low"
countryDataSubset <- dt.popcombo[scenario == scenarioChoice & region_code.IMPACT159 == ctyChoice & year %in% yearChoice, ]
countryDataSubset[, c("scenario", "region_code.IMPACT159", "year") := NULL]
dataSetChoice <- "ssp" # choices are UN and ssp
popchoice <- paste("Population", dataSetChoice, sep = ".")

ggplot(data = countryDataSubset, 
       mapping = aes(x = Age, fill = Gender, 
                     y = ifelse(test = Gender == "Male", 
                                yes = -get(popchoice), no = get(popchoice)))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(countryDataSubset$Population.ssp) * c(-1,1)) +
  labs(y = popchoice) +
  coord_flip()

# code for animated pyramids https://raw.githubusercontent.com/walkerke/teaching-with-datavis/master/pyramids/rcharts_pyramids.R

# desc <- paste("UN population projections, in millions, for", j, level.varname[k], "variant", sep = " ")
# cleanup(inDT, outName, fileloc("uData"), desc = desc)


# ageList <-
#  c(
#   "Aged0-4", "Aged5-9", "Aged10-14", "Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34",
#   "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64", "Aged65-69",
#   "Aged70-74","Aged75-79","Aged80-84","Aged85-89", "Aged90-94", "Aged95-99","Aged100+"
#  )


