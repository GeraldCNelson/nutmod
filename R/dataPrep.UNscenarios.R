#' @title "Preparation of Population Data from the UN"
#' @keywords data management, SSP, UN, population data
#' @name "dataPrep.UNscenarios.R"
#' @description
#' This script reads in the Shared Socioeconomic Profiles population data written by dataPrep.SSP.R and
#' the UN population data and has the start of some code to do population trees. It writes out
#' dt.pop.AfrAgFutures, the UN population data needed for the AfricanAgFutures project

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
dt.SSPPop <- getNewestVersion("dt.SSPPopClean", fileloc("uData")) # age/gender data for all the SSPs.
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
dt.scenariosLookup  <- as.data.table(read_excel("data-raw/AfricanAgFutures/scenlookupAfrAgFutures.xlsx"))
allAfricaCodes <- keyVariable("allAfricaCodes")
setnames(dt.SSPPop, old = "value", new = "Population.ssp")

dt.SSPPop[, scenario := substring(scenario, 1, 4)]
dt.SSPPop <- dt.SSPPop[scenario %in% "SSP1", scenario := "Low"
                       ][scenario %in% "SSP2", scenario := "Medium"
                         ][scenario %in% "SSP3", scenario := "High"][!scenario %in% c("SSP4", "SSP5")]

# read in and manipulate UN pop data
level <- c("ESTIMATES", "LOW VARIANT", "MEDIUM VARIANT", "HIGH VARIANT")
level.varname <- c("Low", "Medium", "High")
gender <- c("FEMALE", "MALE")
ageList <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
             "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100Plus")
ageListNew <- gsub("-", "_", ageList)
dt.popUN_male <- data.table( scenario = character(), region_code.IMPACT159 = character(), year = character(), 
                             ageGenderCode = character(), value = numeric())
dt.popUN_female <- data.table( scenario = character(), region_code.IMPACT159 = character(), year = character(), 
                               ageGenderCode = character(), value = numeric())

keeplistCol <- c("Variant", "Country code", "Reference date (as of 1 July)", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
for (j in gender) {
  fName <- paste0("data/universalData/UNPopulation/WPP2017_POP_F07_3_POPULATION_BY_AGE_", j, ".xlsx")
  if (j %in% "MALE") fName <- paste0("data/universalData/UNPopulation/WPP2017_POP_F07_2_POPULATION_BY_AGE_", j, ".xlsx")
  cat(str(fName))
  
  for (k in 1:length(level)) { # level is ESTIMATES (for 2010), low, medium, high
    col_types = c("text", "text", "text", 
                  "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric",  "numeric")
    if (k == 1) col_types = c("text", "text", "text", # ESTIMATES has an extra column
                              "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
    
    dt <- as.data.table(read_excel(fName, skip = 16, sheet = level[k], col_names = TRUE , col_types = col_types, na = "…",))
    dt[, setdiff(names(dt), keeplistCol) := NULL]
    setnames(dt, old = names(dt), new = c("scenario", "UNI_code", "year", paste0(substr(j, 1, 1), ageListNew)))
    if (k == 1) {dt <- dt[year %in% "2010",]} # keep year 2010 from the ESTIMATES worksheet
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
    if (k == 1) {
      # need to repeat 2010 (from ESTIMATES) for the three scenarios
      for (l in level.varname) {
        dt.long[, scenario := l]
        if (j %in% "MALE") dt.popUN_male <- rbind(dt.popUN_male, dt.long)
        if (j %in% "FEMALE") dt.popUN_female <- rbind(dt.popUN_female, dt.long)
      }
    } else {
      if (j %in% "MALE") dt.popUN_male <- rbind(dt.popUN_male, dt.long)
      if (j %in% "FEMALE") dt.popUN_female <- rbind(dt.popUN_female, dt.long)
    }
    # newName <- paste("dt.pop",  str_to_lower(j, locale = "en"),  level.varname[k], sep = "_")
    # assign(newName, dt.long)
  }
}

# combine female and male data into one file called dt.popUN -----
setnames(dt.popUN_female, old = c("ageGenderCode", "value"), new = c("Age", "Population"))
setnames(dt.popUN_male, old = c("ageGenderCode", "value"), new = c("Age", "Population"))
dt.popUN_male[, Age := gsub("M", "", Age)]
dt.popUN_female[, Age := gsub("F", "", Age)]
dt.popUN_female[, Gender := "Female"]
dt.popUN_male[, Gender := "Male"]
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

# generate scenario-specific data for the Africa Ag Futures project. View dt.scenariosLookup
dt.Med_base_NoCC <- copy(dt.popcombo)
dt.Med_base_NoCC <- dt.Med_base_NoCC[scenario %in% "Medium", ][
  ,Population := Population.UN][
    , c("Population.ssp", "Population.UN") := NULL]

dt.Med_base_CC <- copy(dt.Med_base_NoCC)
dt.Med_pes_CC <- copy(dt.Med_base_NoCC)
dt.Med_opt_CC <- copy(dt.Med_base_NoCC)

dt.HighAfr_pes_CC <- copy(dt.popcombo)
dt.HighAfr_pes_CC <- dt.HighAfr_pes_CC[region_code.IMPACT159 %in% allAfricaCodes & scenario %in% "High", Population := Population.UN][
  scenario %in% "Medium" & !region_code.IMPACT159 %in% allAfricaCodes, Population := Population.UN][
    , c("Population.ssp", "Population.UN") := NULL][
      !is.na(Population),]

dt.LowAfr_opt_CC <- copy(dt.popcombo)
dt.LowAfr_opt_CC <- dt.LowAfr_opt_CC[region_code.IMPACT159 %in% allAfricaCodes & scenario %in% "High", Population := Population.UN][
  scenario %in% "Medium" & !region_code.IMPACT159 %in% allAfricaCodes, Population := Population.UN][
    , c("Population.ssp", "Population.UN") := NULL][
      !is.na(Population),]

dt.SSP2_SSP2_noCC <- copy(dt.popcombo)
dt.SSP2_SSP2_noCC <- dt.SSP2_SSP2_noCC[scenario %in% "Medium", ][
  , Population := Population.ssp][
    , c("Population.ssp", "Population.UN") := NULL]
dt.SSP2_SSP2_CC <- copy(dt.SSP2_SSP2_noCC)

# combine individual scenarios into one large population file
dt.Med_base_NoCC[, scenario := "Med_base_NoCC"]
dt.Med_base_CC[, scenario := "Med_base_CC"]
dt.Med_pes_CC[, scenario := "Med_pes_CC"]
dt.Med_opt_CC[, scenario := "Med_opt_CC"]
dt.HighAfr_pes_CC[, scenario := "HighAfr_pes_CC"]
dt.LowAfr_opt_CC[, scenario := "LowAfr_opt_CC"]
dt.SSP2_SSP2_noCC[, scenario := "SSP2_SSP2_noCC"]
dt.SSP2_SSP2_CC[, scenario := "SSP2_SSP2_CC"]
dt.pop.AfrAgFutures <- rbindlist(list( dt.Med_base_CC, dt.HighAfr_pes_CC, dt.LowAfr_opt_CC)) # revised Nov 19, 2018
# change scenario names for revised submission Nov 20, 2018
dt.pop.AfrAgFutures <- dt.pop.AfrAgFutures[scenario %in% "Med_base_CC", scenario := "Reference"]
dt.pop.AfrAgFutures <- dt.pop.AfrAgFutures[scenario %in% "HighAfr_pes_CC", scenario := "Pessimistic"]
dt.pop.AfrAgFutures <- dt.pop.AfrAgFutures[scenario %in% "LowAfr_opt_CC", scenario := "Optimistic"]

# combine age and gender to match with SSP pop data
dt.cleanup <- dt.pop.AfrAgFutures[Gender %in% "Female", Gender := "F"][Gender %in% "Male", Gender := "M"]
dt.cleanup[, ageGenderCode := paste0(Gender, Age)][, c("Age", "Gender") := NULL]
setnames(dt.cleanup, old = "Population", new = "value")
dt.cleanup <- unique(dt.cleanup)
inDT <- dt.cleanup
outname <- "dt.pop.AfrAgFutures"
desc <- "Population scenarios for the African Ag Futures project that combine SSP and UN population data sets with age and gender information"
cleanup(inDT, outname, fileloc("uData"), desc = desc)

# code borrowed from https://rpubs.com/walkerke/pyramids_ggplot2 to do population pyramids
ctyChoice <- "AFG"
yearChoice <- "X2050"
scenarioChoice = "Low"
countryDataSubset <- dt.popcombo[scenario == scenarioChoice & region_code.IMPACT159 == ctyChoice & year %in% yearChoice, ]
countryDataSubset[, c("scenario", "region_code.IMPACT159", "year") := NULL]
dataSetChoice <- "ssp"# choices are UN and ssp
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

library(XML)
library(reshape2)
library(rCharts)
library(gganimate) # needs to be installed with devtools::install_github('thomasp85/gganimate'); get info at https://github.com/thomasp85/gganimate

getAgeTable <- function(country, year, dt.popcombo, scenario) {
  dt <- copy(dt.popcombo)
  dt <- dt(region_code.IMPACT159 %in% country & year %in% year & scenario %in% scenario, )
  # c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="  
  # c2 <- "&R=-1&C="
  # yrs <- gsub(" ", "", toString(year))
  # url <- paste0(c1, yrs, c2, country)
  # df <- data.frame(readHTMLTable(url))
  # nms <- c("Year", "Age", "total", "Male", "Female", "percent", "pctMale", "pctFemale", "sexratio")  
  names(df) <- nms  
  cols <- c(1, 3:9)
  df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df <- df[df$Age != 'Total', ]  
  ord <- 1:nrow(df)
  df <- cbind(df, ord)
  return(df)
}


