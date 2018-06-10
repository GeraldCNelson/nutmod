#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords SSP data cleanup
# Intro -------------------------------------------------------------------
#' @, desc = description
#' This script reads in the SSP data and renames the variables in the SSP file. It deletes all years except those in
#' keepYearList (with year 0 added)
#' It keeps population results only from the IIASA-WiC POP model.

#Copyright (C) 2015-2017 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#   GNU General Public License for more details at http://www.gnu.org/licenses/.
source("R/nutrientModFunctions.R")
sourceFile <- "dataPrep.SSP.R"
createScriptMetaData()

SSPcsv <-     fileNameList("SSPcsv")
SSPdataZip <-   fileNameList("SSPdataZip")
keepYearList <-  keyVariable("keepYearList")
#' need to include the n-1 year for the elasticity calculations for fish and alcohol
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X",year0,sep = "")
keepYearList <- c(year0,keepYearList)
modelListGDP <-  fileNameList("modelListGDP")
modelListPop <-   fileNameList("modelListPop")

dt.SSP <- as.data.table(readr::read_csv(unz(SSPdataZip, filename = SSPcsv), col_names = TRUE, guess_max = 2000, cols(
  `2000` = col_double(),
  `2005` = col_double()
)))

#' add X to the year column name so R is happy with years as column names
data.table::setnames(dt.SSP, make.names(names(dt.SSP)))

#' drop all years except those in keepYearList, created in dataPrep.setup.R
keepListCol <- c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT", keepYearList)
dt.SSP[, setdiff(names(dt.SSP), keepListCol) := NULL]
#make all names lower case and change region to ISO_code
oldNameList <- names(dt.SSP)
newNameList <- c("model", "scenario", "ISO_code", "variable", "unit", keepYearList)
data.table::setnames(dt.SSP, oldNameList, newNameList)

#' There are 21 scenarios; 4 each for SSP scenarios 1, 2, 3, and 5 and
#'  5 for SSP scenario 4.
#' This is the list of dt.SSP.regions3 scenarios
#' "SSPx_v9_130424" is from PIK and just for the US
#' "SSPx_v9_130325" is from OECD and is just GDP and population
#' "SSPx_v9_130219" is from IIASA and is just population and GDP
#' "SSPx_v9_130115" contains info from IIASA on population broken down by age,
#'  gender, and education and NCAR on population broken down by rural and urban.
#'  The IIASA data are from 2010 to 2100.
#'  Keep only the population results from IIASA for the age and gender breakdown and
#'  for the years 2010 to 2050.

#'  the lists of pop and GDP models are defined in nutrientModFunctions.R
#'  Note: the different models don't have results for the same set of countries
#'  The OECD Env-GrowthP population data set is only total but does include 2005.
#'  The IIASA-WiC POP data set includes age and gender distributions but doesn't include 2005.

# create cleaned up GDP SSP data -------

dt.SSP.GDP <- dt.SSP[model %in% modelListGDP & !variable %in% "Population",]
deleteListCol <- "model"
dt.SSP.GDP[, (deleteListCol) := NULL]
idVars <- c("scenario", "ISO_code", "variable", "unit")
dt.SSP.GDP.melt <- data.table::melt(
  dt.SSP.GDP,
  id.vars = idVars,
  variable.name = "year",
  value.name = "value.GDP",
  measure.vars = keepYearList,
  variable.factor = FALSE
)
#' change GDP|PPP to GDP because R doesn't like | to be used in a variable name
dt.SSP.GDP.melt[, variable := "GDP"]
data.table::setorder(dt.SSP.GDP.melt, scenario, ISO_code)
dt.SSP.GDP.melt <- dt.SSP.GDP.melt[!ISO_code %in% keyVariable("dropListCty"),]
inDT <- dt.SSP.GDP.melt
outName <- "dt.SSPGDPClean"
desc <- "GDP information by country and scenario with cleaned up column names"
cleanup(inDT,outName,fileloc("uData"), desc = desc)

# create cleaned up population SSP data -----
#' @param dt.SSP.pop - data table with the SSP results from the model identified in modelListPop
dt.SSP.pop <- dt.SSP[model == modelListPop,]

# Create population age and gender data set by removing rows with education breakdown -----
#' #' @param popList - variable name for population
#' popList <- "Population"

#' @param ageList - variable name for age group categories
# ageList <-
#  c(
#   "Aged0-4", "Aged5-9", "Aged10-14", "Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34",
#   "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64", "Aged65-69",
#   "Aged70-74","Aged75-79","Aged80-84","Aged85-89", "Aged90-94", "Aged95-99","Aged100+"
#  )

#' #' @param edList - variable name for education categories
#' edList <-
#'  c("No Education", "Primary Education", "Secondary Education","Tertiary Education")
#' genderList <- c("Male", "Female")

#' keep full population count around. Keep 2005 for the gdppercap operations
dt.SSP.pop.tot <-
  dt.SSP.pop[variable == "Population", c("scenario", "ISO_code",  keepYearList), with = FALSE]
data.table::setcolorder(dt.SSP.pop.tot, c("scenario", "ISO_code", "X2005", "X2010", "X2015", "X2020", "X2025",
                                          "X2030", "X2035", "X2040", "X2045", "X2050"))
#' deal with missing 2005 pop data in the IIASA data set by getting 2005 from the OECD data set
dt.SSP.pop.tot[, X2005 := NULL]
keepListCol <- c("scenario", "ISO_code", "X2005")
dt.SSP.pop.2005 <- dt.SSP[model == "OECD Env-Growth" & variable %in% "Population", (keepListCol), with = FALSE]
dt.SSP.pop.2005[, scenario := substr(scenario, 1, 4)]
dt.SSP.pop.tot[, scenario := substr(scenario, 1, 4)]
dt.SSP.pop.tot <- merge(dt.SSP.pop.tot, dt.SSP.pop.2005, by = c("scenario", "ISO_code"))
idVars <- c("scenario", "ISO_code")
dt.SSP.pop.tot.melt <- data.table::melt(
  dt.SSP.pop.tot,
  id.vars = idVars,
  variable.name = "year",
  value.name = "value.pop",
  measure.vars = keepYearList,
  variable.factor = FALSE
)

data.table::setnames(dt.SSP.pop.tot.melt, old = "ISO_code", new = "region_code.SSP")
dt.SSP.pop.tot.melt <- dt.SSP.pop.tot.melt[!region_code.SSP %in% keyVariable("dropListCty"),]
inDT <- dt.SSP.pop.tot.melt
outName <- "dt.SSP.pop.tot"
desc <- "Population information by country and scenario with cleaned up column names"
cleanup(inDT,outName,fileloc("uData"), desc = desc)

#' Remove the aggregates of
#' "Population", "Population|Female" and "Population|Male"
deleteListRow <-
  c("Population", "Population|Female", "Population|Male")
dt.SSP.pop.step1 <- dt.SSP.pop[!variable %in% deleteListRow,]

#' split the variable names apart where there is a |
#' (eg. X|Y becomes X and Y and new columns are created)
dt.SSP.pop.step2 <-
  splitstackshape::cSplit(dt.SSP.pop.step1,
                          'variable',
                          sep = "|",
                          type.convert = FALSE)

#' name the new columns created by the spliting process above
oldNames <-
  c("variable_1", "variable_2", "variable_3", "variable_4")
newNames <- c("population", "gender", "ageGenderCode", "education")
data.table::setnames(dt.SSP.pop.step2, oldNames, newNames)

#' rename variables to align with the requirements names
dt.SSP.pop.step2$ageGenderCode <-
  gsub("Aged", "", dt.SSP.pop.step2$ageGenderCode)
dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Female"] <-
  paste("F",
        dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Female"],
        sep = "")
dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Male"] <-
  paste("M",
        dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Male"],
        sep = "")
dt.SSP.pop.step2$ageGenderCode <-
  gsub("-", "_", dt.SSP.pop.step2$ageGenderCode)
dt.SSP.pop.step2$ageGenderCode <-
  gsub("\\+", "Plus", dt.SSP.pop.step2$ageGenderCode)
dt.SSP.pop.step2 <-
  dt.SSP.pop.step2[order(dt.SSP.pop.step2$ISO_code),]

#' remove rows that breakdown an age group by education
removeRowList <-
  c("No Education",
    "Primary Education",
    "Secondary Education",
    "Tertiary Education")
dt.SSP.pop.step2 <-
  dt.SSP.pop.step2[!dt.SSP.pop.step2$education %in% removeRowList,]

#' remove extraneous columns and keep only the ones needed
# this keepYearList includes year 0 (X2005) because it is needed for the fish and alcohol consumption calcs
keepList <- c("scenario", "ISO_code", "ageGenderCode", keepYearList)
deleteListCol <-
  c("model", "gender", "education", "population", "unit")
dt.SSP.pop.step2[, (deleteListCol) := NULL]
idVars <- c("scenario", "ISO_code", "ageGenderCode")
dt.SSP.pop.step2.melt <- data.table::melt(
  dt.SSP.pop.step2,
  id.vars = idVars,
  variable.name = "year",
  measure.vars = keepYearList,
  variable.factor = FALSE
)
dt.SSP.pop.step2.melt <- dt.SSP.pop.step2.melt[!ISO_code %in% keyVariable("dropListCty"),]
inDT <- dt.SSP.pop.step2.melt
outName <- "dt.SSPPopClean"
desc <- "Population information by country and scenario and age and gender with cleaned up column names"
cleanup(inDT,outName,fileloc("uData"), desc = desc)

finalizeScriptMetadata(metadataDT, sourceFile)
