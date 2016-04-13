#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords SSP data cleanup
# Intro -------------------------------------------------------------------
#' @description
#' This script renames the variables in the SSP file and deletes all years except those in
#' keepYearList (with year 0 added)
#' It keeps population results only from the IIASA-WiC POP model.

#Copyright (C) 2015 Gerald C,Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function")) {source("R/nutrientModFunctions.R")}

SSPcsv <-         fileNameList("SSPcsv")
SSPdataZip <-     fileNameList("SSPdataZip")
keepYearList <-   keyVariable("keepYearList")
#need to include the n-1 year for the elasticity calculations
year1 <- as.numeric(substr(keepYearList[1], 2, 5)); year2 <- as.numeric(substr(keepYearList[2], 2, 5))
year0 <- year1 - (year2 - year1)
year0 <- paste("X",year0,sep="")
keepYearList <- c(year0,keepYearList)
modelListGDP <-   fileNameList("modelListGDP")
modelListPop <-     fileNameList("modelListPop")
temp <- utils::unzip(SSPdataZip, file = SSPcsv)
dt.SSP <-
  data.table::fread(temp, header = TRUE, stringsAsFactors = FALSE)
file.remove(temp)
# add X to the year column name
data.table::setnames(dt.SSP, make.names(names(dt.SSP)))

# drop all years except those in keepYearList, created in dataPrep.setup.R
colKeepList <-
  c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT", keepYearList)
dt.SSP <- dt.SSP[, colKeepList, with = FALSE]
#make all names lower case and change region to ISO_code
oldNameList <- names(dt.SSP)
newNameList <- c("model",
                 "scenario",
                 "ISO_code",
                 "variable",
                 "unit",
                 keepYearList)
data.table::setnames(dt.SSP, oldNameList, newNameList)

#There are 21 scenarios; 4 each for SSP scenarios 1, 2, 3, and 5 and
# 5 for SSP scenario 4.
#This is the list of dt.SSP.regions3 scenarios
#"SSPx_v9_130424" is from PIK and just for the US
#"SSPx_v9_130325" is from OECD and is just GDP and population
#"SSPx_v9_130219" is from IIASA and is just population and GDP
#"SSPx_v9_130115" contains info from IIASA on population broken down by age,
# gender, and education and NCAR on population broken down by rural and urban.
# The IIASA data are from 2010 to 2100.
# Keep only the population results from IIASA for the age and gender breakdown and
# for the years 2010 to 2050.

# the lists of pop and GDP models are defined in dataPrep.setup.R

# create cleaned up GDP SSP data ---
#' @param dt.SSP.GDP - data table with the SSP results from the model identified in modelListGDP
dt.SSP.GDP <- dt.SSP[model == modelListGDP,]
deleteListCol <- "model"
dt.SSP.GDP[, (deleteListCol) := NULL]
deleteListRow <- "Population"
dt.SSP.GDP <- dt.SSP.GDP[!variable %in% deleteListRow, ]
idVars <- c("scenario", "ISO_code", "variable", "unit")
dt.SSP.GDP.melt <- data.table::melt(
  dt.SSP.GDP,
  id.vars = idVars,
  variable.name = "year",
  measure.vars = keepYearList,
  variable.factor = FALSE
)
# change GDP|PPP to GDP because R doesn't like | to be used in a variable name
dt.SSP.GDP.melt[, variable := "GDP"]
data.table::setorder(dt.SSP.GDP.melt, scenario, ISO_code)
inDT <- dt.SSP.GDP.melt
outName <- "dt.SSPGDPClean"
cleanup(inDT,outName,fileloc("mData"))

# create cleaned up population SSP data ---
#' @param dt.SSP.pop - data table with the SSP results from the model identified in modelListPop
dt.SSP.pop <- dt.SSP[model == modelListPop,]
# Create population age and gender data set by removing rows with education breakdown -----
#' #' @param popList - variable name for population
#' popList <- "Population"

#' @param ageList - variable name for age group categories
# ageList <-
#   c(
#     "Aged0-4", "Aged5-9", "Aged10-14",  "Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34",
#     "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54",  "Aged55-59",  "Aged60-64", "Aged65-69",
#     "Aged70-74","Aged75-79","Aged80-84","Aged85-89", "Aged90-94", "Aged95-99","Aged100+"
#   )

#' #' @param edList - variable name for education categories
#' edList <-
#'   c("No Education", "Primary Education", "Secondary Education","Tertiary Education")
#' genderList <- c("Male", "Female")

#keep full population count around for bug checking later
dt.SSP.pop.tot <-
  dt.SSP.pop[variable == "Population", c("scenario", "ISO_code", "unit" , keepYearList), with = FALSE]
deleteListCol <- c("units","X2005")

idVars <- c("scenario", "ISO_code")
measure.vars = keepYearList
dt.SSP.pop.tot.melt <- data.table::melt(
  dt.SSP.pop.tot,
  id.vars = idVars,
  variable.name = "year",
  value.name = "value",
  measure.vars = measure.vars,
  variable.factor = FALSE
)

inDT <- dt.SSP.pop.tot.melt
outName <- "dt.SSP.pop.tot"
cleanup(inDT,outName,fileloc("mData"))

# Remove the aggregates of
# "Population", "Population|Female" and "Population|Male"
deleteListRow <-
  c("Population", "Population|Female", "Population|Male")
dt.SSP.pop.step1 <- dt.SSP.pop[!variable %in% deleteListRow,]

# split the variable names apart where there is a |
# (eg. X|Y becomes X and Y and new columns are created)
dt.SSP.pop.step2 <-
  splitstackshape::cSplit(dt.SSP.pop.step1,
                          'variable',
                          sep = "|",
                          type.convert = FALSE)

#name the new columns created by the spliting process above
oldNames <-
  c("variable_1", "variable_2", "variable_3", "variable_4")
newNames <- c("population", "gender", "ageGenderCode", "education")
data.table::setnames(dt.SSP.pop.step2, oldNames, newNames)

#rename variables to align with the requirements names
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

#remove rows that breakdown an age group by education
removeRowList <-
  c("No Education",
    "Primary Education",
    "Secondary Education",
    "Tertiary Education")
dt.SSP.pop.step2 <-
  dt.SSP.pop.step2[!dt.SSP.pop.step2$education %in% removeRowList,]

#remove extraneous columns and keep only the ones needed
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
inDT <- dt.SSP.pop.step2.melt
outName <- "dt.SSPPopClean"
cleanup(inDT,outName,fileloc("mData"))
