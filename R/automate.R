# a test to see if the compiler helps with the automate script
require(compiler)
enableJIT(3)
ls(all.names = TRUE)
#' Nutrient Modeling automation script
#' @title "Functions needed to make the nutrientModeling shiny app work"
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup, automation
# Intro ---------------------------------------------------------------
#Copyright (C) 2016-2018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

#' @description A script to recreate all data. Each R script is sourced in the order needed.
#' gdxFileNameChoice lets the user chose which gdx to get IMPACT data from.
# Where files are created
# Note: All files have a date suffix to the file name.
# file locations -
#   mData - data
#   iData - data/IMPACTdata - directory with IMPACT data
#   resultsDir - results/gdxDhoice
#   gDir = graphics/gdxChoice
# The metadata file in the results directory provides details on files and file locations used in the analysis
options(warn = 2) # converts all warnings to errors
sourceFile <- "automate.R"

ptm <- proc.time()

#install needed packages
# replaced ggplot2 and readr with tidyverse which includes both. April 12, 2018
# list.of.packages <- c("data.table", "openxlsx", "dplyr", "dtplyr", "utils", "ggplot2", "stringi", "tidyr", "splitstackshape",
#                       "gridExtra","gplots", "RColorBrewer", "RODBC")
list.of.packages <- c("data.table", "openxlsx", "dplyr", "dtplyr", "utils",  "stringi", "splitstackshape",
                      "gridExtra","gplots", "RColorBrewer", "RODBC", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

cat("\nThe packages below are needed and currently available on CRAN or by downloading from github.\n")
cat("\nThis set of scripts needs version 1.9.7 or greater of the data.table package. Version", as.character(packageVersion("data.table")), "is currently being used.\n")
# print(paste("This set of scripts needs version 3.1.23 or greater of the openxlsx package. Version", as.character(packageVersion("openxlsx")), "is currently being used."))

if (packageVersion("openxlsx") < "4.0.17") {
  print("updating openxlsx")
  install.packages(c("Rcpp", "devtools"), dependencies = TRUE)
  require(devtools)
  install_github("awalker89/openxlsx")
}

if (packageVersion("data.table") < "1.10.0") {
  print("updating data.table")
  install.packages("data.table")
}

# the source code needs to be below the install code so R doesn't have to restart
# print(paste("start time is " , proc.time(), sep = ""))
cat("Running nutrientModFunctions.R")
source("R/nutrientModFunctions.R")

createScriptMetaData()

gdxrrwExistenceCheck() #checks if the gdxrrw package is installed; if not, prints directions on how to install and stops.
#choose between 1 of 2 possible gdx files.
# 1 -  Micronutrient-Inputs-07252016.gdx;
#2 - Micronutrient-Inputs-USAID.gdx
gdxCombo <- gdxFileNameChoice()
gdxFileName <- gdxCombo[1]
gdxChoice <- gdxCombo[2]
sourceFile <- "R/automate.R"
metadata() # - creates dt.metaData; holds some key file locations and variable names;
# needs to come after identification of gdxFileName and before library location check
gdxLibraryLocationCheck()

# the gdxrrwSetup.R script needs to be separate because shiny can't deal with the gams package.
sourceFile <- "R/gdxrrwSetup.R"
sourcer(sourceFile)

sourceFile <- "R/dataPrep.IMPACT.R" # creates dt.scenarioListIMPACT and dt.IMPACTgdxParams
sourcer(sourceFile)
# - creates files in iData
# dt.IMPACTmetaData
# paste(dt,varName, sep = ".") - one file for each IMPACT variable, example is dt.PerCapKCAL.2016-06-21.rds
# dt.foodAvailability is created here. Just has food availability from gdx. dt.IMPACTfood adds the fish and alcoholic beverages

sourceFile <- "R/dataPrep.SSP.R"
sourcer(sourceFile)
# - creates files in mData
# dt.SSPGDPClean - SSP GDP data
# dt.SSP.pop.tot
# dt.SSPPopClean - SSP population data including age and gender groups
# dt.IMPACT159.pop.tot - total population by IMPACT 159 region from the SSP population data set; not any more

sourceFile <- "R/dataPrep.regions.R"
sourcer(sourceFile) # - creates dt.regions.all and the list of scenarios

sourceFile <- "R/dataPrep.FBS.R"
sourcer(sourceFile) # - creates dt.FBS, mData

sourceFile <- "R/dataManagement.fishnAlc.R"
sourcer(sourceFile) # dt.fishIncElast, iData - to have a record of what fish income elasticities were used
# dt.fishScenarios, mData
# dt.alcIncElast, iData - to have a record of what alcohol income elasticities were used
# dt.alcScenarios, mData - alcohol consumption scenarios, for input into dataManagement.IMPACT.R

sourceFile <- "R/dataManagement.IMPACT.R"
sourcer(sourceFile) # adds fish and alcohol data, writes out IMPACT variables just for food items (names begin with c), and dt.IMPACTfood file
# Key output is dt.IMPACTfood
#dt.IMPACTfood, iData

# sourceFile <- "R/cFreshD_cleanup.R"
# sourcer(sourceFile) ## match up item name and item codes. The item names are in the comp recalc spreadsheets but item code is not.
# Just need to do this for Fresh fish. c_FreshD. Not needed because results already recorded in dt.compositesLU file.

sourceFile <- "R/dataPrepFishStat.R"
sourcer(sourceFile) # adds fish data for composite fish commodities, writes out IMPACT variables just for composite fish items (names begin with c),
# destination is fileloc("iData")

sourceFile <- "R/dataPrep.ODBCaccess.R"
sourcer(sourceFile) # reads in nutrient data from the USDA nutrient composition access database

# replaced by dataPrepUSDANuts.R feb 25, 2018
# cat("Running dataManagement.ODBCaccess.R\n\n")
# source("R/dataManagement.ODBCaccess.R")
#Manipulates the results of the ODBC_access script and prepares dt.nutrients for later scripts

sourceFile <- "R/dataPrepFortification.R"
sourcer(sourceFile) # reads in data on where fortification occurs (not all types of fortification); writes out data file in appropriate format
# and also creates facet maps with this information

sourceFile <- "R/dataPrepUSDANuts.R"
sourcer(sourceFile)

sourceFile <- "R/dataPrep.NutrientRequirements.R"
sourcer(sourceFile) # newDFname, mData - nutrient requirements adjusted to SSP age and gender categories, example is req.RDA.macro.ssp.2016-06-22.rds

sourceFile <- "R/dataManagement.SSPPop.R"
sourcer(sourceFile) #paste(gsub(".ssp","",nutReqName),"percap",sep = "."), mData - Nutrient requirements adjusted for population distribution, example is req.EAR.percap.2016-06-24.rds

sourceFile <- "R/dataManagement.foodNnuts.R"
sourcer(sourceFile) #resultsD - creates dt.foodNnuts, dt.nutrients.kcals, dt.nutrients.sum.all, dt.nutrients.sum.staples,
# dt.nutrients.nonstapleShare, dt.foodAvail.foodGroup

sourceFile <- "R/nutrientCalcs.R"
sourcer(sourceFile)
#writes
# paste("food.agg.",reqShortName,sep = "") fileloc("resultsDir"), "csv")
# "dt.nutrients.sum", fileloc("resultsDir"))

sourceFile <- "R/nutCalcsProcessing.R"
sourcer(sourceFile) # should probably be rewritten so it doesn't take so long to run
#writes
# paste(reqShortName, "all.sum", sep = ".")
# paste(reqShortName, "sum.req.ratio", sep = ".")
# paste(reqShortName, "all.ratio", sep = ".")
# paste(reqShortName, "all.req.ratio", sep = ".")
# cleanup(inDT, outName, fileloc("resultsDir"))
# "all.req.ratio.cMax"
# cleanup(inDT, outName, fileloc("resultsDir"))
"all.req.ratio.cMin"
# dt.energy.ratios - ratio of kcals from specific sources to total kcals

sourceFile <- "R/diversityMetrics.R"
sourcer(sourceFile)

#creating list of .rds files in results
cat("create list of .rds files in data\n\n")
dt.resultsFiles <- data.table::as.data.table(list.files(path = fileloc("resultsDir"), pattern = "*.rds"))
data.table::setnames(dt.resultsFiles, old = "V1", new = "fileName")
dt.resultsFiles[, reqTypeName := gsub(".{15}$","",dt.resultsFiles$fileName)]

# this csv file is hand edited. Don't delete!
descriptionLookup <- fread(paste(fileloc("rawData"), "descriptionLookup.csv", sep = "/"))
dt.resultsFiles <- merge(dt.resultsFiles,descriptionLookup, by = "reqTypeName", all.x = TRUE)
inDT <- dt.resultsFiles
outName <- "resultFileLookup"
desc <- " Description of contents of some files."
cleanup(inDT, outName, fileloc("mData"), desc = desc)

sourceFile <- "R/copyFilestoNutrientModeling.R"
sourcer(sourceFile)  # move results needed for the shiny app.R in the nutrientModeling folder

library(dtplyr)# generate graphs
sourceFile <- "R/aggRun.R"
sourcer(sourceFile)

sourceFile <- "R/finalGraphCreation.R"
sourcer(sourceFile)

sourceFile <- "R/dataPrep.metadata.R"
sourcer(sourceFile)


