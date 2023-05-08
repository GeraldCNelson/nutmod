# a test to see if the compiler helps with the automate script
# require(compiler)
# enableJIT(3)
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

#' @description {
#' A script to recreate all data. Each R script is sourced in the order needed.
#' }
description <- "A script to recreate all data. Each R script is sourced in the order needed."
sourceFile <- "automate.R"
# Where files are created
# Note: All files have a date suffix to the file name.
# file locations -
#   mData - data
#   iData - data/IMPACTdata - directory with IMPACT data
#   resultsDir - results/gdxDhoice
#   gDir = graphics/gdxChoice
# The metadata file in the results directory provides details on files and file locations used in the analysis
options(warn = 2) # converts all warnings to errors


#install needed packages
# replaced ggplot2 and readr with tidyverse which includes both. April 12, 2018
# list.of.packages <- c("data.table", "openxlsx", "dplyr", "dtplyr", "utils", "ggplot2", "stringi", "tidyr", "splitstackshape",
#                       "gridExtra","gplots", "RColorBrewer", "RODBC")
# packages not loaded in nutrientModFunctions.R
list.of.packages <- c("data.table", "openxlsx",  "utils",  "stringi", "splitstackshape", "grid",
                      "gridExtra","gplots", "RColorBrewer", "RODBC", "tidyverse", "sp", "sf", "broom", "raster", "rgeos", "rgdal", 
                      "ggthemes", "qdapRegex", "Cairo", "extrafont", "staplr", "spData", "fmsb", "mapview", "tmap", "tmaptools", "maps", 
                      "mapproj", "leaflet", "ggplot2", "ggiraph", "ggiraphExtra", "readxl", 
                      "shiny", "shinythemes", "shiny.router", "shinyjs", "DT", "officer", "magrittr"
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

cat("\nThe packages below are needed and currently available on CRAN or by downloading from github.\n")
cat("\nThis set of scripts needs version 1.9.7 or greater of the data.table package. Version", as.character(packageVersion("data.table")), "is currently being used.\n")
# print(paste("This set of scripts needs version 3.1.23 or greater of the openxlsx package. Version", as.character(packageVersion("openxlsx")), "is currently being used."))

if (packageVersion("openxlsx") < "4.1.3") {
  print("updating openxlsx")
  install.packages(c("Rcpp", "devtools"), dependencies = TRUE)
  require(devtools)
  install_github("ycphs/openxlsx")
}

if (packageVersion("data.table") < "1.12.0") {
  print("updating data.table")
  install.packages("data.table")
}

# the source command for the nutrient modeling code needs to be below the install code so R doesn't have to restart
# print(paste("start time is " , proc.time(), sep = ""))
cat("Running nutrientModFunctions.R\n")
source("R/nutrientModFunctions.R")

createScriptMetaData()
gdxChoice <- "SSPs"
metadata() # - creates dt.metaData; holds some key file locations and variable names;

# only needs to be run when new regions or aggregations are added
srcFile <- "dataPrep.regions.R"
sourcer(srcFile) # - creates dt.regions.all and the list of scenarios
# only needs to be run when a new SSP data file if obtained
srcFile <- "dataPrep.SSP.R"
sourcer(srcFile)
# only needs to be run when new FBS info from FAO arrives
srcFile <- "dataPrep.FBS.R"
# sourcer(srcFile) # - creates dt.FBS, mData
# only needs to be run if changes to the script are made
srcFile <- "dataManagement.fishnAlc.R"
sourcer(srcFile)
# only needs to be run if changes to the script are made
srcFile <- "dataPrep.ODBCaccess.R"
sourcer(srcFile) # reads in nutrient data from the USDA nutrient composition access database
srcFile <- "dataManagement.Pop.R" #name changed from dataManagement.SSPPop.R Oct 5, 2018 to reflect the fact that this also handles the combined UN/SSP age gender info
sourcer(srcFile) # mData - Nutrient requirements adjusted for population distribution, example is req.EAR.percap.2016-06-24.rds

 # srcFile <- "dataPrep.IMPACT.R" currently commehted out because the output files it creates only need to be done once
 # sourcer(srcFile)
# - creates files in iData
# paste(dt,varName, sep = ".") - one file for each IMPACT variable, example is perCapKcalPerDay.2016-06-21.rds
# dt.foodAvailability is created here. Just has food availability from gdx. dt.IMPACTfood adds the fish and alcoholic beverages

srcFile <- "dataManagement.IMPACT.R"
sourcer(srcFile) # adds fish and alcohol data, writes out IMPACT variables just for food items (names begin with c), and dt.IMPACTfood file
# Key output is dt.IMPACTfood
#dt.IMPACTfood, iData

srcFile <- "dataPrepFishStat.R"
sourcer(srcFile) # adds fish data for composite fish commodities, writes out IMPACT variables just for composite fish items (names begin with c),
# destination is fileloc("iData")

# # only run dataPrepFortification.R if the switch choice is three
# gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
# if (gdxSwitchCombo[3] == 3) {
#   
#   srcFile <- "dataPrepFortification.R"
#   sourcer(srcFile) # reads in data on where fortification occurs (not all types of fortification); writes out data file in appropriate format
# }

srcFile <- "dataPrepUSDANuts.R"
sourcer(srcFile)

srcFile <- "dataPrep.NutrientRequirements.R"
sourcer(srcFile) # newDFname, mData - nutrient requirements adjusted to SSP age and gender categories, example is req.RDA.macro.ssp.2016-06-22.rds

srcFile <- "dataManagement.foodNnuts.R"
sourcer(srcFile)

srcFile <- "nutrientCalcs.R"
sourcer(srcFile)

srcFile <- "nutCalcsProcessing.R"
sourcer(srcFile) 

srcFile <- "diversityMetrics.R"
sourcer(srcFile)

#creating list of .rds files in results.
cat("create list of .rds files in data\n\n")
source("R/nutrientModFunctions.R") # added here because clearmemory in diversityMetrics.R deletes all the functions
createScriptMetaData()
dt.resultsFiles <- data.table::as.data.table(list.files(path = fileloc("resultsDir"), pattern = "*.rds"))
data.table::setnames(dt.resultsFiles, old = "V1", new = "fileName")
dt.resultsFiles[, reqTypeName := gsub(".{15}$", "", dt.resultsFiles$fileName)]

# this csv file is hand edited. Don't delete!
descriptionLookup <- fread(paste(fileloc("rawData"), "descriptionLookup.csv", sep = "/"))
dt.resultsFiles <- merge(dt.resultsFiles,descriptionLookup, by = "reqTypeName", all.x = TRUE)
inDT <- dt.resultsFiles
outName <- "resultFileLookup"
desc <- " Description of contents of some files."
srcFile <- "automate.R"
cleanup(inDT, outName, fileloc("mData"), desc = desc)
finalizeScriptMetadata(metadataDT, srcFile)


srcFile <- "aggRun.R"
sourcer(srcFile)

srcFile <- "dataPrepSingleScenario.R" # compares CGE results to non-CGE results
sourcer(srcFile)

srcFile <- "finalGraphCreation.R"
sourcer(srcFile)

srcFile <- "dataPrep.metadata.R" # documents files created
sourcer(srcFile)

srcFile <- "RawDataInfo.R"
sourcer(srcFile)  # documents exogenous data used and what script uses it. Also a list of the scripts and what they do.

srcFile <- "copyFilestoNutrientModeling.R"
sourcer(srcFile)  # move results needed for the shiny app.R in the nutrientModeling folder


