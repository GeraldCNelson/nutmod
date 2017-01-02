#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2016 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

#' @description A script to recreate all data. Each R script is sourced in the order needed. gdxFileNameChoice lets the user chose which gdx to get IMPACT data from.
# Where files are created
# Note: All files have a date suffix to the file name.
# file locations -2
#   mData - data
#   iData - data/IMPACTdata - directory with IMPACT data
#   resultsDir - results

options(warn = 2) # converts all warnings to errors
ptm <- proc.time()

#install needed packages
list.of.packages <- c("data.table", "openxlsx", "dplyr", "utils", "ggplot2", "stringi", "tidyr", "splitstackshape",
                      "gridExtra","gplots")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

print("The packages below are needed and currently available on CRAN or by downloading from github")
print(paste("This set of scripts needs version 1.9.7 or greater of the data.table package. Version", as.character(packageVersion("data.table")), "is currently being used."))
# print(paste("This set of scripts needs version 3.1.23 or greater of the openxlsx package. Version", as.character(packageVersion("openxlsx")), "is currently being used."))

if (unlist(packageVersion("openxlsx")[[1]])[2] < 2 & unlist(packageVersion("openxlsx")[[1]])[3] < 23) {
  unlist(strsplit(packageVersion("openxlsx"), ".", fixed = TRUE))[3]
  print("updating openxlsx")
  install.packages(c("Rcpp", "devtools"), dependencies = TRUE)
  require(devtools)
  install_github("awalker89/openxlsx")
}

if (unlist(packageVersion("data.table")[[1]])[2] < 9) {
  unlist(strsplit(packageVersion("openxlsx"), ".", fixed = TRUE))[3]
  print("updating data.table")
  install.packages("data.table")
}

# the source code needs to be below the install code so R doesn't have to restart
# print(paste("start time is " , proc.time(), sep = ""))
print("Running nutrientModFunctions.R")
source("R/nutrientModFunctions.R")

print("Running workbookFunctions.R")
source("R/workbookFunctions.R")

print("Running nutrientCalcFunctions.R")
source("R/nutrientCalcFunctions.R")

gdxrrwExistenceCheck() #checks if the gdxrrw package is installed; if not, prints directions on how to install and stops.
#choose between 1 of 2 possible gdx files. 1 -  Micronutrient-Inputs-07252016.gdx; 2 - Micronutrient-Inputs-USAID.gdx
gdxFileName <- gdxFileNameChoice()
metadata() # - creates dt.metaData; holds some key file locations and variable names;
# needs to come after identification of gdxFileName and before library location check
gdxLibraryLocationCheck()

# the gdxrrwSetup.R script needs to be separate because shiny can't deal with the gams package.
source("R/gdxrrwSetup.R") # creates dt.scenarioListIMPACT and dt.IMPACTgdxParams
cat("Running dataPrep.IMPACT.R\n\n")
source("R/dataPrep.IMPACT.R")
# - creates files in iData
# dt.IMPACTmetaData
# paste(dt,varName, sep = ".") - one file for each IMPACT variable, example is dt.PerCapKCAL.2016-06-21.rds
# dt.CSEs

cat("Running dataPrep.SSP.R\n\n")
source("R/dataPrep.SSP.R")
# - creates files in mData
# dt.SSPGDPClean - SSP GDP data
# dt.SSP.pop.tot
# dt.SSPPopClean - SSP population data including age and gender groups
# dt.IMPACT159.pop.tot - total population by IMPACT 159 region from the SSP population data set; not any more

cat("Running dataPrep.regions.R\n\n")
source("R/dataPrep.regions.R") # - creates dt.regions.all and the list of scenarios

cat("Running dataPrep.FBS.R\n\n")
source("R/dataPrep.FBS.R") # - creates dt.FBS, mData

cat("Running dataManagement.fishnAlc.R\n\n")
source("R/dataManagement.fishnAlc.R")
# dt.fishIncElast, iData - to have a record of what fish income elasticities were used
# dt.fishScenarios, mData
# dt.alcIncElast, iData - to have a record of what alcohol income elasticities were used
# dt.alcScenarios, mData - alcohol consumption scenarios, for input into dataManagement.IMPACT.R

cat("Running dataManagement.IMPACT.R\n\n")
source("R/dataManagement.IMPACT.R")
# adds fish and alcohol data, writes out IMPACT variables just for food items (names begin with c), and dt.IMPACTfood file
#paste(fileShortName, "food, sep = "."), iData - just data for food commodities, example is dt.CSEs.food.2016-06-21.rds
#dt.IMPACTfood, iData

cat("Running dataPrep.ODBCaccess.R\n\n")
source("R/dataPrep.ODBCaccess.R")
# reads in nutrient data from the USDA nutrient composition access database

cat("Running dataManagement.ODBCaccess.R\n\n")
source("R/dataManagement.ODBCaccess.R")
#Manipulates the results of the ODBC_access script and prepare for dataPrep.nutrientData.R

# print("Running dataPrep.nutrientData.R")
# source("R/dataPrep.nutrientData.R") # - creates dt.cookingRet and dt.nutrients, mData. Note that
# # dt.nutrients does NOT take into account loss in cooking. That is done later and depends on a switch (search for switch.xxx .

cat("Running dataPrep.NutrientRequirements.R\n\n")
source("R/dataPrep.NutrientRequirements.R")
# newDFname, mData - nutrient requirements adjusted to SSP age and gender categories, example is req.RDA.macro.ssp.2016-06-22.rds

# print("Running bioavail.R")
# source("R/bioavail.R")
# # does adjustments to iron and zinc for bioavailability. Results are in files called PR.xxx. Calculation are now done in
# nutrientCalcs.R

cat("Running dataManagement.SSPPop.R\n\n")
source("R/dataManagement.SSPPop.R")
#paste(gsub(".ssp","",nutReqName),"percap",sep = "."), mData - Nutrient requirements adjusted for population distribution, example is req.EAR.percap.2016-06-24.rds

cat("Running nutrientCalcs.R\n\n")
source("R/nutrientCalcs.R")
#writes
# paste("food.agg.",reqShortName,sep = "") fileloc("resultsDir"), "csv")
# "dt.nutrients.sum", fileloc("resultsDir"))

cat("Running nutCalcsProcessing.R\n\n")
source("R/nutCalcsProcessing.R")
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

cat("Running diversityMetrics.R\n\n")
source("R/diversityMetrics.R")
#creating list of .rds files in data
cat("create list of .rds files in data\n\n")
dt.resultsFiles <- data.table::as.data.table(list.files(path = fileloc("resultsDir"), pattern = "*.rds"))
data.table::setnames(dt.resultsFiles, old = "V1", new = "fileName")
dt.resultsFiles[, reqTypeName := gsub(".{15}$","",dt.resultsFiles$fileName)]

# this csv file is hand edited. Don't delete!
descriptionLookup <- fread(paste(fileloc("rawData"), "descriptionLookup.csv", sep = "/"))
dt.resultsFiles <- merge(dt.resultsFiles,descriptionLookup, by = "reqTypeName")
inDT <- dt.resultsFiles
outName <- "resultFileLookup"
cleanup(inDT, outName, fileloc("mData"))
print("Copying files for shiny app")
source("R/copyFilestoNutrientModeling.R") # move results needed for the shiny app.R in the nutrientModeling folder

# generate graphs
source("R/aggRun.R")

print("Copying files to Rnw directory for Sweave")
source("R/copyFilestoSweavedDir.R")

# create pdfs of graphics to be pasted into the word doc. They are located in the Rnw directory
library(cacheSweave )
origWD <- getwd()
RnwWD <- paste0(origWD,"/Rnw")
setwd(paste0(origWD,"/Rnw"))
Sweave( "compileVitsAdequacy.Rnw", driver = cacheSweaveDriver, encoding = "utf-8")
Sweave( "compileMinrlsAdequacy.Rnw", driver = cacheSweaveDriver, encoding = "utf-8")
Sweave( "compileMacroAdequacy.Rnw", driver = cacheSweaveDriver, encoding = "utf-8")
Sweave( "compileFoodgroupAdequacy.Rnw", driver = cacheSweaveDriver, encoding = "utf-8")
Sweave( "compileDiversity.Rnw", driver = cacheSweaveDriver, encoding = "utf-8")
Sweave( "compileNBS.Rnw", driver = cacheSweaveDriver, encoding = "utf-8")

tools::texi2pdf("compileVitsAdequacy.tex", clean = FALSE, quiet = TRUE)
tools::texi2pdf("compileMinrlsAdequacy.tex", clean = FALSE, quiet = TRUE)
tools::texi2pdf("compileMacroAdequacy.tex", clean = FALSE, quiet = TRUE)
tools::texi2pdf("compileFoodgroupAdequacy.tex", clean = FALSE, quiet = TRUE)
tools::texi2pdf("compileDiversity.tex", clean = FALSE, quiet = TRUE)
tools::texi2pdf("compileNBS.tex", clean = FALSE, quiet = TRUE)
setwd(origWD)
options(warn = 1)
