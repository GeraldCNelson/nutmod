# script to recreate all data
# Where files are created
# Note: All files have a date suffix to the file name.
# file locations -
#   mData - data
#   iData - data/IMPACTdata - directory with IMPACT data
#   resultsDir - results

# print(paste("start time is " , proc.time(), sep = ""))
ptm <- proc.time()

#install needed packages
list.of.packages <- c("data.table", "openxlsx", "dplyr", "utils", "ggplot2", "stringi", "tidyr" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

# also need gdxrrw
gdxrrwText <- "The gdxrrw package is needed to run this. It is available at this url, not from CRAN.
https://support.gams.com/gdxrrw:interfacing_gams_and_r. Download the relevant file and use the following command to install
- install.packages('gdxrrw_1.0.0.tgz',repos = NULL). Replace gdxrrw_1.0.0.tgz with the
name of the file you downloaded. If you put it in the main directory of your project, the install.packages command will find it."

list.of.packages <- c("gdxrrw")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (!length(new.packages) == 0) {
  print(gdxrrwText)
  stop("gdxrrw package not installed")
  }

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

print("Running nutrientModFunctions.R")
source("R/nutrientModFunctions.R")

print("Running workbookFunctions.R")
source("R/workbookFunctions.R")

print("Running nutrientCalcFunctions.R")
source("R/nutrientCalcFunctions.R")

print("Running dataPrep.IMPACT.R")
source("R/dataPrep.IMPACT.R")
# - creates files in iData
# dt.IMPACTmetaData
# paste(dt,varName, sep = ".") - one file for each IMPACT variable, example is dt.PerCapKCAL.2016-06-21.rds
# dt.CSEs

print("Running dataPrep.SSP.R")
source("R/dataPrep.SSP.R")
# - creates files in mData
# dt.SSPGDPClean - SSP GDP data
# dt.SSP.pop.tot
# dt.SSPPopClean - SSP population data including age and gender groups
# dt.IMPACT159.pop.tot - total population by IMPACT 159 region from the SSP population data set; not any more

print("Running dataPrep.regions.R")
source("R/dataPrep.regions.R") # - creates dt.regions.all and the list of scenarios

metadata()
print("Running dataPrep.FBS.R")
source("R/dataPrep.FBS.R") # - creates dt.FBS, mData

print("Running dataManagement.fishnAlc.R")
source("R/dataManagement.fishnAlc.R")
# dt.fishIncElast, iData - to have a record of what fish income elasticities were used
# dt.fishScenarios, mData
# dt.alcIncElast, iData - to have a record of what alcohol income elasticities were used
# dt.alcScenarios, mData - alcohol consumption scenarios, for input into dataManagement.IMPACT.R

print("Running dataManagement.IMPACT.R")
source("R/dataManagement.IMPACT.R")
# adds fish and alcohol data, writes out IMPACT variables just for food items (names begin with c), and dt.IMPACTfood file
#paste(fileShortName, "food, sep = "."), iData - just data for food commodities, example is dt.CSEs.food.2016-06-21.rds
#dt.IMPACTfood, iData

print("Running dataPrep.nutrientData.R")
source("R/dataPrep.nutrientData.R") # - creates dt.cookingRet and dt.nutrients, mData. Note that
     # dt.nutrients does NOT take into account loss in cooking. That is done later and depends on a switch (search for switch.xxx .

print("Running dataPrep.NutrientRequirements.R")
source("R/dataPrep.NutrientRequirements.R")
# newDFname, mData - nutrient requirements adjusted to SSP age and gender categories, example is req.RDA.macro.ssp.2016-06-22.rds

print("Running dataManagement.SSPPop.R")
source("R/dataManagement.SSPPop.R")
#paste(gsub(".ssp","",nutReqName),"percap",sep = "."), mData - Nutrient requirements adjusted for population distribution, example is req.EAR.percap.2016-06-24.rds

print("Running nutrientCalcs.R")
source("R/nutrientCalcs.R")
#writes
# paste("food.agg.",reqShortName,sep = "") fileloc("resultsDir"), "csv")
# "dt.nutrients.sum", fileloc("resultsDir"))

print("Running nutCalcsProcessing.R")
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

#creating list of .rds files in data
dt.resultsFiles <- data.table::as.data.table(list.files(path = fileloc("resultsDir"), pattern = "*.rds"))
data.table::setnames(dt.resultsFiles, old = "V1", new = "fileName")
dt.resultsFiles[, reqTypeName := gsub(".{15}$","",dt.resultsFiles$fileName)]
# this csv file is hand edited. Don't delete!
descriptionLookup <- read.csv(paste(fileloc("rawData"), "descriptionLookup.csv", sep = "/"))
dt.resultsFiles <- merge(dt.resultsFiles,descriptionLookup, by = "reqTypeName")
inDT <- dt.resultsFiles
outName <- "resultFileLookup"
cleanup(inDT, outName, fileloc("mData"))

print("Copying files for shiny app")
source("R/copyFilestoNutrientModeling.R") # move results needed for the shiny app.R in the nutrientModeling folder

proc.time() - ptm
