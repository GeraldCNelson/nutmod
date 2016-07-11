# script to recreate all data
# Where files are created
# Note: All files have a date suffix to the file name.
# file locations -
#   mData - data
#   iData - data/IMPACTdata - directory with IMPACT data
#   resData - results

ptm <- proc.time()
source("R/nutrientModFunctions.R")
source("R/workbookFunctions.R")
source("R/nutrientCalcFunctions.R")

metadata()
source("R/dataPrep.regions.R") # - creates dt.regions.all, mData
source("R/dataPrep.IMPACT.R")
# - creates files in iData
# dt.IMPACTmetaData
# paste(dt,varName, sep = ".") - one file for each IMPACT variable, example is dt.PerCapKCAL.2016-06-21.rds
# dt.CSEs

source("R/dataManagement.fish.R")
# dt.fishIncElast, iData - to have a record of what fish income elasticities were used
# dt.fishScenarios, mData

source("R/dataManagement.alcohol.R")
# dt.alcIncElast, iData - to have a record of what alcohol income elasticities were used
# dt.alcScenarios, mData - alcohol consumption scenarios, for input into dataManagement.IMPACT.R

source("R/dataManagement.IMPACT.R")
# adds fish and alcohol data, writes out IMPACT variables just for food items (names begin with c), and dt.IMPACTfood file
#paste(fileShortName, "food, sep = "."), iData - just data for food commodities, example is dt.CSEs.food.2016-06-21.rds
#dt.IMPACTfood, iData

source("R/dataPrep.FBS.R") # - creates dt.FBS, mData
source("R/dataPrep.SSP.R")
# - creates files in mData
# dt.SSPGDPClean - SSP GDP data
# dt.SSP.pop.tot
# dt.SSPPopClean - SSP population data including age and gender groups
# dt.IMPACT159.pop.tot - total population by IMPACT 159 region from the SSP population data set

source("R/dataPrep.nutrientData.R") # - creates dt.cookingRet and dt.nutrients, mData
source("R/dataPrep.NutrientRequirements.R")
# newDFname, mData - nutrient requirements adjusted to SSP age and gender categories, example is req.RDA.macro.ssp.2016-06-22.rds

source("R/dataManagement.SSPPop.R")
#paste(gsub(".ssp","",nutReqName),"percap",sep = "."), mData - Nutrient requirements adjusted for population distribution, example is req.EAR.percap.2016-06-24.rds

source("R/nutrientCalcs.R")
#writes
# paste("food.agg.",reqShortName,sep = "") fileloc("resData"), "csv")
# "dt.nutrients.sum", fileloc("resData"))

source("R/nutCalcsProcessing.R")
#writes
# paste(reqShortName, "all.sum", sep = ".")
# paste(reqShortName, "sum.req.ratio", sep = ".")
# paste(reqShortName, "all.ratio", sep = ".")
# paste(reqShortName, "all.req.ratio", sep = ".")
# cleanup(inDT, outName, fileloc("resData"))
# "all.req.ratio.cMax"
# cleanup(inDT, outName, fileloc("resData"))
"all.req.ratio.cMin"
# dt.energy.ratios - ratio of kcals from specific sources to total kcals

source("R/copyFilestoNutrientModeling.R") # move results needed for the shiny app.R in the nutrientModeling folder
proc.time() - ptm
