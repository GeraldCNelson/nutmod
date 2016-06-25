# script to recreate all data
# Where files are created
# Note: All files have a date suffix to the file name.
# file locations -
#   mData - data
#   iData - data/IMPACTdata - directory with IMPACT data
#   resData - results

ptm <- proc.time()
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
# dt.SSPGDPClean
# dt.SSP.pop.tot
# dt.SSPPopClean
# dt.IMPACT159.pop.tot

source("R/dataPrep.nutrientData.R") # - creates dt.cookingRet and dt.nutrients, mData
source("R/dataPrep.NutrientRequirements.R")
# newDFname, mData - nutrient requirements adjusted to SSP age and gender categories, example is req.RDA.macro.ssp.2016-06-22.rds

source("R/dataManagement.SSPPop.R")
#paste(gsub(".ssp","",nutReqName),"percap",sep = "."), mData - Nutrient requirements adjusted for population distribution, example is req.EAR.percap.2016-06-24.rds
proc.time() - ptm
