#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name aggRun.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
source("R/aggNorder.R")
library(data.table)
# gdxChoice values are either SSPs or USAID
# DTglobal choices are dt.budgetShare,
# aggChoices are regionCodes156, regionCodes11, regionCodesAggReg, regionCodesAggReg2, regionCodes2EconGroup, regionCodesWB

# scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
# scenChoice for SSPs is not needed; there are just the six scenarios
# test values
gdxChoice <- "SSPs"
DTglobal <- "dt.budgetShare"
aggChoice <- "regionCodesWB"
scenChoice <- "scenarioList.prodEnhance"
aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice)
