#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name aggRun.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
  source("R/aggNorder.R")}
library(data.table)
library(optimx)
library(lpSolveAPI)
# see http://lpsolve.sourceforge.net/5.5/R.htm for using lpsolve

# gdxChoice values are either SSPs or USAID
dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
gdxChoice <- dt.metadata[file_description %in% "project source of gdx-based demand data",  file_name_location]

dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")

# keep just one country, one year, and one scenario
dt.IMPACTfood <- dt.IMPACTfood[scenario %in% "SSP2-NoCC-REF" & region_code.IMPACT159 %in% "ARG" & year %in% "X2010", ]
 # switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
 # switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data

 loadSwitchValues()
 #' dt.nutrients.adj is per kg of the raw product after IMPACT conversion and edible portion adjustments applied)
dt.nutrients.adj <- switches() # used only for disqualifying nutrients

reqsListPercap <- keyVariable("reqsListPercap")
deleteListReqs <- c("req.UL.vits_percap", "req.UL.minrls_percap",  "req.AMDR_hi_percap", "req.AMDR_lo_percap", "req.MRVs_percap")
dt.nutsReqPerCap <- getNewestVersion(req)


dt.temp <- data.table::copy(dt.nutsReqPerCap[FALSE,])
for (i in scenarioListIMPACT) {
  SSPName <- unlist(strsplit(i, "-"))[1] # get SSP abbrev
  climModel <- unlist(strsplit(i, "-"))[2] # get climate model abbrev
  experiment <- unlist(strsplit(i, "-"))[3] # get experiment abbrev
  # if (is.na(experiment)) {experiment <- "REF"}
  # print(experiment)
  # may need to add the RCP column later. Currently it's not included in the scenario name.
  temp.nuts <- data.table::copy(dt.nutsReqPerCap)
  temp.nuts <- temp.nuts[scenario == SSPName, ]
  # if (is.na(experiment)) {
  # temp.nuts[,scenario := paste(scenario,climModel, sep = "-")]
  # } else {
  # temp.nuts[,scenario := paste(scenario,climModel, experiment, sep = "-")]
  # }
  temp.nuts[,scenario := paste(SSPName, climModel, experiment, sep = "-")]
  dt.temp <- rbind(dt.temp, temp.nuts)
}
dt.nutsReqPerCap <- dt.temp[,keepListCol, with = FALSE][scenario %in% scenarioListIMPACT,]
