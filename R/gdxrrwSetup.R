# this script must be sourced. Best to do it as part of automate.R
# choose gdx file
source("R/nutrientModFunctions.R")
sourceFile <- "gdxrrwSetup.R"
createScriptMetaData()

#IMPACTgdxfileName <- "Demand Results20150817.gdx" - old gdx
#gdxFileName <- fileNameList("IMPACTgdxfileName")
#gamsSetup() # to load GAMs stuff and create the initial list of IMPACT scenarios
gamsSetup <- function(gdxFileName) {

  # some of this code duplicates code in dataPrep.IMPACT.R. Not good.
  #  gdxrrw::igdx(gamsSysDir = fileNameList("R_GAMS_SYSDIR"), silent = TRUE)
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),gdxFileName, sep = "/")
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, "PWX0",ts = TRUE,
                                                           names = c("scenario", "IMPACT_code", "year", "value")))
  dt.ptemp <- data.table::as.data.table(rapply(dt.ptemp, as.character, classes = "factor", how = "replace"))
  if (!gdxFileName %in% "Micronutrient-Inputs-2018.22.05.gdx") {
    dt.ptemp[scenario %in% c("SSP1-NoCC", "SSP2-GFDL", "SSP2-HGEM","SSP2-HGEM2", "SSP2-IPSL", "SSP2-IPSL2",
                             "SSP2-MIROC", "SSP2-NoCC", "SSP3-NoCC"),
             scenario := paste(scenario, "-REF", sep = "")]
}else{
}
  keepListCol <- "scenario"

  dt.scenarioListIMPACT <- unique(dt.ptemp[, (keepListCol), with = FALSE])

#cleanup scenario names
dt.scenarioListIMPACT <- cleanupScenarioNames(dt.scenarioListIMPACT) # replaces - with _ in a couple of scenarios and removes 2 on a couple of USAID scenarios
#  scenarioComponents <- c("SSP", "climate_model", "experiment")
# #  suppressWarnings(
#     dt.scenarioListIMPACT[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
# #  )
#   # the code above recyles so you end up with the SSP value in experiment if this is a REF scenario
#   # the code below detects this and replaces the SSP value with REF
#   dt.scenarioListIMPACT[(SSP == experiment), experiment := "REF"]
#   dt.scenarioListIMPACT[, scenario := paste(SSP, climate_model, experiment, sep = "-")]
#   #  dt.ptemp <- merge(dt.ptemp, dt.scenarioListIMPACT, by = "scenario")
#   deleteListCol <- c("SSP", "climate_model", "experiment")
#   dt.scenarioListIMPACT[, (deleteListCol) := NULL]
inDT <- dt.scenarioListIMPACT
outName <- "dt.scenarioListIMPACT"
desc <- paste0("List of scenarios that can be used with ", gdxFileName)
cleanup(inDT, outName, fileloc("mData"), "csv", desc = desc)

# create list of IMPACT gdx params
temp <- gdxrrw::gdxInfo(
  gdxName = gdxFileLoc, dump = FALSE, returnList = FALSE, returnDF = TRUE)
# convert to data table and extract just the list of parameters
dt.gdx.param <- data.table::as.data.table(temp$parameters)
#  keepListCol <- c("catNames", "text") # remove index, dim, card, doms, and domnames
deleteListCol <- c("index", "card","doms", "domnames")
dt.gdx.param <- dt.gdx.param[, (deleteListCol) := NULL]
data.table::setnames(dt.gdx.param,old = c("name","text"), new = c("catNames", "description"))
inDT <- dt.gdx.param
outName <- "dt.IMPACTgdxParams"
desc <- paste0("Parameters from the gdx file", gdxFileName, "with IMPACT results")
cleanup(inDT,outName,fileloc("iData"), desc = desc)
}
#getGDXmetaData(gdxFileName)
gamsSetup(gdxFileName)

finalizeScriptMetadata(metadataDT, sourceFile)

