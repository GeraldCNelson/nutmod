# this script must be sourced. Best to do it as part of automate.R
# choose gdx file
source("R/nutrientModFunctions.R")

getgdxFileNameChoice <- function() {
  cat("Choose the IMPACT data gdx file you want to use.\n")
  cat("1. for the nutrient modeling paper\n")
  cat("2. for the USAID nutrient modeling paper\n")
  cat("Note: the relevant gdx file must be in the data-raw/IMPACTdata directory\n")
  choice <- readline(prompt = "Choose the number of the gdx file you want to use. \n")
  if (choice == "1") gdxFileName <- "Micronutrient-Inputs-07252016.gdx" #- gdx with multiple SSP results
  if (choice == "2") gdxFileName <- "Micronutrient-Inputs-USAID.gdx"  #-  gdx for the USAID results
  return(gdxFileName)
}

gdxFileName <- getgdxFileNameChoice()

#IMPACTgdxfileName <- "Demand Results20150817.gdx" - old gdx
#gdxFileName <- fileNameList("IMPACTgdxfileName")
#gamsSetup() # to load GAMs stuff and create the initial list of IMPACT scenarios
gamsSetup <- function(gdxFileName) {
  gdxrrw::igdx(gamsSysDir = fileNameList("R_GAMS_SYSDIR"), silent = TRUE)
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),gdxFileName, sep = "/")
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, "PWX0",ts = TRUE,
                                                           names = c("scenario", "IMPACT_code", "year", "value")))
  keeplistCol <- "scenario"
  dt.scenarioListIMPACT <- dt.ptemp[, keeplistCol, with = FALSE]

  #cleanup scenario names
  dt.scenarioListIMPACT <- cleanupScenarioNames(dt.scenarioListIMPACT)
  scenarioComponents <- c("SSP", "climate_model", "experiment")
  suppressWarnings(
    dt.scenarioListIMPACT[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
  )
  # the code above recyles so you end up with the SSP value in experiment if this is a REF scenario
  # the code below detects this and replaces the SSP value with REF
  dt.scenarioListIMPACT[(SSP == experiment), experiment := "REF"]
  dt.scenarioListIMPACT[, scenario := paste(SSP, climate_model, experiment, sep = "-")]
  #  dt.ptemp <- merge(dt.ptemp, dt.scenarioListIMPACT, by = "scenario")
  deleteListCol <- c("SSP", "climate_model", "experiment")
  dt.scenarioListIMPACT[, (deleteListCol) := NULL]
  dt.scenarioListIMPACT <- unique(dt.scenarioListIMPACT)
  inDT <- dt.scenarioListIMPACT
  outName <- "dt.scenarioListIMPACT"
  cleanup(inDT, outName, fileloc("mData"), "csv")
}
gamsSetup(gdxFileName)


