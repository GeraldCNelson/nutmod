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
  dt.scenarioListIMPACT[, scenarioNew := paste(SSP, climate_model, experiment, sep = "-")]
#  dt.ptemp <- merge(dt.ptemp, dt.scenarioListIMPACT, by = "scenario")
  deleteListCol <- c("SSP", "climate_model", "experiment", "scenario")
  dt.scenarioListIMPACT[, (deleteListCol) := NULL]
  data.table::setnames(dt.scenarioListIMPACT, old = c("scenarioNew"), new = c("scenario"))
  dt.scenarioListIMPACT <- unique(dt.scenarioListIMPACT)
  inDT <- dt.scenarioListIMPACT
  outName <- "dt.scenarioListIMPACT"
  cleanup(inDT, outName, fileloc("mData"), "csv")
}
