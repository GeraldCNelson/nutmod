gamsSetup <- function(gdxFileName) {
  gdxrrw::igdx(gamsSysDir = fileNameList("R_GAMS_SYSDIR"), silent = TRUE)
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),gdxFileName, sep = "/")
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, "PWX0",ts = TRUE,
                                                           names = c("scenario", "IMPACT_code", "year", "value")))
  #cleanup scenario names
  dt.ptemp <- cleanupScenarioNames(dt.ptemp)
  dt.scenarioListIMPACT <- data.table::as.data.table(unique(dt.ptemp$scenario))
  data.table::setnames(dt.scenarioListIMPACT, old = "V1", new = "scenario")
  inDT <- dt.scenarioListIMPACT
  outName <- "dt.scenarioListIMPACT"
  cleanup(inDT, outName, fileloc("mData"), "csv")
}
