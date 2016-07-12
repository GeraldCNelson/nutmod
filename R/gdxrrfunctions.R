gamsSetup <- function() {

  R_GAMS_SYSDIR <- fileNameList("R_GAMS_SYSDIR")
  gdxrrw::igdx(gamsSysDir = R_GAMS_SYSDIR, silent = TRUE)
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(fileNameList("IMPACTgdx"), "PWX0",ts = TRUE,
         names = c("scenario", "IMPACT_code", "year", "value")))
  #cleanup scenario names
  dt.ptemp <- cleanupScenarioNames(dt.ptemp)
 scenarioListIMPACT <- data.table::as.data.table(unique(dt.ptemp$scenario))
 write.csv(scenarioListIMPACT, file = paste(fileloc("mData"),"scenarioListIMPACT.csv",
                                            sep = "/"), row.names = FALSE)
}


