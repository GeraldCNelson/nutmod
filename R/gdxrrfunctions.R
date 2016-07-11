gamsSetup <- function() {

  R_GAMS_SYSDIR <- fileNameList("R_GAMS_SYSDIR")
  gdxrrw::igdx(gamsSysDir = R_GAMS_SYSDIR, silent = TRUE)
  dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(fileNameList("IMPACTgdx"), "PWX0",ts = TRUE,
                                                           names = c("scenario", "IMPACT_code", "year", "value")))
  #cleanup scenario names
  dt.ptemp[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.ptemp[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.ptemp[, scenario := gsub("HGEM2", "HGEM", scenario)]
  inDT <- dt.ptemp
  outName <- "dt.ptemp"
    # this kludge is to get the initial file created
  if (!"dt.ptemp" %in% list.files(fileloc("mData"))) {
    saveRDS(dt.ptemp, file = paste(fileloc("mData"), "/", outName, ".", Sys.Date(), ".rds", sep = ""))
  }

  cleanup(inDT, outName, fileloc("mData"))
}


