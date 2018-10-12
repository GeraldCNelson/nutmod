# choose gdx file and create the list of scenarios and dt.IMPACTgdxParams
source("R/nutrientModFunctions.R")
sourceFile <- "gdxrrwSetup.R"
createScriptMetaData()
library(gdxrrw)

#IMPACTgdxfileName <- "Demand Results20150817.gdx" - old gdx
#gdxFileName <- fileNameList("IMPACTgdxfileName")
#gamsSetup() # to load GAMs stuff and create the initial list of IMPACT scenarios
gdxFileName <- getGdxFileName(gdxChoice)
GAMSloc <- fileNameList("R_GAMS_SYSDIR")
igdx(gamsSysDir = GAMSloc, silent = TRUE)
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
    keepListCol <- "scenario"
    dt.scenarioListIMPACT <- unique(dt.ptemp[, (keepListCol), with = FALSE])
  }
 # if (gdxFileName %in% "Micronutrient-Inputs-2018.21.06.gdx") {
    if (gdxFileName %in% "Micronutrient-Inputs-7.1.2018.gdx") {
      keepListCol <- "scenario"
    dt.scenarioListIMPACT <- unique(dt.ptemp[, (keepListCol), with = FALSE])
    dt.scenarioListIMPACT <- dt.scenarioListIMPACT[!scenario %in% c("GLOBE_SSP2-HGEM-bnpl-SG-25f", "GLOBE_SSP2-HGEM-puls-SG-25f")]
    dt.scenarioListIMPACT[, crop := tstrsplit(scenario, "-", fixed = TRUE, keep = c(3))]
    SSPName <- "SSP2"
    climModel <- "HGEM"
    dt.scenarioListIMPACT[, scenario := paste(SSPName, climModel, paste0("c", crop), sep = "-")]
    dt.scenarioListIMPACT[, crop := NULL]
    }

  if (gdxFileName %in% "BMGF-Africa-NutMod-Inputs-2018.09.21.gdx") {
    keepListCol <- "scenario"
    dt.scenariosLookup  <- as.data.table(read_excel("data-raw/AfricanAgFutures/scenlookupAfrAgFutures.xlsx")) 
    dt.scenarioListIMPACT <- dt.scenariosLookup[, "substantiveNames"][!substantiveNames %in% c("SSP3Afr_base_CC", "SSP1Afr_base_CC"),]
  setnames(dt.scenarioListIMPACT, old = "substantiveNames", new = "scenario")
  # dt.ptemp <- dt.ptemp[!scenario %in% c("AfrAgFutures_scnr09", "AfrAgFutures_scnr10")]
  # for (i in 1:nrow(dt.scenariosLookup)) {
  # dt.ptemp <- dt.ptemp[scenario %in% dt.scenariosLookup$basicNames[i], scenario := dt.scenariosLookup$substantiveNames[i]]
  # }
  # SSPName <- "SSP2"
    # climModel <- "HGEM"
  #  dt.scenarioListIMPACT[, scenario := paste(SSPName, climModel, paste0("c", crop), sep = "-")]
 #   dt.scenarioListIMPACT[, crop := NULL]
  }

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
  desc <- paste0("Parameters from the gdx file", gdxFileName, "with IMPACT results. Creates scenarios list.")
  cleanup(inDT,outName, fileloc("iData"), desc = desc)
  }

#getGDXmetaData(gdxFileName)
gamsSetup(gdxFileName)

finalizeScriptMetadata(metadataDT, sourceFile)
# sourcer <- clearMemory(sourceFile) # removes everything in memory and sources the sourcer function
