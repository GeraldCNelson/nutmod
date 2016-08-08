#' Nutrient Modeling Functions
#'
#' @keywords utilities, nutrient data management functions
#' title: "Functions to facilitate management of nutrient data"
#' @name nutrientModFunctions.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}

# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.devtools <- list(
#     devtools.path = "~/R-dev",
#     devtools.install.args = "",
#     devtools.name = "Gerald C. Nelson",
#     devtools.desc.author = 'person("Gerald", "Nelson",
#     "nelson.gerald.c@gmail.com", role = c("aut", "cre"))',
#     devtools.desc.license = "GPL-3",
#     devtools.desc.suggests = NULL,
#     devtools.desc = list()
#     )
#   toset <- !(names(op.devtools) %in% names(op))
#   if (any(toset))
#     options(op.devtools[toset])
#   invisible()
# }

#'fileloc directory locations for files
#'
#' @param variableName Name of variable holding a path
#'
#' @param rawData - raw data directory
#'
#' @param mData - main data directory
#' @param iData - directory with IMPACT data
#' @param resultsPaperDir - directory with results for the paper
#' @param resultsDir - directory for results
#' @param FBSData - directory where FBS data are kept
#' @param SSPData - the path to the SSP data directory
#' @param IMPACTRawData - the path to the raw IMPACT data directory
#' @param IMPACTCleanData  - the path to the cleaned up IMPACT data directory
#' @return Value of the variableName to be assigned in another script
#'
#' @export
fileloc <- function(variableName) {
  rawData <- "data-raw"
  mData <- "data"
  iData <- "data/IMPACTData"
  nutData <- "data-raw/NutrientData"
  resultsDir <- "results"
  resultsPaperDir <- "results/nutPaper"
  shinyApp <- "nutrientModeling"
  shinyAppData <- "nutrientModeling/data"
  FBSData <- paste(rawData, "FBSData", sep = "/")
  SSPData <- paste(rawData, "SSPData", sep = "/")
  IMPACTRawData <- paste(rawData, "IMPACTData", sep = "/")
  IMPACTCleanData <- paste(mData, "IMPACTData", sep = "/")
  NutrientData <- paste(rawData, "NutrientData", sep = "/")
  if (variableName == "list") {
    return(c(
      "rawData",
      "mData",
      "iData",
      "resultsDir",
      "resultsPaperDir",
      "shinyApp",
      "shinyAppData",
      "FBSData",
      "IMPACTRawData",
      "IMPACTCleanData",
      "NutrientData",
      "SSPData"
    ))
  } else {
    return(eval(parse(text = variableName)))
  }
}

#' getNewestVersion
#'
#' @param fileShortName The substantive (first) part of the file name.
#' @return The most recent file.
#'
#' @export
getNewestVersion <- function(fileShortName, directory, fileType) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  if (missing(fileType)) {fileType <- "rds"}

  # see
  # http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  # regExp <- paste("(?=^", fileShortName, ")(?=.*rawData$)", sep = "")
  regExp <- paste("(?=^", fileShortName, ")(?=.*", fileType, "$)", sep = "")
  filesList <-
    grep(regExp,
         list.files(mData),
         value = TRUE,
         perl = TRUE)
  newestFile <- filesList[length(filesList)]
  if (length(newestFile) == 0) {
    stop(sprintf("There is no file that begins with '%s' in directory %s", fileShortName, mData))
  } else {
    outFile = readRDS(paste(mData,newestFile, sep = "/"))
    return(outFile)
  }
}

#' Title getNewestVersionIMPACT
#' @description read in a .rds file that includes the file fileShortName from the data/IMPACTData directory
#' @param fileShortName The substantive (first) part of the file name.
#' @return The most recent .rds file of IMPACT data
#' @export
getNewestVersionIMPACT <- function(fileShortName) {
  getNewestVersion(fileShortName, fileloc("iData"))
}

#' Title removeOldVersions - removes old version of an rawData file
#'
#' @param fileShortName - short name of the file to be removed
#' @param dir - directory of the file to be removed
#' @export
removeOldVersions <- function(fileShortName,dir) {
  removeFcn <- function(regExp) {
    oldVersionList <-
      grep(regExp,
           list.files(dir),
           value = TRUE,
           perl = TRUE)
    if (length(oldVersionList) > 0) {
      file.remove(paste(dir, oldVersionList, sep = "/"))
    }
  }
  # remove .rawData versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*rawData$)", sep = "")
  removeFcn(regExp)
  # remove .rds versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  removeFcn(regExp)
  # remove .xlsx versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*xlsx$)", sep = "")
  removeFcn(regExp)
  # remove .csv versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*csv$)", sep = "")
  removeFcn(regExp)
}

# #' Title removeOldVersions.xlsx - remove old xlsx versions in preparation for writing out new ones
# #' @param fileShortName - short name of the files to be removed
# #' @export
# removeOldVersions.xlsx <- function(fileShortName,dir) {
#   #mData <- fileloc("mData")
#   # returns a list of all the [fileShortName] files in the mData
#   # directory
#   regExp <- paste("(?=^", fileShortName, ")(?=.*xlsx$)", sep = "")
#   oldVersionList <-
#     grep(regExp,
#          list.files(dir),
#          value = TRUE,
#          perl = TRUE)
#   if (length(oldVersionList) > 0) {
#     file.remove(paste(dir, oldVersionList, sep = "/"))
#   }
# }

#' Title cleanup - remove old versions and save rds and xlsx versions of the file
#' @param inDT - name of the data table or frame to be written out
#' @param outName - short name of the file to be written out
#' @param dir - directory where the cleanup takes place
cleanup <- function(inDT, outName, dir, writeFiles) {

  #mData <- fileloc("mData")
  #convert inDT to a standard order
  sprintf("start cleanup for %s", outName)
  #print(proc.time())

  oldOrder <- names(inDT)
  startOrder <- c("scenario",keyVariable("region"),"year")
  if (all(startOrder %in% oldOrder)) {
    remainder <- oldOrder[!oldOrder %in% startOrder]
    data.table::setcolorder(inDT,c(startOrder,remainder))
    data.table::setorderv(inDT,c(startOrder,remainder))
  }
  # print(paste("removing old versions of ", outName, sep = ""))
  #print(proc.time())

  removeOldVersions(outName,dir)
  #  removeOldVersions.xlsx(outName,dir)
  # save(inDT,
  #      file = paste(dir, "/", outName, ".", Sys.Date(), ".rawData", sep = ""))
  sprintf("writing the rds for %s to %s ", outName, dir)
  # print(proc.time())
  # next line removes any key left in the inDT data table; this may be an issue if a df is used
  data.table::setkey(inDT, NULL)
  saveRDS(inDT,
          file = paste(dir, "/", outName, ".", Sys.Date(), ".rds", sep = ""))

  #print(proc.time())
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  if (nrow(inDT) > 50000) {
    sprintf("number of rows in the data, %s, greater than 50,000. Not writing xlsx or csv", nrow(inDT))
    writeFiles <- writeFiles[!writeFiles %in% c("xlsx", "csv")]
  }
  if ("csv"  %in% writeFiles) {
    #    print(paste("writing the csv for ", outName, " to ",dir, sep = ""))
    write.csv(inDT,file = paste(dir, "/", outName, ".", Sys.Date(), ".csv", sep = ""))
  }
  if ("xlsx"  %in% writeFiles) {
    #    print(paste("writing the xlsx for ", outName, " to ", dir, sep = ""))
    wbGeneral <- openxlsx::createWorkbook()
    outName <- strtrim(outName, c(31))
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = outName)

    openxlsx::writeDataTable(
      wbGeneral,
      inDT,  sheet = outName, startRow = 1, startCol = 1, rowNames = FALSE, colNames = TRUE, withFilter = TRUE)

    openxlsx::setColWidths(
      wbGeneral, sheet = outName, cols = 1:ncol(inDT), widths = "auto" )

    numStyle <- openxlsx::createStyle(numFmt = "0.000")
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = numStyle, rows = 1:nrow(inDT), cols = 2:ncol(inDT),
      gridExpand = TRUE )

    xcelOutFileName = paste(dir, "/", outName, ".", Sys.Date(), ".xlsx", sep = "")
    openxlsx::saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)
    #   print(paste("done writing the xlsx for ", outName, sep = ""))
    #  print(proc.time())
  }
}

cleanupScenarioNames <- function(dt.ptemp) {
  dt.ptemp[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.ptemp[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.ptemp[, scenario := gsub("HGEM2", "HGEM", scenario)]
  dt.ptemp[, scenario := gsub("IPSL2", "IPSL", scenario)]
  return(dt.ptemp)
}

cleanupNutrientNames <- function(nutList) {
  nutList <- gsub("_g_reqRatio","",nutList)
  nutList <- gsub("reqRatio","",nutList)
  nutList <- gsub("vit_","vit ",nutList)
  nutList <- gsub("_µg","",nutList)
  nutList <- gsub("_mg","",nutList)
  nutList <- gsub("_rae"," rae",nutList)
  nutList <- gsub("_g","",nutList)
  nutList <- gsub("totalfiber","total fiber",nutList)
  nutList <- gsub(".ratio.foodGroup","",nutList)
  nutList <- gsub("_","",nutList)
  nutList <- gsub("share","",nutList)
  return(nutList)
}

#' Title keyVariable - Return a key variable, or a list of all possibilities
#' @param keepYearList - list of scenario years to keep
#' @param keepYearList.FBS - list of FBS years to keep
#' @param FBSyearsToAverage - years to average over for base data set
#' @param IMPACTfish_code- variable name list for fish consumption items for IMPACT
#' @param IMPACTalcohol_code - variable name list for alcoholic beverages consumption for IMPACT
#' @param IMPACTfoodCommodList - variable name lists for IMPACT food commodities
#' @param scenarioListSSP.pop - list of scenarios in the SSP pop data
#' @param scenarioListSSP.GDP - list of scenarios in the SSP GDP|PPP data
#' @param DinY - number of days in a year
#' @param reqsList - nutrient requirements basic list
#' @param reqsListSSP - nutrient requirements by SSP age groups
#' @param ctyDeleteList
#' @param switch.useCookingRetnValues - apply the cooking retention values to the nutrient content
#' @param userName - Name of person running the scripts and generating results
#' @param region -  Aggregation scheme from individual countries to regions
#' @param switch.fixFish - if TRUE, fix salmon, tuna, and crustaceans
#' @param switch.changeElasticity - if TRUE, set max fish income elasticities to 1
#' @param commonList - names of the lists of nutrient names common to the nutrient lookup table and the requirements
#' @param dropListCty
#' @return list of key variables
#' @export
keyVariable <- function(variableName) {
  switch.fixFish <- "TRUE"
  switch.changeElasticity <- "TRUE"
  region <- "region_code.IMPACT159"
  keepYearList <- c("X2010", "X2015", "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050")
  keepYearList.FBS <- c("X2000", "X2001", "X2002", "X2003", "X2004", "X2005",
                        "X2006", "X2007", "X2008", "X2009", "X2010", "X2011")
  FBSyearsToAverage <- c("X2004", "X2005", "X2006")

  #' note shrimp, tuna, and salmon are removed in dataManagement.fish.R
  IMPACTfish_code <- c("c_Shrimp", "c_Crust", "c_Mllsc", "c_Salmon", "c_FrshD",
                       "c_Tuna", "c_OPelag", "c_ODmrsl", "c_OMarn", "c_FshOil", "c_aqan",
                       "c_aqpl")
  IMPACTalcohol_code <- c("c_wine", "c_beer", "c_spirits")
  IMPACTfoodCommodList <- sort(c("cbeef", "cpork", "clamb", "cpoul", "ceggs", "cmilk", "cbarl", "cmaiz",
                                 "cmill", "crice", "csorg", "cwhea", "cocer", "ccass", "cpota", "cswpt",
                                 "cyams", "corat", "cbean", "cchkp", "ccowp", "clent", "cpigp", "copul",
                                 "cbana", "cplnt", "csubf", "ctemf", "cvege", "csugr", "cgrnd", "cgdol",
                                 "crpsd", "crpol", "csoyb", "csbol", "csnfl", "csfol", "cplol", "cpkol",
                                 "ctols", "ctool", "ccoco", "ccafe", "cteas", "cothr", IMPACTfish_code,
                                 IMPACTalcohol_code))

  #These are the scenario numbers for the IIASA data with population disaggregated.
  scenarioListSSP.pop <- c("SSP1_v9_130115", "SSP2_v9_130115", "SSP3_v9_130115",
                           "SSP4_v9_130115", "SSP5_v9_130115")
  scenarioListSSP.GDP <- c("SSP1_v9_130325", "SSP2_v9_130325", "SSP3_v9_130325",
                           "SSP4_v9_130325", "SSP5_v9_130325")


  scenarioListIMPACT <- as.character(read.csv(file = paste(fileloc("mData"),"scenarioListIMPACT.csv", sep = "/"), stringsAsFactors = FALSE)[,1])
  DinY <- 365 #see http://stackoverflow.com/questions/9465817/count-days-per-year for a way to deal with leap years
  #' #' countries to remove because of poor data
  #' FSM - Micronesia, Federated States of
  #' GRD - Grenada
  #' PRK - Korea, Democratic People's Republic of
  reqsList <-
    c(
      "req.EAR",
      "req.RDA.vits",
      "req.RDA.minrls",
      "req.RDA.macro",
      "req.UL.vits",
      "req.UL.minrls",
      "req.AMDR.hi",
      "req.AMDR.lo"
    )
  reqsListPercap <- paste(reqsList,".percap", sep = "")
  reqsListSSP <- paste(reqsList,".ssp", sep = "")
  dropListCty <- c("GRL", "FSM", "GRD", "PRK")
  commonList <- paste("common.", reqsList, sep = "")
  c( "common.EAR", "common.RDA.vits", "common.RDA.minrls", "common.RDA.macro", "common.UL.vits","common.UL.minrls")
  switch.useCookingRetnValues <- "TRUE"
  userName <- "Gerald C. Nelson"
  if (variableName == "list") {
    return(
      c(
        "switch.fixFish",
        "switch.changeElasticity",
        "region",
        "keepYearList",
        "keepYearList.FBS",
        "FBSyearsToAverage",
        "IMPACTfish_code",
        "IMPACTalcohol_code",
        "IMPACTfoodCommodList",
        "scenarioListSSP.pop",
        "scenarioListSSP.GDP",
        "scenarioListIMPACT",
        "DinY",
        "reqListSSP",
        "switch.useCookingRetnValues",
        "commonList",
        "userName",
        "dropListCty"
      )
    )
  } else {
    return(eval(parse(text = variableName)))
  }
}

metadata <- function() {
  metadata <-
    data.frame(
      file_name_location = character(1),
      file_description = character(1),
      stringsAsFactors = FALSE
    )
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("DRIs"), "data on nutrient requirements")
  metadata[(nrow(metadata) + 1), ] <-
    c(
      "http://www.nal.usda.gov/fnic/DRI/DRI_Tables/recommended_intakes_individuals.pdf",
      "Source of EARS, RDAs, and ULs"
    )
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("CSEs"), "Consumer Surplus Equivalents for IMPACT commodities")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("IMPACT159regions"),
      "List of IMPACT regions; single countries and country aggregates")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("IMPACTstdRegions"),
      "List of the standard IMPACT large grouping of countries")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("IMPACTgdx"), "IMPACT demand data in gdx form")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("R_GAMS_SYSDIR"),
      "Location and name of GAMS program; needed for the gdx data import process"
    )
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("IMPACTfish"), "data on fish from the IMPACT fish model")
  # nutrient data ------
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("nutrientLU"), "nutrient lookup data for IMPACT commodities")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("dt.foodgroupLU"), "commodity to food group lookup table")
  # SSP information ----
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("SSPdataZip"), "zip file containing the SSP data")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("SSPcsv"), "csv file inside the SSP zip file")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("modelListPop"),
      "List of SSP models to extract population from")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("modelListGDP"),
      "List of SSP models to extract GDP data from")
  metadata[(nrow(metadata) + 1), ] <-
    c(fileNameList("SSP_DRI_ageGroupLU"),
      "lookup tables for SSP to DRI age and gender groups")
  # FBS information ----
  metadata[(nrow(metadata) + 1), ] <-
    c(filelocFBS("FBSdataZip"), "Zip file containing the FBS data")
  metadata[(nrow(metadata) + 1), ] <-
    c("FBS data creation date", filelocFBS("createDate"))
  metadata[(nrow(metadata) + 1), ] <-
    c("FBS lookup table", filelocFBS("FBSlookupTableLink"))
  metadata[(nrow(metadata) + 1), ] <-
    c(filelocFBS("FBSCommodityInfo"),
      "File in the FBS zip file containing the FBS data")
  metadata[(nrow(metadata) + 1), ] <-
    c(filelocFBS("FAOCountryNameCodeLookup"),
      "Lookup table for FAOSTAT and other country identification"
    )
  metadata[(nrow(metadata) + 1), ] <-
    c(filelocFBS("ISOCodes"),
      "List of all ISO 3 codes and the names of the countries they represent")

  inDT <- metadata
  outName <- "metaData"
  cleanup(inDT,outName,fileloc("resultsDir"))
}

#' Title fileNameList returns a list of filenames, with or without complete paths
#'
#' @param DRIFileName - the name of the spreadsheet with the DRI data
#' @param DRIs - the path to and the name of the DRI data file
#' @param CSEFileName - the name of the file with consumer support equivalents (CSEs)
#' @param CSEs - the path to and the file name for the CSE data
#' @param IMPACT159regionsFileName - the file name with the IMPACT159 regions names
#' @param IMPACT159regions - the path to and the file name for the IMPACT159 regions names
#' @param IMPACTregionsUpdateJun2016 - update to IMPACT regions in June 2016
#' @param IMPACTstdRegionsFileName - file name with IMPACT standard global regions
#' @param IMPACTstdRegions - path and file name for the list of IMPACT standard regions
#' @param IMPACTgdxfileName- file name with IMPACT demand results
#' @param IMPACTgdx - name and path to IMPACT demand results gdx file
#' @param gdxLib - path to gdx library
#' @param R_GAMS_SYSDIR - path to gdx library
#' @param IMPACTfishInfo - file name with info in IMPACT fish elasticities and quantities
#' @param IMPACTfish - path and file name for IMPACT fish elasticities and quantities
#' @param IMPACTalcoholInfo - file name with info in IMPACT alcohol elasticities
#' @param IMPACTalcohol - path and file name for IMPACT alcohol elasticities and quantities
#' @param IMPACTfood - path and file name for IMPACT food results
#' @param nutrientFileName - file name for nutrient lookup data
#' @param nutrientLU - path and file name for nutrient lookup data
#' @param commoditydt.foodgroupLUFileName - file name for the commodity to food group lookup spreadsheet
#' @param dt.foodgroupLU - path and file name for the commodity to food group lookup
#' @param SSPdataZipFile - file name of the SSP data in zip format
#' @param SSPdataZip - path and file name for the SSP data zip file
#' @param SSPcsv - name of the SSP data file in the zip file
#' @param modelListPop - list of models (currently only one) for the population data
#' @param modelListGDP - list of models (currently only one) for the GDP data
#' @param SSP_DRI_ageGroupLUFileName - lookup tables for SSP to DRI age and gender groups
#' @param SSP_DRI_ageGroupLU - lookup tables for SSP to DRI age and gender groups
#' @source \url{http://faostat3.fao.org/download/FB/FBS/E} Source of FBS data
#' @return Nothing
#' @export

fileNameList <- function(variableName) {
  IMPACTCleanData      <- fileloc("IMPACTCleanData")
  # NutrientData    <- fileloc("NutrientData")gdx
  nutrientDataDetails <- "data-raw/NutrientData/nutrientDetails"
  SSPData         <- fileloc("SSPData")
  DRIFileName     <- "DRI_IOM_V7.xlsx"
  mData <- fileloc("mData")
  DRIs            <- paste(nutrientDataDetails, DRIFileName, sep = "/")
  # CSE - consumer support equivalent
  #Note: the price a consumer pays is Pc * (1-CSE)
  CSEFileName     <- "CSEs20150824.xlsx"
  CSEs            <- paste(fileloc("IMPACTRawData"), CSEFileName, sep = "/")
  IMPACT159regionsFileName <-
    "IMPACTRegionsFeb2016.xlsx" # this file includes Denmark plus (DNP) and Sudan plus (SDP)
  #' IMPACT159regionsFileName <- "IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
  #' #IMPACT159regionsFileName <- "IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan
  IMPACTregionsUpdateJun2016FileName <- "IMPACT regions update June 6 2016.xlsx"
  IMPACTregionsUpdateJun2016 <- paste(fileloc("IMPACTRawData"), IMPACTregionsUpdateJun2016FileName, sep = "/")
  IMPACT159regions <- paste(fileloc("IMPACTRawData"), IMPACT159regionsFileName, sep = "/")
  IMPACTstdRegionsFileName <- "IMPACT-agg-regionsFeb2016.xlsx"
  IMPACTstdRegions <- paste(fileloc("IMPACTRawData"), IMPACTstdRegionsFileName, sep = "/")
  #IMPACTgdxfileName <- "Micronutrient-Inputs-USAID.gdx"  #-  gdx for the USAID results
  IMPACTgdxfileName <- "Micronutrient-Inputs-07252016.gdx"  #- gdx with SSP1, 2, and 3
  #IMPACTgdxfileName <- "Demand Results20150817.gdx"
  IMPACTgdx         <- paste(fileloc("IMPACTRawData"), IMPACTgdxfileName, sep = "/")
  gdxLib            <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
  R_GAMS_SYSDIR     <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
  IMPACTfishInfo    <- "Fish Elasticities and Quantities IMPACT.xlsx"
  IMPACTfish        <- paste(fileloc("IMPACTRawData"), IMPACTfishInfo, sep = "/")
  IMPACTalcoholInfo    <- "Alcohol Elasticities and Quantities IMPACT.xlsx"
  IMPACTalcohol        <- paste(fileloc("IMPACTRawData"), IMPACTalcoholInfo, sep = "/")
  IMPACTfoodFileName <- "dt.IMPACTfood"
  IMPACTfoodFileInfo <-  paste(mData,"/IMPACTData/",IMPACTfoodFileName,sep = "")
  # nutrient data ------
  nutrientFileName <- "USDA GFS IMPACT V22.xlsx"
  nutrientLU       <- paste(nutrientDataDetails, nutrientFileName, sep = "/")
  foodgroupLUFileName <-
    "food commodity to food group table V4.xlsx"
  foodGroupLU      <-
    paste(nutrientDataDetails, foodgroupLUFileName, sep = "/")
  # SSP information ----
  SSPdataZipFile   <- "SspDb_country_data_2013-06-12.csv.zip"
  SSPdataZip       <- paste(SSPData, SSPdataZipFile, sep = "/")
  #get the name of the file inside the zip. Assumes only 1
  temp             <- unzip(SSPdataZip, list = TRUE)
  SSPcsv           <- temp$Name[1]
  modelListPop     <- "IIASA-WiC POP"
  modelListGDP    <- "OECD Env-Growth"
  SSP_DRI_ageGroupLUFileName <-  "SSP_DRI_ageGroupLookUp.xlsx"
  SSP_DRI_ageGroupLU <-
    paste(nutrientDataDetails, SSP_DRI_ageGroupLUFileName, sep = "/")
  if (variableName == "list") {
    #list of variables that can be returned
    return(
      c("DRIFileName",
        "DRIs",
        "CSEFileName",
        "CSEs",
        "IMPACT159regionsFileName",
        "IMPACT159regions",
        "IMPACTregionsUpdateJun2016FileName",
        "IMPACTregionsUpdateJun2016",
        "IMPACTstdRegionsFileName",
        "IMPACTstdRegions",
        "IMPACTgdxfileName",
        "IMPACTgdx",
        "gdxLib",
        "R_GAMS_SYSDIR",
        "IMPACTfishInfo",
        "IMPACTfish",
        "IMPACTfood",
        "nutrientFileName",
        "nutrientLU",
        "commoditydt.foodgroupLUFileName",
        "dt.foodgroupLU",
        "SSPdataZipFile",
        "SSPdataZip",
        "SSPcsv",
        "modelListPop",
        "modelListGDP",
        "SSP_DRI_ageGroupLUFileName",
        "SSP_DRI_ageGroupLU"
      )
    )
  } else {
    return(eval(parse(text = variableName)))
  }
}

# Food Balance Sheet Information information ----
#'
#' Title filelocFBS Returns a list of files and paths for FBS-related data
#' @source  \url{http://www.fao.org/countryprofiles/iso3list/en/}
#' @param FBSdataZipFile - file name for the Food Balance Sheet data in zip file
#' @param FBSdataZip - path and file name for the Food Balance Sheet zip file
#' @param FBScsv - name of the FBS csv file contained the FBS zip file
#' @param FBSCommodityInfoFileName - worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
#' @param FBSCommodityInfo - path and file name to the worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
#' @param FAOCountryNameCodeLookupFile - file with lookup info for FAO codes and others including ISO3
#' @param FAOCountryNameCodeLookup - path and file name for the lookup spreadsheet
#' @param ISOCodesFile - file name with ISO country codes
#' @param ISOCodes - path and file name for the ISO country codes
#' @param FBSregionsToDrop - countries that do not have enough information or are large regions
#' @return The content of the variable name.
#' @export
filelocFBS <- function(variableName) {
  FBSData <- fileloc("FBSData")
  rawData <- fileloc("rawData")
  #' FBS to ISO lookup table
  FBSlookupTableLink <-
    "http://www.fao.org/countryprofiles/iso3list/en/"
  FBSdataZipFile <- "FoodBalanceSheets_E_All_Data.zip"
  FBSdataZip <- paste(FBSData, FBSdataZipFile, sep = "/")
  list <- unzip(FBSdataZip, list = TRUE)
  createDate <- as.character(list$Date[1])
  temp <-
    unzip(FBSdataZip, list = TRUE) #get the name of the file inside the zip. Assumes only 1
  FBScsv <- temp$Name[1]
  FBSCommodityInfoFileName <- "FBStoIMPACTlookupV3.xlsx"
  FBSCommodityInfo <-
    paste(FBSData, FBSCommodityInfoFileName, sep = "/")
  FAOCountryNameCodeLookupFile <- "FAOCountryNameCodeLookup.xlsx"
  FAOCountryNameCodeLookup <-
    paste(FBSData, FAOCountryNameCodeLookupFile, sep = "/")
  ISOCodesFile <- "ISOCountrycodes.xlsx"
  ISOCodes <- paste(rawData, ISOCodesFile, sep = "/")

  #These regions are reported as their individual member countries during the relevant
  # time period (e.g. after 1999 for Belgium-Luxembourg). Their data entries are all NA.
  # Although Ethiopia PDR doesn't have data, Ethiopia does.
  FBSregionsToDrop <- c("Belgium-Luxembourg", "Czechoslovakia", "Ethiopia PDR",
                        "Montenegro", "Serbia", "Serbia and Montenegro", "Yugoslav SFR", "Europe",
                        "Eastern Europe", "Southern Europe", "Western Europe", "European Union",
                        "USSR", "World", "Netherlands Antilles (former)", "Caribbean")

  if (variableName == "list") {
    return(
      #list of variables that can be returned
      c(
        "FBSdataZipFile",
        "FBSdataZip",
        "FBScsv",
        "FBSCommodityInfoFileName",
        "FBSCommodityInfo",
        "FAOCountryNameCodeLookupFile",
        "FAOCountryNameCodeLookup",
        "ISOCodesFile",
        "ISOCodes",
        "FBSregionsToDrop"
      )
    )
  } else {
    return(eval(parse(text = variableName)))
  }
}

#' Title plusCnst
#' @param region_code.IMPACT - 3 letter code for the new region
#' @param lst - list of countries that go in the region
#' @param region_title - name of the region (eg., Denmark plus)
#' @return data from with the information for an IMPACT region with multiple countries
# plusCnst <- function(region_code.IMPACT, ISO3_list, region_title) {
#   data.frame(region_code.IMPACT159, ISO3_lst, region_name.IMPACT159, stringsAsFactors = FALSE)}

plusCnst <- function(region_code.IMPACT159, ISO3_lst, region_name) {
  data.frame(region_code.IMPACT159, ISO3_lst, region_name, stringsAsFactors = FALSE)
}
#' Title createIMPACT159Regions
#' @return regions.IMPACT159
#' @export

#' Title flagMissingFiles Prints a list of missing files and a hint of how to address
#'
#' @return Nothing
#' @export
flagMissingFiles <- function() {
  shortNameList = data.frame(
    name = c("FBS", "dt.regions.all", "dt.SSPPopClean"),
    script = c("dataPrep.FBS.R", "dataPrep.regions.R", "dataPrep.SSP.R")
  )
  mData <- fileloc("mData")
  for (i in length(shortNameList)) {
    filesList <-
      grep(shortNameList$name[i], list.files(mData), value = TRUE)
    if (length(filesList) == 0) {
      rowNumber <- which(grepl(shortNameList$name[i], shortNameList$name))
      print(paste("Missing data file ", shortNameList$name[i]))
      print(paste(" run R/", shortNameList$script[rowNumber], sep = ""))
      return()
    }
  }
}

# these functions return the maximum or minimum in every row
colMax <- function(dataIn) {
  lapply(dataIn, max, na.rm = TRUE)
}
colMin <- function(dataIn) {
  lapply(dataIn, min, na.rm = TRUE)
}

countryNameLookup <- function(countryCode, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  dt.regions <- getNewestVersion('dt.regions.all', fileloc("mData"))
  if (!countryCode %in% dt.regions$region_code.IMPACT159) {
    stop(sprintf("The country code you entered (%s) is not in the lookup table", countryCode))
  } else {
    countryName <- dt.regions[region_code.IMPACT159 == countryCode,region_name.IMPACT159]
    return(countryName)
  }
}
countryCodeLookup <- function(countryName, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  dt.regions <- getNewestVersion('dt.regions.all', fileloc("mData"))
  if (!countryName %in% dt.regions$region_name.IMPACT159) {
    stop(sprintf("The country name you entered (%s) is not in the lookup table", countryName))
  } else {
    # region159codes <- dt.regions$region_code.IMPACT159
    # rowNum <- dt.regions[region_name.IMPACT159 == countryName, which = TRUE]
    # countryCode <- region159codes[rowNum]
    countryCode <- dt.regions[region_name.IMPACT159 == countryName, region_code.IMPACT159]
    return(countryCode)
  }
}

# test data for reqRatiodatasetup
countryCode <- "AFG"; years <- c("X2010", "X2030", "X2050")
reqTypeName <- "RDA.macro"
dir <- fileloc("resultsDir"); scenarioName <- "SSP3-NoCC"

nutReqDataPrep <- function(reqTypeName, countryCode, scenarioName, years, dir) {
  resultFileLookup <- data.table::as.data.table(read.csv("data/resultFileLookup.csv"))
  reqRatioList <- resultFileLookup[1:6, reqTypeName] # list of req types that include are based on a requirement
  SSPname <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[1]
  climModel <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[2]
  experiment <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[3]
  if (is.na(experiment)) {experiment <- "REF"}

  fileName <- resultFileLookup[reqType == reqTypeName, fileName]
  reqRatios <- getNewestVersion(fileName, dir)
  keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
                   "nutrient", years)
  idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient")

  reqRatios <- reqRatios[, keepListCol, with = FALSE]
  description <- resultFileLookup[reqType == reqTypeName, description]

  measureVars <- years
  reqRatios.long <- data.table::melt(
    data = reqRatios, id.vars = idVars, measure.vars = measureVars, variable.name = "year",
    value.name = "value", variable.factor = FALSE)

  formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + year ~ nutrient")
  reqRatios.wide <- data.table::dcast(
    reqRatios.long,
    formula = formula.ratios,
    value.var = "value")

  i <- countryCode; j <- SSPname; k <- climModel; m <- experiment; l <- years

  reqRatios.nuts <-
    reqRatios.wide[region_code.IMPACT159 %in% i & SSP %in% j & climate_model %in% k &
                     experiment == m]
  #get rid of year along with the others
  deleteListCol <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "year")
  reqRatios.nuts[,(deleteListCol) := NULL]
  spokeCols <- names(reqRatios.nuts)
  #  nutListShort <- cleanupNutrientNames(spokeCols)

  reqRatios.nuts[is.nan(get(spokeCols)),  (spokeCols) := 0, with = FALSE]
  reqRatios.nuts <- reqRatios.nuts[,(spokeCols) := round(.SD,2), .SDcols = spokeCols]

  # radarchart- If maxmin is TRUE, this must include maximum values as row 1 and minimum values as row 2 for each variables
  colMins <- as.list(c(rep.int(0,ncol(reqRatios.nuts))))
  reqRatios.nuts[, nmax := max(.SD), .SDcols = spokeCols]
  cMax <- round(reqRatios.nuts[1,nmax])
  colMaxs <- as.list(c(rep.int(cMax,ncol(reqRatios.nuts) - 1))) # -1 because nmax is a temporary column
  reqRatios.nuts[,nmax := NULL]

  # include the requirement ratio of 1
  reqRatioRow <- as.list(c(rep(1,ncol(reqRatios.nuts))))
  reqRatios.nuts <- rbind(colMaxs, colMins, reqRatioRow, reqRatios.nuts)

  reqRatios.nuts[, (spokeCols) := lapply(.SD, as.numeric), .SDcols = spokeCols]
  reqRatios.nuts[is.nan(get(spokeCols)),  (spokeCols) := 0, with = FALSE]
  data.table::setnames(reqRatios.nuts, old = names(reqRatios.nuts), new = cleanupNutrientNames(names(reqRatios.nuts)))
  return(reqRatios.nuts)
}

nutReqSpiderGraph <- function(reqTypeName, countryCode, scenarioName, years, dir) {
  reqRatios.nuts <- nutReqDataPrep(reqTypeName,countryCode, scenarioName, years, dir)
  resultFileLookup <- data.table::as.data.table(read.csv("data/resultFileLookup.csv"))
  # reqRatioList <- resultFileLookup[1:6, reqType] # list of req types that include are based on a requirement
  # SSP <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[1]
  # climModel <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[2]
  # experiment <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[3]
  # if (is.na(experiment)) {experiment <- "REF"}
  #
  # fileName <- resultFileLookup[reqType == reqTypeName, fileName]
  # reqRatios <- getNewestVersion(fileName, dir)
  # keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
  #                  "nutrient", years)
  # idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient")
  # formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + year ~ nutrient")
  #
  # reqRatios <- reqRatios[, keepListCol, with = FALSE]
  description <- resultFileLookup[reqType == reqTypeName, description]

  chartTitle <- description

  #temp1 <- rbind(colMins, colMaxs, reqRatioRow, reqRatios.wide.nuts)

  legendText <- c("REQ",years)
  colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
  colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )

  lineType <- c(3, 1, 1, 1)
  plot.new()
  par(mar = c(1, 2, 2, 1))
  reqNames <- names(reqRatios.nuts)
    radarchart(reqRatios.nuts, axistype = 2,
             title = chartTitle,
             vlabels = reqNames,
             seg = 3,
             #custom polygon
             pcol = colors_border, plwd = 1, plty = lineType,
             #customgrid colors
             cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
             #custom labels
             vlcex = 0.8,
             maxmin = TRUE
  )

  legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
         col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
         y.intersp = .8)
}

# test data for nutSharedatasetup
country <- "AFG"; years <- c("X2010", "X2030", "X2050")
reqTypeName <- "FG.minrls";
dir <- fileloc("resultsDir"); scenarioName <- "SSP2-NoCC"
nutshareSpiderGraph <- function(reqTypeName, country, scenario, years, dir) {
  #reqRatiodatasetup <- function(reqTypeName,country, scenarioName, years, dir) {
  resultFileLookup <- data.table::as.data.table(read.csv("data/resultFileLookup.csv"))
  dt.foodgroupLU <- getNewestVersion("dt.foodGroupsInfo")
  SSP <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[1]
  climModel <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[2]
  experiment <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[3]
  if (is.na(experiment)) {experiment <- "REF"}

  fileName <- resultFileLookup[reqType == reqTypeName, fileName]
  shareRatios <- getNewestVersion(fileName, dir)
  keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
                   "nutrient", years)
  idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient")

  if ("food_group_code" %in% names(shareRatios)) {
    keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
                     "food_group_code", "nutrient", years)
    idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient", "food_group_code")
    formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + nutrient + year ~ food_group_code")
  }
  if ("staple_code" %in% names(shareRatios)) {
    keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
                     "staple_code", "nutrient", years)
    idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient", "staple_code")
    formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + nutrient + year ~ staple_code")
  }
  shareRatios <- shareRatios[, keepListCol, with = FALSE]
  description <- resultFileLookup[reqType == reqTypeName, description]

  #  measureVars <- keyVariable("keepYearList")
  measureVars <- years
  shareRatios.long <- data.table::melt(
    data = shareRatios, id.vars = idVars, measure.vars = measureVars, variable.name = "year",
    value.name = "value", variable.factor = FALSE)

  shareRatios.wide <- data.table::dcast(
    shareRatios.long,
    formula = formula.ratios,
    value.var = "value")

  i <- country; j <- SSP; k <- climModel; m <- experiment; l <- years

  shareRatios.nuts <-
    shareRatios.wide[region_code.IMPACT159 %in% i & SSP %in% j & climate_model %in% k &
                       experiment == m]
  # shareRatios.nuts[, year := gsub("X","",year)]
  #get rid of year along with the others
  deleteListCol <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "year")
  shareRatios.nuts[,(deleteListCol) := NULL]
  spokeCols <- names(shareRatios.nuts)[2:ncol(shareRatios.nuts)]
  foodGroupShort <- cleanupNutrientNames(spokeCols)
  # if (("food_group_code" %in% names(shareRatios)) | ("staple_code" %in% names(shareRatios))) {
  #  # dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo")
  #   #foodGroupList <- sort(unique(foodGroupsInfo$food_group_code))
  #   stapleList <- unique(dt.foodgroupLU$staple_code)
    shareRatios.nuts[, nutrient := gsub("_g.ratio.foodGroup","", nutrient)]
    shareRatios.nuts[, nutrient := gsub("_µg.ratio.foodGroup","", nutrient)]
    shareRatios.nuts[, nutrient := gsub("_mg.ratio.foodGroup","", nutrient)]
    shareRatios.nuts[, nutrient := gsub("_"," ", nutrient)]
    shareRatios.nuts[, nutrient := gsub("vit ","vitamin ", nutrient)]
 # }
  shareRatios.nuts[is.nan(get(spokeCols)),  (spokeCols) := 0, with = FALSE]
  shareRatios.nuts <- shareRatios.nuts[,(spokeCols) := round(.SD,2), .SDcols = spokeCols]

  #temp1 <- rbind(colMins, colMaxs, reqRatioRow, shareRatios.wide.nuts)

  legendText <- years
  colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
  colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )

  lineType <- c(3, 1, 1, 1)
  plot.new()
  title(description)

  nutrientList <- sort(unique(shareRatios.nuts$nutrient))
  par(mar = c(1, 2, 2, 1))
  nrowsGraphs = round(length(nutrientList)/2)
  par(mfrow = c(nrowsGraphs,2))

  for (i in nutrientList) {
    temp <- copy(shareRatios.nuts[nutrient %in% i, ])# radarchart- If maxmin is TRUE, this must include maximum values as row 1 and minimum values as row 2 for each variables
    temp[, nmax := max(.SD), .SDcols = spokeCols]
    cMax <- round(temp[1,nmax], digits = 2)
    colMaxs <- as.list(c(rep.int(cMax,ncol(temp) - 1))) # -1 because nmax is a temporary column
    temp[,nmax := NULL]
    colMins <- as.list(c(rep.int(0,ncol(temp))))

    temp <- rbind(colMaxs, colMins, temp)
    temp[, (spokeCols) := lapply(.SD, as.numeric), .SDcols = spokeCols]
    temp[is.nan(get(spokeCols)),  (spokeCols) := 0, with = FALSE]
    temp[, nutrient := NULL]

    vnames <- vector(mode = "character", length = length(foodGroupShort))
    for (j in 1:length(foodGroupShort)) {
      vnames[j] <- as.character(dt.foodgroupLU[food_group_codes == foodGroupShort[j], food_groups])
    }

    radarchart(temp, axistype = 2,
               title = i,
               vlabels = vnames,
               seg = 3,
               #custom polygon
               pcol = colors_border, plwd = 1, plty = lineType,
               #customgrid colors
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
               #custom labels
               vlcex = 0.8,
               maxmin = TRUE
    )

    legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
           col = colors_in, text.col = "black", cex = .6, pt.cex = .8, pt.lwd = 1,
           y.intersp = .8)
  }
  return(temp)
}
