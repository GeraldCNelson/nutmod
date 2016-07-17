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
#' @param RData - raw data directory
#'
#' @param mData - main data directory
#' @param iData - directory with IMPACT data
#' @param resData - directory with results
#' @param resultsDir - directory for results
#' @param FBSData - directory where FBS data are kept
#' @param SSPData - the path to the SSP data directory
#' @param IMPACTData - the path to the raw IMPACT data directory
#' @param IMPACTDataClean  - the path to the cleaned up IMPACT data directory
#' @return Value of the variableName to be assigned in another script
#'
#' @export
fileloc <- function(variableName) {
  RData <- "data-raw"
  mData <- "data"
  iData <- "data/IMPACTData"
  resData <- "results"
  resultsDir <- "results"
  shinyApp <- "nutrientModeling"
  shinyAppData <- "nutrientModeling/data"
  FBSData <- paste(RData, "FBSData", sep = "/")
  SSPData <- paste(RData, "SSPData", sep = "/")
  IMPACTData <- paste(RData, "IMPACTData", sep = "/")
  IMPACTDataClean <- paste(mData, "IMPACTData", sep = "/")
  NutrientData <- paste(RData, "NutrientData", sep = "/")
  if (variableName == "list") {
    return(c(
      "RData",
      "mData",
      "iData",
      "resData",
      "resultsDir",
      "shinyApp",
      "shinyAppData",
      "FBSData",
      "IMPACTData",
      "IMPACTDataClean",
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
getNewestVersion <- function(fileShortName, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  # see
  # http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  # regExp <- paste("(?=^", fileShortName, ")(?=.*RData$)", sep = "")
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
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
  iData <- fileloc("iData")
  # see
  # http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  # regExp <- paste("(?=^", fileShortName, ")(?=.*RData$)", sep = "")
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  filesList <-
    grep(regExp,
         list.files(iData),
         value = TRUE,
         perl = TRUE)
  newestFile <- filesList[length(filesList)]
  print(newestFile)
  #  return(load(file = paste(iData, newestFile, sep = "/")))
  return(readRDS(paste(iData, newestFile, sep = "/")))
}

#' Title removeOldVersions - removes old version of an RData file
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
  # remove .Rdata versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*RData$)", sep = "")
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
  #      file = paste(dir, "/", outName, ".", Sys.Date(), ".RData", sep = ""))
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
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = outName)

    openxlsx::writeDataTable(
      wbGeneral,
      inDT,
      sheet = outName,
      startRow = 1,
      startCol = 1,
      rowNames = FALSE,
      colNames = TRUE,
      withFilter = TRUE
    )

    openxlsx::setColWidths(
      wbGeneral,
      sheet = outName,
      cols = 1:ncol(inDT),
      widths = "auto"
    )

    numStyle <- openxlsx::createStyle(numFmt = "0.000")
    openxlsx::addStyle(
      wbGeneral,
      sheet = outName,
      style = numStyle,
      rows = 1:nrow(inDT),
      cols = 2:ncol(inDT),
      gridExpand = TRUE
    )

    xcelOutFileName = paste(dir, "/", outName, ".", Sys.Date(), ".xlsx", sep = "")
    openxlsx::saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)
 #   print(paste("done writing the xlsx for ", outName, sep = ""))
  #  print(proc.time())
  }
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
#' @param useCookingRetnValues - apply the cooking retention values to the nutrient content
#' @param userName - Name of person running the scripts and generating results
#' @param region -  Aggregation scheme from individual countries to regions
#' @param fixFish - if TRUE, fix salmon, tuna, and crustaceans
#' @param changeElasticity - if TRUE, set max fish income elasticities to 1
#' @param commonList - names of the lists of nutrient names common to the nutrient lookup table and the requirements
#' @param dropListCty
#' @return list of key variables
#' @export
keyVariable <- function(variableName) {
  fixFish <- "TRUE"
  changeElasticity <- "TRUE"
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

  # R_GAMS_SYSDIR <- fileNameList("R_GAMS_SYSDIR")
  # gdxrrw::igdx(gamsSysDir = R_GAMS_SYSDIR, silent = TRUE)
  # dt.ptemp <- data.table::as.data.table(gdxrrw::rgdx.param(fileNameList("IMPACTgdx"), "PWX0",ts = TRUE,
  #                                                          names = c("scenario", "IMPACT_code", "year", "value")))

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
  dropListCty <- c("GRL", "FSM", "GRD", "PRK", "JEY")
  commonList <- paste("common.", reqsList, sep = "")
  c( "common.EAR", "common.RDA.vits", "common.RDA.minrls", "common.RDA.macro", "common.UL.vits","common.UL.minrls")
  useCookingRetnValues <- "TRUE"
  userName <- "Gerald C. Nelson"
  if (variableName == "list") {
    return(
      c(
        "fixFish",
        "changeElasticity",
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
        "useCookingRetnValues",
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
    c(fileNameList("foodGroupLU"), "commodity to food group lookup table")
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
  cleanup(inDT,outName,fileloc("resData"))
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
#' @param commodityFoodGroupLookupFileName - file name for the commodity to food group lookup spreadsheet
#' @param foodGroupLU - path and file name for the commodity to food group lookup
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
  IMPACTData      <- fileloc("IMPACTData")
  NutrientData    <- fileloc("NutrientData")
  nutrientDataDetails <- "data-raw/NutrientData/nutrientDetails"
  SSPData         <- fileloc("SSPData")
  DRIFileName     <- "DRI_IOM_V7.xlsx"
  mData <- fileloc("mData")
  DRIs            <- paste(nutrientDataDetails, DRIFileName, sep = "/")
  # CSE - consumer support equivalent
  #Note: the price a consumer pays is Pc * (1-CSE)
  CSEFileName     <- "CSEs20150824.xlsx"
  CSEs            <- paste(IMPACTData, CSEFileName, sep = "/")
  IMPACT159regionsFileName <-
    "IMPACTRegionsFeb2016.xlsx" # this file includes Denmark plus (DNP) and Sudan plus (SDP)
  #' IMPACT159regionsFileName <- "IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
  #' #IMPACT159regionsFileName <- "IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan
  IMPACTregionsUpdateJun2016FileName <- "IMPACT regions update June 6 2016.xlsx"
  IMPACTregionsUpdateJun2016 <-
    paste(IMPACTData, IMPACTregionsUpdateJun2016FileName, sep = "/")
  IMPACT159regions <-
    paste(IMPACTData, IMPACT159regionsFileName, sep = "/")
  IMPACTstdRegionsFileName <- "IMPACT-agg-regionsFeb2016.xlsx"
  IMPACTstdRegions <-
    paste(IMPACTData, IMPACTstdRegionsFileName, sep = "/")
  IMPACTgdxfileName <- "Micronutrient-Inputs20160404.gdx"  #- new larger gdx
  #IMPACTgdxfileName <- "Demand Results20150817.gdx"
  IMPACTgdx         <- paste(IMPACTData, IMPACTgdxfileName, sep = "/")
  gdxLib            <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
  R_GAMS_SYSDIR     <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
  IMPACTfishInfo    <- "Fish Elasticities and Quantities IMPACT.xlsx"
  IMPACTfish        <- paste(IMPACTData, IMPACTfishInfo, sep = "/")
  IMPACTalcoholInfo    <- "Alcohol Elasticities and Quantities IMPACT.xlsx"
  IMPACTalcohol        <- paste(IMPACTData, IMPACTalcoholInfo, sep = "/")
  IMPACTfoodFileName <- "dt.IMPACTfood"
  IMPACTfoodFileInfo <-  paste(mData,"/IMPACTData/",IMPACTfoodFileName,sep = "")
  # nutrient data ------
  nutrientFileName <- "USDA GFS IMPACT V20.xlsx"
  nutrientLU       <- paste(nutrientDataDetails, nutrientFileName, sep = "/")
  commodityFoodGroupLookupFileName <-
    "food commodity to food group table V3.xlsx"
  foodGroupLU      <-
    paste(nutrientDataDetails, commodityFoodGroupLookupFileName, sep = "/")
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
        "commodityFoodGroupLookupFileName",
        "foodGroupLU",
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
  RData <- fileloc("RData")
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
  ISOCodes <- paste(RData, ISOCodesFile, sep = "/")

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
    countryCode <- dt.regions[region_name.IMPACT159 == countryName, region_code.IMPACT159]
    return(countryCode)
  }
}

reqRatiodatasetup <- function(reqType,country, SSP, climModel, experiment, years, dir) {

  scenarioName <- paste(SSP, climModel, experiment, sep = "-")
  scenarioListIMPACT <- keyVariable("scenarioListIMPACT")
  errorMessage <- cbind(paste("Your combination of ", scenarioName,
                              "is not allowed. Please choose from one of the following combinations: ", sep = " "),
                        paste(shQuote(as.character(scenarioListIMPACT), type = "sh"), collapse = ", "))
  if (experiment %in% "REF" & !scenarioName %in% c("SSP2-HGEM-REF", "SSP2-IPSL2-REF", "SSP2-NoCC-REF")) {
    stop(errorMessage)
  }
  if (!experiment %in% "REF" & !scenarioName %in% scenarioListIMPACT) {
    stop(errorMessage)
  }
  # need to do this to match up with what the experiment names are in the data files
   scenarioName <- paste(SSP, climModel, experiment, sep = "-")

  if (reqType == "RDA_macro") {
    reqRatios <- getNewestVersion("RDA.macro.sum.req.ratio", dir)
    reqType <- "RDA, macronutrients"
  }
  if (reqType == "RDA_vits") {
    reqRatios <- getNewestVersion("RDA.vits.sum.req.ratio", dir)
    reqType <- "RDA, vitamins"
  }
  if (reqType == "RDA_minrls") {
    reqRatios <- getNewestVersion("RDA.minrls.sum.req.ratio", dir)
    reqType <- "RDA, minerals"
  }
  if (reqType == "EAR") {
    reqRatios <- getNewestVersion("EAR.sum.req.ratio", dir)
    reqType <- "EAR"
  }
  if (reqType == "UL_vits") {
    reqRatios <- getNewestVersion("UL.vits.sum.req.ratio", dir)
    reqType <- "UL, vitamins"
  }
  if (reqType == "UL_minrls") {
    reqRatios <- getNewestVersion("UL.minrls.sum.req.ratio", dir)
    reqType <- "UL, minerals"
  }

  if (reqType == "kcal_ratios") {
    reqRatios <- getNewestVersion("dt.energy.ratios", dir)
    reqRatios <- reqRatios[!nutrientReq %in% "sugar_g_reqRatio",]
    reqType <- "Share of Kcals"
  }
# region <- keyVariable("region")
  idVars <- c("scenario", "SSP","climate_model", "experiment", region, "nutrientReq")
  measureVars <- keyVariable("keepYearList")
  reqRatios.long <- data.table::melt(
    data = reqRatios,  id.vars = idVars, measure.vars = measureVars, variable.name = "year",
    value.name = "value", variable.factor = FALSE)

  formula.ratios <- paste("scenario + SSP + climate_model + experiment + ", region, "  +  year ~ nutrientReq")
  reqRatios.wide <- data.table::dcast(
    reqRatios.long,
    formula = formula.ratios,
    value.var = "value")

  nutList <- unique(reqRatios$nutrientReq)
  nutListShort <- gsub("_reqRatio","",nutList)
  nutListShort <- gsub("vit_","vit ",nutListShort)
  nutListShort <- gsub("_µg","",nutListShort)
  nutListShort <- gsub("_mg","",nutListShort)
  nutListShort <- gsub("_rae"," rae",nutListShort)
  nutListShort <- gsub("_g"," ",nutListShort)
  nutListShort <- gsub("totalfiber","total fiber",nutListShort)

  i <- country
  j <- SSP
  k <- climModel
  m <- experiment
  l <- years

  #l <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
  reqRatios.wide.nuts <- reqRatios.wide[region_code.IMPACT159 %in% i &
                                          SSP %in% j &
                                          climate_model %in% k &
                                          experiment == m &
                                          year %in% l,
                                        c( "year",nutList), with = FALSE]
  data.table::setnames(reqRatios.wide.nuts,old = names(reqRatios.wide.nuts),
                       new = gsub("_reqRatio","",names(reqRatios.wide.nuts)))

  # include the requirement ratio as a row in calculating the col mins and maxs
  reqRatioRow <- as.list(c("Req",rep(1,ncol(reqRatios.wide.nuts) - 1)))
  temp <- rbind(reqRatios.wide.nuts, reqRatioRow)
  changeCols <- colnames(temp)[2:ncol(temp)]
  temp[is.nan(get(changeCols)),  (changeCols) := 0, with = FALSE]
  colMins <- as.list(c("Min", rep.int(0,ncol(reqRatios.wide.nuts) - 1)))
  temp[, (changeCols) := lapply(.SD, as.numeric), .SDcols = changeCols]
  # next few lines to get maximum ratio value for all the nutrients
  temp[, nmax := max(.SD), .SDcols = colnames(temp)[2:ncol(temp)]]
  cMax <- round(temp[1,nmax])
  temp[,nmax := NULL]
  colMaxs <- as.list(c("Max",rep.int(cMax,ncol(reqRatios.wide.nuts) - 1)))

  temp <- rbind(colMaxs, colMins, reqRatioRow, reqRatios.wide.nuts)
  temp[, (changeCols) := lapply(.SD, as.numeric), .SDcols = changeCols]
  temp <- temp[,(changeCols) := round(.SD,2), .SDcols = changeCols]
  return(list(temp, nutListShort))
}

cleanUpreqType <- function(reqType) {
  if (reqType == "RDA_macro")  {fullName <- "RDA, macronutrients"}
  if (reqType == "RDA_vits")   {fullName <- "RDA, vitamins"}
  if (reqType == "RDA_minrls") {fullName <- "RDA, minerals"}
  if (reqType == "EAR")        {fullName <- "Estimated average requirements"}
  if (reqType == "UL_minrls") {fullName <- "Upper limit, minerals"}
  if (reqType == "UL_vits") {fullName <- "Upper limit, vitamins"}
  if (reqType == "kcal_ratios") {fullName <- "nutrient share in energy intake"}
  return(fullName)
}

nutSpiderGraph <- function(reqType, country, SSP, climModel, experiment, years, dir) {
  temp <- reqRatiodatasetup(reqType,country, SSP, climModel, experiment, years, dir)
  nutListShort <- temp[[2]]
  inputData <- temp[[1]]
  head(inputData)
  chartTitle <- cleanUpreqType(reqType)

  #temp1 <- rbind(colMins, colMaxs, reqRatioRow, reqRatios.wide.nuts)
  colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
  colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )
  lineType <- c(3, 1, 1, 1)
  radarchart(inputData[,!1, with = FALSE], axistype = 2,
             title = chartTitle,
             vlabels = nutListShort,
             seg = 3,
             #custom polygon
             pcol = colors_border, plwd = 1, plty = lineType,
             #custom the grid
             cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
             #custom labels
             vlcex = 0.8
  )
  legend(x = 1.2, y = -.55, legend = gsub("X","",inputData[3:nrow(inputData),years]), bty = "n", pch = 20,
         col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
         y.intersp = .8)
}

cleanupScenarioNames <- function(dt.ptemp) {
  dt.ptemp[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.ptemp[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.ptemp[, scenario := gsub("HGEM2", "HGEM", scenario)]
  return(dt.ptemp)
}

