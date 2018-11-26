#' Nutrient Modeling Functions
#'
#' @keywords utilities, nutrient data management functions
#' @title "Functions to facilitate management of nutrient data"
#' @name nutrientModFunctions.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#'
# Copyright (C) 2015 - 2018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.  This program is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details at
# http://www.gnu.org/licenses/.  Contributors to the work include
# Brendan Power (for coding assistance), Joanne E. Arsenault (for
# constructing the nutrient requirements worksheet) and Joanne E.
# Arsenault, Malcolm Reilly, Jessica Bogard, and Keith Lividini (for
# nutrition expertise)

source("R/workBookFunctions.R")
source("R/nutrientCalcFunctions.R")
library(plyr)
library(dtplyr)
library(tidyverse) # includes ggplot2, tibble, tidyr, and readr
library(data.table) # this is needed everywhere and currently some scripts don't call it
library(ggthemes)
library(qdapRegex) # needed for the TC (title case) function, added Nov 3, 2018

fontFamily <- "Cambria"
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

# sourcer is currently only used in automate.R but could potentially be used elsewhere
sourcer <- function(sourceFile){
  sourceFile <- paste0("R/", sourceFile)
  cat("\nRunning ", sourceFile, "\n")
  source(sourceFile)
}

#'fileloc directory locations for files
#'
#' @param variableName Name of variable holding a path
#'
#' @param rawData - raw data directory
#'
#' @param mData - main data directory
#' @param iData - directory with IMPACT data
#' @param uData - directory with data that are universal to all scenarios
#' @param gDir  - the path to the graphics directory
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
  gdxChoice <- getGdxChoice()
  rawData <- "data-raw"
  mData <- paste("data/", gdxChoice, sep = "")
  gDir <- paste("graphics", gdxChoice, sep = "/")
  iData <- paste(mData, "IMPACTData/", sep = "/")
  uData <- "data/universalData"
  nutData <- "data-raw/NutrientData"
  resultsTop <- "results"
  resultsDir <- paste("results/", gdxChoice, sep = "")
  resultsPaperDir <- "results/nutPaper"
  shinyApp <- "nutrientModeling"
  shinyAppData <- "nutrientModeling/data"
  FBSData <- paste(rawData, "FBSData", sep = "/")
  SSPData <- paste(rawData, "SSPData", sep = "/")
  IMPACTRawData <- paste(rawData, "IMPACTData", sep = "/")
  IMPACTCleanData <- paste(mData, "IMPACTData/", gdxChoice, sep = "/")
  NutrientData <- paste(rawData, "NutrientData", sep = "/")
  nutrientDataDetails <- paste(rawData, "NutrientData", "nutrientDetails", sep = "/")
  if (variableName == "list") {
    return(c(
      "rawData",
      "mData",
      "iData",
      "gDir",
      "resultsDir",
      "resultsTop",
      "resultsPaperDir",
      "shinyApp",
      "shinyAppData",
      "FBSData",
      "IMPACTRawData",
      "IMPACTCleanData",
      "NutrientData",
      "nutrientDataDetails",
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
  #  regExp <- paste("(?=^", fileShortName, ")(?=.*", fileType, "$)", sep = "")
  # stringChar <- unlist(strsplit(list.files(mData)[1], ".", fixed = TRUE))
  # # this is still a potential problem. If the first file in the directory doesn't have the same date
  # dateOfFirst <- stringChar[length(stringChar) - 1]
  # tailLength <- 15 # to remove data and the period and csv or rds
  # if (fileType == "xlsx") tailLength <- 16 # for xlsx files
  # fillIn <- paste('.{', tailLength, '}$', sep = "")
  fileShortNameTest <- paste(fileShortName,"_2", sep = "") # this should get rid of the multiple files problem
  # cat("\nfileShortNameTest ", fileShortNameTest)
  filesofFileType <- list.files(mData)[grep(fileType,list.files(mData))]
  #  cat("\nfilesofFileType ", filesofFileType)
  
  # Note: added grepteststring, changed grep to grepl and fixed to FALSE June 14, 2018
  greptestString <- paste0("^", fileShortNameTest)
  fileLongName <- filesofFileType[grepl(greptestString, filesofFileType, fixed = FALSE)]
  #  cat("\nfileLongName ", fileLongName)
  #  temp <- gsub(fillIn, "", list.files(mData))
  # filesList <-
  #   grep(regExp,
  #        list.files(mData),
  #        value = TRUE,
  #        perl = TRUE)
  # print(filesList)
  # newestFile <- filesList[length(filesList)]
  
  #  if (length(newestFile) == 0) {
  # check to see if the short file name is in the list from the relevant directory
  # cat("\nfileShortName is ", fileShortName))
  #  print(fileLongName)
  if (length(fileLongName) == 0) {
    cat("\nCan't find ", fileShortName, " in  directory ", mData, "\n")
    stop(sprintf("\nThere is no file  '%s' in directory %s, \n", fileShortName, mData))
  }
  if (length(fileLongName) > 1) {
    cat("\nTwo versions of ", fileShortName, " in  directory ", mData, "\n")
    stop(sprintf("\nTwo or more files with  '%s' in their names in directory %s, \n", fileShortName, mData))
  }
  #       cat(fileLongName)
  outFile = readRDS(paste(mData, fileLongName, sep = "/"))
  return(outFile)
}

#' Title getNewestVersionIMPACT
#' @description read in a .rds file that includes the file fileShortName from the data/IMPACTData directory
#' @param fileShortName The substantive (first) part of the file name.
#' @return The most recent .rds file of IMPACT data
#' @export
getNewestVersionIMPACT <- function(fileShortName) {
  getNewestVersion(fileShortName, fileloc("iData"))
}

#' Title removeOldVersions - removes old version of a rawData file
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
      #      print(oldVersionList)
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

createScriptMetaData <- function(sourceFile) {
  if (!exists("metadataDT")){
    metadataDT <<- data.table(outName = character(0), sourcecode = character(0), destDir = character(0), desc = character(0), colNames = character(0))
  }
}

finalizeScriptMetadata <- function(metadataDT, sourceFile) {
  write.csv(metadataDT, file = paste("data/metadata", sourceFile, "csv", sep = "."), row.names = FALSE)
}

clearMemory <- function(sourceFile, gdxChoice = gdxChoice) {
  cat("Clearing memory after running", sourceFile, "\n")
  rmList <- ls(envir = as.environment(1)) # pos=1 says do this in the global environment
  rmList <- rmList[!rmList %in% "gdxChoice"]
  rm(list = rmList, envir = as.environment(1))
  
  # The sourcer function is run here in case the automate script is used to run all the R scripts
  sourcer <- function(sourceFile){
    cat("\nRunning ", sourceFile, "\n")
    sourceFile <- paste0("R/", sourceFile)
    source(sourceFile)
  }
  return(sourcer)
}
#' Title cleanup - remove old versions and save rds and xlsx or csv versions of the file
#' @param inDT - name of the data table or frame to be written out
#' @param outName - short name of the file to be written out
#' @param destDir - directory where the cleanup takes place
#' @param writeFiles - format to use for writing output in addition to RDS
#' @desc brief description of the contents of the file
cleanup <- function(inDT, outName, destDir, writeFiles, desc) {
  sourceFile <- get("sourceFile", envir = .GlobalEnv)
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  if (missing(destDir)) {destDir = fileloc("mData")}
  
  colNames <- paste(colnames(inDT), collapse = ", ")
  outInfo <- list(outName, sourceFile, destDir, desc, colNames)
  metadataDT <<- rbind(metadataDT, outInfo)
  #  cat("\n", "Outfilename: ", outName, " Destination: ", Destination," Script: ", sourceFile," Desc: ", desc," Col. names: ", colNames, "\n")
  #convert to a standard order
  oldOrder <- names(inDT)
  startOrder <- c("scenario",keyVariable("region"),"year")
  if (all(startOrder %in% oldOrder)) {
    remainder <- oldOrder[!oldOrder %in% startOrder]
    data.table::setcolorder(inDT,c(startOrder,remainder))
    data.table::setorderv(inDT,c(startOrder,remainder))
  }
  
  removeOldVersions(outName, destDir)
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
  # print(proc.time())
  # next line removes any key left in the inDT data table; this may be an issue if a df is used
  data.table::setkey(inDT, NULL)
  outFile <- paste(destDir, "/", outName, "_", Sys.Date(), ".rds", sep = "")
  saveRDS(inDT, file = outFile)
  
  # # update files documentation -----
  # Note: fileDocumentation.csv is currently not being used.
  # fileDoc <- data.table::as.data.table(read.csv(paste(fileloc("rawData"), "fileDocumentation.csv", sep = "/"),
  #     header = TRUE, colClasses = c("character","character","character")))
  # fileDoc <- fileDoc[!fileShortName == outName]
  # fileDocUpdate <- as.list(c(outName, outFile, paste0(names(inDT), collapse = ", ")))
  # fileDoc <- rbind(fileDoc, fileDocUpdate)
  # write.csv(fileDoc, paste(fileloc("mData"), "fileDocumentation.csv", sep = "/"), row.names = FALSE)
  #
  # #print(proc.time())
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  if (nrow(inDT) > 75000) {
    sprintf("\nThe number of rows in the data, %s, is greater than 50,000. Not writing xlsx or csv", nrow(inDT))
    writeFiles <- writeFiles[!writeFiles %in% c("xlsx")]
  }
  if ("csv"  %in% writeFiles) {
    sprintf("\nWriting the csv for %s to %s ", outName, destDir)
    write.csv(inDT,file = paste(destDir, "/", outName, "_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
  }
  if ("xlsx"  %in% writeFiles) {
    #    cat("\nwriting the xlsx for ", outName, " to ", dir, sep = ""))
    wbGeneral <- openxlsx::createWorkbook()
    longName <- outName
    outName <- strtrim(outName, c(31))
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = outName)
    
    openxlsx::writeDataTable(
      wbGeneral,
      inDT,  sheet = outName, startRow = 1, startCol = 1, rowNames = FALSE,
      colNames = TRUE, withFilter = TRUE)
    
    openxlsx::setColWidths(
      wbGeneral, sheet = outName, cols = 1:ncol(inDT), widths = "auto" )
    
    numStyle <- openxlsx::createStyle(numFmt = "0.000")
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = numStyle, rows = 1:nrow(inDT) + 1, cols = 2:ncol(inDT), # +1 added Mar 24, 2017
      gridExpand = TRUE )
    
    xcelOutFileName = paste(destDir, "/", longName, "_", Sys.Date(), ".xlsx", sep = "") # added longName functionality June 20, 2018
    openxlsx::saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)
    #   cat("\nDone writing the xlsx for ", outName, sep = "")
    #  print(proc.time())
  }
}

cleanupGraphFiles <- function(inGraphFile, outName, destDir, desc) {
  sprintf("start cleanup for %s", outName)
  sourceFile <- get("sourceFile", envir = .GlobalEnv)
  removeOldVersions(outName, destDir)
  if (missing(destDir)) {destDir = fileloc("mData")}
  
  colNames <- paste(inDT$names, collapse = ", ")
  outInfo <- list(outName, sourceFile, destDir, desc, colNames)
  metadataDT <<- rbind(metadataDT, outInfo)
  
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
  # print(proc.time())
  # next line removes any key left in the inDT data table; this may be an issue if a df is used
  data.table::setkey(inDT, NULL)
  outFile <- paste(destDir, "/", outName, "_", Sys.Date(), ".rds", sep = "")
  saveRDS(inGraphFile, file = outFile)
}

cleanupNutrientNames <- function(nutList) {
  nutList <- gsub("_g_reqRatio","",nutList)
  nutList <- gsub("reqRatio","",nutList)
  nutList <- gsub("vit_","Vitamin ",nutList)
  nutList <- gsub("_µg","",nutList)
  nutList <- gsub("_mg","",nutList)
  nutList <- gsub("_g","",nutList)
  nutList <- gsub("totalfiber","total fiber",nutList)
  nutList <- gsub(".ratio.foodGroup","",nutList)
 nutList <- gsub(".sum.all","",nutList)
  nutList <- gsub("rootsNPlantain","roots and plantain",nutList)
  nutList <- gsub("nutsNseeds","nuts and oilseeds",nutList)
  nutList <- gsub("alcohol","alcoholic beverages",nutList)
  nutList <- gsub("beverages","nonalcoholic beverages",nutList)
  nutList <- gsub("alcoholic nonalcoholic beverages","alcoholic beverages",nutList)
  nutList <- gsub("ft_acds_tot_sat", "saturated fat", nutList)
  nutList <- gsub("_g_AMDR", "", nutList)
  nutList <- gsub("Vitamin b", "Vitamin B", nutList)
  nutList <- gsub("Vitamin c", "Vitamin C", nutList)
  nutList <- gsub("Vitamin d", "Vitamin D", nutList)
  nutList <- gsub("Vitamin e", "Vitamin E", nutList)
  nutList <- gsub("Vitamin k", "Vitamin K", nutList)
  nutList <- TC(nutList)
  nutList <- gsub("A_rae","A (RAE)",nutList)
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
#' @param reqsList_RDA_macro <- list of RDAs for macro nutrients
#' @param reqsList_RDA_minrls <- list of RDAs for mineral nutrients
#' @param reqsList_RDA_vits <- list of RDAs for vitamin nutrients
#' @param reqsListSSP - nutrient requirements by SSP age groups
#' @param ctyDeleteList
#' @param switch.useCookingRetnValues - apply the cooking retention values to the nutrient content
#' @param userName - Name of person running the scripts and generating results
#' @param region -  Aggregation scheme from individual countries to regions
#' @param switch.fixFish - if TRUE, fix salmon, tuna, and crustaceans
#' @param switch.changeElasticity - if TRUE, set max fish income elasticities to 1
#' @param commonList - names of the lists of nutrient names common to the nutrient lookup table and the requirements
#' @param dropListCty
#' @param allAfricaCodes
#' @param SSAfricaCodes
#' @param regions.AfricanAgFutures
#' @param tenregions

#' @return list of key variables
#' @export
keyVariable <- function(variableName) {
  switch.fixFish <- "TRUE"
  switch.useCookingRetnValues <- "TRUE"
  switch.changeElasticity <- "TRUE"
  switch.vars <- "TRUE"
  switch.fortification <- "FALSE" # only relevant if switch.vars is TRUE
  region <- "region_code.IMPACT159"
  keepYearList <- c("X2010", "X2015", "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050")
  keepYearList.FBS <- c("X2000", "X2001", "X2002", "X2003", "X2004", "X2005",
                        "X2006", "X2007", "X2008", "X2009", "X2010", "X2011")
  FBSyearsToAverage.baseyear <- c("X2004", "X2005", "X2006")
  FBSyearsToAverage.startyear <- c("X2009", "X2010", "X2011")
  fishComposites <- c("c_Mllsc", "c_ODmsrl", "c_OPelag", "c_Crust", "c_OMarn", "c_FrshD")
  foodGroups <- c("alcohol", "beverages", "cereals", "dairy", "eggs", "fish", "fruits", "meats", "nutsNseeds", "oils", "pulses", "rootsNPlantain", "sweeteners", "vegetables")
  keepListYears.composites <- c("Y2011", "Y2012", "Y2013")
  #' note shrimp, tuna, and salmon are removed in dataManagement.fish.R
  IMPACTfish_code <- c("c_Shrimp", "c_Crust", "c_Salmon", "c_FrshD", "c_ODmrsl",
                       "c_Tuna",  "c_OMarn",  "c_Mllsc", "c_OPelag", "c_FshOil", "c_aqan",
                       "c_aqpl")
  IMPACTalcohol_code <- c("c_wine", "c_beer", "c_spirits")
  IMPACTfoodCommodList <- sort(c("cbeef", "cpork", "clamb", "cpoul", "ceggs", "cmilk", "cbarl", "cmaiz",
                                 "cmill", "crice", "csorg", "cwhea", "cocer", "ccass", "cpota", "cswpt",
                                 "cyams", "corat", "cbean", "cchkp", "ccowp", "clent", "cpigp", "copul",
                                 "cbana", "cplnt", "csubf", "ctemf", "cvege", "csugr", "cgrnd", "cgdol",
                                 "crpol", "csoyb", "csbol", "csnfl", "csfol", "cplol", "cpkol", "crpsd",
                                 "ctols", "ctool", "ccoco", "ccafe", "cteas", "cothr", "ctoml", IMPACTfish_code, #ctoml added May 31 to be consistent with IFPRI list. FAO includes this as a food item.
                                 IMPACTalcohol_code))
  
  macronutrients <- c("carbohydrate_g", "fat_g",  "protein_g", "totalfiber_g")
  vitamins <- c("vit_a_rae_µg", "vit_b6_mg", "vit_b12_µg", "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg",
                "folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg")
  minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
                "potassium_g", "zinc_mg")
  fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                  "ft_acds_tot_trans_g")
  other <- c("ethanol_g", "caffeine_mg", "cholesterol_mg")
  energy <- c("kcals_fat_g", "kcals_carbohydrate_g", "kcals_protein_g",
              "kcals_ethanol_g", "kcals_sugar_g", "kcals_ft_acds_tot_sat_g")  # note that "energy_kcal" is removed from this list
  addedSugar <- c("sugar_g")
  reqsList_RDA_macro <- c("carbohydrate_g", "totalfiber_g", "protein_g")
  reqsList_RDA_vits <- c("vit_a_rae_µg", "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg", "thiamin_mg", "riboflavin_mg",
                         "niacin_mg", "vit_b6_mg", "folate_µg", "vit_b12_µg", "pantothenicacid_mg", "biotin_µg", "choline_mg")
  reqsList_RDA_minrls <- c("calcium_mg", "chromium_μg", "copper_μg", "fluoride_mg", "iodine_μg", "iron_mg",
                           "magnesium_mg", "manganese_mg", "molybdenum_μg", "phosphorus_mg", "selenium_μg", "zinc_mg",
                           "potassium_g", "sodium_g", "chloride_g")
  #These are the scenario numbers for the IIASA data with population disaggregated.
  scenarioListSSP.pop <- c("SSP1_v9_130115", "SSP2_v9_130115", "SSP3_v9_130115",
                           "SSP4_v9_130115", "SSP5_v9_130115")
  scenarioListSSP.GDP <- c("SSP1_v9_130325", "SSP2_v9_130325", "SSP3_v9_130325",
                           "SSP4_v9_130325", "SSP5_v9_130325")
  SSAfricaCodes <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR",
                     "COD", "COG", "DJI", "ERI", "ETH", "GAB", "GHA", "GIN",
                     "GMB", "GNB", "GNQ", "KEN", "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", # removed "MOR". It's in north Africa. Added Mauritania
                     "MWI", "NAM", "NER", "NGA", "RWA", "SDN", "SDP", "SEN", "SLE", #removed "OAO" other Atlantic Ocean
                     "SOM", "SSD", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
  
  allAfricaCodes <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "DJI", "DZA", 
                      "EGY", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MDG", "MLI", 
                      "MOR", "MOZ", "MRT", "MWI", "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SWZ", "TCD", "TGO", 
                      "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
  regions.AfricanAgFutures <- c("ETH", "NGA", "MWI", "MOZ", "ZMB", "ZWE")
  tenregions <- sort(c("NIC", "BRA", "CHM", "ETH", "IND", "GHA", "TZA", "FRP", "VNM", "USA"))
  
  # scenarioListIMPACT <- as.character(read.csv(file = paste(fileloc("mData"),"scenarioListIMPACT.csv", sep = "/"), stringsAsFactors = FALSE)[,1])
  DinY <- 365 #see http://stackoverflow.com/questions/9465817/count-days-per-year for a way to deal with leap years
  #' #' countries to remove because of poor data
  #' FSM - Micronesia, Federated States of
  #' GRD - Grenada
  #' PRK - Korea, Democratic People's Republic of
  #' GRL - has no data in FAO's FBS
  reqsList <-
    c(
      "req_EAR",
      "req_RDA_vits",
      "req_RDA_minrls",
      "req_RDA_macro",
      "req_UL_vits",
      "req_UL_minrls",
      "req_AMDR_hi",
      "req_AMDR_lo",
      "req_MRVs" # added March 24, 2017
    )
  # ,
  
  reqsListPercap <- paste(reqsList,"_percap", sep = "")
  reqsListSSP <- paste(reqsList,"_ssp", sep = "")
  dropListCty <- c("GRL", "FSM", "GRD", "PRK")
  commonList <- paste("common.", reqsList, sep = "")
  c( "common.EAR", "common.RDA.vits", "common.RDA.minrls", "common.RDA.macro", "common.AMDR_hi", "common.AMDR_lo","common.UL.vits","common.UL.minrls")
  userName <- "Gerald C. Nelson"
  if (variableName == "list") {
    return(
      c(
        "switch.fixFish",
        "switch.changeElasticity",
        "switch.useCookingRetnValues",
        "switch.vars",
        "switch.fortification",
        "region",
        "keepYearList",
        "keepYearList.FBS",
        "FBSyearsToAverage",
        "foodGroups",
        "IMPACTfish_code",
        "IMPACTalcohol_code",
        "IMPACTfoodCommodList",
        "scenarioListSSP.pop",
        "scenarioListSSP.GDP",
        # "scenarioListIMPACT",
        "DinY",
        "reqListSSP",
        "switch.useCookingRetnValues",
        "commonList",
        "userName",
        "dropListCty",
        "macronutrients",
        "vitamins",
        "minerals",
        "energy",
        "fattyAcids",
        "other",
        "addedSugar",
        "allAfricaCodes", 
        "SSAfricaCodes"
      )
    )
  } else {
    return(eval(parse(text = variableName)))
  }
}

# used in automate.R
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
    c(fileNameList("regionsLookup"),
      "Lookup table from ISO codes to various regional groupings")
  metadata[(nrow(metadata) + 1), ] <-
    c(gdxFileName, "IMPACT demand data in gdx form")
  metadata[(nrow(metadata) + 1), ] <-
    c(gdxChoice, "project source of gdx-based demand data")
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
  inDT <- data.table::as.data.table(metadata)
  outName <- "dt.metadata"
  desc <- "Metadata of various kinds"
  cleanup(inDT,outName, fileloc("resultsDir"), desc = desc)
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
#' @param dt.foodGroupLUFileName - file name for the commodity to food group lookup spreadsheet
#' @param dt.foodGroupLU - path and file name for the commodity to food group lookup
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
  IMPACTCleanData <- fileloc("IMPACTCleanData")
  nutrientDataDetails <- paste(fileloc("rawData"), "NutrientData/nutrientDetails", sep = "/")
  SSPData <- fileloc("SSPData")
  DRIFileName <- "DRI_IOM_V8.xlsx"
  mData <- fileloc("mData")
  DRIs  <- paste(nutrientDataDetails, DRIFileName, sep = "/")
  # CSE - consumer support equivalent
  #Note: the price a consumer pays is Pc * (1-CSE)
  CSEFileName <- "CSEs20150824.xlsx"
  CSEs  <- paste(fileloc("IMPACTRawData"), CSEFileName, sep = "/")
  regionsLookupName <- "regions lookup June 15 2018.xlsx" # adds regions for the USAID priorities work
  regionsLookup <- paste(fileloc("rawData"),regionsLookupName, sep = "/")
  #IMPACTgdx <- paste(fileloc("IMPACTRawData"), IMPACTgdxfileName, sep = "/")
  #gdxLib  <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
  R_GAMS_SYSDIR <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
  IMPACTfishInfo <- "Fish Elasticities and Quantities IMPACT.xlsx"
  IMPACTfish   <- paste(fileloc("IMPACTRawData"), IMPACTfishInfo, sep = "/")
  IMPACTalcoholInfo <- "Alcohol Elasticities and Quantities IMPACT.xlsx"
  IMPACTalcohol   <- paste(fileloc("IMPACTRawData"), IMPACTalcoholInfo, sep = "/")
  IMPACTfoodFileName <- "dt.IMPACTfood"
  IMPACTfoodFileInfo <-  paste(mData,"/IMPACTData/",IMPACTfoodFileName,sep = "")
  # nutrient data ------
  nutrientFileName <-  "dt.IMPACTnutrientlookup"
  nutrientLU  <- paste(mData, nutrientFileName, sep = "/")
  foodGroupLUFileName <- "food commodity to food group table V4.xlsx"
  foodGroupLU <- paste(nutrientDataDetails, foodGroupLUFileName, sep = "/")
  # SSP information ----
  if (variableName %in% c("SSPcsv", "SSPdataZip")) {
    SSPdataZipFile   <- "SspDb_country_data_2013-06-12.csv.zip"
    SSPdataZip  <- paste(SSPData, SSPdataZipFile, sep = "/")
    #get the name of the file inside the zip. Assumes only 1
    zipHolder   <- unzip(SSPdataZip, list = TRUE)
    SSPcsv <- zipHolder$Name[1]
  }
  modelListPop <- "IIASA-WiC POP"
  modelListGDP <- "OECD Env-Growth"
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
        #       "IMPACTgdxfileName",
        #       "IMPACTgdx",
        "gdxLib",
        "R_GAMS_SYSDIR",
        "IMPACTfishInfo",
        "IMPACTfish",
        "IMPACTfood",
        "nutrientFileName",
        "nutrientLU",
        "foodGroupLUFileName",
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
  rawData <- fileloc("rawData")
  #' FBS to ISO lookup table
  FBSlookupTableLink <- "http://www.fao.org/countryprofiles/iso3list/en/"
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
  
  countryNameLookup <- function(countryCode, directory) {
    if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
    dt.regions <- getNewestVersion('dt.regions.all', fileloc("uData"))
    if (!countryCode %in% dt.regions$region_code.IMPACT159) {
      stop(sprintf("The country code you entered (%s) is not in the lookup table", countryCode))
    } else {
      countryName <- dt.regions[region_code.IMPACT159 == countryCode,region_name.IMPACT159]
      return(countryName)
    }
  }
  countryCodeLookup <- function(countryName, directory) {
    if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
    dt.regions <- getNewestVersion('dt.regions.all', fileloc("uData"))
    if (!countryName %in% dt.regions$region_name.IMPACT159) {
      stop(sprintf("The country name you entered (%s) is not in the lookup table", countryName))
    } else {
      countryCode <- dt.regions[region_name.IMPACT159 == countryName, region_code.IMPACT159]
      return(countryCode)
    }
  }
  
  countryCodeCleanup <- function(DT) {
    #converts IMPACT code to ISO3 code for largest country in the region.
    DT <- DT[region_code.IMPACT159 %in% "FRP", region_code.IMPACT159 := "FRA"]
    DT <- DT[region_code.IMPACT159 %in% "CHM", region_code.IMPACT159 := "CHN"]
    DT <- DT[region_code.IMPACT159 %in% "CHP", region_code.IMPACT159 := "CHE"]
    DT <- DT[region_code.IMPACT159 %in% "DNP", region_code.IMPACT159 := "DNK"]
    DT <- DT[region_code.IMPACT159 %in% "FNP", region_code.IMPACT159 := "FIN"]
    DT <- DT[region_code.IMPACT159 %in% "ITP", region_code.IMPACT159 := "ITA"]
    DT <- DT[region_code.IMPACT159 %in% "MOR", region_code.IMPACT159 := "MAR"]
    DT <- DT[region_code.IMPACT159 %in% "SPP", region_code.IMPACT159 := "ESP"]
    DT <- DT[region_code.IMPACT159 %in% "UKP", region_code.IMPACT159 := "GBR"]
    DT <- DT[region_code.IMPACT159 %in% "BLX", region_code.IMPACT159 := "BEL"]
    DT <- DT[region_code.IMPACT159 %in% "SDP", region_code.IMPACT159 := "SDN"]
    DT <- DT[region_code.IMPACT159 %in% "RAP", region_code.IMPACT159 := "ARE"]
    DT <- DT[region_code.IMPACT159 %in% "GSA", region_code.IMPACT159 := "SUR"]
    DT <- DT[region_code.IMPACT159 %in% "CRB", region_code.IMPACT159 := "TTO"]
    DT <- DT[region_code.IMPACT159 %in% "OSA", region_code.IMPACT159 := "SIN"]
    DT <- DT[region_code.IMPACT159 %in% "BLT", region_code.IMPACT159 := "LTU"]
    DT <- DT[region_code.IMPACT159 %in% "OBN", region_code.IMPACT159 := "SRB"]
    DT <- DT[region_code.IMPACT159 %in% "OAO", region_code.IMPACT159 := "CPV"]
    DT <- DT[region_code.IMPACT159 %in% "OIO", region_code.IMPACT159 := "MDV"]
    DT <- DT[region_code.IMPACT159 %in% "OPO", region_code.IMPACT159 := "WSM"]
    return(DT)
  }
  
  # test data for reqRatiodatasetup
  countryCode <- "AFG"; years <- c("X2010", "X2030", "X2050")
  reqTypeChoice <- "RDA_reqRatio_macro_all"
  
  regionAgg <- function(aggChoice) {
    # region info setup for aggregating -----
    dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
    I3regions <- sort(unique(dt.regions.all$region_code.IMPACT159))
    tenregions <- keyVariable("tenregions") # returns country codes
    regions.AfricanAgFutures <- keyVariable("regions.AfricanAgFutures") # returns country codes
    AfricaNorth <- c()
    AfricaSouth <- c()
    AfricaWest <- c("Benin", "Burkina Faso", "Cape Verde", "Ivory Coast", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                    "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
    dropList.AfricaWest <- c("Cape Verde", "Gambia", "Guinea", "Guinea-Bissau", "Mauritania")
    AfricaEast <- sort(c("Tanzania", "Kenya", "Uganda", "Rwanda", "Burundi", "South Sudan", "Djibouti", "Eritrea", "Ethiopia", 
                         "Somalia", "Comoros", "Mauritius", "Seychelles", "Mozambique", "Madagascar", "Malawi", "Zambia", "Sudan"))
    dropList.AfricaEast <- c("Comoros", "Djibouti", "Eritrea", "Seychelles", "Somalia", "South Sudan")
    AggReg1 <- sort(unique(dt.regions.all$region_code.AggReg1))
    AggReg2 <- sort(unique(dt.regions.all$region_code.AggReg2))
    twoEconGroup <- sort(unique(dt.regions.all$region_code.EconGroup))
    WB <- sort(unique(dt.regions.all$region_code.WB.income))
    regionNamestenregions <- unique(dt.regions.all[region_code.IMPACT159 %in% tenregions, region_name.IMPACT159])
    regionNamesAggReg1 <- unique(dt.regions.all$region_name.AggReg1)
    regionNamesAggReg2 <- unique(dt.regions.all$region_name.AggReg2)
    regionNamestwoEconGroup <- unique(dt.regions.all$region_name.EconGroup)
    regionNamesWB <- unique(dt.regions.all$region_name.WB)
    
    # regionCodestenregions
    if (aggChoice == "tenregions") {
      keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
      dt.regions.all <- dt.regions.all[region_code.IMPACT159 %in% tenregions,]
      dt.regions.all <- dt.regions.all[, region_code := region_code.IMPACT159]
    }
    # regionCodes regions.AfricanAgFutures
    if (aggChoice == "regions.AfricanAgFutures") {
      keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
      dt.regions.all <- dt.regions.all[region_code.IMPACT159 %in% regions.AfricanAgFutures,]
      dt.regions.all <- dt.regions.all[, region_code := region_code.IMPACT159]
    }
    # regionCodesI3regions
    if (aggChoice == "I3regions") {
      keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
      dt.regions.all <- dt.regions.all[, region_code := region_code.IMPACT159]
    }
    # regionCodesAggReg1
    if (aggChoice == "AggReg1") {
      keepListCol <- c("region_code.IMPACT159", "region_code.AggReg1", "region_name.AggReg1")
    }
    # regionCodesAggReg2
    if (aggChoice == "AggReg2") {
      keepListCol <- c("region_code.IMPACT159", "region_code.AggReg2", "region_name.AggReg2")
    }
    # regionCodestwoEconGroup
    if (aggChoice == "twoEconGroup") {
      keepListCol <- c("region_code.IMPACT159", "region_code.EconGroup", "region_name.EconGroup")
    }
    # regionCodesWB
    if (aggChoice == "WB") {
      keepListCol <- c("region_code.IMPACT159", "region_code.WB.income", "region_name.WB.income")
    }
    dt.regions <- unique(dt.regions.all[, (keepListCol), with = FALSE])
    data.table::setnames(dt.regions, old = keepListCol, new = c("region_code.IMPACT159", "region_code", "region_name"))
    return(dt.regions)
  }
  
  gdxrrwExistenceCheck <- function(){
    # the GAMS gdxrrw package is needed to import data from IMPACT (in R scripts gdxrrwSetup.R, dataPrep.IMPACT.R and dataManagement.IMPACT.R)
    gdxrrwText <- 'The gdxrrw package is needed to run this. It is not available from CRAN; use this url:
https://support.gams.com/gdxrrw:interfacing_gams_and_r. Download the relevant file and use the following command to install
- install.packages("gdxrrw_1.0.4.tgz",repos = NULL). Replace gdxrrw_1.0.4.tgz with the
name of the file you downloaded. Note that if you need the source version, download and install gdxrrw_1.0.4.tar.gz or a newer version. If you put it in the main directory of your project, the install.packages command will find it.
After GAMS is installed you need to tell R where the GAMS library is located. Here are some examples
- mac installation - /Applications/GAMS/gams24.5_osx_x64_64_sfx
- linux installation - /opt/gams/gams24.3_linux_x64_64_sfx
- windows installation - C:\\GAMS\\win32\24.7
  Sometimes there is a problem with the way the package has been zipped. If you get a message like
rawToChar(block[seq_len(ns)], run the following commands in a shell.

- mv gdxrrw_1.0.4.tar.gz foo.tar.gz
- gunzip foo.tar.gz
- tar xf foo.tar
- tar czf gdxrrw_1.0.4.tar.gz gdxrrw'
    
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    list.of.packages <- c("gdxrrw")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if (!length(new.packages) == 0) {
      cat(gdxrrwText)
      stop("gdxrrw package not installed")
    }
  }
  
  gdxLibraryLocationCheck <- function() {
    dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
    gdxLibLoc <- dt.metadata[file_description %in%
                               "Location and name of GAMS program; needed for the gdx data import process", file_name_location]
    if (gdxrrw::igdx(gamsSysDir = gdxLibLoc, silent = TRUE) %in% 0L) {
      cat("\nThe nutrient modeling software thinks your GAMS liberary path is", gdxLibLoc, " R can't find it there.")
      if (choice %in% "n") {
        cat("\nType the correct path in the console; e.g. C:\\GAMS\\win32\24.7 ")
        GAMSlibloc <- readline()
        dt.metadata[file_description %in% "Location and name of GAMS program; needed for the gdx data import process", file_name_location := GAMSlibloc]
        cat("\nThe nutrient modeling software now thinks your GAMS liberary path is", gdxLibLoc)
        inDT <- dt.metadata
        outName <- dt.metadata.gdx
        desc <- "Metadata on the gdx file with IMPACT results."
        cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
      }
    }
  }
  
  switchChoices <- function(switchChoice) {
    switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
    switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
    if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
    if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
    if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  }
  
  gdxFileNameChoice <- function() {
    # this function needs to have the same choices as getGdxChoice
    
    cat("Choose the IMPACT data gdx file you want to use.\n")
    cat("1. for the nutrient modeling paper\n")
    cat("2. for the USAID priority setting paper, 2018\n")
    cat("3. for the African Agricultural Futures project\n")
    cat("Note: the relevant gdx file must be in the data-raw/IMPACTdata directory\n")
    choice <- readline(prompt = "Choose the number of the gdx file you want to use. \n")
    #  choice <- "1" # so there will be a definite value
    if (choice  %in% "1") {
      gdxFileName <- "Micronutrient-Inputs-07252016.gdx" # - gdx with multiple SSP results
      gdxChoice <- "SSPs"
    }
  
    if (choice %in% "2") {
      gdxFileName <- "Micronutrient-Inputs-7.1.2018.gdx"  #-  gdx for the USAID prod enhance results
      gdxChoice <- "USAIDPrdNhance"
    }
    
    if (choice %in% "3") {
      #    gdxFileName <- "Africa-NutMod-Inputs-2018.09.21.gdx"  #-  gdx for the African ag futures results
      gdxFileName <- "Africa-NutMod-Inputs-1018.10.21.gdx"  #-  gdx for the African ag futures results
      gdxChoice <- "AfricanAgFutures"
    }
    
    cat("\nYour gdx file name choice is ", gdxFileName, "\n")
    cat("Choose the switches you want to use.\n")
    cat("1. Base nutrient data only.\n")
    cat("2. Base nutrient data and country specific varieties.\n")
    cat("3. Base nutrient data, country specific varieties, and selected fortification.\n")
    cat("4. Country specific varieties only.\n")
    choice <- readline(prompt = "Choose 1, 2, 3 or 4. \n")
    if (!choice %in% c(1,2,3,4)) {stop(sprintf("Your choice %s needs to be one of 1, 2, or 3", choice))}
    
    cat("\nDo you need to update any files that are not IMPACT specific (new population data, nutrient composition, FBS data?")
    update <- readline(prompt = "Y/N. \n")
    gdxSwitchCombo <- cbind(gdxFileName, gdxChoice, choice, update)
    
    write.csv(gdxSwitchCombo, file = paste0(getwd(), "/results/", gdxChoice, "/gdxInfo.csv"), row.names = FALSE)
    return(gdxSwitchCombo)
  }
  
  
  # store world map dataframe -----
  storeWorldMapDF <- function(){
    library("maptools")
    require("rgdal")
    require("rgeos")
    require("dplyr")
    sourceFile <- "storeWorldMapDF in R/nutrientModFunctions.R"
    createScriptMetaData()
    # check to see if worldMap already exists
    # naturalearth world map geojson
    # updated source of data is http://www.naturalearthdata.com/downloads/50m-cultural-vectors/
    
    #world <- readOGR(dsn="https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
    #world <- readOGR(dsn = "data-raw/spatialData/ne_50m_admin_0_countries.geojson", layer = "OGRGeoJSON")
    #  world <- rgdal::readOGR(dsn = "data-raw/spatialData/ne_110m_admin_0_countries.geojson", layer = "OGRGeoJSON")
    
    # changed to uDir June 9, 2018  filelocMap <- "data-raw/spatialData"
    filelocMap <- fileloc("uData")
    fn <- file.path(filelocMap, "ne_50m_admin_0_countries.zip")
    cat("\n") # so the output from download.file starts on a new line
    
    # temp <- list.files(fileloc("uData"))
    
    # if (length(grep("worldMap", temp)) == 0) { commented out July 22, 2018
    # on the assumption that this is only run when something is wrong with the existing map.
    suppressMessages(download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", fn))
    unzip(fn, exdir = filelocMap)
    
    #   shpFile <- file.path(filelocMap, "ne_50m_admin_0_countries")
    world.raw <- rgdal::readOGR(dsn = filelocMap, layer = "ne_50m_admin_0_countries")
    # centroids of interest if adding names to the center of a polygon. Not used at the moment
    # centroids.df <- as.data.frame(coordinates(world.raw))
    # names(centroids.df) <- c("Longitude", "Latitude")
    # create new regions code is based on https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/
    
    #keep just the basic information needed
    regions.to.merge <- c("SSD", "SDN") # ISO codes for all the countries in the new region
    dataToKeep <- c("ISO_A3", "NAME_EN", "REGION_UN") # get the complete list with names(world.raw)
    #need to have values for each of the items in dataToKeep for the new region
    name.newRegion <- "Sudan"
    ISOcode.newRegion <- "SDN"
    REGION_UN.newRegion <- "Africa"
    lu <- data.frame(ISO_A3 = ISOcode.newRegion,
                     NAME_EN = name.newRegion,
                     REGION_UN = REGION_UN.newRegion) # data frame to be added to the new region polygon
    
    world.raw@data <- world.raw@data[,dataToKeep]
    world.raw.subset <- subset(world.raw, ISO_A3 %in% regions.to.merge)
    
    # add a column to disolve over with gUnaryUnion
    world.raw.subset@data$region.new <- name.newRegion
    world.raw.subset <- gUnaryUnion(world.raw.subset, id = world.raw.subset@data$region.new)
    
    #now add back in the descriptive data data frame
    # If you want to recreate an object with a data frame
    # make sure row names match
    row.names(world.raw.subset) <- as.character(1:length(world.raw.subset))
    world.raw.subset <- SpatialPolygonsDataFrame(world.raw.subset, lu)
    
    # now remove the merged regions from the original file
    temp <- subset(world.raw, !ISO_A3 %in% regions.to.merge)
    
    # now add back the new region
    world.raw <- rbind(temp, world.raw.subset)
    
    # remove antarctica and some other small countries
    world <- world.raw[!world.raw$ISO_A3 %in% c("ATA"),]
    othersToRemove <- c("ABW", "AIA", "ALA", "AND", "ASM", "AFT")
    world <- world[!world$ISO_A3 %in% othersToRemove,]
    SSAfricaCodes <- keyVariable("SSAfricaCodes")
    allAfricaCodes <- keyVariable("allAfricaCodes")
    
    africa <- world[world@data$REGION_UN == "Africa",]
    africa$REGION_UN <- factor(africa$REGION_UN)
    europe <-   world[world@data$REGION_UN  == "Europe",]
    europe$REGION_UN <- factor(europe$REGION_UN)
    americas <- world[world@data$REGION_UN  == "Americas",]
    americas$REGION_UN <- factor(americas$REGION_UN)
    asia <- world[world@data$REGION_UN  == "Asia",]
    asia$REGION_UN <- factor(asia$REGION_UN)
    
    # alternate projections
    projVal.longlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84  +towgs84=0,0,0'
    # wintri projection
    projVal.wintri <- "+proj=wintri +datum=WGS84 +no_defs +over" #this doesn't work when you try to create a shapefile with raster:shapefile
    # mercator projection
    projVal.mercat <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
    #Robinson projection
    projVal.robin <- "+proj=robin +datum=WGS84 +ellps=WGS84"
    
    defaultProj <- projVal.robin
    
    world <- sp::spTransform(world, CRSobj = defaultProj)
    africa <- sp::spTransform(africa, CRSobj = defaultProj)
    europe <- sp::spTransform(europe, CRSobj = defaultProj)
    americas <- sp::spTransform(americas, CRSobj= defaultProj)
    asia <- sp::spTransform(asia, CRSobj = defaultProj)
    
    fpusMap <- readOGR(dsn = "data-raw/IMPACTData/IMPACT_FPU_Map", layer = "fpu2015_polygons_v3_multipart_polygons", stringsAsFactors = FALSE)
    fpusMap <- sp::spTransform(fpusMap, CRSobj = defaultProj)
    fpusMap <- gBuffer(fpusMap, byid=TRUE, width=0)
    # fpusMap$FPU2015 <- factor(fpusMap$FPU2015)
    # fpusMap$USAFPU <- factor(fpusMap$USAFPU)
    # 
    data.fpus <- fpusMap@data
    data.fpus$region_code.IMPACT159 <- substring(data.fpus$FPU2015,5,7) # add region_code.IMPACT159 column to the fpusMap data frame
    fpusMap@data <- data.fpus
    fpusMap$region_code.IMPACT159 <- factor(fpusMap$region_code.IMPACT159)
    
    # keep only the SSAfrica countries for data.fpus.SSA
    fpusMap_SSA <- subset(fpusMap, region_code.IMPACT159  %in% SSAfricaCodes)
    # redo factor after subsetting
    fpusMap_SSA$FPU2015 <- factor(fpusMap_SSA$FPU2015)
    fpusMap_SSA$USAFPU <- factor(fpusMap_SSA$USAFPU)
    fpusMap_SSA$region_code.IMPACT159 <- factor(fpusMap_SSA$region_code.IMPACT159)
    
    # all the fpus on the African continent
    allAfricaCodes <- keyVariable("allAfricaCodes")
    fpusMap_allAfrica <- subset(fpusMap, region_code.IMPACT159  %in% allAfricaCodes)
    # redo factor after subsetting
    fpusMap_allAfrica$FPU2015 <- factor(fpusMap_allAfrica$FPU2015)
    fpusMap_allAfrica$USAFPU <- factor(fpusMap_allAfrica$USAFPU)
    fpusMap_allAfrica$region_code.IMPACT159 <- factor(fpusMap_allAfrica$region_code.IMPACT159)
    
    # raster::shapefile(fpusMap_SSA, "fpusMap_SSA.shp", overwrite = TRUE)
    
    # fpusMap_SSA@data <- data.fpus.SSA
    
    
    # saveRDS(world, file = paste(filelocMap, "worldFile.RDS", sep = "/"))
    # saveRDS(africa, file = paste(filelocMap, "africaFile.RDS", sep = "/"))
    # saveRDS(europe, file = paste(filelocMap, "europeFile.RDS", sep = "/"))
    # saveRDS(americas, file = paste(filelocMap, "americasFile.RDS", sep = "/"))
    # saveRDS(asia, file = paste(filelocMap, "asiaFile.RDS", sep = "/"))
    # saveRDS(fpusMap, file = paste(filelocMap, "fpusMap.RDS", sep = "/"))
    # 
    
    #world.simp <- gSimplify(world, tol = .1, topologyPreserve = TRUE)
    # alternative would be CRS("+proj=longlat")) for WGS 84
    # dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
    # pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)
    # gpclibPermit() # seems to be needed now May 29, 2018 but maybe not
    worldMap <- broom::tidy(world, region = "ISO_A3")
    worldMap$region <- worldMap$id # added Juy 22, 2018 to get rid of warning about a missing region column
    inDT <- worldMap
    outName <- "worldMap"
    desc <- "World map file used to create facet maps of the world"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    africaMap <- broom::tidy(africa, region = "ISO_A3")
    africaMap$region <- africaMap$id # added Juy 22, 2018 to get rid of warning about a missing region column
    inDT <- africaMap
    outName <- "africaMap"
    desc <- "Africa map file used to create facet maps of Africa"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    europeMap <- broom::tidy(europe, region = "ISO_A3")
    inDT <- europeMap
    outName <- "europeMap"
    desc <- "Europe map file used to create facet maps of Europe"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    americasMap <- broom::tidy(americas, region = "ISO_A3")
    inDT <- americasMap
    outName <- "americasMap"
    desc <- "Americas map file used to create facet maps of Americas"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    asiaMap <- broom::tidy(asia, region = "ISO_A3")
    inDT <- asiaMap
    outName <- "asiaMap"
    desc <- "Asia map file used to create facet maps of Asia"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    fpusMap <- broom::tidy(fpusMap, region = "FPU2015")
    inDT <- fpusMap
    outName <- "fpusMap"
    desc <- "World map file with food production unit (FPU) polygons used to create facet maps of FPU level data"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    fpusMap_SSA <- broom::tidy(fpusMap_SSA, region = "FPU2015")
    fpusMap_SSA$region <- fpusMap_SSA$id
    inDT <- fpusMap_SSA
    outName <- "fpusMap_SSA"
    desc <- "Sub-Saharan AFrica map file with food production unit (FPU) polygons used to create facet maps of FPU level data"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
    
    fpusMap_allAfrica <- broom::tidy(fpusMap_allAfrica, region = "FPU2015")
    fpusMap_allAfrica$region <- fpusMap_allAfrica$id
    inDT <- fpusMap_allAfrica
    outName <- "fpusMap_allAfrica"
    desc <- "All AFrica map file with food production unit (FPU) polygons used to create facet maps of FPU level data"
    cleanup(inDT, outName, fileloc("uData"), desc = desc)
  }
  
  generateBreakValues <- function(fillLimits, numLimits, decimals) { # added Sep 13, 2018. May not be appropriate
    fillRange <- fillLimits[2] - fillLimits[1]
    fillStep <- fillRange/numLimits
    breakValues <- numeric(numLimits)
    breakValues[1] <- round(fillLimits[1], digits = decimals)
    breakValues[numLimits] <- round(fillLimits[2], digits = decimals)
    for (i in 2:(numLimits - 1)) {
      breakValues[i] <- breakValues[i-1] + fillStep
    }
    #' middle two values shift the palette gradient
    #  breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
    # breakValues <- round(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high), digits = decimals)
    #  breakValues <- rescale(breakValues, to = c(0,1)) # the break values MUST be from 0 to 1. Already done in facetMaps() July 10, 2018
    return(breakValues)
  }
  
  library(scales)
  # new version that allows different maps, added Sep 13, 2108. Not sure why graphsListHolder left out. Added back in Oct 8, 2018
  facetMaps <- function(mapFile, DTfacetMap, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, displayOrder, width = 7, height = 4) {
    #b <- rescale(breakValues, to = c(0,1)) # the values option in scale_fill_gradientn MUST be from 0 to 1
    numLimits = 4
    bb <- generateBreakValues(fillLimits = fillLimits, numLimits = numLimits, decimals = 0)
    b <- rescale(bb, to = c(0,1))
    f <- fillLimits
    cat("fillLimits :", f, "\n")
    cat("breaks :", b, "\n")
    cat("width :", width,  "\n")
    cat("height :", height,  "\n")
    p <- palette
    n <- facetColName
    displayOrder <- gsub("X", "", displayOrder)
    d <- data.table::copy(DTfacetMap)
    d[, (n) := factor(get(n), levels = displayOrder)] 
    keepListCol <- c("id", facetColName, "value")
    if (!isTRUE(all.equal(sort(names(d)), sort(keepListCol)))) {d[, setdiff(names(d), keepListCol) := NULL]} # this format added Oct 30, 2018
    d <- unique(d)
    gg <- ggplot(data = d, aes(map_id = id))
    gg <- gg + geom_map(aes(fill = value), map = mapFile, color=NA, size = 0.5) # "#ffffff" is white
    gg <- gg + expand_limits(x = mapFile$long, y = mapFile$lat)
    gg <- gg + ggthemes::theme_map()
    gg <- gg + facet_wrap(facets = n)
    gg <- gg + theme(legend.position = "bottom")
    #  gg <- gg + geom_point(data = xd, aes(size="xx.sub1", shape = NA), colour = "grey50")#  gg <- gg + guides(colour = guide_legend(title.position = "top"))
    # gg <- gg + guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
    #        size = guide_legend(title.position="top", title.hjust = 0.5))
    gg <- gg +  theme(axis.ticks = element_blank(), 
                      axis.title = element_blank(), 
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(), 
                      strip.text = element_text(family = fontFamily, face = "plain", size = 7))
    
    gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                    na.value = "grey50",
                                    guide = "colorbar") #, values = bb, breaks = f, limits = f, labels = f, aesthetics = "fill")
    
    graphsListHolder[[fileName]] <- gg
    assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
    ggsave(file = paste0(fileloc("gDir"),"/",fileName,".png"), plot = gg,
           width = width, height = height)
    # use scenarios as columns. Not sure this will work for all files. Oct 22, 2018
    formula.wide <- paste0("id ~ ", facetColName)
    temp <- dcast(data = d, formula = formula.wide, value.var = "value")
    inDT <- temp # changed to temp from d Oct 31, 2018
    outName <- paste(fileName, "_data")
    desc <- paste("data for ", fileName )
    cleanup(inDT, outName, fileloc("gDir"), "csv", desc = desc)
    return(gg) # If this is returned and not captured by gg <- xxx then it appears in the plot window of rstudio
  }
  
  
  
  # truncates the column value to the range of fillLimits
  truncateDT <- function(DT, fillLimits){ # function to make sure every country gets a color. The fillLimits values are identified ahead of time and entered manually into the code below
    dt <- copy(DT)
    # truncate range, upper and lower
    dt[value < fillLimits[1], value := fillLimits[1]]
    dt[value > fillLimits[2], value := fillLimits[2]]
    return(dt)
  }
  
  # getGdxChoice function updated Aug 18, 2018 to deal with separate projects
  # this function needs to have the same choices as gdxFileNameChoice
  getGdxChoice <- function() {
    if (!"gdxChoice" %in% ls(envir = .GlobalEnv)) {
      cat("Choose the IMPACT project you are working on.\n")
      cat("1. for the nutrient modeling paper\n")
      cat("2. for the USAID nutrient modeling paper\n")
      cat("3. for the USAID priority setting paper, 2018\n")
      cat("4. for the African Agricultural Futures project")
      
      choice <- readline(prompt = "Choose the number of the gdx file you want to use. \n")
      #  choice <- "1" # so there will be a definite value
      if (choice  %in% "1") {
        gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/SSPs/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
      }
      if (choice  %in% "2") {
        gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/USAID/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
      }
      if (choice  %in% "3") {
        gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/USAIDPrdNhance/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
      }
      
      if (choice %in% "4") {
        gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/AfricanAgFutures/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
      }
      
      gdxChoice <- gdxSwitchCombo[,2]
      #    cat("gdxChoice is", gdxChoice)
      assign("gdxChoice", gdxChoice, envir = .GlobalEnv)
    }
    return(gdxChoice)
  }
  
  # added Oct 8, 2018 so that everytime this function is sourced it checks for a gdxChoice value in the global environment
  gdxChoice <- getGdxChoice()
  
  setGdxChoice <- function() {
    cat("Choose the IMPACT project you are working on.\n")
    cat("1. for the nutrient modeling paper\n")
    cat("2. for the USAID priority setting paper, 2018\n")
    cat("3. for the African Ag Futures project, 2018\n")
    choice <- readline(prompt = "Choose the number of the gdx file you want to use. \n")
    
    if (choice  %in% "1") {
      gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/SSPs/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
    }
    if (choice  %in% "2") {
      gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/USAIDPrdNhance/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
    }
    
    if (choice  %in% "3") {
      gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/AfricanAgFutures/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
    }
    gdxChoice <- gdxSwitchCombo[,2]
    cat("gdxChoice is", gdxChoice)
    assign("gdxChoice", gdxChoice, envir = .GlobalEnv)
    
    #    return(gdxChoice)
  }
  
  getGdxFileName <- function(gdxChoice) {
    gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/", gdxChoice, "/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
    gdxChoice <- gdxSwitchCombo[, 1]
    return(gdxChoice)
  }
  
  getSwitchChoice <- function() {
    gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/", gdxChoice, "/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
    gdxChoice <- gdxSwitchCombo[,3]
    if (gdxChoice == 1) choice = 1
    if (gdxChoice == 2) choice = c(1,2)
    if (gdxChoice == 3) choice = c(1,2,3)
    if (gdxChoice == 4) choice = c(4)
    return(choice)
  }
  
  # # use this function to get a tablegrob. Which can then be placed in a plot. This is done in aggNorder.R
  g_legend <- function(plot.in) {
    tmp <- ggplot2::ggplot_gtable(ggplot_build(plot.in))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  fmt_dcimals <- function(decimals=0){
    function(x) format(x,nsmall = decimals,scientific = FALSE)
  }
  