#' Nutrient Modeling Functions
#'
#' @keywords utilities, nutrient data management functions
#' title: "Functions needed to make the nutrientModeling shiny app work"
#' @name global.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}

getGdxChoice <- function() {
  gdxCombo <- read.csv(file = paste0(getwd(), "/results/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)
  gdxChoice <- gdxCombo[,2]
  return(gdxChoice)
}

fileloc <- function(variableName) {
  gdxChoice <- getGdxChoice()
  rawData <- "data-raw"
  mData <- paste("data/", gdxChoice, sep = "")
  gDir <- paste("graphics", gdxChoice, sep = "/")
  iData <- paste(mData, "IMPACTData/", sep = "/")
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
  filesofFileType <- list.files(mData)[grep(fileType,list.files(mData))]
  fileLongName <- filesofFileType[grep(fileShortNameTest, filesofFileType, fixed = TRUE)]
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
  # print(paste("fileShortName is ", fileShortName))
  #  print(fileLongName)
  if (length(fileLongName) == 0) {
    stop(sprintf("There is no file  '%s' in directory %s", fileShortName, mData))
  } else {
    #   print(fileLongName)
    outFile = readRDS(paste(mData, fileLongName, sep = "/"))
    return(outFile)
  }
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

#capitalize words
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if (strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

cleanupScenarioNames <- function(dt.ptemp) {
  # replaces - with _ and removes 2 from a couple of USAID scenario
  dt.ptemp[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.ptemp[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.ptemp[, scenario := gsub("HGEM2", "HGEM", scenario)]
  dt.ptemp[, scenario := gsub("IPSL2", "IPSL", scenario)]
  return(dt.ptemp)
}

cleanupNutrientNames <- function(nutList) {
  nutList <- gsub("_g.reqRatio","",nutList)
  nutList <- gsub("reqRatio","",nutList)
  nutList <- gsub("vit_","Vitamin ",nutList)
  nutList <- gsub("_µg","",nutList)
  nutList <- gsub("_mg","",nutList)
  nutList <- gsub("_rae"," rae",nutList)
  nutList <- gsub("_g","",nutList)
  nutList <- gsub("totalfiber","total fiber",nutList)
  nutList <- gsub(".ratio.foodGroup","",nutList)
  # nutList <- gsub("_share","",nutList)
  nutList <- gsub(".sum.all","",nutList)
  nutList <- gsub("rootsNPlantain","Roots\nand plantain",nutList)
  nutList <- gsub("nutsNseeds","Nuts\nand seeds",nutList)
  nutList <- gsub("beverages","Beverages,\nother",nutList)
  nutList <- gsub("alcohol","Beverages,\nalcoholic",nutList)
  nutList <- gsub("NonAlcoholic Beveragesic Beverages","Alcoholic beverages",nutList)
  nutList <- gsub("ft_acds_tot_sat", "Saturated fat", nutList)
  nutList <- gsub("_g_AMDR", "", nutList)
  return(nutList)
}

cleanupNutrientNamesFacetGraph <- function(nutList) {
  nutList <- gsub("_g.reqRatio","",nutList)
  nutList <- gsub("reqRatio","",nutList)
  nutList <- gsub("vit_","Vitamin ",nutList)
  nutList <- gsub("_µg"," (µg)",nutList)
  nutList <- gsub("_mg"," (mg)",nutList)
  nutList <- gsub("_rae"," rae",nutList)
  nutList <- gsub("_g"," (g)",nutList)
  nutList <- gsub("totalfiber","total fiber",nutList)
  nutList <- gsub(".ratio.foodGroup","",nutList)
  # nutList <- gsub("_share","",nutList)
  nutList <- gsub(".sum.all","",nutList)
  nutList <- gsub("rootsNPlantain","Roots\nand plantain",nutList)
  nutList <- gsub("nutsNseeds","Nuts\nand seeds",nutList)
  nutList <- gsub("beverages","Beverages,\nother",nutList)
  nutList <- gsub("alcohol","Beverages,\nalcoholic",nutList)
  nutList <- gsub("alcoholic nonalcoholic beverages","alcoholic beverages",nutList)
  nutList <- gsub("ft_acds_tot_sat", "saturated fat", nutList)
  nutList <- gsub("_g_AMDR", "", nutList)
  return(nutList)
}

#' Title keyVariable - Return a key variable, or a list of all possibilities

keyVariable <- function(variableName) {
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
                                 "crpol", "csoyb", "csbol", "csnfl", "csfol", "cplol", "cpkol", "crpsd",
                                 "ctols", "ctool", "ccoco", "ccafe", "cteas", "cothr", IMPACTfish_code,
                                 IMPACTalcohol_code))

  #These are the scenario numbers for the IIASA data with population disaggregated.
  scenarioListSSP.pop <- c("SSP1_v9_130115", "SSP2_v9_130115", "SSP3_v9_130115",
                           "SSP4_v9_130115", "SSP5_v9_130115")
  scenarioListSSP.GDP <- c("SSP1_v9_130325", "SSP2_v9_130325", "SSP3_v9_130325",
                           "SSP4_v9_130325", "SSP5_v9_130325")

  # scenarioListIMPACT <- as.character(read.csv(file = paste(fileloc("mData"),"scenarioListIMPACT.csv", sep = "/"), stringsAsFactors = FALSE)[,1])
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
      "req.AMDR_hi",
      "req.AMDR_lo",
      "req.MRVs" # added March 24, 2017
      # commmented out because as of Dec 25, 2016, PR requirements for iron and zinc are now in req.RDA.minrls
      # ,
      # "req.PR.iron",
      # "req.PR.zinc"
    )
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
        "region",
        "keepYearList",
        "keepYearList.FBS",
        "FBSyearsToAverage",
        "IMPACTfish_code",
        "IMPACTalcohol_code",
        "IMPACTfoodCommodList",
        "scenarioListSSP.pop",
        "scenarioListSSP.GDP",
        #        "scenarioListIMPACT",
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

countryNameLookup <- function(countryCode, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  #  dt.regions <- getNewestVersion('dt.regions.all', fileloc("mData"))
  if (!countryCode %in% dt.regions.all$region_code.IMPACT159) {
    stop(sprintf("The country code you entered (%s) is not in the lookup table", countryCode))
  } else {
    countryName <- dt.regions.all[region_code.IMPACT159 == countryCode,region_name.IMPACT159]
    return(countryName)
  }
}
countryCodeLookup <- function(countryName, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  #  dt.regions <- getNewestVersion('dt.regions.all', fileloc("mData"))
  if (!countryName %in% countryNames) {
    stop(sprintf("The country name you entered (%s) is not in the lookup table", countryName))
  } else {
    # region159codes <- dt.regions$region_code.IMPACT159
    # rowNum <- dt.regions[region_name.IMPACT159 == countryName, which = TRUE]
    # countryCode <- region159codes[rowNum]
    #' this next step is somewhat kludgy. EG there are four countries represented in CHM
    countryCode <- unique(dt.regions.all[region_name.IMPACT159 == countryName, region_code.IMPACT159])
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
#
# nutReqDataPrep <- function(reqTypeChoice, countryCode, scenarioName, years, dir) {
#   resultFileLookup <- getNewestVersion("resultFileLookup", fileloc("mData"))
#   # SSPname <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[1]
#   # climModel <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[2]
#   experiment <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[3]
#   #if (is.na(experiment)) {experiment <- "REF"}
#   if (is.na(experiment)) {scenarioName <- paste(scenarioName, "-REF", sep = "")}
#
#   fileName <- resultFileLookup[reqTypeName == reqTypeChoice, fileName]
#   if (length(fileName) == "0") print(paste(reqTypeChoice, "is not a valid choice", sep = " "))
#   reqRatios.long <- getNewestVersion(reqTypeChoice, dir)
#   # reqRatios.long <- reqRatios.long[scenario %in% scenarioName, ]
#   reqRatios.long <- reqRatios.long[scenario %in% scenarioName &
#                                      region_code.IMPACT159 %in% countryCode &
#                                      year %in% years, ]
#   formula.ratios <- paste("scenario + region_code.IMPACT159 + year ~ nutrient")
#   reqRatios.wide <- data.table::dcast(
#     reqRatios.long,
#     formula = formula.ratios,
#     value.var = "value")
#
#   # i <- countryCode; j <- SSPname; k <- climModel; m <- experiment; l <- years
#   #
#   # reqRatios.nuts <-
#   #   reqRatios.wide[region_code.IMPACT159 %in% i & SSP %in% j & climate_model %in% k &
#   #                    experiment == m]
#   #get rid of year along with the others
#   deleteListCol <- c("scenario", "region_code.IMPACT159", "year")
#   reqRatios.wide[,(deleteListCol) := NULL]
#   spokeCols <- names(reqRatios.wide)
#   #  nutListShort <- cleanupNutrientNames(spokeCols)
#
#   reqRatios.wide[is.nan(get(spokeCols)),  (spokeCols) := 0]
#   reqRatios.wide <- reqRatios.wide[,(spokeCols) := round(.SD,2), .SDcols = spokeCols]
#
#   # radarchart- If maxmin is TRUE, this must include maximum values as row 1 and minimum values as row 2 for each variables
#   colMins <- as.list(c(rep.int(0,ncol(reqRatios.wide))))
#   reqRatios.wide[, nmax := max(.SD), .SDcols = spokeCols]
#   cMax <- round(reqRatios.wide[1,nmax])
#   colMaxs <- as.list(c(rep.int(cMax,ncol(reqRatios.wide) - 1))) # -1 because nmax is a temporary column
#   reqRatios.wide[,nmax := NULL]
#
#   # include the requirement ratio of 1
#   reqRatioRow <- as.list(c(rep(1,ncol(reqRatios.wide))))
#   reqRatios.wide <- rbind(colMaxs, colMins, reqRatioRow, reqRatios.wide)
#
#   reqRatios.wide[, (spokeCols) := lapply(.SD, as.numeric), .SDcols = spokeCols]
#   reqRatios.wide[is.nan(get(spokeCols)),  (spokeCols) := 0]
#   data.table::setnames(reqRatios.wide, old = names(reqRatios.wide), new = cleanupNutrientNames(names(reqRatios.wide)))
#   return(reqRatios.wide)
# }
#
# nutReqSpiderGraph <- function(reqTypeName, countryCode, scenarioName, years, dir) {
#   reqRatios.nuts <- nutReqDataPrep(reqTypeName,countryCode, scenarioName, years, dir)
#   resultFileLookup <- getNewestVersion("resultFileLookup")
#   # reqRatioList <- resultFileLookup[1:6, reqType] # list of req types that include are based on a requirement
#   # SSP <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[1]
#   # climModel <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[2]
#   # experiment <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[3]
#   # if (is.na(experiment)) {experiment <- "REF"}
#   #
#   # fileName <- resultFileLookup[reqType == reqTypeName, fileName]
#   # reqRatios <- getNewestVersion(fileName, dir)
#   # keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
#   #                  "nutrient", years)
#   # idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient")
#   # formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + year ~ nutrient")
#   #
#   # reqRatios <- reqRatios[, keepListCol, with = FALSE]
#   description <- resultFileLookup[reqTypeName == reqTypeChoice, description]
#
#   chartTitle <- description
#
#   #temp1 <- rbind(colMins, colMaxs, reqRatioRow, reqRatios.wide.nuts)
#
#   legendText <- c("REQ",years)
#   colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
#   colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )
#
#   lineType <- c(3, 1, 1, 1)
#   plot.new()
#   par(mar = c(1, 2, 2, 1))
#   reqNames <- names(reqRatios.nuts)
#   radarchart(reqRatios.nuts, axistype = 2,
#              title = chartTitle,
#              vlabels = reqNames,
#              seg = 3,
#              #custom polygon
#              pcol = colors_border, plwd = 1, plty = lineType,
#              #customgrid colors
#              cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
#              #custom labels
#              vlcex = 0.8,
#              maxmin = TRUE
#   )
#
#   legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
#          col = colors_in, text.col = "black", cex = .9, pt.cex = .9, pt.lwd = 1,
#          y.intersp = .9)
# }

# # test data for nutSharedatasetup
# countryCode <- "AFG"; years <- c("X2010", "X2030", "X2050")
# reqFileName <- "RDA.minrls.FG.ratio"
# # dir <- fileloc("resultsDir"); scenarioName <- "SSP2-NoCC"
# nutshareSpiderGraph <- function(reqFileName, countryCode, scenario, years, dir) {
#   #reqRatiodatasetup <- function(reqTypeName,country, scenarioName, years, dir) {
#   resultFileLookup <- getNewestVersion("resultFileLookup")
#   dt.foodGroupLU <- getNewestVersion("dt.foodGroupsInfo")
#   SSP <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[1]
#   climModel <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[2]
#   experiment <- stringi::stri_split_fixed(scenarioName, "-", simplify = TRUE)[3]
#   if (is.na(experiment)) {experiment <- "REF"}
#
#   #  fileName <- resultFileLookup[reqType == reqTypeName, fileName]
#   shareRatios <- getNewestVersion(reqFileName, dir)
#   # keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
#   keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159",
#                    "nutrient", years)
#   idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient")
#
#   if ("food_group_code" %in% names(shareRatios)) {
#     #    keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
#     keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159",
#                      "food_group_code", "nutrient", years)
#     idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient", "food_group_code")
#     formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + nutrient + year ~ food_group_code")
#   }
#   if ("staple_code" %in% names(shareRatios)) {
#     #   keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "RCP", "region_code.IMPACT159",
#     keepListCol <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159",
#                      "staple_code", "nutrient", years)
#     idVars <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "nutrient", "staple_code")
#     formula.ratios <- paste("scenario + SSP + climate_model + experiment + region_code.IMPACT159 + nutrient + year ~ staple_code")
#   }
#   shareRatios <- shareRatios[, keepListCol, with = FALSE]
#   description <- resultFileLookup[reqTypeName == reqTypeChoice, description]
#
#   #  measureVars <- keyVariable("keepYearList")
#   measureVars <- years
#   shareRatios.long <- data.table::melt(
#     data = shareRatios, id.vars = idVars, measure.vars = measureVars, variable.name = "year",
#     value.name = "value", variable.factor = FALSE)
#
#   shareRatios.wide <- data.table::dcast(
#     shareRatios.long,
#     formula = formula.ratios,
#     value.var = "value")
#
#   i <- countryCode; j <- SSP; k <- climModel; m <- experiment; l <- years
#
#   shareRatios.nuts <-
#     shareRatios.wide[region_code.IMPACT159 %in% i & SSP %in% j & climate_model %in% k &
#                        experiment == m]
#   # shareRatios.nuts[, year := gsub("X","",year)]
#   #get rid of year along with the others
#   deleteListCol <- c("scenario", "SSP", "climate_model", "experiment", "region_code.IMPACT159", "year")
#   shareRatios.nuts[,(deleteListCol) := NULL]
#   spokeCols <- names(shareRatios.nuts)[2:ncol(shareRatios.nuts)]
#   foodGroupShort <- cleanupNutrientNames(spokeCols)
#   # if (("food_group_code" %in% names(shareRatios)) | ("staple_code" %in% names(shareRatios))) {
#   #  # dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo")
#   #   #foodGroupList <- sort(unique(foodGroupsInfo$food_group_code))
#   #   stapleList <- unique(dt.foodGroupLU$staple_code)
#   shareRatios.nuts[, nutrient := gsub("_g.ratio.foodGroup","", nutrient)]
#   shareRatios.nuts[, nutrient := gsub("_µg.ratio.foodGroup","", nutrient)]
#   shareRatios.nuts[, nutrient := gsub("_mg.ratio.foodGroup","", nutrient)]
#   shareRatios.nuts[, nutrient := gsub("_"," ", nutrient)]
#   shareRatios.nuts[, nutrient := gsub("vit ","vitamin ", nutrient)]
#   # }
#   shareRatios.nuts[is.nan(get(spokeCols)),  (spokeCols) := 0, with = FALSE]
#   shareRatios.nuts <- shareRatios.nuts[,(spokeCols) := round(.SD,2), .SDcols = spokeCols]
#
#   #temp1 <- rbind(colMins, colMaxs, reqRatioRow, shareRatios.wide.nuts)
#
#   legendText <- years
#   colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
#   colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )
#
#   lineType <- c(3, 1, 1, 1)
#   plot.new()
#   title(description)
#
#   nutrientList <- sort(unique(shareRatios.nuts$nutrient))
#   par(mar = c(1, 2, 2, 1))
#   nrowsGraphs = round(length(nutrientList)/2)
#   par(mfrow = c(nrowsGraphs,2))
#
#   for (i in nutrientList) {
#     temp <- copy(shareRatios.nuts[nutrient %in% i, ])# radarchart- If maxmin is TRUE, this must include maximum values as row 1 and minimum values as row 2 for each variables
#     temp[, nmax := max(.SD), .SDcols = spokeCols]
#     cMax <- round(temp[1,nmax], digits = 2)
#     colMaxs <- as.list(c(rep.int(cMax,ncol(temp) - 1))) # -1 because nmax is a temporary column
#     temp[,nmax := NULL]
#     colMins <- as.list(c(rep.int(0,ncol(temp))))
#
#     temp <- rbind(colMaxs, colMins, temp)
#     temp[, (spokeCols) := lapply(.SD, as.numeric), .SDcols = spokeCols]
#     temp[is.nan(get(spokeCols)),  (spokeCols) := 0, with = FALSE]
#     temp[, nutrient := NULL]
#
#     vnames <- vector(mode = "character", length = length(foodGroupShort))
#     for (j in 1:length(foodGroupShort)) {
#       vnames[j] <- as.character(dt.foodGroupLU[food_group_codes == foodGroupShort[j], food_groups])
#     }
#
#     radarchart(temp, axistype = 2,
#                title = i,
#                vlabels = vnames,
#                seg = 3,
#                #custom polygon
#                pcol = colors_border, plwd = 1, plty = lineType,
#                #customgrid colors
#                cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
#                #custom labels
#                vlcex = 0.8,
#                maxmin = TRUE
#     )
#
#     legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
#            col = colors_in, text.col = "black", cex = .6, pt.cex = .8, pt.lwd = 1,
#            y.intersp = .8)
#   }
#   return(temp)
# }


# generateWorldMaps -----
# code to generate choropleth world maps. In principle it should be able to handle an arbitrary number of scenarios
generateWorldMaps <- function(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName){
  scenGraphs <- list()
  for (j in 1:length(scenOrder)) {
    #    titletext <- paste0(titleText, scenOrder[j])
    titletext <- NULL
    temp.sp <- spData[scenario %in% scenOrder[j],]
    #    temp.sp[,scenario := NULL]
    temp.sp <- as.data.frame(temp.sp)
    summary(temp.sp)
    plotName.new <- paste0("plot.", gsub("-", "_", scenOrder[j]))
    print(plotName.new)
    gg <- ggplot(temp.sp, aes(map_id = id))
    gg <- gg + geom_map(aes(fill = temp.sp$value), map = worldMap, color = "white")
    gg <- gg + expand_limits(x = worldMap$long, y = worldMap$lat)
    gg <- gg + labs(title =  titletext, hjust = 0.5, x = NULL, y = NULL) +
      theme(plot.title = element_text(size = 10, hjust = 0.5)) +
      scale_fill_gradient(low = lowColor, high = highColor, guide = "legend", name = legendText, limits = fillLimits) +
      labs(lText = legendText) +
      #  theme(legend.position = "bottom") +
      theme(legend.justification = c(0,0), legend.position = c(0,0)) +
      # guides(lText = guide_legend(title.position="top", title.hjust = 0.5))  +
      theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
    scenGraphs[[plotName.new]] <- gg
  }

  # good source of information on using grid to place graphics - https://stat.ethz.ch/R-manual/R-devel/library/grid/doc/grid.pdf

  # code below is modified from multiplot
  cols <- 2
  numPlots <- length(scenGraphs)
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols), byrow = TRUE)
  grid.newpage()
  # +1 is for the title
  rows <- nrow(layout) + 1
  gridHeight <- unit(rep_len(1, rows), "null")
  pushViewport(viewport(layout = grid.layout(rows, ncol(layout), widths = unit(rep_len(1, cols), "null"), heights = unit(c(1, 5,5,5), "null"))))
  # title goes in the first row and across all columns
  grid.text(titleText, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:cols))

  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    # Get the i,j matrix positions of the regions that contain this subplot
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    pdf(paste(fileloc("gDir"), "/worldMaps", fileName, ".pdf", sep = ""), width = 7, height = 5.2, useDingbats = FALSE)

    print(scenGraphs[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                         layout.pos.col = matchidx$col))
    dev.off()
  }
}

# store world map dataframe -----
storeWorldMapDF <- function(){
  # naturalearth world map geojson
  #world <- readOGR(dsn="https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
  #world <- readOGR(dsn = "data-raw/spatialData/ne_50m_admin_0_countries.geojson", layer = "OGRGeoJSON")
  world <- rgdal::readOGR(dsn = "data-raw/spatialData/ne_110m_admin_0_countries.geojson", layer = "OGRGeoJSON")

  # remove antarctica and some other small countries
  world <- world[!world$iso_a3 %in% c("ATA"),]
  othersToRemove <- c("ABW", "AIA", "ALA", "AND", "ASM", "AFT")
  world <- world[!world$iso_a3 %in% othersToRemove,]
  world <- world[!world$type %in% "Dependency",]
  world <- sp::spTransform(world, CRS("+proj=longlat"))

  #world.simp <- gSimplify(world, tol = .1, topologyPreserve = TRUE)
  # alternative would be CRS("+proj=longlat")) for WGS 84
  # dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
  # pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)
  worldMap <- broom::tidy(world, region = "iso_a3")
  inDT <- worldMap
  outName <- "worldMap"
  cleanup(inDT, outName, fileloc("mData"))
}

facetMaps <- function(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder) {
  b <- breakValues
  f <- fillLimits
  p <- palette
  n <- facetColName
  d <- data.table::copy(DT)
  d[, (n) := factor(get(n), levels = displayOrder)]
  gg <- ggplot(data = d, aes(map_id = id))
  gg <- gg + geom_map(aes(fill = value), map = worldMap)
  gg <- gg + expand_limits(x = worldMap$long, y = worldMap$lat)
  gg <- gg + facet_wrap(facets = n)
  gg <- gg + theme(legend.position = "bottom")
  gg <- gg +  theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),
                    axis.text.y = element_blank(), strip.text = element_text(family = "Times", face = "plain"))
  gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                  na.value = "grey50", values = b,
                                  guide = "colorbar", limits = f)
  gg

  graphsListHolder[[fileName]] <- gg
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".pdf"), plot = gg,
         width = 7, height = 6)
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

#' code specifically for shiny app -----
years <- c("X2010", "X2030", "X2050")
yearsClean <- gsub("X", "", years)
fontFamily <- "Times"
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
scenarioNames <- unique(dt.scenarioListIMPACT$scenario)
scenarioNames <- scenarioNames[!scenarioNames %in% c( "SSP2-IPSL-REF", "SSP2-MIROC-REF", "SSP2-GFDL-REF")]
resultFileLookup <- getNewestVersion("resultFileLookup", fileloc("mData"))
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("mData"))
dt.regions.all[, region_name.IMPACT159 := gsub(" plus", "", region_name.IMPACT159)] # used to get to country code
dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo", fileloc("mData"))
countryNamesPlus <- sort(unique(dt.regions.all$region_name.IMPACT159))
countryNames <- gsub(" plus", "", countryNamesPlus)
#' development files
"dt.metadata" <- getNewestVersion("dt.metadata", fileloc("mData"))
"dt.IMPACTgdxParams" <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
FGreqChoices <- c("macro nutrients", "minerals", "vitamins")
staplesReqChoices <- c("energy","macro nutrients", "minerals", "vitamins")
initialCountryName <- "Afghanistan"
initialCountryCode <- countryCodeLookup(initialCountryName, fileloc("mData"))

# rsconnect::setAccountInfo(name = 'nutrientmodeling',
#                           token = '3E8A9773C46C19C6EF42EE20C8C65BF0',
#                           secret = 'hk4UOGDwRKr5Pkw2hKMzSxcRqL0GRsoU67shiwR/')
# rsconnect::deployApp(appDir = paste(getwd(),"nutrientModeling", sep = "/"))

spiderGraphData <- function(countryName, scenarioName, DT, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  DT <- DT[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
  DT[, year := gsub("X", "", year)]
  if (displayColumnName %in% "nutrient") DT[, nutrient := cleanupNutrientNames(nutrient)]
  if (displayColumnName %in% "food_group_code") DT[, food_group_code := cleanupNutrientNames(food_group_code)]
  formula.wide <- sprintf("scenario + region_code.IMPACT159 + year ~ %s", displayColumnName)
  DT <- dcast(data = DT, formula = formula.wide, value.var = "value")
  return(DT)
}

graphData <- function(countryName, scenarioName, DT, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  DT[, year := gsub("X", "", year)]
  DT <- DT[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
  if (displayColumnName %in% "nutrient") DT[, nutrient := cleanupNutrientNames(nutrient)]
  if (displayColumnName %in% "food_group_code") DT[, food_group_code := cleanupNutrientNames(food_group_code)]
  print(unique(DT$food_group_code))
  return(DT)
}

spiderGraphOutput <- function(spiderData, scenarioName) {
  titleText <- paste("Scenario: ", scenarioName)
  p <- ggRadar(data = spiderData, mapping = aes(colour = year),
               rescale = FALSE, interactive = FALSE, size = 2,
               legend.position = "right")
  p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                           face = "plain")) + ggtitle(titleText)
  p <- p + theme(axis.text = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p + theme(legend.text = element_text(size = 12, family = fontFamily, face = "plain"))
  return(p)
}

load_data <- function() {
  #load data that are not year or scenario specific; these are handled in the observe code in the server
  #' development files
  "dt.metadata" <- getNewestVersion("dt.metadata", fileloc("mData"))
  "dt.IMPACTgdxParams" <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))

  loadNresize <- function(dt) {
    #   print(dt)
    temp <- getNewestVersion(dt, fileloc("mData"))
    temp <- (temp[year %in% years])
    temp <- temp[scenario %in% scenarioNames]
    assign(dt, temp, envir = .GlobalEnv) # this puts the data sets into the global environment
    return(dt)
  }
  dataSetsToLoad <- c(
    #' affordability data
    "dt.budgetShare",

    #' availability data
    "dt.foodAvail.foodGroup",
    #    "dt.nutrients.sum.all",

    #' adequacy data
    "dt.nutrients.kcals",
    "food_agg_AMDR_hi",
    "RDA.macro_sum_reqRatio",
    "RDA.vits_sum_reqRatio",
    "RDA.minrls_sum_reqRatio",
    "dt.nutBalScore",
    "dt.MRVRatios",
    #     "dt.nutrients.sum.FG", # move to special
    # "RDA.vits_FG_reqRatio",
    # "RDA.minrls_FG_reqRatio",

    #' diversity data
    "dt.shannonDiversity",
    "dt.KcalShare.nonstaple",
    "dt.RAOqe"
  )
  withProgress(message = 'Loading data', value = 0, {
    # Number of times we'll go through the loop
    n <- length(dataSetsToLoad)

    for (i in 1:n) {
      # load the data
      dt <- loadNresize(dataSetsToLoad[i])
      assign(dataSetsToLoad[i], dt)
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Loading file", i, "of", n))
    }
  })

  hide("loading_page")
  shinyjs::show("mainTabsetPanel")
}

plotByRegionBarAMDR <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, scenOrder, colorList, AMDR_lo, AMDR_hi,
                                graphsListHolder, plotErrorBars) {
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)

  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  # temp <- temp[order(region_code)]
  #  if (aggChoice %in% "WB") regionNameOrder <- c("Low income", "Lower middle income", "Upper middle income", "High income")
  if (aggChoice %in% "WB") regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  scenarioNameOrder <- scenOrder
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  if (gdxChoice %in% "USAID")  temp <- renameUSAIDscenarios(temp)

  # draw bars
  pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""), width = 7, height = 5.2, useDingbats = FALSE)
  if (round(max(temp$value) - yRange[2]) == 0) yRange[2] <- max(temp$value) # will hopefully deal with rare situation
  # when all elements of value are the same as the max y range
  p <- ggplot(temp, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    #    theme(legend.position = "right") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", face = "plain")) +
    theme(axis.title.y = element_text(family = "Times", face = "plain")) +
    # scale_y_continuous(limits = yRange) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times", face = "plain")) +
    ggtitle(plotTitle) +
    #   ylim(yRange) +
    labs(y = yLab, x = NULL) +
    geom_hline(aes(yintercept = AMDR_lo,  color = "green")) +
    geom_text( aes(.75, AMDR_lo + 2, label = "Low", color = "green")) +
    geom_hline(aes(yintercept = AMDR_hi,  color = "dark red")) +
    geom_text( aes(.75, AMDR_hi + 2, label = "High", color = "green"))
  # code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)


  print(p)
  # legend <- g_legend(p)
  dev.off()

  # save data
  formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, gsub("-REF","",scenarios))]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)

  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  #  textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
}

load_data_special <- function(data_name) {
  withProgress(message = 'Loading data', {
    if (!exists(data_name)) {
      temp <- getNewestVersion(data_name, fileloc("mData"))
      temp <- (temp[year %in% years])
      temp <- temp[scenario %in% scenarioNames]
      assign(data_name, temp, envir = .GlobalEnv) # this puts the data sets into the global environment
      return(data_name)
    }
    incProgress(1)})
}

