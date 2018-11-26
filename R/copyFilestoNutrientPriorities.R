#' @title "Copy files needed to make the nutrientPriorities shiny app work"
#' @keywords utilities, copy files, nutrient modeling shiny app
#' @name copyFilestoNutrientPriorities.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @description
#' copy files from the dir directory (typically results) to the nutrientPriorities/data directory.
#' files to copy are
#' dt.regions.all
#' dt.scenarioListIMPACT
#' dt.foodAvail.foodGroup
#' dt.nutrients.sum.staples
#' dt.shannonDiversity
#' resultFileLookup
#' dt.nutrients.kcals
#' dt.budgetShare
#' dt.metadata
#' dt.IMPACTgdxParams
#' dt.foodGroupsInfo
#' reqRatio_sum_RDA_macro
#' reqRatio_sum_RDA_vits
#' reqRatio_sum_RDA_minrls
#' gdxinfo.csv
#' dt.nutrients
#' worldMap
#' dt.yldRnfd
#' dt.yldIrr

#Copyright (C) 2016, 2017 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
#   for more details at http://www.gnu.org/licenses/.

#' @include nutrientModFunctions.R
#' @include workBookFunctions.R
#' @include nutrientCalcFunctions.R
source("R/nutrientModFunctions.R")

gdxChoice <- getGdxChoice()

# #for testing
# fileShortName <- "RDA.macro_sum_reqRatio"
# sourceDir <- fileloc("resultsDir")
destDir <- "nutrientPriorities/data"
fileType <- "rds"

#' function that copies files from one directory to another taking into account file name structure in nutrient modeling
copyFile <- function(fileShortName, sourceDir, destDir, fileType) {
  regExp <- paste("(?=^", fileShortName, ")(?=.*",fileType,"$)", sep = "")
  oldVersionList <-
    grep(regExp, list.files(sourceDir), value = TRUE, perl = TRUE)
  oldVersionListnutrientPriorities <-
    grep(regExp, list.files(destDir), value = TRUE, perl = TRUE)

  if (length(oldVersionListnutrientPriorities) > 0) {
    file.remove(paste(destDir, oldVersionListnutrientPriorities, sep = "/"))}
  print(oldVersionList)
  file.copy(from = paste(sourceDir, oldVersionList, sep = "/"), to = destDir, overwrite = TRUE)
}

for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  #  dt.nutrients.adj <- getNewestVersion(paste("dt.nutrients.sum.all", suffix, sep = "."), fileloc("resultsDir"))
  # dt.foodNnuts <- getNewestVersion("dt.foodNnuts", suffix, fileloc("resultsDir"))
  # dt.nutrients.sum.FG <- getNewestVersion("dt.nutrients.sum.FG", suffix, fileloc("resultsDir"))
  # dt.foodAvail.foodGroup <- getNewestVersion("dt.foodAvail.foodGroup", suffix, fileloc("resultsDir"))

  copyListFromSpecificResults <- paste(c( "dt.compDI", "dt.foodAvail_foodGroup", "dt.KcalShare_nonstaple",
                                         "dt.MRVRatios", "dt.nutBalScore", "dt.nutrients_kcals", "dt.nutrients_sum_all", "dt.nutrients_sum_FG", "dt.RAOqe", "dt.shannonDiversity",
                                         "food_agg_AMDR_hi", "reqRatio_sum_RDA_macro", "reqRatio_sum_RDA_minrls",
                                         "reqRatio_sum_RDA_vits", "dt.foodAvail.foodGroup"), suffix, sep = ".")
  copyListFromSpecificResults <- c(copyListFromSpecificResults, "dt.budgetShare") # added because dt.budgetShare is identical for all suffixes
  copyListFromSpecificResultsNoSuffix <- c("dt.metadataTot")
  copyListFromiData <- c("dt.IMPACTgdxParams", "dt.nutrients.var")
  copyListFromuData <- c("dt.regions.all", "worldMap", "africaMap", "americasMap", "asiaMap")
  copyListFrommData <-c("dt.scenarioListIMPACT", "dt.foodGroupsInfo", "resultFileLookup")
  copyListFromgDir <- c("dt.yldRnfd.var", "dt.yldIrr.var")

  #' special copy for the gdxInfo file which is just below results
  invisible(file.copy("results/nutrientPriorities/gdxInfo.csv", "nutrientPriorities/data"))

  #' copy from results/gdxname
  for (i in copyListFromSpecificResults) {
    print(sprintf("copying file %s from %s to %s", i, fileloc("resultsDir"), destDir))
    copyFile(fileShortName = i, sourceDir = fileloc("resultsDir"), destDir, fileType = fileType)
  }

  #' copy from graphics
  for (i in copyListFromgDir) {
    print(sprintf("copying file %s from %s to %s", i, fileloc("gDir"), destDir))
    copyFile(i, fileloc("gDir"), destDir, "rds")
  }

  #' copy from from results without suffix
  for (i in copyListFromSpecificResultsNoSuffix) {
    print(sprintf("copying file %s from %s to %s", i, fileloc("mData"), destDir))
    copyFile(fileShortName = i, sourceDir = fileloc("mData"), destDir, fileType = fileType)
  }
}

# copy data from the IMPACT directory
for (i in copyListFromiData) {
  print(sprintf("copying file %s from %s to %s", i, fileloc("iData"), destDir))
  copyFile(fileShortName = i, sourceDir = fileloc("iData"), destDir, fileType = fileType)
}

# copy data from the data directory
for (i in copyListFrommData) {
  print(sprintf("copying file %s from %s to %s", i, fileloc("mData"), destDir))
  copyFile(fileShortName = i, sourceDir = fileloc("mData"), destDir, fileType = fileType)
}

# copy data from the universal data directory
for (i in copyListFromuData) {
  print(sprintf("copying file %s from %s to nutrientPriorities/data", i, fileloc("uData")))
  copyFile(fileShortName = i, sourceDir = fileloc("uData"), destDir, fileType = fileType)
}

#' next line commented out because global.R diverges from nutrientModFunctions.R
#file.copy("R/nutrientModFunctions.R", "nutrientPriorities/global.R", overwrite = TRUE)

# # zip up csv files in the results directory. commented out March 11, 2018
# zipFileName <- paste("results/resultsCSVzip", suffix, Sys.Date(), "zip", sep = "_" )
# regExp <- paste("(?=^", ")(?=.*csv$)", sep = "")
# zipList <-     grep(regExp, list.files(fileloc("resultsDir")), value = TRUE,  perl = TRUE)
# zipList <- paste("results", zipList, sep = "/")

