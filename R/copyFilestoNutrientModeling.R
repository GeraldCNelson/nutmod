#' Copy files to shiny app directories
#' title: "Copy files needed to make the nutrientModeling shiny app work"
#' @keywords utilities, copy files, nutrient modeling shiny app
#' @name copyFilestoNutrientModeling.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @description
#' copy files from the dir directory (typically results) to the nutrientModeling directory.
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
#' RDA.macro_sum_reqRatio
#' RDA.vits_sum_reqRatio
#' RDA.minrls_sum_reqRatio
#' RDA.macro_FG_reqRatio
#' RDA.vits_FG_reqRatio
#' RDA.minrls_FG_reqRatio
#' AMDR_hi_sum_reqRatio
#' gdxinfo.csv

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
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}
#for testing
fileShortName <- "RDA.macro_sum_reqRatio"
sourceDir <- fileloc("resultsDir")
destDir <- paste("nutrientModeling/data", getGdxChoice(), sep = "/")
fileType <- "rds"

#' function that copies files from one directory to another taking into account file name structure in nutrient modeling
copyFile <- function(fileShortName, sourceDir, destDir, fileType) {
  regExp <- paste("(?=^", fileShortName, ")(?=.*",fileType,"$)", sep = "")
  oldVersionList <-
    grep(regExp, list.files(sourceDir), value = TRUE, perl = TRUE)
  oldVersionListNutrientModeling <-
    grep(regExp, list.files(destDir), value = TRUE, perl = TRUE)

  if (length(oldVersionListNutrientModeling) > 0) {
    file.remove(paste(destDir, oldVersionListNutrientModeling, sep = "/"))}
  print(oldVersionList)
  file.copy(from = paste(sourceDir, oldVersionList, sep = "/"), to = destDir, overwrite = TRUE)
}

copyListFromResults <- c( "dt.budgetShare",
                         "RDA.macro_sum_reqRatio", "RDA.vits_sum_reqRatio", "RDA.minrls_sum_reqRatio", "food_agg_AMDR_hi",
                         "dt.nutrients.sum.all", "dt.nutrients.kcals",
                         "dt.KcalShare.nonstaple","dt.RAOqe", "dt.compDI", "dt.nutBalScore", "dt.metadata",
                         "dt.nutrients.sum.all", "dt.foodAvail.foodGroup",
                         "dt.shannonDiversity", "dt.MRVRatios", "dt.nutrients.sum.FG", "dt.nutrients.adj")
copyListFromData <- c("dt.regions.all", "dt.foodGroupsInfo", "resultFileLookup", "dt.scenarioListIMPACT")
copyListFromiData <- c("dt.IMPACTgdxParams")

#' special copy for the gdxInfo file which is just below results
invisible(file.copy("results/gdxInfo.csv", "nutrientModeling/data"))

#' copy from results/gdxname
for (i in copyListFromResults) {
  print(sprintf("copying file %s from results to %s", i, destDir))
  copyFile(i, fileloc("resultsDir"), destDir, "rds")
}
for (i in copyListFromData) {
  print(sprintf("copying file %s from %s to nutrientModeling/data", i, fileloc("mData")))
  copyFile(i, fileloc("mData"), destDir, "rds")
}
for (i in copyListFromiData) {
  copyFile(i, fileloc("iData"), destDir, "rds")
}

#' next line commented out because global.R diverges from nutrientModFunctions.R
#file.copy("R/nutrientModFunctions.R", "nutrientModeling/global.R", overwrite = TRUE)

# zip up csv files in the results directory
zipFileName <- paste("results/resultsCSVzip", Sys.Date(), "zip", sep = "_" )
regExp <- paste("(?=^", ")(?=.*csv$)", sep = "")
zipList <-     grep(regExp, list.files(fileloc("resultsDir")), value = TRUE,  perl = TRUE)
zipList <- paste("results", zipList, sep = "/")

