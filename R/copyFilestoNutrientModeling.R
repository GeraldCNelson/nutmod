# copy files from the dir directory (typically results) to the nutrientModeling directory.

#Copyright (C) 2016 Gerald C. Nelson, except where noted

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
fileShortName <- "RDA.macro.sum.req.ratio"
sourceDir <- fileloc("resultsDir")
destDir <- "nutrientModeling/data"
fileType <- "rds"

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

copyListFromResults <- c("dt.energy_ratios", "dt.budgetShare",
                         "RDA.macro_sum_reqRatio", "RDA.vits_sum_reqRatio", "RDA.minrls_sum_reqRatio",
                         "RDA.macro_staples_ratio","RDA.vits_staples_ratio", "RDA.minrls_staples_ratio",
                         "RDA.macro_FG_reqRatio","RDA.vits_FG_reqRatio", "RDA.minrls_FG_reqRatio",
#                         "UL.vits.sum.req.ratio", "UL.minrls.sum.req.ratio",
#                         "UL.minrls.FG.ratio", "UL.vits.FG.ratio",
                         "dt.nonStapleKcalShare","dt.RAOqe", "dt.compQI", "dt.compDI", "dt.nutBalScore", "dt.metadata",
                         "dt.nutrients.sum.all",  "dt.nutrients.sum.staples")
#, "dt.nutrients.sum.staples")

copyListFromData <- c("dt.regions.all", "dt.foodGroupsInfo", "resultFileLookup", "dt.scenarioListIMPACT")
copyListFromiData <- c("dt.IMPACTgdxParams")
#copyCsvFromData <- c("fileDocumentation.csv")

for (i in copyListFromResults) {
  print(sprintf("copying file %s from results to nutrientModeling/data", i))
  copyFile(i, fileloc("resultsDir"), "nutrientModeling/data", "rds")
}
for (i in copyListFromData) {
  print(sprintf("copying file %s from %s to nutrientModeling/data", i, fileloc("mData")))
  copyFile(i, fileloc("mData"),"nutrientModeling/data", "rds")
}
for (i in copyListFromiData) {
  copyFile(i, fileloc("iData"),"nutrientModeling/data", "rds")
}
# for (i in copyCsvFromData) {
#   copyFile(i, fileloc("mData"),"nutrientModeling/data", "csv")
# }

file.copy("R/nutrientModFunctions.R", "nutrientModeling/global.R", overwrite = TRUE)

# zip up csv files in the results directory

zipFileName <- paste("results/resultsCSVzip", Sys.Date(), "zip", sep = "_" )
regExp <- paste("(?=^", ")(?=.*csv$)", sep = "")
zipList <-     grep(regExp, list.files(fileloc("resultsDir")), value = TRUE,  perl = TRUE)
zipList <- paste("results", zipList, sep = "/")
# zip(zipfile = zipFileName, files = zipList, flags = "-r9X", extras = "",  zip = Sys.getenv("R_ZIPCMD", "zip"))

