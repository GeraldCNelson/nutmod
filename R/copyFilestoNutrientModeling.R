# copy files from the dir directory (typically results) to the nutrientModeling directory.
#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
if (!exists("getNewestVersion", mode = "function")) {
  source("R/nutrientModFunctions.R")
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

copyListFromResults <- c("EAR.sum.req.ratio","dt.energy.ratios", "budgetShare",
                         "RDA.macro.sum.req.ratio", "RDA.vits.sum.req.ratio", "RDA.minrls.sum.req.ratio",
                         "RDA.macro.staples.ratio","RDA.vits.staples.ratio", "RDA.minrls.staples.ratio",
                         "RDA.macro.FG.ratio","RDA.vits.FG.ratio", "RDA.minrls.FG.ratio",
                         "UL.vits.sum.req.ratio", "UL.minrls.sum.req.ratio")

copyListFromData <- c("dt.regions.all", "dt.ptemp")

for (i in copyListFromResults) {
  print(sprintf("copying file %s from results to nutrientModeling/data", i))
  copyFile(i, fileloc("resultsDir"), "nutrientModeling/data", "rds")
}
for (i in copyListFromData) {
  copyFile(i, fileloc("mData"),"nutrientModeling/data", "rds")
}

file.copy("R/nutrientModFunctions.R", "nutrientModeling/global.R", overwrite = TRUE)
