# copy files from the graphics directory  to the Rnw directory.

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
fileName <- "iron_mg_iron_bioavail_reqRatio_WB.pdf"
sourceDir <- fileloc("gDir")
destDir <- "Rnw"

copyFile <- function(fileName, sourceDir, destDir) {
  file.copy(from = paste(sourceDir, fileName, sep = "/"), to = destDir, overwrite = TRUE)
}

copyListFromMacro <- c("carbohydrate_g_macro_reqRatio", "protein_g_macro_reqRatio", "totalfiber_g_macro_reqRatio",
                       "fat_g_AMDR_hi", "fat_g_AMDR_lo", "carbohydrate_g_AMDR_hi", "carbohydrate_g_AMDR_lo",
                       "protein_g_AMDR_hi", "protein_g_AMDR_lo")
copyListFromMacro.WB <- paste0(copyListFromMacro, "_WB.pdf")
copyListFromMacro.tenregions <- paste0(copyListFromMacro, "_tenregions.pdf")

copyListFromMinrls <- c("calcium_mg_minrls_reqRatio","folate_µg_vits_reqRatio",
                        "iron_mg_minrls_reqRatio",
                        "magnesium_mg_minrls_reqRatio", "phosphorus_mg_minrls_reqRatio",
                        "potassium_g_minrls_reqRatio","zinc_mg_minrls_reqRatio")
copyListFromMinrls.WB <- paste0(copyListFromMinrls, "_WB.pdf")
copyListFromMinrls.tenregions <- paste0(copyListFromMinrls, "_tenregions.pdf")

copyListFromVits <- c("niacin_mg_vits_reqRatio", "riboflavin_mg_vits_reqRatio",
                      "thiamin_mg_vits_reqRatio", "vit_a_rae_µg_vits_reqRatio", "vit_b6_mg_vits_reqRatio",
                      "vit_b12_µg_vits_reqRatio", "vit_c_mg_vits_reqRatio", "vit_d_µg_vits_reqRatio",
                      "vit_e_mg_vits_reqRatio", "vit_k_µg_vits_reqRatio")
copyListFromVits.WB         <- paste0(copyListFromVits, "_WB.pdf")
copyListFromVits.tenregions <- paste0(copyListFromVits, "_tenregions.pdf")

copyListFromAvailByFG <- c("alcohol_foodAvail_foodGroup", "beverages_foodAvail_foodGroup",
                           "cereals_foodAvail_foodGroup", "dairy_foodAvail_foodGroup",
                           "eggs_foodAvail_foodGroup", "fish_foodAvail_foodGroup", "fruits_foodAvail_foodGroup",
                           "meats_foodAvail_foodGroup", "nutsNseeds_foodAvail_foodGroup", "oils_foodAvail_foodGroup",
                           "pulses_foodAvail_foodGroup", "rootsNPlaintain_foodAvail_foodGroup",
                           "sweeteners_foodAvail_foodGroup", "vegetables_foodAvail_foodGroup")
copyListFromAvailByFG.WB <- paste0(copyListFromAvailByFG, "_WB.pdf")
copyListFromAvailByFG.tenregions <- paste0(copyListFromAvailByFG, "_tenregions.pdf")

copyListFromDiversity <- c("nonStapleShare", "RAOqe")
copyListFromDiversity.WB <- paste0(copyListFromDiversity, "_WB.pdf")
copyListFromDiversity.tenregions <- paste0(copyListFromDiversity, "_tenregions.pdf")

copyListFromNBS  <- c("compQI", "compDI", "NutBalScore")
copyListFromNBS.WB <- paste0(copyListFromNBS, "_WB.pdf")
copyListFromNBS.tenregions <- paste0(copyListFromNBS, "_tenregions.pdf")

copyListFromOther <- c("caffeine_mg_nutrients.avail", "cholesterol_mg_nutrients.avail")
copyListFromOther.WB <- paste0(copyListFromOther, "_WB.pdf")
copyListFromOther.tenregions <- paste0(copyListFromOther, "_tenregions.pdf")

copyList <- c(copyListFromMacro.WB, copyListFromMacro.tenregions,
              copyListFromMinrls.WB, copyListFromMinrls.tenregions,
              copyListFromVits.WB, copyListFromVits.tenregions,
              copyListFromAvailByFG.WB, copyListFromAvailByFG.tenregions,
              copyListFromDiversity.WB, copyListFromDiversity.tenregions,
              copyListFromNBS.WB, copyListFromNBS.tenregions,
              copyListFromOther.WB, copyListFromOther.tenregions)

for (i in copyList) {
  print(sprintf("copying file %s from graphics directory to %s", i, destDir))
  copyFile(i, sourceDir, destDir)
}
# for (i in copyListFromMinrls) {
#   print(sprintf("copying file %s from graphics directory to %s", i, destDir))
#   copyFile(i, sourceDir, destDir)
# }
# for (i in copyListFromVits) {
#   print(sprintf("copying file %s from graphics directory to %s", i, destDir))
#   copyFile(i, sourceDir, destDir)
# }
#
# for (i in copyListFromAvailByFG) {
#   print(sprintf("copying file %s from graphics directory to %s", i, destDir))
#   copyFile(i, sourceDir, destDir)
# }
# for (i in copyListFromDiversity) {
#   print(sprintf("copying file %s from graphics directory to %s", i, destDir))
#   copyFile(i, sourceDir, destDir)
# }
# for (i in copyListFromNBS) {
#   print(sprintf("copying file %s from graphics directory to %s", i, destDir))
#   copyFile(i, sourceDir, destDir)
# }

#rename files in Snw
copyListNew <- gsub(".","_",copyList, fixed = TRUE) # get rid of all periods
copyListNew <- gsub("_pdf",".pdf",copyListNew, fixed = TRUE) # add one back for the file type
copyList <- paste(destDir, copyList, sep = "/")
copyListNew <- paste(destDir, copyListNew, sep = "/")

for (i in 1:length(copyList)) {
  file.rename(from = copyList[i],to = copyListNew[i])
}


