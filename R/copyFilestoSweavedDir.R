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

copyListFromMacro <- c("carbohydrate_g_macro_reqRatio_WB.pdf", "protein_g_macro_reqRatio_WB.pdf", "totalfiber_g_macro_reqRatio_WB.pdf")
copyListFromMinrls <- c("calcium_mg_minrls_reqRatio_WB.pdf","folate_µg_vits_reqRatio_WB.pdf","iron_mg_iron_bioavail_reqRatio_WB.pdf",
                        "zinc_mg_zinc_bioavail_reqRatio_WB.pdf", "magnesium_mg_minrls_reqRatio_WB.pdf", "phosphorus_mg_minrls_reqRatio_WB.pdf",
                        "potassium_g_minrls_reqRatio_WB.pdf")
copyListFromVits <- c("niacin_mg_vits_reqRatio_WB.pdf", "riboflavin_mg_vits_reqRatio_WB.pdf",
                      "thiamin_mg_vits_reqRatio_WB.pdf", "vit_a_rae_µg_vits_reqRatio_WB.pdf", "vit_b6_mg_vits_reqRatio_WB.pdf",
                      "vit_b12_µg_vits_reqRatio_WB.pdf", "vit_c_mg_vits_reqRatio_WB.pdf", "vit_d_µg_vits_reqRatio_WB.pdf",
                      "vit_e_mg_vits_reqRatio_WB.pdf", "vit_k_µg_vits_reqRatio_WB.pdf")
copyListFromAvailByFG <- c("alcohol_foodAvail_foodGroup_WB.pdf", "beverages_foodAvail_foodGroup_WB.pdf",
                           "cereals_foodAvail_foodGroup_WB.pdf", "dairy_foodAvail_foodGroup_WB.pdf",
                           "eggs_foodAvail_foodGroup_WB.pdf", "fish_foodAvail_foodGroup_WB.pdf", "fruits_foodAvail_foodGroup_WB.pdf",
                           "meats_foodAvail_foodGroup_WB.pdf", "nutsNseeds_foodAvail_foodGroup_WB.pdf", "oils_foodAvail_foodGroup_WB.pdf",
                           "pulses_foodAvail_foodGroup_WB.pdf", "rootsNPlaintain_foodAvail_foodGroup_WB.pdf",
                           "sweeteners_foodAvail_foodGroup_WB.pdf", "vegetables_foodAvail_foodGroup_WB.pdf")
copyListFromOther <- c("caffeine_mg_nutrients.avail_WB.pdf", "cholesterol_mg_nutrients.avail_WB.pdf")

for (i in copyListFromMacro) {
  print(sprintf("copying file %s from graphics directory to %s", i, destDir))
  copyFile(i, sourceDir, destDir)
}
for (i in copyListFromMinrls) {
  print(sprintf("copying file %s from graphics directory to %s", i, destDir))
  copyFile(i, sourceDir, destDir)
}
for (i in copyListFromVits) {
  print(sprintf("copying file %s from graphics directory to %s", i, destDir))
  copyFile(i, sourceDir, destDir)
}

for (i in copyListFromAvailByFG) {
  print(sprintf("copying file %s from graphics directory to %s", i, destDir))
  copyFile(i, sourceDir, destDir)
}

#rename files in Snw
copyList <- c(copyListFromMacro, copyListFromMinrls, copyListFromVits, copyListFromAvailByFG)
copyListNew <- gsub(".","_",copyList, fixed = TRUE) # get rid of all periods
copyListNew <- gsub("_pdf",".pdf",copyListNew, fixed = TRUE) # add one back for the file type
copyList <- paste0(getwd(),"/graphics/", copyList)
copyListNew <- paste0(getwd(),"/graphics/", copyListNew)

for (i in 1:length(copyList)) {
  file.rename(from = copyList[i],to = copyListNew[i])
}


