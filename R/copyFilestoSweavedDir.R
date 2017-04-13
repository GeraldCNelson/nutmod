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
gdxChoice <- getGdxChoice()
if (gdxChoice %in% "SSPs") scenChoiceList <- "scenOrderSSP"
if (gdxChoice %in% "USAID") scenChoiceList <- c("prodEnhance", "waterMan", "addEnhance", "comp")
#for testing
sourceDir <- paste(fileloc("gDir"), sep = "")
destDir <- "Rnw"

copyFile <- function(fileName, sourceDir, destDir) {
  fileName <- paste(gdxChoice, l, fileName, sep = "_")
  file.copy(from = paste(sourceDir, fileName, sep = "/"), to = destDir, overwrite = TRUE)
}

macroList <- c("carbohydrate_g", "protein_g", "fat_g", "totalfiber_g")
macroList.reqRatio <- macroList[!macroList %in% "fat_g"]
copyListFromMacro <- paste("macro_reqRatio", macroList.reqRatio, sep = "_")
copyListFromMacro.WB <- paste0(copyListFromMacro, "_WB.pdf")
copyListFromMacro.tenregions <- paste0(copyListFromMacro, "_tenregions.pdf")
copyListFromMacro.AggReg1 <- paste0(copyListFromMacro, "_AggReg1.pdf")

macroList.AMDR <- macroList[!macroList %in% "totalfiber_g"]
AMDRHiList <- paste("AMDR_hi", macroList.AMDR, sep = "_")
AMDRLoList <- paste("AMDR_lo", macroList.AMDR, sep = "_")
AMDRShareList <- paste("AMDRShare", macroList.AMDR, sep = "_")

copyListFromAMDRHi.WB <- paste0(AMDRHiList, "_WB.pdf")
copyListFromAMDRHi.tenregions <- paste0(AMDRHiList, "_tenregions.pdf")
copyListFromAMDRHi.AggReg1 <- paste0(AMDRHiList, "_AggReg1.pdf")
copyListFromAMDRLo.WB <- paste0(AMDRLoList, "_WB.pdf")
copyListFromAMDRLo.tenregions <- paste0(AMDRLoList, "_tenregions.pdf")
copyListFromAMDRLo.AggReg1 <- paste0(AMDRLoList, "_AggReg1.pdf")
copyListFromAMDRShare.WB <- paste0(AMDRShareList, "_WB.pdf")
copyListFromAMDRShare.tenregions <- paste0(AMDRShareList, "_tenregions.pdf")
copyListFromAMDRShare.AggReg1 <- paste0(AMDRShareList, "_AggReg1.pdf")

minrlsList <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
copyListFromMinrls <- paste("minrls_reqRatio", minrlsList, sep = "_")
copyListFromMinrls.WB <- paste0(copyListFromMinrls, "_WB.pdf")
copyListFromMinrls.tenregions <- paste0(copyListFromMinrls, "_tenregions.pdf")
copyListFromMinrls.AggReg1 <- paste0(copyListFromMinrls, "_AggReg1.pdf")

vitsList <- c("folate_µg", "niacin_mg", "riboflavin_mg",
              "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
              "vit_b12_µg", "vit_c_mg", "vit_d_µg",  "vit_e_mg", "vit_k_µg")
copyListFromVits <- paste("vits_reqRatio", vitsList, sep = "_")
copyListFromVits.WB         <- paste0(copyListFromVits, "_WB.pdf")
copyListFromVits.tenregions <- paste0(copyListFromVits, "_tenregions.pdf")
copyListFromVits.AggReg1 <- paste0(copyListFromVits, "_AggReg1.pdf")

foodGroups <- c(c("alcohol", "beverages", "cereals", "dairy",
                  "eggs", "fish", "fruits", "meats", "nutsNseeds", "oils",
                  "pulses", "rootsNPlaintain","sweeteners", "vegetables"))

copyListFromAvailByFG <- paste("foodAvail_foodGroup", foodGroups, sep = "_")
copyListFromAvailByFG.WB <- paste0(copyListFromAvailByFG, "_WB.pdf")
copyListFromAvailByFG.tenregions <- paste0(copyListFromAvailByFG, "_tenregions.pdf")

copyListFromDiversity <- c("nonStapleShare", "RAOqe")
copyListFromDiversity.WB <- paste0(copyListFromDiversity, "_WB.pdf")
copyListFromDiversity.tenregions <- paste0(copyListFromDiversity, "_tenregions.pdf")

#copyListFromNBS  <- c("compQI", "compDI", "NutBalScore")
copyListFromNBS  <- c( "compDI", "NutBalScore")
copyListFromNBS.WB <- paste0(copyListFromNBS, "_WB.pdf")
copyListFromNBS.tenregions <- paste0(copyListFromNBS, "_tenregions.pdf")

otherList <- c("caffeine_mg", "cholesterol_mg")
copyListFromOther <- paste("nutrients.avail", otherList, sep = "_")
copyListFromOther.WB <- paste0(copyListFromOther, "_WB.pdf")
copyListFromOther.tenregions <- paste0(copyListFromOther, "_tenregions.pdf")

copyListBudget <- c("budgetShare_WB.pdf", "budgetShareBoxPlot_2050_WB.pdf",
                    "budgetShare_tenregions.pdf", "budgetShareBoxPlot_2050_tenregions.pdf")

copyListDiversity <- c("nutrients.nonstaples.share_energy_kcal_tenregions.pdf",
                       "nutrients.nonstaples.share_energy_kcal_WB.pdf")

copyListEnergy <- c("nutrients.avail_energy_kcal_WB.pdf", "nutrients.avail_energy_kcal_tenregions.pdf")

copyList <- c(copyListFromMacro.WB, copyListFromMacro.tenregions,
              copyListFromMinrls.WB, copyListFromMinrls.tenregions,
              copyListFromVits.WB, copyListFromVits.tenregions,
              copyListFromAvailByFG.WB, copyListFromAvailByFG.tenregions,
              copyListFromDiversity.WB, copyListFromDiversity.tenregions,
              copyListFromNBS.WB, copyListFromNBS.tenregions,
              copyListFromOther.WB, copyListFromOther.tenregions,
              copyListFromAMDRHi.WB, copyListFromAMDRHi.tenregions,
              copyListFromAMDRLo.WB, copyListFromAMDRLo.tenregions,
              copyListFromAMDRShare.WB, copyListFromAMDRShare.tenregions,
              copyListBudget)
for (l in scenChoiceList) {
  for (i in copyList) {
    fileName <- paste(gdxChoice, l, i, sep = "_")
    print(sprintf("copying file %s from %s directory to %s", fileName, sourceDir, destDir))
    file.copy(from = paste(sourceDir, fileName, sep = "/"), to = destDir, overwrite = TRUE)
    copyFile(i, sourceDir, destDir)
  }
}

#rename files in Snw
copyListNew <- gsub(".","_",copyList, fixed = TRUE) # get rid of all periods
copyListNew <- gsub("_pdf",".pdf",copyListNew, fixed = TRUE) # add one back for the file type

for (l in scenChoiceList) {
  copyList <- paste(gdxChoice, l, copyList, sep = "_")
}
copyList <- paste(destDir, copyList, sep = "/")
copyListNew <- paste(destDir, copyListNew, sep = "/")

for (i in 1:length(copyList)) {
  file.rename(from = copyList[i], to = copyListNew[i])
}


