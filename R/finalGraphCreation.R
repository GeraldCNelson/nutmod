#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords final graphs
#' @title Calculate final graph combinations for the nutrient modeling paper
#' @name finalGraphCreation.R
#' @include nutrientModFunctions.R
#' @include workBookFunctions.R
#' @include nutrientCalcFunctions.R
#' @include aggNorder.R
#' @description This code writes out pdfs of the graphs used in the final nutrient modeling paper.
#' The code grabs the individual graphs created in aggRun.R and places them in pdf file to be incorporated into the final word doc. There are 1 to 6
#' individual graphs per pdf. The layout is guided by the layoutMatrixx structure, where the last x is replaced by a number (1 to 6). The heightsx variables
#' control how tall the graph is in inches. The width is determined by the height variable and the relative width from the original file.

#Copyright (C) 2015-2017 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

source("R/nutrientModFunctions.R")
source("R/aggNorder.R")
library(data.table)
library(grid)
library(Cairo) # may be necessary for Calibri font
library(extrafont) # may be necessary for Calibri font
library(staplr) # to merge pdfs and other manipulation
sourceFile <- "finalGraphCreation.R"

gdxChoice <- getGdxChoice()
# delete all files in gDir/final - Commmented out June 4, 2018 because already done in aggRun.R
# graphicsPath <- paste(fileloc("gDir"), "final", sep = "/")
# graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
# graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
# invisible(unlink(graphicsFileList, recursive = FALSE))

# choose png or pdf output here.
fileouttype.device = cairo_pdf
fileouttype <- "pdf"
suffix = "var"
pdfDimensions <- data.table(fileName = character(0), height = character(0), width = character(0))
#layoutPlots <- function(figchoice, aggChoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths, suffix) {
layoutPlots <- function(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths) {
  cat("\n", figchoice, "figures:", get(figchoice))
  plotNameList <- paste(prefix, "_", get(figchoice), sep = "")
  plotNumberList <- which(graphNames %in% plotNameList)
  cat("\nlayoutPlots plotNumberList:", plotNumberList, "\n")
  plotNumberList <- c(plotNumberList, legendNum)
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = colNum2, nrow = rowNum,
    layout_matrix = layout,
    widths = colWidths, heights = colHeights
  )
  ggsave(file = fileName,
         plot = g.out,
         device = fileouttype.device,
         width = sum(colWidths), height = sum(colHeights) + 1) # +1 added to make bottom of the legend grob clear Nov 4, 2018
  
  embed_fonts(fileName, outfile=fileName)
  
  cat("\n", fileName)
  pdfDimensions <-  as.list(c(fileName, sum(colHeights), sum(colWidths)))
  return(pdfDimensions)
}

# prefix defined here
if (gdxChoice %in% "SSPs") {prefix <- "SSPs_scenOrderSSP"; scenChoiceList <- "scenOrderSSP"}  # need a different one for USAID
if (gdxChoice %in% "USAIDPrdNhance") {prefix <- "USAIDPrdNhance_scenOrderUSAIDPrdNhance"; scenChoiceList <- "scenOrderUSAIDPrdNhance"} # need a different one for USAID

colNum2 <- 2 # how many columns of graphs per page
colWidths1 <- c(7) # how wide the column are
colWidths2 <- c(2.9, 2.9) # how wide the 2 columns are
layoutMatrix6 <- rbind(c(1, 2), c(3, 4), c(5, 6), c(7)) # the order in which the graphs are placed on the page. The 7th graph is the legend.
layoutMatrix5 <- rbind(c(1, 2), c(3, 4), c(5, 6))
layoutMatrix4 <- rbind(c(1, 2), c(3, 4), c(5))
layoutMatrix3 <- rbind(c(1, 2), c(3, 4))
layoutMatrix2 <- rbind(c(1, 2), c(3))
layoutMatrix1 <- rbind(c(1), c(2))
heights6 <- c(2.75, 2.75, 2.75, 0.2)
heights5 <- c(2.9, 2.9, 2.9)
heights4 <- c(2.9, 2.9, 0.2)
heights3 <- c(2.9, 2.9)
heights2 <- c(2.9, 0.2)
heights1 <- c(4, 0.2)

#' starting names list
macroList <- c("carbohydrate_g", "protein_g", "fat_g", "totalfiber_g")
minrlsList <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
vitsList <- c("folate_µg", "niacin_mg", "riboflavin_mg",
              "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
              "vit_b12_µg", "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg")
macroList.reqRatio <- paste(prefix, macroList[!macroList %in% "fat_g"], ".reqRatio", sep = "")
macroList.AMDR <- macroList[!macroList %in% "totalfiber_g"]

# figs definitions are below. Some combine different aggregation choices. All use the different suffixes.

# commented out switchloop Nov 5, 2018
# for (switchloop in getSwitchChoice()) {
#   switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
#   switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#   if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
#   if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
#   if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}

gc(verbose = FALSE) # garbage collection Not sure this makes any difference
# These cooking retention and fixFish switches are not needed here because this code only write out graphs files.
# get the file that holds all the graphs produced for suffix
newFile <- paste("graphsListHolder", suffix, sep = ".")
graphsListHolder <- getNewestVersion(fileShortName = newFile, directory = fileloc("gDir"), fileType = "rds")
finalDir <- paste0(fileloc("gDir"),"/final/")
# get the names of all the graphs in the graphsListerHolder file
graphNames <- names(graphsListHolder)

legendHorizontal <- paste("legend", "bottom", gdxChoice, scenChoiceList, "WB", sep = "_") # using WB instead of aggChoice because I think WB and tenregion legends are the same
legendVertical <- paste("legend", "right", gdxChoice, scenChoiceList,  "WB", sep = "_")
legendHorizontalNum <- which(graphNames %in% legendHorizontal)
legendVerticalNum <- which(graphNames %in% legendVertical)

#' Figs that combine a single plot from two regions - WB and tenregions -----
# includes WB on left and tenregion on right
figs6.nutavail.zinc <- paste(c("nutrients.avail_zinc_mg"), sep = "_") # leave suffix out of this code

for (figchoice in c("figs6.nutavail.zinc")) {
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix2
  colWidths <- colWidths2
  colHeights <- heights2
  plotNameList <- paste(prefix, "_", get(figchoice), "_", "WB", ".", suffix,  sep = "")
  plotNameList <- c(plotNameList, paste(prefix, "_", get(figchoice), "_", "tenregions", ".", suffix, sep = ""))
  #' rev here reverses the order in which the graphs are plotted. removed July 12, 2018
  plotNumberList <- which(graphNames %in% plotNameList)
  fileName <- paste(finalDir, figchoice, ".", suffix, ".", fileouttype, sep = "")
  
  cat("\nfigs6.nutavail.zinc plotNumberList:", plotNumberList, "\n")
  
  plotNumberList <- c(plotNumberList, legendNum)
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = colNum2, nrow = rowNum,
    layout_matrix = layout,
    widths = colWidths, heights = colHeights
  )
  fileName <- paste(finalDir, figchoice, ".", suffix, ".", fileouttype, sep = "")
  fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
  ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights) +1, device = fileouttype.device)
  embed_fonts(fileName, outfile=fileName)
  
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

fig6.facetMapRR_CCDelta <- paste("facetmap_nutReqRatioChange_climate", suffix, sep = ".")
fig5.facetMapRR_IncDelta <- paste("facetmap_nutReqRatioChange_income", suffix, sep = ".")
facetMapRR_2050NoCC <- paste("facetmap_nutReqRatio_2050_NoCC", suffix, sep = ".")
fig4.facetMapRR_2010 <- paste("facetmap_nutReqRatio_2010", suffix, sep = ".")
figs1.facetMapBudgetShare2010_50 <- paste("facetmap_budgetShare_2010_50_SSP2_HGEM.world", sep = ".") 
figs9.1.facetMapMRV_2010 <- paste("facetmap_MRVRatio_2010", suffix, sep = ".")
figs9.2.facetMapMRV_IncDelta <- paste("facetmap_MRVRatioChange_income", suffix, sep = ".")
figs9.3.facetMapMRV_CCDelta <- paste("facetmap_MRVRatioChange_climate", suffix, sep = ".")

# plot facet maps
if (gdxChoice %in% "SSPs") {
  # special handling of files not created in aggrun.R for SSPs
  fileListOld <- c("SSPs_scenOrderSSP_CGEeffects.png", "SSPs_scenOrderSSP_facetmap_macroMetrics_2050.png")
  fileListNew <- c("figs2.CGEmacroMetrics.png", "figs3.facetMapMacroMetrics.png")
  oldDir = paste(getwd(), "graphics", gdxChoice, sep = "/")
  newDir = paste(getwd(), "graphics", gdxChoice, "final", sep = "/")
  
  for (i in 1:length(fileListOld)) {
    file.copy(paste(oldDir, fileListOld[i], sep = "/"), newDir, overwrite = TRUE)
    file.rename(paste(newDir, fileListOld[i], sep = "/"), (paste(newDir, fileListNew[i], sep = "/")))
    cat("dest: ", paste(newDir, fileListNew[i], sep = "/"), "\n")
  }
  # end special handling
  
  for (figchoice in c("fig6.facetMapRR_CCDelta", "fig5.facetMapRR_IncDelta", "fig4.facetMapRR_2010", "figs1.facetMapBudgetShare2010_50", 
                      "figs9.1.facetMapMRV_2010", "figs9.2.facetMapMRV_IncDelta","figs9.3.facetMapMRV_CCDelta")) {
    plotNameList <- paste(prefix, "_",  get(figchoice), sep = "")
    cat(plotNameList)
    plotNumberList <- which(graphNames %in% plotNameList)
    cat("\nSSPs facet plotNumberList:", plotNumberList, "\n")
    fileName <- paste(finalDir, figchoice, ".", suffix, ".", fileouttype, sep = "")
    colWidths <- 7
    colHeights <- 4
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = 1, nrow = 1,
      layout_matrix = layout,
      widths = colWidths, heights = colHeights
    )
    #      ggsave(file = fileName, plot = graphsListHolder[[plotNumberList]], width = colWidths, height = colHeights)
    ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights) +1, device = fileouttype.device, dpi = 300)
    embed_fonts(fileName, outfile=fileName)
    fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
    pdfDimensions <- rbind(pdfDimensions, fileDims)
  }
}

if (gdxChoice %in% "USAIDPrdNhance") {
  for (figchoice in c( "fig5.facetMapRR_IncDelta", "fig4.facetMapRR_2010",
                       "figs9.1.facetMapMRV_2010", "figs9.2.facetMapMRV_IncDelta")) {
    plotNameList <- paste(prefix, "_",  get(figchoice), sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    cat("\nUSAIDPrdNhance facet plotNumberList:", plotNumberList, "\n")
    fileName <- paste(finalDir, figchoice, ".", suffix, ".", fileouttype, sep = "")
    colWidths <- 7
    colHeights <- 6
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = 1, nrow = 1,
      layout_matrix = layout,
      widths = colWidths, heights = colHeights
    )
    ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights) +1, device = fileouttype.device)
    embed_fonts(fileName, outfile=fileName)
    fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
    pdfDimensions <- rbind(pdfDimensions, fileDims)
  }
}

#' Fig 1 budget, combines bar charts and box plots; box plots not for tenregions -----
fig1.budgetShare <- paste(c("budgetShare", "budgetShareBoxPlot_2050"), "_", "WB", ".", suffix, sep = "")

for (figchoice in c("fig1.budgetShare")) {
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix2
  colWidths <- colWidths2
  colHeights <-heights2
  fileName <- paste(finalDir, figchoice, "_", aggChoice = "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

#' Fig S1 availability -----
#' foodgroup availability ratio part 1 - 6 graphs
figs4.foodavail.1 <- paste(c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
                             "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
                             "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish"), "_", "WB", ".", suffix, sep = "")

#' foodgroup availability  part 2 - 6 graphs
figs4.foodavail.2 <- paste(c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
                             "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
                             "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlantain"), "_", "WB", ".", suffix, sep = "")
#' foodgroup availability", part 3 - 2 graphs
figs4.foodavail.3 <- paste(c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables"), "_", "WB", ".", suffix, sep = "")

#' Fig 2 micronutrients adequacy ratio ------
#' micronutrients adequacy", "part 2 and 3 vitamins
fig2.1.adequacy.vits <- paste(c("reqRatio_vits_folate_µg", "reqRatio_vits_niacin_mg",
                                "reqRatio_vits_riboflavin_mg", "reqRatio_vits_thiamin_mg",
                                "reqRatio_vits_vit_a_rae_µg", "reqRatio_vits_vit_b6_mg"),  "_", "WB", ".", suffix, sep = "")

fig2.2.adequacy.vits <- paste(c("reqRatio_vits_vit_b12_µg", "reqRatio_vits_vit_c_mg",
                                "reqRatio_vits_vit_d_µg", "reqRatio_vits_vit_e_mg",
                                "reqRatio_vits_vit_k_µg"), "_", "WB", ".", suffix, sep = "")

# vits candidates for removal - niacin, thiamin, vitamin b6

#' micronutrients adequacy", "part 1 minerals
fig2.3.adequacy.minrls <- paste(c("reqRatio_minrls_calcium_mg", "reqRatio_minrls_iron_mg",
                                  "reqRatio_minrls_magnesium_mg", "reqRatio_minrls_phosphorus_mg",
                                  "reqRatio_minrls_potassium_g", "reqRatio_minrls_zinc_mg"), "_", "WB", ".", suffix, sep = "")

#' Fig S2 kcal availability - 1 graph

figs5.energyavail.WB <- paste(c("kcals.tot.perDay"),  "_", "WB", ".", suffix, sep = "")
figs11.budgetShare <- paste(c("budgetShare"),  "_", "tenregions", ".", suffix, sep = "")
figs13.energyavail.10regions <- paste(c("kcals.tot.perDay"),  "_", "tenregions", ".", suffix, sep = "")

#' Fig 2 macro nutrients adequacy ratio, 3 plots per page
fig2.adequacy.macro <- paste(c("reqRatio_macro_carbohydrate_g", "reqRatio_macro_protein_g",
                               "reqRatio_macro_totalfiber_g"),  "_", "WB", ".", suffix, sep = "")

#'  #' Fig 2 minrl nutrients adequacy ratio, 6 plots per page
#' fig2.adequacy.minrls <- paste(c("minrls_calcium_mg", "minrls_iron_mg", "minrls_magnesium_mg", "minrls_phosphorus_mg",
#'                                 "minrls_potassium_g", "minrls_zinc_mg"),  "_", "WB", ".", suffix, sep = "")
#'
#'   #' Fig 2 vits 1 nutrients adequacy ratio, 6 plots per page
#' fig2.adequacy.vits.1 <- paste(c("vits_folate_µg", "vits_niacin_mg", "vits_riboflavin_mg", "vits_thiamin_mg",
#'                                 "vits_vit_a_rae_µg", "vits_vit_b6_mg" ),  "_", "WB", ".", suffix, sep = "")
#'
#' #' Fig 2 vits 2 nutrients adequacy ratio, 5 plots per page
#' fig2.adequacy.vits. <- paste(c("vits_vit_b6_µg", "vits_vit_c_mg", "vits_vit_d_µg", "vits_vit_e_mg",
#'                                 "vits_vit_k_µg"),  "_", "WB", ".", suffix, sep = "")

#' Fig  macro AMDRs ------, 3 plots per page
fig3.AMDRhiLo <- paste(c("AMDRShare_carbohydrate_g", "AMDRShare_protein_g",
                         "AMDRShare_fat_g"),  "_", "WB", ".", suffix, sep = "")
figs15.AMDRhiLo <- paste(c("AMDRShare_carbohydrate_g", "AMDRShare_protein_g",
                           "AMDRShare_fat_g"),  "_", "tenregions", ".", suffix, sep = "")

# minrls candidates for removal - magnesium, phosphorus

#' Fig 5 composite DI and nutrient balance ------
figs8.compDINB <- paste(c("compDI", "NutBalScore"), "_", "WB", ".", suffix, sep = "")
figs17.compDINB <- paste(c("compDI", "NutBalScore"), "_", "tenregions", ".", suffix, sep = "")

#'  FigS4 x Rao and nonstaple share ------
figs7.RaoNenergyShareNonStaples <- paste(c("nonStapleShare", "RAOqe"), "_", "WB", ".", suffix, sep = "")

#' FigsS6. Ratio of disqualifying nutrient to its MRV
figs10.badRatios <- paste(c("badRatios_ethanol_g", "badRatios_ft_acds_tot_sat_g", "badRatios_sugar_g"), "_", "WB", ".", suffix, sep = "")

#' Fig S8 availability -----
#' foodgroup availability ratio part 1 - 6 graphs
figs12.foodavail.1 <- paste(c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
                              "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
                              "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish"), "_", "tenregions", ".", suffix, sep = "")

#' foodgroup availability  part 2 - 6 graphs
figs12.foodavail.2 <- paste(c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
                              "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
                              "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlantain"), "_", "tenregions", ".", suffix, sep = "")

#' foodgroup availability", part 3 - 2 graphs
figs12.foodavail.3 <- paste(c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables"), "_", "tenregions", ".", suffix, sep = "")

#' Fig S10 tenregions micronutrients adequacy ratio ------
#' micronutrients adequacy", "part 2 and 3 vitamins
figs16.1.adequacy.vits <- paste(c("reqRatio_vits_folate_µg", "reqRatio_vits_niacin_mg",
                                  "reqRatio_vits_riboflavin_mg", "reqRatio_vits_thiamin_mg",
                                  "reqRatio_vits_vit_a_rae_µg", "reqRatio_vits_vit_b6_mg"),  "_", "tenregions", ".", suffix, sep = "")

figs16.2.adequacy.vits <- paste(c("reqRatio_vits_vit_b12_µg", "reqRatio_vits_vit_c_mg",
                                  "reqRatio_vits_vit_d_µg", "reqRatio_vits_vit_e_mg",
                                  "reqRatio_vits_vit_k_µg"), "_", "tenregions", ".", suffix, sep = "")

# vits candidates for removal - niacin, thiamin, vitamin b6

#' micronutrients adequacy", "part 1 minerals
figs16.3.adequacy.minrls <- paste(c("reqRatio_minrls_calcium_mg", "reqRatio_minrls_iron_mg",
                                    "reqRatio_minrls_magnesium_mg", "reqRatio_minrls_phosphorus_mg",
                                    "reqRatio_minrls_potassium_g", "reqRatio_minrls_zinc_mg"), "_", "tenregions", ".", suffix, sep = "")

figs14.adequacy.macro <- paste(c("reqRatio_macro_carbohydrate_g", "reqRatio_macro_protein_g",
                                 "reqRatio_macro_totalfiber_g"), "_", "tenregions", ".", suffix, sep = "")


#' Figs with 6 plots -----
rowNum <- 4
legendNum <- legendHorizontalNum
layout <- layoutMatrix6
colWidths <- colWidths2
colHeights <-heights6

for (figchoice in c( "fig2.1.adequacy.vits", "fig2.3.adequacy.minrls" )) {
  cat("\nfigchoice: ", figchoice, "\n")
  fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c("figs4.foodavail.1", "figs4.foodavail.2")) {
  cat("\nfigchoice: ", figchoice, "\n")
  fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c( "figs12.foodavail.1", "figs12.foodavail.2", "figs16.1.adequacy.vits", "figs16.3.adequacy.minrls")) {
  cat("\nfigchoice: ", figchoice, "\n")
  fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".", fileouttype, sep = "")
  cat("fileName: ", fileName, "\n")
  
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

#' Figs with 5 plots -----
for (figchoice in c("fig2.2.adequacy.vits")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 3
  legendNum <- legendVerticalNum
  layout <- layoutMatrix5
  colWidths <- colWidths2
  colHeights <-heights5
  fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c("figs16.2.adequacy.vits")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 3
  legendNum <- legendVerticalNum
  layout <- layoutMatrix5
  colWidths <- colWidths2
  colHeights <-heights5
  fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

#' Figs with 1 plot and horizontal legend at bottom
for (figchoice in c("figs5.energyavail.WB")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix1
  colWidths <- 5.8
  colHeights <- heights1
  plotNameList <- paste(prefix, "_", get(figchoice), sep = "")
  plotNumberList <- which(graphNames %in% plotNameList)
  plotNumberList <- c(plotNumberList, legendNum)
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = length(colWidths), nrow = rowNum,
    layout_matrix = layout,
    widths = colWidths, heights = colHeights
  )
  fileName <- paste(finalDir, figchoice, "_", aggChoice = "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
  #   ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights))
  ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights) +1, device = fileouttype.device)
  embed_fonts(fileName, outfile=fileName)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c("figs11.budgetShare")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix1
  colWidths <- 5.8
  colHeights <- heights1
  plotNameList <- paste(prefix, "_", get(figchoice), sep = "")
  plotNumberList <- which(graphNames %in% plotNameList)
  plotNumberList <- c(plotNumberList, legendNum)
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = length(colWidths), nrow = rowNum,
    layout_matrix = layout,
    widths = colWidths, heights = colHeights
  )
  fileName <- paste(finalDir, figchoice, "_", aggChoice = "tenregions", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
  ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights), device = fileouttype.device)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c("figs13.energyavail.10regions")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix1
  colWidths <- 5.8
  colHeights <- heights1
  plotNameList <- paste(prefix, "_", get(figchoice), sep = "")
  plotNumberList <- which(graphNames %in% plotNameList)
  plotNumberList <- c(plotNumberList, legendNum)
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = length(colWidths), nrow = rowNum,
    layout_matrix = layout,
    widths = colWidths, heights = colHeights
  )
  fileName <- paste(finalDir, figchoice, "_", aggChoice = "tenregions", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
  #    ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights))
  ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights) +1, device = fileouttype.device)
  embed_fonts(fileName, outfile=fileName)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

#' Figs with 2 plots -----
for (figchoice in c("figs4.foodavail.3",  "figs8.compDINB", "figs7.RaoNenergyShareNonStaples")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix2
  colWidths <- colWidths2
  colHeights <-heights2
  fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c("figs12.foodavail.3", "figs17.compDINB")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix2
  colWidths <- colWidths2
  colHeights <-heights2
  fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

#' Figs with 3 plots -----
for (figchoice in c("fig3.AMDRhiLo", "fig2.adequacy.macro", "figs10.badRatios")) {
  cat("\nfigchoice: ", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendVerticalNum
  layout <- layoutMatrix3
  colWidths <- colWidths2
  colHeights <-heights3
  fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}

for (figchoice in c("figs14.adequacy.macro", "figs15.AMDRhiLo")) {
  cat("\nfigchoice:", figchoice, "\n")
  rowNum <- 2
  legendNum <- legendVerticalNum
  layout <- layoutMatrix3
  colWidths <- colWidths2
  colHeights <-heights3
  fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".", fileouttype, sep = "")
  fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
  pdfDimensions <- rbind(pdfDimensions, fileDims)
}
# } commented out switchloop Nov 15.

dt.finalGraphicsNames <- as.data.table(read_csv(paste0(fileloc("resultsDir"), "/finalGraphicsNames.csv"), col_names = FALSE))

for (i in 1:nrow(dt.finalGraphicsNames)){
  fileFrom <- paste0(fileloc("gDir"), "/final/", dt.finalGraphicsNames$X1[i])
  fileTo <- paste0(fileloc("gDir"), "/final/", dt.finalGraphicsNames$X2[i])
  file.rename(fileFrom, fileTo)
}

# combine selected pdfs

fig2list.in <- c("figure2.0.pdf", "figure2.1.pdf", "figure2.2.pdf", "figure2.3.pdf")
fig2list.out <- "figure2.pdf"
figS4list.in <- c("figureS4.1.pdf", "figureS4.2.pdf", "figureS4.3.pdf")
figS4list.out <- "figureS4.pdf"
figS9list.in <- c("figureS9.1.pdf", "figureS9.2.pdf", "figureS9.3.pdf")
figS9list.out <- "figureS9.pdf"
figS12list.in <- c("figureS12.1.pdf", "figureS12.2.pdf", "figureS12.3.pdf")
figS12list.out <- "figureS12.pdf"
figS16list.in <- c("figureS16.1.pdf", "figureS16.2.pdf", "figureS16.3.pdf")
figS16list.out <- "figureS16.pdf"
input_directory <- paste0(fileloc("gDir"), "/final")
lists <- c("fig2", "figS4", "figS9", "figS12", "figS16")
for (i in 1:length(lists)){
  list.in <- get(paste0(lists[i], "list.in"))
  list.out <- get(paste0(lists[i], "list.out"))
  input_directory <- paste0(fileloc("gDir"), "/final")
  output_filepath <- paste(input_directory, list.out, sep = "/")
  staple_pdf(input_files = paste(input_directory, list.in, sep = "/"),
             output_filepath = output_filepath)
}
  
  
  createScriptMetaData()
  inDT <- pdfDimensions
  outName <- "pdfDimensions"
  desc <- "final graph file name, height and width"
  cleanup(inDT, outName, fileloc("gDir"), desc = desc)
  # how to get rid of x axis labels for the first graph in listHolder
  # temp <- graphsListHolder[[1]]
  # temp + theme(axis.title.x=element_blank(),
  #              +              axis.text.x=element_blank(),
  #              +              axis.ticks.x=element_blank())
  