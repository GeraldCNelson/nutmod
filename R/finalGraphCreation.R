#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords final graphs
#' @title Calculate final graph combinations for the nutrient modeling paper
#' @name aggRun.R
#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
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
sourceFile <- "finalGraphCreation.R"

gdxChoice <- getGdxChoice()
# delete all files in gDir/final - Commmented out June 4, 2018 because already done in aggRun.R
# graphicsPath <- paste(fileloc("gDir"), "final", sep = "/")
# graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
# graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
# invisible(unlink(graphicsFileList, recursive = FALSE))
pdfDimensions <- data.table(fileName = character(0), height = character(0), width = character(0))
#layoutPlots <- function(figchoice, aggChoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths, suffix) {
layoutPlots <- function(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths) {
  cat(get(figchoice))
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
         device = "png",
         width = sum(colWidths), height = sum(colHeights))
  cat("\n", fileName)
  pdfDimensions <-  as.list(c(fileName, sum(colHeights), sum(colWidths)))
  return(pdfDimensions)
}

# prefix defined here
if (gdxChoice %in% "SSPs") prefix <- "SSPs_scenOrderSSP"; scenChoiceList <- "scenOrderSSP"  # need a different one for USAID
if (gdxChoice %in% "USAIDPriorities") prefix <- "USAIDPriorities_scenOrderUSAIDPriorities"; scenChoiceList <- "scenOrderUSAIDPriorities" # need a different one for USAID

colNum2 <- 2 # how many columns of graphs per page
colWidths1 <- c(7) # how wide the column are
colWidths2 <- c(2.9, 2.9) # how wide the 2 columns are
layoutMatrix6 <- rbind(c(1, 2), c(3, 4), c(5, 6), c(7)) # the order in which the graphs are placed on the page. The 7th graph is the legend.
layoutMatrix5 <- rbind(c(1, 2), c(3, 4), c(5, 6))
layoutMatrix4 <- rbind(c(1, 2), c(3, 4), c(5))
layoutMatrix3 <- rbind(c(1, 2), c(3, 4))
layoutMatrix2 <- rbind(c(1, 2), c(3))
layoutMatrix1 <- rbind(c(1,2))
heights6 <- c(2.75, 2.75, 2.75, 0.2)
heights5 <- c(2.9, 2.9, 2.9)
heights4 <- c(2.9, 2.9, 0.2)
heights3 <- c(2.9, 2.9)
heights2 <- c(2.9, 0.2)
heights1 <- c(4)

#' starting names list
macroList <- c("carbohydrate_g", "protein_g", "fat_g", "totalfiber_g")
minrlsList <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
vitsList <- c("folate_µg", "niacin_mg", "riboflavin_mg",
              "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
              "vit_b12_µg", "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg")
macroList.reqRatio <- paste(prefix, macroList[!macroList %in% "fat_g"], ".reqRatio", sep = "")
macroList.AMDR <- macroList[!macroList %in% "totalfiber_g"]

# figs definitions are below. Some combine different aggregation choices. All use the different suffixes.

for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
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

  #' Figs with that combine a single plot from two regions - WB and tenregions -----
  #' FigS3 zinc bioavailability  ------
  # includes WB on left and tenregion on right
  figS3.nutavail.zinc <- paste(c("nutrients.avail_zinc_mg"), sep = "_") # leave suffix out of this code

  for (figchoice in c("figS3.nutavail.zinc")) {
    rowNum <- 2
    legendNum <- legendHorizontalNum
    layout <- layoutMatrix2
    colWidths <- colWidths2
    colHeights <- heights2
    plotNameList <- paste(prefix, "_", get(figchoice), "_", "WB", ".", suffix,  sep = "")
    plotNameList <- c(plotNameList, paste(prefix, "_", get(figchoice), "_", "tenregions", ".", suffix, sep = ""))
    #' rev here reverses the order in which the graphs are plotted
    plotNumberList <- rev(which(graphNames %in% plotNameList))
    fileName <- paste(finalDir, figchoice, ".", suffix, ".png", sep = "")

    cat("\nfigS3.nutavail.zinc plotNumberList:", plotNumberList, "\n")

    plotNumberList <- c(plotNumberList, legendNum)
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum2, nrow = rowNum,
      layout_matrix = layout,
      widths = colWidths, heights = colHeights
    )
    fileName <- paste(finalDir, figchoice, ".", suffix, ".png", sep = "")
    fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
    ggsave(file = fileName, plot = g.out, width = sum(colWidths), height = sum(colHeights))
    pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  fig6.facetMapRR_CCDelta <- paste("facetmap_nutReqRatioChange_climate", suffix, sep = ".")
  fig5.facetMapRR_IncDelta <- paste("facetmap_nutReqRatioChange_income", suffix, sep = ".")
  facetMapRR_2050NoCC <- paste("facetmap_nutReqRatio_2050_noCC", suffix, sep = ".")
  fig4.facetMapRR_2010 <- paste("facetmap_nutReqRatio_2010", suffix, sep = ".")
  facetMapMRV_2010 <- paste("facetmap_MRVRatio_2010", suffix, sep = ".")
  facetMapMRV_IncDelta <- paste("facetmap_MRVRatioChange_income", suffix, sep = ".")
  facetMapMRV_CCDelta <- paste("facetmap_MRVRatioChange_climate", suffix, sep = ".")

  # plot facet maps
  if (gdxChoice %in% "SSPs") {
    for (figchoice in c("fig6.facetMapRR_CCDelta", "fig5.facetMapRR_IncDelta", "facetMapRR_2050NoCC", "fig4.facetMapRR_2010",
                        "facetMapMRV_2010", "facetMapMRV_IncDelta","facetMapMRV_CCDelta")) {
      plotNameList <- paste(prefix, "_",  get(figchoice), sep = "")
      cat(plotNameList)
      plotNumberList <- which(graphNames %in% plotNameList)
      cat("\nSSPs facet plotNumberList:", plotNumberList, "\n")
      fileName <- paste(finalDir, figchoice, ".", suffix, ".png", sep = "")
      colWidths <- 7
      colHeights <- 6
      ggsave(file = fileName, plot = graphsListHolder[[plotNumberList]], width = colWidths, height = colHeights)
      fileDims <- as.list(c(fileName, sum(colHeights), sum(colWidths)))
      pdfDimensions <- rbind(pdfDimensions, fileDims)
    }
  }

  if (gdxChoice %in% "USAIDPriorities") {
    for (figchoice in c( "fig5.facetMapRR_IncDelta", "fig4.facetMapRR_2010",
                         "facetMapMRV_2010", "facetMapMRV_IncDelta")) {
      plotNameList <- paste(prefix, "_",  get(figchoice), sep = "")
      plotNumberList <- which(graphNames %in% plotNameList)
      cat("\nUSAIDPriorities facet plotNumberList:", plotNumberList, "\n")
      fileName <- paste(finalDir, figchoice, ".", suffix, ".png", sep = "")
      colWidths <- 7
      colHeights <- 6
      ggsave(file = fileName, plot = graphsListHolder[[plotNumberList]], width = colWidths, height = colHeights)
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
    fileName <- paste(finalDir, figchoice, "_", aggChoice = "WB", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
    pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  #' Fig S1 availability -----
  #' foodgroup availability ratio part 1 - 6 graphs
  figS1.foodavail.1 <- paste(c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
                               "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
                               "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish"), "_", "WB", ".", suffix, sep = "")

  #' foodgroup availability  part 2 - 6 graphs
  figS1.foodavail.2 <- paste(c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
                               "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
                               "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlantain"), "_", "WB", ".", suffix, sep = "")
  #' foodgroup availability", part 3 - 2 graphs
  figS1.foodavail.3 <- paste(c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables"), "_", "WB", ".", suffix, sep = "")

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

  figS2.energyavail <- paste(c("nutrients.avail_energy_kcal"),  "_", "tenregions", ".", suffix, sep = "")

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

  # minrls candidates for removal - magnesium, phosphorus

  #' Fig 5 composite DI and nutrient balance ------
  fig5.compDINB <- paste(c("compDI", "NutBalScore"), "_", "WB", ".", suffix, sep = "")

  #'  FigS4 x Rao and nonstaple share ------
  figS4.RaoNenergyShareNonStaples <- paste(c("nonStapleShare", "RAOqe"), "_", "WB", ".", suffix, sep = "")

  #' FigsS6. Ratio of disqualifying nutrient to its MRV
  figS6.badRatios <- paste(c("badRatios_ethanol_g", "badRatios_ft_acds_tot_sat_g", "badRatios_sugar_g"), "_", "WB", ".", suffix, sep = "")

  #' Fig S8 availability -----
  #' foodgroup availability ratio part 1 - 6 graphs
  figS8.foodavail.1 <- paste(c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
                               "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
                               "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish"), "_", "tenregions", ".", suffix, sep = "")

  #' foodgroup availability  part 2 - 6 graphs
  figS8.foodavail.2 <- paste(c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
                               "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
                               "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlantain"), "_", "tenregions", ".", suffix, sep = "")

  #' foodgroup availability", part 3 - 2 graphs
  figS8.foodavail.3 <- paste(c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables"), "_", "tenregions", ".", suffix, sep = "")

  #' Fig S10 tenregions micronutrients adequacy ratio ------
  #' micronutrients adequacy", "part 2 and 3 vitamins
  figS10.1.adequacy.vits <- paste(c("reqRatio_vits_folate_µg", "reqRatio_vits_niacin_mg",
                                    "reqRatio_vits_riboflavin_mg", "reqRatio_vits_thiamin_mg",
                                    "reqRatio_vits_vit_a_rae_µg", "reqRatio_vits_vit_b6_mg"),  "_", "tenregions", ".", suffix, sep = "")

  figS10.2.adequacy.vits <- paste(c("reqRatio_vits_vit_b12_µg", "reqRatio_vits_vit_c_mg",
                                    "reqRatio_vits_vit_d_µg", "reqRatio_vits_vit_e_mg",
                                    "reqRatio_vits_vit_k_µg"), "_", "tenregions", ".", suffix, sep = "")

  # vits candidates for removal - niacin, thiamin, vitamin b6

  #' micronutrients adequacy", "part 1 minerals
  figS10.3.adequacy.minrls <- paste(c("reqRatio_minrls_calcium_mg", "reqRatio_minrls_iron_mg",
                                      "reqRatio_minrls_magnesium_mg", "reqRatio_minrls_phosphorus_mg",
                                      "reqRatio_minrls_potassium_g", "reqRatio_minrls_zinc_mg"), "_", "tenregions", ".", suffix, sep = "")

  figS10.4.adequacy.macro <- paste(c("reqRatio_macro_carbohydrate_g", "reqRatio_macro_protein_g",
                                     "reqRatio_macro_fiber_g"), "_", "tenregions", ".", suffix, sep = "")


  #' Figs with 6 plots -----
  rowNum <- 4
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix6;
  colWidths <- colWidths2
  colHeights <-heights6

  for (figchoice in c( "fig2.1.adequacy.vits", "fig2.3.adequacy.minrls" )) {
    fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
    pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  #xxx
  for (figchoice in c("figS1.foodavail.1", "figS1.foodavail.2")) {
    fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  for (figchoice in c( "figS8.foodavail.1", "figS8.foodavail.2", "figS10.1.adequacy.vits", "figS10.3.adequacy.minrls")) {
    fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  #' Figs with 5 plots -----
  rowNum <- 3
  legendNum <- legendVerticalNum
  layout <- layoutMatrix5
  colWidths <- colWidths2
  colHeights <-heights5
  for (figchoice in c("fig2.2.adequacy.vits")) {
    fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  for (figchoice in c("figS10.2.adequacy.vits")) {
    fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }
  #' #' Figs with 1 plot - commented out temporarily March 11, 2108
  #' for (figchoice in c("fig6")) {
  #'   rowNum <- 1
  #'   legendNum <- legendVerticalNum
  #'   layout <- layoutMatrix1
  #'   colWidths <- colWidths1
  #'   colHeights <-heights1
  #'   fileName <- paste(finalDir, figchoice, "_", aggChoice, ".", suffix, ".png", sep = "")
  #'   fileDims <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths = c(4, 1), suffix)
  #' }

  #' Figs with 2 plots -----
  rowNum <- 2
  legendNum <- legendHorizontalNum
  layout <- layoutMatrix2
  colWidths <- colWidths2
  colHeights <-heights2
  for (figchoice in c("figS1.foodavail.3",  "fig5.compDINB", "figS4.RaoNenergyShareNonStaples")) {
    fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

  #' Figs with 3 plots -----
  rowNum <- 2
  legendNum <- legendVerticalNum
  layout <- layoutMatrix3
  colWidths <- colWidths2
  colHeights <-heights3
  for (figchoice in c("fig3.AMDRhiLo", "fig2.adequacy.macro", "figS6.badRatios")) {
    cat("\nfigchoice: ", figchoice, "\n")
    fileName <- paste(finalDir, figchoice, "_", "WB", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }

 for (figchoice in c("figS8.foodavail.3", "figS10.4.adequacy.macro")) {
    cat("\nfigchoice: ", figchoice, "\n")
    fileName <- paste(finalDir, figchoice, "_", "tenregions", ".", suffix, ".png", sep = "")
    fileDims <- layoutPlots(fileName, figchoice, prefix, rowNum, legendNum, layout, colHeights = colHeights, colWidths = colWidths)
        pdfDimensions <- rbind(pdfDimensions, fileDims)
  }
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
