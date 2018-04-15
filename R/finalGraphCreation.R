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

#Copyright (C) 2015-2017 Gerald C,Nelson, except where noted

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

#' gdxChoice values are either SSPs or USAID
gdxChoice <- getGdxChoice()
# delete all files in gDir/final
graphicsPath <- paste(fileloc("gDir"), "final", sep = "/")
graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
invisible(unlink(graphicsFileList, recursive = FALSE))

layoutPlots <- function(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix) {
  rowNum <- rowNum
  plotNameList <- paste(prefix, "_", get(figchoice), sep = "")
  plotNumberList <- which(graphNames %in% plotNameList)
  plotNumberList <- c(plotNumberList, legendNum)
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = colNum, nrow = rowNum,
    layout_matrix = layout,
    widths = colWidths, heights = height
  )
  ggsave(file = paste(finalDir, figchoice, "_", aggChoice, "_", suffix, ".pdf", sep = ""),
         plot = g.out,
         width = 7, height = sum(height))
  cat("\n", finalDir, figchoice, "_", aggChoice, "_", suffix, ".pdf", sep = "")
  return(g.out)
}

if (gdxChoice %in% "SSPs") prefix <- "SSPs_scenOrderSSP" # need a different one for USAID

colNum <- 2 # how many columns of graphs per page
colWidths <- c(2.9, 2.9) # how wide the columns are
layoutMatrix6 <- rbind(c(1, 2), c(3, 4), c(5, 6), c(7)) # the order in which the graphs are places on the page. The 7th graph is the legend.
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

for (switchloop in 1:3) {
  # These cooking retention and fixFish switches are not needed here because this code only write out graphs files.
  # switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  # switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  # get the file that holds all the graphs produced for suffix
  graphsListHolder <- getNewestVersion(paste("graphsListHolder", suffix, sep = "."), fileloc("gDir"))
  finalDir <- paste0(fileloc("gDir"),"/final/")
 # get the names of all the graphs in the graphsListerHolder file
   graphNames <- names(graphsListHolder)
  legendHorizontal <- "legend_bottom_SSPs_scenOrderSSP_tenregions"
  legendHorizontalNum <- which(graphNames %in% legendHorizontal)
  legendVertical <- "legend_right_SSPs_scenOrderSSP_tenregions"
  legendVerticalNum <- which(graphNames %in% legendVertical)

  #' Figs with that combine a single plot from two regions - WB and tenregions -----
  #' FigS3 zinc bioavailability  ------
  # includes WB on left and tenregion on right
  figS3.nutavail.zinc <- paste(c("nutrients.avail_zinc_mg"), sep = "_") # leave suffix out of this code

  for (figchoice in c("figS3.nutavail.zinc")) {
    rowNum <- 2
    legendNum <- legendHorizontalNum
    layout <- layoutMatrix2
    height <- heights2
    plotNameList <- paste(prefix, "_", get(figchoice), "_", "WB", "_", suffix,  sep = "")
    plotNameList <- c(plotNameList, paste(prefix, "_", get(figchoice), "_", "tenregions", "_", suffix, sep = ""))
    #' rev here reverses the order in which the graphs are plotted
    plotNumberList <- rev(which(graphNames %in% plotNameList))
    plotNumberList <- c(plotNumberList, legendNum)
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layout,
      widths = colWidths, heights = height
    )
    ggsave(file = paste(finalDir, figchoice, "_", suffix, ".pdf", sep = ""),
           plot = g.out, width = 7, height = height)
  }

  fig8.facetMapRR_CCDelta <- paste("facetmap_nutReqRatioChange_climate", suffix, sep = "_")
  fig7.facetMapRR_IncDelta <- paste("facetmap_nutReqRatioChange_income", suffix, sep = "_")
  facetMapRR_2050NoCC <- paste("facetmap_nutReqRatio_2050_noCC", suffix, sep = "_")
  fig6.facetMapRR_2010 <- paste("facetmap_nutReqRatio_2010", suffix, sep = "_")
  facetMapMRV_2010 <- paste("facetmap_MRVRatio_2010", suffix, sep = "_")
  facetMapMRV_IncDelta <- paste("facetmap_MRVRatioChange_income", suffix, sep = "_")
  facetMapMRV_CCDelta <- paste("facetmap_MRVRatioChange_climate", suffix, sep = "_")

  # plot facet maps
  for (figchoice in c("fig8.facetMapRR_CCDelta", "fig7.facetMapRR_IncDelta", "facetMapRR_2050NoCC", "fig6.facetMapRR_2010",
                      "facetMapMRV_2010", "facetMapMRV_IncDelta","facetMapMRV_CCDelta")) {
    plotNameList <- paste(prefix, "_",  get(figchoice), sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    ggsave(file = paste(finalDir, figchoice, "_", suffix, ".pdf", sep = ""),
           plot = graphsListHolder[[plotNumberList]], width = 7, height = 6)
  }

  for (aggChoice in c("WB", "tenregions")) {
    # Figs definitions that use both WB and tenregions graphs
    # fig nums updated to resubmitted version, Mar 26, 2018

    #' Fig 1 budget, combines bar charts and box plots; box plots not for tenregions -----
    for (aggChoice in c("WB"))
      fig1.budgetShare <- paste(c("budgetShare", "budgetShareBoxPlot_2050"), aggChoice, suffix, sep = "_")

    for (figchoice in c("fig1.budgetShare")) {
      rowNum <- 2
      legendNum <- legendHorizontalNum
      layout <- layoutMatrix2
      height <- heights2
      #      aggChoice.WB <- "WB"
      g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
    }
  }

  #' Fig S3 availability -----
  #' foodgroup availability ratio part 1 - 6 graphs
  figS1.foodavail.1 <- paste(c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
                                 "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
                                 "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish"), aggChoice, suffix, sep = "_")

  #' foodgroup availability  part 2 - 6 graphs
  figS1.foodavail.2 <- paste(c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
                                  "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
                                  "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlantain"), aggChoice, suffix, sep = "_")
  #' foodgroup availability", part 3 - 2 graphs
  figS1.foodavail.3 <- paste(c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables"), aggChoice, suffix, sep = "_")

  #' Fig 6 kcal availability - 1 graph

  figS2.energyavail <- paste(c("nutrients.avail_energy_kcal"), aggChoice, suffix, sep = "_")

  #' Fig 2 macro nutrients adequacy ratio, 6 plots per page
  fig2.adequacy.macro <- paste(c("macro_reqRatio_carbohydrate_g", "macro_reqRatio_protein_g",
                                  "macro_reqRatio_totalfiber_g"), aggChoice, suffix, sep = "_")

  #' Fig  macro AMDRs ------, 3 plots per page
  {fig3.AMDRhiLo <- paste(c("AMDRShare_carbohydrate_g", "AMDRShare_protein_g",
                            "AMDRShare_fat_g"), aggChoice, suffix, sep = "_")
  }

  #' Fig 4 micronutrients adequacy ratio ------
  #' micronutrients adequacy", "part 2 and 3 vitamins
  fig4.1.adequacy.vits <- paste(c("vits_reqRatio_folate_µg", "vits_reqRatio_niacin_mg",
                                   "vits_reqRatio_riboflavin_mg", "vits_reqRatio_thiamin_mg",
                                   "vits_reqRatio_vit_a_rae_µg", "vits_reqRatio_vit_b6_mg"), aggChoice, suffix, sep = "_")

  fig4.2.adequacy.vits <- paste(c("vits_reqRatio_vit_b12_µg", "vits_reqRatio_vit_c_mg",
                                  "vits_reqRatio_vit_d_µg", "vits_reqRatio_vit_e_mg",
                                  "vits_reqRatio_vit_k_µg"), aggChoice, suffix, sep = "_")

  # vits candidates for removal - niacin, thiamin, vitamin b6

  #' micronutrients adequacy", "part 1 minerals
  fig4.3.adequacy.minrls <- paste(c("minrls_reqRatio_calcium_mg", "minrls_reqRatio_iron_mg",
                                    "minrls_reqRatio_magnesium_mg", "minrls_reqRatio_phosphorus_mg",
                                    "minrls_reqRatio_potassium_g", "minrls_reqRatio_zinc_mg"), aggChoice, suffix, sep = "_")

  # minrls candidates for removal - magnesium, phosphorus

  #' Fig S5 composite DI and nutrient balance ------
  figS5.compDINB <- paste(c("compDI", "NutBalScore"), aggChoice, suffix, sep = "_")

  #'  Fig 11 diversity metrics ------
  figS14.RaoNenergyShareNonStaples <- paste(c("nonStapleShare", "RAOqe"), aggChoice, suffix, sep = "_")

  #' FigsS7. Ratio of disqualifying nutrient to its MRV
  figsS7 <- paste(c("badRatios_ethanol_g", "badRatios_ft_acds_tot_sat_g", "badRatios_sugar_g"), aggChoice, suffix, sep = "_")

  #' Figs with 6 plots -----
  for (figchoice in c("figS1.foodavail.1", "figS1.foodavail.2", "fig4.1.adequacy.vits", "fig4.3.adequacy.minrls")) {
    rowNum <- 4; legendNum <- legendHorizontalNum
    layout <- layoutMatrix6; height <- heights6
    g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
  }

  #' Figs with 5 plots -----
  for (figchoice in c("fig4.2.adequacy.vits")) {
    rowNum <- 3
    legendNum <- legendVerticalNum
    layout <- layoutMatrix5
    height <- heights5
    g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
  }
  #' #' Figs with 1 plot - commented out temporarily March 11, 2108
  #' for (figchoice in c("fig6")) {
  #'   rowNum <- 1
  #'   legendNum <- legendVerticalNum
  #'   layout <- layoutMatrix1
  #'   height <- heights1
  #'   g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths = c(4, 1), suffix)
  #' }

  #' Figs with 2 plots -----
  for (figchoice in c("figS1.foodavail.3", "figS5.compDINB", "figS14.RaoNenergyShareNonStaples")) {
    rowNum <- 2
    legendNum <- legendHorizontalNum
    layout <- layoutMatrix2
    height <- heights2
    g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
  }
  #' Figs with 3 plots -----
  for (figchoice in c("figsS7", "fig3.AMDRhiLo", "fig2.adequacy.macro")) {
    rowNum <- 2
    legendNum <- legendVerticalNum
    layout <- layoutMatrix3
    height <- heights3
    cat("\nfigchoice: ", figchoice)
    g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
  }
}

  # how to get rid of x axis labels for the first graph in listHolder
  # temp <- graphsListHolder[[1]]
  # temp + theme(axis.title.x=element_blank(),
  #              +              axis.text.x=element_blank(),
  #              +              axis.ticks.x=element_blank())
