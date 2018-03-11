#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name aggRun.R
#' @include nutrientModFunctions.R

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

#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
  source("R/aggNorder.R")}
library(data.table)
library(grid)
#' gdxChoice values are either SSPs or USAID
dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
gdxChoice <- dt.metadata[file_description %in% "project source of gdx-based demand data", file_name_location]

if (gdxChoice %in% "SSPs") prefix <- "SSPs_scenOrderSSP" # need a different one for USAID

colNum <- 2
colWidths <- c(2.9, 2.9)
layoutMatrix6 <- rbind(c(1, 2), c(3, 4), c(5, 6), c(7))
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

for (aggChoice in c("WB", "tenregions")) {

for (switchloop in 1:3) {
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  graphsListHolder <- getNewestVersion(paste("graphsListHolder", suffix, sep = "."), fileloc("gDir"))

  graphNames <- names(graphsListHolder)

  #' Fig 4 budget -----
  {
    fig4 <- paste(c("budgetShare", "budgetShareBoxPlot_2050"), aggChoice, suffix, sep = "_")
  }

  #' Fig 5 availability -----
  #' foodgroup availability ratio part 1
  {
    fig5.1 <- paste(c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
                "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
                "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish"), aggChoice, suffix, sep = "_")
  }

  #' foodgroup availability  part 2
  {
    fig5.2 <- paste(c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
                "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
                "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlantain"), aggChoice, suffix, sep = "_")
  }
  #' foodgroup availability", "part 3
  {
    fig5.3 <- paste(c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables"), aggChoice, suffix, sep = "_")
  }

  #' Fig 6 kcal availability -----

  {
    fig6 <- paste(c("nutrients.avail_energy_kcal"), aggChoice, suffix, sep = "_")
  }

  #' Fig 7 macro nutrients adequacy ratio
  {
    fig7 <- paste(c("macro_reqRatio_carbohydrate_g", "macro_reqRatio_protein_g",
              "macro_reqRatio_totalfiber_g"), aggChoice, suffix, sep = "_")
  }

  #' Fig 8 macro AMDRs ------
  {
    fig8 <- paste(c("AMDRShare_carbohydrate_g", "AMDRShare_protein_g",
              "AMDRShare_fat_g"), aggChoice, suffix, sep = "_")
  }

  #' Fig 9 micronutrients adequacy ratio ------
  #' micronutrients adequacy", "part 2 and 3 vitamins
  {
    fig9.2 <- paste(c("vits_reqRatio_folate_µg", "vits_reqRatio_niacin_mg",
                "vits_reqRatio_riboflavin_mg", "vits_reqRatio_thiamin_mg",
                "vits_reqRatio_vit_a_rae_µg", "vits_reqRatio_vit_b6_mg"), aggChoice, suffix, sep = "_")
  }


  fig9.3 <- paste(c("vits_reqRatio_vit_b12_µg", "vits_reqRatio_vit_c_mg",
               "vits_reqRatio_vit_d_µg", "vits_reqRatio_vit_e_mg",
               "vits_reqRatio_vit_k_µg"), aggChoice, suffix, sep = "_")

  # vits candidates for removal - niacin, thiamin, vitamin b6

  #' micronutrients adequacy", "part 1 minerals

  fig9.1 <- paste(c("minrls_reqRatio_calcium_mg", "minrls_reqRatio_iron_mg",
               "minrls_reqRatio_magnesium_mg", "minrls_reqRatio_phosphorus_mg",
               "minrls_reqRatio_potassium_g", "minrls_reqRatio_zinc_mg"), aggChoice, suffix, sep = "_")

  # minrls candidates for removal - magnesium, phosphorus

  #' Fig 10 composite DI and nutrient balance ------
  fig10 <- paste(c("compDI", "NutBalScore"), aggChoice, suffix, sep = "_")

  #'  Fig 11 diversity metrics ------
  fig11 <- paste(c("nonStapleShare", "RAOqe"), aggChoice, suffix, sep = "_")

  #' Figs1 zinc bioavailability  ------
  # includes WB on left and tenregion on right
  figs1 <- paste(c("nutrients.avail_zinc_mg"), aggChoice, suffix, sep = "_")

  #' Figs2. Ratio of disqualifying nutrient to its MRV
  figs2 <- paste(c("badRatios_ethanol_g", "badRatios_ft_acds_tot_sat_g", "badRatios_sugar_g"), aggChoice, suffix, sep = "_")

  legendHorizontal <- "legend_bottom_SSPs_scenOrderSSP_tenregions"
  legendHorizontalNum <- which(graphNames %in% legendHorizontal)
  legendVertical <- "legend_right_SSPs_scenOrderSSP_tenregions"
  legendVerticalNum <- which(graphNames %in% legendVertical)

  layoutPlots <- function(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix) {
    rowNum <- rowNum
#    plotNameList <- paste(prefix, "_", get(figchoice), "_", suffix, aggChoice, sep = "")
    plotNameList <- paste(prefix, "_", get(figchoice), sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    plotNumberList <- c(plotNumberList, legendNum)
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layout,
      widths = colWidths, heights = height
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, "_", suffix, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = sum(height))
    print(paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, "_", suffix, ".pdf", sep = ""))
    return(g.out)
  }

    #' Figs with 6 plots -----
    for (figchoice in c("fig5.1", "fig5.2", "fig9.1", "fig9.2" )) {
      rowNum <- 4; legendNum <- legendHorizontalNum
      layout <- layoutMatrix6; height <- heights6
     g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
    }

    #' Figs with 5 plots -----
    for (figchoice in c("fig9.3")) {
      rowNum <- 3
      legendNum <- legendVerticalNum
      layout <- layoutMatrix5
      height <- heights5
      g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
    }
    #' Figs with 1 plot
    for (figchoice in c("fig6")) {
      rowNum <- 1
      legendNum <- legendVerticalNum
      layout <- layoutMatrix1
      height <- heights1
      g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths = c(4, 1), suffix)
    }

    #' Figs with 2 plots -----
    for (figchoice in c("fig5.3", "fig10", "fig11")) {
      rowNum <- 2
      legendNum <- legendHorizontalNum
      layout <- layoutMatrix2
      height <- heights2
      g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
    }
    #' Figs with 3 plots -----
    for (figchoice in c("fig7", "fig8", "figs2")) {
      rowNum <- 2
      legendNum <- legendVerticalNum
      layout <- layoutMatrix3
      height <- heights3
      g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
    }
  }

  #' Figs with 2 plots but only for "WB" -----
  for (figchoice in c("fig4")) {
    rowNum <- 2
    legendNum <- legendHorizontalNum
    layout <- layoutMatrix2
    height <- heights2
    aggChoice <- "WB"
    g.out <- layoutPlots(figchoice, aggChoice, prefix, rowNum, legendNum, layout, height, colWidths, suffix)
  }

  #' Figs with that combine a single plot from two regions - WB and tenregions -----
  for (figchoice in c("figs1")) {
    rowNum <- 2
    legendNum <- legendHorizontalNum
    layout <- layoutMatrix2
    height <- heights2
    plotNameList <- paste(prefix, "_", get(figchoice), "_", "WB",  sep = "")
    plotNameList <- c(plotNameList, paste(prefix, "_", get(figchoice), "_", "tenregions",  sep = ""))
    #' rev here reverses the order in which the graphs are plotted
    plotNumberList <- rev(which(graphNames %in% plotNameList))
    plotNumberList <- c(plotNumberList, legendNum)
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layout,
      widths = colWidths, heights = height
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, "_", suffix, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = height)
  }

  facetMapRR_CCDelta <- paste("facetmap_nutReqRatioChange_climate", suffix, sep = "_")
  facetMapRR_IncDelta <- paste("facetmap_nutReqRatioChange_income", suffix, sep = "_")
  facetMapRR_2050NoCC <- paste("facetmap_nutReqRatio_2050_noCC", suffix, sep = "_")
  facetMapRR_2010 <- paste("facetmap_nutReqRatio_2010", suffix, sep = "_")
  facetMapMRV_2010 <- paste("facetmap_MRVRatio_2010", suffix, sep = "_")
  facetMapMRV_IncDelta <- paste("facetmap_MRVRatioChange_income", suffix, sep = "_")
  facetMapMRV_CCDelta <- paste("facetmap_MRVRatioChange_climate", suffix, sep = "_")

  # plot facet maps
  for (figchoice in c("facetMapRR_CCDelta", "facetMapRR_IncDelta", "facetMapRR_2050NoCC", "facetMapRR_2010",
                      "facetMapMRV_2010", "facetMapMRV_IncDelta","facetMapMRV_CCDelta")) {
    plotNameList <- paste(prefix, "_",  get(figchoice), sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", suffix, ".pdf", sep = ""),
           plot = graphsListHolder[[plotNumberList]],
           width = 7, height = 6)
  }
}

# how to get rid of x axis labels for the first graph in listHolder
# temp <- graphsListHolder[[1]]
# temp + theme(axis.title.x=element_blank(),
#              +              axis.text.x=element_blank(),
#              +              axis.ticks.x=element_blank())
