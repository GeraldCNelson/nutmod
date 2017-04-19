#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name aggRun.R
#' @include nutrientModFunctions.R
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

aggChoice <- "WB" # need to have tenregion and aggReg1 too

#' starting names list
macroList <- c("carbohydrate_g", "protein_g", "fat_g", "totalfiber_g")
minrlsList <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
vitsList <- c("folate_µg", "niacin_mg", "riboflavin_mg",
              "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
              "vit_b12_µg", "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg")
macroList.reqRatio <- paste(prefix, macroList[!macroList %in% "fat_g"], ".reqRatio", sep = "")
macroList.AMDR <- macroList[!macroList %in% "totalfiber_g"]


graphsListHolder <- getNewestVersion("graphsListHolder", fileloc("gDir"))
graphNames <- names(graphsListHolder)

#' Fig 1 budget -----
{
  fig1 <- c("budgetShare", "budgetShareBoxPlot_2050")
}

#' Fig 2 availability -----
#' foodgroup availability ratio part 1
{
  fig2.1 <- c("foodAvail_foodGroup_alcohol", "foodAvail_foodGroup_beverages",
              "foodAvail_foodGroup_cereals", "foodAvail_foodGroup_dairy",
              "foodAvail_foodGroup_eggs", "foodAvail_foodGroup_fish")
}

#' foodgroup availability  part 2
{
  fig2.2 <- c("foodAvail_foodGroup_fruits", "foodAvail_foodGroup_meats",
              "foodAvail_foodGroup_nutsNseeds", "foodAvail_foodGroup_oils",
              "foodAvail_foodGroup_pulses", "foodAvail_foodGroup_rootsNPlaintain")
}
#' foodgroup availability", "part 3
{
  fig2.3 <- c("foodAvail_foodGroup_sweeteners", "foodAvail_foodGroup_vegetables")
}

#' Fig 3 map of staple food groups -----

#' Fig 4 kcal availability -----

{
  fig4 <- c("nutrients.avail_energy_kcal")
}

#' Fig 5 macro nutrients adequacy ratio
{
  fig5 <- c("macro_reqRatio_carbohydrate_g", "macro_reqRatio_protein_g",
            "macro_reqRatio_totalfiber_g")
}

#' Fig 6 macro AMDRs ------
{
  fig6 <- c("AMDRShare_carbohydrate_g", "AMDRShare_protein_g",
            "AMDRShare_fat_g")
}

#' Fig 7 micronutrients adequacy ratio ------
#' micronutrients adequacy", "part 1 and 2 vitamins
{
  fig7.1 <- c("vits_reqRatio_folate_µg", "vits_reqRatio_niacin_mg",
              "vits_reqRatio_riboflavin_mg", "vits_reqRatio_thiamin_mg",
              "vits_reqRatio_vit_a_rae_µg", "vits_reqRatio_vit_b6_mg")
}


{fig7.2 <- c("vits_reqRatio_vit_b12_µg", "vits_reqRatio_vit_c_mg",
             "vits_reqRatio_vit_d_µg", "vits_reqRatio_vit_e_mg",
             "vits_reqRatio_vit_k_µg")}

# vits candidates for removal - niacin, thiamin, vitamin b6

#' micronutrients adequacy", "part 3 minerals
# [nup=2x3]
{fig7.3 <- c("minrls_reqRatio_calcium_mg", "minrls_reqRatio_iron_mg",
             "minrls_reqRatio_magnesium_mg", "minrls_reqRatio_phosphorus_mg",
             "minrls_reqRatio_potassium_g", "minrls_reqRatio_zinc_mg")}

# minrls candidates for removal - magnesium, phosphorus

#' Fig 8 composite DI and nutrient balance ------
fig8 <- c("compDI", "NutBalScore")

#'  Fig 9 diversity metrics ------
fig9 <- c("nonStapleShare", "RAOqe")

#' Figs1 zinc bioavailability  ------
# includes WB on left and tenregion on right
facetMapRR_CCDelta <- "facetmap_nutReqRatioChange_climate"
facetMapRR_IncDelta <- "facetmap_nutReqRatioChange_income"
figs1 <- c("nutrients.avail_zinc_mg")

#' Figs2. Ratio of disqualifying nutrient to its MRV
figs2 <- c("badRatios_ethanol_g", "badRatios_ft_acds_tot_sat_g", "badRatios_sugar_g")

colNum <- 2
colWidths <- c(2.7, 2.7)
layoutMatrix6 <- rbind(c(1, 2), c(3, 4), c(5, 6), c(7))
layoutMatrix5 <- rbind(c(1, 2), c(3, 4), c(5, 6))
layoutMatrix4 <- rbind(c(1, 2), c(3, 4), c(5))
layoutMatrix3 <- rbind(c(1, 2), c(3, 4))
layoutMatrix2 <- rbind(c(1, 2), c(3))
layoutMatrix1 <- rbind(c(1,2))
heights6 <- c(2.5, 2.5, 2.5, 0.5)
heights5 <- c(2.5, 2.5, 2.5)
heights4 <- c(2.5, 2.5, 0.5)
heights3 <- c(2.5, 2.5)
heights2 <- c(2.5, 0.5)
heights1 <- c(3)

legendHorizontal <- 9 # number of the horizontal layout
legendVertical <- 10 # number of the vertical layout

for (aggChoice in c("WB", "tenregions")) {
  #' Figs with 6 plots -----
  for (figchoice in c("fig2.1", "fig2.2", "fig7.1", "fig7.3" )) {
    rowNum <- 4
    plotNameList <- paste(prefix, "_", get(figchoice), "_", aggChoice,  sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    plotNumberList <- c(plotNumberList, legendHorizontal) # add bottom legend onto end
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layoutMatrix6,
      colWidths = colWidths, heights = heights6
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = 8)
    # dev.off
  }

  #' Figs with 5 plots -----
  for (figchoice in c("fig7.2")) {
    rowNum <- 3
    plotNameList <- paste(prefix, "_", get(figchoice), "_", aggChoice,  sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    plotNumberList <- c(plotNumberList, legendVertical) # add bottom legend onto end
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layoutMatrix5,
      colWidths = colWidths, heights = heights5
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = 8)
    # dev.off
  }

  #' Figs with 1 plot
  for (figchoice in c("fig4")) {
    rowNum <- 1
    plotNameList <- paste(prefix, "_", get(figchoice), "_", aggChoice,  sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    plotNumberList <- c(plotNumberList, legendVertical) # add vertical legend
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layoutMatrix1,
      widths = c(5,1), heights = heights1
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = 4)
  }

  #' Figs with 2 plots -----
  for (figchoice in c("fig1", "fig2.3", "fig8", "fig9")) {
    rowNum <- 2
    plotNameList <- paste(prefix, "_", get(figchoice), "_", aggChoice,  sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    plotNumberList <- c(plotNumberList, legendHorizontal) # add bottom legend onto end
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layoutMatrix2,
      widths = colWidths, heights = heights2
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = 3)
    # dev.off
  }

  #' Figs with 3 plots -----
  for (figchoice in c("fig5", "fig6", "figs2")) {
    rowNum <- 2
    plotNameList <- paste(prefix, "_", get(figchoice), "_", aggChoice,  sep = "")
    plotNumberList <- which(graphNames %in% plotNameList)
    plotNumberList <- c(plotNumberList, legendVertical) # add bottom legend onto end
    g.out <- grid.arrange(
      grobs = graphsListHolder[plotNumberList],
      ncol = colNum, nrow = rowNum,
      layout_matrix = layoutMatrix3,
      widths = colWidths, heights = heights3
    )
    ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
           plot = g.out,
           width = 7, height = 6)
  }
}
#' Figs with that combine a single plot from two regions - WB and tenregions -----
for (figchoice in c("figs1")) {
  rowNum <- 2
  plotNameList <- paste(prefix, "_", get(figchoice), "_", "WB",  sep = "")
  plotNameList <- c(plotNameList, paste(prefix, "_", get(figchoice), "_", "tenregions",  sep = ""))

  plotNumberList <- which(graphNames %in% plotNameList)
  plotNumberList <- c(plotNumberList, legendHorizontal) # add bottom legend onto end
  g.out <- grid.arrange(
    grobs = graphsListHolder[plotNumberList],
    ncol = colNum, nrow = rowNum,
    layout_matrix = layoutMatrix2,
    widths = colWidths, heights = heights2
  )
  ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
         plot = g.out,
         width = 7, height = 3)
}

# plot facet maps
for (figchoice in c("facetMapRR_CCDelta", "facetMapRR_IncDelta")) {
  plotNameList <- paste(prefix, "_", get(figchoice),  sep = "")
  plotNumberList <- which(graphNames %in% plotNameList)
  plotNumberList <- c(plotNumberList, legendVertical) # add vertical legend

  # g.out <- grid.arrange(
  #   grobs = graphsListHolder[plotNumberList],
  #   ncol = colNum, nrow = rowNum,
  #   layout_matrix = layoutMatrix1,
  #   widths = c(5,1), heights = heights1
 # )
  ggsave(file = paste(fileloc("gDir"),"/final/", figchoice, "_", aggChoice, ".pdf", sep = ""),
         plot = graphsListHolder[plotNumberList],
         width = 7, height = 4)
}

# how to get rid of x axis labels for the first graph in listHolder
# temp <- graphsListHolder[[1]]
# temp + theme(axis.title.x=element_blank(),
#              +              axis.text.x=element_blank(),
#              +              axis.ticks.x=element_blank())
