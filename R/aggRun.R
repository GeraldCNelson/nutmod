#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Aggregate and graph nutrient modeling data
#' @name aggRun.R
#' @include nutrientModFunctions.R

#' @name aggNorder.R
#' @keywords aggregate data, sort data
#' @description
#' This script generates graphs of nutrient information aggregated to various levels.

#Copyright (C) 2016, 2017 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
source("R/aggNorder.R")

library(data.table)
library(RColorBrewer)
# next 3 libraries needed for world maps
library(sp)
library(broom)
library(rgdal)
sourceFile <- "aggRun.R"
createScriptMetaData()

# gdxChoice values are either SSPs or USAID
gdxChoice <- getGdxChoice()
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA.macro_sum_reqRatio, RDA.minrls_sum_reqRatio, RDA.vits_sum_reqRatio
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

# create list variable to hold ggplot output
graphsListHolder <- list()

# delete all files in gDir
graphicsPath <- fileloc("gDir")
graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
#unlink works like file.remove but with fewer messages
invisible(unlink(graphicsFileList, recursive = FALSE))

# delete all files in gDir/final
graphicsPath <- paste(fileloc("gDir"), "final", sep = "/")
graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
invisible(unlink(graphicsFileList, recursive = FALSE))


# function to choose whether errorbars are displayed
chooseErrorBars <- function(aggChoice) {
  if (aggChoice == "tenregions") plotErrorBars = FALSE
  if (aggChoice == "WB") plotErrorBars = TRUE
  if (aggChoice == "AggReg1") plotErrorBars = TRUE
  return(plotErrorBars)
}
# start of code that works with the switches
for (switchloop in 1:3) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}

  # at the moment temp is here to remind me that the names in multipleNutsFileList need to correspond with those in multipleNutsListShortName
  temp <-  c("dt.nutrients.sum.all", "nutrients.avail",
             "RDA.macro_sum_reqRatio", "macro_reqRatio",
             "RDA.minrls_sum_reqRatio", "minrls_reqRatio",
             "RDA.vits_sum_reqRatio", "vits_reqRatio",
             #    "dt.nutrients.nonstapleShare", "dt.nutrients.nonstapleShare", commented out Mar 9 2018
             "dt.MRVRatios", "badRatios",
             "AMDR_hi_sum_reqRatio", "AMDR_hi",
             "AMDR_lo_sum_reqRatio", "AMDR_lo",
             "dt.foodAvail.foodGroup", "foodAvail.foodGroup")

  multipleNutsFileList <- c(paste("dt.nutrients.sum.all", suffix, sep = "."),
                            paste("RDA.macro_sum_reqRatio", suffix, sep = "."),
                            paste("RDA.minrls_sum_reqRatio", suffix, sep = "."),
                            paste("RDA.vits_sum_reqRatio", suffix, sep = "."),
                            # paste("dt.nutrients.nonstapleShare", suffix, sep = "."), commented out Mar 9 2018
                            paste("dt.MRVRatios", suffix, sep = "."),
                            paste("AMDR_hi_sum_reqRatio", suffix, sep = "."),
                            paste("AMDR_lo_sum_reqRatio", suffix, sep = "."),
                            paste("dt.foodAvail.foodGroup", suffix, sep = "."))
  multipleNutsListShortName <- c("nutrients.avail",
                                 "macro_reqRatio",
                                 "minrls_reqRatio",
                                 "vits_reqRatio",
                                 # "nutrients.nonstaples.share", commented out Mar 9 2018
                                 # "zinc_bioavail_reqRatio",suffix,sep = ".",
                                 # "iron_bioavail_reqRatio",suffix, sep = ".",
                                 "badRatios",
                                 # the next three were commented out. I'm uncommenting out Mar 12, 2018
                                 "AMDR_hi",
                                 "AMDR_lo",
                                 "foodAvail.foodGroup"
  )
  #nutrients grouping
  macroNutrients <- keyVariable("macronutrients")
  vitamins <- keyVariable("vitamins")
  minerals <- keyVariable("minerals")
  kcals <- keyVariable("energy") # the keyVariable includes "kcals.ft_acds_tot_sat_g"; may need to delete
  fattyAcids <- keyVariable("fattyAcids")
  # other <- c("sugar_g", "cholesterol_mg") not used below

  # scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
  # scenChoice for SSPs is scenOrder.SSPs
  if (gdxChoice == "SSPs") {
    scenario.base <- "SSP2-NoCC-REF"
    #  scenOrder <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF",
    #                 "SSP2-IPSL-REF", "SSP2-HGEM-REF")
    scenOrderSSP <- c("2010",  "SSP2-NoCC-REF", "SSP2-HGEM-REF", "SSP1-NoCC-REF","SSP3-NoCC-REF")
    scenChoiceList <- "scenOrderSSP"
    scenChoice.name <- gdxChoice
    aggChoiceListBarChart <- c("tenregions", "WB") # missing AggReg2 and  "2EconGroup
  }

  if (gdxChoice == "USAID") {
    scenario.base <- "SSP2-NoCC-REF"
    # USAID scenario combo choices

    # old and new scenario names
    # DT[scenario == "SSP2-NoCC-REF", scenario := "REF_NoCC"]
    # DT[scenario == "SSP2-HGEM2-REF", scenario := "REF_HGEM"]
    # DT[scenario == "SSP2-IPSL2-REF", scenario := "REF_IPSL"]
    # DT[scenario == "SSP2-HGEM-LoYld2", scenario := "MED"]
    # DT[scenario == "SSP2-HGEM-RegYld2", scenario := "REGION"]
    # DT[scenario == "SSP2-HGEM-HiYld2", scenario := "HIGH"]
    # DT[scenario == "SSP2-HGEM-HiNARS2", scenario := "HIGH_NARS"]
    # DT[scenario == "SSP2-HGEM-HiREFF2", scenario := "HIGH_RE"]
    # DT[scenario == "SSP2-HGEM-IRREXP2", scenario := "IX"]
    # DT[scenario == "SSP2-NoCC-IRREXP2", scenario := "IX_NoCC"]
    # DT[scenario == "SSP2-IPSL-IRREXP2", scenario := "IX_IPSL"]
    # DT[scenario == "SSP2-HGEM-IRREXP-WUE2", scenario := "IX_WUE"]
    # DT[scenario == "SSP2-NoCC-IRREXP-WUE2", scenario := "IX_WUE_NoCC"]
    # DT[scenario == "SSP2-IPSL-IRREXP-WUE2", scenario := "IX_WUE_IPSL"]
    # DT[scenario == "SSP2-HGEM-SWHC2", scenario := "ISW"]
    # DT[scenario == "SSP2-NoCC-SWHC2", scenario := "ISW_NoCC"]
    # DT[scenario == "SSP2-IPSL-SWHC2", scenario := "ISW_IPSL"]
    # DT[scenario == "SSP2-HGEM-PHL-DEV2", scenario := "RPHL"]
    # DT[scenario == "SSP2-HGEM-MMEFF2", scenario := "RMM"]
    # DT[scenario == "SSP2-HGEM-Pangloss2", scenario := "COMP"]
    # DT[scenario == "SSP2-NoCC-Pangloss2", scenario := "COMP_NoCC"]
    # DT[scenario == "SSP2-IPSL-Pangloss2", scenario := "COMP_IPSL"]

    prodEnhance <- c("2010", scenario.base, "SSP2-HGEM-LoYld2", "SSP2-HGEM-HiYld2", "SSP2-HGEM-HiNARS2", "SSP2-HGEM-HiREFF2", "SSP2-HGEM-RegYld2")
    waterMan <- c("2010", scenario.base, "SSP2-HGEM-IRREXP2", "SSP2-HGEM-IRREXP-WUE2", "SSP2-HGEM-SWHC2",
                  "SSP2-IPSL-IRREXP-WUE2", "SSP2-IPSL-IRREXP2", "ISW_NoCC", "ISW_IPSL")
    addEnhance <- c("2010", scenario.base, "SSP2-HGEM-PHL-DEV2", "SSP2-HGEM-MMEFF2")
    comp <- c("2010", scenario.base, "SSP2-HGEM-Pangloss", "SSP2-NoCC-Pangloss2", "SSP2-IPSL-Pangloss2")
    #    aggChoiceListBarChart <- c("AggReg1") commented out Mar 18, 2018

    #USAID scenario list choice
    scenChoiceList <- c("prodEnhance", "waterMan", "addEnhance", "comp")
    scenChoice.name <- gdxChoice
  }

  #for testing
  #aggChoice <- "WB"; DTglobal <- "dt.shannonDiversity"

  # dt.pop.2010.ref <- dt.pop[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]
  dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
  mergedVals <- c("scenario", "region_code", "year")

  for (l in scenChoiceList) {
    #put colorlist here so its easy to see alignment of colors and bars
    scenOrder <- get(scenChoiceList)
    paletteChoice <- "OrRd" #choices are described in the help for RColorBrewer
    if (l %in% "scenOrderSSP") {
      #put here so its easy to see alignment of colors and bars
      #    colorList <- c("black", "red", "green4", "red3", "red4")
      colorsNeeded <- 4
      colorList <- c("#000000", brewer.pal(colorsNeeded, paletteChoice))

      # a kludge to make the climate scenario green (#2ca25f)
      colorList[3] <- "#2CA25F"

    } else {
      colorsNeeded <- length(get(l))
      #    colorList <- c("black", rainbow(10)[1:length(get(l))])
      colorList <- c("#000000", brewer.pal(colorsNeeded, paletteChoice))
    }

    for (aggChoice in aggChoiceListBarChart) {
      plotErrorBars <- chooseErrorBars(aggChoice)
      #   generate the legends -----
      #' needs to be inside l and aggChoice loop -----
      for (legendOrient in c("bottom", "right")) {
        legendname <- paste("legend", legendOrient, gdxChoice, l, aggChoice, sep = "_")
        graphsListHolder[[legendname]] <- updateLegendGrobs(l, aggChoice, legendLoc = legendOrient, mergedVals)
      }
      #    # Shannon Diversity -----
      #    cat("\nWorking on bar chart for Shannon Diversity for", i)
      #    DT <- aggNorder(gdxChoice, DTglobal = "dt.shannonDiversity", aggChoice, scenChoice = get(l), mergedVals)
      #    ylab <- "(percent)"
      #    filename = "ShannonDiversity"
      #    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      #    plotByRegionBar(dt = DT, fileName = filename, plotTitle = "Shannon Diversity", yLab = ylab,
      #                    yRange = yRangeMinMax, aggChoice, suffix, scenOrder, oneLine = FALSE, colorList)
      # #   yRange = c(0, 80), aggChoice, oneLine = FALSE)

      # Budget share -----
      cat("\nWorking on bar chart for budget share for", suffix, "for", aggChoice)
      DTglobal <- getNewestVersion(paste("dt.budgetShare", suffix, sep = "."), fileloc("resultsDir"))
      DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      ylab <- "(percent)"
      fileName <- paste(gdxChoice, l, "budgetShare", aggChoice, suffix, sep = "_")
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      yRangeMinMax <- c(0,60) #custom for budgetShare
      plotErrorBars <- chooseErrorBars(suffix)
      plotByRegionBar(dt = DT, fileName, plotTitle = "Food budget share of per capita income",
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                      graphsListHolder, plotErrorBars)
      #  yLab = "(percent)", yRange = c(0, 50), aggChoice, oneLine = FALSE)

      cat("\nDone with bar chart for budget share for", aggChoice)
      #     # MFAD -----
      #     cat("\nWorking on bar chart for MFAD for", i)
      #     DT <- aggNorder(gdxChoice, DTglobal = "dt.MFAD", aggChoice, scenChoice = get(l), mergedVals)
      #     ylab <- "(percent)"
      #     if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      #     plotByRegionBar(dt = DT, fileName = "MFAD", plotTitle = "Modified Functional Attribute Diversity",
      #                     yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder, oneLine = FALSE, colorList)
      #   #  yLab = "(percent)", yRange = c(0, 100), aggChoice, oneLine = FALSE)
      # print(paste("Done with bar chart for MFAD for", i))

      # RAOqe -----
      cat("\nWorking on bar chart for Rao's QE for", aggChoice)
      DTglobal <- getNewestVersion(paste("dt.RAOqe", suffix, sep = "."), fileloc("resultsDir"))
      DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      ylab <- ""
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      fileName <- paste(gdxChoice, l, "RAOqe", aggChoice, suffix, sep = "_")
      plotByRegionBar(dt = DT, fileName, plotTitle = "Rao's quadratic entropy",
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE,
                      colorList, graphsListHolder, plotErrorBars)
      #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

      # # food availability -----
      # cat("\nWorking on bar chart for food availability for", i))
      # foodAvail.out <- aggNorder(gdxChoice, DTglobal = "dt.foodAvail.foodGroup", aggChoice, scenChoice = get(l), mergedVals)
      # plotByRegionBar(dt = foodAvail.out, fileName = "foodAvail.foodGroup", plotTitle = "Food availability by food group",
      #                 yLab = "(grams)", yRange = c(0, 100), aggChoice)

      # nonstaple share of kcals ------
      cat("\nWorking on bar chart for the nonstaple share of kcals for", suffix, "for", aggChoice)

      #   DTglobal <- getNewestVersion("dt.KcalShare.nonstaple", fileloc("resultsDir"))
      DTglobal <- getNewestVersion(paste("dt.KcalShare.nonstaple", suffix, sep = "."), fileloc("resultsDir"))

      DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l),
                      mergedVals =  c("scenario", "region_code", "year"), plotErrorBars)
      ylab <- "(percent)"
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      fileName = paste(gdxChoice, l, "nonStapleShare", aggChoice, suffix, sep = "_")
      plotByRegionBar(dt = DT, fileName, plotTitle = "Non-staple share of energy",
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE,
                      colorList, graphsListHolder, plotErrorBars)
      #   yLab = "(percent)", yRange = c(0, 100), aggChoice, oneLine = FALSE)

      # nutrition benefit score -----
      cat("\nWorking on bar chart for the NBS for", suffix, "for", aggChoice)
      DTglobal <- getNewestVersion(paste("dt.nutBalScore", suffix, sep = "."), fileloc("resultsDir"))
      DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      ylab <- "" # this creates the ylab variable and leaves it empty. NULL deletes it!
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      fileName <- paste(gdxChoice, l, "NutBalScore", aggChoice, suffix,  sep = "_")
      plotByRegionBar(dt = DT, fileName, plotTitle = "Nutrient balance score",
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                      graphsListHolder, plotErrorBars)
      #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

      #   # composite QI score -----
      #   cat("\nWorking on bar chart for the QI composite for", i))
      #   DT <- aggNorder(gdxChoice, DTglobal = "dt.compQI", aggChoice, scenChoice = get(l), mergedVals)
      #   ylab <- ""
      #   if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      #   plotByRegionBar(dt = DT, fileName = "compQI", plotTitle = "Composite qualifying index",
      #                   yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder, oneLine = FALSE, colorList)
      # #  yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

      # composite DI score ----- xxxx why doesn't DTglobal include aggChoice
      cat("\nWorking on bar chart for disqualifying nutrients for", suffix, "for", aggChoice)
      fileName = paste(gdxChoice, l, "compDI", aggChoice, suffix,  sep = "_")
      DTglobal <- getNewestVersion(paste("dt.compDI", suffix, sep = "."), fileloc("resultsDir")) # don't put aggChoice in here
      DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      ylab <- ""
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      plotByRegionBar(dt = DT, fileName, plotTitle = "Composite disqualifying index",
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l),
                      oneLine = FALSE, colorList, graphsListHolder, plotErrorBars)
      #    yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

      # total kcal bar chart ----
      cat("\nWorking on stacked kcal bar chart for", suffix, "for", aggChoice)

      DTglobal <- getNewestVersion(paste("dt.nutrients.kcals", suffix, sep = "."), fileloc("resultsDir"))
      #   DTglobal <- getNewestVersion("dt.nutrients.kcals", fileloc("resultsDir"))
      DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l),
                      mergedVals =  c("scenario", "region_code", "year", "nutrient"), plotErrorBars)
      DT <- DT[nutrient %in% c("kcalsPerDay.carbohydrate", "kcalsPerDay.fat", "kcalsPerDay.other", "kcalsPerDay.protein"), ]
      #    DT <- DT[nutrient %in% c("kcalsPerDay.tot"), ]
      DT[, nutrient := gsub("kcalsPerDay.", "", nutrient)]
      yLab <- "(Kcals)"
      if (yLab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, round(max(DT$value)) )}
      fileName = paste(gdxChoice, l, "kcals.values", aggChoice, suffix, sep = "_")
      plotByRegionStackedBar(dt = DT, fileName, plotTitle = "Average daily dietary energy by source",
                             yLab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList)

      # food groups -----
      cat("\nWorking on bar chart for dt.foodAvail.foodGroup for", suffix, "for", aggChoice)

      DTglobal <- getNewestVersion(paste("dt.foodAvail.foodGroup", suffix, sep = "."), fileloc("resultsDir"))

      #    temp.in <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))
      #  temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

       foodGroupList <- unique(DTglobal$food_group_code)
      plotErrorBars <- chooseErrorBars(aggChoice)

      for (fg in foodGroupList) {
        units <- "grams"
        DT <- data.table::copy(DTglobal)
        DT <- DT[food_group_code %in% fg,]
        DT[, food_group_code := NULL]
        DT <- aggNorder(gdxChoice, DTglobal = DT, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)

        fg.longName <- cleanupNutrientNames(fg)
        nutTitle <- paste(tolower(fg.longName), sep = "")
        ylab = paste("(",units,")",sep = "")
        if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
        fileName = paste(gdxChoice, l, "foodAvail_foodGroup", fg, aggChoice, suffix, sep = "_")
        plotByRegionBar(dt = DT, fileName, plotTitle = nutTitle,
                        yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                        graphsListHolder, plotErrorBars)
        #      yRange = c(0, 80), aggChoice, oneLine = FALSE)
      }

      cat("\nDone with food groups --------------------\n")

      # multiple nutrients loop -----
      for (k in 1:length(multipleNutsFileList)) {
        cat("\nWorking on multiple nut file", multipleNutsFileList[k], " for", suffix, "for", aggChoice)
        cat("\nk is ", k)
        temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
        #     temp.in <- temp.in[nutrient %in% keepListNuts,]
        plotErrorBars <- chooseErrorBars(aggChoice)

        nutsToKeep <- unique(temp.in$nutrient)[!unique(temp.in$nutrient) %in%
                                                 c("caffeine_mg", "cholesterol_mg", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g",
                                                   "kcals.ft_acds_tot_sat_g", "kcals.protein_g", "kcals.sugar_g", "kcalsPerDay.carbohydrate", "kcalsPerDay.ethanol",
                                                   "kcalsPerDay.fat", "kcalsPerDay.ft_acds_tot_sat",
                                                   "kcalsPerDay.other", "kcalsPerDay.protein", "kcalsPerDay.sugar", "kcalsPerDay.tot")]
        for (nut in nutsToKeep) {
          nutshortName <- cleanupNutrientNames(nut)
          nutlongName <- dt.nutrientNames_Units[1, (nut)]
          units <- dt.nutrientNames_Units[2, (nut)]
          if (units %in% "g") units <- "grams"
          DT <- temp.in[nutrient %in% nut,]
          DT[, nutrient := NULL]
          DT <- aggNorder(gdxChoice, DTglobal = DT, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
          DTmax <- max(DT$value)
          # cat("\nDTmax:", DTmax)

          #' create file specific name and related info
          if (multipleNutsFileList[k] %in% paste("dt.nutrients.sum.all", suffix, sep = "."))  {
            #         nutTitle <- paste("Average daily availability of ", tolower(nutlongName), sep = "")
            nutTitle <- paste(tolower(nutlongName), sep = "")
            ylab = paste("(",units,")",sep = "")
            drawOneLine = FALSE
            yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )
          }
          if (multipleNutsFileList[k] %in% c(paste("RDA.macro_sum_reqRatio", suffix, sep = "."),
                                             paste("RDA.minrls_sum_reqRatio", suffix, sep = "."),
                                             paste("RDA.vits_sum_reqRatio", suffix, sep = ".")))  {
            nutTitle <- nutshortName
            ylab = "(Adequacy ratio)"
            drawOneLine = 1
            yRangeMinMax <- c(0,1.5)
          }
          if (multipleNutsFileList[k] %in% paste("dt.MRVRatios", suffix, sep = "."))  {
            #        nutTitle <- paste("Ratio of ", nutshortName, " availability to MRV", sep = "")
            nutTitle <- nutshortName
            #          ylab = "(Maximal reference value)"
            ylab = ""
            yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )

            drawOneLine = FALSE
          }
          if (multipleNutsFileList[k] %in% paste("AMDR_hi_sum_reqRatio", suffix, sep = "."))  {
            nutTitle <- paste("AMDR high, ", nutshortName, sep = "")
            ylab = "(AMDR, high)"
            drawOneLine = 1
            yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )

          }
          if (multipleNutsFileList[k] %in% paste("AMDR_lo_sum_reqRatio", suffix, sep = "."))  {
            nutTitle <- paste("AMDR low, ", nutshortName, sep = "")
            ylab = "(AMDR, low)"
            drawOneLine = 1
            yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )
          }
          #commented out March 9, 2018
          # if (multipleNutsFileList[k] == paste("dt.nutrients.nonstapleShare", suffix, sep = "."))  {
          #   #        nutTitle <- paste("Non-staple share of ", nutshortName, " availability", sep = "")
          #   nutTitle <- paste(nutshortName, sep = "")
          #   ylab = "(percent)"
          #   drawOneLine = FALSE
          # }
 #          if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,103)} else {yRangeMinMax <- c(0, 1.5) }

          fileName = paste(gdxChoice, l, multipleNutsListShortName[k], nut, aggChoice, suffix, sep = "_")
          cat("\nfileName is ", fileName,"\n")
          #         print(summary(DT))
          plotByRegionBar(dt = DT, fileName,
                          plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix,
                          scenOrder = get(l), oneLine = drawOneLine, colorList, graphsListHolder, plotErrorBars)
        }
      }

      cat("\nDone with ", multipleNutsFileList[k], " ----------------")
    }
  }
  cat("\nDone with scenChoiceList loop ----------")

  #' box plot of budget share in 2050  ----
  DTglobal <- getNewestVersion(paste("dt.budgetShare", suffix, sep = "."), fileloc("resultsDir"))
  #' get rid of Somalia because it's budget share is 500 * per cap income
  DTglobal <- DTglobal[!region_code.IMPACT159 == "SOM",]
  DT <- data.table::copy(DTglobal)
  scenChoice <- "SSP2-HGEM-REF" # this is the 2050 value for this scenario

  ylab <- "(percent)"
  plottitle = "Food budget share of 2050 per capita income"
  #' statement below excludes tenregions, which is not relevant for a box plot
  for (aggChoice in aggChoiceListBarChart[!aggChoiceListBarChart %in% "tenregions"]) {
    DT <- aggNorder(gdxChoice, DT, aggChoice, scenChoice, mergedVals, plotErrorBars)
    plotErrorBars = FALSE # not relevant for a boxplot
    #' aggregate to and retain only the relevant regions
    fileName <- paste(gdxChoice, l, "budgetShareBoxPlot_2050", aggChoice, suffix,  sep = "_")
    # dt.regions <- regionAgg(aggChoice)
    # DT <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
    # keepListCol.incShare <- c("scenario" ,"year", "region_code.IMPACT159", "region_code", "region_name", "incShare.PCX0")
    # DT <- DT[, (keepListCol.incShare), with = FALSE]
    # scenario.base <- "SSP2-NoCC-REF"
    # DT <- DT[year == "X2050" & scenario == scenario.base]
    # DT[, c("year") := NULL]
    plotByBoxPlot2050(dt = DT, fileName, plotTitle = plottitle, yLab = ylab, yRange = c(0,50), aggChoice, suffix)
  }
  #' the method commented out below to get the box plots stats doesn't work with geom_boxplot. The following link has a
  #' way to do this but I'm not going to implement right now.
  #' http://stackoverflow.com/questions/23970118/how-to-add-summary-information-to-geom-boxplot

  #' Also see https://plot.ly/ggplot2/box-plots/ discussion on stat_summary

  # line plots -----
  #this is not working right now
  # plotByRegionLine("dt.shannonDiversity", "ShannonDiversity", "Shannon Diversity", yRange = c(20, 80), "I3regions")

  # write out zip files of the csv files in gDir. Commented out Mar 18, 2018
  # filenames.csv <- list.files(path = paste(fileloc("gDir"), sep = "/"), pattern = ".csv")
  # for (l in scenChoiceList) {
  #   temp <- filenames.csv[grep(l, filenames.csv)]
  #   cat("\nWriting .zip file of csv files for", l)
  #   zip(zipfile = paste(fileloc("gDir"), "/", gdxChoice, "_", l, "_csvfiles.zip", sep = ""),
  #       files = paste(fileloc("gDir"), "/", temp, sep = ""), extras = "-qdgds 10m", flags = "-j")
  # }

  # construct graphs for ratios ------
  DT.master <- getNewestVersion(paste("food_agg_AMDR_hi", suffix, sep = "."), fileloc("resultsDir")) # both hi and lo have the same values
  for (macroNut in c("kcalsPerDay.fat_share", "kcalsPerDay.protein_share", "kcalsPerDay.carbohydrate_share")) {
    cat("\nWorking on AMDR for", suffix, "for", macroNut)
    if (macroNut %in%  "kcalsPerDay.fat_share") nutName <- "fat_g"
    if (macroNut %in%  "kcalsPerDay.protein_share") nutName <- "protein_g"
    if (macroNut %in%  "kcalsPerDay.carbohydrate_share") nutName <- "carbohydrate_g"

    for (aggChoice in aggChoiceListBarChart) {
      plotErrorBars <- chooseErrorBars(aggChoice)
      DT <- data.table::copy(DT.master)
      keepListCol <- c("scenario", "region_code.IMPACT159", "year", macroNut)
      deleteListCol <- names(DT)[!names(DT) %in% keepListCol]
      DT[, (deleteListCol) := NULL]
      DT[, value := get(macroNut)]
      DT <- aggNorder(gdxChoice, DT, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)

      # units <- "percent"
      ylab = paste("(",units,")",sep = "")

      #AMDR lo values
      if (nutName %in% "fat_g") AMDR_lo = 20
      if (nutName %in% "carbohydrate_g") AMDR_lo = 45
      if (nutName %in% "protein_g") AMDR_lo = 10

      #AMDR hi values
      if (nutName %in% "fat_g") AMDR_hi = 35
      if (nutName %in% "carbohydrate_g") AMDR_hi = 65
      if (nutName %in% "protein_g") AMDR_hi = 35
      yRangeMinMax <- c(0,70)
      fileName = paste(gdxChoice, l, "AMDRShare", nutName, aggChoice, suffix, sep = "_")
      nutTitle <- capwords(cleanupNutrientNames(nutName))
      plotByRegionBarAMDR(dt = DT, fileName,
                          plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix,
                          scenOrder = get(l), colorList, AMDR_lo, AMDR_hi, graphsListHolder, plotErrorBars)
    }
  }

  # facet maps, food availability by food groups  -----
  cat("\nWorking on facet maps for", suffix)
  worldMap <- getNewestVersion("worldMap", fileloc("mData")) # run storeWorldMapDF() if this is not available
  #DT <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))

  DT <- getNewestVersion(paste("dt.foodAvail.foodGroup", suffix, sep = "."), fileloc("resultsDir"))

  deleteFoodGroups <- c("alcohol", "beverages", "fish", "meats", "oils", "dairy", "eggs", "nutsNseeds")
  DT <- DT[!food_group_code %in% deleteFoodGroups,]
  DT[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
  DT <- DT[year == "X2010" & scenario %in% gsub("-", "_", scenario.base) |
             year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
  DT[food_group_code %in% "nutsNseeds", food_group_code := "nuts and seeds"]
  DT[food_group_code %in% "rootsNPlantain", food_group_code := "roots and plantain"]

  facetColName <- "food_group_code"

  # availability in quantity terms 2050, no CC -----
  legendText <- "Grams per day, 2050, \nno climate change"
  fillLimits <- c(0, 500)
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below give a smooth change
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
  displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste(gdxChoice, l, "facetmap", "FGAvail", "2050", "noCC", suffix, sep = "_")
  facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  #facet map, climate change effect on availability -----
  # convert to wide to do operations aross scenarios
  formula.wide <- "id + food_group_code ~ scenario"
  DT.wide <- data.table::dcast(
    data = DT,
    formula = formula.wide,
    value.var = "value")

  DT.wide[, delta.cc := 100 * (SSP2_HGEM_REF - SSP2_NoCC_REF) / SSP2_NoCC_REF]
  DT.wide[, delta.inc := 100 * (SSP2_NoCC_REF - X2010) / X2010]

  fillLimits.cc <- c(-14, 1)
  DT.wide[, value := delta.cc]
  DT.wide[value < fillLimits.cc[1], value := fillLimits.cc[1]]
  DT.wide[value > fillLimits.cc[2], value := fillLimits.cc[2]]
  legendText <- "Climate Change Effect, 2050, \n(percent)"
  myPalette <- colorRampPalette(rev(brewer.pal(9, "Reds")))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below gives a smooth change
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
  displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste(gdxChoice, l, "facetmap", "FGAvailChange", "climate", suffix, sep = "_")
  facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # increase in availability due to income growth
  DT.wide[, value := delta.inc]

  legendText <- "Income Growth Effect, \n2010-2050, (percent)"
  fillLimits.inc <- c(-30, 100)
  DT.wide[, value := delta.inc]
  DT.wide[value < fillLimits.inc[1], value := fillLimits.inc[1]]
  DT.wide[value > fillLimits.inc[2], value := fillLimits.inc[2]]
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  palette <- myPalette(4)
  facetColName <- "food_group_code"
  #' middle two values shift the palette gradient; the code below gives a smooth change
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
  displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste(gdxChoice, l, "facetmap", "FGAvailChange", "income", suffix, sep = "_")
  facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  #' facet maps, req ratio results ------
  #' use this both for the nutrients to keep and the order in which they should be displayed
  keepListNuts <- c("protein_g", "carbohydrate_g", "calcium_mg", "iron_mg", "zinc_mg", "folate_µg", "vit_a_rae_µg",
                    "vit_d_µg", "vit_e_mg", "vit_b12_µg")

  # adequacy ratios by suffix (base, var, varFort)
  DT.macro <- getNewestVersion(paste("RDA.macro_sum_reqRatio", suffix, sep = "."), fileloc("resultsDir"))
  DT.vits <- getNewestVersion(paste("RDA.vits_sum_reqRatio", suffix, sep = "."), fileloc("resultsDir"))
  DT.minrls <- getNewestVersion(paste("RDA.minrls_sum_reqRatio", suffix, sep = "."), fileloc("resultsDir"))
  DT <- do.call("rbind", list(DT.macro, DT.vits, DT.minrls))

  DT <- DT[nutrient %in% keepListNuts,]
  DT[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
  DT <- DT[year == "X2010" & scenario %in% gsub("-", "_", scenario.base) |
             year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  DT[, nutrient := capwords(cleanupNutrientNames(nutrient))]
  # DT[, nutrient := capwords(nutrient)]
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
  facetColName <- "nutrient"

  #' adequacy 2010 -----
  legendText <- "adequacy ratio, 2010"
  fillLimits <- c(0, 5)
  myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
  palette <- myPalette(4)

  #' middle two values shift the palette gradient;
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], 1, 2, fillLimits[2]))
  temp <- data.table::copy(DT)
  temp <- temp[scenario %in% "X2010",]
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatio", "2010", suffix, sep = "_")
  facetMaps(worldMap, DT = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # adequacy 2050, no CC -----
  legendText <- "adequacy ratio, 2050, \nno climate change"
  fillLimits <- c(0, 5)
  myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
  palette <- myPalette(4)

  #' middle two values shift the palette gradient;
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], 1, 2, fillLimits[2]))
  temp <- data.table::copy(DT)
  temp <- temp[scenario %in% "SSP2_NoCC_REF",]
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatio", "2050", "noCC", suffix, sep = "_")
  facetMaps(worldMap, DT = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # convert to wide to do operations aross scenarios
  formula.wide <- "id + nutrient ~ scenario"
  DT.wide <- data.table::dcast(
    data = DT,
    formula = formula.wide,
    value.var = "value")

  DT.wide[, delta.cc := 100 * (SSP2_HGEM_REF - SSP2_NoCC_REF) / SSP2_NoCC_REF]
  DT.wide[, delta.inc := 100 * (SSP2_NoCC_REF - X2010) / X2010]

  #facet map, climate change effect on adequacy -----
  legendText <- "Climate Change Effect, 2050, \n(percent)"
  fillLimits.cc <- c(-10, 2)
  DT.wide[, value := delta.cc]
  DT.wide[value < fillLimits.cc[1], value := fillLimits.cc[1]]
  DT.wide[value > fillLimits.cc[2], value := fillLimits.cc[2]]

  #myPalette <- colorRampPalette(rev(brewer.pal(9, "Blues")))
  myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below gives a smooth change
  fillRange <- fillLimits.cc[2] - fillLimits.cc[1]
  breakValues <- scales::rescale(c(fillLimits.cc[1], fillLimits.cc[1] + fillRange/3, fillLimits.cc[1] + fillRange/1.5, fillLimits.cc[2]))
  #breakValues <- scales::rescale(c(fillLimits.cc[1], fillLimits.cc[1], -2, -1 , fillLimits.cc[2]))
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange",  "climate", suffix, sep = "_")
  facetMaps(worldMap, DT = DT.wide, fileName, legendText, fillLimits = fillLimits.cc, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # increase in adequacy due to income growth
  legendText <- "Income Growth Effect, \n2010-2050, (percent)"
  fillLimits.inc <- c(8, 100)
  DT.wide[, value := delta.inc]
  DT.wide[value < fillLimits.inc[1], value := fillLimits.inc[1]]
  DT.wide[value > fillLimits.inc[2], value := fillLimits.inc[2]]

  #myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
  myPalette <- colorRampPalette(brewer.pal(9, "Blues"))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below gives a smooth change
  fillRange <- fillLimits.inc[2] - fillLimits.inc[1]
  breakValues <- scales::rescale(c(fillLimits.inc[1], fillLimits.inc[1] + fillRange/3, fillLimits.inc[1] + fillRange/1.5, fillLimits.inc[2]))
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange", "income", suffix, sep = "_")
  facetMaps(worldMap, DT = DT.wide, fileName, legendText, fillLimits = fillLimits.inc, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # facet map, MRV ratio results -----
  DT <- getNewestVersion(paste("dt.MRVRatios", suffix, sep = "."), fileloc("resultsDir"))
  keepListNuts <- c("ft_acds_tot_sat_g", "sugar_g")
  DT <- DT[nutrient %in% keepListNuts,]
  DT[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
  DT <- DT[year == "X2010" & scenario %in% gsub("-", "_", scenario.base) |
             year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  DT[, nutrient := cleanupNutrientNames(nutrient)]
  #DT[, nutrient := capwords(nutrient)]
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")

  facetColName <- "nutrient"

  #' disqualifying index ratio 2010 -----
  legendText <- "disqualifying index ratio, 2010"
  fillLimits <- c(0, 4)
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  palette <- myPalette(4)

  #' middle two values shift the palette gradient;
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], 1, 2, fillLimits[2]))
  temp <- data.table::copy(DT)
  temp <- temp[scenario %in% "X2010",]
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "MRVRatio", "2010", suffix, sep = "_")
  facetMaps(worldMap, DT = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # disqualifying index ratio 2050, no CC -----
  legendText <- "disqualifying index ratio, \nno climate change"
  fillLimits <- c(0, 4)
  myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
  palette <- myPalette(4)

  #' middle two values shift the palette gradient;
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], 1, 2, fillLimits[2]))
  temp <- data.table::copy(DT)
  temp <- temp[scenario %in% "SSP2_NoCC_REF",]
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "MRVRatio", "2050", "noCC", suffix, sep = "_")
  facetMaps(worldMap, DT = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # convert to wide to do operations aross scenarios
  formula.wide <- "id + nutrient ~ scenario"
  DT.wide <- data.table::dcast(
    data = DT,
    formula = formula.wide,
    value.var = "value")

  DT.wide[, delta.cc := 100 * (SSP2_HGEM_REF - SSP2_NoCC_REF) / SSP2_NoCC_REF]
  DT.wide[, delta.inc := 100 * (SSP2_NoCC_REF - X2010) / X2010]

  #facet map, climate change effect on disqualifying index ratio -----
  legendText <- "Climate Change Effect, 2050, \n(percent)"
  fillLimits.cc <- c(-3, 6)
  DT.wide[, value := delta.cc]
  DT.wide[value < fillLimits.cc[1], value := fillLimits.cc[1]]
  DT.wide[value > fillLimits.cc[2], value := fillLimits.cc[2]]

  #myPalette <- colorRampPalette(rev(brewer.pal(9, "Reds")))
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below gives a smooth change
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
  DT <- DT.wide
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "MRVRatioChange", "climate", suffix, sep = "_")
  facetMaps(worldMap, DT = DT.wide, fileName, legendText, fillLimits = fillLimits.cc, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # increase in adequacy due to income growth
  legendText <- "Income Growth Effect, \n2010-2050, (percent)"
  fillLimits.inc <- c(-20, 75)
  DT.wide[, value := delta.inc]
  DT.wide[value < fillLimits.inc[1], value := fillLimits.inc[1]]
  DT.wide[value > fillLimits.inc[2], value := fillLimits.inc[2]]

  # myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  myPalette <- colorRampPalette(brewer.pal(9, "Reds"))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below gives a smooth change
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
  DT <- DT.wide
  displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
  fileName <- paste(gdxChoice, l, "facetmap", "MRVRatioChange", "income", suffix, sep = "_")
  facetMaps(worldMap, DT = DT.wide, fileName, legendText, fillLimits = fillLimits.inc, palette, facetColName, graphsListHolder, breakValues, displayOrder)

  # filenames.pdf <- list.files(path = paste(fileloc("gDir"), sep = "/"), pattern = ".pdf")
  # for (scenChoice in scenChoiceList) {
  #   #create zip file of all the graphics outputs for a set of scenarios
  #   temp <- filenames.csv[grep(scenChoice, filenames.pdf)]
  #   cat("\nWriting .zip file of pdf files for", scenChoice)
  #   zip(zipfile = paste(fileloc("gDir"), "/", gdxChoice, scenChoice, "_pdffiles.zip", sep = ""),
  #       files = paste(fileloc("gDir"), "/", temp, sep = ""), extras = "-qdgds 10m", flags = "-j")

  #create  xlsx file with the data used to create the  WB figures -----
  rowCounter <- 1
  aggChoice <- "WB"
  # list of potential nutrients to add to the table
  # macroNutrients <- c("carbohydrate_g", "protein_g", "fat_g",  "totalfiber_g")
  # vitamins <- keyVariable("vitamins")
  # minerals <- keyVariable("minerals")
  # kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
  energy <- c("energy_kcal")
  # addedSugar <- c("sugar_g")
  # fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
  #                 "ft_acds_tot_trans_g")
  #
  nutlistmacro <- keyVariable("macronutrients")[!keyVariable("macronutrients") %in% "fat_g"]
  #    nutlistmacroAMDRlo <- c("carbohydrate_g_AMDRlo", "protein_g_AMDRlo",  "fat_g_AMDRlo")
  nutlistmacroAMDRShare <- paste("AMDRShare", c("carbohydrate_g", "protein_g",  "fat_g"), sep = "_")
  diversity <- c("nonStapleShare", "RAOqe")
  # #diversity.2 <- c("NutBalScore", "compDI", "compQI")
  nutBal <- c("compDI", "NutBalScore")
  budgetShare <- "budgetShare"
  nonStapleShareKcals <- "nonStapleShare"
  # #boxStats <- "boxstats"
  dailyAvail.foodgroup <- foodGroupList

  csvHolder <- data.table::data.table(scenario = character(0),
                                      # "Low Income" = numeric(0), "Low middle income" = numeric(0),
                                      # "Upper middle income" = numeric(0), "High income" = numeric(0),
                                      # "2050 CC effect, low income" = numeric(0),
                                      # "2050 CC effect, low middle income" = numeric(0),
                                      # "2050 CC effect, upper middle income" = numeric(0),
                                      # "2050 CC effect, high income" = numeric(0))
                                      lowInc = numeric(0), lowMidInc = numeric(0), upMidInc = numeric(0), highInc = numeric(0),
                                      DlowInc = numeric(0), DlowMidInc = numeric(0), DupMidInc = numeric(0), DhighInc = numeric(0))

  incCats <- c("lowInc", "lowMidInc", "upMidInc", "highInc")
  DincCats <- c("DlowInc", "DlowMidInc", "DupMidInc", "DhighInc")
  #  scen2050list <- c("SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
  scen2050list <- c("SSP2-HGEM")

  figsData <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb = figsData, sheetName = "FigureData")
  colHeaders <- c("Scenario","Low Income", "Low middle income", "Upper middle income", "High income",
                  "2050 CC effect, low income", "2050 CC effect, low middle income",
                  "2050 CC effect, upper middle income", "2050 CC effect, high income")

  #write column names to the spreadsheet
  openxlsx::writeData(
    wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = TRUE, startCol = 1,
    x = as.list(colHeaders),
    startRow = rowCounter
  )
  rowCounter <- rowCounter + 1
  #boxstats removed for now
  for (figtype in c(budgetShare, dailyAvail.foodgroup, energy, nutlistmacro, nutlistmacroAMDRShare,
                    vitamins, minerals, nutBal, diversity)) {
    #   if (i %in% c(budgetShare, boxStats)) {
    if (figtype %in% c(budgetShare)) {
      filename <- paste(gdxChoice, l, figtype, "WB", suffix, sep = "_")
      figInfo <- "4, affordability,"
    }

    # if (figtype %in% nutlistmacro) {
    filename <- paste(gdxChoice, l, "macro_reqRatio", figtype, aggChoice, suffix, sep = "_")
    figInfo <- "5, adequacy, macro nutrients, "
  }
  if (figtype %in% nutlistmacroAMDRShare) {
    filename <- paste(gdxChoice, l, figtype, aggChoice, suffix, sep = "_")
    #       filename <- gsub("AMDRShare", "_", filename) # a kludge to deal with the fact that these are macronutrients
    figInfo <- "6, Share of total kilocalories, "
  }
  if (figtype %in% minerals) {
    filename <- paste(gdxChoice, l, "minrls_reqRatio", figtype, aggChoice, suffix, sep = "_")
    figInfo <- "7, adequacy, minerals, "
  }
  if (figtype %in% vitamins) {
    filename <- paste(gdxChoice, l, "vits_reqRatio", figtype, aggChoice, suffix, sep = "_")
    figInfo <- "7, adequacy, vitamins, "
  }
  if (figtype %in% nutBal) {
    filename <- paste(gdxChoice, l, figtype, aggChoice, suffix, sep = "_")
    figInfo <- "8, Nutrient balance metrics, "
  }
  if (figtype %in% diversity) {
    filename <- paste(gdxChoice, l, figtype, aggChoice, suffix, sep = "_")
    figInfo <- "9, diversity metrics, "
  }
  # moved to supplementary information
  if (figtype %in% dailyAvail.foodgroup) {
    filename <- paste(gdxChoice, l, "foodAvail_foodGroup", figtype, aggChoice, suffix, sep = "_")
    figInfo <- "S1, food group daily availability (g), "
  }
  # moved to supplementary information
  if (figtype %in% energy) {
    filename <- paste(gdxChoice, l, "kcals.values", aggChoice, suffix, sep = "_")
    figInfo <- "S2, average daily availability dietary energy (kcals), "
  }

  fileIn <- data.table::fread(paste(fileloc("gDir"), "/", filename, ".csv", sep = ""), select = 2:6)
  for (j in scen2050list) {
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == gsub("-REF", "", scenario.base), get(incCats[k])]
      fileIn[scenario %in% j, DincCats[k] := (get(incCats[k]) - baseVal) / baseVal]
    }
  }
  #write name of data set to the spreadsheet
  nutshortName <- cleanupNutrientNames(figtype)
  openxlsx::writeData(
    wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = FALSE, startCol = 1,
    x = paste("Figure", figInfo, nutshortName),
    startRow = rowCounter
  )
  rowCounter <- rowCounter + 1
cat("\nrowCounter: ", rowCounter)
  openxlsx::writeData(
    wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = FALSE, startCol = 1,
    x = fileIn,
    startRow = rowCounter
  )
  rowCounter <- rowCounter + nrow(fileIn)
  cat("rowCounter: ", rowCounter)

  category <- data.table::data.table(scenario = figtype,
                                     lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
                                     DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  csvHolder <- rbind(csvHolder, category)
  csvHolder <- rbind(csvHolder, fileIn)

  # data.table::setnames(csvHolder,
  #                      old = names(csvHolder),
  #                      new = colHeaders)

  openxlsx::addStyle(
    wb = figsData, sheet = "FigureData", style = numStyle, cols = 2:length(csvHolder), rows = 2:nrow(csvHolder),
    gridExpand = TRUE)

  openxlsx::saveWorkbook(wb = figsData, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_", aggChoice, "_", suffix, ".xlsx", sep = ""),
                         overwrite = TRUE)
  data.table::fwrite(csvHolder, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_", aggChoice, ".csv", sep = ""), na = "", row.names = FALSE)

  # aggregate RDA reqRatios to regions -----
  filesToAgg <- c("RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio")
  for (fname in filesToAgg) {
    for (aggChoice in c("WB")) {
      #      for (aggChoice in c("AggReg1", "WB")) { commented out Mar 18, 2018
      DT <- getNewestVersion(paste(fname, suffix, sep = "."), fileloc("resultsDir"))
      DT <- merge(DT, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))
      dt.regions <- regionAgg(aggChoice)
      # aggregate to and retain only the relevant regions; region code is the code for the region
      merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")

      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]
      keepListCol <- c("scenario", "region_code", "region_name", "year", "nutrient", "value")
      deleteListCol <- names(merged)[!names(merged) %in% keepListCol]
      merged[, (deleteListCol) := NULL]
      merged <- unique(merged)
      inDT <- merged
      outName <- paste(fname, aggChoice, suffix, sep = ".")
      desc <- paste0("Aggregate RDA req ratios from data file ", fname)
      cleanup(inDT, outName, destDir = fileloc("resultsDir"), desc = desc)
    }
  }

  # save graph files for future use
  inDT <- graphsListHolder
  outName <- paste("graphsListHolder", suffix, sep = ".")
  desc <- paste0("File with graphics created for presentation for ", suffix )
  cleanupGraphFiles(inDT, outName, fileloc("gDir"), desc = desc)
}
finalizeScriptMetadata(metadataDT, sourceFile)
