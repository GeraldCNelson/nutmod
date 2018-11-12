#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Aggregate and graph nutrient modeling data
#' @name aggRun.R
#' @include nutrientModFunctions.R
library(Cairo) # may be necessary for Calibri font
library(extrafont) # may be necessary for Calibri font

#' @name aggRun.R
#' @keywords aggregate, sort, and graph data
#' @description
#' This script generates graphs of nutrient information aggregated to various levels.

#Copyright (C) 2016 - 2018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

source("R/nutrientModFunctions.R")
source("R/aggNorder.R") # needed to load function updateLegendGrobs. Could move this function elsewhere April 30, 2018
library(RColorBrewer)
# next 3 libraries needed for world maps
library(sp)
library(broom)
library(rgdal)
sourceFile <- "aggRun.R"
createScriptMetaData()

# gdxChoice values are SSPs, USAID, or USAIDPriorities
gdxChoice <- getGdxChoice()
# DTGlobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, reqRatio_sum_RDA_macro, reqRatio_sum_RDA_minrls, reqRatio_sum_RDA_vits
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

dt.nutcodeLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/NutrientCodeLookup.xlsx"))

#nutrients grouping
macroNutrients <- keyVariable("macronutrients")
vitamins <- keyVariable("vitamins")
minerals <- keyVariable("minerals")
kcals <- keyVariable("energy") # the keyVariable includes "kcals.ft_acds_tot_sat_g"; may need to delete
fattyAcids <- keyVariable("fattyAcids")
# other <- c("sugar_g", "cholesterol_mg") not used below

# create list variable to hold ggplot output
graphsListHolder <- list()

# # delete all files in gDir - commented out all the deletion code July 22, 2018
# graphicsPath <- fileloc("gDir")
# graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
# graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
# #unlink works like file.remove but with fewer messages
# invisible(unlink(graphicsFileList, recursive = FALSE))
#
# # delete all files in gDir/final
# graphicsPath <- paste(fileloc("gDir"), "final", sep = "/")
# graphicsFileList <- list.files(graphicsPath, all.files = TRUE)
# graphicsFileList <- paste(graphicsPath, graphicsFileList, sep = "/")
# invisible(unlink(graphicsFileList, recursive = FALSE))

# function to choose whether errorbars are displayed
chooseErrorBars <- function(aggChoice) {
  if (aggChoice == "tenregions") plotErrorBars = FALSE
  if (aggChoice == "WB") plotErrorBars = TRUE
  if (aggChoice == "AggReg1") plotErrorBars = TRUE
  return(plotErrorBars)
}
# start of code that works with the switches
for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  
  # at the moment temp is here to remind me that the names in multipleNutsFileList need to correspond with those in multipleNutsListShortName
  temp <-  c("dt.nutrients_sum_all", "nutrients_avail",
             "reqRatio_sum_RDA_macro", "reqRatio_macro",
             "reqRatio_sum_RDA_minrls", "reqRatio_minrls",
             "reqRatio_sum_RDA_vits", "reqRatio_vits",
             #    "dt,nutrients_nonstapleShare", "dt,nutrients_nonstapleShare", commented out Mar 9 2018
             "dt_MRVRatios", "badRatios",
             "reqRatio_sum_AMDR_hi", "AMDR_hi",
             #             "reqRatio_sum_AMDR_lo", "AMDR_lo",
             "dt.foodAvail_foodGroup", "foodAvail_foodGroup")
  
  multipleNutsFileList <- c(paste("dt.nutrients_sum_all", suffix, sep = "."),
                            paste("reqRatio_sum_RDA_macro", suffix, sep = "."),
                            paste("reqRatio_sum_RDA_minrls", suffix, sep = "."),
                            paste("reqRatio_sum_RDA_vits", suffix, sep = "."),
                            # paste("dt.nutrients_nonstapleShare", suffix, sep = "."), commented out Mar 9 2018
                            paste("dt.MRVRatios", suffix, sep = "."),
                            paste("reqRatio_sum_AMDR_hi", suffix, sep = "."),
                            #                        paste("reqRatio_sum_AMDR_lo", suffix, sep = "."),
                            paste("dt.foodAvail_foodGroup", suffix, sep = "."))
  multipleNutsListShortName <- c("nutrients.avail",
                                 "reqRatio_macro",
                                 "reqRatio_minrls",
                                 "reqRatio_vits",
                                 # "nutrients.nonstaples.share", commented out Mar 9 2018
                                 # "zinc_bioavail_reqRatio",suffix,sep = ".",
                                 # "iron_bioavail_reqRatio",suffix, sep = ".",
                                 "badRatios",
                                 "AMDR_hi",
                                 #   "AMDR_lo",
                                 "foodAvail.foodGroup"
  )
  
  
  # scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
  # scenChoice for SSPs is scenOrder.SSPs
  if (gdxChoice == "SSPs") {
    #    scenario.base <- "SSP2_NoCC_REF"
    scenario.base.NoCC <-  "SSP2_NoCC"
    scenario.base.CC <-  "SSP2_HGEM"
    
    scenOrderSSP <- c("2010",  "SSP2_NoCC", "SSP2_HGEM", "SSP1_NoCC","SSP3_NoCC")
    scenChoiceList <- "scenOrderSSP"
    scenChoice.name <- gdxChoice
    aggChoiceListBarChart <- c("WB", "tenregions") # missing AggReg2 and  "2EconGroup
  }
  
  if (gdxChoice == "USAIDPriorities") {
    dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
   scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)
    crops.cereals <- c("cmaiz", "crice", "cwhea")
    crops.rootsNtubers <- c("ccass", "cpota", "cyams")
    scenario.base.CC <-  "SSP2_HGEM_cf"
    scenChoiceList <- "scenarioListIMPACT"
    # scenarioListIMPACT has too many scenarios. I'm going to try a few.
    scenOrderUSAIDPriorities <- c("2010", scenario.base.CC, paste0("SSP2_HGEM_", crops.cereals))
    cat("\nscenOrderUSAIDPriorities:", scenOrderUSAIDPriorities, "\n")
    scenChoiceList <- "scenOrderUSAIDPriorities"
    scenChoice.name <- gdxChoice
    aggChoiceListBarChart <- c("WB", "tenregions")
  }
  
  if (gdxChoice == "USAID") {
    scenario.base.NoCC <- "SSP2_NoCC"
    # USAID scenario combo choices
    
    # old and new scenario names
    # DT[scenario == "SSP2_NoCC_REF", scenario := "REF_NoCC"]
    # DT[scenario == "SSP2_HGEM2_REF", scenario := "REF_HGEM"]
    # DT[scenario == "SSP2_IPSL2_REF", scenario := "REF_IPSL"]
    # DT[scenario == "SSP2_HGEM_LoYld2", scenario := "MED"]
    # DT[scenario == "SSP2_HGEM_RegYld2", scenario := "REGION"]
    # DT[scenario == "SSP2_HGEM_HiYld2", scenario := "HIGH"]
    # DT[scenario == "SSP2_HGEM_HiNARS2", scenario := "HIGH_NARS"]
    # DT[scenario == "SSP2_HGEM_HiREFF2", scenario := "HIGH_RE"]
    # DT[scenario == "SSP2_HGEM_IRREXP2", scenario := "IX"]
    # DT[scenario == "SSP2_NoCC_IRREXP2", scenario := "IX_NoCC"]
    # DT[scenario == "SSP2_IPSL_IRREXP2", scenario := "IX_IPSL"]
    # DT[scenario == "SSP2_HGEM_IRREXP_WUE2", scenario := "IX_WUE"]
    # DT[scenario == "SSP2_NoCC_IRREXP_WUE2", scenario := "IX_WUE_NoCC"]
    # DT[scenario == "SSP2_IPSL_IRREXP_WUE2", scenario := "IX_WUE_IPSL"]
    # DT[scenario == "SSP2_HGEM_SWHC2", scenario := "ISW"]
    # DT[scenario == "SSP2_NoCC_SWHC2", scenario := "ISW_NoCC"]
    # DT[scenario == "SSP2_IPSL_SWHC2", scenario := "ISW_IPSL"]
    # DT[scenario == "SSP2_HGEM_PHL_DEV2", scenario := "RPHL"]
    # DT[scenario == "SSP2_HGEM_MMEFF2", scenario := "RMM"]
    # DT[scenario == "SSP2_HGEM_Pangloss2", scenario := "COMP"]
    # DT[scenario == "SSP2_NoCC_Pangloss2", scenario := "COMP_NoCC"]
    # DT[scenario == "SSP2_IPSL_Pangloss2", scenario := "COMP_IPSL"]
    
    prodEnhance <- c("2010", scenario.base.CC, "SSP2_HGEM_LoYld2", "SSP2_HGEM_HiYld2", "SSP2_HGEM_HiNARS2", "SSP2_HGEM_HiREFF2", "SSP2_HGEM_RegYld2")
    waterMan <- c("2010", scenario.base.CC, "SSP2_HGEM_IRREXP2", "SSP2_HGEM_IRREXP_WUE2", "SSP2_HGEM_SWHC2",
                  "SSP2_IPSL_IRREXP_WUE2", "SSP2_IPSL_IRREXP2", "ISW_NoCC", "ISW_IPSL")
    addEnhance <- c("2010", scenario.base.CC, "SSP2_HGEM_PHL_DEV2", "SSP2_HGEM_MMEFF2")
    comp <- c("2010", scenario.base.CC, "SSP2_HGEM_Pangloss", "SSP2_NoCC_Pangloss2", "SSP2_IPSL_Pangloss2")
    #    aggChoiceListBarChart <- c("AggReg1") commented out Mar 18, 2018
    
    #USAID scenario list choice
    scenChoiceList <- c("prodEnhance", "waterMan", "addEnhance", "comp")
    scenChoice.name <- gdxChoice
  }
  
  dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
  
  # mergedVals is used in aggNorder to determine what regions get merged. The regionAgg function sets region_code to the proper values for the relevant region aggregation.
  mergedVals <- c("scenario", "region_code", "year")
  
  l <- scenChoiceList # removed the list because scenChoiceList only has one value
  #put colorlist here so its easy to see alignment of colors and bars
  scenOrder <- get(scenChoiceList)
  paletteChoice <- "OrRd" #choices are described in the help for RColorBrewer
  if (l %in% "scenOrderSSP") {
    #put here so its easy to see alignment of colors and bars
    #    colorList <- c("black", "red", "green4", "red3", "red4")
    colorsNeeded <- 4
    colorList <- c("darkgray", brewer.pal(colorsNeeded, paletteChoice)) # change 2010 color to darkgray from #000000, May 29, 2018
    
    # a kludge to make the climate scenario green (#2ca25f)
    colorList[3] <- "#2CA25F"
    
  } else {
    colorsNeeded <- length(get(l))
    cat("colorsNeeded: ", colorsNeeded)
    #    colorList <- c("black", rainbow(10)[1:length(get(l))])
    colorList <- c("darkgray", brewer.pal(colorsNeeded, paletteChoice)) # change 2010 color to darkgray from #000000, May 29, 2018
  }
  
  for (aggChoice in aggChoiceListBarChart) {
    plotErrorBars <- chooseErrorBars(aggChoice)
    #   generate the legends -----
    #' needs to be inside l and aggChoice loop -----
    for (legendOrient in c("bottom", "right")) {
      legendname <- paste("legend", legendOrient, gdxChoice, l,  aggChoice, sep = "_")
      graphsListHolder[[legendname]] <- updateLegendGrobs(l, aggChoice, legendLoc = legendOrient, mergedVals)
    }
    #    # Shannon Diversity -----
    #    cat("\nWorking on bar chart for Shannon Diversity for", i)
    #    DT <- aggNorder(gdxChoice, DTGlobal = "dt.shannonDiversity", aggChoice, scenChoice = get(l), mergedVals)
    #    ylab <- "(percent)"
    #    filename = "ShannonDiversity"
    #    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    #    plotByRegionBar(dt = DT, fileName = filename, plotTitle = "Shannon Diversity", yLab = ylab,
    #                    yRange = yRangeMinMax, aggChoice, suffix, scenOrder, oneLine = FALSE, colorList)
    # #   yRange = c(0, 80), aggChoice, oneLine = FALSE)
    
    # Budget share -----
    cat("\nWorking on bar chart for budget share (Identical for all suffixes) for", aggChoice, "\n")
    DTGlobal <- getNewestVersion(paste("dt.budgetShare", sep = "."), fileloc("resultsDir"))
     DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- "(percent)"
    fileName <- paste(gdxChoice, "_", l, "_",  "budgetShare", "_", aggChoice, ".", suffix, sep = "")
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    yRangeMinMax.budgetshare <- c(0,50) # custom for budgetShare
    plotErrorBars <- chooseErrorBars(suffix)
    # plotByRegionBar(dt = DT, fileName, plotTitle = "Food budget share of per capita income", # old version. New version has no plot title
    plotByRegionBar(dt = DT, fileName, plotTitle = "",
                    yLab = ylab, yRange = yRangeMinMax.budgetshare, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars)
    
    #  yLab = "(percent)", yRange = c(0, 50), aggChoice, oneLine = FALSE)
    
    cat("\nDone with bar chart for budget share for", aggChoice, "\n")
    #     # MFAD -----
    #     cat("\nWorking on bar chart for MFAD for", i)
    #     DT <- aggNorder(gdxChoice, DTGlobal = "dt.MFAD", aggChoice, scenChoice = get(l), mergedVals)
    #     ylab <- "(percent)"
    #     if (ylab %in%  "(percent)") {yRangeMinMax.budgetshare <- c(0,100)} else {yRangeMinMax.budgetshare <- c(0, max(DT$value) )}
    #     plotByRegionBar(dt = DT, fileName = "MFAD", plotTitle = "Modified Functional Attribute Diversity",
    #                     yLab = ylab, yRange = yRangeMinMax.budgetshare, aggChoice,  scenOrder, oneLine = FALSE, colorList)
    #   #  yLab = "(percent)", yRange = c(0, 100), aggChoice, oneLine = FALSE)
    # print(paste("Done with bar chart for MFAD for", i))
    
    # RAOqe -----
    cat("\nWorking on bar chart for Rao's QE for", aggChoice, "\n")
    DTGlobal <- getNewestVersion(paste("dt.RAOqe", suffix, sep = "."), fileloc("resultsDir"))
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    fileName <- paste(gdxChoice, "_", l, "_",  "RAOqe", "_", aggChoice, ".", suffix, sep = "")
    plotByRegionBar(dt = DT, fileName, plotTitle = "Rao's quadratic entropy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE,
                    colorList, graphsListHolder, plotErrorBars)
    #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    # nonstaple share of kcals ------
    cat("\nWorking on bar chart for the nonstaple share of kcals for", suffix, "for", aggChoice, "\n")
    
    DTGlobal <- getNewestVersion(paste("dt.KcalShare_nonstaple", suffix, sep = "."), fileloc("resultsDir"))
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l),
                    mergedVals, plotErrorBars)
    ylab <- "(percent)"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    fileName = paste(gdxChoice, "_", l, "_",  "nonStapleShare", "_", aggChoice, ".", suffix, sep = "")
    plotByRegionBar(dt = DT, fileName, plotTitle = "Non-staple share of energy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE,
                    colorList, graphsListHolder, plotErrorBars)
    
    # nutrition benefit score -----
    cat("\nWorking on bar chart for the NBS for", suffix, "for", aggChoice, "\n")
    DTGlobal <- getNewestVersion(paste("dt.nutBalScore", suffix, sep = "."), fileloc("resultsDir"))
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- "" # this creates the ylab variable and leaves it empty. NULL deletes it!
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    fileName <- paste(gdxChoice, "_", l, "_",  "NutBalScore", "_", aggChoice, ".", suffix,  sep = "")
    plotByRegionBar(dt = DT, fileName, plotTitle = "Nutrient balance score",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars)
    #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    #   # composite QI score -----
    #   cat("\nWorking on bar chart for the QI composite for", i))
    #   DT <- aggNorder(gdxChoice, DTGlobal = "dt.compQI", aggChoice, scenChoice = get(l), mergedVals)
    #   ylab <- ""
    #   if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    #   plotByRegionBar(dt = DT, fileName = "compQI", plotTitle = "Composite qualifying index",
    #                   yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder, oneLine = FALSE, colorList)
    # #  yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    # composite DI score ----- 
    # xxxx why doesn't DTGlobal include aggChoice
    cat("\nWorking on bar chart for disqualifying nutrients for", suffix, "for", aggChoice, "\n")
    fileName = paste(gdxChoice, "_", l, "_",  "compDI", "_", aggChoice, ".", suffix,  sep = "")
    DTGlobal <- getNewestVersion(paste("dt.compDI", suffix, sep = "."), fileloc("resultsDir")) # don't put aggChoice in here
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName, plotTitle = "Composite disqualifying index",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l),
                    oneLine = FALSE, colorList, graphsListHolder, plotErrorBars)
    #    yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    # total kcal stacked bar chart ----
    cat("\nWorking on stacked kcal bar chart for", suffix, "for", aggChoice, "\n")
    
    DTGlobal <- getNewestVersion(paste("dt.nutrients_kcals", suffix, sep = "."), fileloc("resultsDir"))
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    
    # do stacked bar chart of sources of kcals
    DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l),
                    mergedVals =  c("scenario", "region_code", "year", "nutrient"), plotErrorBars) # I think nutrient might be ignored in aggNorder. July 17, 2018
    DT <- DT[nutrient %in% c("kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_other", "kcalsPerDay_protein"), ]
    DT[, nutrient := gsub("kcalsPerDay_", "", nutrient)]
    yLab <- "(Kcals)"
    if (yLab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, round(max(DT$value)) )}
    fileName = paste(gdxChoice, "_", l, "_",  "kcals.values_bySource", "_", aggChoice, ".", suffix, sep = "")
    plotByRegionStackedBar(dt = DT, fileName, plotTitle = "Average daily dietary energy by source",
                           yLab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList, graphsListHolder = graphsListHolder)
    
    
    # do bar chart of total kcals by agg choice
    cat("\nWorking on total daily kcal bar chart for", suffix, "for", aggChoice, "\n")
    DT <- DTGlobal[nutrient %in% "kcalsPerDay_tot"]
    DT[, nutrient := NULL]
    DT <- aggNorder(gdxChoice, DTaggNorder = DT, aggChoice, scenChoice = get(l),  mergedVals, plotErrorBars)
    
    yLab <- "(Kcals per day)"
    yRangeMinMax <- c(0, round(max(DT$value)))
    fileName = paste(gdxChoice, "_", l, "_",  "kcals.tot.perDay", "_", aggChoice, ".", suffix, sep = "")
    
    plotByRegionBar(dt = DT, fileName, plotTitle = "",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars)
    
    # food availability by food group -----
    cat("\nWorking on bar chart for dt.foodAvail.foodGroup for", suffix, "for", aggChoice, "\n")
    
    DTGlobal <- getNewestVersion(paste("dt.foodAvail_foodGroup", suffix, sep = "."), fileloc("resultsDir"))
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    
    foodGroupList <- sort(unique(DTGlobal$food_group_code))
    plotErrorBars <- chooseErrorBars(aggChoice)
    
    for (fg in foodGroupList) {
      units <- "grams"
      DT <- data.table::copy(DTGlobal)
      DT <- DT[food_group_code %in% fg,]
      DT[, food_group_code := NULL]
      DT <- aggNorder(gdxChoice, DTaggNorder = DT, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      
      fg.longName <- cleanupNutrientNames(fg)
      nutTitle <- paste(tolower(fg.longName), sep = "")
      ylab = paste("(",units,")",sep = "")
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      fileName = paste(gdxChoice, "_", l, "_",  "foodAvail_foodGroup", "_", fg, "_", aggChoice, ".", suffix, sep = "")
      plotByRegionBar(dt = DT, fileName, plotTitle = nutTitle,
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                      graphsListHolder, plotErrorBars)
      #      yRange = c(0, 80), aggChoice, oneLine = FALSE)
    }
    
    cat("\nDone with food groups --------------------\n")
    
    # nutrient availability/adequacy loop -----
    multipleNutsFileList <- multipleNutsFileList[!multipleNutsFileList %in% "dt.foodAvail_foodGroup.var"]
    for (k in 1:length(multipleNutsFileList)) {
      cat("Working on multiple nut file", multipleNutsFileList[k], " for", suffix, "for", aggChoice, "\n") 
      cat("k is ", k, "\n")
      DTGlobal <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
      DTGlobal[, scenario := gsub("-", "_", scenario)]
      DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nov 3, 2018
      #     temp.in <- temp.in[nutrient %in% keepListNuts,]
      plotErrorBars <- chooseErrorBars(aggChoice)
      
      keepListNuts <- unique(DTGlobal$nutrient)[!unique(DTGlobal$nutrient) %in%
                                                  c("caffeine_mg", "cholesterol_mg", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g",
                                                    "kcals.ft_acds_tot_sat_g", "kcals.protein_g", "kcals.sugar_g", "kcalsPerDay_carbohydrate", "kcalsPerDay_ethanol",
                                                    "kcalsPerDay_fat", "kcalsPerDay_ft_acds_tot_sat",
                                                    "kcalsPerDay_other", "kcalsPerDay_protein", "kcalsPerDay_sugar", "kcalsPerDay_tot",
                                                    "kcals_carbohydrate_g", "kcals_ethanol_g", "kcals_fat_g", "kcals_ft_acds_tot_sat_g", "kcals_protein_g", "kcals_sugar_g")] # added Nov 6, 2018
      cat("keeplistnuts for ", multipleNutsFileList[k], ":", keepListNuts)
      keepListNutsNames <- character()
      for (i in 1:length(keepListNuts)) {
        keepListNutsNames[i] <- dt.nutcodeLU[nutCode %in% keepListNuts[i], NutrShortDesc]
      }
      dt.nuts <- data.table(nutListtemp = keepListNuts, nutListtempclean = keepListNutsNames)
      
      for (nut in keepListNuts) {
        #          nutshortName <- cleanupNutrientNames(nut)
        nutshortName <- dt.nuts[nutListtemp %in% nut, nutListtempclean]
        nutlongName <- dt.nutrientNames_Units[1, (nut)]
        units <- dt.nutrientNames_Units[2, (nut)]
        if (units %in% "g") units <- "grams"
        DT <- DTGlobal[nutrient %in% nut,]
        DT[, nutrient := NULL]
        DT <- aggNorder(gdxChoice, DTaggNorder = DT, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
        DTmax <- max(DT$value)
        # cat("\nDTmax:", DTmax)
        
        #' create file specific name and related info
        if (multipleNutsFileList[k] %in% paste("dt.nutrients_sum_all", suffix, sep = "."))  {
          #         nutTitle <- paste("Average daily availability of ", tolower(nutlongName), sep = "")
          nutTitle <- paste(tolower(nutlongName), sep = "")
          ylab = paste("(",units,")",sep = "")
          drawOneLine = FALSE
          yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )
        }
        if (multipleNutsFileList[k] %in% c(paste("reqRatio_sum_RDA_macro", suffix, sep = "."),
                                           paste("reqRatio_sum_RDA_minrls", suffix, sep = "."),
                                           paste("reqRatio_sum_RDA_vits", suffix, sep = ".")))  {
          nutTitle <- nutshortName
          ylab = "(Adequacy ratio)"
          drawOneLine = 1
          yRangeMinMax <- c(0, 3.0) # maximum is 3
        }
        if (multipleNutsFileList[k] %in% paste("dt.MRVRatios", suffix, sep = "."))  {
          #        nutTitle <- paste("Ratio of ", nutshortName, " availability to MRV", sep = "")
          nutTitle <- nutshortName
          #          ylab = "(Maximal reference value)"
          ylab = ""
          yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )
          drawOneLine = FALSE
        }
        
        # only AMDRshare is used so no need for the individual AMDR graphs
        # if (multipleNutsFileList[k] %in% paste("reqRatio_sum_AMDR_hi", suffix, sep = "."))  {
        #   nutTitle <- paste("AMDR high, ", nutshortName, sep = "")
        #   ylab = "(AMDR, high)"
        #   drawOneLine = 1
        #   yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )
        #
        # }
        # if (multipleNutsFileList[k] %in% paste("reqRatio_sum_AMDR_lo", suffix, sep = "."))  {
        #   nutTitle <- paste("AMDR low, ", nutshortName, sep = "")
        #   ylab = "(AMDR, low)"
        #   drawOneLine = 1
        #   yRangeMinMax <- c(0, max(DT$value) + 0.02 * DTmax )
        # }
        #commented out March 9, 2018
        # if (multipleNutsFileList[k] == paste("dt.nutrients.nonstapleShare", suffix, sep = "."))  {
        #   #        nutTitle <- paste("Non-staple share of ", nutshortName, " availability", sep = "")
        #   nutTitle <- paste(nutshortName, sep = "")
        #   ylab = "(percent)"
        #   drawOneLine = FALSE
        # }
        #          if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,103)} else {yRangeMinMax <- c(0, 1.5) }
        
        fileName = paste(gdxChoice, "_", l, "_",  multipleNutsListShortName[k], "_", nut, "_", aggChoice, ".", suffix, sep = "")
        cat("fileName is ", fileName,"\n")
        #         print(summary(DT))
        plotByRegionBar(dt = DT, fileName,
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix,
                        scenOrder = get(l), oneLine = drawOneLine, colorList, graphsListHolder, plotErrorBars)
      }
    }
    
    cat("\nDone with ", multipleNutsFileList[k], " ----------------")
  }
  
  cat("\nDone with scenChoiceList loop ----------")
  
  # box plot of budget share in 2050  -----
  DTGlobal <- getNewestVersion(paste("dt.budgetShare", sep = "."), fileloc("resultsDir"))
  DTGlobal[, scenario := gsub("-", "_", scenario)]
  DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
  
  #' get rid of Somalia because it's budget share is 500 * per cap income
  DTGlobal <- DTGlobal[!region_code.IMPACT159 == "SOM",]
  DT <- data.table::copy(DTGlobal)
  scenChoice <- scenario.base.CC # this is the 2050 value for this scenario. Made this the CC scenario Oct 21, 2018
  
  ylab <- "(percent)"
  #  plottitle = "Food budget share of 2050 per capita income" # commented out July 11, 2018 because the title will be in the figure title in word
  plottitle = ""
  #' statement below excludes tenregions, which is not relevant for a box plot
  for (aggChoice in aggChoiceListBarChart[!aggChoiceListBarChart %in% "tenregions"]) {
    DT <- aggNorder(gdxChoice, DTaggNorder = DT, aggChoice, scenChoice, mergedVals, plotErrorBars)
    plotErrorBars = FALSE # not relevant for a boxplot
    #' aggregate to and retain only the relevant regions
    fileName <- paste(gdxChoice, "_", l, "_",  "budgetShareBoxPlot_2050", "_", aggChoice, ".", suffix,  sep = "")
    # dt.regions <- regionAgg(aggChoice)
    # DT <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
    # keepListCol.incShare <- c("scenario" ,"year", "region_code.IMPACT159", "region_code", "region_name", "incShare.PCX0")
    # DT <- DT[, (keepListCol.incShare), with = FALSE]
    # scenario.base.NoCC <- "SSP2_NoCC_REF"
    # DT <- DT[year == "X2050" & scenario == scenario.base.NoCC]
    # DT[, c("year") := NULL]
    
    plotByBoxPlot2050(dt = DT, fileName, plotTitle = plottitle, yLab = ylab, yRange = yRangeMinMax.budgetshare, aggChoice, suffix, graphsListHolder = graphsListHolder)
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
  
  # construct graphs for AMDR ratios ------
  DTGlobal <- getNewestVersion(paste("food_agg_AMDR_hi", suffix, sep = "."), fileloc("resultsDir")) # both hi and lo have the same values
  DTGlobal[, scenario := gsub("-", "_", scenario)]
  DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
  AMDRNuts <- c("carbohydrate_g.kcalpercent", "fat_g.kcalpercent", "protein_g.kcalpercent")
  # for (macroNut in c("fat_g.Q", "protein_g.Q", "carbohydrate_g.Q")) {
  for (macroNut in AMDRNuts) { #updated, hopefully correctly, Oct 30, 2018
    cat("\nWorking on AMDRhilo for", suffix, "for", macroNut, "\n")
    if (macroNut %in%  "fat_g.kcalpercent") nutName <- "fat_g"
    if (macroNut %in%  "protein_g.kcalpercent") nutName <- "protein_g"
    if (macroNut %in%  "carbohydrate_g.kcalpercent") nutName <- "carbohydrate_g"
    
    for (aggChoice in aggChoiceListBarChart) {
      plotErrorBars <- chooseErrorBars(aggChoice)
      dt <- data.table::copy(DTGlobal)
      keepListCol <- c("scenario", "region_code.IMPACT159", "year", macroNut)
      dt[, setdiff(names(dt), keepListCol) := NULL]
      dt[, value := get(macroNut)]
      dt <- aggNorder(gdxChoice, DTaggNorder = dt, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      
      units <- "percent" # AMDR ratios are in percent
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
      
      fileName = paste(gdxChoice, "_", l, "_",  "AMDRShare", "_", nutName, "_", aggChoice, ".", suffix, sep = "")
      nutTitle <- capwords(cleanupNutrientNames(nutName))
      nutTitle <- paste(nutTitle, "share of total kilocalories", sep = " ")
      
      plotByRegionBarAMDR(dt, fileName,
                          plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix,
                          scenOrder = get(l), colorList, AMDR_lo, AMDR_hi, graphsListHolder = graphsListHolder, plotErrorBars)
      
    }
  }
}
# facet maps for the world  -----
projmap <- getNewestVersion("worldMap", fileloc("uData")) # run storeWorldMapDF() if this is not available
# 2050 budget share map -----
cat("\nWorking on budget share facet maps. \n") # Just do it once, using var suffix
if (suffix %in% "var") {
  DTGlobal <- getNewestVersion(paste("dt.budgetShare", sep = "."), fileloc("resultsDir"))
  DTGlobal[, scenario := gsub("-", "_", scenario)]
  DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
  DT <- DTGlobal[year == "X2010" & scenario %in% scenario.base.CC |
                   year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
  keepListCol <- c("scenario", "id",  "incShare.PCX0" )
  DT[, setdiff(names(DT), keepListCol) := NULL]
  DT <- DT[!id %in% "SOM"][, value := incShare.PCX0][, incShare.PCX0 := NULL]
  DT <- unique(DT)
  
  # do map
  #    DT <- DTGlobal[scenario %in% c("X2010","SSP2_HGEM_REF", "SSP2_NoCC_REF",  "SSP1_NoCC_REF",  "SSP3_NoCC_REF" )]
  DT[, scenario := gsub("X", "", scenario)]
  DT <- DT[scenario %in% get(l),]
  scenOrder <- gsub("_REF", "", get(l)) # put get(l) in the gsub statement Oct 30, 2018
  DT <- DT[, scenario := gsub("_REF", "", scenario)]
  #scenOrder <- c("2010", "SSP2_HGEM", "SSP2_NoCC", "SSP1_NoCC",  "SSP3_NoCC")
  DT <- unique(DT)
  DT[, scenario := factor(scenario, levels = scenOrder)]
  unique(DT$scenario)
  facetColName <- "scenario"
  legendText <- "Food expenditure share of income,\n 2010 and 2050"
  fillLimits <- c(0, 50)
  temp <- truncateDT(DT, fillLimits = fillLimits)
  #   numLimits <- 4
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType))) # Reds can only have a maximum of 9 values
  palette <- myPalette(4)
  displayOrder <- scenOrder # default - alphabetically sorted
  #    displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "budgetShare", "_", "2010_50", "_", "SSP2_HGEM", ".", "world", sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, 
            graphsListHolder = graphsListHolder, displayOrder)
  
}
# facet map, food availability by food groups -----
cat("\nWorking on food availability by selected food groups facet maps for", suffix, "\n")
DTGlobal <- getNewestVersion(paste("dt.foodAvail_foodGroup", suffix, sep = "."), fileloc("resultsDir"))
DTGlobal[, scenario := gsub("-", "_", scenario)]
DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
deleteListRow <- c("alcohol", "beverages", "fish", "meats", "oils", "dairy", "eggs", "nutsNseeds") # remove these food groups
DT <- DTGlobal[!food_group_code %in% deleteListRow,]
DT <- DT[year == "X2010" & scenario %in% scenario.base.CC |
           year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
DT[food_group_code %in% "nutsNseeds", food_group_code := "nuts and oilseeds"]
DT[food_group_code %in% "rootsNPlantain", food_group_code := "roots and plantain"]

#DT is the starting point. Need temp versions for each of the facet maps
# facet map, availability in quantity terms 2050, no CC -----
if (gdxChoice %in% "SSPs") {
  facetColName <- "food_group_code"
  legendText <- "Grams per day, 2050, \nno climate change"
  fillLimits <- c(0, 500)
  temp <- truncateDT(DT, fillLimits = fillLimits)
  temp <- temp[scenario %in% scenario.base.NoCC]
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
  palette <- myPalette(4)
  displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "FGAvail", "_", "2050", "_", "NoCC", ".", suffix, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder,  displayOrder)
}

# convert to wide to do operations aross scenarios for the food availability deltas
formula.wide <- "id + food_group_code ~ scenario"
DT.wide <- data.table::dcast(
  data = DT,
  formula = formula.wide,
  value.var = "value")

if (gdxChoice %in% c("SSPs", "USAID")) { # USAIDPriorities doesn't have a noClimate Change scenario
  #facet map, climate change effect on availability -----
  DT.wide[, value := 100 * (get(scenario.base.CC) - get(scenario.base.NoCC)) / get(scenario.base.NoCC)] 
  facetColName <- "food_group_code"
  legendText <- "Climate Change Effect on Availability in 2050, \n(percent)"
  fillLimits <- c(-14, 1)
  temp <- truncateDT(DT.wide, fillLimits = fillLimits)
  keepListCol <- c("id", facetColName, "value")
  temp[, setdiff(names(temp), keepListCol) := NULL]
  paletteType <- "Reds"
  myPalette <- colorRampPalette(rev(brewer.pal(9, paletteType)))
  palette <- myPalette(4)
  displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "FGAvailChange", "_", "climate", ".", suffix, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, displayOrder)
}
# increase in availability due to income growth -----
DT.wide[, value := 100 * (get(scenario.base.NoCC) - X2010) / X2010]
facetColName <- "food_group_code"
legendText <- "Income growth effect on availability, \n2010-2050, (percent)"
fillLimits <- c(-30, 100)
temp <- truncateDT(DT.wide, fillLimits = fillLimits)
paletteType <- "Spectral"
myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
palette <- myPalette(4)
displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "FGAvailChange", "_", "income", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder, displayOrder)

#' facet maps, adequacy results ------
cat("\nWorking on adequacy facet maps for", suffix, "\n")

#' use this both for the nutrients to keep and the order in which they should be displayed
keepListNuts <- c("carbohydrate_g", "protein_g", "calcium_mg", "iron_mg", "zinc_mg", "folate_µg", "vit_a_rae_µg",
                  "vit_d_µg", "vit_e_mg", "vit_b12_µg")
keepListNutsNames <- character()
for (i in 1:length(keepListNuts)) {
  keepListNutsNames[i] <- dt.nutcodeLU[nutCode %in% keepListNuts[i], NutrShortDesc]
}
# adequacy ratios by suffix (base, var, varFort) -----
fileName.DT.macro <- paste("reqRatio_sum_RDA_macro", suffix, sep = ".")
DT.macro <- getNewestVersion(fileName.DT.macro, fileloc("resultsDir"))
fileName.DT.vits <- paste("reqRatio_sum_RDA_vits", suffix, sep = ".")
DT.vits <- getNewestVersion(fileName.DT.vits, fileloc("resultsDir"))
fileName.DT.minrls <- paste("reqRatio_sum_RDA_minrls", suffix, sep = ".")
DT.minrls <- getNewestVersion(fileName.DT.minrls, fileloc("resultsDir"))
DT <- do.call("rbind", list(DT.macro, DT.vits, DT.minrls))
DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
DT[, scenario := gsub("-", "_", scenario)]
DT[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018

DT <- DT[nutrient %in% keepListNuts,]
DT <- DT[year == "X2010" & scenario %in% gsub("-", "_", scenario.base.CC) |
           year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

#nutListtemp = sort(unique(DT$nutrient))
dt.nuts <- data.table(nutListtemp = keepListNuts, nutListtempclean = keepListNutsNames)

DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
DT[, nutrient := NULL]
setnames(DT, old = c("nutListtempclean", "region_code.IMPACT159"), new = c("nutrient", "id"))

#' adequacy 2010 -----
facetColName <- "nutrient"
legendText <- "Adequacy ratio, 2010"
fillLimits <- c(0, 3) # changed from 5 May 29, 2018
temp <- truncateDT(DT, fillLimits =  fillLimits)
temp <- temp[scenario %in% "X2010",]
paletteType <- "Spectral"
myPalette <- colorRampPalette(brewer.pal(11, paletteType))
palette <- myPalette(4)
displayOrder <- keepListNutsNames
fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "nutReqRatio", "_", "2010", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, displayOrder)

if (gdxChoice %in% c("SSPs")) {
  # adequacy 2050, no CC -----
  facetColName <- "nutrient"
  legendText <- "Adequacy ratio, 2050, \nno climate change"
  fillLimits <- c(0, 3) # changed from 5, May 29, 2018
  temp <- truncateDT(DT, fillLimits)
  temp <- temp[scenario %in% scenario.base.NoCC,]
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  displayOrder <- keepListNutsNames
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "nutReqRatio", "_", "2050", "_", "NoCC", ".", suffix, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, displayOrder)
  
  # convert to wide to do operations aross scenarios for 2050
  formula.wide <- "id + nutrient ~ scenario"
  DT.wide <- data.table::dcast(
    data = DT,
    formula = formula.wide,
    value.var = "value")
  
  #facet map, climate change effect on adequacy in 2050 -----
  DT.wide[, value := 100 * ( get(scenario.base.CC) - get(scenario.base.NoCC)) / get(scenario.base.NoCC)]  
  facetColName <- "nutrient"
  legendText <- "Climate change effect on adequacy, 2050, \n(percent)"
  fillLimits <- c(-10, 2)
  temp <- truncateDT(DT.wide, fillLimits = fillLimits)
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  displayOrder <- keepListNutsNames
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "nutReqRatioChange", "_", "climate", ".", suffix, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder, displayOrder)
}
#facet map, income growth (2010 to 2050) effect on adequacy
DT.wide[, value := 100 * (get(scenario.base.NoCC) - X2010) / X2010]
facetColName <- "nutrient"
legendText <- "Income growth effect on adequacy, \n2010-2050, (percent)"
fillLimits <- c(0, 100) # changed from 8 to 0 July 7, 2018
temp <- truncateDT(DT.wide, fillLimits)
paletteType <- "Blues"
myPalette <- colorRampPalette(brewer.pal(9, paletteType))
palette <- myPalette(4)
displayOrder <- keepListNutsNames
fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "nutReqRatioChange", "_", "income", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder, displayOrder)

# facet map, MRV ratio results -----
cat("\nWorking on MRV ratio facet maps for", suffix, "\n")
DTGlobal <- getNewestVersion(paste("dt.MRVRatios", suffix, sep = "."), fileloc("resultsDir"))
DTGlobal[, scenario := gsub("-", "_", scenario)]
DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
keepListNuts <- c("ft_acds_tot_sat_g", "sugar_g")
keepListNutsNames <- character()
for (i in 1:length(keepListNuts)) {
  keepListNutsNames[i] <- dt.nutcodeLU[nutCode %in% keepListNuts[i], NutrShortDesc]
}
DT <- DTGlobal[nutrient %in% keepListNuts,]
DT <- DT[year == "X2010" & scenario %in% gsub("-", "_", scenario.base.CC) |
           year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
dt.nuts <- data.table(nutListtemp = keepListNuts, nutListtempclean = keepListNutsNames)
DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
DT[, nutrient := NULL]
setnames(DT, old = "nutListtempclean", new = "nutrient")
data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")

#' disqualifying index ratio 2010 -----
facetColName <- "nutrient"
legendText <- "disqualifying index ratio, 2010"
fillLimits <- c(0, 4)
temp <- truncateDT(DT, fillLimits =  fillLimits)
temp <- temp[scenario %in% "X2010",]
paletteType <- "Spectral"
myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
palette <- myPalette(4)
displayOrder <- keepListNutsNames
fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "MRVRatio", "_", "2010", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, displayOrder, height = 3)

# convert to wide to do operations aross scenarios
formula.wide <- "id + nutrient ~ scenario"
DT.wide <- data.table::dcast(
  data = DT,
  formula = formula.wide,
  value.var = "value")

# disqualifying index ratio 2050, no CC -----
cat("\nWorking on disqualifying index ratio facet maps for", suffix, "\n")
if (gdxChoice %in% c("SSPs")) {
  facetColName <- "nutrient"
  legendText <- "Disqualifying index ratio, \nno climate change"
  fillLimits <- c(0, 4)
  temp <- truncateDT(DT, fillLimits)
  temp <- temp[scenario %in% scenario.base.NoCC]
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  displayOrder <- keepListNutsNames
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "MRVRatio", "_","2050", "_", "NoCC", ".", suffix, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder, displayOrder, height = 3)
  
  #facet map, climate change effect on disqualifying index ratio -----
  DT.wide[, value := 100 * ( get(scenario.base.CC) - get(scenario.base.NoCC)) / get(scenario.base.NoCC)]  
  facetColName <- "nutrient"
  legendText <- "Disqualifying index ratio, \nclimate change effect, 2050, \n(percent)"
  fillLimits <- c(-3, 6)
  temp <- truncateDT(DT.wide, fillLimits = fillLimits)
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
  palette <- myPalette(4)
  displayOrder <- keepListNutsNames
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "MRVRatioChange", "_", "climate", ".", suffix, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder, height = 3)
}
# increase in disqualifying index ratio due to income growth -----
DT.wide[, value := 100 * (get(scenario.base.NoCC) - X2010) / X2010]
facetColName <- "nutrient"
legendText <- "Income growth effect on disqualifying index ratio, \n2010-2050, (percent)"
#fillLimits <- c(-20, 75)
fillLimits <- c(-10, 100)
temp <- truncateDT(DT.wide, fillLimits)
paletteType <- "Reds"
myPalette <- colorRampPalette(brewer.pal(9, paletteType))
palette <- myPalette(4)
displayOrder <- keepListNutsNames
fileName <- paste(gdxChoice, "_", l, "_facetmap_", "MRVRatioChange", "_", "income", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder, displayOrder)

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
nutBal <- c("compDI", "NutBalScore")
budgetShare <- "budgetShare"
nonStapleShareKcals <- "nonStapleShare"
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
#  scen2050list <- c("SSP2_GFDL", "SSP2_IPSL", "SSP2_HGEM")
scen2050list <- c("SSP2_HGEM")

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
    filename <- paste(gdxChoice, "_", l, "_", figtype, "_", "WB", ".", suffix, sep = "")
    figInfo <- "1, affordability,"
  }
  
  if (figtype %in% nutlistmacro) {
    filename <- paste(gdxChoice, "_", l, "_", "reqRatio_macro", "_", figtype, "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "2, adequacy, macro nutrients, "
  }
  if (figtype %in% nutlistmacroAMDRShare) {
    filename <- paste(gdxChoice, "_", l, "_", figtype, "_", aggChoice, ".", suffix, sep = "")
    #       filename <- gsub("AMDRShare", "_", filename) # a kludge to deal with the fact that these are macronutrients
    figInfo <- "3, Share of total kilocalories, "
  }
  if (figtype %in% minerals) {
    filename <- paste(gdxChoice, "_", l, "_", "reqRatio_minrls",  "_", figtype, "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "2, adequacy, minerals, "
  }
  if (figtype %in% vitamins) {
    filename <- paste(gdxChoice, "_", l, "_",  "reqRatio_vits",  "_", figtype, "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "2, adequacy, vitamins, "
  }
  if (figtype %in% nutBal) {
    filename <- paste(gdxChoice, "_", l, "_",  figtype, "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "4, Nutrient balance metrics, "
  }
  if (figtype %in% diversity) {
    filename <- paste(gdxChoice, "_", l, "_",  figtype, "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "9, diversity metrics, "
  }
  # moved to supplementary information
  if (figtype %in% dailyAvail.foodgroup) {
    filename <- paste(gdxChoice, "_", l, "_",  "foodAvail_foodGroup",  "_", figtype, "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "S1, food group daily availability (g), "
  }
  # moved to supplementary information.
  if (figtype %in% energy) {
    filename <- paste(gdxChoice, "_", l, "_",  "kcals.tot.perDay",  "_", aggChoice, ".", suffix, sep = "")
    figInfo <- "S2, average daily availability dietary energy (kcals), "
  }
  
  fileIn <- data.table::fread(paste(fileloc("gDir"), "/", filename, ".csv", sep = ""), select = 2:6)
  cat("fileIn:", filename, "\n")
  for (j in scen2050list) {
    # inCats is income categories
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == scenario.base.NoCC, get(incCats[k])]
      fileIn[scenario %in% j, DincCats[k] := (get(incCats[k]) - baseVal) / baseVal] # the delta effects are calculated here
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
  cat("rowCounter: ", rowCounter, "\n")
  openxlsx::writeData(
    wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = FALSE, startCol = 1,
    x = fileIn,
    startRow = rowCounter
  )
  rowCounter <- rowCounter + nrow(fileIn)
  cat("rowCounter: ", rowCounter, "\n")
  
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
  
  openxlsx::saveWorkbook(wb = figsData, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_", aggChoice, ".", suffix, ".xlsx", sep = ""),
                         overwrite = TRUE)
  data.table::fwrite(csvHolder, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_", aggChoice, ".", suffix, ".csv", sep = ""), na = "", row.names = FALSE)
}
# aggregate RDA reqRatios to regions -----
filesToAgg <- c("reqRatio_sum_RDA_macro", "reqRatio_sum_RDA_minrls", "reqRatio_sum_RDA_vits")
for (fname in filesToAgg) {
  for (aggChoice in c("WB")) {
    #      for (aggChoice in c("AggReg1", "WB")) { commented out Mar 18, 2018
    DTGlobal <- getNewestVersion(paste(fname, suffix, sep = "."), fileloc("resultsDir"))
    DTGlobal <- merge(DTGlobal, dt.pop, by = c("scenario","region_code.IMPACT159", "year")) ### where does dt.pop come from? It is loaded in aggNorder.R
    DTGlobal[, scenario := gsub("-", "_", scenario)]
    DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
    dt.regions <- regionAgg(aggChoice)
    # aggregate to and retain only the relevant regions; region code is the code for the region
    merged <- merge(DTGlobal, dt.regions, by = "region_code.IMPACT159")
    
    merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]
    keepListCol <- c("scenario", "region_code", "region_name", "year", "nutrient", "value")
    merged[, setdiff(names(merged), keepListCol) := NULL]
    merged <- unique(merged)
    inDT <- merged
if (gdxChoice %in% "SSPs") inDT <- inDT[scenario %in% scenOrderSSP, ]
    outName <- paste(fname, aggChoice, suffix, sep = ".")
    desc <- paste0("Aggregate RDA req ratios from data file ", fname)
    cleanup(inDT, outName, destDir = fileloc("resultsDir"), desc = desc)
  }
}

# save graph files for future use
inDT <- graphsListHolder
outName <- paste("graphsListHolder", suffix, sep = ".")
desc <- paste0("File with graphics created for presentation with ", suffix, " nutrient data" )
cleanupGraphFiles(inDT, outName, fileloc("gDir"), desc = desc)

finalizeScriptMetadata(metadataDT, sourceFile)
# sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
