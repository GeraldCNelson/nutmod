#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Aggregate and graph nutrient modeling data
#' @name aggRun.R
#' @include nutrientModFunctions.R

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

gdxChoice <- "AfricanAgFutures" # needs to be here so script in nutrientModFunctions.R can find it
regions.AfricanAgFutures <- keyVariable("regions.AfricanAgFutures")
allAfricaCodes <- keyVariable("allAfricaCodes")
source("R/nutrientModFunctions.R")
source("R/aggNorder.R") # needed to load function updateLegendGrobs. Could move this function elsewhere April 30, 2018
library(RColorBrewer)
# next 3 libraries needed for world maps
library(sp)
library(broom)
library(rgdal)
sourceFile <- "aggRun.AfrAgFutures.R"

createScriptMetaData()

# DTGlobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, reqRatio_sum_RDA_macro, reqRatio_sum_RDA_minrls, reqRatio_sum_RDA_vits
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB, regions.AfricanAgFutures

# needed for getting cleaned up nutrient names
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
fileNameHolder <- character(0)

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
  if (aggChoice == "regions.AfricanAgFutures") plotErrorBars = FALSE
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
             #           "reqRatio_sum_AMDR_hi",  "AMDR_hi", # commented out Oct 17, 2018
             #             "reqRatio_sum_AMDR_lo", "AMDR_lo",
             "dt.foodAvail_foodGroup", "foodAvail_foodGroup")
  
  multipleNutsFileList <- c(paste("dt.nutrients_sum_all", suffix, sep = "."),
                            paste("reqRatio_sum_RDA_macro", suffix, sep = "."),
                            paste("reqRatio_sum_RDA_minrls", suffix, sep = "."),
                            paste("reqRatio_sum_RDA_vits", suffix, sep = "."),
                            # paste("dt.nutrients_nonstapleShare", suffix, sep = "."), commented out Mar 9 2018
                            paste("dt.MRVRatios", suffix, sep = "."),
                            #                          paste("reqRatio_sum_AMDR_hi", suffix, sep = "."), # commented out Oct 17, 2018
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
                                 # "AMDR_hi",
                                 #   "AMDR_lo",
                                 "foodAvail.foodGroup"
  )
  
  if (gdxChoice == "AfricanAgFutures") {
    # updated Nov 20, 2018 for revised Gates scenarios
    dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
    scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)
    scenarioListIMPACT <- c("X2010", scenarioListIMPACT[!scenarioListIMPACT %in% c("Med_pes_CC", "Med_opt_CC", "SSP2_SSP2_noCC", "SSP2_SSP2_CC")])
    # scenario.base.NoCC <-  "Med_base_NoCC"
    # scenario.base.CC <-  "Med_base_CC"
    # scenario.base.NoCC <-  "Med_base_NoCC"
    scenario.base.CC <-  "Reference"
    scenChoiceList <- "scenarioListIMPACT"
    scenChoice.name <- gdxChoice
    # aggChoiceListBarChart <- c("WB", "tenregions")
    aggChoiceListBarChart <- c("regions.AfricanAgFutures")
  }
  
  dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
  
  # mergedVals is used in aggNorder to determine what regions get merged. The regionAgg function sets region_code to the proper values for the relevant region aggregation.
  mergedVals <- c("scenario", "region_code", "year")
  
  for (l in scenChoiceList) {
    #put colorlist here so its easy to see alignment of colors and bars
    scenOrder <- get(scenChoiceList)
    paletteChoice <- "OrRd" #choices are described in the help for RColorBrewer
    
    colorsNeeded <- length(get(l))
    cat("colorsNeeded: ", colorsNeeded, "\n")
    #    colorList <- c("black", rainbow(10)[1:length(get(l))])
    colorList <- c("darkgray", brewer.pal(colorsNeeded, paletteChoice)) # change 2010 color to darkgray from #000000, May 29, 2018
    
    for (aggChoice in aggChoiceListBarChart) {
      plotErrorBars <- chooseErrorBars(aggChoice)
      #   generate the legends -----
      #' needs to be inside l and aggChoice loop -----
      for (legendOrient in c("bottom", "right")) {
        legendname <- paste("legend", legendOrient, sep = "_")
        fileName <- paste( legendname, sep = "")
        fileToUse <- dt.budgetShare # need a file from the results dir to get the legend
        p <- updateLegendGrobs(l, aggChoice, legendLoc = legendOrient, mergedVals)
        graphsListHolder[[legendname]] <- p
        if (legendOrient %in% "bottom") ggsave(file = paste0(fileloc("gDir"),"/",fileName,".png"), plot = p,
                                               width = 4.5, height = .4)
      }
    }
    #    # Shannon Diversity -----
    #    cat("\nWorking on bar chart for Shannon Diversity for", i)
    #    dt <- aggNorder(gdxChoice, DTGlobal = "dt.shannonDiversity", aggChoice, scenChoice = get(l), mergedVals)
    #    ylab <- "(percent)"
    #    filename = "ShannonDiversity"
    #    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    #    plotByRegionBar(dt, fileName = filename, plotTitle = "Shannon Diversity", yLab = ylab,
    #                    yRange = yRangeMinMax, aggChoice, suffix, scenOrder, oneLine = FALSE, colorList)
    # #   yRange = c(0, 80), aggChoice, oneLine = FALSE)
    
    # Budget share -----
    cat("\nWorking on bar chart for budget share (Identical for all suffixes) for", aggChoice, "\n")
    DTGlobal <- getNewestVersion(paste("dt.budgetShare", sep = "."), fileloc("resultsDir"))
    dt <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- "(percent)"
    fileName <- paste(gdxChoice, "_",   "budgetShare",  ".", suffix, sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    yRangeMinMax.budgetshare <- c(0,50) # custom for budgetShare
    plotErrorBars <- chooseErrorBars(suffix)
    # plotByRegionBar(dt, fileName, plotTitle = "Food budget share of per capita income", # old version. New version has no plot title
    plotByRegionBar(dt, fileName, plotTitle = "",
                    yLab = ylab, yRange = yRangeMinMax.budgetshare, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars, fill = "scenario")
    
    #  yLab = "(percent)", yRange = c(0, 50), aggChoice, oneLine = FALSE)
    
    cat("\nDone with bar chart for budget share for", aggChoice, "\n")
    #     # MFAD -----
    #     cat("\nWorking on bar chart for MFAD for", i)
    #     dt <- aggNorder(gdxChoice, DTGlobal = "dt.MFAD", aggChoice, scenChoice = get(l), mergedVals)
    #     ylab <- "(percent)"
    #     if (ylab %in%  "(percent)") {yRangeMinMax.budgetshare <- c(0,100)} else {yRangeMinMax.budgetshare <- c(0, max(dt$value) )}
    #     plotByRegionBar(dt, fileName = "MFAD", plotTitle = "Modified Functional Attribute Diversity",
    #                     yLab = ylab, yRange = yRangeMinMax.budgetshare, aggChoice,  scenOrder, oneLine = FALSE, colorList)
    #   #  yLab = "(percent)", yRange = c(0, 100), aggChoice, oneLine = FALSE)
    # print(paste("Done with bar chart for MFAD for", i))
    
    # RAOqe -----
    cat("\nWorking on bar chart for Rao's QE for", aggChoice, "\n")
    DTGlobal <- getNewestVersion(paste("dt.RAOqe", suffix, sep = "."), fileloc("resultsDir"))
    dt <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    fileName <- paste(gdxChoice, "_",   "RAOqe", ".", suffix, sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    plotByRegionBar(dt, fileName, plotTitle = "Rao's quadratic entropy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE,
                    colorList, graphsListHolder, plotErrorBars)
    #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    # # food availability -----
    # cat("\nWorking on bar chart for food availability for", i))
    # foodAvail.out <- aggNorder(gdxChoice, DTGlobal = "dt.foodAvail.foodGroup", aggChoice, scenChoice = get(l), mergedVals)
    # plotByRegionBar(dt = foodAvail.out, fileName = "foodAvail.foodGroup", plotTitle = "Food availability by food group",
    #                 yLab = "(grams per day)", yRange = c(0, 100), aggChoice)
    
    # nonstaple share of kcals ------
    cat("\nWorking on bar chart for the nonstaple share of kcals for", suffix, "for", aggChoice, "\n")
    
    DTGlobal <- getNewestVersion(paste("dt.KcalShare_nonstaple", suffix, sep = "."), fileloc("resultsDir"))
    dt <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l),
                    mergedVals, plotErrorBars)
    ylab <- "(percent)"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    fileName = paste(gdxChoice, "_",   "nonStapleShare", ".", suffix, sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    plotByRegionBar(dt, fileName, plotTitle = "Non-staple share of energy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE,
                    colorList, graphsListHolder, plotErrorBars)
    
    # nutrition benefit score -----
    cat("\nWorking on bar chart for the NBS for", suffix, "for", aggChoice, "\n")
    DTGlobal <- getNewestVersion(paste("dt.nutBalScore", suffix, sep = "."), fileloc("resultsDir"))
    dt <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- "" # this creates the ylab variable and leaves it empty. NULL deletes it!
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    fileName <- paste(gdxChoice, "_",   "NutBalScore", ".", suffix,  sep = "")
    plotByRegionBar(dt, fileName, plotTitle = "Nutrient balance score",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars)
    #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    #   # composite QI score -----
    #   cat("\nWorking on bar chart for the QI composite for", i))
    #   dt <- aggNorder(gdxChoice, DTGlobal = "dt.compQI", aggChoice, scenChoice = get(l), mergedVals)
    #   ylab <- ""
    #   if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    #   plotByRegionBar(dt, fileName = "compQI", plotTitle = "Composite qualifying index",
    #                   yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder, oneLine = FALSE, colorList)
    # #  yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    # composite DI score ----- 
    cat("\nWorking on bar chart for disqualifying nutrients for", suffix, "for", aggChoice, "\n")
    fileName = paste(gdxChoice, "_",   "compDI", ".", suffix,  sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    DTGlobal <- getNewestVersion(paste("dt.compDI", suffix, sep = "."), fileloc("resultsDir")) # don't put aggChoice in here
    dt <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
    plotByRegionBar(dt, fileName, plotTitle = "Composite disqualifying index",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l),
                    oneLine = FALSE, colorList, graphsListHolder, plotErrorBars)
    #    yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)
    
    # total kcal stacked bar chart ----
    cat("\nWorking on stacked kcal bar chart for", suffix, "for", aggChoice, "\n")
    
    DTGlobal <- getNewestVersion(paste("dt.nutrients_kcals", suffix, sep = "."), fileloc("resultsDir"))
    # do stacked bar chart of sources of kcals
    dt <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l),
                    mergedVals =  c("scenario", "region_code", "year", "nutrient"), plotErrorBars) # I think nutrient might be ignored in aggNorder. July 17, 2018
    dt <- dt[nutrient %in% c("kcalsPerDay_carbohydrate", "kcalsPerDay_fat",  "kcalsPerDay_protein"), ] # removed "kcalsPerDay_other" from this list since they are all zero for the African countries
    dt[, nutrient := gsub("kcalsPerDay_", "", nutrient)]
    yLab <- "(Kcals)"
    if (yLab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, round(max(dt$value)) )}
    fileName = paste(gdxChoice, "_",   "kcals.values_bySource", ".", suffix, sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    plotByRegionStackedBar(dt, fileName, plotTitle = "Average daily dietary energy by source",
                           yLab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList, graphsListHolder = graphsListHolder)
    
    # do bar chart of total kcals by agg choice
    cat("\nWorking on total daily kcal bar chart for", suffix, "for", aggChoice, "\n")
    dt <- DTGlobal[nutrient %in% "kcalsPerDay_tot"]
    dt[, nutrient := NULL]
    dt <- aggNorder(gdxChoice, DTaggNorder = dt, aggChoice, scenChoice = get(l),  mergedVals, plotErrorBars)
    
    yLab <- "(Kcals)"
    yRangeMinMax <- c(0, round(max(dt$value)))
    fileName = paste(gdxChoice, "_",   "kcals.tot.perDay", ".", suffix, sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    plotByRegionBar(dt, fileName, plotTitle = "",
                    yLab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder = graphsListHolder, plotErrorBars)
    
    # food groups -----
    cat("\nWorking on bar chart for dt.foodAvail.foodGroup for", suffix, "for", aggChoice, "\n")
    
    DTGlobal <- getNewestVersion(paste("dt.foodAvail_foodGroup", suffix, sep = "."), fileloc("resultsDir"))
    
    foodGroupList <- sort(unique(DTGlobal$food_group_code))
    plotErrorBars <- chooseErrorBars(aggChoice)
    
    for (fg in foodGroupList) {
      units <- "grams per day"
      dt <- data.table::copy(DTGlobal)
      dt <- dt[food_group_code %in% fg,]
      dt[, food_group_code := NULL]
      dt <- aggNorder(gdxChoice, DTaggNorder = dt, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
      
      fg.longName <- cleanupNutrientNames(fg)
      nutTitle <- paste(tolower(fg.longName), sep = "")
      ylab = paste("(",units,")",sep = "")
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(dt$value) )}
      fileName = paste(gdxChoice, "_",   "foodAvail_foodGroup", "_", fg, ".", suffix, sep = "")
      fileNameHolder <- append(fileNameHolder, fileName)
      plotByRegionBar(dt, fileName, plotTitle = nutTitle,
                      yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix, scenOrder = get(l), oneLine = FALSE, colorList,
                      graphsListHolder = graphsListHolder, plotErrorBars)
      #      yRange = c(0, 80), aggChoice, oneLine = FALSE)
    }
    
    cat("\nDone with food groups --------------------\n")
    
    # multiple nutrients loop -----
    for (k in 1:length(multipleNutsFileList)) {
      cat("\nWorking on multiple nut file", multipleNutsFileList[k], " for", suffix, "for", aggChoice) #, "\n"
      cat("\nk is ", k, "\n")
      temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
      #     temp.in <- temp.in[nutrient %in% keepListNuts,]
      plotErrorBars <- chooseErrorBars(aggChoice)
      
      nutsToKeep <- unique(temp.in$nutrient)[!unique(temp.in$nutrient) %in%
                                               c("caffeine_mg", "cholesterol_mg", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g",
                                                 "kcals.ft_acds_tot_sat_g", "kcals.protein_g", "kcals.sugar_g", "kcalsPerDay_carbohydrate", "kcalsPerDay_ethanol", 
                                                 "kcalsPerDay_fat", "kcalsPerDay_ft_acds_tot_sat",
                                                 "kcalsPerDay_other", "kcalsPerDay_protein", "kcalsPerDay_sugar", "kcalsPerDay_tot")]
      for (nut in nutsToKeep) {
        nutshortName <- cleanupNutrientNames(nut)
        nutlongName <- dt.nutrientNames_Units[1, (nut)]
        units <- dt.nutrientNames_Units[2, (nut)]
        if (units %in% "g") units <- "grams per day" 
        dt <- temp.in[nutrient %in% nut,]
        dt[, nutrient := NULL]
        dt <- aggNorder(gdxChoice, DTaggNorder = dt, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
        DTmax <- max(dt$value)
        # cat("\nDTmax:", DTmax)
        
        #' create file specific name and related info
        if (multipleNutsFileList[k] %in% paste("dt.nutrients_sum_all", suffix, sep = "."))  {
          #         nutTitle <- paste("Average daily availability of ", tolower(nutlongName), sep = "")
          nutTitle <- paste(tolower(nutlongName), sep = "")
          ylab = paste("(",units,")",sep = "")
          drawOneLine = FALSE
          yRangeMinMax <- c(0, max(dt$value) + 0.02 * DTmax )
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
          yRangeMinMax <- c(0, max(dt$value) + 0.02 * DTmax )
          drawOneLine = FALSE
        }
        
        fileName = paste(gdxChoice, "_",   multipleNutsListShortName[k], "_", nut, ".", suffix, sep = "")
        fileNameHolder <- append(fileNameHolder, fileName)
        cat("fileName is ", fileName,"\n")
        #         print(summary(DT))
        plotByRegionBar(dt, fileName,
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix,
                        scenOrder = get(l), oneLine = drawOneLine, colorList, graphsListHolder = graphsListHolder, plotErrorBars)
      }
    }
    
    cat("\nDone with ", multipleNutsFileList[k], " ----------------")
  }
}
cat("\nDone with scenChoiceList loop ----------")

#' box plot of budget share in 2050  ----
DTGlobal <- getNewestVersion(paste("dt.budgetShare", sep = "."), fileloc("resultsDir"))
#' get rid of Somalia because it's budget share is 500 * per cap income
DTGlobal <- DTGlobal[!region_code.IMPACT159 == "SOM",]
dt <- data.table::copy(DTGlobal)
scenChoice <- scenario.base.CC # this is the 2050 value for this scenario

ylab <- "(percent)"
plottitle = "Food budget share of 2050 per capita income" 
#plottitle = ""
#' statement below excludes regions.AfricanAgFutures, which is not relevant for a box plot
for (aggChoice in aggChoiceListBarChart[!aggChoiceListBarChart %in% "regions.AfricanAgFutures"]) {
  dt <- aggNorder(gdxChoice, DTaggNorder = dt, aggChoice, scenChoice, mergedVals, plotErrorBars)
  plotErrorBars = FALSE # not relevant for a boxplot
  #' aggregate to and retain only the relevant regions
  fileName <- paste(gdxChoice, "_",   "budgetShareBoxPlot_2050", ".", suffix,  sep = "")
  plotByBoxPlot2050(dt, fileName, plotTitle = plottitle, yLab = ylab, yRange = yRangeMinMax.budgetshare, aggChoice, suffix, graphsListHolder = graphsListHolder)
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
DT.master <- getNewestVersion(paste("food_agg_AMDR_hi", suffix, sep = "."), fileloc("resultsDir")) # both hi and lo have the same values
AMDRNuts <- c("carbohydrate_g.kcalpercent", "fat_g.kcalpercent", "protein_g.kcalpercent")
# for (macroNut in c("kcalsPerDay_fat_share", "kcalsPerDay_protein_share", "kcalsPerDay_carbohydrate_share")) {
for (macroNut in AMDRNuts) {
  cat("\nWorking on AMDRhilo for", suffix, "for", macroNut, "\n")
  if (macroNut %in%  "fat_g.kcalpercent") nutName <- "fat_g"
  if (macroNut %in%  "protein_g.kcalpercent") nutName <- "protein_g"
  if (macroNut %in%  "carbohydrate_g.kcalpercent") nutName <- "carbohydrate_g"
  
  for (aggChoice in aggChoiceListBarChart) {
    plotErrorBars <- chooseErrorBars(aggChoice)
    dt <- data.table::copy(DT.master)
    keepListCol <- c("scenario", "region_code.IMPACT159", "year", macroNut)
    dt[, setdiff(names(dt), keepListCol) := NULL]
    dt[, value := get(macroNut)]
    dt <- aggNorder(gdxChoice, DTaggNorder = dt, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
    
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
    
    fileName = paste(gdxChoice, "_",   "AMDRShare", "_", nutName, ".", suffix, sep = "")
    fileNameHolder <- append(fileNameHolder, fileName)
    nutTitle <- capwords(cleanupNutrientNames(nutName))
    print("nutTitle")
    print(nutTitle)
    plotByRegionBarAMDR(dt, fileName,
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice, suffix,
                        scenOrder = get(l), colorList, AMDR_lo, AMDR_hi, graphsListHolder = graphsListHolder, plotErrorBars)
  }
}

# facet maps for the world  -----
projmap <- getNewestVersion("worldMap", fileloc("uData")) # run storeWorldMapDF() if this is not available
if (gdxChoice %in% "AfricanAgFutures") projmap <- getNewestVersion("africaMap", fileloc("uData")) # run storeWorldMapDF() if this is not available
# 2050 budget share map
cat("\nWorking on budget share facet maps. \n") # Just do it once, using var suffix
if (suffix %in% "var") {
  DTGlobal <- getNewestVersion(paste("dt.budgetShare", sep = "."), fileloc("resultsDir"))
  dt <- DTGlobal[year == "X2010" & scenario %in% gsub("-", "_", scenario.base.CC) |
                   year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]
  dt <- countryCodeCleanup(dt) # converts IMPACT region codes to ISO3 codes for largest country in the region
  data.table::setnames(dt, old = "region_code.IMPACT159", new = "id")
  keepListCol <- c("scenario", "id",  "incShare.PCX0" )
  dt[, setdiff(names(dt), keepListCol) := NULL]
  dt <- dt[!id %in% "SOM"][, value := incShare.PCX0][, incShare.PCX0 := NULL]
  dt <- unique(dt)
  
  # do map
  dt <- dt[scenario %in% get(l),]
  scenOrder <- get(l)
  dt <- unique(dt)
  dt[, scenario := factor(scenario, levels = scenOrder)]
  dt[, scenario := gsub("X", "", scenario)]
  facetColName <- "scenario"
  legendText <- "Food expenditure share of income"
  fillLimits <- c(0, 70)
  temp <- truncateDT(dt, fillLimits = fillLimits)
  temp <- temp[id %in% allAfricaCodes,]
  #   numLimits <- 4
  #    breakValues <- generateBreakValues(fillLimits = fillLimits, numLimits = numLimits, decimals = 0)
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType))) # Reds can only have a maximum of 9 values
  palette <- myPalette(4)
  displayOrder <- get(l)
  fileName <- paste(gdxChoice, "_",  "facetmap", "_", "budgetShare", sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName,
            graphsListHolder = graphsListHolder, displayOrder, width = 5)
}

# facet map, food availability by food groups -----
cat("\nWorking on food availability by selected food groups facet maps for", suffix, "\n")
DTGlobal <- getNewestVersion(paste("dt.foodAvail_foodGroup", suffix, sep = "."), fileloc("resultsDir"))
deleteListRow <- c("alcohol", "beverages", "fish", "meats", "oils", "dairy", "eggs", "nutsNseeds") # remove these food groups
DTGlobal <- DTGlobal[!food_group_code %in% deleteListRow,]
DTGlobal[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
dt <- DTGlobal[year == "X2010" & scenario %in% gsub("-", "_", "Reference") |
                 year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

dt <- countryCodeCleanup(dt) # converts IMPACT region codes to ISO3 codes for largest country in the region
data.table::setnames(dt, old = "region_code.IMPACT159", new = "id")
dt[food_group_code %in% "nutsNseeds", food_group_code := "nuts and oilseeds"]
dt[food_group_code %in% "rootsNPlantain", food_group_code := "roots and plantain"]

#dt is the starting point. Need temp versions for each of the facet maps
# facet map, availability in quantity terms 2050, no CC -----

# facetColName <- "food_group_code"
# legendText <- "Grams per day, 2050, \nno climate change"
# fillLimits <- c(0, 1000)
# temp <- truncateDT(dt, fillLimits = fillLimits)
# temp <- temp[id %in% allAfricaCodes,]
# temp <- temp[scenario %in% scenario.base.NoCC,]
# paletteType <- "Spectral"
# myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
# palette <- myPalette(4)
# displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
# fileName <- paste(gdxChoice, "_",  "facetmap", "_", "FGAvail", "_", "2050", "_", "noCC", ".", suffix, sep = "")
# facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder,  displayOrder)

# convert to wide to do operations aross scenarios for the food availability deltas
formula.wide <- "id + food_group_code ~ scenario"
DT.wide <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "value")

# commented out Nov 21, 2018 because no climate change scenarios not included
#facet map, climate change effect on food availability -----
# DT.wide[, value := 100 * (get(scenario.base.CC) - get(scenario.base.NoCC)) / get(scenario.base.NoCC)] 
# facetColName <- "food_group_code"
# legendText <- "Climate change effect on\navailability in 2050 (percent)"
# fillLimits <- c(-14, 1)
# temp <- truncateDT(DT.wide, fillLimits = fillLimits)
# temp <- temp[id %in% allAfricaCodes,]
# keepListCol <- c("id", facetColName, "value")
# temp[, setdiff(names(temp), keepListCol) := NULL]
# paletteType <- "Reds"
# myPalette <- colorRampPalette(rev(brewer.pal(9, paletteType)))
# palette <- myPalette(4)
# displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
# fileName <- paste(gdxChoice, "_",  "facetmap", "_", "FGAvailChange", "_", "climate", ".", suffix, sep = "")
# facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

# # increase in food availability due to income growth -----
# DT.wide[, value := 100 * (get(scenario.base.NoCC) - X2010) / X2010]
# facetColName <- "food_group_code"
# legendText <- "Income growth effect on availability\nno climate change, \n2010-2050, (percent)"
# fillLimits <- c(-30, 100)
# temp <- truncateDT(DT.wide, fillLimits = fillLimits)
# temp <- temp[id %in% allAfricaCodes,]
# paletteType <- "Spectral"
# myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
# palette <- myPalette(4)
# displayOrder <- sort(unique(temp[, get(facetColName)])) # default - alphabetically sorted
# fileName <- paste(gdxChoice, "_",  "facetmap", "_", "FGAvailChange", "_", "income", ".", suffix, sep = "")
# facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

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
DT.macro <- getNewestVersion(paste("reqRatio_sum_RDA_macro", suffix, sep = "."), fileloc("resultsDir"))
DT.vits <- getNewestVersion(paste("reqRatio_sum_RDA_vits", suffix, sep = "."), fileloc("resultsDir"))
DT.minrls <- getNewestVersion(paste("reqRatio_sum_RDA_minrls", suffix, sep = "."), fileloc("resultsDir"))
dt <- do.call("rbind", list(DT.macro, DT.vits, DT.minrls))
dt <- countryCodeCleanup(dt) # converts IMPACT region codes to ISO3 codes for largest country in the region

dt <- dt[nutrient %in% keepListNuts,]
dt <- dt[year == "X2010" & scenario %in% gsub("-", "_", scenario.base.CC) |
           year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

dt <- countryCodeCleanup(dt) # converts IMPACT region codes to ISO3 codes for largest country in the region

# nutListtemp = sort(unique(dt$nutrient))
# dt.nuts <- data.table(nutListtemp = nutListtemp, nutListtempclean = capwords(cleanupNutrientNames(nutListtemp))) # use to get clean spelling of nutrient
dt.nuts <- data.table(nutListtemp = keepListNuts, nutListtempclean = keepListNutsNames)

dt <- merge(dt, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
dt[, nutrient := NULL]
setnames(dt, old = "nutListtempclean", new = "nutrient")

data.table::setnames(dt, old = "region_code.IMPACT159", new = "id")

#' adequacy 2010 -----
facetColName <- "nutrient"
legendText <- "Adequacy ratio, 2010"
fillLimits <- c(0, 3) # changed from 5 May 29, 2018
temp <- truncateDT(dt, fillLimits =  fillLimits)
temp <- temp[id %in% allAfricaCodes,]
temp <- temp[scenario %in% "X2010",]
paletteType <- "Spectral"
myPalette <- colorRampPalette(brewer.pal(11, paletteType))
palette <- myPalette(4)
displayOrder <- keepListNutsNames
fileName <- paste(gdxChoice, "_",  "facetmap", "_", "nutReqRatio", "_", "2010", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

# # adequacy 2050, no CC -----
# facetColName <- "nutrient"
# legendText <- "Adequacy ratio, 2050, \nno climate change"
# fillLimits <- c(0, 3) # changed from 5, May 29, 2018
# temp <- truncateDT(dt, fillLimits =  fillLimits)
# temp <- temp[id %in% allAfricaCodes,]
# temp <- temp[scenario %in% scenario.base.NoCC,]
# paletteType <- "Spectral"
# myPalette <- colorRampPalette(brewer.pal(11, paletteType))
# palette <- myPalette(4)
# displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
# fileName <- paste(gdxChoice, "_",  "facetmap", "_", "nutReqRatio", "_", "2050", "_", "noCC", ".", suffix, sep = "")
# facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

# adequacy 2050,  CC -----
facetColName <- "nutrient"
fillLimits <- c(0, 3) # changed from 5, May 29, 2018
paletteType <- "Spectral"
myPalette <- colorRampPalette(brewer.pal(11, paletteType))
palette <- myPalette(4)
displayOrder <- keepListNutsNames
for (i in scenarioListIMPACT[!scenarioListIMPACT %in% "X2010"]) {
  temp <- truncateDT(dt, fillLimits)
  legendText <- paste ("Adequacy ratio, 2050,", i, "scenario", sep = " ")
  temp <- temp[scenario %in% i,]
  fileName <- paste(gdxChoice, "_", l, "_", "facetmap", "_", "nutReqRatio", "_", "2050", "_", "CC", ".", i, sep = "")
  facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, displayOrder)
}

# convert to wide to do operations aross scenarios for 2050
formula.wide <- "id + nutrient ~ scenario"
DT.wide <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "value")

#facet map, climate change effect on adequacy in 2050 -----
DT.wide[, value := 100 * ( get(scenario.base.CC) - get(scenario.base.NoCC)) / get(scenario.base.NoCC)]  
facetColName <- "nutrient"
legendText <- "Climate change effect on\nadequacy, 2050, (percent)"
fillLimits <- c(-10, 2)
temp <- truncateDT(DT.wide, fillLimits = fillLimits)
temp <- temp[id %in% allAfricaCodes,]
paletteType <- "Spectral"
myPalette <- colorRampPalette(brewer.pal(11, paletteType))
palette <- myPalette(4)
displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
fileName <- paste(gdxChoice, "_",  "facetmap", "_", "nutReqRatioChange", "_", "climate", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

#facet map, income growth (2010 to 2050) effect on adequacy
DT.wide[, value := 100 * (get(scenario.base.NoCC) - X2010) / X2010]
facetColName <- "nutrient"
legendText <- "Income growth effect on adequacy (no climate change), \n2010-2050, (percent)"
fillLimits <- c(0, 100) # changed from 8 to 0 July 7, 2018
temp <- truncateDT(DT.wide, fillLimits)
temp <- temp[id %in% allAfricaCodes,]
myPalette <- colorRampPalette(brewer.pal(9, paletteType))
paletteType <- "Blues"
palette <- myPalette(4)
#  breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
fileName <- paste(gdxChoice, "_",  "facetmap", "_", "nutReqRatioChange", "_", "income", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

# facet map, MRV ratio results -----
cat("\nWorking on MRV ratio facet maps for", suffix, "\n")
dt <- getNewestVersion(paste("dt.MRVRatios", suffix, sep = "."), fileloc("resultsDir"))
keepListNuts <- c("ft_acds_tot_sat_g", "sugar_g")
dt <- dt[nutrient %in% keepListNuts,]
dt[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
dt <- dt[year == "X2010" & scenario %in% gsub("-", "_", scenario.base.CC) |
           year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

dt <- countryCodeCleanup(dt) # converts IMPACT region codes to ISO3 codes for largest country in the region
nutListtemp = sort(unique(dt$nutrient))
dt.nuts <- data.table(nutListtemp = nutListtemp, nutListtempclean = capwords(cleanupNutrientNames(nutListtemp)))
dt <- merge(dt, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
dt[, nutrient := NULL]
setnames(dt, old = "nutListtempclean", new = "nutrient")
data.table::setnames(dt, old = "region_code.IMPACT159", new = "id")

#' disqualifying index ratio 2010 -----
facetColName <- "nutrient"
legendText <- "disqualifying index ratio, 2010"
fillLimits <- c(0, 4)
temp <- truncateDT(dt, fillLimits =  fillLimits)
temp <- temp[id %in% allAfricaCodes,]
temp <- temp[scenario %in% "X2010",]
paletteType <- "Spectral"
#  breakValues <- scales::rescale(c(breakValue.low, 1, 2, breakValue.high))
myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
palette <- myPalette(4)
displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
fileName <- paste(gdxChoice, "_",  "facetmap", "_", "MRVRatio", "_", "2010", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder, height = 3)

# convert to wide to do operations aross scenarios
formula.wide <- "id + nutrient ~ scenario"
DT.wide <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "value")

# disqualifying index ratio 2050, no CC -----
cat("\nWorking on disqualifying index ratio facet maps for", suffix, "\n")
facetColName <- "nutrient"
legendText <- "disqualifying index ratio, \nno climate change"
fillLimits <- c(0, 4)
temp <- temp[scenario %in% scenario.base.NoCC,]
temp <- truncateDT(temp, fillLimits)
temp <- temp[id %in% allAfricaCodes,]
paletteType <- "Spectral"
myPalette <- colorRampPalette(brewer.pal(11, paletteType))
palette <- myPalette(4)
displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
fileName <- paste(gdxChoice, "_",  "facetmap", "_", "MRVRatio", "_","2050", "_", "noCC", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder, height = 3)

#facet map, climate change effect on disqualifying index ratio -----
DT.wide[, value := 100 * ( get(scenario.base.CC) - get(scenario.base.NoCC)) / get(scenario.base.NoCC)]
facetColName <- "nutrient"
legendText <- "Climate change effect on the\ndisqualifying index, 2050, (percent)"
fillLimits <- c(-3, 6)
temp <- truncateDT(DT.wide, fillLimits = fillLimits)
temp <- temp[id %in% allAfricaCodes,]
paletteType <- "Spectral"
myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
palette <- myPalette(4)
displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
fileName <- paste(gdxChoice, "_",  "facetmap", "_", "MRVRatioChange", "_", "climate", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder, height = 3)

# increase in disqualifying index ratio due to income growth -----
DT.wide[, value := 100 * (get(scenario.base.NoCC) - X2010) / X2010]
facetColName <- "nutrient"
legendText <- "Income growth effect on disqualifying index ratio, \n2010-2050, (percent)"
fillLimits <- c(-10, 100)
temp <- truncateDT(DT.wide, fillLimits)
temp <- temp[id %in% allAfricaCodes,]
paletteType <- "Reds"
myPalette <- colorRampPalette(brewer.pal(9, paletteType))
palette <- myPalette(4)
displayOrder <- capwords(cleanupNutrientNames(keepListNuts))
fileName <- paste(gdxChoice, "_", l, "_facetmap_", "MRVRatioChange", "_", "income", ".", suffix, sep = "")
facetMaps(mapFile = projmap, DTfacetMap = temp, fileName, legendText, fillLimits = fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)

#create  xlsx file with the data used to create the  WB figures -----
rowCounter <- 1
aggChoice <- "regions.AfricanAgFutures"
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

# not needed since not doing WB
# incCats <- c("lowInc", "lowMidInc", "upMidInc", "highInc")
# DincCats <- c("DlowInc", "DlowMidInc", "DupMidInc", "DhighInc")

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
    filename <- paste(gdxChoice, "_",  figtype, ".", suffix, sep = "")
    figInfo <- "4, affordability,"
  }
  
  if (figtype %in% nutlistmacro) {
    filename <- paste(gdxChoice, "_",  "reqRatio_macro", "_", figtype, ".", suffix, sep = "")
    figInfo <- "5, adequacy, macro nutrients, "
  }
  if (figtype %in% nutlistmacroAMDRShare) {
    filename <- paste(gdxChoice, "_",  figtype, "_", aggChoice, ".", suffix, sep = "")
    #       filename <- gsub("AMDRShare", "_", filename) # a kludge to deal with the fact that these are macronutrients
    figInfo <- "6, Share of total kilocalories, "
  }
  if (figtype %in% minerals) {
    filename <- paste(gdxChoice, "_",  "reqRatio_minrls",  "_", figtype, ".", suffix, sep = "")
    figInfo <- "7, adequacy, minerals, "
  }
  if (figtype %in% vitamins) {
    filename <- paste(gdxChoice, "_",   "reqRatio_vits",  "_", figtype, ".", suffix, sep = "")
    figInfo <- "7, adequacy, vitamins, "
  }
  if (figtype %in% nutBal) {
    filename <- paste(gdxChoice, "_",   figtype, ".", suffix, sep = "")
    figInfo <- "8, Nutrient balance metrics, "
  }
  if (figtype %in% diversity) {
    filename <- paste(gdxChoice, "_",   figtype, ".", suffix, sep = "")
    figInfo <- "9, diversity metrics, "
  }
  
  # moved to supplementary information
  if (figtype %in% dailyAvail.foodgroup) {
    filename <- paste(gdxChoice, "_",   "foodAvail_foodGroup",  "_", figtype, ".", suffix, sep = "")
    figInfo <- "S1, food group daily availability (g), "
  }
  # moved to supplementary information.
  if (figtype %in% energy) {
    filename <- paste(gdxChoice, "_",   "kcals.tot.perDay", suffix, sep = "")
    figInfo <- "S2, average daily availability dietary energy (kcals), "
  }
  
  # not needed perhaps. October 15, 2018
  # fileIn <- data.table::fread(paste(fileloc("gDir"), "/", filename, ".csv", sep = ""), select = 2:6)
  # cat("fileIn:", filename)
  # for (j in scen2050list) {
  #   # inCats is income categories
  #   for (k in 1:length(incCats)) {
  #     baseVal <- fileIn[scenario == gsub("-REF", "", scenario.base), get(incCats[k])]
  #     fileIn[scenario %in% j, DincCats[k] := (get(incCats[k]) - baseVal) / baseVal]
  #   }
  # }
  #write name of data set to the spreadsheet
  # nutshortName <- cleanupNutrientNames(figtype)
  # openxlsx::writeData(
  #   wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = FALSE, startCol = 1,
  #   x = paste("Figure", figInfo, nutshortName),
  #   startRow = rowCounter
  # )
  # rowCounter <- rowCounter + 1
  # cat("rowCounter: ", rowCounter, "\n")
  # openxlsx::writeData(
  #   wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = FALSE, startCol = 1,
  #   x = fileIn,
  #   startRow = rowCounter
  # )
  # rowCounter <- rowCounter + nrow(fileIn)
  # cat("rowCounter: ", rowCounter, "\n")
  # 
  # category <- data.table::data.table(scenario = figtype,
  #                                    lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
  #                                    DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  # csvHolder <- rbind(csvHolder, category)
  # csvHolder <- rbind(csvHolder, fileIn)
  # 
  # # data.table::setnames(csvHolder,
  # #                      old = names(csvHolder),
  # #                      new = colHeaders)
  
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
  for (aggChoice in c("regions.AfricanAgFutures")) {
    #      for (aggChoice in c("AggReg1", "WB")) { commented out Mar 18, 2018
    dt <- getNewestVersion(paste(fname, suffix, sep = "."), fileloc("resultsDir"))
    dt <- merge(dt, dt.pop, by = c("scenario","region_code.IMPACT159", "year")) ### where does dt.pop come from? It is loaded in aggNorder.R
    dt.regions <- regionAgg(aggChoice)
    # aggregate to and retain only the relevant regions; region code is the code for the region
    merged <- merge(dt, dt.regions, by = "region_code.IMPACT159")
    
    merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]
    keepListCol <- c("scenario", "region_code", "region_name", "year", "nutrient", "value")
    merged[, setdiff(names(merged), keepListCol) := NULL]
    merged <- unique(merged)
    indt <- merged
    outName <- paste(fname, aggChoice, suffix, sep = ".")
    desc <- paste0("Aggregate RDA req ratios from data file ", fname)
    cleanup(indt, outName, destDir = fileloc("resultsDir"), desc = desc)
  }
}

# save graph files for future use
inDT <- graphsListHolder
outName <- paste("graphsListHolder", suffix, sep = ".")
desc <- paste0("File with graphics created for presentation with ", suffix, " nutrient data" )
cleanupGraphFiles(inDT, outName, fileloc("gDir"), desc = desc)

finalizeScriptMetadata(metadataDT, sourceFile)
#sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
