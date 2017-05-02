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
library(RColorBrewer)
# next 3 libraries needed for world maps
library(sp)
library(broom)
library(rgdal)

# gdxChoice values are either SSPs or USAID
dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
gdxChoice <- getGdxChoice()
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA_macro.sum.reqRatio, RDA_minrls.sum.reqRatio, RDA_vits.sum.reqRatio
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

# create list variable to hold ggplot output
graphsListHolder <- list()

# delete all old files, pdf, csv, gz, and zip
graphicsPath <- fileloc("gDir")
graphicsFileList <- list.files(fileloc("gDir"))
filesToRemove.pdf <- paste(graphicsPath, graphicsFileList[grep(".pdf", graphicsFileList)], sep = "/")
filesToRemove.csv <- paste(graphicsPath, graphicsFileList[grep(".csv", graphicsFileList)], sep = "/")
filesToRemove.gz <- paste(graphicsPath, graphicsFileList[grep(".gz", graphicsFileList)], sep = "/")
filesToRemove.zip <- paste(graphicsPath, graphicsFileList[grep(".zip", graphicsFileList)], sep = "/")

#unlink works like file.remove but with fewer messages
invisible(unlink(filesToRemove.pdf, recursive = FALSE))
invisible(unlink(filesToRemove.csv, recursive = FALSE))
invisible(unlink(filesToRemove.gz, recursive = FALSE))
invisible(unlink(filesToRemove.zip, recursive = FALSE))

multipleNutsFileList <- c("dt.nutrients.sum.all",
                          "RDA.macro_sum_reqRatio",
                          "RDA.minrls_sum_reqRatio",
                          "RDA.vits_sum_reqRatio",
                          "dt.nutrients.nonstapleShare",
                          "dt.MRVRatios")
 #                         "AMDR_hi_sum_reqRatio",
 #                         "AMDR_lo_sum_reqRatio")
#                          "dt.foodAvail.foodGroup")
multipleNutsListShortName <- c("nutrients.avail",
                               "macro_reqRatio",
                               "minrls_reqRatio",
                               "vits_reqRatio",
                               "nutrients.nonstaples.share",
                               # "zinc_bioavail_reqRatio",
                               # "iron_bioavail_reqRatio",
                               "badRatios")
                               # "AMDR_hi",
                               # "AMDR_lo")
#                               "foodAvail.foodGroup")

#nutrients grouping
macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")
other <- c("sugar_g", "cholesterol_mg")

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
  aggChoiceListBarChart <- c("AggReg1")

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
  paletteChoice <- "OrRd" #choices are described in the help for RcolorBrewer

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
    if (aggChoice == "tenregions") plotErrorBars = FALSE
    if (aggChoice == "WB") plotErrorBars = TRUE
    if (aggChoice == "AggReg1") plotErrorBars = TRUE
    #   generate the legends -----
    #' needs to be inside l and aggChoice loop -----
    for (legendOrient in c("bottom", "right")) {
      legendname <- paste("legend", legendOrient, gdxChoice, l, aggChoice, sep = "_")
      graphsListHolder[[legendname]] <- updateLegendGrobs(l, aggChoice, legendOrient, mergedVals)
    }
    #    # Shannon Diversity -----
    #    print(paste("Working on bar chart for Shannon Diversity for", i))
    #    DT <- aggNorder(gdxChoice, DTglobal = "dt.shannonDiversity", aggChoice, scenChoice = get(l), mergedVals)
    #    ylab <- "(percent)"
    #    filename = "ShannonDiversity"
    #    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    #    plotByRegionBar(dt = DT, fileName = filename, plotTitle = "Shannon Diversity", yLab = ylab,
    #                    yRange = yRangeMinMax, aggChoice, scenOrder, oneLine = FALSE, colorList)
    # #   yRange = c(0, 80), aggChoice, oneLine = FALSE)

    # Budget share -----
    cat(paste("\n\nWorking on bar chart for budget share for", aggChoice))
    DTglobal <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals)
    ylab <- "(percent)"
    fileName <- paste(gdxChoice, l, "budgetShare", aggChoice, sep = "_")
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    yRangeMinMax <- c(0,60) #custom for budgetShare
    plotByRegionBar(dt = DT, fileName, plotTitle = "Food budget share of per capita income",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars)
    #  yLab = "(percent)", yRange = c(0, 50), aggChoice, oneLine = FALSE)

    cat(paste("\nDone with bar chart for budget share for", aggChoice))
    #     # MFAD -----
    #     print(paste("Working on bar chart for MFAD for", i))
    #     DT <- aggNorder(gdxChoice, DTglobal = "dt.MFAD", aggChoice, scenChoice = get(l), mergedVals)
    #     ylab <- "(percent)"
    #     if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    #     plotByRegionBar(dt = DT, fileName = "MFAD", plotTitle = "Modified Functional Attribute Diversity",
    #                     yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder, oneLine = FALSE, colorList)
    #   #  yLab = "(percent)", yRange = c(0, 100), aggChoice, oneLine = FALSE)
    # print(paste("Done with bar chart for MFAD for", i))
    # RAOqe -----
    cat(paste("\n\nWorking on bar chart for Rao's QE for", aggChoice))
    DTglobal <- getNewestVersion("dt.RAOqe", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals)
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    fileName <- paste(gdxChoice, l, "RAOqe", aggChoice, sep = "_")
    plotByRegionBar(dt = DT, fileName, plotTitle = "Rao's quadratic entropy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE,
                    colorList, graphsListHolder, plotErrorBars)
    #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

    # # food availability -----
    # print(paste("Working on bar chart for food availability for", i))
    # foodAvail.out <- aggNorder(gdxChoice, DTglobal = "dt.foodAvail.foodGroup", aggChoice, scenChoice = get(l), mergedVals)
    # plotByRegionBar(dt = foodAvail.out, fileName = "foodAvail.foodGroup", plotTitle = "Food availability by food group",
    #                 yLab = "(grams)", yRange = c(0, 100), aggChoice)

    # nonstaple share of kcals ------
    cat(paste("\n\nWorking on bar chart for the nonstaple share of kcals for", aggChoice))
    DTglobal <- getNewestVersion("dt.KcalShare.nonstaple", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l),
                    mergedVals =  c("scenario", "region_code", "year", "staple_code"))
    ylab <- "(percent)"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    fileName = paste(gdxChoice, l, "nonStapleShare", aggChoice, sep = "_")
    plotByRegionBar(dt = DT, fileName, plotTitle = "Non-staple share of energy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE,
                    colorList, graphsListHolder, plotErrorBars)
    #   yLab = "(percent)", yRange = c(0, 100), aggChoice, oneLine = FALSE)

    # nutrition benefit score -----
    cat(paste("\n\nWorking on bar chart for the NBS for", aggChoice))
    DTglobal <- getNewestVersion("dt.nutBalScore", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals)
    ylab <- "" # this creates the ylab variable and leaves it empty. NULL deletes it!
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    fileName <- paste(gdxChoice, l, "NutBalScore", aggChoice,  sep = "_")
    plotByRegionBar(dt = DT, fileName, plotTitle = "Nutrient balance score",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE, colorList,
                    graphsListHolder, plotErrorBars)
    #   yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

    #   # composite QI score -----
    #   print(paste("Working on bar chart for the QI composite for", i))
    #   DT <- aggNorder(gdxChoice, DTglobal = "dt.compQI", aggChoice, scenChoice = get(l), mergedVals)
    #   ylab <- ""
    #   if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    #   plotByRegionBar(dt = DT, fileName = "compQI", plotTitle = "Composite qualifying index",
    #                   yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder, oneLine = FALSE, colorList)
    # #  yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

    # composite DI score -----
    cat(paste("\n\nWorking on bar chart for disqualifying nutrients for", aggChoice))
    fileName = paste(gdxChoice, l, "compDI", aggChoice,  sep = "_")
    DTglobal <- getNewestVersion("dt.compDI", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals)
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName, plotTitle = "Composite disqualifying index",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice, scenOrder = get(l),
                    oneLine = FALSE, colorList, graphsListHolder, plotErrorBars)
    #    yLab = NULL, yRange = c(0, 100), aggChoice, oneLine = FALSE)

    # stacked kcal bar chart ----
    cat(paste("\n\nWorking on stacked kcal bar chart for ", aggChoice))
    DTglobal <- getNewestVersion("dt.kcals.values", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l),
                    mergedVals =  c("scenario", "region_code", "year", "nutrient"))
    DT <- DT[nutrient %in% c("kcalsPerDay.carbohydrate", "kcalsPerDay.fat", "kcalsPerDay.other", "kcalsPerDay.protein"), ]
    yLab <- "(Kcals)"
    if (yLab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, round(max(DT$value)) )}
    fileName = paste(gdxChoice, l, "kcals.values", sep = "_")
    plotByRegionStackedBar(dt = DT, fileName, plotTitle = "Average daily dietary energy by source",
                           yLab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE, colorList)

    # food groups -----
    cat(paste("\n\nWorking on bar chart for dt.foodAvail.foodGroup for", aggChoice))
    temp.in <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))
    #  temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

    #keep just the scenario.base scenario for 2010 and rename the scenario to 2010. Now done in aggNorder
    # temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
    #                      year == "X2050",][year == "X2010", scenario := "2010"]
    foodGroupList <- unique(temp.in$food_group_code)
    if (aggChoice == "tenregions") plotErrorBars = FALSE
    if (aggChoice == "WB") plotErrorBars = TRUE
    if (aggChoice == "AggReg1") plotErrorBars = TRUE

    for (fg in foodGroupList) {
      units <- "grams"
      DT <- data.table::copy(temp.in)
      DT <- DT[food_group_code %in% fg,]
      DT[, food_group_code := NULL]
      DT <- aggNorder(gdxChoice, DTglobal = DT, aggChoice, scenChoice = get(l), mergedVals)

      fg.longName <- cleanupNutrientNames(fg)
      nutTitle <- paste(tolower(fg.longName), sep = "")
      ylab = paste("(",units,")",sep = "")
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      fileName = paste(gdxChoice, l, "foodAvail_foodGroup", fg, aggChoice, sep = "_")
      plotByRegionBar(dt = DT, fileName, plotTitle = nutTitle,
                      yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE, colorList,
                      graphsListHolder, plotErrorBars)
      #      yRange = c(0, 80), aggChoice, oneLine = FALSE)
    }

    cat(paste("\nDone with food groups --------------------\n\n"))

    # multiple nutrients loop -----
    for (k in 1:length(multipleNutsFileList)) {
      cat(paste("\n\nWorking on multiple nut file", multipleNutsFileList[k]), " for ", aggChoice)
      temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
 #     temp.in <- temp.in[nutrient %in% keepListNuts,]
      if (aggChoice == "tenregions") plotErrorBars = FALSE
      if (aggChoice == "WB") plotErrorBars = TRUE
      if (aggChoice == "AggReg1") plotErrorBars = TRUE

      nutsToKeep <- unique(temp.in$nutrient)[!unique(temp.in$nutrient) %in%
             c("caffeine_mg", "cholesterol_mg", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g" )]
      for (nut in nutsToKeep) {
        nutshortName <- cleanupNutrientNames(nut)
        nutlongName <- dt.nutrientNames_Units[1, (nut)]
        units <- dt.nutrientNames_Units[2, (nut)]
        if (units %in% "g") units <- "grams"
        DT <- temp.in[nutrient %in% nut,]
        DT[, nutrient := NULL]
        DT <- aggNorder(gdxChoice, DTglobal = DT, aggChoice, scenChoice = get(l), mergedVals)

        #' create file specific name and related info
        if (multipleNutsFileList[k] == "dt.nutrients.sum.all")  {
          #         nutTitle <- paste("Average daily availability of ", tolower(nutlongName), sep = "")
          nutTitle <- paste(tolower(nutlongName), sep = "")
          ylab = paste("(",units,")",sep = "")
          drawOneLine = FALSE
        }
        if (multipleNutsFileList[k] %in% c("RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio"))  {
          nutTitle <- nutshortName
          ylab = "(Adequacy ratio)"
          drawOneLine = 1
        }
        if (multipleNutsFileList[k] %in% c("dt.MRVRatios"))  {
          #        nutTitle <- paste("Ratio of ", nutshortName, " availability to MRV", sep = "")
          nutTitle <- nutshortName
          #          ylab = "(Maximal reference value)"
          ylab = ""
          drawOneLine = FALSE
        }
        if (multipleNutsFileList[k] %in% "AMDR_hi_sum_reqRatio")  {
          nutTitle <- paste("AMDR high, ", nutshortName, sep = "")
          ylab = "(AMDR, high)"
          drawOneLine = 1
        }
        if (multipleNutsFileList[k] %in% "AMDR_lo_sum_reqRatio")  {
          nutTitle <- paste("AMDR low, ", nutshortName, sep = "")
          ylab = "(AMDR, low)"
          drawOneLine = 1
        }
        if (multipleNutsFileList[k] == "dt.nutrients.nonstapleShare")  {
          #        nutTitle <- paste("Non-staple share of ", nutshortName, " availability", sep = "")
          nutTitle <- paste(nutshortName, sep = "")
          ylab = "(percent)"
          drawOneLine = FALSE
        }
        if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,103)} else {yRangeMinMax <- c(0, max(DT$value) + 0.02 * max(DT$value) )}

        fileName = paste(gdxChoice, l, multipleNutsListShortName[k], nut, aggChoice, sep = "_")
        plotByRegionBar(dt = DT, fileName,
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice,
                        scenOrder = get(l), oneLine = drawOneLine, colorList, graphsListHolder, plotErrorBars)
      }
    }

  cat(paste("\nDone with ", multipleNutsFileList[k], " ----------------"))
  }
}
cat("\nDone with scenChoiceList loop ----------")

#' box plot of budget share in 2050  ----
DTglobal <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
#' get rid of Somalia because it's budget share is 500 * per cap income
DTglobal <- DTglobal[!region_code.IMPACT159 == "SOM",]
DT <- data.table::copy(DTglobal)
scenChoice <- "SSP2-HGEM-REF" # this is the 2050 value for this scenario

ylab <- "(percent)"
plottitle = "Food budget share of 2050 per capita income"
#' statement below excludes tenrgions, which is not relevant for a box plot
for (aggChoice in aggChoiceListBarChart[!aggChoiceListBarChart %in% "tenregions"]) {
  DT <- aggNorder(gdxChoice, DT, aggChoice, scenChoice, mergedVals)
  plotErrorBars = FALSE # not relevant for a boxplot
  #' aggregate to and retain only the relevant regions
  fileName <- paste(gdxChoice, l, "budgetShareBoxPlot_2050", aggChoice, sep = "_")
  # dt.regions <- regionAgg(aggChoice)
  # DT <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
  # keepListCol.incShare <- c("scenario" ,"year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0")
  # DT <- DT[, (keepListCol.incShare), with = FALSE]
  # scenario.base <- "SSP2-NoCC-REF"
  # DT <- DT[year == "X2050" & scenario == scenario.base]
  # DT[, c("year") := NULL]
  plotByBoxPlot2050(dt = DT, fileName, plotTitle = plottitle, yLab = ylab, yRange = c(0,50), aggChoice)
}
#' the method commented out below to get the box plots stats doesn't work with geom_boxplot. The following link has a
#' way to do this but I'm not going to implement right now.
#' http://stackoverflow.com/questions/23970118/how-to-add-summary-information-to-geom-boxplot

#' Also see https://plot.ly/ggplot2/box-plots/ discussion on stat_summary

# line plots -----
#this is not working right now
# plotByRegionLine("dt.shannonDiversity", "ShannonDiversity", "Shannon Diversity", yRange = c(20, 80), "I3regions")

# write out zip files of the csv files in gDir
filenames.csv <- list.files(path = paste(fileloc("gDir"), sep = "/"), pattern = ".csv")
for (l in scenChoiceList) {
  temp <- filenames.csv[grep(l, filenames.csv)]
  print(paste("writing .zip file of csv files for", l))
  zip(zipfile = paste(fileloc("gDir"), "/", gdxChoice, "_", l, "_csvfiles.zip", sep = ""),
      files = paste(fileloc("gDir"), "/", temp, sep = ""), extras = "-qdgds 10m", flags = "-j")
}

# construct graphs for AMDR ratios ------
DT.master <- getNewestVersion("food_agg_AMDR_hi", fileloc("resultsDir")) # both hi and lo have the same values
for (macroNut in c("fat_g.Q", "protein_g.Q", "carbohydrate_g.Q")) {
  cat(paste("\nWorking on AMDR for", macroNut))
  if (macroNut %in%  "fat_g.Q") nutName <- "fat_g"
  if (macroNut %in%  "protein_g.Q") nutName <- "protein_g"
  if (macroNut %in%  "carbohydrate_g.Q") nutName <- "carbohydrate_g"

  for (aggChoice in aggChoiceListBarChart) {
    if (aggChoice == "tenregions") plotErrorBars = FALSE
    if (aggChoice == "WB") plotErrorBars = TRUE
    if (aggChoice == "AggReg1") plotErrorBars = TRUE
    DT <- data.table::copy(DT.master)
    keepListCol <- c("scenario", "region_code.IMPACT159", "year", macroNut)
    DT <- DT[, (keepListCol), with = FALSE]
    DT[, value := get(macroNut)]
    DT <- aggNorder(gdxChoice, DT, aggChoice, scenChoice = get(l), mergedVals)

    units <- "percent"
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
    fileName = paste(gdxChoice, l, "AMDRShare", nutName, aggChoice, sep = "_")
    nutTitle <- capwords(cleanupNutrientNames(nutName))
    plotByRegionBarAMDR(dt = DT, fileName,
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice,
                        scenOrder = get(l), colorList, AMDR_lo, AMDR_hi, graphsListHolder, plotErrorBars)
  }
}

# facet maps, food availability by food groups  -----
cat("\n Working on facet maps")
worldMap <- getNewestVersion("worldMap", fileloc("mData"))
DT <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))
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
fileName <- paste(gdxChoice, l, "facetmap", "FGAvail", "2050", "noCC", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "FGAvailChange", "climate", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "FGAvailChange", "income", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

# facet maps, req ratio results ------
#' use this both for the nutrients to keep and the order in which they should be displayed
keepListNuts <- c("protein_g", "carbohydrate_g", "calcium_mg", "iron_mg", "zinc_mg", "folate_µg", "vit_a_rae_µg", "vit_d_µg", "vit_e_mg", "vit_b12_µg")

DT.macro <- getNewestVersion("RDA.macro_sum_reqRatio", fileloc("resultsDir"))
DT.vits <- getNewestVersion("RDA.vits_sum_reqRatio", fileloc("resultsDir"))
DT.minrls <- getNewestVersion("RDA.minrls_sum_reqRatio", fileloc("resultsDir"))
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
fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatio", "2010", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatio", "2050", "noCC", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange", "climate", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange", "income", sep = "_")
facetMaps(worldMap, DT = DT.wide, fileName, legendText, fillLimits = fillLimits.inc, palette, facetColName, graphsListHolder, breakValues, displayOrder)

# facet map, MRV ratio results -----
DT <- getNewestVersion("dt.MRVRatios", fileloc("resultsDir"))
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
fileName <- paste(gdxChoice, l, "facetmap", "MRVRatio", "2010", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "MRVRatio", "2050", "noCC", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "MRVRatioChange", "climate", sep = "_")
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
fileName <- paste(gdxChoice, l, "facetmap", "MRVRatioChange", "income", sep = "_")
facetMaps(worldMap, DT = DT.wide, fileName, legendText, fillLimits = fillLimits.inc, palette, facetColName, graphsListHolder, breakValues, displayOrder)

filenames.pdf <- list.files(path = paste(fileloc("gDir"), sep = "/"), pattern = ".pdf")
for (scenChoice in scenChoiceList) {
  #create zip file of all the graphics outputs for a set of scenarios
  temp <- filenames.csv[grep(scenChoice, filenames.pdf)]
  print(paste("writing .zip file of pdf files for", scenChoice))
  zip(zipfile = paste(fileloc("gDir"), "/", gdxChoice, scenChoice, "_pdffiles.zip", sep = ""),
      files = paste(fileloc("gDir"), "/", temp, sep = ""), extras = "-qdgds 10m", flags = "-j")

  #create  xlsx file with the data used to create the  WB figures -----
  rowCounter <- 1
  aggChoice <- "WB"
   # list of potential nutrients to add to the table
    # macroNutrients <- c("carbohydrate_g", "protein_g", "fat_g",  "totalfiber_g")
    # vitamins <- c( "folate_µg", "thiamin_mg", "niacin_mg", "riboflavin_mg",
    #                "vit_b6_mg", "vit_a_rae_µg", "vit_b12_µg","vit_c_mg",
    #                "vit_d_µg", "vit_e_mg", "vit_k_µg")
    # minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
    #               "potassium_g", "zinc_mg")
    # kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
    energy <- c("energy_kcal")
    # addedSugar <- c("sugar_g")
    # fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
    #                 "ft_acds_tot_trans_g")
    #
    nutlistmacro <- c("carbohydrate_g", "protein_g",  "totalfiber_g") # fat is excluded here
#    nutlistmacroAMDRlo <- c("carbohydrate_g_AMDRlo", "protein_g_AMDRlo",  "fat_g_AMDRlo")
    nutlistmacroAMDRShare <- paste("AMDRShare", c("carbohydrate_g", "protein_g",  "fat_g"), sep = "_")
    # # nutlistminrls <- c("calcium_mg", "magnesium_mg", "potassium_g", "phosphorus_mg")
    # # nutlistvits <- c("folate_µg", "riboflavin_mg", "vit_a_rae_µg","vit_b12_µg", "vit_c_mg", "vit_e_mg",  "vit_d_µg",
    # #                  "vit_k_µg", "thiamin_mg")
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
        filename <- paste(gdxChoice, l, figtype, "WB", sep = "_")
        figInfo <- "4, affordability,"
      }
      if (figtype %in% dailyAvail.foodgroup) {
        filename <- paste(gdxChoice, l, "foodAvail_foodGroup", figtype, aggChoice, sep = "_")
        figInfo <- "5, food group daily availability (g), "
      }
      if (figtype %in% energy) {
        filename <- paste(gdxChoice, l, "nutrients.avail_energy_kcal", aggChoice, sep = "_")
        figInfo <- "6, average daily availability dietary energy (kcals), "
      }
      if (figtype %in% nutlistmacro) {
        filename <- paste(gdxChoice, l, "macro_reqRatio", figtype, aggChoice, sep = "_")
        figInfo <- "7, adequacy, macro nutrients, "
      }
      if (figtype %in% nutlistmacroAMDRShare) {
        filename <- paste(gdxChoice, l, figtype, aggChoice, sep = "_")
 #       filename <- gsub("AMDRShare", "_", filename) # a kludge to deal with the fact that these are macronutrients
        figInfo <- "8, Share of total kilocalories, "
      }
      if (figtype %in% minerals) {
        filename <- paste(gdxChoice, l, "minrls_reqRatio", figtype, aggChoice, sep = "_")
        figInfo <- "9, adequacy, minerals, "
      }
      if (figtype %in% vitamins) {
        filename <- paste(gdxChoice, l, "vits_reqRatio", figtype, aggChoice, sep = "_")
        figInfo <- "9, adequacy, vitamins, "
      }
      if (figtype %in% nutBal) {
        filename <- paste(gdxChoice, l, figtype, aggChoice, sep = "_")
        figInfo <- "10, Nutrient balance metrics, "
      }
      if (figtype %in% diversity) {
        filename <- paste(gdxChoice, l, figtype, aggChoice, sep = "_")
      figInfo <- "11, diversity metrics, "
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

    openxlsx::writeData(
      wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = FALSE, startCol = 1,
      x = fileIn,
      startRow = rowCounter
    )
    rowCounter <- rowCounter + nrow(fileIn)

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
      gridExpand = TRUE
    )

    openxlsx::saveWorkbook(wb = figsData, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_", aggChoice, ".xlsx", sep = ""),
                           overwrite = TRUE)
    data.table::fwrite(csvHolder, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_", aggChoice, ".csv", sep = ""), na = "", row.names = FALSE)
  }

  # aggregate to regions -----
  filesToAgg <- c("RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio")
  for (fname in filesToAgg) {
    for (aggChoice in c("AggReg1", "WB")) {
      DT <- getNewestVersion(fname, fileloc("resultsDir"))
      DT <- merge(DT, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))
      dt.regions <- regionAgg(aggChoice)
      # aggregate to and retain only the relevant regions; region code is the code for the region
      merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")

      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]
      keepListCol <- c("scenario", "region_code", "region_name", "year", "nutrient", "value")
      merged <- merged[, (keepListCol), with = FALSE]
      merged <- unique(merged)
      inDT <- merged
      outName <- paste(fname, aggChoice, sep = ".")
      cleanup(inDT, outName, destDir = fileloc("resultsDir"), writeFiles = "csv")
    }
  }
}
# save graph files for future use
inGraphFile <- graphsListHolder
outName <- "graphsListHolder"
cleanupGraphFiles(inGraphFile, outName, fileloc("gDir"))
