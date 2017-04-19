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
                          "dt.MRVRatios",
                          "AMDR_hi_sum_reqRatio",
                          "AMDR_lo_sum_reqRatio")
#                          "dt.foodAvail.foodGroup")
multipleNutsListShortName <- c("nutrients.avail",
                               "macro_reqRatio",
                               "minrls_reqRatio",
                               "vits_reqRatio",
                               "nutrients.nonstaples.share",
                               # "zinc_bioavail_reqRatio",
                               # "iron_bioavail_reqRatio",
                               "badRatios",
                               "AMDR_hi",
                               "AMDR_lo")
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
    #   generate the legends. needs to be inside l and i loop -----
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
    cat(paste("\nWorking on bar chart for budget share for", aggChoice))
    DTglobal <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals)
    ylab <- "(percent)"
    fileName <- paste(gdxChoice, l, "budgetShare", aggChoice, sep = "_")
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    yRangeMinMax <- c(0,60) #custom for budgetShare
    plotByRegionBar(dt = DT, fileName, plotTitle = "Food expenditures share of per capita income",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE, colorList, graphsListHolder,
                    plotErrorBars)
    #  yLab = "(percent)", yRange = c(0, 50), aggChoice, oneLine = FALSE)

    print(paste("Done with bar chart for budget share for", aggChoice))
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
    cat(paste("\nWorking on bar chart for Rao's QE for", aggChoice))
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

    # nonstaple share of kcals -----
    cat(paste("\nWorking on bar chart for the nonstaple share of kcals for", aggChoice))
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
    cat(paste("\nWorking on bar chart for the NBS for", aggChoice))
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
    cat(paste("\nWorking on bar chart for disqualifying nutrients for", aggChoice))
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
    cat(paste("\nWorking on stacked kcal bar chart for ", aggChoice))
    DTglobal <- getNewestVersion("dt.kcals.values", fileloc("resultsDir"))
    DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l),
                    mergedVals =  c("scenario", "region_code", "year", "nutrient"))
    DT <- DT[nutrient %in% c("kcalsPerDay.carbohydrate", "kcalsPerDay.fat", "kcalsPerDay.other", "kcalsPerDay.protein"), ]
    yLab <- "(Kcals)"
    if (yLab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, round(max(DT$value)) )}
    fileName = paste(gdxChoice, l, "kcals.values", sep = "_")
    plotByRegionStackedBar(dt = DT, fileName, plotTitle = "Average daily dietary energy by source",
                           yLab, yRange = yRangeMinMax, aggChoice,  scenOrder = get(l), oneLine = FALSE, colorList)
  }

  # food groups -----
  cat(paste("\nWorking on dt.foodAvail.foodGroup"))
  temp.in <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))
  #  temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

  #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
  temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
                       year == "X2050",][year == "X2010", scenario := "2010"]
  foodGroupList <- unique(temp.in$food_group_code)
  for (aggChoice in aggChoiceListBarChart) {
    if (aggChoice == "tenregions") plotErrorBars = FALSE
    if (aggChoice == "WB") plotErrorBars = TRUE
    if (aggChoice == "AggReg1") plotErrorBars = TRUE

    for (fg in foodGroupList) {
      units <- "grams"
      DT <- data.table::copy(temp.in)
      DT <- DT[food_group_code %in% fg,]
      DT[, food_group_code := NULL]
      DT <- aggNorder(gdxChoice, DTglobal = DT, aggChoice, scenChoice = get(l), mergedVals)
      # dt.regions <- regionAgg(aggChoice)
      # # aggregate to and retain only the relevant regions
      # merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
      # merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
      # #      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code")]
      # keepListCol <- c("scenario", "region_code", "region_name", "value")
      # DT <- unique(merged[, (keepListCol), with = FALSE])
      # DT <- DT[, region_name := gsub(" plus", "", region_name)]
      # DT <- DT[scenario %in% get(l), ]
      #
      # if (gdxChoice == "USAID")  {
      #
      #   # keep only needed USAID scenarios
      #   scenOrder <- c(get(l))
      #   DT <- DT[scenario %in% get(l), ]
      # }

      # # order scenarios, first write the number into the number variable scenarioOrder
      # DT[, scenarioOrder := match(scenario, scenOrder)]
      # data.table::setorder(DT, scenarioOrder)
      # DT[, scenarioOrder := NULL]
      #
      # DT <- orderRegions(DT, aggChoice)
      # #      nutTitle <- paste("Average daily availability of ", tolower(FG.longName), sep = "")
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
  }
  cat(paste("Done with food groups\n\n"))

  # several nutrients -----
  for (k in 1:length(multipleNutsFileList)) {

    #    if (multipleNutsFileList[k] %in% "dt.MRVRatios") browser()

    cat(paste("Working on multiple nut file", multipleNutsFileList[k]))

    temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
    #    temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

    #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
    temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
                         year == "X2050",][year == "X2010", scenario := "2010"]
    for (aggChoice in aggChoiceListBarChart) {
      if (aggChoice == "tenregions") plotErrorBars = FALSE
      if (aggChoice == "WB") plotErrorBars = TRUE
      if (aggChoice == "AggReg1") plotErrorBars = TRUE

      for (j in unique(temp.in$nutrient)) {
        #     for (j in c(macroNutrients, vitamins, minerals)) {
        nutshortName <- cleanupNutrientNames(j)
        nutshortName <- capwords(nutshortName)
        # if (j %in% c("kcals.ethanol", "kcals.fat", "kcals.carbohydrate","kcals.protein", "kcals.sugar")) {
        #     nutlongName <- "energy"
        #     units <- "kilocalories"
        # } else {
        nutlongName <- dt.nutrientNames_Units[1, (j)]
        units <- dt.nutrientNames_Units[2, (j)]
        # }
        if (units %in% "g") units <- "grams"
        DT <- temp.in[nutrient %in% j,]
        DT[, nutrient := NULL]
        DT <- aggNorder(gdxChoice, DTglobal = DT, aggChoice, scenChoice = get(l), mergedVals)

        # dt.regions <- regionAgg(i)
        # # aggregate to and retain only the relevant regions; region code is the code for the region
        # merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
        # merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
        # keepListCol <- c("scenario", "region_code", "region_name", "value")
        # DT <- unique(merged[, (keepListCol), with = FALSE])
        # DT <- DT[, region_name := gsub(" plus", "", region_name)]
        # #        if (gdxChoice == "USAID") DT <- renameUSAIDscenarios(DT)
        #
        # DT <- DT[scenario %in% get(l), ]
        # if (gdxChoice == "USAID")  {
        #
        #   # keep only needed USAID scenarios
        #   scenOrder <- c(get(l))
        #   DT <- DT[scenario %in% get(l), ]
        # }
        #
        # # order scenarios, first write the number into the number variable scenarioOrder
        # DT[, scenarioOrder := match(scenario, scenOrder)]
        # data.table::setorder(DT, scenarioOrder)
        # DT[, scenarioOrder := NULL]
        #
        # DT <- orderRegions(DT, aggChoice)

        # create file specific name and related info
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
        if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}

        fileName = paste(gdxChoice, l, multipleNutsListShortName[k], j, aggChoice, sep = "_")
        plotByRegionBar(dt = DT, fileName,
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice,
                        scenOrder = get(l), oneLine = drawOneLine, colorList, graphsListHolder, plotErrorBars)
      }
    }
    print(paste("Done with ", multipleNutsFileList[k]))
    #    cat("\n\n")
  }
  # end of scenChoiceList loop

  # box plot of budget share in 2050 new ----
  dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
  # get rid of Somalia because it's budget share is 500 * per cap income
  dt.budgetShare <- dt.budgetShare[!region_code.IMPACT159 == "SOM",]

  ylab <- "(percent)"

  # plottitle = "IMPACT food budget share of 2050 per capita income"
  plottitle = "Food budget share of 2050 per capita income"
  for (aggChoice in aggChoiceListBarChart) {
    if (aggChoice == "tenregions") plotErrorBars = FALSE
    if (aggChoice == "WB") plotErrorBars = TRUE
    if (aggChoice == "AggReg1") plotErrorBars = TRUE

    # aggregate to and retain only the relevant regions
    fileName <- paste(gdxChoice, l, "budgetShareBoxPlot_2050", aggChoice, sep = "_")
    dt.regions <- regionAgg(aggChoice)
    DT <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
    keepListCol.incShare <- c("scenario" ,"year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0")
    DT <- DT[, (keepListCol.incShare), with = FALSE]
    scenario.base <- "SSP2-NoCC-REF"
    DT <- DT[year == "X2050" & scenario == scenario.base]
    DT[, c("year") := NULL]
    plotByBoxPlot2050(dt = DT, fileName, plotTitle = plottitle, yLab = ylab, yRange = c(0,50), aggChoice )
  }
  # the method commented out below to get the box plots stats doesn't work with geom_boxplot. The following link has a
  # way to do this but I'm not going to implement right now.
  # http://stackoverflow.com/questions/23970118/how-to-add-summary-information-to-geom-boxplot

  # Also see https://plot.ly/ggplot2/box-plots/ discussion on stat_summary

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
  for (macroNut in c("fat_g.Q", "protein_g.Q", "carbohydrate_g.Q")) {
    dt.AMDR <- getNewestVersion("food_agg_AMDR_hi", fileloc("resultsDir")) # both hi and lo have the same values
    cat(paste("\nWorking on AMDR for", macroNut))
    keepListCol <- c("scenario", "region_code.IMPACT159", "year", macroNut)
    temp.in <- dt.AMDR[, (keepListCol), with = FALSE]
    data.table::setnames(temp.in, old = macroNut, new = "value")
    temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

    #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
    if (macroNut %in%  "fat_g.Q") nutName <- "fat_g"
    if (macroNut %in%  "protein_g.Q") nutName <- "protein_g"
    if (macroNut %in%  "carbohydrate_g.Q") nutName <- "carbohydrate_g"

    for (aggChoice in aggChoiceListBarChart) {
      if (aggChoice == "tenregions") plotErrorBars = FALSE
      if (aggChoice == "WB") plotErrorBars = TRUE
      if (aggChoice == "AggReg1") plotErrorBars = TRUE

      DT <- temp.in[year == "X2010" & scenario == scenario.base |
                      year == "X2050",][year == "X2010", scenario := "2010"]
      #  print(paste(l, i, j, sep = ", "))

      units <- "percent"
      dt.regions <- regionAgg(aggChoice)
      # aggregate to and retain only the relevant regions; region code is the code for the region
      merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
      keepListCol <- c("scenario", "region_code", "region_name", "value")
      DT <- unique(merged[, (keepListCol), with = FALSE])
      DT <- DT[, region_name := gsub(" plus", "", region_name)]

      DT <- DT[scenario %in% get(l), ]
      # if (gdxChoice == "USAID")  {
      #   # keep only needed USAID scenarios
      scenOrder <- c(get(l))
      #   DT <- DT[scenario %in% get(l), ]
      #   scenChoice.name <- l
      # }
      #
      # order scenarios, first write the number into the number variable scenarioOrder
      DT[, scenarioOrder := match(scenario, scenOrder)]
      data.table::setorder(DT, scenarioOrder)
      DT[, scenarioOrder := NULL]
      DT <- orderRegions(DT, aggChoice)
      nutshortName <- cleanupNutrientNames(nutName)
      nutshortName <- capwords(nutshortName)
      #     nutTitle <- paste("Share of kilocalories from ", capwords(nutshortName), sep = "")
      nutTitle <- paste("", capwords(nutshortName), sep = "")
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
      plotByRegionBarAMDR(dt = DT, fileName,
                          plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice,
                          scenOrder = get(l), colorList, AMDR_lo, AMDR_hi, graphsListHolder, plotErrorBars)
    }
  }
}
# facet maps, food availability by food groups  -----
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
DT[food_group_code %in% "rootsNPlaintain", food_group_code := "roots and plantain"]
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

fileName <- paste(gdxChoice, l, "facetmap", "FGAvail", "2050", "noCC", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues)
#facet map, climate change effect on availability -----
# convert to wide to do operations aross scenarios
formula.wide <- "id + food_group_code ~ scenario"
DT.wide <- data.table::dcast(
  data = DT,
  formula = formula.wide,
  value.var = "value")

DT.wide[, delta.cc := 100 * (SSP2_HGEM_REF - SSP2_NoCC_REF) / SSP2_NoCC_REF]
DT.wide[, delta.inc := 100 * (SSP2_NoCC_REF - X2010) / X2010]

DT.wide[, value := delta.cc]

legendText <- "Climate Change Effect, 2050, \n(percent)"
fillLimits <- c(-14, 1)
myPalette <- colorRampPalette(rev(brewer.pal(9, "Reds")))
palette <- myPalette(4)
#' middle two values shift the palette gradient; the code below gives a smooth change
fillRange <- fillLimits[2] - fillLimits[1]
breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))

fileName <- paste(gdxChoice, l, "facetmap", "FGAvailChange", "climate", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues)

# increase in availability do to income growth
# get rid of old 'value'
DT.wide[, value := delta.inc]

legendText <- "Income Growth Effect, \n2010-2050, (percent)"
fillLimits <- c(-30, 100)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
palette <- myPalette(4)
facetColName <- "food_group_code"
#' middle two values shift the palette gradient; the code below gives a smooth change
fillRange <- fillLimits[2] - fillLimits[1]
breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))

fileName <- paste(gdxChoice, l, "facetmap", "FGAvailChange", "income", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues)

# facet map, req ratio results ------
DT.macro <- getNewestVersion("RDA.macro_sum_reqRatio", fileloc("resultsDir"))
DT.vits <- getNewestVersion("RDA.vits_sum_reqRatio", fileloc("resultsDir"))
DT.minrls <- getNewestVersion("RDA.minrls_sum_reqRatio", fileloc("resultsDir"))
DT <- do.call("rbind", list(DT.macro, DT.vits, DT.minrls))
keepListNuts <- c("protein_g", "carbohydrate_g", "calcium_mg", "iron_mg", "zinc_mg", "folate_µg", "vit_a_rae_µg", "vit_d_µg", "vit_e_mg", "vit_b12_µg")
 DT <- DT[nutrient %in% keepListNuts,]
DT[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
DT <- DT[year == "X2010" & scenario %in% gsub("-", "_", scenario.base) |
           year == "X2050",][year == "X2010", scenario := "X2010"][, year := NULL]

DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
DT[, nutrient := cleanupNutrientNames(nutrient)]
DT[, nutrient := capwords(nutrient)]
data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")

facetColName <- "nutrient"

# adequacy 2050, no CC -----
legendText <- "adequacy ratio, 2050, \nno climate change"
fillLimits <- c(0, 5)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
palette <- myPalette(4)

#' middle two values shift the palette gradient;
fillRange <- fillLimits[2] - fillLimits[1]
breakValues <- scales::rescale(c(fillLimits[1], 1, 2, fillLimits[2]))

fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange", "2050", "noCC", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues)

# convert to wide to do operations aross scenarios
formula.wide <- "id + nutrient ~ scenario"
DT.wide <- data.table::dcast(
  data = DT,
  formula = formula.wide,
  value.var = "value")

DT.wide[, delta.cc := 100 * (SSP2_HGEM_REF - SSP2_NoCC_REF) / SSP2_NoCC_REF]
DT.wide[, delta.inc := 100 * (SSP2_NoCC_REF - X2010) / X2010]
#facet map, climate change effect on adequacy -----
DT.wide[, value := delta.cc]
legendText <- "Climate Change Effect, 2050, \n(percent)"
fillLimits <- c(-11, 10)
#myPalette <- colorRampPalette(rev(brewer.pal(9, "Reds")))
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
palette <- myPalette(4)
#' middle two values shift the palette gradient; the code below gives a smooth change
fillRange <- fillLimits[2] - fillLimits[1]
breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
DT <- DT.wide
fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange", "climate", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues)

# increase in availability due to income growth
DT.wide[, value := delta.inc]
legendText <- "Income Growth Effect, \n2010-2050, (percent)"
fillLimits <- c(-40, 100)
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
palette <- myPalette(4)
#' middle two values shift the palette gradient; the code below gives a smooth change
fillRange <- fillLimits[2] - fillLimits[1]
breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
DT <- DT.wide
fileName <- paste(gdxChoice, l, "facetmap", "nutReqRatioChange", "income", sep = "_")
facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues)

#create zip file of all the graphics outputs for a set of scenarios
filenames.pdf <- list.files(path = paste(fileloc("gDir"), sep = "/"), pattern = ".pdf")
for (scenChoice in scenChoiceList) {
  temp <- filenames.csv[grep(scenChoice, filenames.pdf)]
  print(paste("writing .zip file of pdf files for", scenChoice))
  zip(zipfile = paste(fileloc("gDir"), "/", gdxChoice, scenChoice, "_pdffiles.zip", sep = ""),
      files = paste(fileloc("gDir"), "/", temp, sep = ""), extras = "-qdgds 10m", flags = "-j")

  #create  xlsx file with the data used to create the  WB figures -----
  if ("WB"  %in%  aggChoiceListBarChart) {
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
    nutlistmacroAMDRlo <- c("carbohydrate_g_AMDRlo", "protein_g_AMDRlo",  "fat_g_AMDRlo")
    nutlistmacroAMDRhi <- c("carbohydrate_g_AMDRhi", "protein_g_AMDRhi",  "fat_g_AMDRhi")
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
    scen2050list <- c("SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")

    figsData <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb = figsData, sheetName = "FigureData")
    colHeaders <- c("Scenario","Low Income", "Low middle income", "Upper middle income", "High income",
                    "2050 CC effect, low income", "2050 CC effect, low middle income",
                    "2050 CC effect, upper middle income", "2050 CC effect, high income")
    rowCounter <- 1

    #write column names to the spreadsheet
    openxlsx::writeData(
      wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = TRUE, startCol = 1,
      x = as.list(colHeaders),
      startRow = rowCounter
    )
    rowCounter <- rowCounter + 1
    #boxstats removed for now
    for (figtype in c(budgetShare, dailyAvail.foodgroup, energy, nutlistmacro, nutlistmacroAMDRlo, nutlistmacroAMDRhi,
                      vitamins, minerals, nutBal, diversity)) {
      #   if (i %in% c(budgetShare, boxStats)) {
      if (figtype %in% c(budgetShare)) {
        filename <- paste(gdxChoice, l, figtype, "WB", sep = "_")

        figInfo <- "1, affordability,"
      }
      if (figtype %in% dailyAvail.foodgroup) {
        filename <- paste(gdxChoice, l, "foodAvail_foodGroup", figtype, "WB", sep = "_")
        figInfo <- "2, food group daily availability (g), "
      }
      if (figtype %in% energy) {
        filename <- paste(gdxChoice, "nutrients.avail_energy_kcal", "WB", sep = "_")
        figInfo <- "4, average daily availability dietary energy (kcals), "
      }
      if (figtype %in% nutlistmacro) {
        filename <- paste(gdxChoice, "macro_reqRatio", figtype, "WB", sep = "_")
        figInfo <- "5, adequacy, macro nutrients, "
      }
      if (figtype %in% nutlistmacroAMDRlo) {
        filename <- paste(gdxChoice, l, "AMDR_lo", figtype, "WB", sep = "_")
        filename <- gsub("_AMDRlo_", "_", filename) # a kludge to deal with the fact that these are macronutrients
        figInfo <- "6, AMDR low, macro nutrients, "
      }
      if (figtype %in% nutlistmacroAMDRhi) {
        filename <- paste(gdxChoice, l, "AMDR_hi", figtype, "WB", sep = "_")
        filename <- gsub("_AMDRhi_", "_", filename) # a kludge to deal with the fact that these are macronutrients
        figInfo <- "6, AMDR high, macro nutrients, "
      }
      if (figtype %in% minerals) {
        filename <- paste(gdxChoice, l, "minrls_reqRatio", figtype, "WB", sep = "_")
        figInfo <- "7, adequacy, minerals, "
      }
      if (figtype %in% vitamins) {
        filename <- paste(gdxChoice, l, "vits_reqRatio", figtype, "WB", sep = "_")
        figInfo <- "7, adequacy, vitamins, "
      }
      if (figtype %in% nutBal) {
        filename <- paste(gdxChoice, l, figtype, "WB", sep = "_")
        figInfo <- "8, Nutrient balance metrics, "
      }
      if (figtype %in% nonStapleShareKcals)
        filename <- paste(gdxChoice, l, figtype, "WB", sep = "_")
      figInfo <- "9, diversity metrics, "
    }
    if (figtype %in% diversity) {
      filename <- paste(gdxChoice, l, figtype, "WB", sep = "_")
      figInfo <- "9, diversity metrics, "
    }

    fileIn <- data.table::fread(paste(fileloc("gDir"), "/", filename, ".csv", sep = ""), select = 2:6)
    for (j in scen2050list) {
      for (k in 1:length(incCats)) {
        baseVal <- fileIn[scenario == gsub("-REF", "", scenario.base), get(incCats[k])]
        fileIn[scenario == j, DincCats[k] := (get(incCats[k]) - baseVal) / baseVal]
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
    csvHolder <- rbind(csvHolder,category)
    csvHolder <- rbind(csvHolder, fileIn)

    data.table::setnames(csvHolder,
                         old = names(csvHolder),
                         new = colHeaders)

    openxlsx::addStyle(
      wb = figsData, sheet = "FigureData", style = numStyle, cols = 2:length(csvHolder), rows = 2:nrow(csvHolder),
      gridExpand = TRUE
    )

    openxlsx::saveWorkbook(wb = figsData, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_WB.xlsx", sep = ""),
                           overwrite = TRUE)
    data.table::fwrite(csvHolder, file = paste(fileloc("gDir"), "/", gdxChoice, "_", "reqTable_WB.csv", sep = ""), na = "", row.names = FALSE)
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
