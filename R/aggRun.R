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
# gdxChoice values are either SSPs or USAID
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA_macro.sum.reqRatio, RDA_minrls.sum.reqRatio, RDA_vits.sum.reqRatio
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB
dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
gdxFileName <- dt.metadata[file_description %in% "IMPACT demand data in gdx form", file_name_location]
aggChoiceListBarChart <- c("tenregions", "WB", "AggReg1") # missing AggReg2 and  "2EconGroup
multipleNutsFileList <- c("dt.nutrients.sum.all",
                          "RDA.macro_sum_reqRatio",
                          "RDA.minrls_sum_reqRatio",
                          "RDA.vits_sum_reqRatio",
                          "dt.nutrients.nonstapleShare",
                          "dt.MRVRatios",
                          "AMDR_hi_sum_reqRatio",
                          "AMDR_lo_sum_reqRatio",
                          "dt.foodAvail.foodGroup")
multipleNutsListShortName <- c("nutrients.avail",
                               "macro_reqRatio",
                               "minrls_reqRatio",
                               "vits_reqRatio",
                               "nutrients.nonstaples.share",
                               # "zinc_bioavail_reqRatio",
                               # "iron_bioavail_reqRatio",
                               "badRatios",
                               "AMDR_hi",
                               "AMDR_lo",
                               "foodAvail.foodGroup")

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
if (gdxFileName == "Micronutrient-Inputs-07252016.gdx") {
  gdxChoice <- "SSPs"
  scenario.base <- "SSP2-NoCC-REF"
#  scenOrder <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF",
#                 "SSP2-IPSL-REF", "SSP2-HGEM-REF")
  scenOrderSSP <- c("2010",  "SSP2-NoCC-REF", "SSP2-HGEM-REF", "SSP1-NoCC-REF","SSP3-NoCC-REF")
  scenChoiceList <- "scenOrderSSP"
  scenChoice.name <- "SSP"
}

if (gdxFileName == "Micronutrient-Inputs-USAID.gdx") {
  gdxChoice <- "USAID"
  scenario.base <- "SSP2-NoCC-NA"
  # USAID scenario combo choices
  prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
  waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
  addEnhance <- c("RPHL", "RMM")
  comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")

  #USAID scenario list choice
  scenChoiceList <- c("prodEnhance", "waterMan", "addEnhance", "comp")
  #  scenChoice <- get(scenChoice.name)
}

#for testing
#aggChoice <- "WB"; DTglobal <- "dt.shannonDiversity"

# dt.pop.2010.ref <- dt.pop[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]
dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))

for (l in scenChoiceList) {
  #put colorlist here so its easy to see alignment of colors and bars
  scenOrder <- get(scenChoiceList)
 if (l %in% "scenOrderSSP") {
   colorList <- c("black", "red", "green4", "red3", "red4") #put here so its easy to see alignment of colors and bars
 } else {
   colorList <- c("black", rainbow(10)[1:length(c("prodEnhance", "waterMan", "addEnhance", "comp")) - 1])
}

  for (i in aggChoiceListBarChart) {
    # Shannon Diversity -----
    print(paste("Working on bar chart for Shannon Diversity for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.shannonDiversity", aggChoice = i, get(l))
    ylab <- "(percent)"
    filename = "ShannonDiversity"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = filename, plotTitle = "Shannon Diversity", yLab = ylab,
                    yRange = yRangeMinMax, aggChoice = i, scenOrder, oneLine = FALSE, colorList)
 #   yRange = c(0, 80), aggChoice = i, oneLine = FALSE)

    # Budget share -----
    print(paste("Working on bar chart for budget share for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.budgetShare", aggChoice = i, get(l))
    ylab <- "(percent)"
    filename <- "budgetShare"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    if (filename %in% "budgetShare") yRangeMinMax <- c(0,60) #custom
    plotByRegionBar(dt = DT, fileName = filename, plotTitle = "IMPACT food budget share of per capita income",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
  #  yLab = "(percent)", yRange = c(0, 50), aggChoice = i, oneLine = FALSE)

print(paste("Done with bar chart for budget share for", i))
    # MFAD -----
    print(paste("Working on bar chart for MFAD for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.MFAD", aggChoice = i, get(l))
    ylab <- "(percent)"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = "MFAD", plotTitle = "Modified Functional Attribute Diversity",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
  #  yLab = "(percent)", yRange = c(0, 100), aggChoice = i, oneLine = FALSE)
print(paste("Done with bar chart for MFAD for", i))
    # RAOqe -----
    print(paste("Working on bar chart for Rao's QE for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.RAOqe", aggChoice = i, scenChoice = get(l))
    DTglobalylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = "RAOqe", plotTitle = "Rao's quadratic entropy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
 #   yLab = NULL, yRange = c(0, 100), aggChoice = i, oneLine = FALSE)

    # # food availability -----
    # print(paste("Working on bar chart for food availability for", i))
    # foodAvail.out <- aggNorder(gdxChoice, DTglobal = "dt.foodAvail.foodGroup", aggChoice = i, get(l))
    # plotByRegionBar(dt = foodAvail.out, fileName = "foodAvail.foodGroup", plotTitle = "Food availability by food group",
    #                 yLab = "(grams)", yRange = c(0, 100), aggChoice = i)

    # nonstaple share of kcals -----
    print(paste("Working on bar chart for the nonstaple share of kcals for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.KcalShare.nonstaple", aggChoice = i, get(l))
    ylab <- "(percent)"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = "nonStapleShare", plotTitle = "Non-staple share of energy",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
 #   yLab = "(percent)", yRange = c(0, 100), aggChoice = i, oneLine = FALSE)

    # nutrition benefit score -----
    print(paste("Working on bar chart for the NBS for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.nutBalScore", aggChoice = i, get(l))
    ylab <- "" # this creates the ylab variable and leaves it empty. NULL deletes it!
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = "NutBalScore", plotTitle = "Nutrient balance score",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
 #   yLab = NULL, yRange = c(0, 100), aggChoice = i, oneLine = FALSE)

    # composite QI score -----
    print(paste("Working on bar chart for the QI composite for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.compQI", aggChoice = i, get(l))
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = "compQI", plotTitle = "Composite qualifying index",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
  #  yLab = NULL, yRange = c(0, 100), aggChoice = i, oneLine = FALSE)

    # composite DI score -----
    print(paste("Working on bar chart for the NBS for", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.compDI", aggChoice = i, get(l))
    ylab <- ""
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
    plotByRegionBar(dt = DT, fileName = "compDI", plotTitle = "Composite disqualifying index",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
#    yLab = NULL, yRange = c(0, 100), aggChoice = i, oneLine = FALSE)

    # stacked kcal bar chart ----
    print(paste("Working on stacked kcal bar chart for ", i))
    DT <- aggNorder(gdxChoice, DTglobal = "dt.kcals.values", aggChoice = i, get(l))
    DT <- DT[nutrient %in% c("sugar_g", "ft_acds_tot_sat_g"), ]
    ylab <- "(Kcals)"
    if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, round(max(DT$value)) )}
    plotByRegionStackedBar(dt = DT, fileName = "kcals.values", plotTitle = "Average daily dietary energy by source",
                    yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)

  }

  # food groups -----
  print(paste("Working on dt.foodAvail.foodGroup"))
  temp.in <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))
  temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

  #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
  temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
                       year == "X2050",][year == "X2010", scenario := "2010"]
  foodGroupList <- unique(temp.in$food_group_code)
  for (i in aggChoiceListBarChart) {
    for (j in foodGroupList) {
      #     for (j in c(macroNutrients, vitamins, minerals, fattyAcids, others)) {
      FG.shortName <- j
      FG.longName <- cleanupNutrientNames(j)
      units <- "g"
      DT <- data.table::copy(temp.in)
      if (units == "g") units <- "grams"
      DT <- DT[food_group_code %in% j,]
      DT[, food_group_code := NULL]
      dt.regions <- regionAgg(i)
      # aggregate to and retain only the relevant regions
      merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
      #      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code")]
      keepListCol <- c("scenario", "region_code", "region_name", "value")
      DT <- unique(merged[, (keepListCol), with = FALSE])
      if (gdxChoice == "USAID") DT <- renameUSAIDscenarios(DT)

      DT <- DT[scenario %in% get(l), ]
      #     if (gdxChoice == "SSPs") {
      # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
      # DT <- DT[scenario %in% get(l), ]
      # DT[, scenarioOrder := match(scenario, scenOrder.SSPs)]
      # data.table::setorder(DT, scenarioOrder)
      # DT[, scenarioOrder := NULL]
      # #      }
      if (gdxChoice == "USAID")  {
        # scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
        # scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
        # scenarioList.addEnhance <- c("RPHL", "RMM")
        # scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
        #       scenario.base <- "REF_HGEM"

        # keep only needed USAID scenarios
        scenOrder <- c("2010", l)
        DT <- DT[scenario %in% get(l), ]
        scenChoice.name <- l
      }

      # order scenarios, first write the number into the number variable scenarioOrder
      DT[, scenarioOrder := match(scenario, scenOrder)]
      data.table::setorder(DT, scenarioOrder)
      DT[, scenarioOrder := NULL]

      DT <- orderRegions(DT, i)
      nutTitle <- paste("Average daily availability of ", tolower(FG.longName), sep = "")
      ylab = paste("(",units,")",sep = "")
      if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
      plotByRegionBar(dt = DT, fileName = paste(j, "foodAvail_foodGroup", sep = "_"), plotTitle = nutTitle,
                      yLab = ylab, yRange = yRangeMinMax, aggChoice = i,  scenOrder, oneLine = FALSE, colorList)
#      yRange = c(0, 80), aggChoice = i, oneLine = FALSE)
print(j)
    }
  }
  cat(paste("Done with food groups\n\n"))

  # several nutrients -----
  for (k in 1:length(multipleNutsFileList)) {
    print(paste("Working on multiple nut file", multipleNutsFileList[k]))
    temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
    temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

    #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
    temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
                         year == "X2050",][year == "X2010", scenario := "2010"]
    for (i in aggChoiceListBarChart) {
      for (j in unique(temp.in$nutrient)) {
        #     for (j in c(macroNutrients, vitamins, minerals)) {
        nutshortName <- cleanupNutrientNames(j)
        # if (j %in% c("kcals.ethanol", "kcals.fat", "kcals.carbohydrate","kcals.protein", "kcals.sugar")) {
        #     nutlongName <- "energy"
        #     units <- "kilocalories"
        # } else {
        nutlongName <- dt.nutrientNames_Units[1, (j)]
        units <- dt.nutrientNames_Units[2, (j)]
        # }
        DT <- data.table::copy(temp.in)
        if (units == "g") units <- "grams"
        DT <- DT[nutrient %in% j,]
        DT[, nutrient := NULL]
        dt.regions <- regionAgg(i)
        # aggregate to and retain only the relevant regions; region code is the code for the region
        merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
        merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
        keepListCol <- c("scenario", "region_code", "region_name", "value")
        DT <- unique(merged[, (keepListCol), with = FALSE])
        if (gdxChoice == "USAID") DT <- renameUSAIDscenarios(DT)

        DT <- DT[scenario %in% get(l), ]
        #     if (gdxChoice == "SSPs") {
        # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
        # DT <- DT[scenario %in% get(l), ]
        # DT[, scenarioOrder := match(scenario, scenOrder.SSPs)]
        # data.table::setorder(DT, scenarioOrder)
        # DT[, scenarioOrder := NULL]
        # #      }
        if (gdxChoice == "USAID")  {
          # scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
          # scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
          # scenarioList.addEnhance <- c("RPHL", "RMM")
          # scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
          #       scenario.base <- "REF_HGEM"

          # keep only needed USAID scenarios
          scenOrder <- c("2010", l)
          DT <- DT[scenario %in% get(l), ]
          scenChoice.name <- l
        }

        # order scenarios, first write the number into the number variable scenarioOrder
        DT[, scenarioOrder := match(scenario, scenOrder)]
        data.table::setorder(DT, scenarioOrder)
        DT[, scenarioOrder := NULL]
        #     }

        DT <- orderRegions(DT, i)
        reqRatioFiles <- c( "RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio", "dt.badRatios" )
        if (multipleNutsFileList[k] == "dt.nutrients.sum.all")  {
          nutTitle <- paste("Average daily availability of ", tolower(nutlongName), sep = "")
          ylab = paste("(",units,")",sep = "")
          drawOneLine = FALSE
        }
        if (multipleNutsFileList[k] %in% c("RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio"))  {
          nutTitle <- nutshortName
          ylab = "(Adequacy ratio)"
          drawOneLine = TRUE
        }
        if (multipleNutsFileList[k] %in% c("dt.MRVRatios"))  {
          nutTitle <- paste("Ratio of nutrient to MRV for , ", nutshortName, sep = "")
          ylab = "(Maximal reference value)"
          drawOneLine = TRUE
        }

         if (multipleNutsFileList[k] %in% "AMDR_hi_sum_reqRatio")  {
           nutTitle <- paste("AMDR high, ", nutshortName, sep = "")
           ylab = "(AMDR, high)"
           drawOneLine = TRUE
         }
         if (multipleNutsFileList[k] %in% "AMDR_lo_sum_reqRatio")  {
           nutTitle <- paste("AMDR low, ", nutshortName, sep = "")
           ylab = "(AMDR, low)"
           drawOneLine = TRUE
         }
        if (multipleNutsFileList[k] == "dt.nutrients.nonstapleShare")  {
                    nutTitle <- paste("Non-staple share of ", nutshortName, " availability", sep = "")
          ylab = "(percent)"
          drawOneLine = FALSE
        }
        if (ylab %in%  "(percent)") {yRangeMinMax <- c(0,100)} else {yRangeMinMax <- c(0, max(DT$value) )}
        plotByRegionBar(dt = DT, fileName = paste(j, multipleNutsListShortName[k], sep = "_"),
                        plotTitle = nutTitle, yLab = ylab, yRange = yRangeMinMax, aggChoice = i,
                        scenOrder, oneLine = drawOneLine, colorList)
      }
    }
    print(paste("Done with ", multipleNutsFileList[k]))
#    cat("\n\n")
  }
} # end of scenChoiceList loop

# box plot of budget share in 2050 new ----
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
#dt.regions <- regionAgg("WB")
# aggregate to and retain only the relevant regions
dt.budgetShare <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
keepListCol.incShare <- c("scenario" ,"year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0")
dt.budgetShare <- dt.budgetShare[, (keepListCol.incShare), with = FALSE]
# get rid of Somalia because it's budget share is 500 * per cap income
dt.budgetShare <- dt.budgetShare[!region_code.IMPACT159 == "SOM",]
scenario.base <- "SSP2-NoCC-REF"
dt.budgetShare <- dt.budgetShare[year == "X2050" & scenario == scenario.base]
dt.budgetShare[, c("year") := NULL]
DT <- dt.budgetShare

ylab <- "(percent)"
filename <- "budgetShareBoxPlot"
plottitle = "IMPACT food budget share of 2050 per capita income"
for (i in aggChoiceListBarChart) {
#  DTalt <- aggNorder(gdxChoice, DTglobal = "dt.budgetShare", aggChoice = i, get(l))

plotByBoxPlot2050(dt = DT, fileName = filename, plotTitle = plottitle, yLab = ylab, yRange = c(0,50), aggChoice = i )
}
# the method commented out below to get the box plots stats doesn't work with geom_boxplot. The following link has a
# way to do this but I'm not going to implement right now.
# http://stackoverflow.com/questions/23970118/how-to-add-summary-information-to-geom-boxplot

# box.stats <- data.table::as.data.table(box.test$stats)
# box.stats.labels <- data.table(rownum = c(1,2,3,4,5), name = c("lowest value", "box bottom", "median", "box top", "highest value"))
# # do this name change so the box stats can be rbinded with the other data
# box.stats <- cbind(box.stats.labels, box.stats)
# data.table::setnames(box.stats,
#                      old = c("name", "V1", "V2", "V3", "V4"),
#                      new = c("scenario", "lowInc", "lowMidInc", "upMidInc", "highInc"))
# fwrite(box.stats, file = "graphics/boxstats_WB.csv")
# box plots, old code -----
# dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
# dt.regions <- regionAgg("WB")
# # aggregate to and retain only the relevant regions
# dt.budgetShare <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
# keepListCol.incShare <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0")
# dt.budgetShare <- dt.budgetShare[, (keepListCol.incShare), with = FALSE]
# scenario.base <- "SSP2-NoCC-REF"
# dt.budgetShare <- dt.budgetShare[year == "X2010" & scenario == scenario.base |
#                                    year == "X2050",]
# dt.budgetShare <- dt.budgetShare[year == "X2010", scenario := "2010"][, year := NULL]
# # get rid of Somalia because it's budget share is 500 * per cap income
# dt.budgetShare <- dt.budgetShare[!region_code.IMPACT159 == "SOM",]
# temp <- dt.budgetShare
# WBregionLevels <- c("lowInc", "lowMidInc", "upMidInc", "highInc")
# temp[,region_code := factor(region_code, levels = WBregionLevels)]
# # sort by the region codes
# temp <- temp[order(region_code),]
# yRange <- range(temp$incSharePCX0)
# pdf(paste("graphics/budgetShareBoxPlot", ".pdf", sep = ""))
# box.test <- boxplot(incSharePCX0 ~ region_code, data = temp, range = 0,
#                     #                    at = c(1, 2, 4, 5, 6),
#                     xaxt = 'n',
#                     ylim = yRange,
#                     ylab = "(percent)",
#                     col = c('white', 'white smoke', 'gray'))
# axis(side = 1,
#      at = c(1, 2, 3, 4),
#      labels = FALSE)
# labels <- gsub("-REF","", unique(temp$region_name))
# #    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
# #   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
# title('Expenditures share of per capita income, 2050\nall countries, all scenarios')
# text(
#   c(1, 2, 3, 4),
#   par("usr")[3] - 0.1, srt = 45, adj = 1.2,
#   labels = labels, xpd = TRUE,  cex = 0.6, cex.axis = 0.6)
# # abline(h = 1, lty = 3, lwd = 0.8)
# dev.off()


# line plots -----
#this is not working right now
# plotByRegionLine("dt.shannonDiversity", "ShannonDiversity", "Shannon Diversity", yRange = c(20, 80), "I3regions")

# write out zip files of the data files in resultsDir
for (i in multipleNutsFileList) {
  print(paste("writing zip file for", i))
  inDT <- getNewestVersion(i, fileloc("resultsDir"))
  write.csv(inDT, file = gzfile(paste("graphics/",i,".csv.zip", sep = "")))
}

#create zip file of all the graphics outputs for a set of scenarios
graphs.fileNames <- list.files(path = "graphics")
# the extras options gets rid of all the messages and replaces them with a dot every 10 MB
zip(zipfile = paste(fileloc("gDir"),"/", scenChoice.name,".zip", sep = ""),
                     files = paste(fileloc("gDir"),"/", graphs.fileNames, sep = ""), extras = "-qdgds 10m")

# csv to table code  -----
# list of potential nutrients to add to the table
macroNutrients <- c("carbohydrate_g", "protein_g", "fat_g",  "totalfiber_g")
vitamins <- c( "folate_µg", "thiamin_mg", "niacin_mg", "riboflavin_mg",
               "vit_b6_mg", "vit_a_rae_µg", "vit_b12_µg","vit_c_mg",
               "vit_d_µg", "vit_e_mg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")

nutlistmacro <- c("carbohydrate_g", "protein_g",  "totalfiber_g") # fat is excluded here
# nutlistminrls <- c("calcium_mg", "magnesium_mg", "potassium_g", "phosphorus_mg")
# nutlistvits <- c("folate_µg", "riboflavin_mg", "vit_a_rae_µg","vit_b12_µg", "vit_c_mg", "vit_e_mg",  "vit_d_µg",
#                  "vit_k_µg", "thiamin_mg")
diversity.1 <- c("nonStapleShare", "RAOqe")
diversity.2 <- c("NutBalScore", "compDI", "compQI")
budgetShare <- "budgetShare"
nonStapleShareKcals <- "nonStapleShare"
#boxStats <- "boxstats"
dailyAvail.foodgroup <- foodGroupList

csvHolder <- data.table::data.table(scenario = character(0),
                                    # "Low Income" = numeric(0), "Low middle income" = numeric(0),
                                    # "Upper middle income" = numeric(0), "High income" = numeric(0),
                                    # "2050 climate change effect, low income" = numeric(0),
                                    # "2050 climate change effect, low middle income" = numeric(0),
                                    # "2050 climate change effect, upper middle income" = numeric(0),
                                    # "2050 climate change effect, high income" = numeric(0))
                                    lowInc = numeric(0), lowMidInc = numeric(0), upMidInc = numeric(0), highInc = numeric(0),
                                    DlowInc = numeric(0), DlowMidInc = numeric(0), DupMidInc = numeric(0), DhighInc = numeric(0))

incCats <- c("lowInc", "lowMidInc", "upMidInc", "highInc")
DincCats <- c("DlowInc", "DlowMidInc", "DupMidInc", "DhighInc")
scen2050list <- c("SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
#create  xlsx file with the data used to create the figures -----
figsData <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb = figsData, sheetName = "FigureData")
colHeaders <- c("Low Income", "Low middle income", "Upper middle income", "High income",
                "2050 climate change effect, low income", "2050 climate change effect, low middle income",
                "2050 climate change effect, upper middle income", "2050 climate change effect, high income")
rowCounter <- 1

#write column names to the spreadsheet
openxlsx::writeData(
  wb = figsData, sheet = "FigureData", rowNames = FALSE, colNames = TRUE, startCol = 1,
  x = as.list(colHeaders),
  startRow = rowCounter
)
rowCounter <- rowCounter + 1
#boxstats removed for now
#for (i in c(budgetShare, boxStats, nutlistmacro, vitamins, minerals, diversity.1, diversity.2, dailyAvail.foodgroup)) {
  for (i in c(budgetShare, nutlistmacro, vitamins, minerals, diversity.1, diversity.2, dailyAvail.foodgroup)) {
 #   if (i %in% c(budgetShare, boxStats)) {
      if (i %in% c(budgetShare)) {
        fileName <- paste(i, "WB", sep = "_")
    figInfo <- "1, affordability,"
  }
  if (i %in% dailyAvail.foodgroup) {
    fileName <- paste(i, "foodAvail_foodGroup_WB", sep = "_")
    figInfo <- "2, food group availability, "
  }
  if (i %in% nutlistmacro) {
    fileName <- paste(i, "macro_reqRatio_WB", sep = "_")
    figInfo <- "3, adequacy, macro nutrients, "
  }
  if (i %in% vitamins) {
    fileName <- paste(i, "vits_reqRatio_WB", sep = "_")
    figInfo <- "4, adequacy, vitamins, "
  }
  if (i %in% minerals) {
    fileName <- paste(i, "minrls_reqRatio_WB", sep = "_")
    figInfo <- "4, adequacy, minerals, "
  }
  # if (i %in% nutlistbioavail) {
  #   fileName <- paste(i, "bioavail_reqRatio_WB", sep = "_")
  #   figInfo <- "4, adequacy, minerals, "
  # }
  if (i %in% nonStapleShareKcals) {
    fileName <- paste(i, "WB", sep = "_")
    figInfo <- "5, diversity metrics, "
  }
  if (i %in% diversity.1) {
    fileName <- paste(i, "WB", sep = "_")
    figInfo <- "5, diversity metrics, "
  }
  if (i %in% diversity.2) {
    fileName <- paste(i, "WB", sep = "_")
    figInfo <- "2, Nutrient balance metrics, "
  }

  fileIn <- data.table::fread(paste("graphics/", fileName, ".csv", sep = ""), select = 2:6)
  for (j in scen2050list) {
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == "SSP2-NoCC", get(incCats[k])]
      fileIn[scenario == j, DincCats[k] := (get(incCats[k]) - baseVal) / baseVal]
    }
  }
  #write name of data set to the spreadsheet
  nutshortName <- cleanupNutrientNames(i)
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

  category <- data.table::data.table(scenario = i,
                                     lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
                                     DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  csvHolder <- rbind(csvHolder,category)
  csvHolder <- rbind(csvHolder, fileIn)
}

data.table::setnames(csvHolder,
                     old = c("lowInc", "lowMidInc", "upMidInc", "highInc", "DlowInc", "DlowMidInc", "DupMidInc", "DhighInc"),
                     new = colHeaders)

openxlsx::addStyle(
  wb = figsData, sheet = "FigureData", style = numStyle, cols = 2:length(csvHolder), rows = 2:nrow(csvHolder),
  gridExpand = TRUE
)

openxlsx::saveWorkbook(wb = figsData, file = paste("graphics/reqTable.xlsx", sep = ""),
                       overwrite = TRUE)
data.table::fwrite(csvHolder, file = paste("graphics/reqTable.csv", sep = ""), na = "")

# source("R/aggregateResults.R") # is this still necessary?
