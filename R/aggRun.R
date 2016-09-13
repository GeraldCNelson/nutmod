#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name aggRun.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
source("R/aggNorder.R")
library(data.table)
# gdxChoice values are either SSPs or USAID
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA.macro.sum.req.ratio, RDA.minrls.sum.req.ratio, RDA.vits.sum.req.ratio
# - dt.nutrients.sum.staples, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

# scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
# scenChoice for SSPs is scenarioList.SSPs

aggChoiceListBarChart <- c("tenregions", "WB", "AggReg1") # missing AggReg1 and  "2EconGroup
multipleNutsFileList <- c("dt.nutrients.sum.all", "RDA.macro.sum.req.ratio", "RDA.minrls.sum.req.ratio", "RDA.vits.sum.req.ratio",
                          "dt.nutrients.sum.staples") # "dt.energy.ratios" not included
multipleNutsListShortName <- c("nutrients.avail", "macro.req.ratio", "minrls.req.ratio", "vits.req.ratio",
                               "nutrients.sum.staples", "energy.ratios")
macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")
gdxChoice <- "SSPs"
DTglobal <- "dt.shannonDiversity"
aggChoice <- "WB"
scenChoice <- "SSPs"
for (i in aggChoiceListBarChart) {
  SD.out <- aggNorder(gdxChoice, DTglobal, aggChoice = i, scenChoice)
  plotByRegionBar(dt = SD.out, fileName = "ShannonDiversity", title = "Shannon Diversity", yLab = "percent", yRange = c(0, 80), aggChoice = i)
}

for (i in aggChoiceListBarChart) {
  budgetShare.out <- aggNorder(gdxChoice, DTglobal = "dt.budgetShare", aggChoice = i, scenChoice)
  plotByRegionBar(dt = budgetShare.out, fileName = "budgetShare", title = "IMPACT food budget share of per capita income", yLab = "percent", yRange = c(0, 50), aggChoice = i)
}

for (k in 1:length(multipleNutsFileList)) {
  temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
  shortName <- multipleNutsListShortName[k]

  for (i in aggChoiceListBarChart) {
    for (j in c(macroNutrients, vitamins, minerals)) {
      nutName <- cleanupNutrientNames(j)
      DT <- data.table::copy(temp.in)
#      DT[, nutrient := cleanupNutrientNames(nutrient)]
      units <- gsub(nutName, "", j);  units <- gsub("_", "", units)
      if (units == "g") units = "grams"
      if (units == "mg") units = "milligrams"
      if (units == "µg") units = "micrograms"
      DT <- DT[nutrient %in% nutName,]
      DT[, nutrient := NULL]
      dt.regions <- regionAgg(i)
      # aggregate to and retain only the relevant regions
      temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
      merged <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
      keepListCol <- c("scenario",  "year", "region_code", "region_name", "value")
      DT <- unique(merged[, (keepListCol), with = FALSE])
      #keep just the scenario.base scenario for 2010 and rename the scenario to 2010, then delete year column
      DT <- DT[year == "X2010" & scenario == scenario.base |
                 year == "X2050",]
      DT <- DT[year == "X2010", scenario := "2010"][, year := NULL]
      if (gdxChoice == "SSPs") {
        # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
        scenChoice <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
        DT <- DT[scenario %in% scenChoice, ] # doesn't need eval-parse because the list is defined inside the function
        DT[, scenarioOrder := match(scenario, scenChoice)]
        data.table::setorder(DT, scenarioOrder)
        DT[, scenarioOrder := NULL]
      }
      DT <- orderRegions(DT, i)
      reqRatioFiles <- c( "RDA.macro.sum.req.ratio", "RDA.minrls.sum.req.ratio", "RDA.vits.sum.req.ratio")
      if (multipleNutsFileList[k] == "dt.nutrients.sum.all")  nutTitle <- paste("Average daily availability of ", nutName, sep = "")
      if (multipleNutsFileList[k] %in% reqRatioFiles)  {
        nutTitle <- paste("Average availability as share of RDA, ", nutName, sep = "")
        units = "percent"
      }
      if (multipleNutsFileList[k] %in% "dt.nutrients.sum.staples")  {
        nutTitle <- paste("Staple share of , ", nutName, " availability", sep = "")
        units = "percent"
      }
      plotByRegionBar(dt = DT, fileName = paste(j, shortName, sep = "."), title = nutTitle, yLab = paste("(", units, ")", sep = ""), yRange = NULL, aggChoice = i)
    }
  }
}

# nutsSum.out <- aggNorder(gdxChoice, DTglobal = " dt.nutrients.sum.all", aggChoice, scenChoice)
# # RDA.macro.sum.req.ratio has ratio of avail to adequacy for mineral nutrients; columns are years
# macroReqRatio.out <- aggNorder(gdxChoice, DTglobal = "RDA.macro.sum.req.ratio", aggChoice, scenChoice)
# # RDA.minrls.sum.req.ratio has ratio of avail to adequacy for mineral nutrients; columns are years
# minrlsReqRatio.out <- aggNorder(gdxChoice, DTglobal = "RDA.minrls.sum.req.ratio", aggChoice, scenChoice)
# # dt.nutrients.sum.staples has a year column, staple code column and a column for each of the nutrients
# # dt.energy.ratios has share of energy from carbohydrates, energy share (always 1), ethanol, fat, protein, carbohydrates, sugar
# energyRatios.out <- aggNorder(gdxChoice, DTglobal = "dt.energy.ratios", aggChoice, scenChoice)


plotByRegionLine(dt.shannonDiversity, "ShannonDiversity", "Shannon Diversity", yRange = c(20, 80), "I3regions")
plotByRegionBar(dt = SD.out, fileName = "ShannonDiversity", title = "Shannon Diversity", yLab = "percent", yRange = c(0, 80), aggChoice = "WB")

