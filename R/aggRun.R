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
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

# scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
# scenChoice for SSPs is scenarioList.SSPs

aggChoiceListBarChart <- c("tenregions", "WB", "AggReg1") # missing AggReg1 and  "2EconGroup
multipleNutsFileList <- c("dt.nutrients.sum.all", "RDA.macro.sum.req.ratio", "RDA.minrls.sum.req.ratio", "RDA.vits.sum.req.ratio",
                          "dt.nutrients.nonstapleShare") # "dt.energy.ratios" not included
multipleNutsListShortName <- c("nutrients.avail", "macro.req.ratio", "minrls.req.ratio", "vits.req.ratio",
                               "nutrients.nonstaples.share", "energy.ratios")
#nutrients grouping
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

gdxChoice <- "USAID"
# USAID scenario combo choices
prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
addEnhance <- c("RPHL", "RMM")
scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
#USAID scenario list choice
scenChoice <- prodEnhance
# choose name of base scenario
scenario.base <- "SSP2-NoCC-REF"; if (gdxChoice == "USAID") scenario.base <- "SSP2-NoCC-NA"

#for testing
aggChoice <- "WB"; DTglobal <- "dt.shannonDiversity"; aggChoice = "WB"

# population for weighting -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
dt.pop.2010.ref <- dt.pop[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]
"dt.nutrientNames_Units" <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))

for (i in aggChoiceListBarChart) {
  SD.out <- aggNorder(gdxChoice, DTglobal = "dt.shannonDiversity", aggChoice = i, scenChoice)
  plotByRegionBar(dt = SD.out, fileName = "ShannonDiversity", title = "Shannon Diversity", yLab = "percent", yRange = c(0, 80), aggChoice = i)
}

for (i in aggChoiceListBarChart) {
  budgetShare.out <- aggNorder(gdxChoice, DTglobal = "dt.budgetShare", aggChoice = i, scenChoice)
  plotByRegionBar(dt = budgetShare.out, fileName = "budgetShare", title = "IMPACT food budget share of per capita income", yLab = "percent", yRange = c(0, 50), aggChoice = i)
}
# do files with several nutrients
for (k in 1:length(multipleNutsFileList)) {
  temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
  #keep just the scenario.base scenario for 2010 and rename the scenario to 2010, then delete year column
  temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
             year == "X2050",]
  temp.in <- temp.in[year == "X2010", scenario := "2010"]
  temp.in <- temp.in[, year := NULL]
  shortName <- multipleNutsListShortName[k]

  for (i in aggChoiceListBarChart) {
    for (j in unique(temp.in$nutrient)) {
      #     for (j in c(macroNutrients, vitamins, minerals)) {
      nutshortName <- cleanupNutrientNames(j)
      # if (j %in% c("kcals.ethanol", "kcals.fat", "kcals.carbohydrate","kcals.protein", "kcals.sugar")) {
      #     nutlongName <- "energy"
      #     units <- "kilocalories"
      # } else {
      nutlongName <- dt.nutrientNames_Units[1,get(j)]
      units <- dt.nutrientNames_Units[2,get(j)]
      # }
      DT <- data.table::copy(temp.in)
      #      DT[, nutrient := cleanupNutrientNames(nutrient)]
      if (units == "g") units <- "grams"
      DT <- DT[nutrient %in% j,]
      DT[, nutrient := NULL]
      dt.regions <- regionAgg(i)
      # aggregate to and retain only the relevant regions
      temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
      merged <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code")]
      keepListCol <- c("scenario", "region_code", "region_name", "value")
      DT <- unique(merged[, (keepListCol), with = FALSE])

      if (gdxChoice == "SSPs") {
        # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
        scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
        DT <- DT[scenario %in% scenChoice, ] # doesn't need eval-parse because the list is defined inside the function
        DT[, scenarioOrder := match(scenario, scenOrder.SSPs)]
        data.table::setorder(DT, scenarioOrder)
        DT[, scenarioOrder := NULL]
      }
      if (gdxChoice == "USAID") {
        DT <- renameUSAIDscenarios(DT)
        scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
        scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
        scenarioList.addEnhance <- c("RPHL", "RMM")
        scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
        scenario.base <- "REF_HGEM"

        # keep only needed USAID scenarios
        scenOrder.USAID <- c("2010", scenChoice)
        DT <- DT[scenario %in% scenChoice, ] # only needed for the USAID results

         # order scenarios, first write the number into the number variable scenarioOrder
        DT[, scenarioOrder := match(scenario, scenOrder.USAID)]
        data.table::setorder(DT, scenarioOrder)
        DT[, scenarioOrder := NULL]
      }

      DT <- orderRegions(DT, i)
      reqRatioFiles <- c( "RDA.macro.sum.req.ratio", "RDA.minrls.sum.req.ratio", "RDA.vits.sum.req.ratio")
      if (multipleNutsFileList[k] == "dt.nutrients.sum.all")  nutTitle <- paste("Average daily availability of ", nutlongName, sep = "")
      if (multipleNutsFileList[k] %in% reqRatioFiles)  {
        nutTitle <- paste("Average availability as share of RDA, ", nutlongName, sep = "")
        units = "percent"
      }
      if (multipleNutsFileList[k] == "dt.nutrients.nonstapleShare")  {
        nutTitle <- paste("Non-staple share of ", nutlongName, " availability", sep = "")
        units = "percent"
      }
      plotByRegionBar(dt = DT, fileName = paste(j, shortName,"prodEnhance", sep = "."), title = nutTitle, yLab = paste("(", units, ")", sep = ""), yRange = NULL, aggChoice = i)
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

# write out zip files
for (i in multipleNutsFileList) {
  fileToWrite <- i
  inDT <- getNewestVersion(fileToWrite, fileloc("resultsDir"))
  write.csv(inDT, file = gzfile(paste(fileToWrite,".csv.zip", sep = "")))
}
