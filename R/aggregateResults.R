#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title aggregate country results to different aggregation units
#' @name aggRun.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
library(data.table)
print("Aggregating results in aggregateResults.R")
# gdxChoice values are either SSPs or USAID
gdxChoice <- "SSPs"
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA.macro.sum.reqRatio, RDA.minrls.sum.reqRatio, RDA.vits.sum.reqRatio
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

# scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
# scenChoice for SSPs is scenarioList.SSPs
scenario.base <- "SSP2-NoCC-REF"; if (gdxChoice == "USAID") scenario.base <- "SSP2-NoCC-NA"

aggChoiceListBarChart <- c("WB", "AggReg1") # missing "tenregions", AggReg2 and  "2EconGroup
multipleNutsFileList <- c("dt.nutrients.sum.all", "RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio",
                          "dt.nutrients.nonstapleShare", "dt.energy_ratios", "PR.iron_sum_reqRatio", "PR.zinc_sum_reqRatio")
multipleNutsListShortName <- c("nutrients_avail", "macro_reqRatio", "minrls_reqRatio", "vits_reqRatio",
                               "nutrients_nonstaples_share", "energy_ratios")

# population for weighting -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
for (i in aggChoiceListBarChart) {
  for (j in multipleNutsFileList) {
    print(j)
    DT <- getNewestVersion(j, fileloc("resultsDir"))

    dt.regions <- regionAgg(i)
    # aggregate to and retain only the relevant regions
    temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
#    merged <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
    merged <- merge(temp, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))
    for (k in unique(DT$nutrient)) {
 #     merged <- merged[, paste("value",i,sep = ".") := weighted.mean(value, PopX0), by = c("scenario", "region_code.IMPACT159", "year", "nutrient")]
      merged <- merged[, paste("value",i,sep = ".") := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]

          }
    keepListCol <- c("scenario", "region_code", "region_name", "nutrient", "year", paste("value",i,sep = "."))
    DT <- unique(merged[, (keepListCol), with = FALSE])
    inDT <- DT
    outName <- paste(j,i, sep = ".")
    cleanup(inDT, outName, fileloc("resultsDir"), "csv")
  }
}
