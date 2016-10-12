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
# gdxChoice values are either SSPs or USAID
gdxChoice <- "SSPs"
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA.macro.sum.req.ratio, RDA.minrls.sum.req.ratio, RDA.vits.sum.req.ratio
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB

# scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
# scenChoice for SSPs is scenarioList.SSPs
scenario.base <- "SSP2-NoCC-REF"; if (gdxChoice == "USAID") scenario.base <- "SSP2-NoCC-NA"

aggChoiceListBarChart <- c("WB", "AggReg1") # missing "tenregions", AggReg2 and  "2EconGroup
multipleNutsFileList <- c("dt.nutrients.sum.all", "RDA.macro.sum.req.ratio", "RDA.minrls.sum.req.ratio", "RDA.vits.sum.req.ratio",
                          "dt.nutrients.nonstapleShare", "dt.energy.ratios", "PR.iron.sum.req.ratio", "PR.zinc.sum.req.ratio")
multipleNutsListShortName <- c("nutrients.avail", "macro.req.ratio", "minrls.req.ratio", "vits.req.ratio",
                               "nutrients.nonstaples.share", "energy.ratios")

# population for weighting -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
#dt.pop.2010.ref <- dt.pop[year ==  "X2010" & scenario == scenario.base,][,c("scenario", "year") :=  NULL]
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
