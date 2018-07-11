library(RColorBrewer)
source("R/nutrientModFunctions.R")
sourceFile <- "USAIDPriorities.R"
createScriptMetaData()

# year(s) to save for results report
keepListYear <- c("X2030")
keepListSSPScen <- "SSP2"
# list of crops with produtivity increase
cropList <- c("cbana", "cbarl", "cbean", "ccass", "cchkp", "ccowp", "cgrnd",
              "clent", "cmaiz", "cmill", "copul", "cpigp", "cplnt", "cpota",
              "crice", "csorg", "csoyb", "cswpt", "cwhea", "cyams")
cropNames <- c("Banana", "Barley", "Beans", "Cassava", "Chickpea", "Cowpea", "Groundnuts",
               "Lentils", "Maize", "Millet", "Other pulses", "Pigeonpea", "Plantain", "Potato",
               "Rice", "Sorghum", "Soybean", "Sweet potato", "Wheat", "Yam")
cropCategory <- c("Roots, Tubers & Bananas", "Cereal Grains", "Oilseeds & Pulses", "Roots, Tubers & Bananas", "Oilseeds & Pulses", "Oilseeds & Pulses", "Oilseeds & Pulses",
                  "Oilseeds & Pulses", "Cereal Grains", "Cereal Grains", "Oilseeds & Pulses", "Oilseeds & Pulses", "Roots, Tubers & Bananas", "Roots, Tubers & Bananas",
                  "Cereal Grains", "Cereal Grains", "Oilseeds & Pulses", "Roots, Tubers & Bananas", "Cereal Grains", "Roots, Tubers & Bananas")
cropInfo <- data.table(cropList = cropList, cropNames = cropNames, cropCategory = cropCategory)

keepListNuts.macroMinrls <- c(keyVariable("macronutrients"), keyVariable("minerals"))
keepListNuts.macroMinrls <- keepListNuts.macroMinrls[!keepListNuts.macroMinrls %in% "fat_g"]
keepListNuts.vits <- keyVariable("vitamins")
keepListNuts.vits <- keepListNuts.vits[!keepListNuts.vits %in% c("riboflavin_mg", "thiamin_mg" )]
keepListNuts <- c(keepListNuts.macroMinrls, keepListNuts.vits)

# list of scenarios in the analysis
scenarioList <- c("SSP2-HGEM-cf", paste0("SSP2-HGEM-", cropList))
scenarioList.crops <- scenarioList[!scenarioList %in% "SSP2-HGEM-cf"]
cropRatios <- paste(scenarioList.crops, "ratio", sep = ".")
beginningCols <- c("region_code.IMPACT159", "year")

# options for regions to aggregate over
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
aggregationRegionChoices <- c("region_code.WB.income","region_name.WB.income",
                              "region_code.WB.spatial","region_name.WB.spatial",
                              "region_code.Africa","region_name.Africa", "region_code.EconGroup", "region_name.EconGroup",
                              "region_code.smallGroup", "region_name.smallGroup",
                              "region_code.MDIreg1","region_name.MDIreg1", "region_code.MDIreg2", "region_name.MDIreg2" )
keepListCol <- c("region_code.IMPACT159","region_name.IMPACT159",
                 "region_code.WB.income","region_name.WB.income",
                 "region_code.WB.spatial","region_name.WB.spatial",
                 "region_code.Africa","region_name.Africa", "region_code.EconGroup", "region_name.EconGroup",
                 "region_code.smallGroup", "region_name.smallGroup",
                 "region_code.MDIreg1","region_name.MDIreg1", "region_code.MDIreg2", "region_name.MDIreg2")
dt.regions.all[, setdiff(names(dt.regions.all), keepListCol) := NULL]
dt.regions.all <- unique(dt.regions.all)

# load the data
# get adequacy data by nutrient groups
reqRatio_sum_RDA_macro.var <- getNewestVersion("reqRatio_sum_RDA_macro.var", fileloc("resultsDir"))
#reqRatio_sum_RDA_macro.var[, nutrientType := "macro"]
reqRatio_sum_RDA_minrls.var <- getNewestVersion("reqRatio_sum_RDA_minrls.var", fileloc("resultsDir"))
#reqRatio_sum_RDA_minrls.var[, nutrientType := "minrls"]
reqRatio_sum_RDA_vits.var <- getNewestVersion("reqRatio_sum_RDA_vits.var", fileloc("resultsDir"))
#reqRatio_sum_RDA_vits.var[, nutrientType := "vits"]
reqRatio.RDA.all.var <- rbind(reqRatio_sum_RDA_macro.var, reqRatio_sum_RDA_minrls.var, reqRatio_sum_RDA_vits.var)

# food availability by food group
dt.foodAvail_foodGroup.var <- getNewestVersion("dt.foodAvail_foodGroup.var", fileloc("resultsDir"))

# nutrient availability by food group_by
dt.nutrients_sum_FG.var <- getNewestVersion("dt.nutrients_sum_FG.var", fileloc("resultsDir"))

# Rao diversity metric
dt.RAOqe.var <- getNewestVersion("dt.RAOqe.var", fileloc("resultsDir"))

# kcal share from nonstaples. This file also has other columns. value is the share column, in percent
dt.KcalShare_nonstaple.share.var <- getNewestVersion("dt.KcalShare_nonstaple.var", fileloc("resultsDir"))

# keep just the keepListYear of years
reqRatio.RDA.all.var <- reqRatio.RDA.all.var[year %in% keepListYear]
dt.foodAvail_foodGroup.var <- dt.foodAvail_foodGroup.var[year %in% keepListYear]
dt.nutrients_sum_FG.var <- dt.nutrients_sum_FG.var[year %in% keepListYear]
dt.RAOqe.var <- dt.RAOqe.var[year %in% keepListYear]
dt.KcalShare_nonstaple.share.var <- dt.KcalShare_nonstaple.share.var[year %in% keepListYear]
keepListCol <- c("region_code.IMPACT159", "scenario", "year", "staple_code",  "value")
dt.KcalShare_nonstaple.share.var[, setdiff(names(dt.KcalShare_nonstaple.share.var), keepListCol) := NULL]

# combine with region alternatives
reqRatio.RDA.all.var <- merge(reqRatio.RDA.all.var, dt.regions.all, by = "region_code.IMPACT159", allow.cartesian=TRUE)
dt.foodAvail_foodGroup.var <- merge(dt.foodAvail_foodGroup.var, dt.regions.all, by = "region_code.IMPACT159", allow.cartesian=TRUE)
dt.nutrients_sum_FG.var <- merge(dt.nutrients_sum_FG.var, dt.regions.all, by = "region_code.IMPACT159", allow.cartesian=TRUE)
dt.RAOqe.var <- merge(dt.RAOqe.var, dt.regions.all, by = "region_code.IMPACT159", allow.cartesian=TRUE)
dt.KcalShare_nonstaple.share.var <- merge(dt.KcalShare_nonstaple.share.var, dt.regions.all, by = "region_code.IMPACT159", allow.cartesian=TRUE)

baseCol <- "SSP2-HGEM-cf"
ratioCalc <- function(DT, outName, sumCol, byCol, desc) {
  regionFormula <- paste("+", aggregationRegionChoices)
  regionFormula <- paste(regionFormula, collapse = " " )
  beginningColsFormula <- paste(beginningCols, "+")
  beginningColsFormula <- paste(beginningColsFormula, collapse = " " )

  if (missing(sumCol) & missing(byCol)) {shareFormula <- paste(beginningColsFormula, regionFormula, "~ scenario")
  } else if (missing(byCol)) {shareFormula <- paste(beginningColsFormula, sumCol, regionFormula, "~ scenario")
  } else {shareFormula <- paste(beginningColsFormula, sumCol," + ", byCol, regionFormula, "~ scenario")}

  # cat(shareFormula)
  DT.wide <- dcast(data = DT,
                   formula = shareFormula,
                   value.var = "value")

  # % increase in adequacy, availability, etc.
  if (missing(byCol)) {
    inDT <- DT.wide[, (cropRatios) := lapply(.SD, function(x) (-1 + x / get(baseCol)) * 100), .SDcols = (scenarioList.crops)]
  }else{
    inDT <- DT.wide[, (cropRatios) := lapply(.SD, function(x) (-1 + x / get(baseCol)) * 100), .SDcols = (scenarioList.crops), by = byCol]
  }

  if (missing(byCol)) newColOrder <- c(beginningCols, sumCol, baseCol, scenarioList.crops, cropRatios, aggregationRegionChoices)
  if (missing(sumCol)) newColOrder <- c(beginningCols, baseCol, scenarioList.crops, cropRatios, aggregationRegionChoices)
  if (exists("byCol")) {
    if (!(sumCol %in% byCol)) {
      newColOrder <- c(beginningCols, sumCol, byCol, baseCol, scenarioList.crops, cropRatios, aggregationRegionChoices)}
    else {
      newColOrder <- c(beginningCols, sumCol, baseCol, scenarioList.crops, cropRatios, aggregationRegionChoices)}
  }

  # cat( names(DT.wide)[!names(DT.wide) %in% beginningCols])
  setcolorder(DT.wide, new = newColOrder)

  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
}

desc <- "Change in the adequacy ratio from productivity investments"
ratioCalc(DT = reqRatio.RDA.all.var, sumCol = "nutrient", byCol = NULL, outName = "increase.reqRatio.RDA.all.var", desc = desc)

desc <- "Change in food availability by food groups from productivity investments"
ratioCalc(DT = dt.foodAvail_foodGroup.var, sumCol = "food_group_code", byCol = "food_group_code", outName = "increase.dt.foodAvail_foodGroup.var", desc = desc)

# desc <- "Change in the RAO diversity index from productivity investments"
# ratioCalc(DT = dt.RAOqe.var, sumCol = "value", outName = "increase.dt.RAOqe.var", desc = desc)

desc <- "Change in nutrient availability by food groups from productivity investments"
ratioCalc(DT = dt.nutrients_sum_FG.var, sumCol = "nutrient", byCol = "food_group_code", outName = "increase.dt.nutrients_sum_FG.var", desc = desc)
# note. kcal share from nonstaples by food group changes even for groups like alcohol because the total kcals (the denominator) change by scenario

desc <- "Change in the Kcal share from nonstaples from productivity investments"
ratioCalc(DT = dt.KcalShare_nonstaple.share.var, sumCol = "staple_code", byCol = NULL, outName = "increase.dt.KcalShare_nonstaple.share.var", desc = desc)

#' population data set used for weighting by population -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
dt.pop <- dt.pop[year %in% keepListYear]
#  merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]

# sum by some regions
# reqRatiosSum <- paste0(scenarioList.crops, ".sum")
# increase.reqRatio.RDA.all.var <- getNewestVersion("increase.reqRatio.RDA.all.var", fileloc("resultsDir"))
#
# increase.reqRatio.RDA.all.var <- merge(increase.reqRatio.RDA.all.var, dt.pop, by = c("region_code.IMPACT159"))
# increase.reqRatio.RDA.all.var <- increase.reqRatio.RDA.all.var[, value.agg := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year", "nutrient")]
# increase.reqRatio.RDA.all.var[, (reqRatiosSum) := sum(scenarioList.crops), by = ]

# source("R/aggNorder.R")

worldMap <- getNewestVersion("worldMap", fileloc("uData")) # run storeWorldMapDF() if this is not available

increase.dt.KcalShare_nonstaple.share.var <- getNewestVersion("increase.dt.KcalShare_nonstaple.share.var", fileloc("resultsDir"))
increase.dt.KcalShare_nonstaple.share.var[, c("SSP2-HGEM-cf", scenarioList.crops, aggregationRegionChoices) := NULL]

increase.reqRatio.RDA.all.var <- getNewestVersion("increase.reqRatio.RDA.all.var", fileloc("resultsDir"))
increase.reqRatio.RDA.all.var[, c(scenarioList.crops, aggregationRegionChoices) := NULL]
increase.reqRatio.RDA.all.var <- increase.reqRatio.RDA.all.var[nutrient %in% keepListNuts]
increase.reqRatio.RDA.all.var <- melt(increase.reqRatio.RDA.all.var,
                                      id.vars = c("region_code.IMPACT159", "year", "nutrient"),
                                      variable.name = "scenario",
                                      measure.vars = c("SSP2-HGEM-cf", cropRatios),
                                      value.name = "value",
                                      variable.factor = FALSE)
# work on facetmaps for 2030 adequacy ratios
DTorig <- increase.reqRatio.RDA.all.var[scenario %in% "SSP2-HGEM-cf"]
facetColName <- "nutrient"
legendText <- "Adequacy ratio, 2030"
fillLimits <- c(0, 3)

# do facetmap for macronutrients and minerals base 2030 values
for (i in c("keepListNuts.macroMinrls", "keepListNuts.vits")) {
  DT <- copy(DTorig)
  DT <- DT[nutrient %in% get(i)]
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  nutListtemp = sort(unique(DT$nutrient))
  dt.nuts <- data.table(nutListtemp = nutListtemp, nutListtempclean = capwords(cleanupNutrientNames(nutListtemp)))
  DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
  DT[, nutrient := NULL]
  setnames(DT, old = "nutListtempclean", new = "nutrient")

  DT <- truncateDT(DT, fillLimits =  fillLimits)
  paletteType <- "Spectral"
  breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 0)
  # breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  graphsListHolder <- list()
  if (i %in% "keepListNuts.macroMinrls") filePart <- "macroMinrlsAR"
  if (i %in% "keepListNuts.vits") filePart <- "vitsAR"

  displayOrder <- capwords(cleanupNutrientNames(get(i))) # do this to keep the macro nutrients first
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
  fileName <- paste("facetmap", "_", filePart, "_", "2030", ".", "var", sep = "")
  facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
}

# do facet maps for the scenarios
DTorig <- increase.reqRatio.RDA.all.var[!scenario %in% "SSP2-HGEM-cf"]
DTorig <- countryCodeCleanup(DTorig) # converts IMPACT region codes to ISO3 codes for largest country in the region
fillLimits <- c(0, 0.5)
DTorig[, scenario := gsub("SSP2-HGEM-", "", scenario)][, scenario := gsub(".ratio", "", scenario)]

cropList <- gsub("SSP2-HGEM-", "", scenarioList.crops)
cropList <- gsub(".ratio", "", cropList)

for (j in cropList) {
  for (i in c("keepListNuts.macroMinrls", "keepListNuts.vits")) {
    DT <- copy(DTorig)
    DT <- DT[scenario %in% j]
    DT <- DT[nutrient %in% get(i)]

    nutListtemp = sort(unique(DT$nutrient))
    dt.nuts <- data.table(nutListtemp = nutListtemp, nutListtempclean = capwords(cleanupNutrientNames(nutListtemp)))
    DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
    DT[, nutrient := NULL]
    setnames(DT, old = "nutListtempclean", new = "nutrient")

    DT <- truncateDT(DT, fillLimits =  fillLimits)
    paletteType <- "Spectral"
    breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 1)
    # breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
    myPalette <- colorRampPalette(brewer.pal(11, paletteType))
    palette <- myPalette(4)
    graphsListHolder <- list()
    if (i %in% "keepListNuts.macroMinrls") filePart <- "macroMinrlsARDelta"
    if (i %in% "keepListNuts.vits") filePart <- "vitsARDelta"
    legendText <- "Change in adequacy ratio, 2030 (percent)"

    displayOrder <- capwords(cleanupNutrientNames(get(i)))
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
    fileName <- paste("facetmap", "_", j, "_", filePart, "_", "2030", ".", "var", sep = "")
    facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
  }
}

# do individual facet maps
for (j in cropList) {
  for (i in keepListNuts) {
    DT <- copy(DTorig)
    DT <- DT[scenario %in% j]
    DT <- DT[nutrient %in% i]

    nutListtemp = sort(unique(DT$nutrient))
    dt.nuts <- data.table(nutListtemp = nutListtemp, nutListtempclean = capwords(cleanupNutrientNames(nutListtemp)))
    DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
    DT[, nutrient := NULL]
    setnames(DT, old = "nutListtempclean", new = "nutrient")

    DT <- truncateDT(DT, fillLimits =  fillLimits)
    paletteType <- "Spectral"
    breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 1)
    # breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
    myPalette <- colorRampPalette(brewer.pal(11, paletteType))
    palette <- myPalette(4)
    graphsListHolder <- list()
    if (i %in% "keepListNuts.macroMinrls") filePart <- "macroMinrlsARDelta"
    if (i %in% "keepListNuts.vits") filePart <- "vitsARDelta"
    legendText <- "Change in adequacy ratio, 2030 (percent)"

    displayOrder <- capwords(cleanupNutrientNames(i))
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
    fileName <- paste("facetmap", "_crop ", j, "_nut_", i, "_", "2030", ".", "var", sep = "")
    facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
  }
}

