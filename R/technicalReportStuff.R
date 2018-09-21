


library(RColorBrewer)
source("R/nutrientModFunctions.R")
sourceFile <- "technicalReportStuff.R"
graphsListHolder <- list()

keepListNuts.macroMinrls <- c(keyVariable("macronutrients"), keyVariable("minerals"))
keepListNuts.macroMinrls <- keepListNuts.macroMinrls[!keepListNuts.macroMinrls %in% "fat_g"]
keepListNuts.vits <- keyVariable("vitamins")
keepListNuts.vits <- keepListNuts.vits[!keepListNuts.vits %in% c("riboflavin_mg", "thiamin_mg" )]
keepListNuts <- c("keepListNuts.macroMinrls", "keepListNuts.vits")

worldMap <- getNewestVersion("worldMap", fileloc("uData")) # run storeWorldMapDF() if this is not available

# food group sources of nutrients
dt.nutrients_sum_FG.var <- getNewestVersion("dt.nutrients_sum_FG.var", fileloc("resultsDir"))
yearVal <- "X2010"
scenarioVal <- "SSP2-HGEM-REF"
dt.nutrients_sum_FG.var <- dt.nutrients_sum_FG.var[year %in% yearVal & scenario %in% scenarioVal]
dt.nutrients_sum_FG.var[, c("year", "scenario") := NULL]
dt.nutrients_sum_FG.var[, sumFG.var := sum(value), by = c("region_code.IMPACT159", "nutrient" )]
dt.nutrients_sum_FG.var[,FGratio := 100 * value/sumFG.var]
dt.nutrients_sum_FG.var[, value := NULL]
FGlist <- sort(unique(dt.nutrients_sum_FG.var$food_group_code))
for (j in keepListNuts) {
  for (i in FGlist) {
    DT <- copy(dt.nutrients_sum_FG.var)
    DT <- DT[food_group_code %in% i & nutrient %in% get(j)]
    DT[, value := FGratio]
    DT[, c("food_group_code", "sumFG.var", "FGratio") := NULL]
    DT <- unique(DT)
    DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
    facetColName <- "nutrient"
    legendText <- paste(i, "share of nutrients, 2050 (percent)")
    fillLimits <- c(0, 75)
    DT <- truncateDT(DT, fillLimits =  fillLimits)
    paletteType <- "Spectral"
    numLimits <- 4
    breakValues <- generateBreakValues(fillLimits = fillLimits, numLimits = numLimits, decimals = 0)
    myPalette <- colorRampPalette(brewer.pal(11, paletteType))
    palette <- myPalette(4)
    displayOrder <- capwords(cleanupNutrientNames(get(j)))

    dt.nuts <- data.table(nutListtemp = get(j), nutListtempclean = capwords(cleanupNutrientNames(get(j))))
    DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
    DT[, nutrient := NULL]
    setnames(DT, old = "nutListtempclean", new = "nutrient")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "id") # needs to be after cleanupNutrientNames

    fileName <- paste("facetmap", "_", "FGShareNuts", "_", i, "_", gsub("keepListNuts.", "", j), "_", gsub("X", "", yearVal), ".", "var", sep = "")
    facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
  }

  # calculate var specific effects on nutrients
  dt.nutrients_sum_FG.base <- getNewestVersion("dt.nutrients_sum_FG.base", fileloc("resultsDir"))
  yearVal <- "X2010"
  dt.nutrients_sum_FG.base <- dt.nutrients_sum_FG.base[year %in% yearVal & scenario %in% scenarioVal]
  dt.nutrients_sum_FG.base[, c("year", "scenario") := NULL]
  dt.nutrients_sum_FG.base[, sumFG.base := sum(value), by = c("region_code.IMPACT159", "nutrient")]
  dt.nutrients_sum_FG.base[, c("value") := NULL]
  #  dt.nutrients_sum_FG.var[, FGratio := NULL]
  dt.nutsCombined <- merge(dt.nutrients_sum_FG.base, dt.nutrients_sum_FG.var, by = c("region_code.IMPACT159", "nutrient", "food_group_code" ))
  dt.nutsCombined[, delta := 100 * (sumFG.var - sumFG.base)/sumFG.base]
  dt.nutsCombined[, food_group_code := NULL]
  dt.nutsCombined <- unique(dt.nutsCombined)
  DT <- copy(dt.nutsCombined)
  DT <- DT[nutrient %in% get(j)]
  DT[, value := delta]
  keepListCol <- c("region_code.IMPACT159", "value", "nutrient")
  DT[, setdiff(names(DT), keepListCol) := NULL]
  DT <- unique(DT)
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  facetColName <- "nutrient"
  legendText <- paste("Nutrient change from country-specific varieties, 2050 (percent)")
  fillLimits <- c(-50, 75)
  DT <- truncateDT(DT, fillLimits =  fillLimits)
  paletteType <- "Spectral"
  numLimits <- 4
  breakValues <- generateBreakValues(fillLimits = fillLimits, numLimits = numLimits, decimals = 0)
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  displayOrder <- capwords(cleanupNutrientNames(get(j)))

  dt.nuts <- data.table(nutListtemp = get(j), nutListtempclean = capwords(cleanupNutrientNames(get(j))))
  DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
  DT[, nutrient := NULL]
  setnames(DT, old = "nutListtempclean", new = "nutrient")
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id") # needs to be after cleanupNutrientNames

  fileName <- paste("facetmap", "_", "varDelta", "_", gsub("keepListNuts.", "", j),"_", gsub("X", "", yearVal), ".", "var", sep = "")
  facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
}

# do facet map of the effect of fortification on nutrient availability
allNuts <- c(keepListNuts.macroMinrls, keepListNuts.vits)
dt.nutrients_sum_all.var <- getNewestVersion("dt.nutrients_sum_all.var", fileloc("resultsDir"))
dt.nutrients_sum_all.var <- dt.nutrients_sum_all.var[nutrient %in% allNuts]
setnames(dt.nutrients_sum_all.var, old = "value", new = "value.var")
dt.nutrients_sum_all.varFort <- getNewestVersion("dt.nutrients_sum_all.varFort", fileloc("resultsDir"))
dt.nutrients_sum_all.varFort <- dt.nutrients_sum_all.varFort[nutrient %in% allNuts]
setnames(dt.nutrients_sum_all.varFort, old = "value", new = "value.varFort")
dt.nutrient.sum.all <- merge(dt.nutrients_sum_all.var, dt.nutrients_sum_all.varFort, by = c("scenario", "region_code.IMPACT159", "year", "nutrient"))
dt.nutrient.sum.all[, delta := 100 * (value.varFort - value.var)/value.var]
dt.nutrient.sum.all[, c("value.var", "value.varFort") := NULL]
dt.nutrient.sum.all <- dt.nutrient.sum.all[year %in% yearVal & scenario %in% scenarioVal]
dt.nutrient.sum.all[, c("year", "scenario") := NULL]
dt.nutrient.sum.all <- unique(dt.nutrient.sum.all)
setnames(dt.nutrient.sum.all, old = "delta", new = "value")
for (j in keepListNuts) {
  DT <- copy(dt.nutrient.sum.all)
  DT <- DT[nutrient %in% get(j)]
  # keepListCol <- c("region_code.IMPACT159", "value", "nutrient")
  # DT[, setdiff(names(DT), keepListCol) := NULL]
 # DT <- unique(DT)
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  facetColName <- "nutrient"
  legendText <- paste("Nutrient change from fortification, 2050 (percent)")
  fillLimits <- c(-0, 200)
  DT <- truncateDT(DT, fillLimits =  fillLimits)
  paletteType <- "Spectral"
  numLimits <- 4
  breakValues <- generateBreakValues(fillLimits = fillLimits, numLimits = numLimits, decimals = 0)
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  displayOrder <- capwords(cleanupNutrientNames(get(j)))

  dt.nuts <- data.table(nutListtemp = get(j), nutListtempclean = capwords(cleanupNutrientNames(get(j))))
  DT <- merge(DT, dt.nuts, by.x = "nutrient", by.y = "nutListtemp")
  DT[, nutrient := NULL]
  setnames(DT, old = "nutListtempclean", new = "nutrient")
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id") # needs to be after cleanupNutrientNames

  fileName <- paste("facetmap", "_", "fortDelta", "_", gsub("keepListNuts.", "", j),"_", gsub("X", "", yearVal), ".", "var", sep = "")
  facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

}

