source("R/nutrientModFunctions.R")
sourceFile <- "USAIDnutrientRanking.R"
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

# list of scenarios in the analysis
scenarioList <- c("SSP2-HGEM-cf", paste0("SSP2-HGEM-", cropList))
scenarioList.crops <- scenarioList[!scenarioList %in% "SSP2-HGEM-cf"]
cropRatios <- paste(scenarioList.crops, "ratio", sep = ".")
beginningCols <- c("region_code.IMPACT159", "year")

# options for regions to aggregate over
# dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
aggregationRegionChoices <- c("region_code.WB.income","region_name.WB.income",
                              "region_code.WB.spatial","region_name.WB.spatial",
                              "region_code.Africa","region_name.Africa", "region_code.EconGroup", "region_name.EconGroup",
                              "region_code.smallGroup", "region_name.smallGroup",
                              "region_code.MDIreg1","region_name.MDIreg1", "region_code.MDIreg2", "region_name.MDIreg2" )
keepListCol.regions <- c(
  # "region_code.IMPACT159","region_name.IMPACT159",
  # "region_code.WB.income","region_name.WB.income",
  # "region_code.WB.spatial","region_name.WB.spatial",
  # "region_code.Africa","region_name.Africa", "region_code.EconGroup", "region_name.EconGroup",
  "region_code.smallGroup", "region_name.smallGroup",
  "region_code.MDIreg1","region_name.MDIreg1", "region_code.MDIreg2", "region_name.MDIreg2")
# dt.regions.all[, setdiff(names(dt.regions.all), keepListCol) := NULL]
# dt.regions.all <- unique(dt.regions.all)

# load the data
# get adequacy data by nutrients
d.reqRatio.RDA.all.var <- getNewestVersion("increase.reqRatio.RDA.all.var", fileloc("resultsDir"))
d.reqRatio.RDA.all.var[, c(scenarioList.crops) := NULL]
# keepListCol.regions <- c("region_code.smallGroup", "region_name.smallGroup", "region_code.MDIreg1", "region_name.MDIreg1" ,
#                  "region_code.MDIreg2", "region_name.MDIreg2")
deleteListCol <- aggregationRegionChoices[!aggregationRegionChoices %in% keepListCol.regions]
d.reqRatio.RDA.all.var[, (deleteListCol) := NULL]
# keepListNuts <- c("protein_g", "carbohydrate_g", "calcium_mg", "iron_mg", "zinc_mg", "folate_µg", "vit_a_rae_µg",
#                  "vit_d_µg", "vit_e_mg", "vit_b12_µg")

keepListNuts.macroMinrls <- c(keyVariable("macronutrients"), keyVariable("minerals"))
keepListNuts.macroMinrls <- keepListNuts.macroMinrls[!keepListNuts.macroMinrls %in% "fat_g"]
keepListNuts.vits <- keyVariable("vitamins")
keepListNuts.vits <- keepListNuts.vits[!keepListNuts.vits %in% c("riboflavin_mg", "thiamin_mg" )]
keepListNuts <- c(keepListNuts.macroMinrls, keepListNuts.vits)

d.reqRatio.RDA.all.var <- d.reqRatio.RDA.all.var[nutrient %in% keepListNuts]
d.reqRatio.RDA.all.var <- melt(d.reqRatio.RDA.all.var,
                               id.vars = c("region_code.IMPACT159", "year", "nutrient", keepListCol.regions),
                               variable.name = "scenario",
                               measure.vars = c("SSP2-HGEM-cf", cropRatios),
                               value.name = "value",
                               variable.factor = FALSE)

#' population data set used for weighting by population -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
dt.pop <- dt.pop[year %in% keepListYear]
d.reqRatio.RDA.all.var[, scenario := gsub(".ratio", "", scenario)] # so scenario names are the same as in dt.pop

d.reqRatio.RDA.all.var <- merge(d.reqRatio.RDA.all.var, dt.pop, by = c("region_code.IMPACT159", "scenario", "year"))
d.reqRatio.RDA.all.var[, scenario := gsub("SSP2-HGEM-", "", scenario)]
d.reqRatio.RDA.all.var <- d.reqRatio.RDA.all.var[scenario %in% "cf", scenario := "NoInvestment"]

regionCodeToAgg <- "region_code.MDIreg2"
regionNameToAgg <- "region_name.MDIreg2"
d.reqRatio.RDA.all.var[, value.agg := weighted.mean(value, PopX0), by = c(regionCodeToAgg, "scenario", "nutrient")]

keepListCol <- c(regionCodeToAgg, regionNameToAgg, "scenario", "nutrient", "value.agg")
d.reqRatio.RDA.all.var[, setdiff(names(d.reqRatio.RDA.all.var), keepListCol) := NULL]
d.reqRatio.RDA.all.var <- unique(d.reqRatio.RDA.all.var)

# remove noInvestment from the rank ordering
d.reqRatio.RDA.all.var <- d.reqRatio.RDA.all.var[!scenario %in% "NoInvestment"]
temp <- length(unique(d.reqRatio.RDA.all.var$scenario))+ 1
d.reqRatio.RDA.all.var[, value.rnk := temp - rank(value.agg), by = c(regionCodeToAgg, "nutrient")]

ARcutoff <- 2

d.reqRatio.RDA.all.var <- d.reqRatio.RDA.all.var[!(scenario %in% "NoInvestment" & value.agg > ARcutoff)]
# d.reqRatio.RDA.all.var <- d.reqRatio.RDA.all.var[!scenario %in% "NoInvestment"]

formula.wide <- paste0("scenario + ", regionCodeToAgg,  "+",  regionNameToAgg,  "~ nutrient")
d.reqRatio.RDA.all.var.wide <- dcast(
  data = d.reqRatio.RDA.all.var,
  formula = formula.wide,
  value.var = "value.rnk")

d.reqRatio.RDA.all.var.wide[, NIPI := rowSums(.SD, na.rm=T), .SDcols=keepListNuts]
d.reqRatio.RDA.all.var.wide[, (keepListNuts) := NULL]

formula.wide <- paste0("scenario ~ ", regionNameToAgg)
DT.wide <- data.table::dcast(
  data = d.reqRatio.RDA.all.var.wide,
  formula = formula.wide,
  value.var = "NIPI")

# one way to check is that the columns should all sum to the same number.
colNamesToSum <- names(DT.wide)[!names(DT.wide) %in% "scenario"] #
DT.wide[, lapply(.SD, sum, na.rm=TRUE), .SDcols = (colNamesToSum) ]

# add crop names and categories from cropInfo
DT.wide <- merge(cropInfo, DT.wide,  by.y = "scenario", by.x = "cropList")
DT.wide[, cropList := NULL]
sortOrder <- c("Cereal Grains", "Roots, Tubers & Bananas", "Oilseeds & Pulses")
DT.wide[, sortOrder := match(cropCategory, sortOrder)]
setorder(DT.wide, sortOrder)
DT.wide[, sortOrder := NULL]

temp <- nrow(DT.wide)+ 1

colNamesToRank <- names(DT.wide)[!names(DT.wide) %in% c("cropNames", "cropCategory")]
colNamesToRank.rank <- paste(colNamesToRank, "rank", sep =".")
DT.wide[, (colNamesToRank.rank) := lapply(.SD, rank), .SDcols = (colNamesToRank)]

DT.wide[, (colNamesToRank) := NULL]
setnames(DT.wide, old = names(DT.wide), new = gsub(".rank", "", names(DT.wide)))
inDT <- DT.wide
outName <- paste("nutrientInvest", gsub("region_code","", regionCodeToAgg), sep = ".")
desc = "Ranking of crop-specific investments for contribution to adequacy of multiple nutrients"
cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

# do calculations for kcal share from nonstaples

d.KcalShare_nonstaple.share.var <- getNewestVersion("increase.dt.KcalShare_nonstaple.share.var", fileloc("resultsDir"))
d.KcalShare_nonstaple.share.var[, c(scenarioList.crops) := NULL]
# do facetmap
DT <- copy(d.KcalShare_nonstaple.share.var)
DT <- DT[staple_code %in% "nonstaple"]
DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
facetColName <- "staple_code"
legendText <- "nonstaple share of Kcals, 2030"
fillLimits <- c(0, 75)
setnames(DT, old ="SSP2-HGEM-cf", new = "value")

# DT[, scenario := gsub("-", "_", scenario)] # needed to have valid column names
# DT <- DT[, scenario :=  gsub("-", "_", scenario)]
DT <- truncateDT(DT, fillLimits =  fillLimits)
paletteType <- "Spectral"
breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 0)
# breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
myPalette <- colorRampPalette(rev(brewer.pal(11, paletteType)))
palette <- myPalette(4)
graphsListHolder <- list()

displayOrder <- "nonstaple"
data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
fileName <- paste("facetmap", "_", "nonstapleShare", "_", "2030", ".", "var", sep = "")
facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)

# do rank order or something like that for nonstaple share
dt.rank <- copy(d.KcalShare_nonstaple.share.var)
dt.rank[, c("year", "staple_code", "SSP2-HGEM-cf") := NULL]

