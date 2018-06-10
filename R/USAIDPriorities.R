source("R/nutrientModFunctions.R")

dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
keepListCol <- c("region_code.IMPACT159","region_name.IMPACT159",
                "region_code.WB.income","region_name.WB.income",
                "region_code.WB.spatial","region_name.WB.spatial",
                "region_code.Africa","region_name.Africa", "region_code.EconGroup", "region_name.EconGroup")
dt.regions.all[, setdiff(names(dt.regions.all), keepListCol) := NULL]
dt.regions.all <- unique(dt.regions.all)

# adequacy ratio calculations
reqRatio_sum_RDA_macro.base <- getNewestVersion("reqRatio_sum_RDA_macro.base", fileloc("resultsDir"))
reqRatio_sum_RDA_macro.base[, nutrientType := "macro"]
reqRatio_sum_RDA_minrls.base <- getNewestVersion("reqRatio_sum_RDA_minrls.base", fileloc("resultsDir"))
reqRatio_sum_RDA_minrls.base[, nutrientType := "minrls"]
reqRatio_sum_RDA_vits.base <- getNewestVersion("reqRatio_sum_RDA_vits.base", fileloc("resultsDir"))
reqRatio_sum_RDA_vits.base[, nutrientType := "vits"]

reqRatio.all.base <- rbind(reqRatio_sum_RDA_macro.base, reqRatio_sum_RDA_minrls.base, reqRatio_sum_RDA_vits.base)

keepYearList <- c("X2030")
reqRatio.all.base <- reqRatio.all.base[year %in% keepYearList]
reqRatio.all.base <- merge(reqRatio.all.base, dt.regions.all, by = "region_code.IMPACT159", allow.cartesian=TRUE)


shareFormula <- "nutrient + region_code.IMPACT159 + region_name.IMPACT159 +
region_code.WB.income + region_name.WB.income + region_code.WB.spatial + region_name.WB.spatial +
region_code.Africa + region_name.Africa + region_code.EconGroup + region_name.EconGroup +
nutrientType + year ~ scenario "
reqRatio.all.base.wide <- data.table::dcast(data = reqRatio.all.base,
                                                      formula = shareFormula,
                                                      value.var = "value")
beginningCols <- c("region_code.IMPACT159", "nutrientType", "year", "nutrient", "SSP2-HGEM-cf")
newColOrder <- c(beginningCols, names(reqRatio.all.base.wide)[!names(reqRatio.all.base.wide) %in% beginningCols])
setcolorder(reqRatio.all.base.wide, new = newColOrder)

cropScenarios <- names(reqRatio.all.base.wide)[!names(reqRatio.all.base.wide) %in% beginningCols]
cropRatios <- paste(cropScenarios, "ratio", sep = ".")

reqRatio.all.base.wide[, (cropRatios) := lapply(.SD, "/", `SSP2-HGEM-cf`), .SDcols =  (cropScenarios)]

reqRatio.all.base.wide[, (cropRatios) := lapply(.SD, function(x) (-1 + x / `SSP2-HGEM-cf`) * 100), .SDcols = (cropScenarios)]
write.csv(reqRatio.all.base.wide, file = "adequacyRatioImprovs_all.csv")


#food availability by food group
dt.nutrients_sum_all.base_2018-06-05

# adequacy ratio calculations
dt.nutrients_sum_all.base <- getNewestVersion("dt.nutrients_sum_all.base", fileloc("resultsDir"))
keepYearList <- c("X2030")
dt.nutrients_sum_all.base <- dt.nutrients_sum_all.base[year %in% keepYearList]

shareFormula <- "nutrient + region_code.IMPACT159 + nutrientType + year ~ scenario "
dt.nutrients_sum_all.base.wide <- data.table::dcast(data = dt.nutrients_sum_all.base,
                                            formula = shareFormula,
                                            value.var = "value")
beginningCols <- c("region_code.IMPACT159", "nutrientType", "year", "nutrient", "SSP2-HGEM-cf")
newColOrder <- c(beginningCols, names(dt.nutrients_sum_all.base.wide)[!names(dt.nutrients_sum_all.base.wide) %in% beginningCols])
setcolorder(dt.nutrients_sum_all.base.wide, new = newColOrder)

cropScenarios <- names(dt.nutrients_sum_all.base.wide)[!names(reqRatio.all.base.wide) %in% beginningCols]
cropRatios <- paste(cropScenarios, "ratio", sep = ".")

dt.nutrients_sum_all.base.wide[, (cropRatios) := lapply(.SD, "/", `SSP2-HGEM-cf`), .SDcols =  (cropScenarios)]

dt.nutrients_sum_all.base.wide[, (cropRatios) := lapply(.SD, function(x) (-1 + x / `SSP2-HGEM-cf`) * 100), .SDcols = (cropScenarios)]
write.csv(dt.nutrients_sum_all.base.wide, file = "results/availRatioImprovs_all.csv")

# nutrient availability

dt.nutrients_sum_all.base <- getNewestVersion("dt.nutrients_sum_all.base", fileloc("resultsDir"))
keepYearList <- c("X2030")
dt.nutrients_sum_all.base <- dt.nutrients_sum_all.base[year %in% keepYearList]

shareFormula <- "nutrient + region_code.IMPACT159 + nutrientType + year ~ scenario "
dt.nutrients_sum_all.base.wide <- data.table::dcast(data = dt.nutrients_sum_all.base,
                                                      formula = shareFormula,
                                                      value.var = "value")
beginningCols <- c("region_code.IMPACT159", "nutrientType", "year", "nutrient", "SSP2-HGEM-cf")
newColOrder <- c(beginningCols, names(dt.nutrients_sum_all.base.wide)[!names(dt.nutrients_sum_all.base.wide) %in% beginningCols])
setcolorder(dt.nutrients_sum_all.base.wide, new = newColOrder)

cropScenarios <- names(dt.nutrients_sum_all.base.wide)[!names(reqRatio.all.base.wide) %in% beginningCols]
cropRatios <- paste(cropScenarios, "ratio", sep = ".")

dt.nutrients_sum_all.base.wide[, (cropRatios) := lapply(.SD, "/", `SSP2-HGEM-cf`), .SDcols =  (cropScenarios)]

dt.nutrients_sum_all.base.wide[, (cropRatios) := lapply(.SD, function(x) (-1 + x / `SSP2-HGEM-cf`) * 100), .SDcols = (cropScenarios)]
write.csv(dt.nutrients_sum_all.base.wide, file = "results/availRatioImprovs_all.csv")
