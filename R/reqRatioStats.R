DT.macro <- getNewestVersion("RDA_reqRatio_macro_sum", fileloc("resultsDir"))
DT.vits <- getNewestVersion("RDA_reqRatio_vits_sum", fileloc("resultsDir"))
DT.minrls <- getNewestVersion("RDA_reqRatio_minrls_sum", fileloc("resultsDir"))
DT <- do.call("rbind", list(DT.macro, DT.vits, DT.minrls))
DT <- DT[year %in% c("X2010", "X2050"),]

aggChoice <- "WB"
dt.regions <- regionAgg(aggChoice)
# aggregate to and retain only the relevant regions
temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
merged <- merge(temp, dt.pop, by = c("scenario","region_code.IMPACT159","year"))
merged <- merged[, value.region := weighted.mean(value, PopX0), by = c("scenario","year", "region_code", "nutrient" )]
merged <- merged[, min.region := min(value), by = c("scenario","region_code","year","region_code", "nutrient")]
merged <- merged[, max.region := max(value), by = c("scenario", "year","region_code", "nutrient")]

# make it wide with nutrients in columns
formula.wide <- "scenario + region_code.IMPACT159 + year + value + region_code + region_name + PopX0 + value.region + min.region + max.region ~ nutrient"
merged.wide <- data.table::dcast(
  data = merged,
  formula = formula.wide,
  value.var = "value")
