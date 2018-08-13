#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name dataPrep.IMPACT.R
#' @include nutrientModFunctions.R
source("R/nutrientModFunctions.R")

dt.FBS <- getNewestVersion("dt.FBS", fileloc("uData"))
dt.FBS <- dt.FBS[variable %in% "KcalPerCapPerDay",]
dt.FBS[, kcals.sum := sum(value), by = c("year", "ISO_code")]
dt.FBS[, c("IMPACT_code", "value") := NULL]
dt.FBS <- unique(dt.FBS)
keepYearList <- c("X2009", "X2010", "X2011")
dt.FBS <- dt.FBS[year %in% keepYearList,]
dt.FBS[, kcals.FBS.2010 := mean(kcals.sum), by = c( "ISO_code")]
dt.FBS[, c("year", "kcals.sum") := NULL]
dt.FBS <- unique(dt.FBS)
dt.FBS[, c("variable_code", "variable", "unit", "country_name") := NULL]

#from nut modeling process
dt.nutrients_sum_all.var <- getNewestVersion("dt.nutrients_sum_all.var", fileloc("resultsDir"))
dt.nutrients_sum_all.var <- dt.nutrients_sum_all.var[nutrient %in% "kcalsPerDay_tot" & year %in% "X2010" & scenario %in% "SSP2-NoCC-REF"
,]
dt.nutrients_sum_all.var[, c("year", "nutrient", "scenario") := NULL]
dt.nutrients_sum_all.var <- unique(dt.nutrients_sum_all.var)
setnames(dt.nutrients_sum_all.var, old = "value", new = "kcals.nutmod.2010")

kcals.compare <- merge(dt.nutrients_sum_all.var, dt.FBS, by.x = "region_code.IMPACT159", by.y = "ISO_code")

# get kcals from IMPACT gdx
dt.PerCapKCAL <- getNewestVersion("dt.PerCapKCAL", fileloc("iData"))
dt.PerCapKCAL <- dt.PerCapKCAL[year %in% "X2010" & scenario %in% "SSP2-NoCC-REF"]
dt.PerCapKCAL[, c("year", "scenario") := NULL]
dt.PerCapKCAL <- unique(dt.PerCapKCAL)
setnames(dt.PerCapKCAL, old = "PerCapKCAL", new = "kcals.IMPACT.2010")
kcals.compare <- merge(kcals.compare, dt.PerCapKCAL, by = "region_code.IMPACT159")
kcals.compare[, delta.IMP_FBS := kcals.FBS.2010 - kcals.IMPACT.2010]
kcals.compare[, delta.FBS_nutmod := kcals.FBS.2010 - kcals.nutmod.2010]
kcals.compare[, delta.IMP_nutmod := kcals.IMPACT.2010 - kcals.nutmod.2010]
inDT <- kcals.compare
outName <- "dt.KcalsComparison"
desc <- "Comparisons of daily kcal availability from FAO's FBS, from output from the IMPACE model, and from calculations in the nutrient modeling code."
cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
