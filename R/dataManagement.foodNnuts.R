#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2016 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

#' @description To be added

#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}

# Read in all data first and standardize variable names -----
# Read in IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")

# get the list of scenarios in the IMPACT data for use below
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)

# read in nutrients data and optionally apply cooking retention values -----
switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#dt.nutrients is per 100 gm of the raw product (ie before edible portion is applied)
#dt.nutrients.adj is per kg of food
dt.nutrients.adj <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

# convert food availability from per year to per day
# dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")]
dt.IMPACTfood[, FoodAvailability := NULL]

dt.foodNnuts <- merge(dt.IMPACTfood, dt.nutrients.adj, by = "IMPACT_code", all = TRUE)

list.minrls <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
list.vits <- c("folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
              "vit_b12_µg", "vit_c_mg", "vit_d_µg",  "vit_e_mg", "vit_k_µg")
list.macro <- c("carbohydrate_g", "protein_g",  "totalfiber_g", "fat_g")
list.other <- c("caffeine_mg", "cholesterol_mg", "phytate_mg", "sugar_g", "ethanol_g")
list.energy <- c("kcals.fat_g", "kcals.carbohydrate_g", "kcals.protein_g",
                 "kcals.ethanol_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g")  # note that "energy_kcal" is removed from this list
list.fattyAcids <- c("ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g",  "ft_acds_tot_trans_g")

list.tot <- c(list.minrls, list.vits, list.macro, list.other, list.energy, list.fattyAcids)
names.tot <- paste(list.tot, "Q", sep = ".")
dt.foodNnuts[, kcalsPerCommod := foodAvailpDay * energy_kcal]

for (j in 1:length(list.tot)) {
  data.table::set(dt.foodNnuts, i = NULL, j = names.tot[j], value = dt.foodNnuts[[list.tot[j]]] * dt.foodNnuts[['foodAvailpDay']])
}
data.table::setorder(dt.foodNnuts, NULL)
dt.foodNnuts[, `:=`(
  foodQ.sum = sum(foodAvailpDay),
  kcalsPerDay.tot = sum(kcalsPerCommod),
  kcalsPerDay.carbohydrate = sum(kcals.carbohydrate_g.Q),
  kcalsPerDay.fat = sum(kcals.fat_g.Q),
  kcalsPerDay.protein = sum(kcals.protein_g.Q),
  kcalsPerDay.ethanol = sum(kcals.ethanol_g.Q),
  kcalsPerDay.sugar = sum(kcals.sugar_g.Q),
  kcalsPerDay.ft_acds_tot_sat = sum(kcals.ft_acds_tot_sat_g.Q)),
  by = c("scenario", "year", "region_code.IMPACT159")
  ]

#cap the amount of ethanol to 100 gm per day. Converted to kcals.
ethanolKcals <- 6.9 # needed for ethanol kcals cap
kcals.ethanol.cap <- 100 * ethanolKcals
dt.foodNnuts[kcalsPerDay.ethanol > kcals.ethanol.cap, kcalsPerDay.ethanol := kcals.ethanol.cap]

dt.foodNnuts[, kcalsPerDay.other := kcalsPerDay.tot - (kcalsPerDay.carbohydrate + kcalsPerDay.fat + kcalsPerDay.protein) ]
deleteListCol <- c(list.tot, "usda_code", "Ref_Desc", "phytate_source")
dt.foodNnuts[, (deleteListCol) := NULL]
data.table::setnames(dt.foodNnuts, old = c(names.tot), new = c(list.tot))
outName <- "dt.foodNnuts"
cleanup(dt.foodNnuts, outName, fileloc("resultsDir"))

# produce subsets that are more manageable in size -----
# dt.nutrients.kcals -----
dt.nutrients.kcals <- data.table::copy(dt.foodNnuts)
deleteListCol <- c("pcGDPX0", "PCX0", "PWX0",  "Long_Desc", "retentioncode_aus", "RetnDesc",
                  "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g",
                   "calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg",
                  "potassium_g", "zinc_mg", "folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg", "vit_a_rae_µg",
                  "vit_b6_mg", "vit_b12_µg", "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg", "carbohydrate_g", "protein_g",
                  "totalfiber_g", "fat_g", "caffeine_mg", "cholesterol_mg", "phytate_mg", "sugar_g", "ethanol_g")
dt.nutrients.kcals[, (deleteListCol) := NULL]

kcalsSources <- c("kcalsPerDay.other", "kcalsPerDay.carbohydrate", "kcalsPerDay.fat",  "kcalsPerDay.protein")
kcalsShare <- paste0(kcalsSources, "_share")
# calculate ratio of energy to kcals per day for all items in sumlist
dt.nutrients.kcals[, (kcalsShare) := lapply(.SD, "/", kcalsPerDay.tot/100), .SDcols =  (kcalsSources)] # divide by 100 to get to percent
dt.nutrients.kcals <- unique(dt.nutrients.kcals)

outName <- "dt.nutrients.kcals"
cleanup(dt.nutrients.kcals, outName, fileloc("resultsDir"))

# dt.nutrients.sum.all ------
dt.nutrients.sum.all <- data.table::copy(dt.foodNnuts)
dt.bioavail_zinc <- getNewestVersion("dt.bioavail_zinc", fileloc("resultsDir"))
dt.bioavail_iron <- getNewestVersion("dt.bioavail_iron", fileloc("resultsDir"))
keepListCol.zinc <- c("scenario", "region_code.IMPACT159", "year", "bioavailability.zinc")
keepListCol.iron <- c("scenario", "region_code.IMPACT159", "year", "bioavailability.iron")
dt.bioavail_zinc <- dt.bioavail_zinc[,(keepListCol.zinc), with = FALSE]
dt.bioavail_zinc <- unique(dt.bioavail_zinc)
dt.bioavail_iron <- dt.bioavail_iron[,(keepListCol.iron), with = FALSE]

list.tot <- c(list.tot, "energy_kcal")
dt.nutrients.sum.all <- merge(dt.nutrients.sum.all, dt.bioavail_zinc, by = c("scenario","region_code.IMPACT159", "year" ))
dt.nutrients.sum.all[, zinc_mg := zinc_mg * bioavailability.zinc / 100]
dt.nutrients.sum.all <- merge(dt.nutrients.sum.all, dt.bioavail_iron, by = c("scenario","region_code.IMPACT159", "year" ))
dt.nutrients.sum.all[, iron_mg := iron_mg * bioavailability.iron / 100]
dt.nutrients.sum.all[, c("bioavailability.zinc", "bioavailability.iron") := NULL]
dt.nut.wide <- data.table::copy(dt.nutrients.sum.all)
dt.nut.wide[, (list.tot) := lapply(.SD, sum), .SDcols = (list.tot),
                by =  c("scenario", "region_code.IMPACT159", "year")]
dt.nut.wide <- unique(dt.nut.wide)
dt.nut.long <- data.table::melt(dt.nut.wide,
                                id.vars = c("scenario","region_code.IMPACT159", "year"),
                                variable.name = "nutrient",
                                measure.vars = list.tot,
                                value.name = "value",
                                variable.factor = FALSE)

inDT <- unique(dt.nut.long)
outName <- "dt.nutrients.sum.all" #this includes phytate. Might want to remove later.
cleanup(inDT,outName, fileloc("resultsDir"))

# dt.nutrients.sum.staples ------

dt.nutrients.sum.staples <- dt.nutrients.sum.all[, (list.tot) := lapply(.SD, sum), .SDcols = (list.tot),
                                                 by = c("scenario", "region_code.IMPACT159", "year", "staple_code")]
dt.nutrients.sum.staples <- unique(dt.nutrients.sum.staples)

dt.nutrients.sum.staples.long <- data.table::melt(dt.nutrients.sum.staples,
                                id.vars = c("scenario","region_code.IMPACT159", "staple_code", "year"),
                                variable.name = "nutrient",
                                measure.vars = list.tot,
                                value.name = "value",
                                variable.factor = FALSE)
dt.nutrients.sum.staples.long <- unique(dt.nutrients.sum.staples.long) #note that this contains phytate
inDT <- dt.nutrients.sum.staples.long
outName <- "dt.nutrients.sum.staples"
cleanup(inDT,outName, fileloc("resultsDir"))

# dt.nutrients.nonstapleShare ------
shareFormula <- "scenario + region_code.IMPACT159 + nutrient + year ~ staple_code"
dt.nut.nonstaple.share.wide <- data.table::dcast(data = dt.nutrients.sum.staples.long,
                                                 formula = shareFormula,
                                                 value.var = "value")
dt.nut.nonstaple.share.wide[, value := 100 * nonstaple/(nonstaple + staple) ][is.na(value), value := 0]

dt.nut.nonstaple.share.long <- dt.nut.nonstaple.share.wide[,c("nonstaple", "staple") := NULL]
inDT <- dt.nut.nonstaple.share.long
outName <- "dt.nutrients.nonstapleShare"
cleanup(inDT, outName, fileloc("resultsDir"))

# dt.foodAvail.foodGroup -----
#sum and convert to grams
dt.foodAvail.foodGroup <- dt.nutrients.kcals[, foodavail.foodgroup.sum := sum(foodAvailpDay) * 1000,
                           by = c("scenario", "region_code.IMPACT159", "year", "food_group_code")]
dt.foodAvail.foodGroup[, c("IMPACT_code", "foodAvailpDay") := NULL]
dt.foodAvail.foodGroup <- unique(dt.foodAvail.foodGroup)
data.table::setnames(dt.foodAvail.foodGroup, old = "foodavail.foodgroup.sum", new = "value")
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "food_group_code", "value")
deleteListCol <- names(dt.foodAvail.foodGroup)[!names(dt.foodAvail.foodGroup) %in% (keepListCol)]
dt.foodAvail.foodGroup[,(deleteListCol) := NULL]
dt.foodAvail.foodGroup <- unique(dt.foodAvail.foodGroup)
inDT <- dt.foodAvail.foodGroup
outName <- "dt.foodAvail.foodGroup"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")
