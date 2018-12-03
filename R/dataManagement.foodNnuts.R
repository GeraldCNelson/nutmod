#' @title Calculate nutrient content from food availability
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
#' @name dataManagement.foodNnuts.R
#' @description Does all the calculations combining food availability with nutrient content.

#' @include nutrientModFunctions.R

source("R/nutrientModFunctions.R")

sourceFile <- "dataManagement.foodNnuts.R"
createScriptMetaData()

# Read in all data first and standardize variable names -----
# Read in IMPACT food data ----------
dt.IMPACTfood <- getNewestVersion("dt.IMPACTfood", fileloc("iData")) # has food availability quantity information, pc income and prices. These don't change with vars and varFort

# budget share and price growth don't depend on suffix
budgetShare(dt.IMPACTfood)
priceGrowth(dt.IMPACTfood)

# get the list of scenarios in the IMPACT data for use below
scenarioListIMPACT <- keyVariable("scenarioListIMPACT")

# read in nutrients data. switch variable determines which nutrient info to include -----

for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  
  #dt.nutrients is per 100 gm
  #dt is per kg of food
  dt <- switches() # is specific to countries if switchloop = 2 or 3. nutrient composition of composites vary in .var because the composition of composites changes by country
  
  dt.foodNnuts <- merge(dt.IMPACTfood, dt, by = c("region_code.IMPACT159", "IMPACT_code"), allow.cartesian = TRUE)
  
  #make order the same for all versions of dt.foodNnuts
  setcolorder((dt.foodNnuts), neworder = c("scenario", "year", "region_code.IMPACT159", "IMPACT_code", "FoodAvailability", "pcGDPX0", "PCX0", "PWX0",
                                           "foodAvailpDay", "phytate_mg", "calcium_mg", "carbohydrate_g", "energy_kcal", "ethanol_g", "fat_g", "folate_µg",
                                           "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g", "iron_mg", "magnesium_mg", "niacin_mg", "phosphorus_mg",
                                           "potassium_g", "protein_g", "riboflavin_mg", "sugar_g", "thiamin_mg", "totalfiber_g", "vit_a_rae_µg", "vit_b12_µg", "vit_b6_mg",
                                           "vit_c_mg", "vit_d_µg", "vit_e_mg", "vit_k_µg", "zinc_mg", "kcals_fat_g", "kcals_protein_g", "kcals_carbohydrate_g",
                                           "kcals_sugar_g", "kcals_ft_acds_tot_sat_g", "kcals_ethanol_g", "food_group_code", "staple_code"))
  #gdp per cap and prices removed here.
  deleteListCol <- c("pcGDPX0", "FoodAvailability", "PCX0", "PWX0")
  dt.foodNnuts[, (deleteListCol) := NULL]
  
  list.minrls <- keyVariable("minerals")
  list.vits <- keyVariable("vitamins")
  list.macro <- keyVariable("macronutrients")
  list.energy <- keyVariable("energy")
  #list.other <- c("caffeine_mg", "cholesterol_mg", "phytate_mg", "sugar_g", "ethanol_g")
  
  #' the lists below differ slighly from the keyVariables. Need to fix this up some time.
  list.other <- c( "phytate_mg", "sugar_g", "ethanol_g")
  #list.fattyAcids <- c("ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g",  "ft_acds_tot_trans_g")
  list.fattyAcids <- c("ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g")
  
  list.tot <- c(list.minrls, list.vits, list.macro, list.other, list.energy, list.fattyAcids)
  names.tot <- paste(list.tot, "Q", sep = "_")
  dt.foodNnuts[, kcalsPerCommod := foodAvailpDay * energy_kcal]
  
  # set NAs to zero
  dt.foodNnuts[, (list.tot) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = list.tot]
  
  
  # calculate the total quantity of a nutrient available for each food item as the amount per 100 g * quantity of food available per day
  # for Norway in 2010 average daily egg availability was 2.739431e-02, egg content of vitamin d is 17.6 micro grams per kg
  # (converted from 100 grams by switches) so vit d availability (vit_d_µg.Q)
  # is 2.739431e-02 * 17.6 = 0.482139798
  for (j in 1:length(list.tot)) {
    data.table::set(dt.foodNnuts, i = NULL, j = names.tot[j], value = dt.foodNnuts[[list.tot[j]]] * dt.foodNnuts[['foodAvailpDay']])
  }
  # deleteListCol <- c("kcals_fat_g", "kcals_protein_g", "kcals_carbohydrate_g", "kcals_sugar_g", "kcals_ft_acds_tot_sat_g", "kcals_ethanol_g")
  # dt.foodNnuts[, (deleteListCol) := NULL]
  
  dt.foodNnuts[, `:=`(
    foodQ.sum = sum(foodAvailpDay),
    kcalsPerDay_tot = sum(kcalsPerCommod),
    kcalsPerDay_carbohydrate = sum(kcals_carbohydrate_g_Q),
    kcalsPerDay_fat = sum(kcals_fat_g_Q),
    kcalsPerDay_protein = sum(kcals_protein_g_Q),
    kcalsPerDay_ethanol = sum(kcals_ethanol_g_Q),
    kcalsPerDay_sugar = sum(kcals_sugar_g_Q),
    kcalsPerDay_ft_acds_tot_sat = sum(kcals_ft_acds_tot_sat_g_Q)),
    by = c("scenario", "year", "region_code.IMPACT159")
    ]
  
  #cap the amount of ethanol to 100 gm per day. Converted to kcals_
  ethanolKcals <- 6.9 # needed for ethanol kcals cap
  kcals_ethanol.cap <- 100 * ethanolKcals
  dt.foodNnuts[kcalsPerDay_ethanol > kcals_ethanol.cap, kcalsPerDay_ethanol := kcals_ethanol.cap]
  
  dt.foodNnuts[, kcalsPerDay_other := kcalsPerDay_tot - (kcalsPerDay_carbohydrate + kcalsPerDay_fat + kcalsPerDay_protein) ]
  #deleteListCol <- c(list.tot, "usda_code", "Ref_Desc", "phytate_source")
  # deleteListCol <- list.tot[!list.tot %in% c("kcals_fat_g", "kcals_carbohydrate_g", "kcals_protein_g", "kcals_ethanol_g",
  #                                            "kcals_sugar_g", "kcals_ft_acds_tot_sat_g")]
  dt.foodNnuts[, (list.tot) := NULL]
  
  # rename [nutrient].Q to just [nutrient]
  data.table::setnames(dt.foodNnuts, old = c(names.tot), new = c(list.tot))
  dt.foodNnuts <- adjustBioavailability(dt.foodNnuts)
  dt.foodNnuts[, scenario := gsub("-", "_", scenario)]
  dt.foodNnuts[, scenario := gsub("_REF", "", scenario)]
  outName <- paste("dt.foodNnuts", suffix, sep = ".")
  inDT <- dt.foodNnuts
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  desc <- "Combines dt.IMPACTfood with nutrients and kcalsPerDay"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
  
  #' produce subsets that are more manageable in size -----
  
  #' dt.nutrients.kcals -----
  dt.nutrients.kcals <- data.table::copy(dt.foodNnuts)
  deleteListCol <- c( "foodAvailpDay", "foodQ.sum", "ft_acds_tot_sat_g",  "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                      keyVariable("minerals"), keyVariable("vitamins"), keyVariable("macronutrients"),
                      "phytate_mg", "sugar_g", "ethanol_g", "energy_kcal")
  dt.nutrients.kcals[, (deleteListCol) := NULL]
  
  kcalsSources <- c("kcalsPerDay_other", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat",  "kcalsPerDay_protein",
                    "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat")
  kcalsShare <- paste0(kcalsSources, "_share")
  #' calculate ratio of energy to kcals per day for all items in sumlist
  dt.nutrients.kcals[, (kcalsShare) := lapply(.SD, "/", kcalsPerDay_tot/100), .SDcols =  (kcalsSources)] # divide by 100 to get to percent
  
  #' keep only results that are for the country, ie, not for individual commodities
  deleteListCol <- c( "IMPACT_code", "kcalsPerCommod",
                      "kcals_fat_g", "kcals_carbohydrate_g", "kcals_protein_g", "kcals_ethanol_g", "kcals_sugar_g", "kcals_ft_acds_tot_sat_g")
  dt.nutrients.kcals[, (deleteListCol) := NULL]
  dt.nutrients.kcals <- unique(dt.nutrients.kcals)
  
  idVars <- c("scenario", "region_code.IMPACT159", "year")
  measureVars <- names(dt.nutrients.kcals)[!names(dt.nutrients.kcals) %in% idVars &
                                             !names(dt.nutrients.kcals) %in% c("food_group_code", "staple_code")]
  dt.nutrients.kcals <- data.table::melt(dt.nutrients.kcals,
                                         id.vars = idVars,
                                         measure.vars = measureVars,
                                         variable.name = "nutrient",
                                         value.name = "value",
                                         variable.factor = FALSE)
  inDT <- unique(dt.nutrients.kcals)
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  outName <- paste("dt.nutrients_kcals", suffix, sep = ".")
  desc <- "Kcals and kcals shares from carbohydrates, fat, etc. by country"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
  
  #' dt.nutrients.sum.all ------
  dt.nutrients.sum.all <- data.table::copy(dt.foodNnuts)
  deleteListCol <- c("energy_kcal")
  dt.nutrients.sum.all[, (deleteListCol) := NULL]
  
  # the list.tot values are supposed to be average daily availability
  dt.nutrients.sum.all[, (list.tot) := lapply(.SD, sum), .SDcols = (list.tot),
                       by =  c("scenario", "region_code.IMPACT159", "year")]
  dt.nutrients.sum.all <- unique(dt.nutrients.sum.all)
  measureVars <- c(list.tot, "kcalsPerDay_tot", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein",
                   "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat", "kcalsPerDay_other")
  dt.nutrients.sum.all <- data.table::melt(dt.nutrients.sum.all,
                                           id.vars = c("scenario","region_code.IMPACT159", "year"),
                                           variable.name = "nutrient",
                                           measure.vars = measureVars,
                                           value.name = "value",
                                           variable.factor = FALSE)
  dt.nutrients.sum.all <- unique(dt.nutrients.sum.all)
  inDT <- dt.nutrients.sum.all
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  outName <- paste("dt.nutrients_sum_all", suffix, sep = ".") #this includes phytate. Might want to remove later.
  desc <- "Sum of each nutrient from each of the food items"
  cleanup(inDT,outName, fileloc("resultsDir"), desc = desc)
  
  #' dt.nutrients.sum.staples
  dt.nutrients.sum.staples <- data.table::copy(dt.foodNnuts)
  deleteListCol <- c("energy_kcal")
  dt.nutrients.sum.staples[, (deleteListCol) := NULL]
  
  deleteListCol <- c("kcalsPerDay_tot", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein",
                     "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat", "kcalsPerDay_other",
                     "foodQ.sum")
  dt.nutrients.sum.staples[, (deleteListCol) := NULL]
  dt.nutrients.sum.staples[, (list.tot) := lapply(.SD, sum), .SDcols = (list.tot),
                           by = c("scenario", "region_code.IMPACT159", "year", "staple_code")]
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "kcalsPerCommod", "food_group_code")
  dt.nutrients.sum.staples[, (deleteListCol) := NULL]
  dt.nutrients.sum.staples <- unique(dt.nutrients.sum.staples)
  # measureVars <- c(list.tot, "kcalsPerDay_tot", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein",
  #                  "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat", "kcalsPerDay_other")
  measureVars <- c(list.tot)
  dt.nutrients.sum.staples.long <- data.table::melt(dt.nutrients.sum.staples,
                                                    id.vars = c("scenario","region_code.IMPACT159", "year", "staple_code"),
                                                    measure.vars = measureVars,
                                                    variable.name = "nutrient",
                                                    value.name = "value",
                                                    variable.factor = FALSE)
  inDT <- unique(dt.nutrients.sum.staples.long)
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  outName <- paste("dt.nutrients_sum_staples", suffix, sep = ".")
  desc <- "Sum of each nutrient from staples and nonstaples"
  cleanup(inDT,outName, fileloc("resultsDir"), desc = desc)
  
  #' dt.nutrients_sum_FG
  dt.nutrients_sum_FG <- data.table::copy(dt.foodNnuts)
  deleteListCol <- c("energy_kcal")
  dt.nutrients_sum_FG[, (deleteListCol) := NULL]
  
  dt.nutrients_sum_FG[, (list.tot) := lapply(.SD, sum), .SDcols = (list.tot),
                      by = c("scenario", "region_code.IMPACT159", "year", "food_group_code")]
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "kcalsPerCommod", "staple_code")
  dt.nutrients_sum_FG[, (deleteListCol) := NULL]
  dt.nutrients_sum_FG <- unique(dt.nutrients_sum_FG)
  
  dt.nutrients_sum_FG <- data.table::melt(dt.nutrients_sum_FG,
                                          id.vars = c("scenario","region_code.IMPACT159", "year", "food_group_code"),
                                          variable.name = "nutrient",
                                          measure.vars = list.tot,
                                          value.name = "value",
                                          variable.factor = FALSE)
  dt.nutrients_sum_FG <- unique(dt.nutrients_sum_FG) #note that this contains phytate
  inDT <- dt.nutrients_sum_FG
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  outName <- paste("dt.nutrients_sum_FG", suffix, sep = ".")
  desc <- "Sum of each nutrient from each food group"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
  
  #' dt.KcalShare.nonstaple
  dt.KcalShare.nonstaple <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "kcalsPerCommod", "kcalsPerDay_tot", "staple_code",
                   "kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein", "kcalsPerDay_other", "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat")
  dt.KcalShare.nonstaple[, setdiff(names(dt.KcalShare.nonstaple), keepListCol) := NULL]
  dt.KcalShare.nonstaple <- unique(dt.KcalShare.nonstaple)
  dt.KcalShare.nonstaple[,value := sum(kcalsPerCommod) / kcalsPerDay_tot, by = c("scenario", "region_code.IMPACT159", "year", "staple_code")]
  deleteListCol <- c("IMPACT_code", "kcalsPerCommod", "kcalsPerDay_tot")
  dt.KcalShare.nonstaple[, (deleteListCol) := NULL]
  dt.KcalShare.nonstaple <- unique(dt.KcalShare.nonstaple)
  dt.KcalShare.nonstaple <- dt.KcalShare.nonstaple[staple_code %in% "nonstaple",]
  dt.KcalShare.nonstaple <- unique(dt.KcalShare.nonstaple)
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "value")
  # convert to percent
  dt.KcalShare.nonstaple[,value := value * 100]
  inDT <- dt.KcalShare.nonstaple
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  outName <- paste("dt.KcalShare_nonstaple", suffix, sep = ".")
  desc <- "Share of kcals from nonstaples in value column; other Kcal data included"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
  
  # # dt.nutrients.nonstapleShare, not currently used (Feb 2018) so commented out ------
  # dt.nut.nonstaple.share <- data.table::copy(dt.nutrients.sum.staples.long)
  # shareFormula <- "scenario + region_code.IMPACT159 + nutrient + year ~ staple_code"
  # dt.nut.nonstaple.share.wide <- data.table::dcast(data = dt.nut.nonstaple.share,
  #                                                  formula = shareFormula,
  #                                                  value.var = "value")
  # dt.nut.nonstaple.share.wide[, value := 100 * nonstaple/(nonstaple + staple) ][is.na(value), value := 0]
  #
  # dt.nutrients.nonstapleShare <- dt.nut.nonstaple.share.wide[,c("nonstaple", "staple") := NULL]
  
  #' dt.foodAvail.foodGroup -----
  #sum and convert to grams. Note that the food group alcohol results are for the grams in the total beverage, not ethanol.
  
  dt.foodAvail.foodGroup <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("scenario", "region_code.IMPACT159", "IMPACT_code", "year", "food_group_code", "foodAvailpDay")
  dt.foodAvail.foodGroup[, setdiff(names(dt.foodAvail.foodGroup), keepListCol) := NULL]
  
  dt.foodAvail.foodGroup <- dt.foodAvail.foodGroup[, foodavail.foodgroup.sum := sum(foodAvailpDay) * 1000,
                                                   by = c("scenario", "region_code.IMPACT159", "year", "food_group_code")]
  deleteListCol <- c("IMPACT_code", "foodAvailpDay")
  dt.foodAvail.foodGroup[, (deleteListCol) := NULL]
  dt.foodAvail.foodGroup <- unique(dt.foodAvail.foodGroup)
  data.table::setnames(dt.foodAvail.foodGroup, old = "foodavail.foodgroup.sum", new = "value")
  dt.foodAvail.foodGroup <- unique(dt.foodAvail.foodGroup)
  inDT <- dt.foodAvail.foodGroup
  inDT <- inDT[scenario %in% c("SSP1_NoCC", "SSP2_HGEM", "SSP2_NoCC","SSP3_NoCC"),]
  inDT <- inDT[year %in% c("X2010", "X2030", "X2050"),]
  outName <- paste("dt.foodAvail_foodGroup", suffix, sep = ".")
  desc <- "Sum of food available by food group, kgs per day"
  cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
}
finalizeScriptMetadata(metadataDT, sourceFile)
