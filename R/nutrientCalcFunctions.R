#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.
#' @name nutrientCalcFunctions.R
#' @description Has functions for cooking retention and budget share calculations
#'

switches <- function() {
  dt.nutrients.base <- getNewestVersion("dt.nutrients.base", fileloc("iData"))
  dt.nutrients.var <- getNewestVersion("dt.nutrients.var", fileloc("iData"))
  dt.nutrients.varFort <- getNewestVersion("dt.nutrients.varFort", fileloc("iData"))

  if (switch.vars == FALSE) {
    # old code commented out
    # dt.nutrients <- dt.USDAnutrients[usda_code %in% unique(dt.singleCodeLU$usda_code),]
    # dt.nutrients <- rbind(dt.composites.wld, dt.nutrients)
    #
    #   dt.nutrients <- getNewestVersion("dt.nutrients", fileloc("mData"))
    #   deleteListCol <- c("Ref_Desc", "phytate_source","ft_acds_tot_trans_g", "caffeine_mg", "cholesterol_mg",
    #                      "retentioncode_aus", "RetnDesc")
    #   dt.nutrients[, (deleteListCol) := NULL]
    dt.nutrients <- data.table::copy(dt.nutrients.base)

  } else {
    dt.nutrients <- data.table::copy(dt.nutrients.var)
    if (switch.fortification == TRUE) {
      dt.nutrients <- data.table::copy(dt.nutrients.varFort)
    }
  }

  deleteListCol <- c("ft_acds_tot_trans_g", "caffeine_mg", "cholesterol_mg")
  dt.nutrients[, (deleteListCol) := NULL]

  cols.cookretn <- names(dt.nutrients)[grep("_cr",names(dt.nutrients))]
  colsNotToMultiply <- c("IMPACT_code", "usda_code","Long_Desc", "food_group_code",
                         "staple_code", cols.cookretn,
                         "IMPACT_conversion", "edible_share")
  if (switch.vars == TRUE) {
    colsNotToMultiply <- c("region_code.IMPACT159", colsNotToMultiply)
  }
  nutrients.list <- names(dt.nutrients)[!(names(dt.nutrients) %in% colsNotToMultiply)]
  # dt.nutrients is in nutrient per 100 grams of the edible portion
  # reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
  dt.nutrients[ , (nutrients.list) := lapply(.SD, `*`, IMPACT_conversion / 100), .SDcols = nutrients.list]
  # reduce nutrient amount by conversion of all items to edible share
  dt.nutrients[ , (nutrients.list) := lapply(.SD, `*`, edible_share / 100), .SDcols = nutrients.list]

  # drop columns that are not needed.
  deleteListCol <- c("edible_share", "IMPACT_conversion")
  dt.nutrients[, (deleteListCol) := NULL]

  # use cooking retention values if TRUE -----
  if (switch.useCookingRetnValues == "TRUE") {
    cols.cookretn <- names(dt.nutrients)[grep("_cr",names(dt.nutrients))]
    colsToMultiply <- gsub("_cr", "", cols.cookretn)

    for (i in 1:length(cols.cookretn)) {
      nutrientName <-colsToMultiply[i]
      # multiply amount of nutrient times the cooking retention value (in percent) and divide by 100 to get to share
      dt.nutrients[,(nutrientName) := get(nutrientName) *
                     get(cols.cookretn[i]) / 100]
    }
    dt.nutrients <- dt.nutrients[,(c(cols.cookretn)) := NULL]
  }
  # fix fish if TRUE -----
  if (switch.fixFish == "TRUE")  {
    deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
    dt.nutrients <- dt.nutrients[!IMPACT_code %in% deleteListRow,]
  }
  # convert to nutrients per kg of food
  colsToMultiply <- names(dt.nutrients)[!names(dt.nutrients) %in% colsNotToMultiply]
  dt.nutrients[, (colsToMultiply) := lapply(.SD, function(x) (x * 10)), .SDcols = colsToMultiply]
  return(dt.nutrients)
}

# not being used because the vars stuff is in function above. Can delete.
# cookingRetFishCorrect.varieties <- function(switch.useCookingRetnValues, switch.fixFish) {
#   dt.nutrients <- getNewestVersion("dt.nutVarieties_sr28", fileloc("mData"))
#
#   # dt.nutrients <- merge(dt.nutrients, dt.bioavail_iron, by = c("scenario", "region_code.IMPACT159", "year"))
#   # dt.nutrients <- merge(dt.nutrients, dt.bioavail_zinc, by = c("scenario", "region_code.IMPACT159", "year"))
#
#   cols.cookretn <- names(dt.nutrients)[grep("_cr",names(dt.nutrients))]
#   colsNotToMultiply <- c("IMPACT_code" ,"usda_code","Long_Desc", "food_group_code",
#                          "staple_code", "Ref_Desc", "retentioncode_aus", "RetnDesc", cols.cookretn, "phytate_source",
#                          "IMPACT_conversion", "edible_share", "food_group_code", "staple_code")
#
#   nutrients.list <- names(dt.nutrients)[!(names(dt.nutrients) %in% colsNotToMultiply)]
#   # dt.nutrients is in nutrient per 100 grams of the edible portion
#   # reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
#   dt.nutrients[ , (nutrients.list) := lapply(.SD, `*`, IMPACT_conversion / 100), .SDcols = nutrients.list]
#   # reduce nutrient amount by conversion of all items to edible share
#   dt.nutrients[ , (nutrients.list) := lapply(.SD, `*`, edible_share / 100), .SDcols = nutrients.list]
#
#   # drop columns that are not needed.
#   deleteListCol <- c("edible_share", "IMPACT_conversion")
#   dt.nutrients[, (deleteListCol) := NULL]
#
#   # use cooking retention values if TRUE -----
#   if (switch.useCookingRetnValues == "TRUE") {
#     # get cooking retention values. Update: commented out because dt.nutrients now has crs in it (11/25/2017)
#     # dt.cookRetn <- getNewestVersion("dt.cookingRet")
#     # data.table::setkey(dt.nutrients,IMPACT_code)
#     # data.table::setkey(dt.cookRetn,IMPACT_code)
#     # dt.temp <- dt.nutrients[dt.cookRetn]
#
#     for (i in 1:length(cols.cookretn)) {
#       nutrientName <-
#         substr(x = cols.cookretn[i], 1, nchar(cols.cookretn[i]) - 3)
#       nutRetName <- cols.cookretn[i]
#       # multiply amount of nutrient times the cooking retention value (in percent) and divide by 100 to get to share
#       dt.nutrients[,(nutrientName) := eval(parse(text = nutrientName)) *
#                      eval(parse(text = nutRetName)) / 100]
#     }
#     dt.nutrients <- dt.nutrients[,(c(cols.cookretn)) := NULL]
#   }
#   # fix fish if TRUE -----
#   if (switch.fixFish == "TRUE")  {
#     deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
#     dt.nutrients <- dt.nutrients[!IMPACT_code %in% deleteListRow,]
#   }
#   # convert to nutrients per kg of food
#   colsToMultiply <- names(dt.nutrients)[!names(dt.nutrients) %in% colsNotToMultiply]
#   dt.nutrients[, (colsToMultiply) := lapply(.SD, function(x) (x * 10)), .SDcols = colsToMultiply]
#   return(dt.nutrients)
# }

#' Title budgetShare
#' calculate the share of per capita income spent on IMPACT commodities
#' writes out data table to the results directory
#' @param dt.IMPACTfood
#' @param region - the grouping of countries to aggregate to
#' @return null
#' @export
budgetShareNpriceGrowth <- function(dt.foodNnuts, suffix) {
  #' prices are in 2005 dollars per metric ton
  #' pcGDP is in 1000 2005 dollars.
  #' 'FoodAvailability' variable is in kgs/person/year. DinY is days in year
  dt.budget <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("FoodAvailability", "PWX0", "PCX0", "pcGDPX0", "scenario", "region_code.IMPACT159", "year")
  dt.budget[, setdiff(names(dt.budget), keepListCol) := NULL]

  # data.table::setkeyv(dt.temp, c("scenario", "region_code.IMPACT159", "year"))
  # budget is in 1000 2005 dollars
  # dt.temp[, budget.PWX0 := (sum(FoodAvailability * PWX0 / 1000 )) / 1000, by = eval(data.table::key(dt.temp))]
  # dt.temp[, budget.PCX0 := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = eval(data.table::key(dt.temp))]
  dt.budget[, budget.PWX0 := (sum(FoodAvailability * PWX0 / 1000 )) / 1000, by = c("scenario", "region_code.IMPACT159", "year")]
  dt.budget[, budget.PCX0 := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("scenario", "region_code.IMPACT159", "year")]
  # data.table::setkey(dt.temp, budget.PWX0) is this necessary?
  # dt.budget <- dt.temp[!duplicated(budget.PCX0),]
  #  dt.budget <- data.table::copy(dt.temp)
  deleteListCol <- c("FoodAvailability", "PCX0","PWX0")
  dt.budget[,(deleteListCol) := NULL]
  dt.budget <- unique(dt.budget)
  # at world prices -----
  dt.budget[, incShare.PWX0 := 100 * budget.PWX0 / pcGDPX0 ]
  # at domestic prices -----
  dt.budget[, incShare.PCX0 := 100 * budget.PCX0 / pcGDPX0 ]
  data.table::setkeyv(dt.budget, c("scenario", "region_code.IMPACT159", "year"))
  inDT <- dt.budget
  outName <- paste("dt.budgetShare", suffix, sep = ".")
  desc <- "share of food budget (domestic and world prices) in per capita GDP"
  cleanup(inDT,outName,fileloc("resultsDir"), desc = desc)

  # get world price change from 2010 to 2050 by food groups
  #  dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo", fileloc("mData"))
  #  dt.foodGroupsInfo[, c("description", "food_group_assignment", "food_groups", "food_group_codes", "staple_category") := NULL]
  # dt.temp <- merge(dt.foodGroupsInfo, dt.temp, by = "IMPACT_code")
  dt.PriceGrowth <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "FoodAvailability", "food_group_code", "staple_code", "PWX0")
  dt.PriceGrowth[, setdiff(names(dt.PriceGrowth), keepListCol) := NULL]
  deleteListRow <- c("c_aqan","c_aqpl", "c_beer", "c_Crust", "c_FrshD", "c_FshOil", "c_Mllsc", "c_ODmrsl", "c_OMarn",
                     "c_OPelag", "c_spirits", "c_wine")
  keepListYears <- c("X2010", "X2050")
  dt.PriceGrowth <- dt.PriceGrowth[!IMPACT_code %in% deleteListRow & year %in% keepListYears]
  dt.PriceGrowth <- unique(dt.PriceGrowth)
  dt.PriceGrowth <- dt.PriceGrowth[, growthRatePW :=  lapply(.SD, function(x)((x/data.table::shift(x))^(1/(2050 - 2010)) - 1) * 100),
                                   .SDcols = "PWX0", by = c("scenario","IMPACT_code")]
  dt.PriceGrowth <- dt.PriceGrowth[year %in% "X2050"]
  dt.PriceGrowth[, c("year", "PWX0") := NULL]

  # Not used elsewhere. commented out March 22, 2018
  #     formula.wide.scen <- "IMPACT_code + FoodAvailability + food_group_code + staple_code ~ scenario"
  #   dt.PriceGrowth.wide.scen <- data.table::dcast(
  #     data = dt.PriceGrowth,
  #     formula = formula.wide.scen,
  #     value.var = "growthRatePW")

  # formula.wide.FG <- "IMPACT_code + FoodAvailability + staple_code + scenario ~ food_group_code"
  # dt.PriceGrowth.wide.FG <- data.table::dcast(
  #   data = dt.PriceGrowth,
  #   formula = formula.wide.FG,
  #   value.var = "growthRatePW")
  #
  # formula.wide.staple <- "IMPACT_code + FoodAvailability + food_group_code + scenario ~ staple_code"
  # dt.PriceGrowth.wide.staple <- data.table::dcast(
  #   data = dt.PriceGrowth,
  #   formula = formula.wide.staple,
  #   value.var = "growthRatePW")

  #calculate weighted average price growth rate by food groups
  dt.PriceGrowth[, growthRateAve.FG := weighted.mean(growthRatePW, FoodAvailability), by = c("region_code.IMPACT159", "scenario", "food_group_code")]
  #calculate weighted average price growth rate by food groups
  dt.PriceGrowth[, growthRateAve.staple := weighted.mean(growthRatePW, FoodAvailability), by = c("region_code.IMPACT159", "scenario", "staple_code")]

  dt.PriceGrowth.FG <- data.table::copy(dt.PriceGrowth)
  deleteListCol <- c("IMPACT_code","growthRatePW", "FoodAvailability")
  dt.PriceGrowth.FG <- unique(dt.PriceGrowth.FG[, c(deleteListCol, "staple_code", "growthRateAve.staple") := NULL])

  dt.PriceGrowth.staple <- data.table::copy(dt.PriceGrowth)
  dt.PriceGrowth.staple <- unique(dt.PriceGrowth.staple[, c(deleteListCol, "food_group_code", "growthRateAve.FG") := NULL])

  formula.wide.staple.scen <- "region_code.IMPACT159 + staple_code  ~ scenario"
  dt.PriceGrowth.staple.wide.scen <- data.table::dcast(
    data = dt.PriceGrowth.staple,
    formula = formula.wide.staple.scen,
    value.var = "growthRateAve.staple")

  inDT <- dt.PriceGrowth.staple.wide.scen
  outName <- "dt.priceGrowth.staple.wide"
  desc <- "Growth in prices of staples"
  cleanup(inDT, outName, fileloc("resultsDir"), "xlsx", desc = desc)

  formula.wide.FG.scen <- "region_code.IMPACT159 + food_group_code  ~ scenario"
  dt.PriceGrowth.FG.wide.scen <- data.table::dcast(
    data = dt.PriceGrowth.FG,
    formula = formula.wide.FG.scen,
    value.var = "growthRateAve.FG")

  inDT <- dt.PriceGrowth.FG.wide.scen
  outName <- "dt.priceGrowth.FG.wide"
  desc <- "Growth in prices of food groups"
  cleanup(inDT, outName, fileloc("resultsDir"), "xlsx", desc = desc)
}
#' iron and zinc bioavailability adjustments
adjustBioavailability <- function(dt.foodNnuts) {
  #  dt.nutsReqPerCap <- getNewestVersion(req)
  #   nutListReq <- c("iron_mg", "zinc_mg" )
  # nutListReq.bio.Q <- paste(c(nutListReq, "phytate_mg", "vit_c_mg", "energy_kcal", "protein_g"), "Q", sep = ".")
  #' create dt.bioavail and use it instead of food.agg. It is merged back into food agg with just iron or zinc
  #    dt.bioavail <- data.table::copy(dt.foodNnuts)
  # dt.foodNnuts nutrient available are average availability per capita per do so no need to do the next step. March 29, 2018
  # dt.bioavail <- dt.bioavail[, (nutListReq.bio.Q) := lapply(.SD, function(x)
  #   (x * dt.foodNnuts[['foodAvailpDay']])), .SDcols = nutListReq.bio]
  #
  # iron bioavailability -----
  #' initialize a data table to hold the bioavailable iron results
  dt.bioavail_iron <- data.table::copy(dt.foodNnuts)
  dt.bioavail_iron[, `:=`(kcal.avail = kcalsPerCommod,
                          kcal.cereals_legumes = 0,
                          iron.heme_mg = 0,
                          iron.nonheme_mg = 0,
                          protein.animal.avail_g = 0,
                          stimsFactor = 0
  )]

  #' constants for iron calcs
  hemeIronshare <- 0.4 # Heme iron = sum of iron from meats, poultry and fish x 0.40
  hemeIronBioavail <- 0.25 # bioavailability of heme iron = Heme iron * 25%
  nonhemeIronShare <- 0.60  #' Nonheme iron = sum of all remaining iron (including iron from MPF x 0.60)

  #' get iron and protein from fish and meats and calculate bioavailable heme iron
  fishNmeats <- c("fish", "meats")
  dt.bioavail_iron[food_group_code %in% fishNmeats, `:=`(
    iron.heme_mg = iron_mg * hemeIronshare * hemeIronBioavail,
    iron.nonheme_mg = iron_mg * nonhemeIronShare,
    protein.animal.avail_g = protein_g)]

  #' get non heme iron from items other than fish and meats (! means not in)
  dt.bioavail_iron[!food_group_code %in% fishNmeats, `:=`(
    iron.nonheme_mg = iron_mg)]

  #' get kcals from cereals and legumes
  dt.bioavail_iron[food_group_code %in% c("cereals", "legumes"), `:=`(
    kcal.cereals_legumes = kcalsPerCommod)]

  #' converted below Tea factor = [100% - {((tea intake (kg/d) * 1L/0.00792 kg) + (coffee intake (kg/d) * 1L/0.0442 kg * 1/1.5)) x 1/0.6L * 60%}]
  #' dt.food.agg[IMPACT_code == "ccafe", stimsFactor := 100 - (foodAvailpDay * (1/0.00792) * (1/0.6) * 60)]
  #' dt.food.agg[IMPACT_code == "cteas", stimsFactor := 100 - (foodAvailpDay * (1/0.0442) * (1/1.5) * (1/0.6) * 60)]
  teaFactor <- 0.00792
  coffeeFactor <- 0.0442
  #' units of stimsFactor are
  dt.bioavail_iron[IMPACT_code == "cteas", stimsFactor := foodAvailpDay * (1/teaFactor)]
  dt.bioavail_iron[IMPACT_code == "ccafs", stimsFactor := foodAvailpDay * (1/coffeeFactor) * (1/1.5)]

  keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "iron_mg",
                   "iron.heme_mg", "iron.nonheme_mg", "kcal.avail", "kcal.cereals_legumes", "vit_c_mg", "protein.animal.avail_g", "stimsFactor")
  dt.bioavail_iron[, setdiff(names(dt.bioavail_iron), keepListCol) := NULL]
  dt.bioavail_iron[,`:=`(
    sum.iron_mg = sum(iron_mg),
    sum.iron.heme_mg = sum(iron.heme_mg),
    sum.iron.nonheme_mg = sum(iron.nonheme_mg),
    sum.kcal.avail = sum(kcal.avail),
    sum.kcal.cereals_legumes = sum(kcal.cereals_legumes),
    sum.protein.animal.avail_g = sum(protein.animal.avail_g),
    sum.stimsFactor = (100 - sum(stimsFactor) * (1/0.6) * 60),
    sum.vit_c_mg = sum(vit_c_mg)),
    by = .(scenario, region_code.IMPACT159, year)]

  dt.bioavail_iron[sum.stimsFactor < 40, sum.stimsFactor := 40]
  deleteListCol <- c("IMPACT_code", "iron_mg", "iron.heme_mg", "iron.nonheme_mg", "kcal.avail", "kcal.cereals_legumes",
                     "protein.animal.avail_g", "stimsFactor", "vit_c_mg")
  dt.bioavail_iron[, (deleteListCol) := NULL]
  dt.bioavail_iron <- unique(dt.bioavail_iron)

  #' adjust non heme iron for interactions with vitamin c and protein. These values from Table 2 in Murphy et al, 1992.
  #' Note: for protein_g_per_1000kcal > 27, nonhemeBioavail is 15 %
  #' for protein_g_per_1000kcal 9- 27, nonhemeBioavail is 15, unless vit_c__mg_per_1000kcal >35

  dt.bioavail_iron[,`:=`(
    vit_c__mg_per_1000kcal = 1000 * sum.vit_c_mg/sum.kcal.avail,
    protein_g_per_1000kcal = 1000 * sum.protein.animal.avail_g/sum.kcal.avail)
    ]

  #' units for nonhemeBioavail are percent
  dt.bioavail_iron[,nonhemeBioavail := 15] # starting value; now adjust down
  dt.bioavail_iron[protein_g_per_1000kcal < 9 & vit_c__mg_per_1000kcal < 35,
                   nonhemeBioavail := 5]
  dt.bioavail_iron[protein_g_per_1000kcal < 9 & vit_c__mg_per_1000kcal >= 35 & vit_c__mg_per_1000kcal <= 105,
                   nonhemeBioavail := 10]
  dt.bioavail_iron[protein_g_per_1000kcal >= 9 & protein_g_per_1000kcal <= 27 & vit_c__mg_per_1000kcal < 35,
                   nonhemeBioavail := 10]

  dt.bioavail_iron[, iron_mg := sum.iron.heme_mg + (sum.iron.nonheme_mg*(nonhemeBioavail/100) * (sum.stimsFactor/100))]
  dt.bioavail_iron[, bioavailability.iron := 100 * iron_mg/sum.iron_mg]
  dt.bioavail_iron <- unique(dt.bioavail_iron)
  inDT <- dt.bioavail_iron
  outName <- paste("dt.bioavail_iron", suffix, sep = ".")
  desc <- "Bioavailable iron"
  cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
  keepListCol <- c("scenario","region_code.IMPACT159", "year", "bioavailability.iron")
  inDT[, setdiff(names(inDT), keepListCol) := NULL]

  #' adjust iron in dt.food.agg. Moved to later. March 29 2018
  # temp <- merge(dt.food.agg, dt.bioavail_iron, by = c("scenario", "region_code.IMPACT159", "year"))
  # dt.food.agg <- temp[,iron_mg.Q := iron_mg.Q * bioavailability.iron/100][,c("bioavailability.iron") := NULL]
  #
  #' zinc bioavailability -----
  dt.bioavail_zinc <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "zinc_mg", "phytate_mg")
  dt.bioavail_zinc[, setdiff(names(dt.bioavail_zinc), keepListCol) := NULL]
  dt.bioavail_zinc[,`:=`(
    sum.zinc_mg = sum(zinc_mg),
    sum.phytate_mg = sum(phytate_mg)),
    by = .(scenario, region_code.IMPACT159, year)]
  deleteListCol <- c("IMPACT_code", "phytate_mg")
  dt.bioavail_zinc[, (deleteListCol) := NULL]
  dt.bioavail_zinc <- unique( dt.bioavail_zinc)
  #' This is based on equation 11 of 1. L. V. Miller, N. F. Krebs, K. M. Hambidge,
  #' A mathematical model of zinc absorption in humans as a function of dietary zinc and phytate.
  #' J. Nutr. 137, 135–41 (2007).
  #' amax - maximal absorption of zinc
  #' taz - total daily absorbed zinc
  #' tdz - total daily dietary zinc
  #' tdp - total daily dietary phytate
  #' units of above are millimoles per day
  #' - kp and kr are the equilibrium dissociation constants of zinc-phytate and zinc-receptor binding, respectively
  zincAtomMass <- 65.38
  phytMolecMass <- 660

  #' Three parameters have been updated in a 2010 paper.
  amax2010 = .091; kr2010 = .033; kp2010 = .68 # updated parameters from
  #' Reference : Hambidge KM, Miller LV, Westcott JE, Sheng X, Krebs NF. Zinc bioavailability and homeostasis.
  #' Am J Clin Nutr. 2010;91:1478S-83S.
  dt.bioavail_zinc[, tdp := sum.phytate_mg/phytMolecMass][, tdz := sum.zinc_mg/zincAtomMass]
  dt.bioavail_zinc[, millernum2010 := amax2010 + tdz + kr2010 * (1 + (tdp / kp2010))]
  dt.bioavail_zinc[, taz := 0.5 * (millernum2010 - sqrt(millernum2010^2 - 4 * amax2010 * tdz))]
  dt.bioavail_zinc[, zinc_mg := taz * zincAtomMass]
  dt.bioavail_zinc[, bioavailability.zinc := 100 * zinc_mg/sum.zinc_mg]
  dt.bioavail_zinc <- unique( dt.bioavail_zinc)
  inDT <-  dt.bioavail_zinc
  outName <- paste("dt.bioavail_zinc", suffix, sep = ".")
  desc <- "bioavailable zinc"
  cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
  keepListCol <- c("scenario","region_code.IMPACT159", "year", "sum.zinc_mg", "sum.phytate_mg", "bioavailability.zinc")
  dt.bioavail_zinc[, setdiff(names(dt.bioavail_zinc), keepListCol) := NULL]

  #' do some graphing
  dt.regions <- regionAgg("WB")
  scenChoice <- "SSP1-NoCC-REF"
  gyear <- "X2010"
  mainTitle <- paste("Dietary zinc vs dietary phytate;\n ", "scenario - ", scenChoice, ", year - ", gyear, sep = "")
  temp.all <- merge( dt.bioavail_zinc, dt.regions, by = "region_code.IMPACT159")
  pdf(paste(fileloc("gDir"), "/phytatePlot",gyear,".pdf", sep = ""))
  par(mai = c(.8,1,0.8,.5),oma = c(1,1,2,1), mfrow = c(2,2))
  for (i in unique(temp.all$region_code)) {
    gTitle <- paste("Income group - ", i, sep = "")
    temp <- temp.all[region_code %in% i & scenario %in% "SSP1-NoCC-REF" & year %in% "X2050",]
    plot(temp$sum.zinc_mg, temp$sum.phytate_mg, type = "p", main = gTitle,
         xlab = "Dietary zinc (mg)", ylab = "Dietary phytate (mg)", ylim = c(800,8000),
         xlim = c(0,30), pch = 16, cex = .7)
  }
  mtext(mainTitle, outer = TRUE, cex = 1)
  dev.off()
  #' get rid of dietary zinc and phytate in dt.bioavail_zinc. Only needed for graphing above
  dt.bioavail_zinc[, c("sum.phytate_mg", "sum.zinc_mg") := NULL]
  dt.bioavail_zinc <- unique(dt.bioavail_zinc)

  #' adjust zinc and iron in dt.foodNnuts
  bioavailNutrients <- merge(dt.bioavail_zinc, dt.bioavail_iron, by = c("scenario", "region_code.IMPACT159", "year"))
  temp <- merge(dt.foodNnuts,  bioavailNutrients, by = c("scenario", "region_code.IMPACT159", "year"))
  dt.foodNnuts <- temp[,zinc_mg := zinc_mg * bioavailability.zinc/100]
  dt.foodNnuts <- temp[,iron_mg := iron_mg * bioavailability.iron/100]
  dt.foodNnuts[,c("bioavailability.zinc", "bioavailability.iron") := NULL]
  return(dt.foodNnuts)
}
