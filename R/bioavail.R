#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
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

#' @description To be added
#'
library(data.table)

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
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "FoodAvailability")
dt.IMPACTfood <- dt.IMPACTfood[, (keepListCol), with = FALSE]
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood <- unique(dt.IMPACTfood)
# convert food availability from per year to per day
dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")][,FoodAvailability := NULL]

# read in nutrients data and optionally apply cooking retention values -----
switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)
dt.foodnNuts <-  merge(dt.IMPACTfood, dt.nutrients, by = "IMPACT_code", all = TRUE)
dt.foodnNuts.zinc <- data.table::copy(dt.foodnNuts)
# Heme iron = sum of iron from MPF x 0.40
# bioavailability of heme iron = Heme iron * 25%
#  Nonheme iron = sum of all remaining iron (including iron from MPF x 0.60)
fishNmeats <- c("fish", "meats")
dt.foodnNuts[food_group_code %in% fishNmeats, `:=`(
  iron.heme = foodAvailpDay * iron_mg * 0.4 * 0.25,
  iron.nonheme = foodAvailpDay * iron_mg * 0.6,
  protein.animal.avail = foodAvailpDay * protein_g)]
dt.foodnNuts[!food_group_code %in% fishNmeats, `:=`(
  iron.nonheme = foodAvailpDay * iron_mg,
  iron.heme = 0,
  protein.animal.avail = 0
)]
dt.foodnNuts[, `:=`(kcal.avail = foodAvailpDay * energy_kcal,
                    vit_c.avail = foodAvailpDay * vit_c_mg,
                    stimsFactor = 0)]
# converted below Tea factor = [100% - {((tea intake (kg/d) * 1L/0.00792 kg) + (coffee intake (kg/d) * 1L/0.0442 kg * 1/1.5)) x 1/0.6L * 60%}]

# dt.foodnNuts[IMPACT_code == "ccafe", stimsFactor := 100 - (foodAvailpDay * (1/0.00792) *          (1/0.6) * 60)]
# dt.foodnNuts[IMPACT_code == "cteas", stimsFactor := 100 - (foodAvailpDay * (1/0.0442) * (1/1.5) * (1/0.6) * 60)]

dt.foodnNuts[IMPACT_code == "cteas", stimsFactor := foodAvailpDay * (1/0.00792)]
dt.foodnNuts[IMPACT_code == "ccafs", stimsFactor := foodAvailpDay * (1/0.0442) * (1/1.5)]

keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "iron_mg",
                 "iron.heme",  "iron.nonheme", "kcal.avail", "vit_c.avail", "protein.animal.avail", "stimsFactor")
dt.foodnNuts <- dt.foodnNuts[, (keepListCol), with = FALSE]
data.table::setkeyv(dt.foodnNuts, c("scenario", "region_code.IMPACT159", "year"))
dt.foodnNuts[,`:=`(
  sum.iron.raw             = sum(iron_mg),
  sum.iron.heme            = sum(iron.heme),
  sum.iron.nonheme         = sum(iron.nonheme),
  sum.kcal.avail           = sum(kcal.avail),
  sum.protein.animal.avail = sum(protein.animal.avail),
  sum.stimsFactor = (100 - sum(stimsFactor) * (1/0.6) * 60),
  sum.vit_c.avail          = sum(vit_c.avail)),
  by = .(scenario, region_code.IMPACT159, year)
  ]

dt.foodnNuts[sum.stimsFactor < 40, sum.stimsFactor := 40]

dt.foodnNuts[, IMPACT_code := NULL]
dt.foodnNuts <- unique(dt.foodnNuts)
dt.foodnNuts[,`:=`(
  vit_c_per_1000kcal = 1000 * sum.vit_c.avail/sum.kcal.avail,
  protein_per_1000kcal = 1000 * sum.protein.animal.avail/sum.kcal.avail)
  ]
dt.foodnNuts[,nonhemeBioavail := 15]
dt.foodnNuts[protein_per_1000kcal < 9 &
               vit_c_per_1000kcal > 35,nonhemeBioavail := 5]
dt.foodnNuts[protein_per_1000kcal >= 9 &
               protein_per_1000kcal < 27 &
               vit_c_per_1000kcal > 35,nonhemeBioavail := 10]
dt.foodnNuts[protein_per_1000kcal < 9 &
               vit_c_per_1000kcal >= 27 &
               vit_c_per_1000kcal > 35,nonhemeBioavail := 10]
dt.foodnNuts[protein_per_1000kcal < 9 &
               vit_c_per_1000kcal >= 35 &
               vit_c_per_1000kcal > 105,nonhemeBioavail := 10]

dt.foodnNuts[, sum.iron.avail := sum.iron.heme + sum.iron.nonheme*nonhemeBioavail/100]
dt.foodnNuts[, iron.diff := sum.iron.avail - sum.iron.raw]

# zinc bioavailability
keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "foodAvailpDay",
                 "energy_kcal", "food_group_code", "zinc_mg")
dt.foodnNuts.zinc <- dt.foodnNuts.zinc[, (keepListCol), with = FALSE]
dt.foodnNuts.zin[, kcals.avail := foodAvailpDay * energy_kcal]
dt.foodnNuts.zinc[food_group_code %in% c("cereals", "legumes"), `:=`(
  sum.kcals.cereals_legumes = sum(kcals.avail)),
  by = .(scenario, region_code.IMPACT159, year)]

