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
# this is in nutrient per 100 grams of food item
dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)
# convert to nutrient per 1 kg
nutlist <- (names(dt.nutrients))[!names(dt.nutrients) %in% c("IMPACT_code", "food_group_code", "staple_code")]
dt.nutrients[, (nutlist) := lapply(.SD, function(x) (x * 10)), .SDcols = nutlist]
dt.foodnNuts <-  merge(dt.IMPACTfood, dt.nutrients, by = "IMPACT_code", all = TRUE)

# do operations over the whole data table
dt.foodnNuts[, `:=`(kcal.avail = foodAvailpDay * energy_kcal,
                    kcal.cereals_legumes = 0,
                    vit_c.avail = foodAvailpDay * vit_c_mg,
                    iron.raw = foodAvailpDay * iron_mg,
                    zinc.raw = foodAvailpDay * zinc_mg,
                    iron.heme = 0,
                    protein.animal.avail = 0,
                    stimsFactor = 0)]
# work on iron
# Heme iron = sum of iron from MPF x 0.40
# bioavailability of heme iron = Heme iron * 25%
#  Nonheme iron = sum of all remaining iron (including iron from MPF x 0.60)
# get iron and protein from fish and meats
fishNmeats <- c("fish", "meats")
dt.foodnNuts[food_group_code %in% fishNmeats, `:=`(
  iron.heme = foodAvailpDay * iron_mg * 0.4 * 0.25,
  iron.nonheme = foodAvailpDay * iron_mg * 0.6,
  protein.animal.avail = foodAvailpDay * protein_g)]

# get ironfrom items other than fish and meats (! means not in)
dt.foodnNuts[!food_group_code %in% fishNmeats, `:=`(
  iron.nonheme = foodAvailpDay * iron_mg)]

# get kcals from cereals and legumes
dt.foodnNuts[food_group_code %in% c("cereals", "legumes"), `:=`(
  kcal.cereals_legumes = foodAvailpDay * energy_kcal)]

# converted below Tea factor = [100% - {((tea intake (kg/d) * 1L/0.00792 kg) + (coffee intake (kg/d) * 1L/0.0442 kg * 1/1.5)) x 1/0.6L * 60%}]
# dt.foodnNuts[IMPACT_code == "ccafe", stimsFactor := 100 - (foodAvailpDay * (1/0.00792) *          (1/0.6) * 60)]
# dt.foodnNuts[IMPACT_code == "cteas", stimsFactor := 100 - (foodAvailpDay * (1/0.0442) * (1/1.5) * (1/0.6) * 60)]

dt.foodnNuts[IMPACT_code == "cteas", stimsFactor := foodAvailpDay * (1/0.00792)]
dt.foodnNuts[IMPACT_code == "ccafs", stimsFactor := foodAvailpDay * (1/0.0442) * (1/1.5)]

keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "iron.raw", "zinc.raw",
                 "iron.heme",  "iron.nonheme", "kcal.avail", "kcal.cereals_legumes", "vit_c.avail", "protein.animal.avail", "stimsFactor")
dt.foodnNuts <- dt.foodnNuts[, (keepListCol), with = FALSE]
# data.table::setkeyv(dt.foodnNuts, c("scenario", "region_code.IMPACT159", "year"))
dt.foodnNuts[,`:=`(
  sum.iron.raw              = sum(iron.raw),
  sum.iron.heme             = sum(iron.heme),
  sum.iron.nonheme          = sum(iron.nonheme),
  sum.kcal.avail            = sum(kcal.avail),
  sum.kcal.cereals_legumes  = sum(kcal.cereals_legumes),
  sum.zinc.raw              = sum(zinc.raw),
  sum.protein.animal.avail  = sum(protein.animal.avail),
  sum.stimsFactor           = (100 - sum(stimsFactor) * (1/0.6) * 60),
  sum.vit_c.avail           = sum(vit_c.avail)),
  by = .(scenario, region_code.IMPACT159, year)]

dt.foodnNuts[sum.stimsFactor < 40, sum.stimsFactor := 40]
deleteListCol <- c("IMPACT_code", "iron.raw",  "iron.heme", "iron.nonheme", "kcal.avail", "kcal.cereals_legumes",
"zinc.raw", "protein.animal.avail", "stimsFactor", "vit_c.avail")
dt.foodnNuts[, (deleteListCol) := NULL]
dt.foodnNuts <- unique(dt.foodnNuts)
dt.foodnNuts[,`:=`(
  vit_c_per_1000kcal = 1000 * sum.vit_c.avail/sum.kcal.avail,
  protein_per_1000kcal = 1000 * sum.protein.animal.avail/sum.kcal.avail)
  ]
dt.foodnNuts[,nonhemeBioavail := 15]
dt.foodnNuts[protein_per_1000kcal < 9  & vit_c_per_1000kcal < 35,
             nonhemeBioavail := 5]
dt.foodnNuts[protein_per_1000kcal >= 9 & protein_per_1000kcal < 28 & vit_c_per_1000kcal > 35,
             nonhemeBioavail := 10]
dt.foodnNuts[protein_per_1000kcal < 9 & vit_c_per_1000kcal >= 35 & vit_c_per_1000kcal > 106,
             nonhemeBioavail := 10]

dt.foodnNuts[, sum.iron.avail := sum.iron.heme + sum.iron.nonheme*nonhemeBioavail/100]
dt.foodnNuts[, iron.diff := sum.iron.avail - sum.iron.raw]

