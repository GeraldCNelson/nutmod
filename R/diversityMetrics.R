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
# library(diverse)
# library(vegan)

library(Matrix) # isSymmetric
library(matrixcalc) # is.positive.definite
library(expm) # sqrtm


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

macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol", "kcals.carbohydrate")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")
others <- c("caffeine_mg", "phytate_mg", "cholesterol_mg")
cookingretention <- c( "thiamin_mg_cr" , "vit_b12_µg_cr", "riboflavin_mg_cr", "niacin_mg_cr", "vit_b6_mg_cr", "calcium_mg_cr",
"iron_mg_cr", "folate_µg_cr",  "potassium_g_cr", "magnesium_mg_cr", "phosphorus_mg_cr",
"vit_a_rae_µg_cr", "vit_c_mg_cr", "vit_e_mg_cr", "zinc_mg_cr" )

# read in nutrients data and optionally apply cooking retention values -----
switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#dt.nutrients is per 100 gm of the raw product (ie before edible portion is applied)
dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
keepListCol <- c(macroNutrients, vitamins, minerals, fattyAcids)
dt.nutrientNames_Units <- dt.nutrientNames_Units[,(keepListCol), with = FALSE]
# MFAD calculations
#mfad = f_i * ((sum over i from 1 to n)((sum over j from 1 to n) of d_ij/n)
#d_ij = sum over k from 1 to K(i_k - j_k)^2
# K is number of nutrients,
# N is number of food items, i and j are different food items
# f_i proportion of ith food item in the diet
keepListCol <- c("IMPACT_code", macroNutrients, vitamins, minerals, fattyAcids)
dt.nutrients <- dt.nutrients[, (keepListCol), with = FALSE]
nutlist <- names(dt.nutrients)[!names(dt.nutrients) %in% "IMPACT_code"]

# for the MFAD, all nutrients must be in the same units. I am choosing g/100g
for (i in nutlist) {

units <- dt.nutrientNames_Units[2, get(i)]
if (units == "mg") dt.nutrients[,get(i) := get(i)/1000]
if (units == "µg") dt.nutrients[,get(i) := get(i)/1e-6]
}

itemlist <- unique(dt.nutrients$IMPACT_code)
d <- vector(mode = "numeric", length = length(nutlist) * length(itemlist))

for (i in 1:length(itemlist)) {
  for (j in 1:length(itemlist)) {
    for (k in 1:length(nutlist)) {
    d[k] <- (dt.nutrients[IMPACT_code == itemlist[i],get(nutlist[k])] - dt.nutrients[IMPACT_code == itemlist[j],get(nutlist[k])])^2
    }
  }
}

dt.nutrients.trans <- transpose(dt.nutrients[,!"IMPACT_code"])
setnames(dt.nutrients.trans, old = names(dt.nutrients.trans), new = itemlist)
d <- matrix(, nrow = length(itemlist), ncol = length(itemlist))
dt.nutrients.trans[,nutrient := nutlist]
for (i in 1:length(itemlist)) {
  for (j in 1:length(itemlist)) {
    d[i,j] <- sum(dt.nutrients.trans[, (get(itemlist[i])-get(itemlist[j]))^2])
  }
}

# qualifying nutrient balance score
#qualifying nutrients
# Water", "protein_g",  "totalfiber_g", "Calcium, Ca", "Iron, Fe", "Magnesium, Mg", "Phosphorus, P", "Potassium, K", "Zinc, Zn", "Vitamin C, total ascorbic acid", "Thiamin", "Riboflavin", "Niacin", "Vitamin B-6", "Folate, DFE", "Vitamin B-12", "Vitamin A, RAE", "Vitamin A, IU", "Vitamin E (alpha-tocopherol)", "Vitamin D (D2 + D3)", "Vitamin D", "Vitamin K (phylloquinone)", "Pantheoic Acid", "Linolenic Acid", "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)
nutrients.qual <- c("protein_g", "totalfiber_g", minerals, vitamins)
nutrients.qual.missing <- c("Vitamin A, IU", "Vitamin D IU",  "Pantheoic Acid", "Linolenic Acid", "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)")
nutrients.disqual <- c("fat_g", "carbohydrate_g", "sodium_mg",  addedSugar, fattyAcids, "caffeine_mg", "cholesterol_mg")
nutrients.other <- c("phytate_mg", "kcals.fat", "kcals.protein", "kcals.carbohydrate", "kcals.sugar", "kcals.ethanol")
dt.nutrients.qual <- dt.nutrients[,c(nutrients.disqual, nutrients.other) := NULL]
kcalRef <- 2000
dt.nutrients.qual[, (nutrients.qual) := get((nutrients.qual)) * kcalRef/energy_kcal]
dt.req.macro <- getNewestVersion("req.RDA.macro.percap")
dt.req.vits <- getNewestVersion("req.RDA.vits.percap")
dt.req.minrls <- getNewestVersion("req.RDA.minrls.percap")


# qualifying index
# the ratio of each qualifying nutrient contained in 2000 kcal of a given food relative to its Dietary Reference Intake (DRI) value.
