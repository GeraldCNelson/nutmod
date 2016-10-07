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

dt.nutrients <- getNewestVersion("dt.nutrients")
keepListCol <- c("IMPACT_code", "protein_g", "fat_g", "carbohydrate_g", "totalfiber_g")
# , "calcium_mg", "iron_mg",
# "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg", "vit_c_mg",
# "thiamin_mg", "riboflavin_mg", "niacin_mg", "vit_b6_mg", "folate_µg",
# "vit_b12_µg", "vit_a_rae_µg", "vit_e_mg", "vit_d_µg", "vit_k_µg",
# "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "cholesterol_mg",
# "ft_acds_tot_trans_g")
dt.nutrients <- dt.nutrients[, (keepListCol), with = FALSE]
nutlist <- colnames(dt.nutrients)[!colnames(dt.nutrients) %in% "IMPACT_code"]
itemlist <- unique(dt.nutrients$IMPACT_code)
d <- vector(mode = "numeric", length = length(nutlist) * length(itemlist))
k = 1
for (i in 1:length(nutlist)) {
  for (j in 2:length(itemlist)) {
    d[k] <- (dt.nutrients[IMPACT_code == itemlist[j],get(nutlist[i])] - dt.nutrients[IMPACT_code == itemlist[j - 1],get(nutlist[i])])^2
    k = k + 1
  }
}
