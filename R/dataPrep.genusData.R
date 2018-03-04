#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, GENuS Data import
# Intro ---------------------------------------------------------------
#Copyright (C) 2018 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify i
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

#' @description Read in the GENuS and prepare for merging with IMPACT data. This script is sourced from dataManagement.ODBCaccess.R
#' library(data.table)
GENuS_char <-  c("X.", "GENuS_foodID", "GENuS_FoodName", "FCT_FoodName")
# GENuS_num <-  c("Calories", "Protein", "Fat", "Carbohydrates", "Vitamin.C", "Vitamin.A",
#                 "Folate", "Calcium", "Iron", "Zinc", "Potassium", "Dietary.Fiber", "Copper", "Sodium", "Phosphorus",
#                 "Thiamin", "Riboflavin", "Niacin", "B6", "Choline", "Magnesium", "Manganese", "Saturated.FA",
#                 "Monounsaturated.FA", "Polyunsaturated.FA", "Omega.3..USDA.only.", "B12..USDA.only.", "Water.moisture",
#                 "Ash", "Refuse")
GENuS.fct.files <- c("WestAfrica", "US", "OldAfrica", "NEASIA",  "India", "ASEANFoods") # "LATINFoods", imported via csv
DT.names <- c("WAfrica", "US", "oldAfrica", "NEASIA",  "India", "ASEAN") # LatAm assigned below
for (i in 1:length(GENuS.fct.files)) {
  load(paste("data-raw/NutrientData/GENus_FCTs/", "FCT_GENuS_", GENuS.fct.files[i],".RData", sep = ""))
  dt <- as.data.table(x)
  if (i == 6) { # get rid of AsIs class for thiamin in ASEANFoods
    class(dt$Thiamin) <- NULL
    dt[Thiamin %in% "nd", Thiamin := NA]
    class(dt$GENuS_FoodName) <- NULL
    class(dt$FCT_FoodName) <- NULL
  }
  dt <- dt[, (GENuS_char) := lapply(.SD, as.character), .SD = GENuS_char]
  dt_NA <- names(dt)[sapply(dt,function(x) all(is.na(x)))]
  #  GENuS_num <-  GENuS_num[!GENuS_num %in% GENuS_NA]
  dt_num <-  names(dt)[!names(dt) %in% dt_NA & !names(dt) %in% GENuS_char]
  dt <- dt[, (dt_num) := lapply(.SD, as.numeric), .SD = dt_num]
  dt <- dt[, (dt_NA) := lapply(.SD, as.logical), .SD = dt_NA]

  #  dt[, GENuS_foodID := paste(GENuS_foodID, DT.names[i], sep = ".")]
  dt[, GENuS_region := DT.names[i]]

  # attributes(dt) <- NULL
  dt[, `X.` := NULL]
  GENusDT <- paste0("dt.GENuS.",DT.names[i])
  assign(GENusDT, dt)
}

#' Import Latam from csv so font encoding is not a problem
library(readr)
dt.GENuS.LatAm <- as.data.table(read_csv("data-raw/NutrientData/GENus_FCTs/FCT_GENuS_LATINFoods.csv", col_names = TRUE, cols(
  .default = col_double(),
  `#` = col_integer(),
  GENuS_foodID = col_character(),
  GENuS_FoodName = col_character(),
  FCT_FoodName = col_character(),
  Folate = col_character(),
  `Dietary Fiber` = col_double(),
  Copper = col_double(),
  B6 = col_double(),
  Choline = col_double(),
  Magnesium = col_double(),
  Manganese = col_double(),
  `Omega-3 (USDA only)` = col_double(),
  `B12 (USDA only)` = col_double(),
  Refuse = col_character()
)))
dt.GENuS.LatAm[, `#` := NULL]
dt.GENuS.LatAm[, GENuS_region := "LatAm"]
# give LatAm the same column names as the other regions
setnames(dt.GENuS.LatAm, old = names(dt.GENuS.LatAm), new = names(dt.GENuS.ASEAN))
GENUS.FCT.list <- list(dt.GENuS.WAfrica, dt.GENuS.US, dt.GENuS.oldAfrica, dt.GENuS.NEASIA, dt.GENuS.LatAm, dt.GENuS.India, dt.GENuS.ASEAN)
GENuS.FCT <- rbindlist(GENUS.FCT.list)


commonNuts.genus <- c("Calories", "Protein", "Fat", "Carbohydrates", "Vitamin.C", "Vitamin.A", "Folate",
                      "Calcium", "Iron", "Zinc", "Potassium",
                      "Dietary.Fiber", "Phosphorus", "Thiamin", "Riboflavin", "Niacin", "B6", "B12..USDA.only.","Magnesium",
                      "Saturated.FA", "Monounsaturated.FA", "Polyunsaturated.FA")

commonNuts.IMPACT <- c("energy_kcal", "protein_g", "fat_g", "carbohydrate_g", "vit_c_mg", "vit_a_rae_µg", "folate_µg",
                       "calcium_mg", "zinc_mg", "iron_mg", "potassium_g",
                       "totalfiber_g",   "phosphorus_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg", "vit_b6_mg",  "vit_b12_µg", "magnesium_mg",
                       "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g")
setnames(GENuS.FCT, old = commonNuts.genus, new = commonNuts.IMPACT)

# unit differences - potassium is mg in genus, g in IMPACT
GENuS.FCT[, potassium_g := potassium_g * 1000]
oldOrder <- names(GENuS.FCT)
newOrder <- c("GENuS_region", oldOrder[!oldOrder %in% "GENuS_region"])
setcolorder(GENuS.FCT, newOrder)

temp <- unique(GENuS.FCT[, c("GENuS_foodID", "GENuS_FoodName"), with = FALSE])

library(readxl)
genusFoodNamesIMPACTlookup <- as.data.table(read_excel("genusFoodNamesIMPACTlookup.xlsx",
                                                       col_types = c("skip", "text", "text",
                                                                     "text", "skip", "skip", "skip",
                                                                     "skip", "skip")))

lookupTable <- merge(genusFoodNamesIMPACTlookup, temp, by.x = "GENUSName", by.y = "GENuS_FoodName")
GENuS.FCT <- merge(GENuS.FCT, lookupTable, by = "GENuS_foodID")
GENuS.FCT[, GENUSName := NULL]
GENuS.FCT[, food_id := paste(GENuS_foodID, GENuS_region, sep = "_")]
GENuS.FCT[, c("GENuS_foodID", "GENuS_region") := NULL]
colsToMove <- c("IMPACT_code", "compositecomponent", "food_id")
newOrder <- c(colsToMove, names(GENuS.FCT)[!names(GENuS.FCT) %in% colsToMove])
setcolorder(GENuS.FCT, newOrder)
# nutrients not in genus but in IMPACT
# "vit_e_mg", "vit_d_µg", "vit_k_µg",  "sugar_g", "ethanol_g", "caffeine_mg", "ft_acds_tot_trans_g", "cholesterol_mg", "retentioncode_aus", "RetnDesc",
# GENuS.FCT[, c("vit_e_mg", "vit_d_µg", "vit_k_µg",  "sugar_g", "ethanol_g", "caffeine_mg", "ft_acds_tot_trans_g", "cholesterol_mg") := NA]

# nutrients in genus but not in IMPACT
# "Copper", "Sodium", "Choline", "Manganese", "Omega.3..USDA.only.", "Water.moisture", "Ash", "Refuse"  in genus

GENuS.FCT[,c("Copper", "Sodium", "Choline", "Manganese", "Omega.3..USDA.only.", "Water.moisture", "Ash", "Refuse") := NULL]
setnames(GENuS.FCT, old = c("food_id", "FCT_FoodName"), new = c("usda_code", "Long_Desc"))




