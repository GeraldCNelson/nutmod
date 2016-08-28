#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro -------------------------------------------------------------------
#' @description
#' This script reads in the nutrient lookup table (file name is held in nutrientLU) for
#' IMPACT commodities (USDA GFS IMPACT Vx).
#' It produces a data frame that has for 100 grams of each IMPACT commodity the amount of several nutrients
#' adjusted for bone in to boneless and edible portion. The choice of using a cooking retention value is in nutrientCalcs.R.
#' Contributors to the work include Brendan Power (for coding assistance), and
#' Joanne E. Arsenault, Malcom Reilly, Jessica Bogard, and Keith Lividini (for nutrition expertise)

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
nutrientLU <- fileNameList("nutrientLU")
foodGroupLU <- fileNameList("foodGroupLU")

# Data loading code for nutrientCalcs -------------------------------------
#' @param nutrients.raw - nutrient content of IMPACT 3 commodities, including fish and alcoholic beverages
LUcolNames <- c("name", "IMPACT_code", "usda_code", "USDA_code_desc", "composite_code", "AUS_code", "comment",
                "edible_share", "inedible_share", "IMPACT_conversion", "proximates", "water_g", "energy_kcal",
                "protein_g", "fat_g", "carbohydrate_g", "totalfiber_g", "sugar_g", "minerals", "calcium_mg",
                "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "sodium_g", "zinc_mg", "vitamins", "vit_c_mg",
                "thiamin_mg", "riboflavin_mg", "niacin_mg", "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
                "vit_e_mg", "vit_d_μg", "vit_k_µg", "lipids", "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g",
                "ft_acds_plyunst_g", "cholesterol_mg", "other", "caffeine_mg", "ft_acds_tot_trans_g", "retentioncode_aus",
                "RetentionDescription", "thiamin_mg_cr", "vit_b12_µg_cr", "riboflavin_mg_cr", "niacin_mg_cr", "vit_b6_mg_cr",
                "calcium_mg_cr", "iron_mg_cr", "folate_µg_cr", "potassium_g_cr", "magnesium_mg_cr", "sodium_g_cr",
                "phosphorus_mg_cr", "vit_a_rae_µg_cr", "vit_c_mg_cr", "vit_e_mg_cr", "zinc_mg_cr")
nutrients.raw <- openxlsx::read.xlsx(nutrientLU, sheet = 1, rows = 3:68,  colNames = TRUE)

#' @param nutrientNames_Units - units for the nutrients in IMPACT159 nutrient list
nutrientNames_Units <- openxlsx::read.xlsx(nutrientLU,sheet = 1,rows = 2:3, colNames = FALSE)
colnames(nutrientNames_Units) <- LUcolNames
#remove columns that are dividers, etc. This leaves only the IMPACT_code, edible share, IMPACT_conversion,
# the nutrient values, and the cooking retention values
deleteListCol <-
  c("name", "usda_code", "USDA_code_desc", "AUS_code", "comment", "water_g", "inedible_share", "proximates",
    "minerals", "vitamins", "lipids", "other", "RetentionCode", "RetentionDescription", "retentioncode_aus"
  )
#' @param nutrients.clean - IMPACT_code, edible share, IMPACT_conversion,  nutrient values, and cooking retention values
nutrients.clean <- nutrients.raw[, !(names(nutrients.raw) %in% deleteListCol)]
nutrientNames_Units <- nutrientNames_Units[, !(names(nutrientNames_Units) %in% deleteListCol)]
#' @param cookretn.cols - the list of column names that contain cooking retention values
keepListCol <-
  c("IMPACT_code",colnames(nutrients.clean[, grep('_cr', names(nutrients.clean))]))
cookretn <- nutrients.clean[,keepListCol]
cookretn.cols <- names(cookretn)[2:length(cookretn)]
cookretn[is.na(cookretn[keepListCol])] <- 1.0
inDT <- data.table::as.data.table(cookretn)
outName <- "dt.cookingRet"
cleanup(inDT,outName,fileloc("mData"))
#get list of nutrients in the food nutrient lookup table
#create list of columns that are not nutrients
temp <-
  c("IMPACT_code", "composite_code", "edible_share",  "IMPACT_conversion", cookretn.cols)

nutrients.list <-
  colnames(nutrients.clean[,!(names(nutrients.clean) %in% temp)])

#convert NAs to 100 (percent) for edible_share, IMPACT_conversion, and cooking retention
colsToConvert <-
  c("IMPACT_conversion", "edible_share")
nutrients.clean[colsToConvert][is.na(nutrients.clean[colsToConvert])] <- 100

#convert the NAs to 0  in the nutrients columns
nutrients.clean[, nutrients.list][is.na(nutrients.clean[, nutrients.list])] <- 0

# # change nutrient denominator unit from 100 gm to 1 kg;
# commented out to leave units that nutritionists are familiar with
# nutrients[,nutrients.list] <- nutrients[,nutrients.list] * 10

# convert IMPACT consumption values to consumer nutrient intake ---
# reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
# reduce nutrient amount by conversion of all items to edible share

nutrients.clean[, nutrients.list] <-
  nutrients.clean[, nutrients.list] * nutrients.clean$IMPACT_conversion / 100
nutrients.clean[, nutrients.list] <-
  nutrients.clean[, nutrients.list] * nutrients.clean$edible_share / 100

#remove extraneous columns
deleteListCol <- c("edible_share", "IMPACT_conversion", cookretn.cols)
nutrients <- nutrients.clean[, !(names(nutrients.clean) %in% deleteListCol)]

# add food groups and staples codes to the nutrients table ---
dt.foodGroupsInfo <- data.table::as.data.table(openxlsx::read.xlsx(
  foodGroupLU, sheet = 1, startRow = 1,colNames = TRUE))
keepListCol <- c("IMPACT_code", "food_group_code","staple_code")
tmp <- dt.foodGroupsInfo[, keepListCol, with = FALSE]
dt.nutrients <- data.table::as.data.table(merge(nutrients, tmp, by = "IMPACT_code"))
#-----------------------
# code to import composite information from spreadsheets ------
fctFiles <- c("comp_fct_beans_cbean.xlsx",
              "comp_fct_mutton and goat_clamb.xlsx",
 #             "comp_fct_other fruits_ctemf.xlsx", - because the comp_recalc file below is what should be used
              "comp_fct_rape and mustard oil_crpol.xlsx")

recalcFiles <- c(
  "comp_recalc_c_Crust_crustaceans_FCT_EPC_012916.xlsx",
  "comp_recalc_c_FrshD_freshwater_Diadr_FCT_EPC_012916.xlsx",
  "comp_recalc_c_Mllsc_mollusks_FCT_EPC_012916.xlsx",
  "comp_recalc_c_ODmrsl_demersal_FCT_EPC_012916.xlsx",
  "comp_recalc_cocer_cereals_FCT.xlsx",
  "comp_recalc_c_OPelag_Pelagic_FCT_EPC_012916.xlsx",
  "comp_recalc_copul_pulses__FCT.xlsx",
  "comp_recalc_cothr_othcrops_treenuts_FCT.xlsx",
  "comp_recalc_csubf_fruits_Subtrop.xlsx",
  "comp_recalc_ctemf_fruits_Other_FCT.xlsx",
  "comp_recalc_ctool_oilcrops_FCT_V2.xlsx",
  "comp_recalc_cvege_vegetables_FCT.xlsx")

nutList <- names(dt.nutrients)
removeList <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
nutList <- nutList[!nutList %in% removeList]

# work comp_recalc spreadsheets. ----
for (i in fctFiles) {
  temp <- gsub(".xlsx", "", i)
  commodName <- substr(temp, nchar(temp) - 4, nchar(temp))
  filePath <- paste("data-raw/NutrientData/nutrientDetails/", i, sep = "")
  dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL))
  keepListCol <- c("edible_share",nutList)
  dt.fct <- dt.fct[,keepListCol, with = FALSE]
   dt.fct[, (nutList) := lapply(.SD, function(x)
    x * dt.fct[['edible_share']]/100 ), .SDcols = nutList]
   dt.fct[,edible_share := NULL]

   #copied here to remind me to include the IMPACT conversion code
   # nutrients.clean[, nutrients.list] <-
   #   nutrients.clean[, nutrients.list] * nutrients.clean$IMPACT_conversion / 100
   # nutrients.clean[, nutrients.list] <-
   #   nutrients.clean[, nutrients.list] * nutrients.clean$edible_share / 100

  # get mean of each column for this commodity
  temp <- dt.fct[, lapply(.SD, mean, na.rm = TRUE)]
  # get original row for this commodity
  temp2 <- dt.nutrients[IMPACT_code %in% commodName,]
  # keep only the descriptor columns for this commodity
  keepListCol <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
  temp2 <- temp2[,keepListCol, with = FALSE]
  # combine the descriptor columns with the new results
  temp <- cbind(temp,temp2)
  # remove old row for this commodity
  dt.nutrients <- dt.nutrients[!IMPACT_code %in% commodName,]
  # add new row for this commodity
  dt.nutrients <- rbind(dt.nutrients, temp)
}

# work comp_fct spreadsheets. They just have a single worksheet ----
for (i in recalcFiles) {
  filePath <- paste("data-raw/NutrientData/nutrientDetails/", i, sep = "")
  commodName <- openxlsx::getSheetNames(filePath)[1]
  dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
                    sheet = "FCT"))

  dt.commod <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
                    sheet = commodName))

  # delete first column of dt.commod which just has notes
  dt.commod <- dt.commod[,1 := NULL]
  keepListCol <- c("item_name", "usda_code", "include", "pcn_fdsupply_avg")
  dt.commod <- dt.commod[,keepListCol, with = FALSE]
  dt.fct <- dt.fct[, c("item_name", "usda_code", "edible_share", nutList), with = FALSE]
  dt.joined <- merge(dt.commod,dt.fct, by = c("item_name","usda_code"))
  dt.joined <- dt.joined[include == 1,]
  dt.joined[is.na(dt.joined)] <- 0
 # nutListtemp <- names(dt.joined)[!names(dt.joined) %in% c("item_name", "usda_code", "include", "pcn_fdsupply_avg", "edible_share" )]
  # multiply each nutrient by its share of production (pcn_fdsupply_avg) and reduce by its edible share

  dt.joined <- dt.joined[, (nutList) := lapply(.SD,
               function(x) x * dt.joined[['pcn_fdsupply_avg']]  * dt.joined[['edible_share']] / 100 ),
               .SDcols = nutList]

  # sum all the weighted shares
  temp <- dt.joined[, lapply(.SD, sum, na.rm = TRUE), .SDcols = nutList]


  # get original row for this commodity
  temp2 <- dt.nutrients[IMPACT_code %in% commodName,]
  # keep only the descriptor columns for this commodity
  keepListCol <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
  temp2 <- temp2[,keepListCol, with = FALSE]
  # combine the descriptor columns with the new results
  temp <- cbind(temp,temp2)
  # remove old row for this commodity
  dt.nutrients <- dt.nutrients[!IMPACT_code %in% commodName,]
  # add new row for this commodity
  dt.nutrients <- rbind(dt.nutrients, temp)
}

# special handling -----
# c_OMarn is the average of c_OPelag and c_ODmrsl
namesToAverage <- c("c_OPelag", "c_ODmrsl")
dt.temp <- dt.nutrients[IMPACT_code %in% namesToAverage,lapply(.SD,mean),
                     .SDcols = c(nutrients.list)]
keepListCol <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
dt.temp2 <- dt.nutrients[IMPACT_code %in% "c_OMarn",keepListCol, with = FALSE]
# combine the descriptor columns with the new results
temp <- cbind(dt.temp,dt.temp2)
# remove old row for this commodity
dt.nutrients <- dt.nutrients[!IMPACT_code %in% "c_OMarn",]
# add new row for this commodity
dt.nutrients <- rbind(dt.nutrients, temp)
# add kcals to the dt.nutrients table -----
# add five new columns - kcals.fat, kcals.protein, kcals.carbs, kcals.ethanol, kcals.sugar

# source of conversion http://www.convertunits.com/from/joules/to/calorie+[thermochemical]
# 1 kJ = 0.23900573614 thermochemical /food calorie (kCal)
# 1 Kcal = 4.184 kJ
# fat 37kJ/g - 8.8432122371 kCal/g; protein 17kJ/g - 4.0630975143 kCal/g; carbs 16kJ/g - 3.8240917782 kCal/g
fatKcals <- 8.8432122371
proteinKcals <- 4.0630975143
carbsKcals <- 3.8240917782
ethanolKcals <- 6.9

# alcoholic beverages need to have ethanol energy content included
# assumptions
#  beer - 4% ethanol, wine - 12% ethanol, spirits - 47% ethanol
dt.nutrients[, kcals.fat := fat_g * fatKcals][, kcals.protein := protein_g * proteinKcals]
dt.nutrients[, kcals.protein := carbohydrate_g * carbsKcals][, kcals.sugar := sugar_g * carbsKcals]
# do alcoholic beverages separately
dt.nutrients[IMPACT_code == "c_beer", kcals.ethanol := ethanolKcals * .04 * 100] # beer
dt.nutrients[IMPACT_code == "c_wine", kcals.ethanol := ethanolKcals * .12 * 100] # wine
dt.nutrients[IMPACT_code == "c_spirits", kcals.ethanol := ethanolKcals * .47 * 100] # spirits
dt.nutrients[is.na(kcals.ethanol), kcals.ethanol := 0]
data.table::setorder(dt.nutrients,IMPACT_code)

inDT <- dt.nutrients
outName <- "dt.nutrients"
cleanup(inDT,outName,fileloc("mData"))

dt.foodGroupsInfo <- data.table::as.data.table(openxlsx::read.xlsx(foodGroupLU, sheet = 1, startRow = 1, cols = 1:8, colNames = TRUE))
inDT <- dt.foodGroupsInfo
outName <- "dt.foodGroupsInfo"
cleanup(inDT,outName,fileloc("mData"))
