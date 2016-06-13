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
if (!exists("getNewestVersion", mode = "function")) {source("R/nutrientModFunctions.R")}
nutrientLU <- fileNameList("nutrientLU")
foodGroupLU <- fileNameList("foodGroupLU")

# Data loading code for nutrientCalcs -------------------------------------
#' @param nutrients.raw - nutrient content of IMPACT 3 commodities, including fish and alcoholic beverages
nutrients.raw <- openxlsx::read.xlsx(nutrientLU, sheet = 1, rows = 3:68, cols = 1:63, colNames = TRUE)

#' @param nutrientNames_Units - units for the nutrients in IMPACT159 nutrient list
nutrientNames_Units <- openxlsx::read.xlsx(nutrientLU,sheet = 1,rows = 1:3,cols = 10:46,colNames = FALSE)

#remove columns that are dividers, etc. This leaves only the IMPACT_code, edible share, IMPACT_conversion,
# the nutrient values, and the cooking retention values
deleteListCol <-
  c(
    "name",
    "usda_code",
    "USDA_code_desc",
    "AUS_code",
    "comment",
    "water_g",
    "inedible_share",
    "proximates",
    "minerals",
    "vitamins",
    "lipids",
    "other",
    "RetentionCode",
    "RetentionDescription",
    "retentioncode_aus"
  )
#' @param nutrients.clean - IMPACT_code, edible share, IMPACT_conversion,  nutrient values, and cooking retention values
nutrients.clean <- nutrients.raw[, !(names(nutrients.raw) %in% deleteListCol)]

#' @param cookretn.cols - the list of column names that contain cooking retention values
keepListCol <-
  c("IMPACT_code",colnames(nutrients.clean[, grep('_cr', names(nutrients.clean))]))
cookretn <- nutrients.clean[,keepListCol]
cookretn.cols <- names(cookretn)[2:length(cookretn)]
cookretn[is.na(cookretn[keepListCol])] <- 1.0
inDT <- cookretn
outName <- "cookingRet"
cleanup(inDT,outName,fileloc("mData"))
#get list of nutrients in the food nutrient lookup table
#create list of columns that are not nutrients
temp <-
  c("IMPACT_code",
    "composite_code",
    "edible_share",
    "IMPACT_conversion",
    cookretn.cols)

nutrients.list <-
  colnames(nutrients.clean[,!(names(nutrients.clean) %in% temp)])

# macro <- c("energy", "protein", "fat", "carbohydrate", "fiber", "sugar")
# minerals <- c("calcium", "iron", "potassium", "sodium", "zinc")
# vitamins <- c("vit_c", "thiamin",	"riboflavin",	"niacin", "vit_b6",	"folate", "vit_b12",
#           "vit_a", 	"vit_e_RAE", "vit_d2_3")
# fattyAcids <-  c("ft_acds_tot_sat", "ft_acds_plyunst")

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
colsToRemove <- c("edible_share", "IMPACT_conversion", cookretn.cols)
nutrients <- nutrients.clean[, !(names(nutrients.clean) %in% colsToRemove)]

# add food groups, staples, and white starches codes to the nutrients table ---
foodGroupsInfo <- openxlsx::read.xlsx(
  foodGroupLU,
  sheet = 1,
  startRow = 1,
  cols = 1:6,
  colNames = TRUE
)
tmp <- foodGroupsInfo[, c("IMPACT_code", "food.group.code","staple.code", "white.starch.code")]
df.nutrients <- merge(nutrients, tmp, by = "IMPACT_code", all = TRUE)
#-----------------------
# code to import composite information from spreadsheets ------
fctFiles <- c("comp_fct_beans_cbean.xlsx",
              "comp_fct_mutton and goat_clamb.xlsx",
              "comp_fct_other fruits_ctemf.xlsx",
              "comp_fct_rape and mustard oil_crpol.xlsx")

recalcFiles <- c(
  "comp_recalc_c_Crust_crustaceans_FCT_EPC_012916.xlsx",
  "comp_recalc_c_FrshD_freshwater_Diadr_FCT_EPC_012916.xlsx",
  "comp_recalc_c_Mllsc_mollusks_FCT_EPC_012916.xlsx",
  "comp_recalc_c_ODmrsl_demersal_FCT_EPC_012916.xlsx",
  #  "comp_recalc_c_OMarn_marineFish_FCT_EPC_012916.xlsx", - not used
  "comp_recalc_cocer_cereals_FCT.xlsx",
  "comp_recalc_c_OPelag_Pelagic_FCT_EPC_012916.xlsx",
  "comp_recalc_copul_pulses__FCT.xlsx",
  "comp_recalc_cothr_othcrops_treenuts_FCT.xlsx",
  "comp_recalc_csubf_fruits_Subtrop.xlsx",
  "comp_recalc_ctemf_fruits_Other_FCT.xlsx",
  "comp_recalc_ctool_oilcrops_FCT.xlsx",
  "comp_recalc_cvege_vegetables_FCT.xlsx")

dt.nutrients <- data.table::as.data.table(df.nutrients)
nutList <- names(dt.nutrients)
removeList <- c("IMPACT_code", "composite_code", "food.group.code", "staple.code", "white.starch.code")
nutList <- nutList[!nutList %in% removeList]
for (i in fctFiles) {
  temp <- gsub(".xlsx", "", i)
  commodName <- substr(temp, nchar(temp) - 4, nchar(temp))
  filePath <- paste("data-raw/NutrientData/nutrientDetails/", i, sep = "")
  dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL))
  dt.fct <- dt.fct[-nrow(dt.fct),]
  dt.fct <- dt.fct[,nutList, with = FALSE]
  # get mean of each column for this commodity
  temp <- dt.fct[, lapply(.SD, mean, na.rm = TRUE)]
  # get original row for this commodity
  temp2 <- dt.nutrients[IMPACT_code %in% commodName,]
  # keep only the descriptor columns for this commodity
  keepListCol <- c("IMPACT_code", "composite_code", "food.group.code", "staple.code", "white.starch.code")
  temp2 <- temp2[,keepListCol, with = FALSE]
  # combine the descriptor columns with the new results
  temp <- cbind(temp,temp2)
  # remove old row for this commodity
  dt.nutrients <- dt.nutrients[!IMPACT_code %in% commodName,]
  # add new row for this commodity
  dt.nutrients <- rbind(dt.nutrients, temp)
}

for (i in recalcFiles) {
  filePath <- paste("data-raw/NutrientData/nutrientDetails/", i, sep = "")
  commodName <- openxlsx::getSheetNames(filePath)[1]
  dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL, sheet = "FCT"))

  dt.commod <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
                                                             sheet = 1))

  dt.commod <- dt.commod[,1 := NULL]
  #dt.commod <- dt.commod[!is.na(usda_code),]
  keepListCol <- c("item_name", "usda_code","include","pcn_fdsupply_avg")
  dt.commod <- dt.commod[,keepListCol, with = FALSE]
  dt.fct <- dt.fct[, c("item_name","usda_code", nutList), with = FALSE]
  dt.joined <- merge(dt.commod,dt.fct, by = c("item_name","usda_code"))
  temp <- dt.joined[include == 1,]
  # multiple each nutrient by its share of production (pcn_fdsupply_avg)
  dt.joined <- dt.joined[, (nutList) := lapply(.SD, function(x) x * dt.joined[['pcn_fdsupply_avg']] ),
                         .SDcols = nutList]
  # sum all the weighted shares
  temp <- dt.joined[, lapply(.SD, sum, na.rm = TRUE), .SDcols = nutList]

  # get original row for this commodity
  temp2 <- dt.nutrients[IMPACT_code %in% commodName,]
  # keep only the descriptor columns for this commodity
  keepListCol <- c("IMPACT_code", "composite_code", "food.group.code", "staple.code", "white.starch.code")
  temp2 <- temp2[,keepListCol, with = FALSE]
  # combine the descriptor columns with the new results
  temp <- cbind(temp,temp2)
  # remove old row for this commodity
  dt.nutrients <- dt.nutrients[!IMPACT_code %in% commodName,]
  # add new row for this commodity
  dt.nutrients <- rbind(dt.nutrients, temp)
}



#--------------------
inDT <- dt.nutrients
outName <- "dt.nutrients"
cleanup(inDT,outName,fileloc("mData"))
#nutrients.out <- iconv(nutrients, from = "UTF-8", to = "Windows-1252") #to deal with mu
