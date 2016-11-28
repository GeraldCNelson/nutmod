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
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
#nutrientLU <- fileNameList("nutrientLU")
#foodGroupLU <- fileNameList("foodGroupLU")

# Data loading code for nutrientCalcs -------------------------------------
#nutrients.raw <- openxlsx::read.xlsx(nutrientLU, sheet = 1, colNames = TRUE)
nutrients.lookup <- getNewestVersion("dt.IMPACTnutrientlookup", fileloc("mData")) # update for new lookup file
nutrients.raw <- nutrients.lookup[3:nrow(nutrients.lookup),]
deleteListCol <- c("usda_code", "RetnDesc",
                   "retentioncode_aus", "Long_Desc", "Ref_Desc")
nutrients.raw[, (deleteListCol) := NULL]
numericCols <- names(nutrients.raw)[!names(nutrients.raw) %in% "IMPACT_code"]
nutrients.raw[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]

nutrientNames_Units <- nutrients.lookup[1:2]
keepListCol <- c("IMPACT_code",names(nutrients.raw)[grep('_cr', names(nutrients.raw))])
cookretn <- nutrients.raw[,keepListCol, with = FALSE]
cookretn.cols <- names(cookretn)[2:length(cookretn)]
for (col in cookretn.cols) cookretn[is.na(get(col)), (col) := 100]
for (col in cookretn.cols) {
  cookretn[get(col) == 0.0, (col) := 100]
}
inDT <- cookretn
outName <- "dt.cookingRet"
cleanup(inDT,outName,fileloc("mData"))

inDT <- data.table::as.data.table(nutrientNames_Units)
deleteListCol <- c("IMPACT_code")
inDT[, (deleteListCol) := NULL]

# need to add "kcals.ethanol", "kcals.fat", "kcals.carbohydrate","kcals.protein", "kcals.sugar"
tempDT <- data.table::data.table(
  kcals.ethanol      = c("Ethanol", "kcals"),
  kcals.fat          = c("Fat", "kcals"),
  kcals.carbohydrate = c("Carbohydrate, by difference", "kcals"),
  kcals.protein      = c("Protein", "kcals"),
  kcals.sugar        = c("Sugars, total", "kcals")
)
inDT <- cbind(inDT,tempDT)
outName <- "dt.nutrientNames_Units"
cleanup(inDT,outName,fileloc("mData"))

#get list of nutrients in the food nutrient lookup table
#create list of columns that are not nutrients; phytate_source is the phytates reference material
temp <-
  c("IMPACT_code", "composite_code", "edible_share",  "IMPACT_conversion", cookretn.cols, "phytate_source")

nutrients.list <- names(nutrients.raw)[!(names(nutrients.raw) %in% temp)]

#convert NAs to 100 (percent) for edible_share, IMPACT_conversion, and cooking retention
colsToConvert <- c("IMPACT_conversion", "edible_share")
for (col in colsToConvert) nutrients.raw[is.na(get(col)), (col) := 0]

#convert the NAs to 0  in the nutrients columns
for (col in nutrients.list) nutrients.raw[is.na(get(col)), (col) := 0]

# convert IMPACT consumption values to consumer nutrient intake ---
# reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
# reduce nutrient amount by conversion of all items to edible share

nutrients.raw[ , (nutrients.list) := lapply(.SD, `*`, IMPACT_conversion / 100), .SDcols = nutrients.list]

nutrients.raw[ , (nutrients.list) := lapply(.SD, `*`, IMPACT_conversion / 100), .SDcols = nutrients.list]
nutrients.raw[ , (nutrients.list) := lapply(.SD, `*`, edible_share / 100), .SDcols = nutrients.list]

#remove extraneous columns
deleteListCol <- c("edible_share", "IMPACT_conversion", cookretn.cols)
nutrients <- nutrients.raw[, !(names(nutrients.raw) %in% deleteListCol)]
nutrientNames_Units <- nutrientNames_Units[, !(names(nutrients.raw) %in% deleteListCol)]
# add food groups and staples codes to the nutrients table ---
dt.foodGroupLU <- getNewestVersion("dt.foodGroupsInfo")
# dt.foodGroupLU <- data.table::as.data.table(openxlsx::read.xlsx(
#   foodGroupLU, sheet = 1, startRow = 1,colNames = TRUE))
keepListCol <- c("IMPACT_code", "food_group_code","staple_code")
tmp <- dt.foodGroupLU[, keepListCol, with = FALSE]
dt.nutrients <- merge(nutrients.raw, tmp, by = "IMPACT_code")
#-----------------------
# code to import composite information from spreadsheets ------
# fctFiles <- c("comp_fct_beans_cbean wphytate.xlsx",
#               "comp_fct_mutton and goat_clamb wphytate.xlsx",
#               #             "comp_fct_other fruits_ctemf.xlsx", - because the comp_recalc file below is what should be used
#               "comp_fct_rape and mustard oil_crpol wphytate.xlsx")
#
# recalcFiles <- c(
#   "comp_recalc_c_Crust_crustaceans_FCT_EPC_012916 wphytate.xlsx",
#   "comp_recalc_c_FrshD_freshwater_Diadr_FCT_EPC_012916 wphytate.xlsx",
#   "comp_recalc_c_Mllsc_mollusks_FCT_EPC_012916 wphytate.xlsx",
#   "comp_recalc_c_ODmrsl_demersal_FCT_EPC_012916 wphytate.xlsx",
#   "comp_recalc_c_OPelag_Pelagic_FCT_EPC_012916 wphytate.xlsx",
#   "comp_recalc_cocer_cereals_FCT wphytate.xlsx",
#   "comp_recalc_copul_pulses__FCT wphytate.xlsx",
#   "comp_recalc_cothr_othcrops_treenuts_FCT wphytate.xlsx",
#   "comp_recalc_csubf_fruits_Subtrop wphytate.xlsx",
#   "comp_recalc_ctemf_fruits_Other_FCT wphytate.xlsx",
#   "comp_recalc_ctols_oilcrops_FCT_V2 wphytate.xlsx",
#   "comp_recalc_cvege_vegetables_FCT wphytate.xlsx")
#
# nutList <- names(dt.nutrients)
# # phytate_source is the phytate reference list
# removeList <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code", "phytate_source")
# nutList <- nutList[!nutList %in% removeList]
#
# # work comp_fct spreadsheets. They just have a single worksheet ----
# for (i in fctFiles) {
#   temp <- gsub(".xlsx", "", i)
#   temp <- gsub(" wphytate", "", temp)
#   commodName <- substr(temp, nchar(temp) - 4, nchar(temp))
#   filePath <- paste("data-raw/NutrientData/nutrientDetails/", i, sep = "")
#   dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL))
#   keepListCol <- c("edible_share",nutList)
#   dt.fct <- dt.fct[,keepListCol, with = FALSE]
#   dt.fct[, (nutList) := lapply(.SD, function(x)
#     x * dt.fct[['edible_share']]/100 ), .SDcols = nutList]
#   dt.fct[,edible_share := NULL]
#
#   #copied here to remind me to include the IMPACT conversion code
#   # nutrients.raw[, nutrients.list] <-
#   #   nutrients.raw[, nutrients.list] * nutrients.raw$IMPACT_conversion / 100
#   # nutrients.raw[, nutrients.list] <-
#   #   nutrients.raw[, nutrients.list] * nutrients.raw$edible_share / 100
#
#   # get mean of each column for this commodity
#   temp <- dt.fct[, lapply(.SD, mean, na.rm = TRUE)]
#   # get original row for this commodity
#   temp2 <- dt.nutrients[IMPACT_code %in% commodName,]
#   # keep only the descriptor columns for this commodity
#   keepListCol <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
#   temp2 <- temp2[,keepListCol, with = FALSE]
#   # combine the descriptor columns with the new results
#   temp <- cbind(temp,temp2)
#   # remove old row for this commodity
#   dt.nutrients <- dt.nutrients[!IMPACT_code %in% commodName,]
#   # add new row for this commodity
#   dt.nutrients <- rbind(dt.nutrients, temp)
# }
#
# # work comp_recalc spreadsheets. They have 3 worksheets - IMPACT commodity name (e.g. c_Crust),
# # FCT, and Weighted Composition
# # The first worksheet has world production values for each commodity to be aggregated to the IMPACT commodity
# # The FCT worksheet has the nutrient lookup info----
#
# for (i in recalcFiles) {
#   filePath <- paste(fileNameList("nutrientDataDetails"), i, sep = "/")
#   commodName <- openxlsx::getSheetNames(filePath)[1]
#   dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
#                                                           sheet = "FCT"))
#   dt.fct[,phytate_mg := as.numeric(phytate_mg)]
#   deleteListCol <- c("inedible_share", "phytate_source","proximates", "minerals", "vitamins", "lipids", "other")
#
#   dt.fct[,(deleteListCol) := NULL]
#   dt.fct[is.na(dt.fct)] <- 0
#   dt.commod <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
#                                                              sheet = commodName))
#
#   # delete first column of dt.commod which just has notes
#   dt.commod <- dt.commod[,1 := NULL]
#   keepListCol <- c("item_name", "usda_code", "include", "pcn_fdsupply_avg")
#   dt.commod <- dt.commod[,keepListCol, with = FALSE]
#   keepListCol <- c("item_name", "usda_code", "edible_share", nutList)
#   dt.fct <- dt.fct[, keepListCol, with = FALSE]
#   dt.joined <- merge(dt.commod,dt.fct, by = c("item_name","usda_code"))
#   dt.joined <- dt.joined[include == 1,]
#   dt.joined[is.na(dt.joined)] <- 0
#   # nutListtemp <- names(dt.joined)[!names(dt.joined) %in% c("item_name", "usda_code", "include", "pcn_fdsupply_avg", "edible_share" )]
#   # multiply each nutrient by its share of production (pcn_fdsupply_avg) and reduce by its edible share
#
#   dt.joined <- dt.joined[, (nutList) :=
#                            lapply(.SD, function(x) x * dt.joined[['pcn_fdsupply_avg']]
#                                   * dt.joined[['edible_share']] / 100 ),
#                          .SDcols = nutList]
#
#   # sum all the weighted shares
#   temp <- dt.joined[, lapply(.SD, sum, na.rm = TRUE), .SDcols = nutList]
#
#   # get original row for this commodity
#   temp2 <- dt.nutrients[IMPACT_code %in% commodName,]
#   # keep only the descriptor columns for this commodity
#   keepListCol <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
#   temp2 <- temp2[,keepListCol, with = FALSE]
#   # combine the descriptor columns with the new results
#   temp <- cbind(temp,temp2)
#   # remove old row for this commodity
#   dt.nutrients <- dt.nutrients[!IMPACT_code %in% commodName,]
#   # add new row for this commodity
#   dt.nutrients <- rbind(dt.nutrients, temp)
# }

# # special handling -----
# # c_OMarn is the average of c_OPelag and c_ODmrsl
# namesToAverage <- c("c_OPelag", "c_ODmrsl")
# dt.temp <- dt.nutrients[IMPACT_code %in% namesToAverage,lapply(.SD,mean),
#                         .SDcols = c(nutrients.list)]
# keepListCol <- c("IMPACT_code", "composite_code", "food_group_code", "staple_code")
# dt.temp2 <- dt.nutrients[IMPACT_code %in% "c_OMarn",keepListCol, with = FALSE]
# combine the descriptor columns with the new results
# temp <- cbind(dt.temp,dt.temp2)
# # remove old row for this commodity
# dt.nutrients <- dt.nutrients[!IMPACT_code %in% "c_OMarn",]
# # add new row for this commodity
# dt.nutrients <- rbind(dt.nutrients, temp)

# add kcals to the dt.nutrients table -----
# add five new columns - kcals.fat, kcals.protein, kcals.carbs, kcals.ethanol, kcals.sugar

# source of conversion http://www.convertunits.com/from/joules/to/calorie+[thermochemical]
# 1 kJ = 0.23900573614 thermochemical /food calorie (kCal)
# 1 Kcal = 4.184 kJ
# fat 37kJ/g - 8.8432122371 kCal/g; protein 17kJ/g - 4.0630975143 kCal/g; carbs 16kJ/g - 3.8240917782 kCal/g
#  beer - 4% ethanol, wine - 12% ethanol, spirits - 47% ethanol
fatKcals <- 8.8432122371
proteinKcals <- 4.0630975143
carbsKcals <- 3.8240917782
ethanolKcals <- 6.9
ethanol.beer <- .04
ethanol.wine <- .12
ethanol.spirits <- .47

# alcoholic beverages need to have ethanol energy content included
dt.nutrients[, kcals.fat := fat_g * fatKcals]
dt.nutrients[, kcals.protein := protein_g * proteinKcals]
dt.nutrients[, kcals.carbohydrate := carbohydrate_g * carbsKcals]
dt.nutrients[, kcals.sugar := sugar_g * carbsKcals]
# do alcoholic beverages separately
dt.nutrients[IMPACT_code == "c_beer", kcals.ethanol := ethanolKcals * ethanol.beer * 100] # beer
dt.nutrients[IMPACT_code == "c_wine", kcals.ethanol := ethanolKcals * ethanol.wine * 100] # wine
dt.nutrients[IMPACT_code == "c_spirits", kcals.ethanol := ethanolKcals * ethanol.spirits * 100] # spirits
dt.nutrients[is.na(kcals.ethanol), kcals.ethanol := 0]
data.table::setorder(dt.nutrients,IMPACT_code)

inDT <- dt.nutrients
outName <- "dt.nutrients"
cleanup(inDT,outName,fileloc("mData"))

# the next few lines were commented out but seem to be needed so I'll uncomment them.
foodGroupLU <- fileNameList("foodGroupLU")
dt.foodGroupsInfo <- data.table::as.data.table(openxlsx::read.xlsx(foodGroupLU, sheet = 1, startRow = 1, cols = 1:8, colNames = TRUE))
inDT <- dt.foodGroupsInfo
outName <- "dt.foodGroupsInfo"
cleanup(inDT,outName,fileloc("mData"))
