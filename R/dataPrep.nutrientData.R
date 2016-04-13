#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro -------------------------------------------------------------------
#' @description
#' This script reads in the nutrient lookup table (file name is held in nutrientLU) for
#' IMPACT commodities (USDA GFS IMPACT Vx).
#' It produces a data frame that has for 100 grams of each IMPACT commodity the amount of several nutrients
#' adjusted for bone in to boneless, edible portion, and cooking retention.
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

#' @param nutrientNames_Units - units for the nutrients in IMPACT3 nutrient list
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
#multiply the amount of a nutrient in a food by the cooking retention value
# moved to nutrientCalcs.R
# for (i in 1:length(cookretn.cols)) {
#   nutrientName <-
#     substr(x = cookretn.cols[i], 1, nchar(cookretn.cols[i]) - 3)
#   nutColName <- paste ("nutrients.clean$", nutrientName, sep = "")
#   #  print(nutColName)
#   nutRetentColName <- paste ("nutrients.clean$", cookretn.cols[i], sep = "")
#   #  print(nutRetentColName)
#   temp <-
#     as.data.frame(eval(parse(text = nutRetentColName)) * eval(parse(text = nutColName)) /
#                     100)
#   colnames(temp) <- nutrientName
#   nutrients.clean[, nutrientName] <- temp
# }

#remove extraneous columns
colsToRemove <- c("edible_share", "IMPACT_conversion", cookretn.cols)
nutrients <- nutrients.clean[, !(names(nutrients.clean) %in% colsToRemove)]

# add food groups and staples codes to the nutrients table ---
foodGroupsInfo <- openxlsx::read.xlsx(
  foodGroupLU,
  sheet = 1,
  startRow = 1,
  cols = 1:5,
  colNames = TRUE
)
tmp <- foodGroupsInfo[, c("IMPACT_code", "food.group.code","staple.code")]
nutrients <- merge(nutrients, tmp, by = "IMPACT_code", all = TRUE)

inDT <- nutrients
outName <- "df.nutrients"
cleanup(inDT,outName,fileloc("mData"))
#nutrients.out <- iconv(nutrients, from = "UTF-8", to = "Windows-1252") #to deal with mu
