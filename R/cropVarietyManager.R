# not clear this is still needed
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data
#' @title Calculate final graph combinations for the nutrient modeling paper
#' @name cropVarietyManager.R
#' @include nutrientModFunctions.R
#' @include workBookFunctions.R
#' @include nutrientCalcFunctions.R
#' @description This code writes out pdfs of the graphs used in the final nutrient modeling paper.
#' The code grabs the individual graphs created in aggRun.R and places them in pdf file to be incorporated into the final word doc. There are 1 to 6
#' individual graphs per pdf. The layout is guided by the layoutMatrixx structure, where the last x is replaced by a number (1 to 6). The heightsx variables
#' control how tall the graph is in inches. The width is determined by the height variable and the relative width from the original file.

#Copyright (C) 2015-2017 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
source("R/nutrientModFunctions.R")

sourceFile <- "cropVarietyManager.R"
createScriptMetaData()

#' load in all the data from the USDA food composition tables
FOOD_DES <- getNewestVersion("FOOD_DES", fileloc("uData"))
NUT_DATA <- getNewestVersion("NUT_DATA", fileloc("uData"))
NUTR_DEF <- getNewestVersion("NUTR_DEF", fileloc("uData"))

# lists needed in final data file assembly
macroNutrients <- c(keyVariable("macronutrients"), "energy_kcal")
vitamins <- keyVariable("vitamins")
minerals <- keyVariable("minerals")
kcals <- keyVariable("energy") # includes "kcals.ft_acds_tot_sat_g"
addedSugar <- keyVariable("addedSugar")
fattyAcids <- keyVariable("fattyAcids")
other <- keyVariable("other")

#' load in a LU table that includes nutCode, which later becomes the variable name for the nutrient
dt.nutcodeLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/NutrientCodeLookup.xlsx"))
dt.nutcodeLU[, Nutr_No := as.character(Nutr_No)]
# Encoding(dt.nutcodeLU$unit) <- "unknown" - for some reason this adds a stray character
nutcodes <- sort(unique(dt.nutcodeLU$Nutr_No))

#' keep just info on the nutrients we're interested in
nut_data <-  NUT_DATA[Nutr_No %in% nutcodes, ]
nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
data.table::setnames(nutr_def, old = "Units", new = "unit")

{#' load in the phytate information
  dt.phytateLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/phytateSources.xlsx"))
  dt.phytateLU[,phytate_mg := as.numeric(phytate_mg)]
  dt.phytateLU <- dt.phytateLU[is.na(phytate_mg), phytate_mg := 0]
  deleteListCol <- c("checked", "valuefrom", "X9", "inedible_share.fromPhytate")
  dt.phytateLU[, (deleteListCol) := NULL]
  #dt.phytateLU[, c("edible_share.fromPhytateSource", "phytate_source", "inedible_share.fromPhytate") := NULL]
}

#' assemble the variety specific information
{
  #' these are all the food item codes from the USDA food composition tables for varieties of wheat, rice, and maize
  wheatCodes <- c("20076", "20071", "20072", "20073", "20074", "20075")
  #wheatNames <- c("durum", "hardRedSpring", "hardRedWinter", "softRedWinter", "hardWhite", "softWhite")
  riceCodes <- c("20036", "20040", "20054", "20446", "20450", "20452", "20444")
  maizeCodes <- c("20314", "20014", "20020")
  #' base wheat is 20072, hardRedWinter
  #' base rice is 20444, Rice, white, long-grain, regular, raw, unenriched
  #' base maize is currently 20020 Cornmeal, whole-grain, yellow; should this be changed?

  variety_data <- nut_data[usda_code %in% c(wheatCodes, riceCodes, maizeCodes),]
  # add IMPACT code for the crop to the variety specific information on nutrients
  variety_data[usda_code %in% wheatCodes, IMPACT_code := "cwhea"]
  variety_data[usda_code %in% riceCodes, IMPACT_code := "crice"]
  variety_data[usda_code %in% maizeCodes, IMPACT_code := "cmaiz"]
}
#' keep description info for just the food items for the varieties of the various crops
food_des <- FOOD_DES[usda_code %in% c(wheatCodes, riceCodes, maizeCodes),]
food_des[is.na(Refuse), Refuse := 100]

#' assemble the data
dt <- merge(nutr_def, variety_data, by = "Nutr_No") #nutr_def has Nutr_No, unit, and NutrDesc (eg "Protein")
dt <- merge(dt, food_des, by = "usda_code") #combine nutrient info with food descriptive info

#' IMPACT code, primary usda_code and IMPACT conversion value (the conversion from the FAOSTAT value to the edible portion)
dt.IMPACTcodeLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/IMPACTCodeLookup.xlsx"))
dt.IMPACTcodeLU <- dt.IMPACTcodeLU[usda_code %in% c(wheatCodes, riceCodes, maizeCodes),] # keep info on crops with varieties
dt.IMPACTcodeLU[is.na(IMPACT_conversion), IMPACT_conversion := 100]

dt <- merge(dt, dt.IMPACTcodeLU, by = c("usda_code","IMPACT_code"), all.x = TRUE)
dt <- dt[is.na(IMPACT_conversion), IMPACT_conversion := 100]

dt.phytateLU <- dt.phytateLU[usda_code %in% unique(dt$usda_code), ]
dt.variety <- merge(dt, dt.phytateLU, by = c("usda_code", "Long_Desc"), all.x = TRUE) #add phytate info
dt.variety <- dt.variety[is.na(edible_share.fromPhytateSource), edible_share.fromPhytateSource := 100]
# dt.variety <- merge(dt.variety, dt.retentionLU, by = "IMPACT_code", all.x = TRUE)
dt.variety <- merge(dt.variety, dt.nutcodeLU, by = c("NutrDesc", "Nutr_No", "unit"), all.x = TRUE)

#' cooking retention info setup -----
{#' dt.retentionLU used to look up IMPACT commodities and their retention code equivalent
  dt.retentionLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/retentionLookup.xlsx"))
  dt.retentionLU[, retentioncode_aus := as.character(retentioncode_aus)]
  #' dt.cookingRetn has retention codes and the effect on nutrient content of cooking the food indicated by the retention descriptions
  dt.cookingRetn <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/USDAcookingretn06.xlsx", colNames = FALSE))
  # get rid of numeric version of the date enter variable; last one, and "FdGrp_Cd - X2"
  dt.cookingRetn[, c("X2","X7") := NULL]
  data.table::setnames(dt.cookingRetn, old = names(dt.cookingRetn), new = c("Retn_Code",  "RetnDesc",
                                                                            "Nutr_No", "NutrDesc", "Retn_Factor"))
  dt.cookingRetn[, Nutr_No := as.character(Nutr_No)][, Retn_Code := as.character(Retn_Code)]
  # drop 5005, ALC BEV,.. because it has wierd nutrient descriptions
  dt.cookingRetn <- dt.cookingRetn[!Retn_Code == "5005",]
  # adjust folate in cooking retention so folate, food (432) code is changed to the code for folate, DFE (435)
  dt.cookingRetn[Nutr_No %in% "432", `:=`(
    Nutr_No = "435",  NutrDesc = "Folate, DFE")]
  # adjust  cooking retention code for Vitamin A, RE to 320
  dt.cookingRetn[Nutr_No %in% "392", `:=`(Nutr_No = "320",  NutrDesc = "Vitamin A, RAE")]
  dt.cookingRetn <- merge(dt.cookingRetn, dt.nutcodeLU, by = c("Nutr_No", "NutrDesc"))
  #' get Australian cooking retention info and pull out just vitamin e
  dt.cookingRetn.aus <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/AusData/NUTTAB 2010 - Retention Factor File.xlsx"))

  #' change names to align with those in dt.cookingRetn
  oldNames <- names(dt.cookingRetn.aus)
  newNames <- c("Retn_Code", "RetnDesc", "Tagname", "NutrDesc", "unit", "Retn_Factor")
  data.table::setnames(dt.cookingRetn.aus, old = oldNames, new = newNames)

  #' Australian retention info used to get just vitamin e cooking retention
  dt.cookingRetn.aus[, Retn_Code := as.character(Retn_Code)]
  dt.cookingRetn.aus <- dt.cookingRetn.aus[Tagname == "VITE",]
  dt.cookingRetn.aus[, Nutr_No := "323"][, nutCode := "vit_e_mg"] # add the nutrient number and nutcode for vitamin e
  dt.cookingRetn.aus[, Retn_Factor := Retn_Factor * 100] # convert to same units as dt.cookRetn
  dt.cookingRetn <- rbind(dt.cookingRetn, dt.cookingRetn.aus)
  #Encoding(dt.cookingRetn$nutCode) <- "unknown"

  #' now add _cr columns and convert cooking retention info  to wide
  dt.cookingRetn[, nutCode := paste0(nutCode,"_cr")]
  cols.cookingRet <- unique(dt.cookingRetn$nutCode) # list of cooking retention columnsformula.wide <- paste("RetnDesc + Retn_Code   ~ nutCode")
  formula.wide <- paste("RetnDesc + Retn_Code ~ nutCode")
  dt.cookingRetn.wide <- data.table::dcast(
    data = dt.cookingRetn,
    formula = formula.wide,
    value.var = "Retn_Factor")
  dt.cookingRetn.wide[is.na(dt.cookingRetn.wide)] <- 100
  dt.cookingRetn.wide <- dt.cookingRetn.wide[Retn_Code %in% dt.retentionLU$retentioncode_aus,]
}
# formula.wide <- paste("usda_code  + Long_Desc + IMPACT_conversion + Ref_Desc +
formula.wide <- paste("IMPACT_code  + Long_Desc + IMPACT_conversion + usda_code  +  Ref_Desc +
                        edible_share + phytate_mg + phytate_source  ~ nutCode")

dt.wide <- data.table::dcast(
  data = dt.variety,
  formula = formula.wide,
  value.var = "Nutr_Val")

dt.wide <- merge(dt.wide,dt.retentionLU, by = c("IMPACT_code"), all.x = TRUE)  #add retention code to dt.wide
dt.wide[, retfactor_desc := NULL]
#' change total fat in ctea to be the sum of the fat constituents
dt.wide["cteas", fat_g := ft_acds_mono_unsat_g + ft_acds_plyunst_g + ft_acds_tot_sat_g + ft_acds_tot_trans_g]
inDT <- merge(dt.wide, dt.cookingRetn.wide, by.x = c("retentioncode_aus"), by.y = c("Retn_Code"), all.x = TRUE)

oldOrder <- names(inDT)
#retnCols <- names(dt.cookingRetn.wide)[grep("_cr",names(dt.cookingRetn.wide))]
for (col in cols.cookingRet) inDT[is.na(get(col)), (col) := 100]
if (!"ft_acds_tot_trans_g" %in% oldOrder) inDT[, ft_acds_tot_trans_g := 0]
head <- c("IMPACT_code", "usda_code", "Long_Desc", "IMPACT_conversion", "Ref_Desc", "edible_share", "phytate_mg")
cookRetInfo <- c("retentioncode_aus", "RetnDesc" )
extran <- oldOrder[!oldOrder %in% c(head,         macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet)]
data.table::setcolorder(inDT,     c(head, extran, macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet))
NAlist <- names(inDT)[6:length(inDT)]
inDT[, (NAlist) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = NAlist]

outName <- "dt.nutVarieties_sr28"
desc <- "USDA varieties used for specific countries"
cleanup(inDT, outName, fileloc("mData"), "xlsx", desc = desc)

finalizeScriptMetadata(metadataDT, sourceFile)
