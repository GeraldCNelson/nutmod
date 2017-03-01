#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2016 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify i
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

#' @description Manipulate the results of the ODBC_access script and prepare data for dataPrep.nutrientData.R

#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}

# load data originally from the Access data base
FOOD_DES <- getNewestVersion("FOOD_DES", fileloc("mData"))
NUT_DATA <- getNewestVersion("NUT_DATA", fileloc("mData"))
NUTR_DEF <- getNewestVersion("NUTR_DEF", fileloc("mData"))

# now manipulate the data
# lists needed everywhere -----
composites <- c("cbean", "clamb", "cocer", "copul", "cothr", "crpol", "csubf", "ctemf",
                "ctols", "cvege", "c_Crust", "c_Mllsc", "c_FrshD", "c_OPelag", "c_ODmrsl", "c_OMarn")
macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g", "energy_kcal")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat_g", "kcals.protein_g", "kcals.sugar_g", "kcals.carbohydrate_g", "kcals.ethanol_g")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")
other <- c("caffeine_mg", "cholesterol_mg")

# lookup tables -----
#  IMPACT nutrient code - nutCode. Also has other info from NUTR_DEF for these nutrients
dt.nutcodeLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/NutrientCodeLookup.xlsx"))
dt.nutcodeLookup[, Nutr_No := as.character(Nutr_No)]
#Encoding(dt.nutcodeLookup$unit) <- "unknown" - for some reason this adds a stray character
nutcodes <- sort(unique(dt.nutcodeLookup$Nutr_No))

# keep just info on the nutrients we're interested in
nut_data <-  NUT_DATA[Nutr_No %in% nutcodes, ]
nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
data.table::setnames(nutr_def, old = "Units", new = "unit")
dt.composites_crop_lookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/composites.crop.lookup.xlsx", cols = 1:7))

# phytate information
dt.phytateLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/phytateSources.xlsx"))
dt.phytateLU[,phytate_mg := as.numeric(phytate_mg)]
dt.phytateLU <- dt.phytateLU[is.na(phytate_mg), phytate_mg := 0]
dt.phytateLU[, Long_Desc := NULL]
#dt.phytateLU[, c("edible_share.fromPhytateSource", "phytate_source", "inedible_share.fromPhytate") := NULL]

# IMPACT codes
dt.IMPACTcodeLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/IMPACTCodeLookup.xlsx"))
dt.IMPACTcodeLookup <- dt.IMPACTcodeLookup[is.na(IMPACT_conversion), IMPACT_conversion := 100]
dt.compositesLookup <- dt.IMPACTcodeLookup[IMPACT_code %in% composites,] # keep info on composite commodities
dt.singleCodeLookup <- dt.IMPACTcodeLookup[!IMPACT_code %in% composites,] # keep info on single commodities
dt.phytateLookup <- dt.phytateLU[usda_code %in% dt.IMPACTcodeLookup$usda_code, ]
dt.retentionLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/retentionLookup.xlsx"))
dt.retentionLookup[, retentioncode_aus := as.character(retentioncode_aus)]
dt.singleCodeLookup <- merge(dt.singleCodeLookup, dt.phytateLookup, by = "usda_code", all.x = TRUE) #add phytate info to single commodities
dt.singleCodeLookup <- dt.singleCodeLookup[is.na(edible_share.fromPhytateSource), edible_share.fromPhytateSource := 100]
dt.singleCodeLookup <- merge(dt.singleCodeLookup, dt.retentionLookup, by = "IMPACT_code", all.x = TRUE)

#cooking retention info setup -----
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
dt.cookingRetn[Nutr_No %in% "392", `:=`(
  Nutr_No = "320",  NutrDesc = "Vitamin A, RAE")]
dt.cookingRetn[Nutr_No %in% "392", Nutr_No := "320"]
dt.cookingRetn <- merge(dt.cookingRetn, dt.nutcodeLookup, by = c("Nutr_No", "NutrDesc"))

# get Australian cooking retention info and pull out just vitamin e
dt.cookingRetn.aus <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/AusData/NUTTAB 2010 - Retention Factor File.xlsx"))

# change names to align with those in dt.cookingRetn
oldNames <- names(dt.cookingRetn.aus)
newNames <- c("Retn_Code", "RetnDesc", "Tagname", "NutrDesc", "unit", "Retn_Factor")
data.table::setnames(dt.cookingRetn.aus, old = oldNames, new = newNames)
dt.cookingRetn.aus[, Retn_Code := as.character(Retn_Code)]
# get just vitamin e cooking retention
dt.cookingRetn.aus <- dt.cookingRetn.aus[Tagname == "VITE",]
dt.cookingRetn.aus[, Nutr_No := "323"][, nutCode := "vit_e_mg"] # add the nutrient number and nutcode for vitamin e
dt.cookingRetn.aus[, Retn_Factor := Retn_Factor * 100] # convert to same units as dt.cookRetn
dt.cookingRetn <- rbind(dt.cookingRetn, dt.cookingRetn.aus)
Encoding(dt.cookingRetn$nutCode) <- "unknown"

#now add _cr columns and convert to wide
dt.cookingRetn[, nutCode := paste0(nutCode,"_cr")]
cols.cookingRet <- unique(dt.cookingRetn$nutCode) # list of cooking retention columns

formula.wide <- paste("RetnDesc + Retn_Code   ~ nutCode")
dt.cookingRetn.wide <- data.table::dcast(
  data = dt.cookingRetn,
  formula = formula.wide,
  value.var = "Retn_Factor")
dt.cookingRetn.wide[is.na(dt.cookingRetn.wide)] <- 100
dt.cookingRetn.wide <- dt.cookingRetn.wide[Retn_Code %in% dt.retentionLookup$retentioncode_aus,]

# get just the single codes; work on composites below -----
USDAcodes <- dt.singleCodeLookup[,usda_code]

# sqlColumns(con, "NUTR_DEF")$COLUMN_NAME
# sqlColumns(con, "FD_GROUP")$COLUMN_NAME

nut_data <- NUT_DATA[usda_code %in% USDAcodes,]
#weight <- WEIGHT[usda_code %in% USDAcodes,]
food_des <- FOOD_DES[usda_code %in% USDAcodes,]
food_des[is.na(Refuse), Refuse := 100]

dt <- merge(nutr_def, nut_data, by = "Nutr_No") #combine nutrient codes and names
dt <- merge(dt, food_des, by = "usda_code") #combine nutrient info with food descriptive info
# note: this was added Jan 1, 2017 because of an error message that showed up after I added more items to the phytate info spreadsheet
#I added allow.cartesian = TRUE
dt <- merge(dt, dt.singleCodeLookup, by = c("usda_code"), all.x = TRUE) #combine IMPACT code info and phytate info
dt <- merge(dt, dt.nutcodeLookup, by = c("NutrDesc", "Nutr_No", "unit"), all.x = TRUE)
Encoding(dt$nutCode) <- "unknown"
data.table::setkey(dt, NULL)

keepListCol <- c("IMPACT_code", "usda_code", "Long_Desc", "IMPACT_conversion", "Ref_Desc",
                 "edible_share", "phytate_mg", "phytate_source", "nutCode", "Nutr_Val")
dt <- dt[, (keepListCol), with = FALSE]
dt <- unique(dt)
formula.wide <- paste("IMPACT_code + usda_code  + Long_Desc + IMPACT_conversion + Ref_Desc +
                      edible_share + phytate_mg + phytate_source  ~ nutCode")

dt.wide <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "Nutr_Val")

dt.wide <- merge(dt.wide,dt.retentionLookup, by = c("IMPACT_code"), all.x = TRUE)  #add retention code to dt.wide
dt.wide[, retfactor_desc := NULL]
# change total fat in ctea to be the sum of the fat constituents
dt.wide["cteas", fat_g := ft_acds_mono_unsat_g + ft_acds_plyunst_g + ft_acds_tot_sat_g + ft_acds_tot_trans_g]
# the requirement for potassium is expressed in grams; the Access data are in mg. We convert it here to g
dt.wide[ ,potassium_g := potassium_g/1000]

#tuna, shrimp, and salmon need to have edible share added
dt.wide <- dt.wide[IMPACT_code %in% "c_Tuna", edible_share := 73.5]
dt.wide <- dt.wide[IMPACT_code %in% "c_Shrimp", edible_share := 87]
dt.wide <- dt.wide[IMPACT_code %in% "c_Salmon", edible_share := 76.9]
inDT <- merge(dt.wide, dt.cookingRetn.wide, by.x = c("retentioncode_aus"), by.y = c("Retn_Code"), all.x = TRUE)
oldOrder <- names(inDT)
#retnCols <- names(dt.cookingRetn.wide)[grep("_cr",names(dt.cookingRetn.wide))]
for (col in cols.cookingRet) inDT[is.na(get(col)), (col) := 100]
if (!"ft_acds_tot_trans_g" %in% oldOrder) inDT[, ft_acds_tot_trans_g := 0]
head <- c("IMPACT_code", "usda_code", "Long_Desc", "IMPACT_conversion", "Ref_Desc", "edible_share", "phytate_mg")
cookRetInfo <- c("retentioncode_aus", "RetnDesc" )
extran <- oldOrder[!oldOrder %in% c(head,         macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet)]
data.table::setcolorder(inDT,     c(head, extran, macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet))
inDT[, 6:length(inDT)][is.na(inDT[, 6:length(inDT)])] <- 0
outName <- "dt.nutSingleCommodLookup_sr28"
cleanup(inDT, outName, fileloc("mData"), "xlsx")
# create dt to hold all the lookup values - dt.nutrients
dt.nutrients <- inDT
# done with single codes -----

# work on composites -----

# remove c_OMarn from the composites list. Later make it the average of c_OPelag and c_ODmrsl
dt.compositesLookup <- dt.compositesLookup[!IMPACT_code %in% "c_OMarn",]
for (i in 1:nrow(dt.compositesLookup)) {
  temp <- strsplit(as.character(dt.compositesLookup[i]),  split = ", ", fixed = TRUE)
  fName <- temp[[1]]
  USDAcodes <- temp[[2]]
  nut_data <- NUT_DATA[usda_code %in% USDAcodes,] # keep just the commodities for this composite
  nut_data <-  nut_data[Nutr_No %in% nutcodes, ]
  nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
  data.table::setnames(nutr_def, old = "Units", new = "unit")
  #weight <- WEIGHT[usda_code %in% USDAcodes,]
  food_des <- FOOD_DES[usda_code %in% USDAcodes,]

  dt <- merge(nutr_def, nut_data, by = "Nutr_No") #combine nutrient codes and names
  dt <- merge(dt, food_des, by = "usda_code") #combine nutrient info with food descriptive info
  dt.phytateLookup <- dt.phytateLookup[usda_code %in% USDAcodes, ]
  dt <- merge(dt, dt.phytateLU, by = "usda_code", all.x = TRUE) #add phytate info to commodities in the composite commodity
  dt[is.na(phytate_mg), phytate_mg := 0]
  #  dt <- merge(dt, dt.compositesLookup, by = c("usda_code"), all.x = TRUE) #combine IMPACT code info and phytate info
  dt <- merge(dt, dt.nutcodeLookup, by = c("NutrDesc", "Nutr_No", "unit"), all.x = TRUE)
  dt[, IMPACT_code := fName]
  Encoding(dt$nutCode) <- "unknown"
  data.table::setkey(dt, NULL)

  #  formula.wide <- paste(" usda_code  + Long_Desc + Ref_Desc +
  # edible_share + phytate_mg  ~ nutCode")
  formula.wide <- paste("IMPACT_code + usda_code  + Long_Desc +  Ref_Desc +
                        edible_share + phytate_mg + phytate_source  ~ nutCode")
  dt.wide <- data.table::dcast(
    data = dt,
    formula = formula.wide,
    value.var = "Nutr_Val")

  dt.wide <- merge(dt.wide, dt.retentionLookup, by = c("IMPACT_code"), all.x = TRUE)
  dt.wide[, retfactor_desc := NULL]

  # nutCode <-dt.nutcodeLookup$nutCode
  # dt.wide[paste0(nutCode,"_cr") == 0, paste(nutCode0,"_cr") := 100]
  # the requirement for potassium is expressed in grams; the Access data are in mg. We convert it here to g
  dt.wide[ ,potassium_g := potassium_g/1000]
  inDT <- merge(dt.wide, dt.cookingRetn.wide, by.x = c("retentioncode_aus"), by.y = c("Retn_Code"), all.x = TRUE)
  inDT[,IMPACT_code := fName][,IMPACT_conversion := dt.compositesLookup[i,IMPACT_conversion]]
  inDT[is.na(get(cols.cookingRet)), (cols.cookingRet) := 100]
  oldOrder <- names(inDT)
  if (!"ft_acds_tot_trans_g" %in% oldOrder) inDT[, ft_acds_tot_trans_g := 0]
  head <- c("IMPACT_code", "usda_code", "Long_Desc", "IMPACT_conversion", "Ref_Desc", "edible_share", "phytate_mg")
  cookRetInfo <- c("retentioncode_aus", "RetnDesc" )
  if (TRUE %in% (!fattyAcids %in% names(inDT))) inDT[,fattyAcids := 0]
  extran <- oldOrder[!oldOrder %in% c(head,     macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet)]
  data.table::setcolorder(inDT,     c(head, extran, macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet))
  inDT[, 6:length(inDT)][is.na(inDT[, 6:length(inDT)])] <- 0
  inDT[,IMPACT_code := fName]
  inDT <- inDT[is.na(IMPACT_conversion), IMPACT_conversion := 100] # to remind us to look into this later.
  outName <- paste0("dt.comp_",fName)
  cleanup(inDT, outName, fileloc("mData"), "xlsx")
  assign(paste0("dt.",dt.compositesLookup$IMPACT_code[i]), inDT)
}

# work on composites that use simple averages of nutrients
comps.simpleAverage <- c("cbean", "clamb",  "crpol")
dt.temp <- data.table::copy(inDT[FALSE,])
for (i in comps.simpleAverage) {
  fName <- paste0("dt.comp_", i)
  temp <- getNewestVersion(fName)
  colsToSum <- names(temp)[!names(temp) %in% c("IMPACT_code", "usda_code", "Long_Desc", "Ref_Desc",
                                               "retentioncode_aus", "RetnDesc", "phytate_source" )]

  temp.sum <- temp[, lapply(.SD, mean, na.rm = TRUE), .SDcols = colsToSum]
  dt.header <- data.table::data.table(IMPACT_code = i, usda_code = NA, Long_Desc = NA, Ref_Desc = NA,
                                      retentioncode_aus = NA, RetnDesc = NA, phytate_source = NA)
  finout <- cbind(dt.header, temp.sum)
  data.table::setcolorder(finout, names(temp))
  #  dt.temp <- rbind(dt.temp,finout)
  temp <- names(dt.nutrients)
  Encoding(temp) <- "unknown"
  data.table::setnames(dt.nutrients, old = names(dt.nutrients), new = temp)
  data.table::setnames(finout, old = names(finout), new = temp)
  dt.nutrients <- rbind(dt.nutrients, finout)
}

#crop composites with weighted averages -----
# prepare crop production data -----
dt.regions.all <- getNewestVersion("dt.regions.all")
keepListCol <- c("region_code.IMPACT159", "FAOSTAT_code")
dt.regions <- dt.regions.all[,(keepListCol), with = FALSE][, FAOSTAT_code := as.character(FAOSTAT_code)]

dt.prod.crops <- data.table::fread("data-raw/FAOSTAT/Production_Crops_E_All_Data.csv")
data.table::setnames(dt.prod.crops, old = names(dt.prod.crops),  new = tolower(names(dt.prod.crops)))
keepListYears.composites <- c("y2011", "y2012", "y2013")
keepListCol <- c("country code", "item code", "item", "element", keepListYears.composites)
dt.prod.crops <- dt.prod.crops[, (keepListCol), with = FALSE]
dt.prod.crops <- dt.prod.crops[element %in% c("Production"),]
dt.prod.crops[,(keepListYears.composites) := lapply(.SD, as.numeric), .SDcols = keepListYears.composites]
data.table::setnames(dt.prod.crops, old = c("country code", "item code"),
                     new = c("FAOSTAT_code", "item_code"))
dt.prod.crops <- merge(dt.regions, dt.prod.crops, by = c("FAOSTAT_code"))

dt.prod.crops.long <- data.table::melt(
  data = dt.prod.crops,
  id.vars = names(dt.prod.crops)[!names(dt.prod.crops) %in% keepListYears.composites],
  measure.vars = keepListYears.composites,
  variable.name = "year",
  value.name = "Production",
  variable.factor = FALSE
)
# aggregate to IMPACT159 regions
data.table::setkeyv(dt.prod.crops.long, c("region_code.IMPACT159","year", "item_code"))
dt.prod.crops.long[, prod.sum := sum(Production), by = eval(data.table::key(dt.prod.crops.long))]
dt.prod.crops.long[, Production := prod.sum][, c("FAOSTAT_code", "prod.sum", "element") := NULL]
dt.prod.crops.wide <- unique(dt.prod.crops.long)
dt.prod.crops.wide[, c("Production")][is.na(dt.prod.crops.wide[, c("Production")])] <- 0

# prepare animal production data ------
dt.prod.animals <- data.table::fread("data-raw/FAOSTAT/Production_LivestockPrimary_E_All_Data.csv")
data.table::setnames(dt.prod.animals, old = names(dt.prod.animals),  new = tolower(names(dt.prod.animals)))
keepListCol <- c("country code", "item code", "item", "element", "unit", keepListYears.composites)

dt.prod.animals <- dt.prod.animals[, (keepListCol), with = FALSE]
dt.prod.animals <- dt.prod.animals[element %in% c("Production") & unit %in% "tonnes",]
dt.prod.animals[,(keepListYears.composites) := lapply(.SD, as.numeric), .SDcols = keepListYears.composites]
data.table::setnames(dt.prod.animals, old = c("country code", "item code"),
                     new = c("FAOSTAT_code", "item_code"))
data.table::setnames(dt.prod.animals, old = names(dt.prod.animals)[!names(dt.prod.animals) %in% "FAOSTAT_code"], new = tolower(names(dt.prod.animals)[!names(dt.prod.animals) %in% "FAOSTAT_code"]))

dt.prod.animals <- merge(dt.regions, dt.prod.animals, by = c("FAOSTAT_code"))

dt.prod.animals.long <- data.table::melt(
  data = dt.prod.animals,
  id.vars = names(dt.prod.animals)[!names(dt.prod.animals) %in% keepListYears.composites],
  measure.vars = keepListYears.composites,
  variable.name = "year",
  value.name = "Production",
  variable.factor = FALSE
)

# aggregate to IMPACT159 regions
data.table::setkeyv(dt.prod.animals.long, c("region_code.IMPACT159","year", "item_code"))
dt.prod.animals.long[, prod.sum := sum(Production), by = eval(data.table::key(dt.prod.animals.long))]
dt.prod.animals.long[, Production := prod.sum][, c("FAOSTAT_code", "prod.sum", "element", "unit") := NULL]
dt.prod.animals.wide <- unique(dt.prod.animals.long)
dt.prod.animals.wide[, c("Production")][is.na(dt.prod.animals.wide[, c("Production")])] <- 0

# prepare trade data ------
dt.trade <- data.table::fread("~/Documents/workspace/nutmod/data-raw/FAOSTAT/Trade_Crops_Livestock_E_All_Data.csv")
data.table::setnames(dt.trade, old = names(dt.trade),  new = tolower(names(dt.trade)))
keepListCol <- c("country code", "item code", "item", "element", keepListYears.composites)

dt.trade <- dt.trade[, (keepListCol), with = FALSE]
dt.trade <- dt.trade[element %in% c("Export Quantity", "Import Quantity"),]
data.table::setnames(dt.trade, old = names(dt.trade)[!names(dt.trade) %in% "FAOSTAT_code"], new = tolower(names(dt.trade)))

dt.trade[,(keepListYears.composites) := lapply(.SD, as.numeric), .SDcols = keepListYears.composites]
data.table::setnames(dt.trade, old = c("country code", "item code"),
                     new = c("FAOSTAT_code", "item_code"))
dt.trade <- merge(dt.regions, dt.trade, by = c("FAOSTAT_code"))
dt.trade.long <- data.table::melt(
  data = dt.trade,
  id.vars = names(dt.trade)[!names(dt.trade) %in% keepListYears.composites],
  measure.vars = keepListYears.composites,
  variable.name = "year",
  value.name = "value",
  variable.factor = FALSE
)
dt.trade.long[element %in% "Import Quantity", element := "importQ"][element %in% "Export Quantity", element := "exportQ"]

# aggregate to IMPACT159 regions
data.table::setkeyv(dt.trade.long, c("region_code.IMPACT159","year", "item_code"))
dt.trade.long[element %in% "importQ", importQ := sum(value), by = eval(data.table::key(dt.trade.long))]
dt.trade.long[element %in% "exportQ", exportQ := sum(value), by = eval(data.table::key(dt.trade.long))]
dt.trade.long[, c("FAOSTAT_code", "element", "value") := NULL]
dt.trade.wide <- unique(dt.trade.long)
dt.trade.wide[, c("exportQ", "importQ")][is.na(dt.trade.wide[,c("exportQ", "importQ")])] <- 0
data.table::setkeyv(dt.trade.wide, c("region_code.IMPACT159","year", "item_code"))

#deal with countries that have both exports and imports
dt.trade.wide[,sum.importQ := sum(importQ), by = eval(data.table::key(dt.trade.long))]
dt.trade.wide[,sum.exportQ := sum(exportQ), by = eval(data.table::key(dt.trade.long))]
dt.trade.wide[, c("importQ", "exportQ") := NULL]
data.table::setnames(dt.trade.wide, old = c("sum.importQ", "sum.exportQ"), new = c("importQ", "exportQ"))
dt.trade.wide <- unique(dt.trade.wide)

dt.prod <- data.table::rbindlist(list(dt.prod.animals.wide, dt.prod.crops.wide))
dt.fb <- merge(dt.prod, dt.trade.wide, by = c("region_code.IMPACT159", "item_code" ,"item", "year"))
dt.fb[, foodAvail := Production + importQ - exportQ]
data.table::setkeyv(dt.fb, c( "region_code.IMPACT159", "item_code"))
dt.fb[, foodAvail := mean(foodAvail), by = eval(data.table::key(dt.fb))]
deleteListCol <- c("year", "Production", "importQ", "exportQ" )
dt.fb[, (deleteListCol) := NULL]
dt.fb <- unique(dt.fb)
dt.fb[, region_code.IMPACT159 := NULL]
dt.fb <- unique(dt.fb)

# work on crop composites -----
cropComposites <- c("cocer", "copul", "cothr",  "csubf", "ctemf", "ctols", "cvege")
for (i in cropComposites) {
  fName <- paste0("dt.comp_", i)
  dt.temp.fName <- getNewestVersion(fName)
  #  sort(dt.temp.fName$usda_code)
  print(paste(fName, nrow(dt.temp.fName), sep = ","))
  # include only items not scheduled to be removed (ie remove = 0)
  dt.lookup <- dt.composites_crop_lookup[composite %in% i & remove %in% 0, ]
  dt.lookup[, item_code := as.character(item_code)]
  deleteListCol <- c("include", "remove", "composite")
  dt.lookup[, (deleteListCol) := NULL]
  #  sort(dt.lookup$usda_code)
  dt.fb.temp <- merge(dt.fb, dt.lookup, by = "item_code")
  data.table::setkeyv(dt.fb.temp, c(  "item_code"))
  # dt.fb.temp[, foodAvail := sum(foodAvail), by = eval(data.table::key(dt.fb.temp))]
  dt.fb.temp[, foodAvailRatio := foodAvail/sum(foodAvail)]
  #  sort(dt.fb.temp$usda_code)
  dt.temp <- merge(dt.fb.temp, dt.temp.fName, by = "usda_code")
  sort(dt.temp$usda_code)
  deleteListCol <- c( "item_code", "item",  "Long_Desc", "foodAvail")
  dt.temp[,(deleteListCol) := NULL]
  colsNotToMultiply <- c("usda_code", "item", "foodAvailRatio",
                         "item_name",    "usda_desc",    "IMPACT_code", "phytate_source", "Ref_Desc", "RetnDesc", "retentioncode_aus")
  colsToMultiply <- names(dt.temp)[!names(dt.temp) %in% colsNotToMultiply]
  # multiply all the columns in colsToMultiply by foodAvailRatio and assign to the colsToMultiply columns
  #saveRDS(dt.temp, file = "dt.vege.rds")
  # dt.temp[, (colsToMultiply) := Map(`*`, mget(colsToMultiply), foodAvailRatio)]
  dt.temp[ , (colsToMultiply) := lapply(.SD, `*`, foodAvailRatio), .SDcols = colsToMultiply]
  #  headerInfo <- c("usda_code",     "IMPACT_code", "Ref_Desc", "RetnDesc", "retentioncode_aus")
  dt.header <- data.table::data.table(IMPACT_code = i,  usda_code = NA, Long_Desc = NA, Ref_Desc = NA,
                                      retentioncode_aus = NA, RetnDesc = NA, phytate_source = NA)
  dt.temp <- dt.temp[, lapply(.SD, sum, na.rm = TRUE), .SDcols = colsToMultiply]
  dt.temp <- cbind(dt.header, dt.temp)
  temp <- names(dt.nutrients)
  Encoding(temp) <- "unknown"
  #data.table::setnames(dt.nutrients, old = names(dt.nutrients), new = temp)
  data.table::setnames(finout, old = names(finout), new = temp)

  temp <- data.table::rbindlist(list(dt.nutrients, dt.temp), use.names = TRUE)
  dt.nutrients <- rbind(dt.nutrients, dt.temp)
}

# work on fish -----
fishfiles <- c("comp_recalc_c_Crust_crustaceans_FCT_EPC_012916 wphytate.xlsx",
               "comp_recalc_c_FrshD_freshwater_Diadr_FCT_EPC_012916 wphytate.xlsx",
               "comp_recalc_c_Mllsc_mollusks_FCT_EPC_012916 wphytate.xlsx",
               "comp_recalc_c_ODmrsl_demersal_FCT_EPC_012916 wphytate.xlsx",
               "comp_recalc_c_OPelag_Pelagic_FCT_EPC_012916 wphytate.xlsx"
)
for (i in fishfiles) {
  # get fish nutrient components
  filePath <- paste(fileloc("nutrientDataDetails"), i, sep = "/")
  commodName <- openxlsx::getSheetNames(filePath)[1]

  #get nutrient info
  fName <- paste0("dt.comp_", commodName)
  dt.temp.fName <- getNewestVersion(fName)
  USDAcodes <- dt.temp.fName$usda_code
  dt.fct <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
                                                          sheet = "FCT"))
  keepListCol <- c("item_name",  "usda_code", "edible_share")
  dt.edibleShare <- dt.fct[, (keepListCol), with = FALSE]
  dt.edibleShare[, usda_code := as.character(usda_code)]
  dt.edibleShare <- dt.edibleShare[usda_code %in% USDAcodes, ]
  # dt.fct[,phytate_mg := as.numeric(phytate_mg)]
  # deleteListCol <- c("inedible_share", "phytate_source","proximates", "minerals", "vitamins", "lipids", "other")
  #
  # dt.fct[,(deleteListCol) := NULL]
  # dt.fct[is.na(dt.fct)] <- 0
  dt.commod <- data.table::as.data.table(openxlsx::read.xlsx(filePath, colNames = TRUE, cols = NULL,
                                                             sheet = commodName))

  prodYears <- c("prod_qty_2011", "prod_qty_2012",  "prod_qty_2013")
  data.table::setkeyv(dt.commod, prodYears)
  for (j in 1:length(prodYears)) {
    dt.commod[get(prodYears[j]) %in% c("-", "...", "0 0"), prodYears[j] := "0" ]
  }

  dt.commod <- dt.commod[, (prodYears) := lapply(.SD, as.numeric), .SDcols = prodYears]
  dt.commod <- dt.commod[, c("usda_code", "include") := lapply(.SD, as.character), .SDcols = c("usda_code", "include")]
  # delete first column of dt.commod which just has notes
  dt.commod <- dt.commod[,1 := NULL]
  #no trade data so just keep production
  keepListCol <- c("item_name", "usda_code", "include", "prod_qty_2011", "prod_qty_2012", "prod_qty_2013")
  dt.commod <- dt.commod[,keepListCol, with = FALSE]
  dt.commod <- dt.commod[usda_code %in% USDAcodes & include == 1,]
  dt.commod[, foodAvail := (prod_qty_2011 + prod_qty_2012 + prod_qty_2013)/3]
  data.table::setkeyv(dt.commod, c("usda_code"))
  # dt.commod[, foodAvail := sum(foodAvail), by = c("include")]
  dt.commod[, foodAvailRatio := foodAvail/sum(foodAvail)]

  dt.temp <- merge(dt.commod, dt.temp.fName, by = "usda_code")

  # get rid of bad edible share values
  dt.temp[,edible_share := NULL]
  dt.temp <- merge(dt.temp, dt.edibleShare, by = c("item_name","usda_code"))
  sort(dt.temp$usda_code)
  deleteListCol <- c( "Long_Desc", "foodAvail", "prod_qty_2011", "prod_qty_2012", "prod_qty_2013", "include" )
  dt.temp[,(deleteListCol) := NULL]
  colsNotToMultiply <- c("usda_code", "item", "foodAvailRatio",
                         "item_name",    "usda_desc",    "IMPACT_code", "phytate_source", "Ref_Desc", "RetnDesc", "retentioncode_aus")

  colsToMultiply <- names(dt.temp)[!names(dt.temp) %in% colsNotToMultiply]
  # multiply all the columns in colsToMultiply by foodAvailRatio and assign to the colsToMultiply columns
  #saveRDS(dt.temp, file = "dt.vege.rds")
  # dt.temp[, (colsToMultiply) := Map(`*`, mget(colsToMultiply), foodAvailRatio)]
  dt.temp[ , (colsToMultiply) := lapply(.SD, `*`, foodAvailRatio), .SDcols = colsToMultiply]
  #  headerInfo <- c("usda_code", "item_name, "IMPACT_code", "Ref_Desc", "RetnDesc", "retentioncode_aus")
  dt.header <- data.table::data.table(IMPACT_code = commodName,  usda_code = NA, Long_Desc = NA, Ref_Desc = NA,
                                      retentioncode_aus = NA, RetnDesc = NA, phytate_source = NA)

  dt.temp <- dt.temp[, lapply(.SD, sum, na.rm = TRUE), .SDcols = colsToMultiply]
  dt.temp <- cbind(dt.header, dt.temp)
  temp <- names(dt.nutrients)
  Encoding(temp) <- "unknown"
  data.table::setnames(dt.nutrients, old = names(dt.nutrients), new = temp)
  data.table::setnames(finout, old = names(finout), new = temp)
  dt.nutrients <- rbind(dt.nutrients, dt.temp)
}

# special handling -----
# c_OMarn is the average of c_OPelag and c_ODmrsl
namesToAverage <- c("c_OPelag", "c_ODmrsl")
dt.temp <- dt.nutrients[IMPACT_code %in% namesToAverage,lapply(.SD,mean),
                        .SDcols = c(colsToMultiply)]
dt.header <- data.table::data.table(IMPACT_code = "c_OMarn",  usda_code = NA,
                                    Long_Desc = NA, Ref_Desc = NA,
                                    RetnDesc = NA, retentioncode_aus = NA, phytate_source = NA)
dt.temp <- cbind(dt.header, dt.temp)
temp <- names(dt.nutrients)
Encoding(temp) <- "unknown"
temp <- gsub("Â","", temp)
data.table::setnames(dt.nutrients, old = names(dt.nutrients), new = temp)
data.table::setnames(finout, old = names(finout), new = temp)
dt.nutrients <- rbind(dt.nutrients, dt.temp)
# add nutrient names and units
# need to add "kcals.ethanol", "kcals.fat_g", "kcals.carbohydrate","kcals.protein", "kcals.sugar"
dt.nutrientNames_Units <- openxlsx::read.xlsx("data-raw/NutrientData/nutrientNames_Units.xlsx", colNames = TRUE)

# the next few lines are just to ensure correct encoding for mu.
temp <- names(dt.nutrientNames_Units)
Encoding(temp) <- "unknown"
data.table::setnames(dt.nutrientNames_Units, old = names(dt.nutrientNames_Units), new = temp)
temp <- names(dt.nutrients)
Encoding(temp) <- "unknown"
data.table::setnames(dt.nutrients, old = names(dt.nutrients), new = temp)

# add kcals to the dt.nutrients table -----
# add five new columns to dt.nutrientNames_Units - kcals.fat_g, kcals.protein, kcals.carbs, kcals.ethanol, kcals.sugar
tempDT <- data.table::data.table(
  kcals.ethanol_g      = c("Ethanol", "kcals"),
  kcals.fat_g          = c("Fat", "kcals"),
  kcals.carbohydrate_g = c("Carbohydrate, by difference", "kcals"),
  kcals.protein_g      = c("Protein", "kcals"),
  kcals.sugar_g        = c("Sugars, total", "kcals")
)

dt.nutrientNames_Units <- cbind(dt.nutrientNames_Units, tempDT)
#write this out to an rds file
inDT <- dt.nutrientNames_Units
outName <- "dt.nutrientNames_Units"
cleanup(inDT, outName, fileloc("mData"))

# Conversion for future reference - The unit of energy is the kilojoule (kJ) or megajoule (1 MJ = 1,000 kJ)
# 4.18 kilojoules are equal to 1 kilocalorie. From https://www.nrv.gov.au/dietary-energy

# source of conversion http://www.convertunits.com/from/joules/to/calorie+[thermochemical]
# 1 kJ = 0.23900573614 thermochemical /food calorie (kCal)
# 1 Kcal = 4.184 kJ
# fat 37kJ/g - 8.8432122371 kCal/g; protein 17kJ/g - 4.0630975143 kCal/g; carbs 16kJ/g - 3.8240917782 kCal/g
#  beer - 4% ethanol, wine - 12% ethanol, spirits - 47% ethanol
kcals.fat_per_g <- 8.84
kcals.protein_per_g <- 4.06
kcals.carbs_per_g <- 3.82
kcals.ethanol_per_g <- 6.9
ethanol.share.beer <- .04
ethanol.share.wine <- .12
ethanol.share.spirits <- .47

dt.nutrients[, `:=`(
  kcals.fat_g = fat_g * kcals.fat_per_g,
  kcals.protein_g = protein_g * kcals.protein_per_g,
  kcals.carbohydrate_g = carbohydrate_g * kcals.carbs_per_g,
  kcals.sugar_g = sugar_g * kcals.carbs_per_g,
  kcals.ft_acds_tot_sat_g = ft_acds_tot_sat_g * kcals.fat_per_g
)]

# Total energy includes everything, from protein, fats, carbohydrates, fiber, ethyl alcohol, etc. all converted at some appropriate rate from gms to kcals
# Carbohydrates by difference doesn’t include alcohol.
#
# So here’s my current plan (Feb 27, 2017)
# Total energy is what I’ll use in the denominator of the various NBC calculations
# I’ll use
# -          8.84 to convert grams of saturated fat to kcals
# -          3.82 to convert grams of sugar to kcals
# -          6.9 to convert grams of alcohol to kcals
#
#
# I’ll initially use saturated fat, sugar, and alcohol as disqualifying nutrients.
# They will have the MRV values of
# -          sugar_g - 10 percent of dietary energy
# -          saturated fat <- 10 percent of dietary energy
# -          ethanol <- 20 * ethanol_per_g

# add kcals.ethanol_g column
dt.nutrients[, kcals.ethanol_g := 0]

# beer
dt.nutrients[IMPACT_code == "c_beer", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.beer * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

# wine
dt.nutrients[IMPACT_code == "c_wine", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.wine * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

# spirits
dt.nutrients[IMPACT_code == "c_spirits", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.spirits * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

dt.nutrients[is.na(kcals.ethanol_g), kcals.ethanol_g := 0]
dt.nutrients[phytate_source == 0, phytate_source := NA]
data.table::setorder(dt.nutrients,IMPACT_code)

# work on food group stuff to add to dt.nutrients
# create dt.foodGroupsInfo from the excel spreadsheet for use in other R scripts.
foodGroupLU <- fileNameList("foodGroupLU")
dt.foodGroupsInfo <- data.table::as.data.table(openxlsx::read.xlsx(foodGroupLU, sheet = 1, startRow = 1, cols = 1:8, colNames = TRUE))
inDT <- dt.foodGroupsInfo
outName <- "dt.foodGroupsInfo"
cleanup(inDT,outName,fileloc("mData"))
keepListCol <- c("IMPACT_code", "food_group_code","staple_code")
dt.foodGroupsInfo <- dt.foodGroupsInfo[, keepListCol, with = FALSE]
dt.nutrients <- merge(dt.nutrients, dt.foodGroupsInfo, by = "IMPACT_code")

inDT <- dt.nutrients
outName <- "dt.nutrients"
cleanup(inDT, outName, fileloc("mData"))

#create  xlsx file with the nutrientlookup data -----
nutSpreadsheet <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb = nutSpreadsheet, sheetName = "nutLookup")

# add main nutrient lookup table
openxlsx::writeData(
  wb = nutSpreadsheet,
  x = dt.nutrientNames_Units,
  sheet = "nutLookup",
  startRow = 1,
  startCol = 1,
  rowNames = FALSE,
  colNames = TRUE
)
openxlsx::writeData(
  wb = nutSpreadsheet,
  x = dt.nutrients,
  sheet = "nutLookup",
  startRow = 4,
  startCol = 1,
  rowNames = FALSE,
  colNames = TRUE
)
openxlsx::addStyle(
  wb = nutSpreadsheet, sheet = "nutLookup", style = numStyle, cols = 1:length(dt.nutrients),
  rows = 4:(length(dt.nutrients)),
  gridExpand = TRUE
)
#create worksheets for composites and add data
for (i in composites[!composites %in% "c_OMarn"]) {
  openxlsx::addWorksheet(wb = nutSpreadsheet, sheetName = i)
  dtName <- eval(parse(text = paste0("dt.", i)))

  openxlsx::writeData(
    wb = nutSpreadsheet,
    x = dtName,
    sheet = i,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = TRUE
  )
  openxlsx::addStyle(
    wb = nutSpreadsheet, sheet = i, style = numStyle, cols = 2:length(dtName),
    rows = 2:(length(dtName)),
    gridExpand = TRUE
  )
}

openxlsx::saveWorkbook(wb = nutSpreadsheet, file = paste0(fileloc("mData"),"/dt.nutrientsDetail.", Sys.Date(), ".xlsx"),
                       overwrite = TRUE)

