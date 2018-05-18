#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient LU
#'
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

#' @description Manipulate the results of the ODBC_access script and prepare nutrient data f
#' These include
#' - USDAnutrients - all the USDA FCT value for the USDA food items used in the analysis
#' - dt.cookinRetn - the cooking retention columns by IMPACT_code
#'
source("R/nutrientModFunctions.R")
source("R/workbookFunctions.R")
source("R/nutrientCalcFunctions.R")
sourceFile <- "dataPrepUSDANuts.R"
createScriptMetaData()

# load data created in the dataPrep.ODBCaccess.R script
FOOD_DES <- getNewestVersion("FOOD_DES", fileloc("mData"))
NUT_DATA <- getNewestVersion("NUT_DATA", fileloc("mData"))
NUTR_DEF <- getNewestVersion("NUTR_DEF", fileloc("mData"))

# Important note: USDA nutrient values are for 100 gm of edible portion of a food item

# load various lookup tables
dt.IMPACTcodeLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/IMPACTCodeLookup.xlsx"))
dt.singleCodeLU <- dt.IMPACTcodeLU[is.na(IMPACT_conversion), IMPACT_conversion := 100] # as of Feb 18, 2018, dt.IMPACTcodeLU is only single commodities and the

# USDA codes from the initial results. It includes conversion from FAOSTAT to retail product.
library(readxl)

# lots of warnings about getting NA when expecting numeric
# dt.compositesLU.fish <- data.table::as.data.table(read_excel("data-raw/NutrientData/nutrientDetails/composites.lookup.fish.xlsx",
#                                                              col_types = c("text", "text", "skip",
#                                                                            "numeric", "numeric", "text", "skip",
#                                                                            "text", "skip")))
dt.fishStatData <- getNewestVersion("dt.fishStatData", fileloc("iData"))
dt.compositesLU.fish <- copy(dt.fishStatData)
dt.compositesLU.fish[, c("prodAve", "region_code.IMPACT159") := NULL]
dt.compositesLU.nofish <- data.table::as.data.table(read_excel("data-raw/NutrientData/nutrientDetails/composites.lookup.nofish.xlsx",
                                                               col_types = c("text", "text", "skip",
                                                                             "numeric", "skip", "numeric", "text",
                                                                             "skip", "text", "skip")))
dt.compositesLU.nofish <- dt.compositesLU.nofish[include == 1,]
dt.compositesLU.nofish[, include := NULL]

# dt.compositesLU.nofish <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/composites.lookup.nofish.xlsx", cols = 1:7))
# the fish data include an extra column used to calculate the edible share. The data for conversion are from an FAO data set called Indicative factors for
# converting prodcut weight to live weight for a selection of major fishery commodities; the pdf is call FAO_ANNEX_I1_fish edible portions.pdf
# dt.compositesLU.fish <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/composites.lookup.fish.xlsx", cols = 1:8))
 dt.compositesLU <- rbind(dt.compositesLU.fish, dt.compositesLU.nofish)

# get usda codes from the variety specific spreadsheet
dt.countryCropVariety <- as.data.table(read_excel("data-raw/NutrientData/countryCropVariety.xlsx", na = "NA"))
dt <- dt.countryCropVariety[,c("IMPACT_code", "usda_code") := NULL]
dt <- dt[-1]
dt <-  unique(unlist(dt))
usdaCodes.var <- dt[!is.na(dt)]

#' phytate information
dt.phytateLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/phytateSourcesWOedibleshare.xlsx"))
dt.phytateLU[,phytate_mg := as.numeric(phytate_mg)]
dt.phytateLU <- dt.phytateLU[is.na(phytate_mg), phytate_mg := 0]
deleteListCol <- c("Long_Desc")
dt.phytateLU[, (deleteListCol) := NULL]

dt.nutcodeLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/NutrientCodeLookup.xlsx"))
dt.nutcodeLU[, Nutr_No := as.character(Nutr_No)]
nutcodes <- sort(unique(dt.nutcodeLU$Nutr_No))
nut_data <-  NUT_DATA[Nutr_No %in% nutcodes, ]
nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
data.table::setnames(nutr_def, old = "Units", new = "unit")

usdaCodes.single <- sort(unique(dt.singleCodeLU$usda_code))
usdaCodes.composites <- sort(unique(dt.compositesLU$usda_code))
usdaCodes <- unique(c(usdaCodes.single, usdaCodes.composites, usdaCodes.var))
# keep just the USDA codes from the single and composite commodity lists. Moved to after files are merged.March 16, 2018
#food_des <- FOOD_DES[usda_code %in% usdaCodes,]
#nut_data <- nut_data[usda_code %in% usdaCodes]
dt <- merge(nutr_def, nut_data, by = "Nutr_No") #combine nutrient codes and names
dt <- merge(dt, FOOD_DES, by = "usda_code") #combine nutrient info with food descriptive info
dt <- merge(dt, dt.phytateLU, by = "usda_code", all.x = TRUE) #add phytate info to single commodities

#dt <- merge(dt, dt.singleCodeLU, by = c("usda_code"), all.y = TRUE, allow.cartesian = TRUE) #combine IMPACT code info and phytate info
dt <- merge(dt, dt.nutcodeLU, by = c("NutrDesc", "Nutr_No", "unit"), all.x = TRUE)
dt <- dt[usda_code %in% usdaCodes,]

formula.wide <- paste("usda_code  + Long_Desc + Ref_Desc +
                      edible_share + phytate_mg + phytate_source  ~ nutCode")
dt.USDAnutrients <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "Nutr_Val")

#dt.wide[, edible_share := pmin(edible_share, edible_share.fromPhytateSource)] # the phytateSource value is more reliable but sticking with the edible share from the USDA source now.
# dt.wide[, edible_share.fromPhytateSource := NULL]
dt.USDAnutrients[is.na(phytate_mg), phytate_mg := 0]

#fix some of the missing values in the USDA fct with imputed values
library(readxl)
# Some food items didn't have all nutrients so we imputed them from other sources, described in sources.
# This meant that the correction for potassium was over written.
USDANutrientImputedValues <- as.data.table(read_excel("data-raw/NutrientData/nutrientDetails/USDANutrientImputedValues.xlsx"))
USDANutrientImputedValues[, notes := NULL]
# The next line of code corrects the problem with potassium
USDANutrientImputedValues[, potassium_g := potassium_g / 1000] # fixed April 5, 2018
usdaCodeUpdate <- unique(USDANutrientImputedValues$usda_code)
dt.USDAnutrients <- dt.USDAnutrients[!usda_code %in% usdaCodeUpdate]
dt.USDAnutrients <- rbind(dt.USDAnutrients, USDANutrientImputedValues)
NAlist <- names(dt.USDAnutrients)[!names(dt.USDAnutrients) %in% c("usda_code", "Long_Desc", "Ref_Desc", "edible_share")]
dt.USDAnutrients[, (NAlist) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = NAlist]
inDT <- dt.USDAnutrients
outName <- "dt.USDAnutrients"
desc <- "All the USDA FCT varieties that are potentially used for nutrient information for IMPACT food items"
cleanup(inDT, outName, fileloc("iData"), desc = desc)

# prepare fish composites data, production average over three years
dt.fishStatData <- getNewestVersion("dt.fishStatData", fileloc("iData"))
usda_codes.fish <- unique(dt.fishStatData$usda_code)
dt.USDAnutrients.fish <- dt.USDAnutrients[usda_code %in% usda_codes.fish,]
dt.USDAnutrients.fish[, c("edible_share") := NULL]
dt.USDAnutrients.fish <- merge(dt.USDAnutrients.fish, dt.fishStatData, by = "usda_code")
setnames(dt.USDAnutrients.fish, old = "prodAve", new = "foodAvail") # so it matches with the crops that can do prod + imp - exp
dt.USDAnutrients.fish[is.na(dt.USDAnutrients.fish)] <- 0
dt.USDAnutrients.fish[, ft_acds_tot_trans_g := as.numeric(ft_acds_tot_trans_g)]

# prepare other composites data, average over 3 years

#' FAOSTAT (crop and animal production and trade) data are (currently) used only for the composite categories, to get nutrient availability by weighting the nutrient composition of composite items
# get FAOSTAT code info
dt.regions.all <- getNewestVersion("dt.regions.all")
keepListCol <- c("region_code.IMPACT159", "FAOSTAT_code")
dt.regions <- dt.regions.all[,(keepListCol), with = FALSE][, FAOSTAT_code := as.character(FAOSTAT_code)]

# read in FAOSTAT data; fish data have a different format so are handled elsewhere
keepListYears.composites <- keyVariable("keepListYears.composites") # years used to average
keepListCol <- c("Country Code", "Item Code", "Item", "Element", "Unit", keepListYears.composites)
dt.FAOSTAT.prod.crops <- fread('unzip -cq data-raw/FAOSTAT/Production_Crops_E_All_Data.zip', select = keepListCol,
                               colClasses = c(`Country Code` = "character", `Item Code` = "character"))
dt.FAOSTAT.prod.animals <- fread('unzip -cq data-raw/FAOSTAT/Production_LivestockPrimary_E_All_Data.zip', select = keepListCol,
                                 colClasses = c(`Country Code` = "character", `Item Code` = "character" ))
dt.FAOSTAT.trade <- fread('unzip -cq data-raw/FAOSTAT/Trade_Crops_Livestock_E_All_Data.zip', select = keepListCol,
                          colClasses = c(`Country Code` = "character", `Item Code` = "character" ))
FAOSTATcombined <- rbindlist(list(dt.FAOSTAT.prod.crops, dt.FAOSTAT.prod.animals, dt.FAOSTAT.trade), use.names = TRUE)
data.table::setnames(FAOSTATcombined, old = c("Country Code", "Item Code", "Item", "Element", "Unit"),
                     new = c("FAOSTAT_code", "item_code", "item_name", "element", "unit"))
#data.table::setnames(FAOSTATcombined, old = names(FAOSTATcombined),  new = tolower(names(FAOSTATcombined)))
keepListCol <- c("FAOSTAT_code", "item_code", "item", "element", "unit", keepListYears.composites)
FAOSTATcombined[,(keepListYears.composites) := lapply(.SD, as.numeric), .SDcols = keepListYears.composites]
FAOSTATcombined <- FAOSTATcombined[element %in% c("Production", "Export Quantity", "Import Quantity") & unit %in% "tonnes",]
FAOSTATcombined <- merge(dt.regions, FAOSTATcombined, by = c("FAOSTAT_code"))
FAOSTATcombined[is.na(FAOSTATcombined)] <- 0
FAOSTATcombined[, itemSum := rowSums(.SD), .SDcols = keepListYears.composites] # sum data from all three years
deleteListCol <- c("unit", keepListYears.composites)
FAOSTATcombined[, (deleteListCol) := NULL]

#' aggregate smaller countries to their IMPACT159 regions
data.table::setkeyv(FAOSTATcombined, c("region_code.IMPACT159", "item_code", "element"))
FAOSTATcombined[, itemSumNew := sum(itemSum), by = eval(data.table::key(FAOSTATcombined))]
FAOSTATcombined <- unique(FAOSTATcombined)
FAOSTATcombined[element %in% "Import Quantity", element := "importQ"][element %in% "Export Quantity", element := "exportQ"]

formula.wide <- "FAOSTAT_code + region_code.IMPACT159 + item_code + item_name ~ element"
FAOSTATcombined.wide <- data.table::dcast(
  data = FAOSTATcombined,
  formula = formula.wide,
  value.var = "itemSum")

FAOSTATcombined.wide[is.na(FAOSTATcombined.wide)] <- 0
FAOSTATcombined.wide[, foodAvail := (Production + importQ - exportQ)/3] # average annual food availability for the 3 years. Summation done above.
deleteListCol <- c("Production", "importQ", "exportQ")
FAOSTATcombined.wide[, (deleteListCol) := NULL]
FAOSTATcombined.wide[foodAvail <0, foodAvail := 0]

# merge FAOSTAT with dt.compositesLU.nofish
composites.temp <- merge(FAOSTATcombined.wide, dt.compositesLU.nofish, by = c("item_code", "item_name"))
composites.temp[, c("FAOSTAT_code") := NULL]
compositesHolder <- merge(composites.temp, dt.USDAnutrients, by = c("usda_code", "edible_share"))

# clean up some missing values
compositesHolder[, ft_acds_tot_trans_g := as.numeric(ft_acds_tot_trans_g)]
NAlist <- c("ethanol_g", "caffeine_mg", "ft_acds_tot_trans_g")
compositesHolder[, (NAlist) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = NAlist]

# combine fish with other composites
dt.composites <- rbind(compositesHolder, dt.USDAnutrients.fish)

dt.composites[, c("item_code", "item_name") := NULL]
dt.composites[, foodAvailpDay := foodAvail * (edible_share/100)/keyVariable("DinY")] # reduce food avail quantity by the edible share ratio and divide by days in year to get average daily availability

# special handling for c_OMarn. The next line reads in the nutrient data for it
dt.compositesLU.fish.c_OMarn <- data.table::as.data.table(read_excel("data-raw/NutrientData/nutrientDetails/c_OMarnData.xlsx",
                                                                     col_types = c("text", "text", "text",
                                                                                   "numeric", "text", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "text", "text")))
# get the availability data for c_Omarn
c_OMarnData <- dt.fishStatData[composite %in% "c_Omarn"]
setnames(c_OMarnData, old = c("prodAve"), new = c("foodAvail"))
c_OMarnData[, c("edible_share", "usda_code", "composite") := NULL]
# c_OMarnData[, value := sum(prodAve), by = "region_code.IMPACT159"][, c("item_code", "item_name", "prodAve") := NULL]
# c_OMarnData <- unique(c_OMarnData)
dt.OMarnNutData <- cbind(c_OMarnData, dt.compositesLU.fish.c_OMarn)
dt.OMarnNutData[, foodAvailpDay := foodAvail * (edible_share/100)/keyVariable("DinY")] # reduce food avail quantity by the edible share ratio and divide by days in year to get average daily availability
keepListCol <- names(dt.composites)
dt.OMarnNutData[, setdiff(names(dt.OMarnNutData), keepListCol) := NULL]
dt.OMarnNutData[, composite := "c_Omarn"][, Long_Desc := "Other marine fish"][, phytate_mg := 0][, phytate_source := "None"]
dt.composites <- rbind(dt.composites, dt.OMarnNutData)

# get weighted share of nutrients for each composite
id.var <- c("usda_code", "region_code.IMPACT159", "composite",  "foodAvailpDay", "Long_Desc") #, "Ref_Desc", "phytate_source"
nuts <- c("phytate_mg", "caffeine_mg", "calcium_mg", "carbohydrate_g", "cholesterol_mg", "energy_kcal", "ethanol_g", "fat_g",
          "folate_µg", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g", "ft_acds_tot_trans_g",
          "iron_mg", "magnesium_mg", "niacin_mg", "phosphorus_mg", "potassium_g", "protein_g", "riboflavin_mg",
          "sugar_g", "thiamin_mg", "totalfiber_g", "vit_a_rae_µg", "vit_b12_µg", "vit_b6_mg", "vit_c_mg",
          "vit_d_µg", "vit_e_mg", "vit_k_µg", "zinc_mg")
edible_share <- "edible_share"

dt.composites.long <- data.table::melt(dt.composites,
                                       id.var = id.var,
                                       variable.name = "nutrient",
                                       measure.var = c(nuts, edible_share),
                                       value.name = "value",
                                       variable.factor = FALSE)
setnames(dt.composites.long, old = "composite", new = "IMPACT_code")

dt.composites.long.var <- copy(dt.composites.long)
dt.composites.long.wld <- copy(dt.composites.long)
dt.composites.long.var <- dt.composites.long.var[, value.weighted.var := weighted.mean(value, foodAvailpDay), by = c( "region_code.IMPACT159", "IMPACT_code", "nutrient")]
dt.composites.long.wld <- dt.composites.long.wld[, value.weighted.wld := weighted.mean(value, foodAvailpDay), by = c( "IMPACT_code", "nutrient")]
deleteListCol.var <- c( "usda_code", "foodAvailpDay", "Long_Desc",  "value")
deleteListCol.wld <- c( "region_code.IMPACT159", "usda_code", "foodAvailpDay", "Long_Desc",  "value")

dt.composites.long.var[, (deleteListCol.var) := NULL]
dt.composites.long.var[nutrient %in% "edible_share" & is.na(value.weighted.var), value.weighted.var := 100]
dt.composites.long.var[is.na(value.weighted.var), value.weighted.var := 0]
dt.composites.long.var <- unique(dt.composites.long.var)

dt.composites.long.wld[, (deleteListCol.wld) := NULL]
dt.composites.long.wld[is.na(edible_share), edible_share := 100]
dt.composites.long.wld[is.na(value.weighted.wld), value.weighted.wld := 0]
dt.composites.long.wld <- unique(dt.composites.long.wld)

formula.wide <- "region_code.IMPACT159 + IMPACT_code  ~ nutrient"
dt.composites.var.wide <- data.table::dcast(
  data = dt.composites.long.var,
  formula = formula.wide,
  value.var = "value.weighted.var")

formula.wide <- "IMPACT_code  ~ nutrient"
dt.composites.wld.wide <- data.table::dcast(
  data = dt.composites.long.wld,
  formula = formula.wide,
  value.var = "value.weighted.wld")

#  set up cooking retention information; organized by IMPACT code, not by USDA code
{
  dt.retentionLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/retentionLookup.xlsx"))
  dt.retentionLU[, retentioncode_aus := as.character(retentioncode_aus)]
  dt.cookingRetn <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/USDAcookingretn06.xlsx", colNames = FALSE))
  # get rid of numeric version of the date enter variable; last one, and "FdGrp_Cd - X2"
  dt.cookingRetn[, c("X2","X7") := NULL]
  data.table::setnames(dt.cookingRetn, old = names(dt.cookingRetn), new = c("Retn_Code",  "RetnDesc",
                                                                            "Nutr_No", "NutrDesc", "Retn_Factor"))
  dt.cookingRetn[, Nutr_No := as.character(Nutr_No)][, Retn_Code := as.character(Retn_Code)]

  #cooking retention info setup -----
  # drop 5005, ALC BEV,.. because it has wierd nutrient descriptions
  dt.cookingRetn <- dt.cookingRetn[!Retn_Code == "5005",]
  # adjust folate in cooking retention so folate, food (432) code is changed to the code for folate, DFE (435)
  dt.cookingRetn[Nutr_No %in% "432", `:=`(
    Nutr_No = "435",  NutrDesc = "Folate, DFE")]
  # adjust  cooking retention code for Vitamin A, RE to 320
  dt.cookingRetn[Nutr_No %in% "392", `:=`(
    Nutr_No = "320",  NutrDesc = "Vitamin A, RAE")]
  # dt.cookingRetn[Nutr_No %in% "392", Nutr_No := "320"]
  dt.cookingRetn <- merge(dt.cookingRetn, dt.nutcodeLU, by = c("Nutr_No", "NutrDesc"))

  #' get Australian cooking retention info and pull out just vitamin e
  {# dt.cookingRetn.aus <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/AusData/NUTTAB 2010 - Retention Factor File.xlsx"))
    library(readxl)
    dt.cookingRetn.aus <- data.table::as.data.table(read_excel("data-raw/NutrientData/AusData/NUTTAB 2010 - Retention Factor File.xlsx",
                                                               col_types = c("text", "text", "text",
                                                                             "text", "text", "numeric")))
    #' change names to align with those in dt.cookingRetn
    oldNames <- names(dt.cookingRetn.aus)
    newNames <- c("Retn_Code", "RetnDesc", "Tagname", "NutrDesc", "unit", "Retn_Factor")
    data.table::setnames(dt.cookingRetn.aus, old = oldNames, new = newNames)
    dt.cookingRetn.aus[, Retn_Code := as.character(Retn_Code)]

    #' get just vitamin e cooking retention
    dt.cookingRetn.aus <- dt.cookingRetn.aus[Tagname == "VITE",]
    dt.cookingRetn.aus[, Nutr_No := "323"][, nutCode := "vit_e_mg"] # add the nutrient number and nutcode for vitamin e
    dt.cookingRetn.aus[, Retn_Factor := Retn_Factor * 100] # convert to same units as dt.cookRetn
    dt.cookingRetn <- rbind(dt.cookingRetn, dt.cookingRetn.aus)
    Encoding(dt.cookingRetn$nutCode) <- "unknown"
    # Encoding(dt.cookingRetn$nutCode) <- "UTF-8"}
  }
  #' now add _cr columns and convert to wide
  dt.cookingRetn[, nutCode := paste0(nutCode,"_cr")]
  cols.cookingRet <- unique(dt.cookingRetn$nutCode) # list of cooking retention columns

  formula.wide <- paste("RetnDesc + Retn_Code ~ nutCode")
  dt.cookingRetn.wide <- data.table::dcast(
    data = dt.cookingRetn,
    formula = formula.wide,
    value.var = "Retn_Factor")
  dt.cookingRetn.wide[is.na(dt.cookingRetn.wide)] <- 100
  dt.cookingRetn.wide <- dt.cookingRetn.wide[Retn_Code %in% dt.retentionLU$retentioncode_aus,]
  dt.cookingRetn.wide <- merge(dt.retentionLU, dt.cookingRetn.wide, by.x = "retentioncode_aus", by.y = "Retn_Code")
  dt.cookingRetn.wide[, retfactor_desc := NULL]

  # add USDA_conversion to cooking retention. Both are based on IMPACT codes rather than USDA codes
  dt.IMPACT_conversion <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/IMPACT_conversion.xlsx"))
  dt.IMPACT_conversion[is.na(IMPACT_conversion), IMPACT_conversion := 100]
  dt.cookingRetn.wide <- merge(dt.IMPACT_conversion, dt.cookingRetn.wide,  by = "IMPACT_code", all.x = TRUE)
  deleteListCol <- c("retentioncode_aus", "RetnDesc" )
  dt.cookingRetn.wide[, (deleteListCol) := NULL]
  dt.cookingRetn.wide[, (names(dt.cookingRetn.wide)) := lapply(.SD, function(x){x[is.na(x)] <- 100; x}), .SDcols = names(dt.cookingRetn.wide)]
  inDT <- dt.cookingRetn.wide
  outName <- "dt.cookingRetn"
  desc <- "Cooking retention for selected nutrients for each food item"
  cleanup(inDT, outName, fileloc("iData"), desc = desc)
}

# add cooking retention to composites
# - wld
dt.cookingRetn.wide.composites <- dt.cookingRetn.wide[IMPACT_code %in% unique(dt.composites.wld.wide$IMPACT_code)]
dt.composites.wld <- merge(dt.composites.wld.wide, dt.cookingRetn.wide.composites, by = "IMPACT_code")
startCols <- c( "IMPACT_code", "IMPACT_conversion", "edible_share")
setcolorder(dt.composites.wld, c( startCols, nuts, cols.cookingRet))
inDT <- dt.composites.wld
inDT[is.na(inDT)] <- 0
outName <- "dt.composites.wld"
desc <- "Nutrient composition for composite food items, identical for all countries"
cleanup(inDT, outName, fileloc("iData"), desc = desc)

# - var (country specific VARieties)
dt.cookingRetn.var.composites <- dt.cookingRetn.wide[IMPACT_code %in% unique(dt.composites.var.wide$IMPACT_code)]
dt.composites.var <- merge(dt.composites.var.wide, dt.cookingRetn.wide.composites, by = "IMPACT_code")
startCols <- c( "region_code.IMPACT159","IMPACT_code", "IMPACT_conversion", "edible_share")
setcolorder(dt.composites.var, c( startCols, nuts, cols.cookingRet))
inDT <- dt.composites.var
inDT[is.na(inDT)] <- 0
outName <- "dt.composites.var"
desc <- "Nutrient composition for composite food items, country-specific information"
cleanup(inDT, outName, fileloc("iData"), desc = desc)

# kcal information to add to nutrient file -----
# Conversion for future reference - The unit of energy is the kilojoule (kJ) or megajoule (1 MJ = 1,000 kJ)
# 4.18 kilojoules are equal to 1 kilocalorie. From https://www.nrv.gov.au/dietary-energy

# source of conversion http://www.convertunits.com/from/joules/to/calorie+[thermochemical]
# 1 kJ = 0.23900573614 thermochemical /food calorie (kCal)
# 1 Kcal = 4.184 kJ
# fat 37kJ/g - 8.8432122371 kCal/g; protein 17kJ/g - 4.0630975143 kCal/g; carbs 16kJ/g - 3.8240917782 kCal/g
kcals.fat_per_g <- 8.84
kcals.protein_per_g <- 4.06
kcals.carbs_per_g <- 3.82
kcals.ethanol_per_g <- 6.9
# ethanol content from FAO FBS Handbook. Orginal numbers in kcals; divide by kcals.ethanol_per_g
# beer - 29 kcals per 100 gm
# wine - 68 kcals per 100 gm
# distilled alcohol 295 kcals per 100 gm.

ethanol.share.beer <- (29 / kcals.ethanol_per_g)/100
ethanol.share.wine <- (68 / kcals.ethanol_per_g)/100
ethanol.share.spirits <- (295 / kcals.ethanol_per_g)/100

# create dt.foodGroupsInfo from the excel spreadsheet for use below and in other R scripts.
foodGroupLU <- fileNameList("foodGroupLU")
dt.foodGroupsInfo <- data.table::as.data.table(openxlsx::read.xlsx(foodGroupLU, sheet = 1, startRow = 1, cols = 1:8, colNames = TRUE))
inDT <- dt.foodGroupsInfo
outName <- "dt.foodGroupsInfo"
desc <- "Lookup for food group and staple/nonstaple assignment of IMPACT commodities"
cleanup(inDT,outName,fileloc("mData"), desc = desc)
keepListCol <- c("IMPACT_code", "food_group_code","staple_code")
dt.foodGroupsInfo <- dt.foodGroupsInfo[, keepListCol, with = FALSE]

#create base single food item nutrient file
dt.nutrients.base <- dt.USDAnutrients[usda_code %in% unique(dt.singleCodeLU$usda_code),]
dt.nutrients.base <- merge(dt.nutrients.base, dt.singleCodeLU, by = "usda_code")
dt.cookingRetn.wide.baseVars <- dt.cookingRetn.wide[IMPACT_code %in% unique(dt.singleCodeLU$IMPACT_code)]
dt.nutrients.base <- merge(dt.nutrients.base, dt.cookingRetn.wide.baseVars, by = c("IMPACT_code", "IMPACT_conversion"))
deleteListCol <- c(  "usda_code", "Long_Desc", "Ref_Desc", "phytate_source")
dt.nutrients.base[, (deleteListCol) := NULL]
dt.nutrients.base <- rbind(dt.nutrients.base, dt.composites.wld)
startCols <- c( "IMPACT_code", "IMPACT_conversion", "edible_share")
setcolorder(dt.nutrients.base, c( startCols, nuts, cols.cookingRet))

# add composites to nutrients base
dt.nutrients.base <- rbind(dt.nutrients.base, dt.composites.wld)

# add kcals info
dt.nutrients.base[, `:=`(
  kcals.fat_g = fat_g * kcals.fat_per_g,
  kcals.protein_g = protein_g * kcals.protein_per_g,
  kcals.carbohydrate_g = carbohydrate_g * kcals.carbs_per_g,
  kcals.sugar_g = sugar_g * kcals.carbs_per_g,
  kcals.ft_acds_tot_sat_g = ft_acds_tot_sat_g * kcals.fat_per_g
)]
kcalsList <- c("kcals.fat_g", "kcals.protein_g", "kcals.carbohydrate_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g")
dt.nutrients.base[, (kcalsList) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = kcalsList]

# add kcals.ethanol_g column
dt.nutrients.base[, kcals.ethanol_g := 0]

# beer
dt.nutrients.base[IMPACT_code == "c_beer", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.beer * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

# wine
dt.nutrients.base[IMPACT_code == "c_wine", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.wine * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

# spirits
dt.nutrients.base[IMPACT_code == "c_spirits", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.spirits * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

dt.nutrients.base[is.na(kcals.ethanol_g), kcals.ethanol_g := 0]

# add staple and food group codes
dt.nutrients.base <- merge(dt.nutrients.base, dt.foodGroupsInfo, by = "IMPACT_code")
dt.nutrients.base <- unique(dt.nutrients.base)
inDT <- dt.nutrients.base
inDT[is.na(inDT)] <- 0
outName <- "dt.nutrients.base"
desc <- "Nutrient composition of IMPACT food items, identical for all countries"
cleanup(inDT, outName, fileloc("iData"), desc = desc)

# work on nutrients for country specific varieties
dt.countryCropVariety <- as.data.table(read_excel("data-raw/NutrientData/countryCropVariety.xlsx", na = "NA"))
ctyNames <- names(dt.countryCropVariety)[!names(dt.countryCropVariety) %in% c("IMPACT_code", "usda_code")]
dt.countryCropVariety[, (ctyNames) := lapply(.SD, function(x) ifelse(is.na(x), usda_code, x)), .SDcols = (ctyNames)]
dt.countryCropVariety <- dt.countryCropVariety[-1,] # remove country names
dt.countryCropVariety.long <- data.table::melt(
  data = dt.countryCropVariety,
  id.var = c("IMPACT_code", "usda_code"),
  measure.var = ctyNames ,
  variable.name = "region_code.IMPACT159",
  value.name = "usda_code.var",
  variable.factor = FALSE
)
dt.countryCropVariety.long[, usda_code := NULL]
setnames(dt.countryCropVariety.long, old = "usda_code.var", new = "usda_code")
dt.nutrients.var <- dt.USDAnutrients[usda_code %in% unique(dt.countryCropVariety.long$usda_code),]
dt.nutrients.var <- merge(dt.countryCropVariety.long, dt.nutrients.var, by = "usda_code")

dt.cookingRetn.wide.var <- dt.cookingRetn.wide[IMPACT_code %in% unique(dt.nutrients.var$IMPACT_code)]
dt.nutrients.var <- merge(dt.nutrients.var, dt.cookingRetn.wide.var, by = c("IMPACT_code"))
deleteListCol <- c(  "usda_code", "Long_Desc", "Ref_Desc", "phytate_source")
dt.nutrients.var[, (deleteListCol) := NULL]
dt.nutrients.var <- rbind(dt.nutrients.var, dt.composites.var)
startCols <- c("region_code.IMPACT159", "IMPACT_code", "IMPACT_conversion", "edible_share")
setcolorder(dt.nutrients.var, c( startCols, nuts, cols.cookingRet))

# add composites to nutrients base
dt.nutrients.var <- rbind(dt.nutrients.var, dt.composites.var)

#add kcals info
dt.nutrients.var[, `:=`(
  kcals.fat_g = fat_g * kcals.fat_per_g,
  kcals.protein_g = protein_g * kcals.protein_per_g,
  kcals.carbohydrate_g = carbohydrate_g * kcals.carbs_per_g,
  kcals.sugar_g = sugar_g * kcals.carbs_per_g,
  kcals.ft_acds_tot_sat_g = ft_acds_tot_sat_g * kcals.fat_per_g
)]
kcalsList <- c("kcals.fat_g", "kcals.protein_g", "kcals.carbohydrate_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g")
dt.nutrients.var[, (kcalsList) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = kcalsList]

# add kcals.ethanol_g column
dt.nutrients.var[, kcals.ethanol_g := 0]

# beer
dt.nutrients.var[IMPACT_code == "c_beer", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.beer * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

# wine
dt.nutrients.var[IMPACT_code == "c_wine", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.wine * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

# spirits
dt.nutrients.var[IMPACT_code == "c_spirits", `:=`(
  kcals.ethanol_g = kcals.ethanol_per_g * ethanol.share.spirits * 100,
  kcals.carbohydrate_g = kcals.carbohydrate_g + kcals.ethanol_g
)]

dt.nutrients.var[is.na(kcals.ethanol_g), kcals.ethanol_g := 0]

# add staple and food group codes
dt.nutrients.var <- merge(dt.nutrients.var, dt.foodGroupsInfo, by = "IMPACT_code")
dt.nutrients.var <- unique(dt.nutrients.var)
inDT <- dt.nutrients.var
inDT[is.na(inDT)] <- 0
outName <- "dt.nutrients.var"
desc <- "Nutrient composition of IMPACT food items, country-specific"
cleanup(inDT, outName, fileloc("iData"), desc = desc)

# now add supplementation. Add this to dt.nutrients.var
dt.fortValues <- getNewestVersion("dt.fortValues")
nutrientsList.fort <- unique(dt.fortValues$Nutrient)
dt.fortValues[, Nutrient := paste0(Nutrient,".fort")]
formula.wide <- "region_code.IMPACT159 + IMPACT_code ~ Nutrient"
dt.fortValues.wide <- data.table::dcast(
  data = dt.fortValues,
  formula = formula.wide,
  value.var = "value")

dt.fortValues.wide[is.na(dt.fortValues.wide)] <- 0
# for (j in names(dt.fortValues.wide)) set(dt.fortValues.wide,which(is.na(dt.fortValues.wide[[j]])),j,0)
dt.nutrients.varFort <- merge(dt.fortValues.wide, dt.nutrients.var, by = c("IMPACT_code", "region_code.IMPACT159"), all.y = TRUE)
dt.nutrients.varFort[is.na(dt.nutrients.varFort)] <- 0

#for (j in names(dt.nutrients.varFort)) set(dt.nutrients.varFort,which(is.na(dt.nutrients.varFort[[j]])),j,0)

dt.nutrients.varFort[, `:=`(
  calcium_mg = calcium_mg + calcium_mg.fort,
  iron_mg = iron_mg + iron_mg.fort,
  niacin_mg = niacin_mg + niacin_mg.fort,
  riboflavin_mg = riboflavin_mg + riboflavin_mg.fort,
  thiamin_mg = thiamin_mg + thiamin_mg.fort,
  vit_a_rae_µg = vit_a_rae_µg + vit_a_rae_µg.fort,
  vit_b12_µg = vit_b12_µg + vit_b12_µg.fort,
  vit_b6_mg = vit_b6_mg + vit_b6_mg.fort,
  vit_d_µg = vit_d_µg + vit_d_µg.fort,
  vit_e_mg = vit_e_mg + vit_e_mg.fort,
  zinc_mg = zinc_mg + zinc_mg.fort
)]
deleteListCol <- c("calcium_mg.fort", "folate_µg.fort", "iron_mg.fort", "niacin_mg.fort",
                   "riboflavin_mg.fort", "thiamin_mg.fort", "vit_a_rae_µg.fort", "vit_b12_µg.fort",
                   "vit_b6_mg.fort", "vit_d_µg.fort", "vit_e_mg.fort", "zinc_mg.fort")
dt.nutrients.varFort[, (deleteListCol) := NULL]
dt.nutrients.varFort <- unique(dt.nutrients.varFort)

inDT <- dt.nutrients.varFort
inDT[is.na(inDT)] <- 0
outName <- "dt.nutrients.varFort"
desc <- "Nutrient composition of IMPACT food items, country-specific with fortification"
cleanup(inDT, outName, fileloc("iData"), desc = desc)

# create dt.nutrientNames_Units for use elsewhere

dt.nutrientNames_Units <- openxlsx::read.xlsx("data-raw/NutrientData/nutrientNames_Units.xlsx", colNames = TRUE)

# the next few lines are just to ensure correct encoding for mu.
# Encoding(temp) <- "UTF-8" makes all the variable names with mu in it encoded as UTF-8 but leaves the rest as unknown, at least on a mac
temp <- names(dt.nutrientNames_Units)
# Encoding(temp) <- "unknown"
Encoding(temp) <- "UTF-8"
data.table::setnames(dt.nutrientNames_Units, old = names(dt.nutrientNames_Units), new = temp)

# add kcals to the dt.nutrients table -----
# add six new columns to dt.nutrientNames_Units - kcals.fat_g, kcals.protein, kcals.carbs, kcals.ethanol,
# kcals.sugar, kcals.ft_acds_tot_sat_g
compositesKcals <- data.table::data.table(
  kcals.ethanol_g      = c("Ethanol", "kcals"),
  kcals.fat_g          = c("Fat", "kcals"),
  kcals.carbohydrate_g = c("Carbohydrate, by difference", "kcals"),
  kcals.protein_g      = c("Protein", "kcals"),
  kcals.sugar_g        = c("Sugars, total", "kcals"),
  kcals.ft_acds_tot_sat_g        = c("Fatty acids, total saturated", "kcals")
)

dt.nutrientNames_Units <- cbind(dt.nutrientNames_Units, compositesKcals)
#write this out to an rds file
inDT <- dt.nutrientNames_Units
outName <- "dt.nutrientNames_Units"
desc <- "Nutrient names and units"
cleanup(inDT, outName, fileloc("mData"), desc = desc)

finalizeScriptMetadata(metadataDT, sourceFile)
