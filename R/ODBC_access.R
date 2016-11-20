library(RODBC)
library(data.table)
source("R/nutrientModFunctions.R")
# some notes on setting RODBC up
# 1.	Install homebrew. Grab the line of code at http://brew.sh/ and paste into a terminal prompt. Here’s what it looks like today
# /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# 2.	After 1. completes successfully, type
# brew install libiodbc
# 3.	After 2. completes successfully, download and install the actualtech driver.
# 4.	In Rstudio (and almost certainly also in R), type
# Install.packages(“RODBC”)
# 5.	This will do a few things and then ask whether you want to compile from source. Answer ‘y’. It will do some more c stuff and then exit successfully.
# 6. run the ODBC manager and set up the connection to the access data base file as a system DSN (user DSN is probably ok too)
# 7. run this script

#read Access data base -----
con <- odbcConnect("nutrientDataSR28",DBMSencoding = "Windows-1252") # this encoding is needed to get the mu character correct
tbls <- sqlTables(con, tableType = "TABLE")

# principal files
FOOD_DES <- as.data.table(sqlFetch(con, "FOOD_DES", as.is = TRUE )) #food description; code is NDB_No, contains Food Group code, FdGrp_Cd)
deleteListCol <- c("FdGrp_Cd","ComName", "ManufacName", "Survey", "Shrt_Desc", "SciName", "N_Factor", "Pro_Factor", "Fat_Factor", "CHO_Factor")
FOOD_DES[,(deleteListCol) := NULL]
FOOD_DES[,edible_share := 100 - Refuse][is.na(edible_share), edible_share := 100]
data.table::setnames(FOOD_DES, old = "NDB_No", new = "usda_code")

NUT_DATA <- as.data.table(sqlFetch(con, "NUT_DATA", as.is = TRUE)) #nutrient data file; code is Nutr_No; contains NDB_No
deleteListCol <- c("Num_Data_Pts", "Std_Error", "Src_Cd", "Deriv_Cd", "Ref_NDB_No",
                   "Add_Nutr_Mark", "Num_Studies", "Min", "Max", "DF", "Low_EB", "Up_EB", "Stat_Cmt", "AddMod_Date")
NUT_DATA[,(deleteListCol) := NULL]
data.table::setnames(NUT_DATA, old = "NDB_No", new = "usda_code")

#not used yet
# WEIGHT <- as.data.table(sqlFetch(con, "WEIGHT", as.is = TRUE)) #gram weight file of a serving; contains NDB_No
# deleteListCol <- c("Seq", "Amount", "Msre_Desc", "Num_Data_Pts", "Std_Dev")
# WEIGHT[,(deleteListCol) := NULL]
# data.table::setnames(WEIGHT, old = "NDB_No", new = "usda_code")
#
# FOOTNOTE <- as.data.table(sqlFetch(con, "FOOTNOTE"))

#support files
FD_GROUP <- as.data.table(sqlFetch(con, "FD_GROUP"))
NUTR_DEF <- as.data.table(sqlFetch(con, "NUTR_DEF", as.is = TRUE)) #nutrient definition
deleteListCol <- c("Tagname", "Num_Dec",  "SR_Order")
NUTR_DEF[,(deleteListCol) := NULL]
SRC_CD <- as.data.table(sqlFetch(con, "SRC_CD")) # source of data
odbcClose(con)

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
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")
other <- c("caffeine_mg", "cholesterol_mg")

# lookup tables -----
#  IMPACT nutrient code - nutCode. Also has other info from NUTR_DEF for these nutrients
dt.nutcodeLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/NutrientCodeLookup.xlsx"))
dt.nutcodeLookup[, Nutr_No := as.character(Nutr_No)]
nutcodes <- sort(unique(dt.nutcodeLookup$Nutr_No))

# phytate information
dt.phytateLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/phytateSources.xlsx"))

# IMPACT codes
dt.IMPACTcodeLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/IMPACTCodeLookup.xlsx"))
dt.IMPACTcodeLookup <- dt.IMPACTcodeLookup[is.na(IMPACT_conversion), IMPACT_conversion := 100]
dt.compositesLookup <- dt.IMPACTcodeLookup[IMPACT_code %in% composites,] # keep into on composite commodities
dt.singleCodeLookup <- dt.IMPACTcodeLookup[!IMPACT_code %in% composites,] # keep info on single commodities
phytateLookup <- dt.phytateLookup[usda_code %in% dt.IMPACTcodeLookup$usda_code, ]

dt.retentionLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/retentionLookup.xlsx"))
dt.retentionLookup[, retentioncode_aus := as.character(retentioncode_aus)]

dt.singleCodeLookup <- merge(dt.singleCodeLookup, phytateLookup, by = "usda_code", all.x = TRUE) #add phytate info to single commodities
dt.singleCodeLookup <- dt.singleCodeLookup[is.na(phytate_mg), phytate_mg := 0]
dt.singleCodeLookup <- merge(dt.singleCodeLookup, dt.retentionLookup, by = "usda_code", all.x = TRUE)

#cooking retention info setup -----
dt.cookingRetn <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/USDAcookingretn06.xlsx", colNames = FALSE))
# get rid of numeric version of the date enter variable; last one, and "FdGrp_Cd - X2"
dt.cookingRetn[, c("X2","X7") := NULL]
data.table::setnames(dt.cookingRetn, old = names(dt.cookingRetn), new = c("Retn_Code",  "RetnDesc",
                                                                          "Nutr_No", "NutrDesc", "Retn_Factor"))
dt.cookingRetn[, Nutr_No := as.character(Nutr_No)][, Retn_Code := as.character(Retn_Code)]
dt.cookingRetn <- merge(dt.cookingRetn, dt.nutcodeLookup, by = c("Nutr_No", "NutrDesc"))
dt.cookingRetn[, nutCode := paste0(nutCode,"_cr")]
cols.cookingRet <- unique(dt.cookingRetn$nutCode) # list of cooking retention columns
formula.wide <- paste("RetnDesc + Retn_Code   ~ nutCode")
dt.cookingRetn.wide <- data.table::dcast(
  data = dt.cookingRetn,
  formula = formula.wide,
  value.var = "Retn_Factor")
dt.cookingRetn.wide <- dt.cookingRetn.wide[Retn_Code %in% dt.retentionLookup$retentioncode_aus,]

# get just the single codes; work on composites below -----
USDAcodes <- dt.singleCodeLookup[,usda_code]

# sqlColumns(con, "NUTR_DEF")$COLUMN_NAME
# sqlColumns(con, "FD_GROUP")$COLUMN_NAME

nut_data <- NUT_DATA[usda_code %in% USDAcodes,]
nut_data <-  nut_data[Nutr_No %in% nutcodes, ]
nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
#weight <- WEIGHT[usda_code %in% USDAcodes,]
food_des <- FOOD_DES[usda_code %in% USDAcodes,]

dt <- merge(nutr_def, nut_data, by = "Nutr_No") #combine nutrient codes and names
dt <- merge(dt, food_des, by = "usda_code") #combine nutrient info with food descriptive info
dt <- merge(dt, dt.singleCodeLookup, by = c("usda_code"), all.x = TRUE) #combine IMPACT code info and phytate info
dt <- merge(dt, dt.nutcodeLookup, by = c("NutrDesc", "Nutr_No", "Units"), all.x = TRUE)
Encoding(dt$nutCode) <- "unknown"

formula.wide <- paste("IMPACT_code + usda_code  + Long_Desc + IMPACT_conversion + Ref_Desc + edible_share + phytate_mg  ~ nutCode")
dt.wide <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "Nutr_Val")

dt.wide <- merge(dt.wide,dt.retentionLookup, by = c("usda_code"), all.x = TRUE)  #add retention code to dt.wide

# the requirement for potassium is expressed in grams; the Access data are in mg. We convert it here to g
dt.wide[ ,potassium_g := potassium_g/1000]
inDT <- merge(dt.wide, dt.cookingRetn.wide, by.x = c("retentioncode_aus"), by.y = c("Retn_Code"), all.x = TRUE)
oldOrder <- names(inDT)
if (!"ft_acds_tot_trans_g" %in% oldOrder) inDT[, ft_acds_tot_trans_g := 0]
head <- c("IMPACT_code", "usda_code", "Long_Desc", "IMPACT_conversion", "Ref_Desc", "edible_share", "phytate_mg")
cookRetInfo <- c("retentioncode_aus", "RetnDesc" )
extran <- oldOrder[!oldOrder %in% c(head,     macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet)]
data.table::setcolorder(inDT,     c(head, extran, macroNutrients, minerals, vitamins, addedSugar, fattyAcids, other, cookRetInfo, cols.cookingRet))
inDT[, 6:length(inDT)][is.na(inDT[, 6:length(inDT)])] <- 0
outName <- "dt.nutSingleCommodLookup.sr28"
cleanup(inDT, outName, fileloc("mData"), "xlsx")
# done with single codes

# remove c_OMarn from the composites list. Later make it the average of c_OPelag and c_ODmrsl
dt.compositesLookup <- dt.compositesLookup[!IMPACT_code %in% "c_OMarn",]
for (i in 1:nrow(dt.compositesLookup)) {
  temp <- strsplit(as.character(dt.compositesLookup[i]),  split = ", ", fixed = TRUE)
  fName <- temp[[1]]
  USDAcodes <- temp[[2]]
  nut_data <- NUT_DATA[usda_code %in% USDAcodes,]
  nut_data <-  nut_data[Nutr_No %in% nutcodes, ]
  nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
  #weight <- WEIGHT[usda_code %in% USDAcodes,]
  food_des <- FOOD_DES[usda_code %in% USDAcodes,]

  dt <- merge(nutr_def, nut_data, by = "Nutr_No") #combine nutrient codes and names
  dt <- merge(dt, food_des, by = "usda_code") #combine nutrient info with food descriptive info
  dt <- merge(dt, dt.singleCodeLookup, by = c("usda_code"), all.x = TRUE) #combine IMPACT code info and phytate info
  dt <- merge(dt, dt.nutcodeLookup, by = c("NutrDesc", "Nutr_No", "Units"), all.x = TRUE)
  Encoding(dt$nutCode) <- "unknown"

  formula.wide <- paste("IMPACT_code + usda_code  + Long_Desc + IMPACT_conversion + Ref_Desc + edible_share + phytate_mg  ~ nutCode")
  dt.wide <- data.table::dcast(
    data = dt,
    formula = formula.wide,
    value.var = "Nutr_Val")

  dt.wide <- merge(dt.wide,dt.retentionLookup, by = c("usda_code"), all.x = TRUE)
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

  outName <- paste0("dt.comp.",fName)
  cleanup(inDT, outName, fileloc("mData"), "xlsx")
}
# work on composites
# work on composites that use simple averages of nutrients
comps.simpaverage <- c("cbean", "clamb",  "crpol")
dt.temp <- data.table::copy(inDT[FALSE,])
for (i in comps.simpaverage) {
  fName <- paste0("dt.comp.", i)
  temp <- getNewestVersion(fName)
  colsToSum <- names(temp)[!names(temp) %in% c("IMPACT_code", "usda_code", "Long_Desc", "Ref_Desc", "retentioncode_aus", "RetnDesc" )]

  temp[, lapply(.SD, mean, na.rm = TRUE), .SDcols = colsToSum]
  finout <- cbind(temp[1,c("IMPACT_code", "usda_code", "Long_Desc", "Ref_Desc", "retentioncode_aus", "RetnDesc" )], temp[, lapply(.SD, mean, na.rm = TRUE), .SDcols = colsToSum])
  data.table::setcolorder(finout, names(temp))
  dt.temp <- rbind(dt.temp,finout)
}

#composites with weighted averages
# prepare crop production data
dt.regions.all <- getNewestVersion("dt.regions.all")
keepListCol <- c("region_code.IMPACT159", "FAOSTAT_code")
dt.regions <- dt.regions.all[,(keepListCol), with = FALSE][, FAOSTAT_code := as.character(FAOSTAT_code)]

dt.prod.crops <- fread("data-raw/FAOSTAT/Production_Crops_E_All_Data.csv")
keepYearList <- c("Y2011", "Y2012", "Y2013")
keepListCol <- c("Country Code", "Country", "Item Code", "Item",  "Element Code", "Element", "Unit", keepYearList)
dt.prod.crops <- dt.prod.crops[, (keepListCol), with = FALSE]
dt.prod.crops <- dt.prod[Element %in% c("Production"),]
dt.prod.crops[,(keepYearList) := lapply(.SD, as.numeric), .SDcols = keepYearList]
dt.prod.crops <- merge( dt.regions, dt.prod, by.x = "FAOSTAT_code", by.y = "Country Code" )

dt.prod.animals <- fread("data-raw/FAOSTAT/Production_LivestockPrimary_E_All_Data.csv")
dt.prod.animals <- dt.prod.animals[, (keepListCol), with = FALSE]
dt.prod.animals <- dt.prod.animals[Element %in% c("Production") & Unit %in% "tonnes",]

dt.trade <- fread("~/Documents/workspace/nutmod/data-raw/FAOSTAT/Trade_Crops_Livestock_E_All_Data.csv")
dt.trade <- dt.trade[, (keepListCol), with = FALSE]
dt.trade <- dt.trade[Element %in% c("Export Quantity", "Import Quantity"),]


# LUcolNames <- c("name", "IMPACT_code", "usda_code", "USDA_code_desc", "composite_code", "AUS_code", "comment",
#                 "edible_share", "inedible_share", "IMPACT_conversion", "proximates", "pytate_mg", "phytate_source", "water_g", "energy_kcal",
#                 "protein_g", "fat_g", "carbohydrate_g", "totalfiber_g", "sugar_g", "minerals", "calcium_mg",
#                 "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg", "vitamins", "vit_c_mg",
#                 "thiamin_mg", "riboflavin_mg", "niacin_mg", "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
#                 "vit_e_mg", "vit_d_µg", "vit_k_µg", "lipids", "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g",
#                 "ft_acds_plyunst_g", "cholesterol_mg", "other", "caffeine_mg", "ft_acds_tot_trans_g", "retentioncode_aus",
#                 "RetentionDescription", "thiamin_mg_cr", "vit_b12_µg_cr", "riboflavin_mg_cr", "niacin_mg_cr", "vit_b6_mg_cr",
#                 "calcium_mg_cr", "iron_mg_cr", "folate_µg_cr", "potassium_g_cr", "magnesium_mg_cr",
#                 "phosphorus_mg_cr", "vit_a_rae_µg_cr", "vit_c_mg_cr", "vit_e_mg_cr", "zinc_mg_cr")
