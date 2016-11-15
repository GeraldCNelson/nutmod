library(RODBC)
library(data.table)
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
con <- odbcConnect("nutrientDataSR28", DBMSencoding = "")
tbls <- sqlTables(con, tableType = "TABLE")

# principal files
FOOD_DES <- as.data.table(sqlFetch(con, "FOOD_DES", as.is = TRUE )) #food description; code is NDB_No, contains Food Group code, FdGrp_Cd)
NUT_DATA <- as.data.table(sqlFetch(con, "NUT_DATA", as.is = TRUE)) #nutrient data file; code is Nutr_No; contains NDB_No
WEIGHT <- as.data.table(sqlFetch(con, "WEIGHT", as.is = TRUE)) #gram weight file of a serving; contains NDB_No
FOOTNOTE <- as.data.table(sqlFetch(con, "FOOTNOTE"))

#support files
FD_GROUP <- as.data.table(sqlFetch(con, "FD_GROUP"))
NUTR_DEF <- as.data.table(sqlFetch(con, "NUTR_DEF", as.is = TRUE)) #nutrient definition
SRC_CD <- as.data.table(sqlFetch(con, "SRC_CD")) # source of data

# as of Nov 10, 2016

USDAcodes <- c("14003", "14532", "14084", "09040", "20004", "13047", "11134", "05123", "16056", "19165",
 "14214", "20020", "16060", "15149", "01123", "4594", "15076", "15123", "16069", "01078",
 "20031", "04055", "04042", "04044", "04506", "16087", "16101", "09277", "10219", "11352",
 "20444", "12036", "20067", "16108", "19335", "11507", "11518", "04670", "04513", "20076",
 "11601", "35011", "11445")

# as of Nov 10, 2016
nutcodes <- c("203", "204", "205", "208", "262", "269", "291", "301", "303", "304", "305", "306",
              "309", "320", "323", "328", "401", "404", "405", "406",
              "415", "418", "430", "435", "601", "605", "606", "645", "646")
dt.nutcodeLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/NutrientCodeLookup.xlsx"))
dt.nutcodeLookup[, Nutr_No := as.character(Nutr_No)]

dt.IMPACTcodeLookup <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/IMPACTCodeLookup.xlsx"))

# sqlColumns(con, "NUTR_DEF")$COLUMN_NAME
# sqlColumns(con, "FD_GROUP")$COLUMN_NAME

nut_data <- NUT_DATA[NDB_No %in% USDAcodes,]
deleteListCol <- c("Num_Data_Pts", "Std_Error", "Src_Cd", "Deriv_Cd", "Ref_NDB_No",
                   "Add_Nutr_Mark", "Num_Studies", "Min", "Max", "DF", "Low_EB", "Up_EB", "Stat_Cmt", "AddMod_Date")
nut_data[,(deleteListCol) := NULL]
nut_data <-  nut_data[Nutr_No %in% nutcodes, ]

nutr_def <-  NUTR_DEF[Nutr_No %in% nutcodes, ]
deleteListCol <- c("Tagname", "Num_Dec",  "SR_Order")
nutr_def[,(deleteListCol) := NULL]

weight <- WEIGHT[NDB_No %in% USDAcodes,]
deleteListCol <- c("Seq", "Amount", "Msre_Desc", "Num_Data_Pts", "Std_Dev")
weight[,(deleteListCol) := NULL]

food_des <- FOOD_DES[NDB_No %in% USDAcodes,]
deleteListCol <- c("ComName", "ManufacName", "Survey", "Shrt_Desc", "SciName", "N_Factor", "Pro_Factor", "Fat_Factor", "CHO_Factor")
food_des[,(deleteListCol) := NULL]

dt <- merge(nutr_def, nut_data, by = "Nutr_No")
dt <- merge(dt, food_des, by = "NDB_No")
dt <- merge(dt.IMPACTcodeLookup, dt, by.x = "usda_code", by.y = "NDB_No")
dt <- merge(dt, dt.nutcodeLookup, by = c("NutrDesc", "Nutr_No", "Units"))

formula.wide <- paste("usda_code + IMPACT_code + FdGrp_Cd + Long_Desc + Ref_Desc + Refuse ~ nutCode")
dt.wide <- data.table::dcast(
  data = dt,
  formula = formula.wide,
  value.var = "Nutr_Val")

dt.wide[, 6:length(dt.wide)][is.na(dt.wide[, 6:length(dt.wide)])] <- 0

dt.cookingRetn <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/USDAcookingretn06.xlsx", colNames = FALSE))
# get rid of numeric version of the date enter variable; last one
dt.cookingRetn[, X7 := NULL]
data.table::setnames(dt.cookingRetn, old = names(dt.cookingRetn), new = c("Retn_Code", "FdGrp_Cd", "RetnDesc",
                                                                         "Nutr_No", "NutrDesc", "Retn_Factor"))
dt.cookingRetn[, Nutr_No := as.character(Nutr_No)][, Retn_Code := as.character(Retn_Code)][, FdGrp_Cd := as.character(FdGrp_Cd)]
dt.cookingRetn <- merge(dt.cookingRetn, dt.nutcodeLookup, by = c("Nutr_No"))
formula.wide <- paste("RetnDesc + Retn_Code + FdGrp_Cd  ~ nutCode")
dt.cookingRetn.wide <- data.table::dcast(
  data = dt.cookingRetn,
  formula = formula.wide,
  value.var = "Retn_Factor")

ch <- dbConnect(RODBCDBI::ODBC(),"USDA")
close(ch)
# or if you prefer
odbcClose(ch)

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
