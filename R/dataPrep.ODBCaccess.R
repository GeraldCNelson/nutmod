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
inDT <- FOOD_DES
outName <- "FOOD_DES"
cleanup(inDT, outName, fileloc("mData"))
inDT <- NUT_DATA
outName <- "NUT_DATA"
cleanup(inDT, outName, fileloc("mData"))
inDT <- NUTR_DEF
outName <- "NUTR_DEF"
cleanup(inDT, outName, fileloc("mData"))


