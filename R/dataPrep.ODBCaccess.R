#library(RODBC)
library(data.table)
source("R/nutrientModFunctions.R")
sourceFile <- "dataPrep.ODBCaccess.R"
createScriptMetaData()
# as of Dec 21, 21017, the code below has been converted to reading in data from the ascii files.
# some notes on setting RODBC up
# 1.	Install homebrew. Grab the line of code at http://brew.sh/ and paste into a terminal prompt. Here’s what it looks like today but should be reviewed at website before use
# /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# 1. if you have already installted homebreiw, type brew update at a terminal prompt.
# 2.	After 1. completes successfully, type
# brew install libiodbc
# 3.	After 2. completes successfully, download and install the actualtech driver.
# 4.	In Rstudio (and almost certainly also in R), type
# install.packages("RODBC")
# 5.	This will do a few things and then ask whether you want to compile from source. Answer ‘y’. It will do some more c stuff and then exit successfully.
# 6. run the ODBC manager and set up the connection to the access data base file as a system DSN (user DSN is probably ok too)
# 7. run this script

#read Access data base -----
# the Access data base is downloaded from https://iapreview.ars.usda.gov/Services/docs.htm?docid=25700.
# It is the USDA National Nutrient Databank for Food Composition.
# It is described at https://iapreview.ars.usda.gov/research/projects/projects.htm?accn_no=426560
#con <- odbcConnect("nutrientDataSR28",DBMSencoding = "Windows-1252") # this encoding is needed to get the mu character correct
#con <- odbcConnect(dsn=paste(getwd(),"data-raw/NutrientData/sr28db/sr28.accdb",sep = "/"),DBMSencoding="Windows-1252") # this encoding is needed to get the mu character correct
# con <- odbcConnect(dsn="nutrientDataSR28",DBMSencoding="Windows-1252") # this encoding is needed to get the mu character correct
# tbls <- sqlTables(con, tableType = "TABLE")

# principal files
#FOOD_DES <- as.data.table(sqlFetch(con, "FOOD_DES", as.is = TRUE )) #food description; code is NDB_No, contains Food Group code, FdGrp_Cd)

# library(readr)
# FOOD_DES <- read_delim("data-raw/NutrientData/sr28asc/FOOD_DES.txt",
#                        "^", quote = "~", escape_double = FALSE,
#                        trim_ws = TRUE)

FOOD_DES <- as.data.table(fread("data-raw/NutrientData/sr28asc/FOOD_DES.txt",
                                   quote = "~", sep = "^", head = FALSE, stringsAsFactors = FALSE,
                                   col.names = c("NDB_No", "FdGrp_Cd", "Long_Desc", "Shrt_Desc", "ComName", "ManufacName", "Survey",
                                                 "Ref_Desc", "Refuse", "SciName", "N_Factor", "Pro_Factor", "Fat_Factor", "CHO_Factor"),
                                   colClasses = c("character", "character", "character", "character", "character", "character", "character",
                                                  "character", "numeric", "character", "numeric", "numeric", "numeric", "numeric")))
setnames(FOOD_DES, old = names(FOOD_DES), new = c("NDB_No", "FdGrp_Cd", "Long_Desc", "Shrt_Desc", "ComName", "ManufacName", "Survey",
                                                  "Ref_Desc", "Refuse", "SciName", "N_Factor", "Pro_Factor", "Fat_Factor", "CHO_Factor"))
deleteListCol <- c("FdGrp_Cd","ComName", "ManufacName", "Survey", "Shrt_Desc", "SciName", "N_Factor", "Pro_Factor", "Fat_Factor", "CHO_Factor")
FOOD_DES[,(deleteListCol) := NULL]
FOOD_DES[,edible_share := 100 - Refuse][is.na(edible_share), edible_share := 100]
data.table::setnames(FOOD_DES, old = "NDB_No", new = "usda_code")
FOOD_DES[, usda_code := as.character(usda_code)]

#NUT_DATA <- as.data.table(sqlFetch(con, "NUT_DATA", as.is = TRUE)) #nutrient data file; code is Nutr_No; contains NDB_No
NUT_DATA <- as.data.table(fread("data-raw/NutrientData/sr28asc/NUT_DATA.txt",
                                   quote = "~", sep = "^", head = FALSE, stringsAsFactors = FALSE,
                                   col.names = c("NDB_No", "Nutr_No", "Nutr_Val", "Num_Data_Pts", "Std_Error", "Src_Cd", "Deriv_Cd",
                                                 "Ref_NDB_No", "Add_Nutr_Mark", "Num_Studies", "Min", "Max", "DF", "Low_EB", "Up_EB",
                                                 "Stat_Cmt", "AddMod_Date", "V18"),
                                   colClasses = c("character", "character",  "numeric", "numeric", "numeric", "character", "character",
                                                  "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "character", "character", "character")))
NUT_DATA[, V18 := NULL] # must be something strange at the end of the txt file
setnames(NUT_DATA, old = names(NUT_DATA), new = c("NDB_No", "Nutr_No", "Nutr_Val", "Num_Data_Pts", "Std_Error", "Src_Cd", "Deriv_Cd",
                                                  "Ref_NDB_No", "Add_Nutr_Mark", "Num_Studies", "Min", "Max", "DF", "Low_EB", "Up_EB",
                                                  "Stat_Cmt", "AddMod_Date"))
deleteListCol <- c("Num_Data_Pts", "Std_Error", "Src_Cd", "Deriv_Cd", "Ref_NDB_No",
                   "Add_Nutr_Mark", "Num_Studies", "Min", "Max", "DF", "Low_EB", "Up_EB", "Stat_Cmt", "AddMod_Date")
NUT_DATA[,(deleteListCol) := NULL]
data.table::setnames(NUT_DATA, old = "NDB_No", new = "usda_code")
#NUT_DATA[, usda_code := as.character(usda_code)][, Nutr_No := as.character(Nutr_No)]

#not used yet
# WEIGHT <- as.data.table(sqlFetch(con, "WEIGHT", as.is = TRUE)) #gram weight file of a serving; contains NDB_No
# deleteListCol <- c("Seq", "Amount", "Msre_Desc", "Num_Data_Pts", "Std_Dev")
# WEIGHT[,(deleteListCol) := NULL]
# data.table::setnames(WEIGHT, old = "NDB_No", new = "usda_code")
#
# FOOTNOTE <- as.data.table(sqlFetch(con, "FOOTNOTE"))

#support files
#FD_GROUP <- as.data.table(sqlFetch(con, "FD_GROUP"))
#NUTR_DEF <- as.data.table(sqlFetch(con, "NUTR_DEF", as.is = TRUE)) #nutrient definition
# the file encoding option makes sure mu comes in as the proper character
NUTR_DEF <- as.data.table(read.csv("data-raw/NutrientData/sr28asc/NUTR_DEF.txt",
                                   quote = "~", sep = "^", head = FALSE, stringsAsFactors = FALSE, fileEncoding = "Windows-1252",
                                   col.names = c("Nutr_No", "Units", "Tagname", "NutrDesc", "Num_Dec", "SR_Order"),
                                   colClasses = c("character", "character", "character", "character", "character")))
deleteListCol <- c("Tagname", "Num_Dec",  "SR_Order")
NUTR_DEF[,(deleteListCol) := NULL][, Nutr_No := as.character(Nutr_No)]

# invisible(SRC_CD <- as.data.table(sqlFetch(con, "SRC_CD"))) # source of data
# odbcClose(con)
inDT <- FOOD_DES
outName <- "FOOD_DES"
desc <- "Food descriptive info from USDA FCT"
cleanup(inDT, outName, fileloc("mData"), desc = desc)
inDT <- NUT_DATA
outName <- "NUT_DATA"
desc <- "Nutrient data from USDA FCT"
cleanup(inDT, outName, fileloc("mData"), desc = desc)
inDT <- NUTR_DEF
outName <- "NUTR_DEF"
desc <- "Nutrient definitions from USDA FCT"
cleanup(inDT, outName, fileloc("mData"), desc = desc)

finalizeScriptMetadata(metadataDT, sourceFile)


