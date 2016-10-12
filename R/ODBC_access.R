library(RODBC)
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
con <- odbcConnect("nutrientData")
tbls <- sqlTables(con, tableType = "TABLE")
# principal files
FOOD_DES <- sqlFetch(con, "FOOD_DES") #food description; code is NDB_No, contains Food Group code, FdGrp_Cd
NUT_DATA <- sqlFetch(con, "NUT_DATA") #nutrient data file; code is Nutrient_No; contains NDB_No
WEIGHT <- sqlFetch(con, "WEIGHT") #gram weight file; contains NDB_No
FOOTNOTE <- sqlFetch(con, "FOOTNOTE")

#support files
FD_GROUP <- sqlFetch(con, "FD_GROUP")
NUTR_DEF <- sqlFetch(con, "NUTR_DEF") #nutrient definition
SRC_CD <- sqlFetch(con, "SRC_CD") # source of data

sqlColumns(con, "NUTR_DEF")$COLUMN_NAME
sqlColumns(con, "FD_GROUP")$COLUMN_NAME



ch <- dbConnect(RODBCDBI::ODBC(),"USDA")
close(ch)
# or if you prefer
odbcClose(ch)

library(RODBC)
dbhandle <- odbcDriverConnect('driver={Microsoft Access Driver (*.mdb, *.accdb)};server=nutrientData;database=/Users/gcn/Documents/workspace/nutmod/data-raw/NutrientData/sr27db/sr27.accdb;trusted_connection=true')
res <- sqlQuery(dbhandle, 'select * from information_schema.tables')
dbhandle <- odbcDriverConnect('driver={Actual Access};database=/Users/gcn/Documents/workspace/nutmod/data-raw/NutrientData/sr27db/sr27.accdb;trusted_connection=true')

