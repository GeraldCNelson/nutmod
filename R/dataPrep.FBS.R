#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, FAOSTAT, data management
#'
#' @description This script reads in the FAO Food Balance Sheet information and does some manipulations of the data
#' @source \url{http://faostat3.fao.org/download/FB/FBS/E}
#'
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function")) {source("R/nutrientModFunctions.R")}
# Intro -------------------------------------------------------------------
#' \description{
#' This script reads in the FAO Food Balance Sheet information from a zip file, does
#' some manipulations of the data, and writes out results to an rds file
#' }.
#This script reads in the FAO Food Balance Sheet information from a zip file, does
# some manipulations of the data,
#and writes out results to an rds file

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

# Read in the FBS data from a zip file
# FAO changed the structure of the zip file some time in 2015.
# Code to read in the old structure remains below but is commented out
# dt.FBSrawData <- as.data.table(read.csv(unzip(FBSdataZip, file = FBScsv),
#                        stringsAsFactors = FALSE,
#                        colClasses = list((character=1:7))))

# temp <- unz(FBSdataZip,FBScsv)
# data <- read.table(unz(FBSdataZip, FBScsv), nrows=10, header=T, quote="\"", sep=",")

colCharacter = c("Country Code", "Country","Item Code","Item","Element Code","Element", "Unit" )

# temp <- read_file(FBSdataZip,col_numeric(colNumeric),col_character("Country.Code"))
FBSdataZip <- filelocFBS("FBSdataZip")
FBScsv <- filelocFBS("FBScsv")
temp <- unzip(paste(getwd(),FBSdataZip,sep = "/"), files = FBScsv)
dt.FBSrawData <- data.table::fread(temp, header = TRUE)
file.remove(temp)

#temp <- read.csv("data-raw/FBSData/FoodBalanceSheets_E_All_Data.csv",nrows = 10)

# old column names

# colnames(FBSrawData) <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
# "element_group", "variable_code", "element","year", "unit", "value", "flag")

colnames(dt.FBSrawData) <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
                          "variable_code", "element","unit",
                          "X1961","X1961F","X1962","X1962F","X1963","X1963F","X1964","X1964F","X1965","X1965F",
                          "X1966","X1966F","X1967","X1967F","X1968","X1968F","X1969","X1969F","X1970","X1970F",
                          "X1971","X1971F","X1972","X1972F","X1973","X1973F","X1974","X1974F","X1975","X1975F",
                          "X1976","X1976F","X1977","X1977F","X1978","X1978F","X1979","X1979F","X1980","X1980F",
                          "X1981","X1981F","X1982","X1982F","X1983","X1983F","X1984","X1984F","X1985","X1985F",
                          "X1986","X1986F","X1987","X1987F","X1988","X1988F","X1989","X1989F","X1990","X1990F",
                          "X1991","X1991F","X1992","X1992F","X1993","X1993F","X1994","X1994F","X1995","X1995F",
                          "X1996","X1996F","X1997","X1997F","X1998","X1998F","X1999","X1999F","X2000","X2000F",
                          "X2001","X2001F","X2002","X2002F","X2003","X2003F","X2004","X2004F","X2005","X2005F",
                          "X2006","X2006F","X2007","X2007F","X2008","X2008F","X2009","X2009F","X2010","X2010F",
                          "X2011","X2011F","X2012","X2012F","X2013","X2013F","X2014","X2014F","X2015","X2015F")

#this drops all the ...F columns which provide information for each year on the data in that year
colKeepList <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
                 "variable_code", "element","unit",
                 "X1961","X1962","X1963","X1964","X1965",
                 "X1966","X1967","X1968","X1969","X1970",
                 "X1971","X1972","X1973","X1974","X1975",
                 "X1976","X1977","X1978","X1979","X1980",
                 "X1981","X1982","X1983","X1984","X1985",
                 "X1986","X1987","X1988","X1989","X1990",
                 "X1991","X1992","X1993","X1994","X1995",
                 "X1996","X1997","X1998","X1999","X2000",
                 "X2001","X2002","X2003","X2004","X2005",
                 "X2006","X2007","X2008","X2009","X2010",
                 "X2011","X2012","X2013","X2014","X2015")

dt.FBSraw <- dt.FBSrawData[,colKeepList, with = FALSE]

# improve on some element names
dt.FBSraw[element=="Food", element := "foodMT"]
dt.FBSraw[element=="Food supply quantity (kg/capita/yr)", element := "perCapKg"]
dt.FBSraw[element=="Food supply (kcal/capita/day)",element := "perCapKcal"]
          dt.FBSraw[element=="Protein supply quantity (g/capita/day)", element := "perCapProt"]
dt.FBSraw[element=="Fat supply quantity (g/capita/day)", element := "perCapFat"]

# change 'element' to 'variable' to make it consistent with other data sources
data.table::setnames(dt.FBSraw,old=c("element","variable_code"),new = c("variable","variable_code"))

#how to drop years with the previous version of the FBS data
# remove years before 2010. The latest year is 2011 currently.
# setkey(dt.FBSraw,year)
# dt.FBSraw <- dt.FBSraw[year > 2009,]

#keep just the years in keepYearList.FBS
keepYearList.FBS  <- keyVariable("keepYearList.FBS")
colKeepListYears <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
                 "variable_code", "variable","unit",
                 keepYearList.FBS)

dt.FBSraw <- dt.FBSraw[,colKeepListYears, with = FALSE]

#old version of convert selected columns to character class
#charConvertList <- c("FAOSTAT_country_code","variable_code","item_code","unit","year")

#old version of convert selected columns to numeric class
charConvertList <- c("FAOSTAT_country_code","variable_code","item_code","unit")
dt.FBSraw[,(keepYearList.FBS) := lapply(.SD, as.numeric), .SDcols=keepYearList.FBS]

#add X to beginning of the year (X2009 instead of 2009), old version of the FBS data
#dt.FBSraw[, year := paste("X", dt.FBSraw$year, sep = "")]

# Read in a worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
FBSCommodityInfo <- filelocFBS("FBSCommodityInfo")
dt.FBScommodLookup <- data.table::as.data.table(openxlsx::read.xlsx(FBSCommodityInfo,
                             sheet = 1,
                             startRow = 1,
                             cols = 1:7,
                             colNames = TRUE))

charConvertList <- c("item_code")
for (col in charConvertList) data.table::set(dt.FBScommodLookup, j = col, value = as.character(dt.FBScommodLookup[[col]]))

# remove the item called "Miscellaneous"
dt.FBScommodLookup <- dt.FBScommodLookup[!item_name == "Miscellaneous",]

# Read in the region lookup table, created in dataPrep.regions.R # this doesn't seem to be used
#dt.regions.all <- getNewestVersion("dt.regions.all")

FAOCountryNameCodeLookup <- filelocFBS("FAOCountryNameCodeLookup")
# Read in the worksheet that has the FAO country code-ISO country name lookup
dt.FBSNameLookup <- data.table::as.data.table(openxlsx::read.xlsx(FAOCountryNameCodeLookup,
                  sheet = 1,
                  startRow = 1,
                  colNames = TRUE))

#convert to character and leave just ISO code and FAOSTAT code
charConvertList <- c("ISO3","FAOSTAT")
dt.FBSNameLookup <- dt.FBSNameLookup[, lapply(.SD, as.character), .SDcols = charConvertList]
data.table::setnames(dt.FBSNameLookup,c("ISO3","FAOSTAT"),c("ISO_code","FAOSTAT_country_code"))

data.table::setkey(dt.FBSraw,FAOSTAT_country_code)
data.table::setkey(dt.FBSNameLookup,FAOSTAT_country_code)
dt.FBS <- dt.FBSraw[dt.FBSNameLookup]

# # Check for aggregations of countries; this should have no content
# FBSDat.countryAggs <- subset(dt.FBS,!(ISO_code %in% regions.ISO$ISO_code))
#
# #get rid of rows that are aggregations of countries
# dt.FBS <- subset(dt.FBS,Country %in% regions.ISO$country_name)

# Create separate data data without the commodities aggregations
aggregates <- c("Animal fats + (Total)",
                "Cereals - Excluding Beer + (Total)",
                "Meat + (Total)",
                "Milk - Excluding Butter + (Total)",
                "Offals + (Total)",
                "Oilcrops + (Total)",
                "Pulses + (Total)",
                "Stimulants + (Total)",
                "Sugar & Sweeteners + (Total)",
                "Vegetable Oils + (Total)",
                "Spices + (Total)",
                "Starchy Roots + (Total)",
                "Sugar Crops + (Total)",
                "Treenuts + (Total)",
                "Vegetables + (Total)",
                "Vegetal Products + (Total)",
                "Alcoholic Beverages + (Total)",
                "Animal Products + (Total)",
                "Aquatic Products, Other + (Total)",
                "Eggs + (Total)",
                "Fish, Seafood + (Total)",
                "Fruits - Excluding Wine + (Total)",
                "Grand Total + (Total)",
                "Miscellaneous + (Total)")
#dt.FBS.aggs <- subset(dt.FBS,(item %in% aggregates))
dt.FBS.commods <- dt.FBS[!item %in% aggregates]

#remove rows where FBS category is 'Miscellaneous' because we don't have an IMPACT equivalent
dt.FBS.commods <- dt.FBS.commods[!item == "Miscellaneous"]

# ARE (United Arab Emirates) doesn't have entries for kcals from alcoholic beverages, even zero

#include IMPACT code and nonIMPACT code assignment in the commodities data frame
data.table::setkey(dt.FBS.commods,item_code)
data.table::setkey(dt.FBScommodLookup,item_code)

temp <- dt.FBS.commods[dt.FBScommodLookup]
deleteColList <- c("FAOSTAT_country_code","IMPACT_missing_code","fish","alcohol")
temp[,(deleteColList) := NULL]

idVars <- c( "country_name","item_code","item",
"variable_code","variable","unit","ISO_code","item_name","definition",
"IMPACT_code")
dt.FBS.commods.melt <- data.table::melt(temp,
                       id.vars = idVars,
                       variable.name = "year",
                       measure.vars = keepYearList.FBS,
                       variable.factor = FALSE)

# need to sum individual FBS commodities to the IMPACT commodity they are in
dt.FBS.commods.melt[,value.sum := sum(value), by = list(ISO_code,variable, IMPACT_code,year)]
# keep "value" around in case we need to compare it

#now get rid of info that is not needed
keepListCol <- c("country_name", "variable_code", "variable", "unit", "ISO_code",
"IMPACT_code", "year", "value.sum")
dt.temp <- dt.FBS.commods.melt[,keepListCol, with=FALSE]
data.table::setkey(dt.temp)
dt.FBS.commods.final <-
#  unique(dt.temp[, c(eval(data.table::key(dt.temp)), keepListCol), with = FALSE])
unique(dt.temp[, (keepListCol), with = FALSE])
data.table::setnames(dt.FBS.commods.final,old = "value.sum", new = "value")
dt.FBS.commods.final[order(ISO_code)]
inDT <- dt.FBS.commods.final
outName <- "dt.FBS"
cleanup(inDT,outName,fileloc("mData"))
