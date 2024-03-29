#' @title Manage FAO FBS data
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, FAOSTAT, Food balance sheet data, data management
#Copyright (C) 2016 - 2018 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify i
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.
# Intro -------------------------------------------------------------------
#' @description {
#' This script reads in the FAO Food Balance Sheet information from a zip file, does
#' some manipulations of the data, and writes out results to an rds file
#' }.
#' @source \url{http://faostat3.fao.org/download/FB/FBS/E}

source("R/nutrientModFunctions.R")

sourceFile <- "dataPrep.FBS.R"
description <- "This script reads in the FAO Food Balance Sheet information from a zip file, does some manipulations of the data, and writes out results to an rds file."
createScriptMetaData()

# Read in the FBS data from a zip file
# FAO changed the structure of the zip file some time in 2015.
# Code to read in the old structure remains below but is commented out
# dt.FBS.raw <- as.data.table(read.csv(unzip(FBSdataZip, file = FBScsv),
#                        stringsAsFactors = FALSE,
#                        colClasses = list((character=1:7))))

# temp <- unz(FBSdataZip,FBScsv)
# data <- read.table(unz(FBSdataZip, FBScsv), nrows=10, header=T, quote="\"", sep=",")

FBSData <- fileloc("FBSData")
FBSdataZipFile <- filelocFBS("FBSdataZipFile")
FBSdataZip <- paste(FBSData, FBSdataZipFile, sep = "/")

dt.FBS.raw <- as.data.table(readr::read_csv(unz(FBSdataZip, filename = "FoodBalanceSheets_E_All_Data.csv"), col_names = TRUE, guess_max = 2000, cols(
  `Country` = col_character(), `Country Code` = col_character(), `Item Code` = col_character(), `Element` = col_character(),
  `Element Code` = col_character(), `Unit` = col_character())
))

# change original column names to 'names that are consistent with other data sources
keepYearList.FBS <- keyVariable("keepYearList.FBS")
keepYearList.FBS.oldname <- gsub("X", "Y", keepYearList.FBS)
data.table::setnames(dt.FBS.raw, old = c("Element", "Element Code", "Country", "Country Code", "Item Code", "Item", "Unit", keepYearList.FBS.oldname),
                     new = c("variable", "variable_code", "country_name", "FAOSTAT_country_code", "item_code", "item", "unit", keepYearList.FBS))

#this drops all the ...F columns which provide information for each year on the data in that year
keepListCol <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
                 "variable_code", "variable", "unit", keyVariable("keepYearList.FBS"))
dt.FBS.raw[, setdiff(names(dt.FBS.raw), keepListCol) := NULL]

# improve on some element names
dt.FBS.raw[variable == "Food", variable := "foodMT"]
dt.FBS.raw[variable == "Food supply quantity (kg/capita/yr)", variable := "kgPerCapPerYear"]
dt.FBS.raw[variable == "Food supply (kcal/capita/day)", variable := "KcalPerCapPerDay"]

# get rid of variables other than perCapKg, kgs per person per *year* and kcals per person per *day*
dt.FBS.raw <- dt.FBS.raw[variable %in% c("kgPerCapPerYear", "KcalPerCapPerDay")]

# Read in a worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code.
FBSCommodityInfo <- filelocFBS("FBSCommodityInfo")
dt.FBScommodLookup <- data.table::as.data.table(openxlsx::read.xlsx(FBSCommodityInfo,
                    sheet = 1, startRow = 1, cols = 1:7, colNames = TRUE))

dt.FBScommodLookup[, item_code := as.character(item_code)]

# remove the item called "Miscellaneous"
dt.FBScommodLookup <- dt.FBScommodLookup[!item_name == "Miscellaneous",]

# Read in the region lookup table, created in dataPrep.regions.R
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))

keeplistCol <- c("ISO_code","FAOSTAT_code")
dt.regions.all[,setdiff(names(dt.regions.all), keeplistCol) := NULL]

data.table::setnames(dt.regions.all,c("FAOSTAT_code"),c("FAOSTAT_country_code"))

# deal with Sudan and Sudan (former issue)
# the old country Sudan (SDN) split into two parts in 2011. A new country called Sudan (SDN) and a second
# country called South Sudan (SSD). The old country data are under the name Sudan (former) in the FBS
# data set, with the FAOSTAT numeric code of 206 in the FBS data. But in the name lookup table they are
# listed as 276 and 277.
# Currently there are no data for South Sudan at all and only up to 2011 for Sudan.
# So I'm going to change the code for Sudan in the name lookup to 206 and see what happens
dt.regions.all[FAOSTAT_country_code == "276", FAOSTAT_country_code := "206"]

data.table::setkey(dt.FBS.raw,FAOSTAT_country_code)
data.table::setkey(dt.regions.all,FAOSTAT_country_code)
dt.FBS <- dt.FBS.raw[dt.regions.all]

# Create separate data  without the commodities aggregations
aggregates <- c("Alcoholic Beverages", # added March 19, 2017
                "Animal fats + (Total)",
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

#remove rows where FBS category is 'Miscellaneous' because we don't have an IMPACT equivalent
itemsToRemove <- c(aggregates, "Miscellaneous")
dt.FBS.commods <- dt.FBS[!item %in% itemsToRemove]

# ARE (United Arab Emirates) doesn't have entries for kcals from alcoholic beverages, even zero

#include IMPACT code and nonIMPACT code assignment in the commodities data frame
data.table::setkey(dt.FBS.commods,item_code)
data.table::setkey(dt.FBScommodLookup,item_code)

temp <- dt.FBS.commods[dt.FBScommodLookup]
deleteListCol <- c("FAOSTAT_country_code","IMPACT_missing_code","fish","alcohol")
temp[,(deleteListCol) := NULL]

idVars <- c( "country_name","item_code","item", "variable_code","variable","unit","ISO_code","item_name","definition",
             "IMPACT_code")
dt.FBS.commods.melt <- data.table::melt(temp,
                                        id.vars = idVars,
                                        measure.vars = keepYearList.FBS,
                                        variable.name = "year",
                                        value.name = "value",
                                        variable.factor = FALSE)

# need to sum individual FBS commodities to the IMPACT commodity they are in
dt.FBS.commods.melt[,value.sum := sum(value), by = list(ISO_code, variable, IMPACT_code, year)]
# keep "value" around in case we need to compare it

# now get rid of info that is not needed
keepListCol <- c("country_name", "variable_code", "variable", "unit", "ISO_code",
                 "IMPACT_code", "year", "value.sum")
dt.FBS.commods.melt[,setdiff(names(dt.FBS.commods.melt), keepListCol) := NULL]
dt.FBS.commods.final <- unique(dt.FBS.commods.melt)
data.table::setnames(dt.FBS.commods.final,old = "value.sum", new = "value")
data.table::setorder(dt.FBS.commods.final, ISO_code)
dt.FBS.commods.final <- dt.FBS.commods.final[!is.na(country_name)]
dt.FBS.commods.final[, (names(dt.FBS.commods.final)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.FBS.commods.final)]

inDT <- dt.FBS.commods.final
outName <- "dt.FBS"
desc <- "Sum FBS commodities, and kcals per day to the IMPACT commodity they are included in" # kcals added July 24, 2018
cleanup(inDT,outName,fileloc("uData"), desc = desc)
finalizeScriptMetadata(metadataDT, sourceFile)
