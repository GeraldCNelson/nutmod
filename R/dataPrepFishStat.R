#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords fish data prep, FishStat
#' @title Prepare fish data from FishStat for use in nutrient modeling

#Copyright (C) 2018 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
#' \description{
#' Reads in fish data from FishStat and manipulates it for use in nutrient modeling. It produces averages over the three years found in the key variable keepListYears.composites.
#' }

# source of the data - http://www.fao.org/fishery/statistics/global-production/en, downloaded Feb 13, 2018 and
# stored as data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1.zip`

source("R/nutrientModFunctions.R")

sourceFile <- "dataPrepFishStat.R"
description <- "Reads in fish data from FishStat and manipulates it for use in nutrient modeling. It produces averages over the three years found in the key variable keepListYears.composites."

createScriptMetaData()
# aquatic plants used as food not included in FishStat so "c_aqpl not included in this code
fishComposites <- keyVariable("fishComposites")
#TS_FI_PRODUCTION <- fread("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/TS_FI_PRODUCTION.csv")
TS_FI_PRODUCTION <- as.data.table(read_csv("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/TS_FI_PRODUCTION.csv", col_names = TRUE, cols(
  Country = col_character(),
  Area = col_character(),
  Source = col_character(),
  Species = col_character(),
  Year = col_character(),
  Quantity = col_double(),
  Quantity_Symbol = col_character()
)))

# Area and source needed.
# Area is where a country fishes. E.g. Japan fishes in multiple places. There are 29 potential places where fishing occurs.
#Source has five options - 3 types of aquaculture and capture production
TS_FI_PRODUCTION[, Quantity := as.numeric(Quantity)]
keepListCol <- c("Country", "Area", "Source", "Species", "Year", "Quantity")
TS_FI_PRODUCTION[, setdiff(names(TS_FI_PRODUCTION), keepListCol) := NULL]
setnames(TS_FI_PRODUCTION, old = "Country", new = "UNI_code")
TS_FI_PRODUCTION[, Year := paste0("Y", Year)]
keepListYears.composites <- keyVariable("keepListYears.composites")
dt.prod <- TS_FI_PRODUCTION[Year %in% keepListYears.composites]

CL_FI_SPECIES_GROUPS <- fread("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/CL_FI_SPECIES_GROUPS.csv")
keepListCol <- c("3Alpha_Code", "Taxonomic_Code", "Name_en",  "Major_Group", "ISSCAAP_Group", "CPC_Group", "CPCdiv_Group")
CL_FI_SPECIES_GROUPS[, setdiff(names(CL_FI_SPECIES_GROUPS), keepListCol) := NULL]

# convert to wide to do the sum of production over three years
formula.wide <- "UNI_code + Area + Source + Species  ~ Year"
dt.prod.wide <- data.table::dcast(
  data = dt.prod,
  formula = formula.wide,
  value.var = "Quantity")
dt.prod.wide[, (names(dt.prod.wide)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.prod.wide)]

dt.prod.wide[, yearAve := rowMeans(.SD), .SDcols = keepListYears.composites]
dt.prod.wide[, (keepListYears.composites) := NULL]

formula.wide <- "UNI_code + Area + Species  ~ Source"
dt.prod.wide <- data.table::dcast(
  data = dt.prod.wide,
  formula = formula.wide,
  value.var = "yearAve")
dt.prod.wide[, (names(dt.prod.wide)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.prod.wide)]

sourceTypes <- c("1", "2", "3", "4")
setnames(dt.prod.wide, old = sourceTypes, new = paste0("S", sourceTypes))
# sum production over source types here
dt.prod.wide[, sourceTot := S1 + S2 + S3 + S4]
dt.prod.wide[, (paste0("S", sourceTypes)) := NULL]

#aggregate by area
areaUnits <- fread("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/CL_FI_AREA_GROUPS.csv")
formula.wide <- "UNI_code + Species  ~ Area"
dt.prod.wide <- data.table::dcast(
  data = dt.prod.wide,
  formula = formula.wide,
  value.var = "sourceTot")

# areaTypes are numeric. As column names they should really have a X in front to make them legitimate column names
areaTypes <- unique(areaUnits$Code)
areaTypes <- areaTypes[!areaTypes %in% "08"] # 8 is Antarctica

# set NA to 0
NAlist <- names(dt.prod.wide)[!names(dt.prod.wide) %in% c("UNI_code", "Species")]
dt.prod.wide[, (NAlist) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = NAlist]
dt.prod.wide[, prodAve := rowSums(.SD), .SDcols = NAlist]
dt.prod.wide[, (NAlist) := NULL]

dt.regions <- getNewestVersion("dt.regions.all", fileloc("uData"))
keepListCol <- c("region_code.IMPACT159", "FAOSTAT_code", "UNI_code", "country_name.ISO")
dt.regions[, setdiff(names(dt.regions), keepListCol) := NULL][, UNI_code := as.character(UNI_code)]

#add IMPACT code regions
fishprod <- merge(dt.prod.wide, dt.regions, by = "UNI_code", all.y = TRUE)
fishprod <- fishprod[!is.na(Species)]
deleteListCol <- c( "FAOSTAT_code", "country_name.ISO")
fishprod[, (deleteListCol) := NULL]

#' aggregate smaller countries to their IMPACT159 regions
#data.table::setkeyv(fishprod, c("Species", "region_code.IMPACT159"))
#fishprod[, prodAveNew := sum(prodAve), by = eval(data.table::key(fishprod))]
fishprod[, prodAve := sum(prodAve), by = c("region_code.IMPACT159", "Species")]
fishprod[, UNI_code := NULL]
fishprod <- unique(fishprod)

fishprod <- merge(fishprod, CL_FI_SPECIES_GROUPS, by.x = "Species", by.y = "3Alpha_Code", all.x = TRUE)

# read in info that aligns species names with composite fish items
library(readxl)
dt.compositesLU.fish <- data.table::as.data.table(read_excel("data-raw/NutrientData/nutrientDetails/composites.lookup.fish.xlsx",
                                                             col_types = c("text", "text", "numeric",
                                                                           "numeric", "numeric", "text", "numeric",
                                                                           "text", "skip")))
dt <- merge(dt.compositesLU.fish, fishprod, by.x = "item_name", by.y = "Name_en")
dt <- dt[!remove %in% "1",]
deleteListCol <- c("include", "remove", "Taxonomic_Code", "Major_Group", "ISSCAAP_Group",  "CPC_Group" , "CPCdiv_Group", "item_code", "ratio_prod_live")
keepListCol <- names(dt)[!names(dt) %in% deleteListCol]
dt[, (deleteListCol) := NULL]
dt <- unique(dt)
setnames(dt, old = c("Species", "prodAve"), new = c("item_code", "prodAve"))
inDT <- dt
outName <- "dt.fishStatData"
desc <- "Production and edible share of fish species and which composite they are part of by country from FishStat"
cleanup(inDT, outName, fileloc("iData"), desc = desc)
finalizeScriptMetadata(metadataDT, sourceFile)
