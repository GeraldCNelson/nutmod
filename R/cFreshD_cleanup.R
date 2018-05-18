# match up item name and item codes. The item names are in the comp recalc spreadsheets but item code is not.
# Just need to do this for Fresh fish. c_FreshD.
# This script is something that might be needed before running dataPrepFishStat.R but I'm not sure April 11, 2018
# the dt.compositesLU file contains the results of this script so I don't think it is necessary to run it anymore. May 15, 2108

# source of the data - http://www.fao.org/fishery/statistics/global-production/en, downloaded Feb 13, 2018 and
# stored as data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1.zip`

source("R/nutrientModFunctions.R")

fishcomposites <- c("c_Milsc", "c_Odmsrl", "c_Opelag", "c_Crust", "c_Omarn", "c_FreshD")
TS_FI_PRODUCTION <- fread("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/TS_FI_PRODUCTION.csv")
# Area and source needed.
# Area is where a country fishes. E.g. Japan fishes in multiple places. There are 29 potential places where fishing occurs.
#Source has five options - 3 types of aquaculture and capture production
keepListCol <- c("Country", "Area", "Source", "Species", "Year", "Quantity")
TS_FI_PRODUCTION[, Quantity := as.numeric(Quantity)]
TS_FI_PRODUCTION <- TS_FI_PRODUCTION[, (keepListCol), with = FALSE]
setnames(TS_FI_PRODUCTION, old = "Country", new = "UNI_code")
TS_FI_PRODUCTION[, Year := paste0("Y", Year)]
keepListYears.composites <- keyVariable("keepListYears.composites")
dt.prod <- TS_FI_PRODUCTION[Year %in% keepListYears.composites]

CL_FI_SPECIES_GROUPS <- fread("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/CL_FI_SPECIES_GROUPS.csv")
keepListCol <- c("3Alpha_Code", "Taxonomic_Code", "Name_en",  "Major_Group", "ISSCAAP_Group", "CPC_Group", "CPCdiv_Group")
CL_FI_SPECIES_GROUPS <- CL_FI_SPECIES_GROUPS[, (keepListCol), with = FALSE]

dt.compositesLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/composites.fish.Lookup.xlsx"))
dt.c_freshD <- dt.compositesLU[composite %in% "c_FreshD",]
dt.c_freshD[, item_code := NULL]
setnames(CL_FI_SPECIES_GROUPS, old = c("3Alpha_Code", "Name_en"), new = c("item_code", "item_name"))
temp <- merge(dt.c_freshD,CL_FI_SPECIES_GROUPS, by = c("item_name"))
deleteListCol <- c("Taxonomic_Code", "Major_Group", "ISSCAAP_Group", "CPC_Group", "CPCdiv_Group")
temp[, (deleteListCol) := NULL]
setcolorder(temp, names(dt.compositesLU))
write.csv(temp, "c_FreshD.csv")
