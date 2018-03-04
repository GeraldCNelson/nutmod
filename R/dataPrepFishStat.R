

# source of the data - http://www.fao.org/fishery/statistics/global-production/en, downloaded Feb 13, 2018 and
# stored as data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1.zip`

{
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}

fishcomposites <- c("c_Opelagic", "c_Milsc", "c_Odmsrl", "c_Opelag", "c_Crust", "c_Omarn", "c_FreshD")
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


# sum production over source types here
formula.wide <- "UNI_code + Area + Source + Species  ~ Year"
dt.prod.wide <- data.table::dcast(
  data = dt.prod,
  formula = formula.wide,
  value.var = "Quantity")

dt.prod.wide[, yearAve := rowMeans(.SD), .SDcols = keepListYears.composites]
dt.prod.wide[, (keepListYears.composites) := NULL]

#aggregate by source

formula.wide <- "UNI_code + Area + Species  ~ Source"
dt.prod.wide <- data.table::dcast(
  data = dt.prod.wide,
  formula = formula.wide,
  value.var = "yearAve")

sourceTypes <- c("1", "2", "3", "4")
for (j in sourceTypes)
  set(dt.prod.wide,which(is.na(dt.prod.wide[[j]])),j,0)
dt.prod.wide[, sourceTot := rowSums(.SD), .SDcols = (sourceTypes)]
dt.prod.wide[, (sourceTypes) := NULL]

#aggregate by area
areaUnits <- fread("data-raw/FAOSTAT/FishStatData/GlobalProuction_2017.1.1/CL_FI_AREA_GROUPS.csv")
formula.wide <- "UNI_code + Species  ~ Area"
dt.prod.wide <- data.table::dcast(
  data = dt.prod.wide,
  formula = formula.wide,
  value.var = "sourceTot")

areaTypes <- unique(areaUnits$Code)
areaTypes <- areaTypes[!areaTypes %in% "08"] # 8 is Antarctica
for (j in areaTypes)
  set(dt.prod.wide,which(is.na(dt.prod.wide[[j]])),j,0)
dt.prod.wide[, prodAve := rowSums(.SD), .SDcols = (areaTypes)]
dt.prod.wide[, (areaTypes) := NULL]

dt.regions.all <- getNewestVersion("dt.regions.all")
keepListCol <- c("region_code.IMPACT159", "FAOSTAT_code", "UNI_code", "country_name.ISO")
dt.regions <- dt.regions.all[,(keepListCol), with = FALSE][, UNI_code := as.character(UNI_code)]

#add IMPACT code regions
fishprod <- merge(dt.prod.wide, dt.regions, by = "UNI_code", all.y = TRUE)
fishprod <- fishprod[!is.na(Species)]
deleteListCol <- c("UNI_code", "FAOSTAT_code", "country_name.ISO")
fishprod[, (deleteListCol) := NULL]

#' aggregate smaller countries to their IMPACT159 regions
data.table::setkeyv(fishprod, c("region_code.IMPACT159"))
fishprod[, prodAveNew := sum(prodAve), by = eval(data.table::key(fishprod))]
fishprod[, prodAve := NULL]
fishprod <- unique(fishprod)

fishprod <- merge(fishprod, CL_FI_SPECIES_GROUPS, by.x = "Species", by.y = "3Alpha_Code", all.x = TRUE)

dt.compositesLU <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/NutrientData/nutrientDetails/composites.fish.Lookup.xlsx"))
dt.compositesLU[, usda_code := as.character(usda_code)]
dt.compositesLU <- dt.compositesLU[composite %in% fishcomposites]
test <- merge(dt.compositesLU, fishprod, by.x = "item_name", by.y = "Name_en")
test <- test[!remove %in% "1",]
deleteListCol <- c("include", "remove", "Taxonomic_Code", "Major_Group", "ISSCAAP_Group",  "CPC_Group" , "CPCdiv_Group", "item_code", "Notes", "ratio_prod_live")
test[, (deleteListCol) := NULL]
test <- unique(test)
setnames(test, old = c("Species", "prodAveNew"), new = c("item_code", "prodAve"))
inDT <- test
outName <- "dt.fishStatData"
cleanup(inDT, outName, fileloc("iData"))
