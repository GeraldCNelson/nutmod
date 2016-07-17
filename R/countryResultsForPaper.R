# code to produce country specific output for the paper
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

ctyList <- c("NIC", "BRA", "CHM", "ETH", "IND", "GHA","TZA", "FRP", "VNM", "USA", "ZAF")
yearList <- c("X2010", "X2050")
scenarioList <- c("SSP2-NoCC", "SSP2-HGEM","SSP2-HGEM-HiNARS2", "SSP2-HGEM-HiREFF2",
                  "SSP2-HGEM-HiYld2", "SSP2-HGEM-IRREXP_WUE2", "SSP2-HGEM-IRREXP2",
                  "SSP2-HGEM-LoYld2", "SSP2-HGEM-SWHC2")
dt.regions.all <- getNewestVersion("dt.regions.all")

useCookingRetnValues <- keyVariable("useCookingRetnValues")
fixFish <- keyVariable("fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
dt.nutrients <- cookingRetFishCorrect(useCookingRetnValues, fixFish)
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resData"))
dt.shannonDiversity <- getNewestVersion("dt.shannonDiversity", fileloc("resData"))
# keep data just for the countries in ctyList, years in yearList and scenarios in scenarioList
dt.IMPACTfood <- dt.IMPACTfood[region_code.IMPACT159 %in% ctyList & scenario %in% scenarioList &
                                 year %in% yearList,]
dt.shannonDiversity <- dt.shannonDiversity[region_code.IMPACT159 %in% ctyList & scenario %in% scenarioList &
                                 year %in% yearList,]

keepListCol <-  c("scenario", "region_code.IMPACT159", "year", "pcGDPX0", "budget.PCX0", "incSharePCX0")
dt.budgetShare <- dt.budgetShare[region_code.IMPACT159 %in% ctyList & scenario %in% scenarioList &
                                   year %in% yearList,keepListCol, with = FALSE]
dt.regions.all <- unique(dt.regions.all[region_code.IMPACT159 %in% ctyList,c("region_code.IMPACT159", "region_name.IMPACT159"), with = FALSE])
dt.regions.all <- dt.regions.all[,region_name.IMPACT159 := gsub(" plus", "", dt.regions.all$region_name.IMPACT159)]
temp <- merge(dt.budgetShare, dt.regions.all, by = "region_code.IMPACT159")
