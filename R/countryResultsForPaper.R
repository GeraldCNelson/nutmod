# code to produce country specific output for the paper
ctyList <- c("ARG", "BRA", "ETH", "IND", "GHA","KEN", "FRP", "VNM", "USA", "ZAF")
yearList <- c("X2010", "X2050")
scenarioList <- c("SSP2-NoCC", "SSP2-HGEM","SSP2-HGEM-HiNARS2", "SSP2-HGEM-HiREFF2",
                  "SSP2-HGEM-HiYld2", "SSP2-HGEM-IRREXP-WUE2", "SSP2-HGEM-IRREXP2",
                  "SSP2-HGEM-LoYld2", "SSP2-HGEM-SWHC2")
dt.regions.all <- getNewestVersion("dt.regions.all")

useCookingRetnValues <- keyVariable("useCookingRetnValues")
fixFish <- keyVariable("fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
dt.nutrients <- cookingRetFishCorrect(useCookingRetnValues, fixFish)
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resData"))
# keep data just for the countries in ctyList and years in yearList
dt.IMPACTfood <- dt.IMPACTfood[region_code.IMPACT159 %in% ctyList &
                                 year %in% yearList,]
dt.budgetShare <- dt.budgetShare[region_code.IMPACT159 %in% ctyList &
                                   year %in% yearList,]
dt.regions.all <- unique(dt.regions.all[region_code.IMPACT159 %in% ctyList,c("region_code.IMPACT159", "region_name.IMPACT159"), with = FALSE])
