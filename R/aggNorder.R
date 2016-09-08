#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name delta.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
# GDP setup -----
library(data.table)
scenario.base <- "SSP2-NoCC-REF"
# population for weighting -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
dt.pop.2010.ref <- dt.pop[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]

aggNorder <- function(gdxChoice, DTglobal, aggChoice, scenChoice) {
  DT <- getNewestVersion(DTglobal, fileloc("resultsDir"))
  setkey(DT, NULL)
  # regions for aggregating -----
  dt.regions.all <- getNewestVersion("dt.regions.all")
  regionCodes156 <- sort(unique(dt.regions.all$region_code.IMPACT159))
  regionCodes11 <- sort(c("NIC", "BRA", "CHM", "ETH", "IND", "GHA","TZA", "FRP", "VNM", "USA", "ZAF"))
  regionCodesAggReg1 <- sort(unique(dt.regions.all$region_code.AggReg1))
  regionCodesAggReg2 <- sort(unique(dt.regions.all$region_code.AggReg2))
  regionCodes2EconGroup <- sort(unique(dt.regions.all$region_code.EconGroup))
  regionCodesWB <- sort(unique(dt.regions.all$region_code.WB))
  regionNames11 <- unique(dt.regions.all[region_code.IMPACT159 %in% regionCodes11, region_name.IMPACT159])
  regionNamesAggReg1 <- unique(dt.regions.all$region_name.AggReg1)
  regionNamesAggReg2 <- unique(dt.regions.all$region_name.AggReg2)
  regionNames2EconGroup <- unique(dt.regions.all$region_name.EconGroup)
  regionNamesWB <- unique(dt.regions.all$region_name.WB)

  temp <- data.table::copy(dt.regions.all)

  # regionCodes11
  if (aggChoice == "regionCodes11") {
    keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
    temp <- temp[region_code.IMPACT159 %in% regionCodes11,]
    temp <- temp[, region_code := region_code.IMPACT159]
  }
  # regionCodes156
  if (aggChoice == "regionCodes156") {
    keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
    temp <- temp[, region_code := region_code.IMPACT159]
  }
  # regionCodesAggReg1
  if (aggChoice == "regionCodesAggReg1") {
    keepListCol <- c("region_code.IMPACT159", "region_code.AggReg1", "region_name.AggReg1")
  }
  # regionCodesAggReg2
  if (aggChoice == "regionCodesAggReg2") {
    keepListCol <- c("region_code.IMPACT159", "region_code.AggReg2", "region_name.AggReg2")
  }
  # regionCodes2EconGroup
  if (aggChoice == "regionCodes2EconGroup") {
    keepListCol <- c("region_code.IMPACT159", "region_code.EconGroup", "region_name.EconGroup")
  }
  # regionCodesWB
  if (aggChoice == "regionCodesWB") {
    keepListCol <- c("region_code.IMPACT159", "region_code.WB", "region_name.WB")
  }
  dt.regions <- unique(temp[, (keepListCol), with = FALSE])
  data.table::setnames(dt.regions, old = keepListCol, new = c("region_code.IMPACT159", "region_code", "region_name"))
  # dt.regions <- unique(temp[region_code.IMPACT159 %in% get(aggChoice), keepListCol, with = FALSE ])

  # aggregate to and retain only the relevant regions
  temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
  merged <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
  # merged <- merged[region_code.IMPACT159 %in% region_code, ]
  # deal with the budget data
  if ("incSharePWX0" %in% names(merged)) {
    keepListCol <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incSharePWX0", "PopX0")
    merged <- merged[, (keepListCol), with = FALSE]
    setnames(merged, old = "incSharePWX0", new = "value")
  }

  #  temp <- temp[, region.budget.share := mean(value), by = c("region_code", "year")]
  merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
  keepListCol <- c("scenario",  "year", "region_code", "region_name", "value")
  DT <- unique(merged[, (keepListCol), with = FALSE])
  #keep just the scenario.base scenario for 2010 and rename the scenario to 2010, then delete year column
  DT <- DT[year == "X2010" & scenario == scenario.base |
             year == "X2050",]
  DT <- DT[year == "X2010", scenario := "2010"][, year := NULL]

  # order of scenario and regions
  if (gdxChoice == "USAID") {
    #rename USAID scenarios
    DT[scenario == "SSP2-NoCC", scenario := "REF_NoCC"]
    DT[scenario == "SSP2-HGEM2", scenario := "REF_HGEM"]
    DT[scenario == "SSP2-IPSL2", scenario := "REF_IPSL"]
    DT[scenario == "SSP2-HGEM-LoYld2", scenario := "MED"]
    DT[scenario == "SSP2-HGEM-RegYld2", scenario := "REGION"]
    DT[scenario == "SSP2-HGEM-HiYld2", scenario := "HIGH"]
    DT[scenario == "SSP2-HGEM-HiNARS2", scenario := "HIGH_NARS"]
    DT[scenario == "SSP2-HGEM-HiREFF2", scenario := "HIGH_RE"]
    DT[scenario == "SSP2-HGEM-IRREXP2", scenario := "IX"]
    DT[scenario == "SSP2-NoCC-IRREXP2", scenario := "IX_NoCC"]
    DT[scenario == "SSP2-IPSL-IRREXP2", scenario := "IX_IPSL"]
    DT[scenario == "SSP2-HGEM-IRREXP-WUE2", scenario := "IX_WUE"]
    DT[scenario == "SSP2-NoCC-IRREXP-WUE2", scenario := "IX_WUE_NoCC"]
    DT[scenario == "SSP2-IPSL-IRREXP-WUE2", scenario := "IX_WUE_IPSL"]
    DT[scenario == "SSP2-HGEM-SWHC2", scenario := "ISW"]
    DT[scenario == "SSP2-NoCC-SWHC2", scenario := "ISW_NoCC"]
    DT[scenario == "SSP2-IPSL-SWHC2", scenario := "ISW_IPSL"]
    DT[scenario == "SSP2-HGEM-PHL-DEV2", scenario := "RPHL"]
    DT[scenario == "SSP2-HGEM-MMEFF2", scenario := "RMM"]
    DT[scenario == "SSP2-HGEM-Pangloss2", scenario := "COMP"]
    DT[scenario == "SSP2-NoCC-Pangloss2", scenario := "COMP_NoCC"]
    DT[scenario == "SSP2-IPSL-Pangloss2", scenario := "COMP_IPSL"]
    scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
    scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
    scenarioList.addEnhance <- c("RPHL", "RMM")
    scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
    scenario.base <- "REF_HGEM"

    # keep only needed scenarios
    DT <- DT[scenario %in% scenChoice, ] # only needed for the USAID results

    # order scenarios, first write the number into the number variable scenarioOrder
    DT[, scenarioOrder := which(scenario == scenChoice)]
    # order by regions
    #     data.table::setorder(temp, pcGDPX0) for data tables ordered from low to high 2010 per cap GDP
    if (choice == "regionCodesWB") {
      merged[region_code == "lowInc" ,newOrder := 1] [region_code == "lowMidInc" ,newOrder := 2][region_code == "upMidInc" ,newOrder := 3]
      merged[region_code == "highInc" ,newOrder := 4]
      dt.regionList[region_code == "lowInc" ,newOrder := 1] [region_code == "lowMidInc" ,newOrder := 2][region_code == "upMidInc" ,newOrder := 3]
      dt.regionList[region_code == "highInc" ,newOrder := 4]
      data.table::setorder(dt.regionList,newOrder)
      dt.regionList[, newOrder := NULL]
    }

  } else {
    # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
    scenario.base <- "SSP2-NoCC-REF"
    scenario.SSP1 <- "SSP1-NoCC-REF"
    scenario.SSP3 <- "SSP3-NoCC-REF"
    scenario.ClimModel.GFDL <- "SSP2-GFDL-REF"
    scenario.ClimModel.IPSL <- "SSP2-IPSL-REF"
    scenario.ClimModel.HGEM <- "SSP2-HGEM-REF"

    # set up order of scenarios
    DT[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
    DT[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
  }
  return(DT)
}

