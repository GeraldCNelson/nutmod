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
library(gridExtra)
library(gplots)

regionAgg <- function(aggChoice) {
  # region info setup for aggregating -----
  dt.regions.all <- getNewestVersion("dt.regions.all")
  I3regions <- sort(unique(dt.regions.all$region_code.IMPACT159))
  tenregions <- sort(c("NIC", "BRA", "CHM", "ETH", "IND", "GHA","TZA", "FRP", "VNM", "USA"))
  AggReg1 <- sort(unique(dt.regions.all$region_code.AggReg1))
  AggReg2 <- sort(unique(dt.regions.all$region_code.AggReg2))
  twoEconGroup <- sort(unique(dt.regions.all$region_code.EconGroup))
  WB <- sort(unique(dt.regions.all$region_code.WB))
  regionNamestenregions <- unique(dt.regions.all[region_code.IMPACT159 %in% tenregions, region_name.IMPACT159])
  regionNamesAggReg1 <- unique(dt.regions.all$region_name.AggReg1)
  regionNamesAggReg2 <- unique(dt.regions.all$region_name.AggReg2)
  regionNamestwoEconGroup <- unique(dt.regions.all$region_name.EconGroup)
  regionNamesWB <- unique(dt.regions.all$region_name.WB)

  # regionCodestenregions
  if (aggChoice == "tenregions") {
    keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
    dt.regions.all <- dt.regions.all[region_code.IMPACT159 %in% tenregions,]
    dt.regions.all <- dt.regions.all[, region_code := region_code.IMPACT159]
  }
  # regionCodesI3regions
  if (aggChoice == "I3regions") {
    keepListCol <- c("region_code.IMPACT159", "region_code", "region_name.IMPACT159")
    dt.regions.all <- dt.regions.all[, region_code := region_code.IMPACT159]
  }
  # regionCodesAggReg1
  if (aggChoice == "AggReg1") {
    keepListCol <- c("region_code.IMPACT159", "region_code.AggReg1", "region_name.AggReg1")
  }
  # regionCodesAggReg2
  if (aggChoice == "AggReg2") {
    keepListCol <- c("region_code.IMPACT159", "region_code.AggReg2", "region_name.AggReg2")
  }
  # regionCodestwoEconGroup
  if (aggChoice == "twoEconGroup") {
    keepListCol <- c("region_code.IMPACT159", "region_code.EconGroup", "region_name.EconGroup")
  }
  # regionCodesWB
  if (aggChoice == "WB") {
    keepListCol <- c("region_code.IMPACT159", "region_code.WB", "region_name.WB")
  }
  dt.regions <- unique(dt.regions.all[, (keepListCol), with = FALSE])
  data.table::setnames(dt.regions, old = keepListCol, new = c("region_code.IMPACT159", "region_code", "region_name"))
  return(dt.regions)
}

orderRegions <- function(DT,aggChoice) {
  # order by regions
  if (aggChoice == "WB") {
    regionOrder <- c("lowInc","lowMidInc", "upMidInc","highInc")
    DT[, regionOrder := match(region_code, regionOrder)]
    data.table::setorder(DT, regionOrder)
    DT[, regionOrder := NULL]
  }
  if (aggChoice == "156") {
    # percap GDP data for ordering
    dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
    dt.pcGDPX0 <- dt.pcGDPX0[scenario %in% eval(parse(text = (scenChoice)))[!eval(parse(text = (scenChoice))) %in% "X2010"],]
    # dt.pcGDPX0 <- unique(dt.pcGDPX0[,c("IMPACT_code","FoodAvailability","PCX0","PWX0","CSE") :=  NULL])
    dt.pcGDPX0 <- dt.pcGDPX0[year %in% c("X2010","X2050"), ]
    #data.table::setkeyv(dt.GDP,c("region_code.IMPACT159"))
    dt.pcGDPX0 <- dt.pcGDPX0[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                             .SDcols = "pcGDPX0", by = c("scenario","region_code.IMPACT159")]
    dt.pcGDPX0growth <- dt.pcGDPX0[year ==  "X2010" & scenario == scenario.base,][,c("scenario","pcGDPX0","year") :=  NULL]
    dt.pcGDPX0.2010.ref <- dt.pcGDPX0[year ==  "X2010" & scenario == scenario.base,][,c("scenario","growthRate","year") :=  NULL]
    data.table::setorder(DT, pcGDPX0) # for data tables ordered from low to high 2010 per cap GDP
    DT <- merge(dt.GDP.2010.ref, DT, by = c("region_code.IMPACT159"))
    data.table::setorder(DT, pcGDPX0)
  }
  if (aggChoice == "I3regions") {
    DT <- DT[region_code %in% regionCodestenregions,]
    # percap GDP data for ordering
    data.table::setorder(DT, aggChoice)
  }
  return(DT)
}

aggNorder <- function(gdxChoice, DTglobal, aggChoice, scenChoice) {
  DT <- getNewestVersion(DTglobal, fileloc("resultsDir"))
  setkey(DT, NULL)
  dt.regions <- regionAgg(aggChoice)
  # aggregate to and retain only the relevant regions
  temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
  merged <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
  # merged <- merged[region_code.IMPACT159 %in% region_code, ]
  # deal with the budget data
  if ("incSharePCX0" %in% names(merged)) {
    keepListCol.incShare <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0", "PopX0")
    keepListCol.pcGDP <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "pcGDPX0", "PopX0")
    dt.incShare <- merged[, (keepListCol.incShare), with = FALSE]
    dt.pcGDP <- merged[, (keepListCol.pcGDP), with = FALSE]
    data.table::setnames(dt.incShare, old = "incSharePCX0", new = "value")
    data.table::setnames(dt.pcGDP, old = "pcGDPX0", new = "value")
    merged <- dt.incShare
  }
  # deal with the ShannonDiversity data
  if ("SDnorm" %in% names(merged)) {
    keepListCol.SD <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "SDnorm", "PopX0")
    merged <- merged[, (keepListCol.SD), with = FALSE]
    data.table::setnames(merged, old = "SDnorm", new = "value")
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
    DT <- renameUSAIDscenarios(DT)
    scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
    scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
    scenarioList.addEnhance <- c("RPHL", "RMM")
    scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
    # keep only needed USAID scenarios
    scenOrder.USAID <- c("2010", scenChoice)
    DT <- DT[scenario %in% scenOrder.USAID, ] # only needed for the USAID results
    # order scenarios, first write the number into the number variable scenarioOrder
    DT[, scenarioOrder := match(scenario, scenOrder.USAID)]
    data.table::setorder(DT, scenarioOrder)
    DT[, scenarioOrder := NULL]
  }
  if (gdxChoice == "SSPs") {
    # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
    scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
    DT <- DT[scenario %in% scenOrder.SSPs, ] # doesn't need eval-parse because the list is defined inside the function

  # order by scenarios
  DT[, scenarioOrder := match(scenario, scenOrder.SSPs)]
  data.table::setorder(DT, scenarioOrder)
  DT[, scenarioOrder := NULL]
  }
  # order by regions
  DT <- orderRegions(DT, aggChoice)
  DT <- DT[, region_name := gsub(" plus", "", region_name)]
  return(DT)
}

plotByRegionBar <- function(dt, fileName, title, yLab, yRange,aggChoice) {
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  if (gdxChoice == "SSPs") colList <- c("black", "red", "red2", "red4", "green", "green2", "green4")
  if (gdxChoice == "USAID") colList <- c("black", rainbow(10)[1:length(scenarios) - 1])
  legendText <- unique(gsub("-REF", "", scenarios))
  #  formula.wide <- "scenario ~ region_code"
  #the use of factor and levels keeps the order of the regions in region_code
  formula.wide <- "scenario ~ factor(region_code, levels=unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, scenarios)]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  #temp.out <- data.table::copy(temp.wide)
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
  temp.wide[, scenario := gsub("-REF", "", scenario)]
  temp <- as.data.frame(temp.wide)
  rownames(temp) <- temp[,1]
  temp[1] = NULL
  temp <- data.matrix(temp)
  # print(temp.wide)
  #  par(mai=c(2,0.82,0.82,0.42))
  pdf(paste("graphics/", fileName,".", aggChoice, ".pdf", sep = ""))
  #layout(matrix(c(1,2)), c(1,1), c(1,3))
  #par(mfrow = c(2,1), mai = c(1,1,1,1))
  mat = matrix(c(1,2))
  layout(mat, heights = c(6,3))
  barplot(temp,  col = colList, ylim = yRange,
          legend.text = rownames(temp), args.legend = list(cex = .5, x = "topright"),
          beside = TRUE, ylab = yLab,  cex.names = .7, las = 2,  main = title)
  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
  dev.off()
  write.csv(temp.wide, file = paste("graphics/", fileName, ".", aggChoice, ".csv", sep = ""))
}

