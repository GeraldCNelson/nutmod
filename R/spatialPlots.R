library(sp)
library(rworldmap)
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
  source("R/aggNorder.R")}
library(data.table)

variablesToPlot <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore","dt.compQI", "dt.compDI","dt.nonStapleKcalShare")

colToPlot <- "value"
for (i in variablesToPlot) {
  print(paste("working on ", i))
  dt.spatialPlotData <- getNewestVersion(i, fileloc("resultsDir"))
  if (i %in% "dt.budgetShare") {setnames(dt.spatialPlotData, old = "incSharePCX0", new = "value");
    mainTitle <- "Budget share (percent)"
    # get rid of Somalia because it's budget share is 500 * per cap income
    dt.spatialPlotData <- dt.spatialPlotData[!region_code.IMPACT159 == "SOM",]
  }
  if (i %in% "dt.RAOqe") {mainTitle <- "Rao's Quadratic Entropy"}
  if (i %in% "dt.nutBalScore") {mainTitle <- "Nutrient Balance Score"}
  if (i %in% "dt.compQI") {mainTitle <- "Composite Qualifying Index"}
  if (i %in% "dt.compDI") {mainTitle <- "Composite Disqualifying Index"}
  if (i %in% "dt.nonStapleKcalShare") {mainTitle <- "Non-staple Share of Kilocalories (percent)"}
  # country code cleanup
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "FRP", region_code.IMPACT159 := "FRA"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "CHM", region_code.IMPACT159 := "CHN"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "CHP", region_code.IMPACT159 := "CHE"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "DNP", region_code.IMPACT159 := "DNK"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "FNP", region_code.IMPACT159 := "FIN"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "ITP", region_code.IMPACT159 := "ITA"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "MOR", region_code.IMPACT159 := "MAR"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "SPP", region_code.IMPACT159 := "ESP"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "UKP", region_code.IMPACT159 := "GBR"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "BLX", region_code.IMPACT159 := "BEL"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "SDP", region_code.IMPACT159 := "SDN"]
  dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "RAP", region_code.IMPACT159 := "ARE"]

  dt.spatialPlotData <- dt.spatialPlotData[year %in% c("X2010", "X2050"),]
  dt.spatialPlotData <- dt.spatialPlotData[year == "X2010", scenario := "2010"][, year := NULL]

  # order by scenarios
  if ("SSP3-NoCC-REF" %in% scenlist) {
    # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
    scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
    dt.spatialPlotData[, scenarioOrder := match(scenario, scenOrder.SSPs)]
    data.table::setorder(dt.spatialPlotData, scenarioOrder)
    dt.spatialPlotData[, scenarioOrder := NULL]
  }
  # get the new order
  scenlist <- unique(dt.spatialPlotData$scenario)
  pdf(paste("graphics/map",i, ".pdf", sep = ""))
   par(mai=c(0,0,0.2,0),oma = c(1,1,4,1),xaxs="i",yaxs="i", mfrow = c(4,2))
  for (k in scenOrder.SSPs) {
    temp <- as.data.frame(dt.spatialPlotData[scenario %in% k,])
    #  mTitle <- paste0("\nModified Functional Attribute Diversity, \n", k)
    mTitle <- k
    sPDF <- joinCountryData2Map(temp, joinCode = "ISO3", nameJoinColumn = "region_code.IMPACT159",
                                suggestForFailedCodes = TRUE,
                                mapResolution = "coarse",  verbose = FALSE)
    mapCountryData(sPDF, nameColumnToPlot = colToPlot, colourPalette =  "diverging", numCats = 10,
                   missingCountryCol = "gray",catMethod = "fixedWidth",
                   mapTitle = mTitle, addLegend = TRUE)
  }
   mtext(mainTitle, outer = TRUE, cex = 1.5)
  dev.off()
}
