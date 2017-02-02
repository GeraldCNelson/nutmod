library(sp)
library(rworldmap)
library(maps)
library(mapdata)
library(ggplot2)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
# library(jsonlite)
# library(RCurl)
# library(gpclib)
library(broom)
library(data.table)
library(grid)
library(gridExtra)
#library(ggthemes)

{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

# naturalearth world map geojson
#world <- readOGR(dsn="https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
world <- readOGR(dsn = "data-raw/spatialData/ne_50m_admin_0_countries.geojson", layer = "OGRGeoJSON")

# remove antarctica
world <- world[!world$iso_a3 %in% c("ATA"),]

world <- spTransform(world, CRS("+proj=longlat"))
#wintri
# alternative would be CRS("+proj=longlat")) for WGS 84
# dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
# pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)

worldMap <- broom::tidy(world, region = "iso_a3")
variablesToPlot <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore","dt.compQI", "dt.compDI","dt.KcalShare.nonstaple", "dt.KcalShare.foodgroup", "dt.foodAvail.foodGroup")

for (i in variablesToPlot) {
  dt.spatialPlotData <- getNewestVersion(i, fileloc("resultsDir"))
  dt.spatialPlotData <- dt.spatialPlotData[year %in% c("X2010", "X2050"),]
  dt.spatialPlotData <- dt.spatialPlotData[year == "X2010", scenario := "2010"][, year := NULL]
  # get the new order
  scenlist <- unique(dt.spatialPlotData$scenario)

  # order by scenarios
  if ("SSP3-NoCC-REF" %in% scenlist) {
    # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios, but just use 1 climate scenario.
    #    scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
    scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP2-HGEM-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF")
    dt.spatialPlotData[, scenarioOrder := match(scenario, scenOrder.SSPs)]
    data.table::setorder(dt.spatialPlotData, scenarioOrder)
    dt.spatialPlotData[, scenarioOrder := NULL]
  }

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
  dt.spatialPlotData <- dt.spatialPlotData[!region_code.IMPACT159 == "SOM",] #get rid of Somalia data.
  data.table::setnames(dt.spatialPlotData, old = "region_code.IMPACT159", new = "id")

  if (i %in% "dt.budgetShare") {
    setnames(dt.spatialPlotData, old = "incSharePCX0", new = "value")
    dt.spatialPlotData[, c("pcGDPX0", "budget.PWX0", "budget.PCX0", "incSharePWX0") := NULL]
    dt.spatialPlotData <- unique(dt.spatialPlotData)
    titleText <- "IMPACT food budget share of per capita income"
    legendText <- "Percent"
  }
  if (i %in% "dt.RAOqe") {
    titleText <- "Rao's Quadratic Entropy"
    legendText <- "Legend"
  }
  if (i %in% "dt.nutBalScore") {
    titleText <- "Nutrient Balance Score"
    legendText <- "Legend"
  }
  if (i %in% "dt.compQI") {
    titleText <- "Composite Qualifying Index"
    legendText <- "Legend"
  }
  if (i %in% "dt.compDI") {
    titleText <- "Composite Disqualifying Index"
    legendText <- "Legend"
  }
  if (i %in% "dt.KcalShare.nonstaple") {
    titleText <- "Non-staple Share of Kilocalories (percent)"
    legendText <- "Legend"
  }
  if (i %in% "dt.KcalShare.foodgroup") {
    titleText <- "Daily per capita food availability by food group (grams)"
    legendText <- "Legend"
  }

  # pdf(paste("graphics/map",i, ".pdf", sep = ""))
  # par(mai = c(0,0,0.2,0), oma = c(1,1,4,1),xaxs="i",yaxs = "i", mfrow = c(4,2))
  scenGraphs <- vector("list", length(scenOrder.SSPs))
  for (j in 1:length(scenOrder.SSPs)) {
    k <- scenOrder.SSPs[j]
    temp.sp <- dt.spatialPlotData[scenario %in% k,]
    temp.sp <- as.data.frame(temp.sp)
    titleText.complete <- paste0(titleText,"\n", k,",", i)
    gg <- ggplot(temp.sp, aes(map_id = id)) +
      geom_map(aes(fill = temp.sp$value), map = worldMap, color = "white") +
      expand_limits(x = worldMap$long, y = worldMap$lat) +
      labs(title =  k, x = NULL, y = NULL) +
      theme(plot.title = element_text(size = 14, hjust = 0.5)) +
      scale_fill_gradient(low = "white", high = "dark red", guide = "legend", name = legendText, limits=c(0, 40)) +
      labs(lText = legendText) +
      #  theme(legend.position = "bottom") +
      theme(legend.justification = c(0,0), legend.position = c(0,0)) +
      # guides(lText = guide_legend(title.position="top", title.hjust = 0.5))  +
      theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
    # print(gg)
     plotName.new <- paste0("plot.", gsub("-", "_",k))
   assign(plotName.new, gg)
 #     scenGraphs[[j]] <- gg
   }
  multiplot(plotlist = scenGraphs, cols = 2)
  # mtext(titleText, outer = TRUE, cex = 1.5)
  # dev.off()
}



# old code -----
#variablesToPlot <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore","dt.compQI", "dt.compDI","dt.KcalShare.nonstaple", "dt.KcalShare.foodgroup", "dt.foodAvail.foodGroup")

# colToPlot <- "value"
# for (i in variablesToPlot) {
#   print(paste("working on ", i))
#   dt.spatialPlotData <- getNewestVersion(i, fileloc("resultsDir"))
#   scenlist <- unique(dt.spatialPlotData$scenario)
#   if (i %in% "dt.budgetShare") {setnames(dt.spatialPlotData, old = "incSharePCX0", new = "value");
#     mainTitle <- "Budget share (percent)"
#     # get rid of Somalia because it's budget share is 500 * per cap income
#     dt.spatialPlotData <- dt.spatialPlotData[!region_code.IMPACT159 == "SOM",]
#   }
#   if (i %in% "dt.RAOqe") {mainTitle <- "Rao's Quadratic Entropy"}
#   if (i %in% "dt.nutBalScore") {mainTitle <- "Nutrient Balance Score"}
#   if (i %in% "dt.compQI") {mainTitle <- "Composite Qualifying Index"}
#   if (i %in% "dt.compDI") {mainTitle <- "Composite Disqualifying Index"}
#   if (i %in% "dt.KcalShare.nonstaple") {mainTitle <- "Non-staple Share of Kilocalories (percent)"}
#   if (i %in% "dt.KcalShare.foodgroup") {mainTitle <- "Daily per capita food availability by food group (grams)"}
#   # country code cleanup
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "FRP", region_code.IMPACT159 := "FRA"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "CHM", region_code.IMPACT159 := "CHN"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "CHP", region_code.IMPACT159 := "CHE"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "DNP", region_code.IMPACT159 := "DNK"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "FNP", region_code.IMPACT159 := "FIN"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "ITP", region_code.IMPACT159 := "ITA"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "MOR", region_code.IMPACT159 := "MAR"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "SPP", region_code.IMPACT159 := "ESP"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "UKP", region_code.IMPACT159 := "GBR"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "BLX", region_code.IMPACT159 := "BEL"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "SDP", region_code.IMPACT159 := "SDN"]
#   dt.spatialPlotData <- dt.spatialPlotData[region_code.IMPACT159 %in% "RAP", region_code.IMPACT159 := "ARE"]
#
#   dt.spatialPlotData <- dt.spatialPlotData[year %in% c("X2010", "X2050"),]
#   dt.spatialPlotData <- dt.spatialPlotData[year == "X2010", scenario := "2010"][, year := NULL]
#
#   # order by scenarios
#   if ("SSP3-NoCC-REF" %in% scenlist) {
#     # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
#     #    scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
#     scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP2-HGEM-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF")
#     dt.spatialPlotData[, scenarioOrder := match(scenario, scenOrder.SSPs)]
#     data.table::setorder(dt.spatialPlotData, scenarioOrder)
#     dt.spatialPlotData[, scenarioOrder := NULL]
#   }
#   # get the new order
#   scenlist <- unique(dt.spatialPlotData$scenario)
#   pdf(paste("graphics/map",i, ".pdf", sep = ""))
#   par(mai=c(0,0,0.2,0), oma = c(1,1,4,1),xaxs="i",yaxs = "i", mfrow = c(3,2))
#   for (k in scenOrder.SSPs) {
#     temp <- as.data.frame(dt.spatialPlotData[scenario %in% k,])
#     #  mTitle <- paste0("\nModified Functional Attribute Diversity, \n", k)
#     mTitle <- k
#     sPDF <- joinCountryData2Map(temp, joinCode = "ISO3", nameJoinColumn = "region_code.IMPACT159",
#                                 suggestForFailedCodes = TRUE,
#                                 mapResolution = "coarse",  verbose = FALSE)
#     mapCountryData(sPDF, nameColumnToPlot = colToPlot, colourPalette =  "diverging", numCats = 10,
#                    missingCountryCol = "gray",catMethod = "fixedWidth",
#                    mapTitle = mTitle, addLegend = TRUE)
#   }
#   mtext(mainTitle, outer = TRUE, cex = 1.5)
#   dev.off()
# }

