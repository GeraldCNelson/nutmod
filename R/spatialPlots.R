library(sp)
library(rworldmap)
library(maps)
library(mapdata)
library(ggplot2)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(jsonlite)
library(RCurl)
library(gpclib)
library(broom)


{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
  source("R/aggNorder.R")}
library(data.table)

variablesToPlot <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore","dt.compQI", "dt.compDI","dt.KcalShare.nonstaple", "dt.KcalShare.foodgroup", "dt.foodAvail.foodGroup")

colToPlot <- "value"
for (i in variablesToPlot) {
  print(paste("working on ", i))
  dt.spatialPlotData <- getNewestVersion(i, fileloc("resultsDir"))
  scenlist <- unique(dt.spatialPlotData$scenario)
  if (i %in% "dt.budgetShare") {setnames(dt.spatialPlotData, old = "incSharePCX0", new = "value");
    mainTitle <- "Budget share (percent)"
    # get rid of Somalia because it's budget share is 500 * per cap income
    dt.spatialPlotData <- dt.spatialPlotData[!region_code.IMPACT159 == "SOM",]
  }
  if (i %in% "dt.RAOqe") {mainTitle <- "Rao's Quadratic Entropy"}
  if (i %in% "dt.nutBalScore") {mainTitle <- "Nutrient Balance Score"}
  if (i %in% "dt.compQI") {mainTitle <- "Composite Qualifying Index"}
  if (i %in% "dt.compDI") {mainTitle <- "Composite Disqualifying Index"}
  if (i %in% "dt.KcalShare.nonstaple") {mainTitle <- "Non-staple Share of Kilocalories (percent)"}
  if (i %in% "dt.KcalShare.foodgroup") {mainTitle <- "Daily per capita food availability by food group (grams)"}
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

# using ggplot
#worldMap <- map_data("world")
# worldMap <- map_data("world")
# ggplot() + geom_polygon(data = worldMap, aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3)

# for theme_map
theme_map <- function(base_size = 9, base_family="") {
  require(grid)
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0,0),
          legend.position  =  c(0,0)
    )
}

# naturalearth world map geojson
#world <- readOGR(dsn="https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
world <- readOGR(dsn = "data-raw/spatialData/ne_50m_admin_0_countries.geojson", layer = "OGRGeoJSON")


# remove antarctica
world <- world[!world$iso_a3 %in% c("ATA"),]

world <- spTransform(world, CRS("+proj=wintri"))
# alternative would be CRS("+proj=longlat")) for WGS 84
# dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
# pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)
#gpclibPermit()
worldMap <- broom::tidy(world, region = "iso_a3")

#gpclibPermit()# data frame of markers
# labs <- data.frame(lat = c(39.5, 35.50),
#                    lon = c(-98.35, 103.27),
#                    title = c("US", "China"))

# pre-project them to winkel-tripel
# coordinates(labs) <-  ~lon+lat
# c_labs <- as.data.frame(SpatialPointsDataFrame(spTransform(
#   SpatialPoints(labs, CRS("+proj=longlat")), CRS("+proj=wintri")),
#   labs@data))

dt.spatialPlotData <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
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

dt.spatialPlotData <- dt.spatialPlotData[year %in% c("X2050"),]
dt.spatialPlotData <- dt.spatialPlotData[year == "X2050", scenario := "2050"][, year := NULL]
dt.spatialPlotData <- dt.spatialPlotData[!region_code.IMPACT159 == "SOM",] #get rid of Somalia data.
data.table::setnames(dt.spatialPlotData, old = "region_code.IMPACT159", new = "id")

dt.spatialPlotData <- as.data.frame(dt.spatialPlotData)

ggplot(worldMap, aes(x = long, y = lat, group=group)) + geom_path() +

  geom_map(map = worldMap, aes(map_id = id, fill = incSharePWX0), color = "black")
#+ coord_map() - Note takes a lot of computation
+  expand_limits(x = worldMap$long, y = worldMap$lat)


gg <- ggplot(dt.spatialPlotData, aes(fill = "incSharePCX0"))
gg <- gg + geom_map( map = worldMap,
                   aes(map_id = id))
 #                   aes(map_id = id, group = group), color = NA)
gg <- gg + geom_map( map = worldMap, color = "white", size = 0.15,
                    aes(map_id = id, fill = incSharePCX0))
#gg <- gg + geom_point(data = c_labs, aes(x = lon, y = lat), size = 4)
gg <- gg + scale_fill_gradient(low = "#f7fcb9", high = "#31a354", name = "Budget Share (percent)")
gg <- gg + labs(title = "IMPACT food budget share of\n2050 per capita income")
gg <- gg + coord_equal(ratio = 1)
gg <- gg + theme_map()
gg <- gg + theme(legend.position = "bottom")
gg <- gg + theme(legend.key = element_blank())
gg <- gg + theme(plot.title=element_text(size = 16))
gg
