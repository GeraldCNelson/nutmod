#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2016 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.
#' @description A script to hold functions used for graphing in aggRun.R.

source("R/nutrientModFunctions.R")
  source("R/renameUSAIDscenarios.R")
# GDP setup -----
library(gridExtra)
library(gplots)

# get gdxChoice
gdxChoice <- getGdxChoice()

library(sp)
#library(rworldmap)
library(maps)
library(mapdata)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
# library(jsonlite)
# library(RCurl)
# library(gpclib)
library(broom)
library(grid)
library(gridExtra)
#library(ggthemes)

# needed for maps of nutrient availability
dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))

# naturalearth world map geojson
#world <- readOGR(dsn="https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
#world <- readOGR(dsn = "data-raw/spatialData/ne_50m_admin_0_countries.geojson", layer = "OGRGeoJSON")
world <- rgdal::readOGR(dsn = "data-raw/spatialData/ne_110m_admin_0_countries.geojson", layer = "OGRGeoJSON")

# remove antarctica and some other small countries
world <- world[!world$iso_a3 %in% c("ATA"),]
othersToRemove <- c("ABW", "AIA", "ALA", "AND", "ASM", "AFT")
world <- world[!world$iso_a3 %in% othersToRemove,]
world <- world[!world$type %in% "Dependency",]

world <-sp::spTransform(world, CRS("+proj=longlat"))

#world.simp <- gSimplify(world, tol = .1, topologyPreserve = TRUE)
#wintri
# alternative would be CRS("+proj=longlat")) for WGS 84
# dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
# pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)

worldMap <- broom::tidy(world, region = "iso_a3")
#worldMap.simp <- broom::tidy(world.simp, region = "iso_a3")

#variablesToPlot.single <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore","dt.compQI", "dt.compDI","dt.KcalShare.nonstaple")
# variablesToPlot.single <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore", "dt.compDI","dt.KcalShare.nonstaple")
#variablesToPlot.mult <- c("dt.KcalShare.foodgroup", "dt.foodAvail.foodGroup", "dt.nutrients.sum.all")
variablesToPlot.mult <- c("dt.KcalShare.foodgroup", "dt.foodAvail.foodGroup")

# generateWorldMaps -----
# code to generate choropleth world maps. In principle it should be able to handle an arbitrary number of scenarios
generateWorldMaps <- function(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName){
  scenGraphs <- list()
  for (j in 1:length(scenOrder)) {
    #    titletext <- paste0(titleText, scenOrder[j])
    titletext <- NULL
    temp.sp <- spData[scenario %in% scenOrder[j],]
    #    temp.sp[,scenario := NULL]
    temp.sp <- as.data.frame(temp.sp)
    summary(temp.sp)
    plotName.new <- paste0("plot.", gsub("-", "_", scenOrder[j]))
    print(plotName.new)
    gg <- ggplot(temp.sp, aes(map_id = id))
    gg <- gg + geom_map(aes(fill = temp.sp$value), map = worldMap, color = "white")
    gg <- gg + expand_limits(x = worldMap$long, y = worldMap$lat)
    gg <- gg + labs(title =  titletext, hjust = 0.5, x = NULL, y = NULL) +
      theme(plot.title = element_text(size = 10, hjust = 0.5)) +
      scale_fill_gradient(low = lowColor, high = highColor, guide = "legend", name = legendText, limits = fillLimits) +
      labs(lText = legendText) +
      #  theme(legend.position = "bottom") +
      theme(legend.justification = c(0,0), legend.position = c(0,0)) +
      # guides(lText = guide_legend(title.position="top", title.hjust = 0.5))  +
      theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
    scenGraphs[[plotName.new]] <- gg
  }
  # multiplot(plotlist = scenGraphs, cols = 2)
  
  # good source of information on using grid to place graphics - https://stat.ethz.ch/R-manual/R-devel/library/grid/doc/grid.pdf
  
  # code below is modified from multiplot
  cols <- 2
  numPlots <- length(scenGraphs)
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols), byrow = TRUE)
  grid.newpage()
  # +1 is for the title
  rows <- nrow(layout) + 1
  gridHeight <- unit(rep_len(1, rows), "null")
  pushViewport(viewport(layout = grid.layout(rows, ncol(layout), widths = unit(rep_len(1, cols), "null"), heights = unit(c(1, 5,5,5), "null"))))
  # title goes in the first row and across all columns
  grid.text(titleText, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:cols))
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    # Get the i,j matrix positions of the regions that contain this subplot
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    pdf(paste(fileloc("gDir"), "/worldMaps", fileName, ".pdf", sep = ""), width = 7, height = 5.2, useDingbats = FALSE)
    
    print(scenGraphs[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                         layout.pos.col = matchidx$col))
    dev.off()
  }
}

# for (i in c(variablesToPlot.single, variablesToPlot.mult)) {
for (i in c(variablesToPlot.mult)) {
  print(paste0("working on ", i))
  dt.spatialPlotData <- getNewestVersion(i, fileloc("resultsDir"))

  dt.spatialPlotData <- dt.spatialPlotData[year %in% c("X2010", "X2050"),]
  #note the use of scenario in i is because there are slight differences in the 2010 values for some variables in some scenarios. This code means only of them is chosen
  dt.spatialPlotData <- dt.spatialPlotData[year == "X2010" & scenario %in% "SSP2-NoCC-REF", `:=`(
    scenario = "2010", year = "2010")]
  dt.spatialPlotData <- dt.spatialPlotData[!year %in% "X2010",]
  dt.spatialPlotData[, year := NULL]
  # get the new order
  scenlist <- unique(dt.spatialPlotData$scenario)

  # order by scenarios
  if ("SSP3-NoCC-REF" %in% scenlist) {
    # do manipulations on the micronutrient modeling gdx data that has 3 SSP scenarios and 3 climate change scenarios, but just use 1 climate scenario.
    #    scenOrder.SSPs <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF", "SSP2-IPSL-REF", "SSP2-HGEM-REF")
    scenOrder <- c("2010", "SSP2-NoCC-REF", "SSP2-HGEM-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF")
    dt.spatialPlotData[, scenarioOrder := match(scenario, scenOrder)]
    data.table::setorder(dt.spatialPlotData, scenarioOrder)
    dt.spatialPlotData[, scenarioOrder := NULL]
  } else {
    stop("SSP3-NoCC-REF is not in scenlist")
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
  # dt.spatialPlotData <- dt.spatialPlotData[!region_code.IMPACT159 == "SOM",] #get rid of Somalia data.
  data.table::setnames(dt.spatialPlotData, old = "region_code.IMPACT159", new = "id")

  if (i %in% "dt.budgetShare") {
    dt.spatialPlotData[, c("pcGDPX0", "budget.PWX0", "budget.PCX0", "incShare.PWX0") := NULL]
    setnames(dt.spatialPlotData, old = "incShare.PCX0", new = "value")
    dt.spatialPlotData <- unique(dt.spatialPlotData)
    titleText <- "IMPACT food budget share of per capita income"
    legendText <- "Percent"
    lowColor <- "green"
    highColor <- "dark red"
    fillLimits <- c(0, 40)
    fileName <- "budgetShare"
    spData <- dt.spatialPlotData
    generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
  }
  if (i %in% "dt.RAOqe") {
    titleText <- "Rao's Quadratic Entropy"
    legendText <- "Legend"
    lowColor <- "dark red"
    highColor <- "green"
    fillLimits <- c(0, 90)
    fileName <- "RAOqe"
    spData <- dt.spatialPlotData
    generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
  }
  if (i %in% "dt.nutBalScore") {
    titleText <- "Nutrient Balance Score"
    legendText <- "Legend"
    lowColor <- "red"
    highColor <- "green"
    fillLimits <- c(0, 90)
    fileName <- "NBS"
    spData <- dt.spatialPlotData
    generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
  }
  if (i %in% "dt.compQI") {
    titleText <- "Composite Qualifying Index"
    legendText <- "Legend"
    lowColor <- "red"
    highColor <- "green"
    fillLimits <- c(0, 20)
    fileName <- "compQI"
    spData <- dt.spatialPlotData
    generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
  }
  if (i %in% "dt.compDI") {
    titleText <- "Composite Disqualifying Index"
    legendText <- "Legend"
    lowColor <- "green"
    highColor <- "dark red"
    fillLimits <- c(0, 90)
    fileName <- "compDI"
    spData <- dt.spatialPlotData
    generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
  }
  if (i %in% "dt.KcalShare.nonstaple") {
    titleText <- "Non-staple Share of Kilocalories"
    legendText <- "(Percent)"
    lowColor <- "red"
    highColor <- "green"
    fillLimits <- c(0, 90)
    fileName <- "KcalShare.nonstaple"
    spData <- dt.spatialPlotData
    generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
  }
  if (i %in% "dt.KcalShare.foodgroup") {
    titleText <- "Foodgroup Share of Kilocalories"
    legendText <- "(Percent)"
    lowColor <- "white"
    highColor <- "dark red"
    fillLimits <- c(0, 90)
    for (l in unique(dt.spatialPlotData$food_group_code)) {
      fileName <- paste0("KcalShare.foodgroup.", l)
      spData <- dt.spatialPlotData[food_group_code %in% l,]
      generateWorldMaps(spData, scenOrder, titleText, legendText, lowColor, highColor, fillLimits, fileName)
    }
  }

  if (i %in% "dt.foodAvail.foodGroup") {
    legendText <- "Grams"
    lowColor <- "white"
    highColor <- "dark red"
    fillLimits <- c(0, 1500)
    scenToPlot <- "SSP2-NoCC-REF"
    for (l in unique(dt.spatialPlotData$food_group_code)) {
      titleText <- paste("Daily per capita availability in 2050 for food group", l, "for ", sep = " ")
      spData <- dt.spatialPlotData[food_group_code %in% l & scenario %in% scenToPlot,]
#      if (l %in% "rootsNPlantain")  fillLimits <- c(0, 2000)
#      fillLimits[2] <- round(max(spData$value))
      fileName <- paste0("foodAvail.foodgroup.", l)
      generateWorldMaps(spData = spData, scenOrder = scenToPlot, titleText = titleText, legendText = legendText, lowColor = lowColor, highColor = highColor,
                        fillLimits = fillLimits, fileName = fileName)
    }
  }
  if (i %in% "dt.nutrients.sum.all") {
    titleText <- "Daily per capita nutrient availability of"
    legendText <- "Legend"
    lowColor <- "white"
    highColor <- "dark red"
    fillLimits <- c(0, 700)
    for (l in unique(dt.spatialPlotData$nutrient)) {
      fileName <- paste0("nutrientAvail.", l)
      spData <- dt.spatialPlotData[nutrient %in% l,]
      fillLimits[2] <- round(max(spData$value))
      nutlongName <- dt.nutrientNames_Units[1, (l)]
      legendText <- dt.nutrientNames_Units[2, (l)]
      titleText <- paste(titleText, nutlongName)
      generateWorldMaps(spData = spData, scenOrder = scenOrder, titleText = titleText, legendText = legendText, lowColor = lowColor, highColor = highColor,
                        fillLimits = fillLimits, fileName = fileName)
    }
  }
}


# old code -----
#variablesToPlot <- c("dt.budgetShare", "dt.RAOqe", "dt.nutBalScore","dt.compQI", "dt.compDI","dt.KcalShare.nonstaple", "dt.KcalShare.foodgroup", "dt.foodAvail.foodGroup")

# colToPlot <- "value"
# for (i in variablesToPlot) {
#   print(paste("working on ", i))
#   dt.spatialPlotData <- getNewestVersion(i, fileloc("resultsDir"))
#   scenlist <- unique(dt.spatialPlotData$scenario)
#   if (i %in% "dt.budgetShare") {setnames(dt.spatialPlotData, old = "incShare.PCX0", new = "value");
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

