library(gridExtra)
library(gplots)
library(RColorBrewer)

source("R/nutrientModFunctions.R")

dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
dt.IMPACTfood[ , `:=`( COUNT = .N ) , by = c("scenario", "year", "region_code.IMPACT159") ]
dt.IMPACTfood[, c("scenario", "year", "IMPACT_code", "PCX0", "PWX0", "pcGDPX0", "FoodAvailability", "foodAvailpDay") := NULL]
dt.IMPACTfood <- unique(dt.IMPACTfood)

dt.spatialPlotData <- dt.IMPACTfood
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

data.table::setnames(dt.spatialPlotData, old = c("region_code.IMPACT159", "COUNT"), new = c("id", "value"))

legendText <- "Count"
fillLimits <- c(20, 60)
titletext <- "Count of food item availability"
spData <- as.data.frame(dt.spatialPlotData)
gg <- ggplot(spData, aes(map_id = id))
gg <- gg + geom_map(aes(fill = spData$value), map = worldMap, color = "white")
gg <- gg + expand_limits(x = worldMap$long, y = worldMap$lat)
gg <- gg + labs(title =  titletext, hjust = 0.5, x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, hjust = 0.5)) +
  scale_fill_gradientn(colours=brewer.pal(5,"RdYlGn"), guide = "legend", name = legendText, limits = fillLimits) +
  labs(lText = legendText) +
  theme(legend.justification = c(0,0), legend.position = c(0,0)) +
  # guides(lText = guide_legend(title.position="top", title.hjust = 0.5))  +
  theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())


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
  pdf(paste(fileloc("gDir"), "/worldMap", fileName, "_", ".pdf", sep = ""), width = 7, height = 5.2, useDingbats = FALSE)

  print(scenGraphs[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                       layout.pos.col = matchidx$col))
  dev.off()
}

