#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords yield data,
#' @title Process and graph IMPACT yield data
#' @name aggRun.R
#' @include nutrientModFunctions.R

#' @name dataManagementYields.R
#' @keywords DSSAT, yield, data
#' @description
#' This script generates yield graphs at the FPU level.

#Copyright (C) 2016 - 2018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

source("R/nutrientModFunctions.R")
library(RColorBrewer)
# next 3 libraries needed for world maps
library(sp)
library(broom)
library(rgdal)
library(readr)
library(rgeos)
sourceFile <- "dataManagementYields.R"
# gdxChoice values are SSPs, USAID,  USAIDPriorities, or AfricanAgFutures
createScriptMetaData()
graphsListHolder <- list()
fileNameHolder <- character(0)

doCountryAgg <- "no" # switch to turn off doing country level aggregations
allAfricaCodes <- keyVariable("allAfricaCodes")
dt.final <- getNewestVersion("yieldData", fileloc("mData"))
# convert yield data from kgs per ha to mt per ha
dt.final[, yield := yield/1000]
dt.final <- dt.final[yield == 0,  yield := NA]
# dt.complete <- dt.complete[is.na(value), value = 0] #should NA values be set to zero?
dt.final[, key := do.call(paste, c(.SD, sep = "_")), .SDcols = c("cropCode", "IRRF", "year", "co2_level")] # combine column elements
dt.final[, region_code.IMPACT159 := substring(FPU,5,7)]
dt.final <- dt.final[region_code.IMPACT159 %in% allAfricaCodes,]
dt.FPUlist <- as.data.table(dt.final$FPU) # used below to have all the FPUs included even if their data is NA
setnames(dt.FPUlist, old = "V1", new = "FPU")
# worldMap <- getNewestVersion("worldMap", fileloc("uData"))
# africaMap <- getNewestVersion("africaMap", fileloc("uData"))
# asiaMap <- getNewestVersion("asiaMap", fileloc("uData"))
# fpusMap <- getNewestVersion("fpusMap", fileloc("uData"))
# fpusMap_SSA <- getNewestVersion("fpusMap_SSA", fileloc("uData"))
fpusMap_allAfrica <- getNewestVersion("fpusMap_allAfrica", fileloc("uData"))

# fpusMap@data$region <- rownames(fpusMap@data)
# fpusMap@data$USAFPU <- NULL # probably not necessary to do this.
# fpusMap <- spTransform(fpusMap, CRS("+proj=wintri")) # see http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot
# fpusMap <- gBuffer(fpusMap, byid=TRUE, width=0)
# fpusMap <- broom::tidy(fpusMap, region = "FPU2015") # causes errors because of binding character and factor vector, but I don't know where the factor vector is

# # get the grid for a world map
# gratDir <- paste0(getwd(), "/data-raw/data-gis/ne_110m_graticules_all")
# grat <- readOGR(gratDir, layer="ne_110m_graticules_15") 
# grat_df <- broom::tidy(grat)
# 
# # get the bounding box for a world map
# bbox <- readOGR(gratDir, layer="ne_110m_wgs84_bounding_box") 
# bbox_df<- broom::tidy(bbox)
# 
# ggplot(bbox_df, aes(long,lat, group=group)) + 
#   geom_polygon(fill="white") +
#   geom_polygon(data=fpusMap, aes(long,lat, group=group, fill=hole)) + 
#   geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
#   labs(title="World map (Robinson)") + 
#   coord_equal() + 
#   theme_opts +
#   scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend
cropChoice <- sort(unique(dt.final$cropCode))
for (i in cropChoice) {
  for (j in c("RF", "IR")) {
    dt.complete <- copy(dt.final)
    dt.complete <- dt.complete[cropCode %in% i & IRRF %in% j, ]
    dt.complete <- unique(dt.complete)
    if (!doCountryAgg %in% "no") {
      # do aggregation to country
      dt <- copy(dt.complete)
      dt <- countryCodeCleanup(dt)
      #dt[, yield.cty := sum(yield), by = c("region_code.IMPACT159", "IRRF")]
      dt[, value := weighted.mean(yield, prod), by = c("region_code.IMPACT159", "IRRF", "year", "co2_level")]
      dt <- dt[, c("FPU", "yield" , "prod", "area", "year", "co2_level") := NULL]
      dt <- unique(dt)
      fillLimits <- c(round(min(dt$value, na.rm = TRUE)), ceiling(max(dt$value, na.rm = TRUE)))
      cat("fillLimits: ", fillLimits)
      # paletteType <- "Greys"
      paletteType <- "Spectral"
      myPalette <- colorRampPalette(brewer.pal(9, paletteType)) # 9 is max for the Greys palleteType
      palette <- myPalette(4)
      facetColName <- "key"
      displayOrder <- sort(unique(dt[, get(facetColName)])) # default - alphabetically sorted
      legendText <- "Yields (mt/ha)"
      setnames(dt, old = "region_code.IMPACT159", new = "id")
      dt <- dt[value %in% "NaN", value := NA]
      fileName <- paste("yieldFacetMaps/", gdxChoice, "_",  "facetmapWorld", "_", i, sep = "")
      
      g <- facetMaps(mapFile = worldMap, DTfacetMap = dt, fileName, legendText, fillLimits, palette, facetColName,graphsListHolder = graphsListHolder, displayOrder)
      print(g)
    }
    
    # do by fpus
    dt <- copy(dt.complete)
    dt[, value := yield]
    dt[, id := FPU]
    dt[, region_code.IMPACT159 := substring(FPU,5,7)]
    dt <- dt[region_code.IMPACT159 %in% allAfricaCodes, ]
    fillLimits <- c(round(min(dt$value, na.rm = TRUE)), ceiling(max(dt$value, na.rm = TRUE)))
    cat("fillLimits: ", fillLimits, "\n")
    # paletteType <- "Greys"
    # myPalette <- colorRampPalette(brewer.pal(9, paletteType)) # 9 is max for the Greys palleteType
    paletteType <- "Spectral"
    myPalette <- colorRampPalette(brewer.pal(11, paletteType)) 
    palette <- myPalette(4)
    facetColName <- "key"
    legendText <- "Yields (mt/ha)"
    displayOrder <- sort(unique(dt[,get(facetColName)]))
    dt <- dt[value %in% "NaN", value := NA]
    fileName <- paste("yieldFacetMaps/", gdxChoice, "_",  "facetmapFPUAfrica", "_", i, "_", j, sep = "")
    g <- facetMaps(mapFile = fpusMap_allAfrica, DTfacetMap = dt, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)
    print(g)
  }
}

dt.final <- unique(dt.final)
dt.final[, key := do.call(paste, c(.SD, sep = "_")), .SDcols = c("cropCode", "IRRF", "year", "co2_level")] # combine column elements
dt.final <- dt.final[!prod == 0 ,]
formula.wide <- sprintf("FPU + cropCode + IRRF + prod   ~ %s", "key")
dt <- dcast(data = dt.final, formula = formula.wide, value.var = "yield")
dt[, region_code.IMPACT159 := substring(FPU,5,7)]
dt <- dt[region_code.IMPACT159 %in% allAfricaCodes, ]
idVars <- c("FPU", "region_code.IMPACT159", "cropCode", "IRRF")

deleteListCol <- names(dt)[!names(dt) %in% idVars]

# CO2Concentration <- c("379", "421", "478", "443", "487", "539", "541", "670", "936")

CO2_1 <- "379"
CO2_2 <- "541"
CO2_2 <- "936"

year1 <- "2005"
year2 <- "2050"
year3 <- "2080"
waterChoice <- c("RF", "IR")
CO2 <- CO2_1
for (crop in cropChoice) {
  for  (water in waterChoice) {
    temp <- copy(dt.final)
    temp <- temp[cropCode %in% crop & IRRF %in% water,]
    klc1 <- paste(crop, water, year1, CO2,  sep = "_")
    klc2 <- paste(crop, water, year2, CO2,  sep = "_")
    klc3 <- paste(crop, water, year3, CO2,  sep = "_")
    keepListCol <- c("FPU", klc1, klc2, klc3)
    temp <- temp[key %in% keepListCol,]
    temp[, setdiff(names(temp), keepListCol) := NULL]
    temp <- unique(temp)
    colName.1 <- paste(crop, water, year1, CO2, sep = "_")
    colName.2 <- paste(crop, water, year2, CO2, sep = "_")
    colName.3 <- paste(crop, water, year3, CO2, sep = "_")
    deltaName.1 <- paste("delta", crop, water, CO2_1, year2, year1, sep = "_")
    deltaName.2 <- paste("delta2", crop, water, CO2_1, year3, year2, sep = "_")
    deltaName.3 <- paste("delta3", crop, water, CO2_1, year2, year1, sep = "_")
    # dt[, (deltaName.1) := 100 * (get(colName.2) - get(colName.1))/get(colName.1)]
    # if (!crop %in% "pota") dt[, (deltaName.2) := 100 * (get(colName.3) - get(colName.2))/get(colName.2)]
    
     f.ratio <- function(x, y) (100 * (y - x)/x)
     dt[, (deltaName.3) := Map(f.ratio,  mget(colName.1), mget(colName.2))]

  }
}

# keepListCol <- c("FPU", "cropCode", "IRRF", "whea_RF_2005_379" , "whea_RF_2050_379", "delta3_whea_RF_379_2050_2005", "delta_whea_RF_379_2050_2005")
# test <- copy(dt)
# test[, setdiff(names(test), keepListCol) := NULL]


dt[, (deleteListCol) := NULL]
idVars <- c("FPU", "region_code.IMPACT159", "cropCode", "IRRF")
measureVars <- names(dt)[!names(dt) %in% idVars]
dt.delta.long  <- data.table::melt(
  data = dt,
  id.vars = idVars,
  measure.vars = measureVars,
  variable.name = "delta",
  value.name = "value",
  variable.factor = FALSE
)
for (crop in cropChoice) {
  temp <- copy(dt.delta.long)
  temp <- temp[cropCode %in% crop &!is.na(value), ]
  temp[, deltaWater := tstrsplit(delta, "_", fixed = TRUE, keep = c(3))]
  temp <- temp[IRRF == deltaWater, ]
  temp[, id := FPU]
  
  fillLimits <- c(round(min(temp$value, na.rm = TRUE)), ceiling(max(temp$value, na.rm = TRUE)))
  cat("crop: ", crop, "\n")
  cat("fillLimits: ", fillLimits, "\n")
  # paletteType <- "Greys"
  # myPalette <- colorRampPalette(brewer.pal(9, paletteType)) # 9 is max for the Greys palleteType
  paletteType <- "Spectral"
  myPalette <- colorRampPalette(brewer.pal(11, paletteType)) 
  palette <- myPalette(4)
  facetColName <- "delta"
  legendText <- "Change in yields (percent)"
  displayOrder <- sort(unique(temp[,get(facetColName)]))
  fileName <- paste("yieldFacetMaps/", gdxChoice, "_",  "Delta", "_", crop, "_", sep = "")
  cat(fileName)
  p <- facetMaps(mapFile = fpusMap_allAfrica, DTfacetMap = temp, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder = graphsListHolder, displayOrder)
  print(p)
}
# save graph files for future use
inDT <- graphsListHolder
outName <- paste("graphsListHolder", sep = ".")
desc <- paste0("File with graphics created for yield facetmaps presentation", " nutrient data" )
cleanupGraphFiles(inDT, outName, paste0(fileloc("gDir"), "/yieldFacetMaps"), desc = desc)

