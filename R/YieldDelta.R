#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords final graphs
#' @title Calculate final graph combinations for the nutrient modeling paper
#' @name aggRun.R
#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
#' @include aggNorder.R
#' @description This code calculates yield deltas over time between 2010 and 2030 and
#' with and without productivity investments in 2030

#Copyright (C) 2015-2017 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

source("R/nutrientModFunctions.R")
source("R/YieldDelta.R")

dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
dt.regions.ISO_A3 <- as.data.table(dt.regions.all[,ISO_code])
setnames(dt.regions.ISO_A3, old = "V1", new = "ISO_code")
cropList <- c("cbana", "cbarl", "cbean", "ccass", "cchkp", "ccowp", "cgrnd",
              "clent", "cmaiz", "cmill", "copul", "cpigp", "cplnt", "cpota",
              "crice", "csorg", "csoyb", "cswpt", "cwhea", "cyams")
cropjList <- gsub("^.", "j", cropList)
cropNames <- c("Banana", "Barley", "Beans", "Cassava", "Chickpea", "Cowpea", "Groundnuts",
               "Lentils", "Maize", "Millet", "Other pulses", "Pigeonpea", "Plantain", "Potato",
               "Rice", "Sorghum", "Soybean", "Sweet potato", "Wheat", "Yam")
cropCategory <- c("Roots, Tubers & Bananas", "Cereal Grains", "Oilseeds & Pulses", "Roots, Tubers & Bananas", "Oilseeds & Pulses", "Oilseeds & Pulses", "Oilseeds & Pulses",
                  "Oilseeds & Pulses", "Cereal Grains", "Cereal Grains", "Oilseeds & Pulses", "Oilseeds & Pulses", "Roots, Tubers & Bananas", "Roots, Tubers & Bananas",
                  "Cereal Grains", "Cereal Grains", "Oilseeds & Pulses", "Roots, Tubers & Bananas", "Cereal Grains", "Roots, Tubers & Bananas")
cropInfo <- data.table(cropList = cropList, cropNames = cropNames, cropCategory = cropCategory)


dt.YLDCTYX0 <- getNewestVersion("dt.YLDCTYX0", fileloc("iData"))
dt.YLDCTYX0 <- dt.YLDCTYX0[IMPACT_code %in% cropjList]
dt.yldRnfd <- dt.YLDCTYX0[year %in% c("X2010", "X2030") & !landUse %in% c("air", "gir")]
dt.yldIrr <- dt.YLDCTYX0[year %in% c("X2010", "X2030") & !landUse %in% c("arf", "gir")]

scenarioList <- c("SSP2-HGEM-cf", paste0("SSP2-HGEM-", cropList))
scenarioList.crops <- scenarioList[!scenarioList %in% "SSP2-HGEM-cf"]
cropRatios <- paste(scenarioList.crops, "ratio", sep = ".")
beginningCols <- c("region_code.IMPACT159", "year")

worldMap <- getNewestVersion("worldMap", fileloc("uData")) # run storeWorldMapDF() if this is not available


# work on facetmaps for 2030 yields by crop after investment
DTorig <- copy(dt.yldRnfd)
DTorig <- DTorig[year %in% "X2030",]
facetColName <- "landUse"
legendText <- "Rainfed Yield, 2030 (mt/ha)"

for (j in cropList) {
  DT <- copy(DTorig)
  scenario.j <- paste0("SSP2-HGEM-", j)
  crop.j <- gsub("^.", "j", j)
  DT <- DT[scenario %in% scenario.j & IMPACT_code %in% crop.j]
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  DT <- merge(DT, dt.regions.ISO_A3, by.x = "region_code.IMPACT159", by.y = "ISO_code", all.y = TRUE)
  setnames(DT, old = c("region_code.IMPACT159", "YLDCTYX0"), new = c("id", "value"))
  DT[, scenario := scenario.j][, year := "X2030"][, IMPACT_code := crop.j]
  DT[,landUse := paste("Rainfed Yield, 2030, ", j)]

  fillLimits <- c(0, round(max(DT$value, na.rm = TRUE)))

  DT <- truncateDT(DT, fillLimits =  fillLimits)
  paletteType <- "Spectral"
  breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 1)
  # breakValues <- scales::rescale(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high))
  myPalette <- colorRampPalette(brewer.pal(11, paletteType))
  palette <- myPalette(4)
  graphsListHolder <- list()
  displayOrder <- sort(unique(DT[,get(facetColName)]))
  fileName <- paste("facetmap", "_yield_", j,  "_", "2030", sep = "")
  desc <- paste("Rainfed yield in 2030 for ", j)
  facetMaps(worldMap, DTfacetMap = DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
}
