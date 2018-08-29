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
library(RColorBrewer)
source("R/nutrientModFunctions.R")
sourceFile <- "YieldDelta.R"
createScriptMetaData()

gdxChoice <- getGdxChoice()
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
cropInfo <- data.table(cropList = cropList, cropjList = cropjList, cropNames = cropNames, cropCategory = cropCategory)

dt.YLDCTYX0 <- getNewestVersion("dt.YLDCTYX0", fileloc("iData"))
dt.YLDCTYX0 <- dt.YLDCTYX0[IMPACT_code %in% cropjList]
# dt.yldRnfd <- dt.YLDCTYX0[year %in% c("X2015", "X2030") & !landUse %in% c("air", "gir")] # gir is an unused variable related to grassland irrigated
# dt.yldIrr <- dt.YLDCTYX0[year %in% c("X2015", "X2030") & !landUse %in% c("arf", "gir")]

dt.yldRnfd <- dt.YLDCTYX0[year %in% c("X2030") & !landUse %in% c("air", "gir")] # gir is an unused variable related to grassland irrigated
dt.yldIrr <- dt.YLDCTYX0[year %in% c("X2030") & !landUse %in% c("arf", "gir")]

formula.wide <- sprintf("region_code.IMPACT159 + year + IMPACT_code ~ %s", "scenario")
valueVar = "YLDCTYX0"
# add delta for rainfed
dt.yldRnfd.wide <- dcast(data = dt.yldRnfd, formula = formula.wide, value.var = valueVar)
#dt.yldRnfd.wide[, base := `SSP2-HGEM-cf`]
prodCols <- names(dt.yldRnfd.wide)[!names(dt.yldRnfd.wide) %in% c("region_code.IMPACT159", "year", "IMPACT_code", "SSP2-HGEM-cf")]
prodCols.delta <- paste0(prodCols, ".delta")
dt.yldRnfd.wide[, (prodCols.delta) := lapply(.SD, `-`, `SSP2-HGEM-cf`), .SDcols = (prodCols)]
dt.yldRnfd.wide[, (prodCols.delta) := lapply(.SD, `/`, `SSP2-HGEM-cf`/100), .SDcols = (prodCols.delta)]

idVars <- c("region_code.IMPACT159", "IMPACT_code", "year")
measureVars <- names(dt.yldRnfd.wide)[!names(dt.yldRnfd.wide) %in% idVars]
dt.yldRnfd <- melt(
  data = dt.yldRnfd.wide,
  id.vars = idVars,
  measure.vars = measureVars,
  variable.name = "scenario",
  value.name = "value",
  variable.factor = FALSE
)
                   
# add delta for irrigated
dt.yldIrr.wide <- dcast(data = dt.yldIrr, formula = formula.wide, value.var = valueVar)
#dt.yldIrr.wide[, base := `SSP2-HGEM-cf`]
prodCols <- names(dt.yldIrr.wide)[!names(dt.yldIrr.wide) %in% c("region_code.IMPACT159", "year", "IMPACT_code", "SSP2-HGEM-cf")]
prodCols.delta <- paste0(prodCols, ".delta")
dt.yldIrr.wide[, (prodCols.delta) := lapply(.SD, `-`, `SSP2-HGEM-cf`), .SDcols = (prodCols)]
dt.yldIrr.wide[, (prodCols.delta) := lapply(.SD, `/`, `SSP2-HGEM-cf`/100), .SDcols = (prodCols.delta)]
measureVars <- names(dt.yldIrr.wide)[!names(dt.yldIrr.wide) %in% idVars]
dt.yldIrr <- melt(
  data = dt.yldIrr.wide,
  id.vars = idVars,
  measure.vars = measureVars,
  variable.name = "scenario",
  value.name = "value",
  variable.factor = FALSE
)

inDT <- dt.yldRnfd
outname <- "dt.yldRnfd.var"
desc <- "rainfed yield data for productivity increase project"
cleanup(inDT, outname, fileloc("gDir"), desc = desc)

inDT <- dt.yldIrr
outname <- "dt.yldIrr.var"
desc <- "irrigated yield data for productivity increase project"
cleanup(inDT, outname, fileloc("gDir"), desc = desc)

# 
# scenarioList <- c("SSP2-HGEM-cf", paste0("SSP2-HGEM-", cropList))
# scenarioList.crops <- scenarioList[!scenarioList %in% "SSP2-HGEM-cf"]
# cropRatios <- paste(scenarioList.crops, "ratio", sep = ".")
# beginningCols <- c("region_code.IMPACT159", "year")
# 
# worldMap <- getNewestVersion("worldMap", fileloc("uData")) # run storeWorldMapDF() if this is not available
# 
# # work on facetmaps for 2030 yields by crop after investment
# graphsListHolder <- list()
# facetColName <- "landUse" # - Needed for the use of the facetmap technology. It's the label for each facet map. In this script there is only one facet.
# for (k in c("dt.yldRnfd", "dt.yldIrr")){
#   DTorig <- copy(eval(parse(text = k)))
#   DTorig[, year := NULL]
#   if (k %in% "dt.yldRnfd") {
#     legendText <- "Rainfed Yield, 2030 (mt/ha)"
#     fileelement <- "yld_rnfd_"
#   }
#   if (k %in% "dt.yldIrr") {
#     legendText <- "Irrigated Yield, 2030 (mt/ha)"
#     fileelement <- "yld_irr_"
#   }
#   for (i in 1:length(cropInfo$cropList)) {
#     j <- cropInfo$cropList[i]
#     jadj <- substring(j, 2) # get rid of c in front of crop name
#  #   DT <- copy(DTorig)
#     scenario.j <- paste0("SSP2-HGEM-", j)
#     crop.j <- gsub("^.", "j", j)
#     DT <- DTorig[scenario %in% scenario.j & IMPACT_code %in% crop.j]
#     DT.base <- DTorig[scenario %in% "SSP2-HGEM-cf" & IMPACT_code %in% crop.j]
#     DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
#     DT.base <- countryCodeCleanup(DT.base) # converts IMPACT region codes to ISO3 codes for largest country in the region
#     DT <- merge(DT, dt.regions.ISO_A3, by.x = "region_code.IMPACT159", by.y = "ISO_code", all.y = TRUE)
#     DT.base <- merge(DT.base, dt.regions.ISO_A3, by.x = "region_code.IMPACT159", by.y = "ISO_code", all.y = TRUE)
#     setnames(DT, old = c("region_code.IMPACT159", "YLDCTYX0"), new = c("id", "value"))
#     setnames(DT.base, old = c("region_code.IMPACT159", "YLDCTYX0"), new = c("id", "value"))
#     DT[, scenario := scenario.j][, IMPACT_code := crop.j]
#     DT.base[, scenario := scenario.j][, IMPACT_code := crop.j]
#     DT[,landUse := paste(cropInfo$cropNames[i], ", ", legendText, ", w Productivity Increase", sep = "")]
#     DT.base[,landUse := paste(cropInfo$cropNames[i], ", ", legendText, ", base", sep = "")]
# 
#     fillLimits <- c(0, round(max(DT$value, na.rm = TRUE)))
# 
#  #   DT <- truncateDT(DT, fillLimits =  fillLimits)
#     paletteType <- "Spectral"
#     breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 1)
#     myPalette <- colorRampPalette(brewer.pal(11, paletteType))
#     palette <- myPalette(4)
#     displayOrder <- sort(unique(DT[,get(facetColName)]))
#     fileName <- paste("facetmap", fileelement, jadj,  "_", "2030", sep = "")
#     fileName.base <- paste("facetmap", fileelement, "base_", jadj,  "_", "2030", sep = "")
#     desc <- paste(legendText, cropInfo$cropNames[i])
#     DT[, c("scenario", "IMPACT_code") := NULL]
#     facetMaps(worldMap, DTfacetMap = DT, fileName = fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
#     facetMaps(worldMap, DTfacetMap = DT.base, fileName = fileName.base, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
#   }
# }
# inDT <- graphsListHolder
# outName <- "graphsListHolder.yields"
# desc <- "2030 yields of crops for USAID priorities project"
# cleanupGraphFiles(inDT, outName, fileloc("gDir"), desc = desc)
