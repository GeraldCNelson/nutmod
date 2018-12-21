#' @title Food budget share calcs with and without CGE adjustments
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Copyright (C) 2018 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

#' @description Imports gdx data for SSP2 HGEM results with and without GLOBE CGE adjustments and
#' calculates the effects on the food budget share.
#' @name dataPrep.SingleScenario.R
#' @include nutrientModFunctions.R
# library(gdxrrw) Not needed because relevant data already extracted from the gdx. Dec 21, 2018
library(RColorBrewer)
source("R/nutrientModFunctions.R")
sourceFile <- "dataPrepSingleScenario.R"
createScriptMetaData()

graphsListHolder <- list()
singleScenario <- TRUE

fileLocation <- paste0(getwd(), "/",fileloc("iData"), "CGEPEcompare/")
dt.FoodAvailability.woGlobe <- readRDS(paste0(fileLocation, "dt.FoodAvailability.SSP2-HGEM-WithoutGLOBE"))
dt.FoodAvailability.wGlobe <- readRDS(paste0(fileLocation, "dt.FoodAvailability.SSP2-HGEM2-WithGLOBE"))
dt.pcGDPX0.woGlobe <- readRDS(paste0(fileLocation, "dt.pcGDPX0.SSP2-HGEM-WithoutGLOBE"))
dt.pcGDPX0.wGlobe <- readRDS(paste0(fileLocation, "dt.pcGDPX0.SSP2-HGEM2-WithGLOBE"))
dt.PCX0.woGlobe <- readRDS(paste0(fileLocation, "dt.PCX0.SSP2-HGEM-WithoutGLOBE"))
dt.PCX0.wGlobe <- readRDS(paste0(fileLocation, "dt.PCX0.SSP2-HGEM2-WithGLOBE"))

# setkey(dt.FoodAvailability.woGlobe)
# setkey(dt.FoodAvailability.wGlobe)
# setkey(dt.pcGDPX0.woGlobe)
# setkey(dt.pcGDPX0.wGlobe)
# setkey(dt.PCX0.woGlobe)
# setkey(dt.PCX0.wGlobe)

# first do wo Globe
dt_woGlobe <- merge(dt.FoodAvailability.woGlobe, dt.PCX0.woGlobe, by = c("region_code.IMPACT159", "year", "IMPACT_code"))
dt_woGlobe[, budget := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("region_code.IMPACT159", "year")]
dt_woGlobe[, c("IMPACT_code", "FoodAvailability", "PCX0"):= NULL]
dt_woGlobe <- unique(dt_woGlobe)
dt_woGlobe <- merge(dt_woGlobe, dt.pcGDPX0.woGlobe, by = c("region_code.IMPACT159", "year"))
dt_woGlobe[, incShare := 100 * budget / pcGDPX0 ]
namesToChange <- c("pcGDPX0", "budget", "incShare")
setnames(dt_woGlobe, old = namesToChange, new = paste0(namesToChange, "_woGlobe"))

# now with Globe
dt_wGlobe <- merge(dt.FoodAvailability.wGlobe, dt.PCX0.wGlobe, by = c("region_code.IMPACT159", "year", "IMPACT_code"))
dt_wGlobe[, budget := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("region_code.IMPACT159", "year")]
dt_wGlobe[, c("IMPACT_code", "FoodAvailability", "PCX0"):= NULL]
dt_wGlobe <- unique(dt_wGlobe)
dt_wGlobe <- merge(dt_wGlobe, dt.pcGDPX0.wGlobe, by = c("region_code.IMPACT159", "year"))
dt_wGlobe[, incShare := 100 * budget / pcGDPX0 ]
namesToChange <- c("pcGDPX0", "budget", "incShare")
setnames(dt_wGlobe, old = namesToChange, new = paste0(namesToChange, "_wGlobe"))
dt <- merge(dt_woGlobe, dt_wGlobe, by = c("region_code.IMPACT159", "year"))
# write.csv(dt, file = "data/IMPACTData/singleScenario/combinedResults.csv") commented out because not used elsewhere Dec 21, 2018

dt <- dt[!region_code.IMPACT159 %in% "SOM",]
dt.50 <- dt[year %in% "X2050",]
dt.50[, year := NULL]
dt.50 <- dt.50[!duplicated(region_code.IMPACT159),]
dt.50[, incShareRatio := 100 * (incShare_wGlobe - incShare_woGlobe)/incShare_woGlobe]
dt.50[, budgetRatio := 100 * (budget_wGlobe - budget_woGlobe)/budget_woGlobe]
dt.50[, incRatio := 100 * (pcGDPX0_wGlobe - pcGDPX0_woGlobe)/pcGDPX0_woGlobe]

#reorder the cols
setcolorder(dt.50, c("region_code.IMPACT159",
                     "pcGDPX0_woGlobe", "pcGDPX0_wGlobe",
                     "budget_woGlobe", "budget_wGlobe",
                     "incShare_woGlobe",  "incShare_wGlobe",
                     "incShareRatio", "budgetRatio", "incRatio"))

dt.50.summary <- as.data.table(summary(dt.50))
dt.50.summary[, V1 := NULL]
dt.50.summary <- dt.50.summary[!V2 %in% "region_code.IMPACT159", ]
sumMeasures <- c("type", "value")
dt.50.summary <- dt.50.summary[, (sumMeasures) := data.table::tstrsplit(N, ":", fixed = TRUE)]
dt.50.summary[, N := NULL]

formula.wide <- "type ~ V2"
dt.50.summary.wide <- data.table::dcast(
  data = dt.50.summary,
  formula = formula.wide,
  value.var = "value")

# some names have acquired unwanted spaces. Fix this
setnames(dt.50.summary.wide, old = names(dt.50.summary.wide), new = gsub(" ", "", names(dt.50.summary.wide)))

#reorder the cols
setcolorder(dt.50.summary.wide, c("type", "pcGDPX0_woGlobe", "pcGDPX0_wGlobe",
                                  "budget_woGlobe",   "budget_wGlobe",
                                  "incShare_woGlobe",  "incShare_wGlobe",
                                  "incRatio", "budgetRatio", "incShareRatio"))

#reorder the rows
dt.50.summary.wide <- dt.50.summary.wide[c(6,1,4,5,2,3), ]

sumStats <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb = sumStats, sheetName = "stats")
openxlsx::writeData(
  wb = sumStats, sheet = "stats", dt.50.summary.wide, rowNames = FALSE, colNames = TRUE, startCol = 1)
openxlsx::saveWorkbook(wb = sumStats, file = paste(fileloc("resultsDir"), "compareCGEPEStats.xlsx", sep = "/"), overwrite = TRUE)
# low-income removal
# noSom <- temp2[!region_code.IMPACT159 %in% c("SOM", "BDI", "LBR", "CAF", "NER", "RWA"),]

# facet maps of deltas due to use of Globe
cat("\n Working on facet maps of deltas due to use of Globe\n")
worldMap <- getNewestVersion("worldMap", fileloc("uData"))

measureVars <- c("pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe", "budget_wGlobe",
                 "incShare_woGlobe", "incShare_wGlobe",
                 "budgetRatio", "incRatio", "incShareRatio")

dt.50.long <- data.table::melt(dt.50,
                               id.vars = "region_code.IMPACT159",
                               variable.name = "metric",
                               measure.vars = measureVars,
                               value.name = "value",
                               variable.factor = FALSE)

dt.50.long[, metric := gsub("X0", "", metric)]
for (j in c("base", "share")) {
  #  for (j in c( "share")) {
  DT <- copy(dt.50.long)
  if (j %in% "base"){
    DT <- DT[metric %in% c("pcGDP_woGlobe", "pcGDP_wGlobe", "budget_woGlobe", "budget_wGlobe",
                           "incShare_woGlobe", "incShare_wGlobe"), ]
    legendText <- "Macro metrics range"
    fillLimits <- c(0, 35)
  }
  if (j %in% "share") {
    DT <- DT[metric %in% c("budgetRatio", "incRatio", "incShareRatio"), ]
    DT[metric %in% c("budgetRatio"), metric := "Food budget effect"]
    DT[metric %in% c("incRatio"), metric := "Income effect"]
    DT[metric %in% c("incShareRatio"), metric := "Food budget share\n of income effect"]
    legendText <- "(percent)"
    fillLimits <- c(-10, 5)
  }
  DT <- truncateDT(DT, fillLimits)
  
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
  facetColName <- "metric"
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  palette <- myPalette(4)
   displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
  prefix <- "SSPs_scenOrderSSP"
  fileName <- paste(prefix,"_facetmap", "_macroMetrics", "_2050", ".pdf", sep = "")
  facetMaps(mapFile = worldMap, DTfacetMap = DT, fileName, legendText, fillLimits = fillLimits, 
            palette = palette, facetColName = facetColName, graphsListHolder = graphsListHolder, displayOrder = displayOrder, width = 7, height = 3)
}

gg <- ggplot(data = dt.50, aes(incRatio, incShareRatio))
gg <- gg + geom_point() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black", ), 
    panel.background = element_blank(),
    axis.line.y = element_line(colour = 'black', size=0.1, linetype='solid'),
    axis.line.x = element_line(colour = 'black', size=0.1, linetype='solid')
  ) 
gg <- gg + xlab("Change in\nper capita income (percent)") + ylab("Change in\nfood budget share of per capita income (percent)")
gg

createMissingDir(fileloc("gDir")) # checks for the existence of the directory and if its not available, creates it Dec 21, 2018
ggsave(file = paste0(fileloc("gDir"),"/", prefix,"_CGEeffects",".pdf"), plot = gg,
       width = 4, height = 4)

lmout <- lm(incShareRatio ~  incRatio, dt.50 )

finalizeScriptMetadata(metadataDT, sourceFile)

