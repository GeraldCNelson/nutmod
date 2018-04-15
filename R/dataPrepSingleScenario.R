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
source("R/nutrientModFunctions.R")

library(gdxrrw)
library(RColorBrewer)
gdxrrw::igdx(gamsSysDir = fileNameList("R_GAMS_SYSDIR"), silent = TRUE)


# #gdxFileName <- "SSP2-HGEM2-WithGLOBE.gdx"
# gdxFileName <- "SSP2-HGEM-WithoutGLOBE.gdx"
singleScenario <- TRUE
fileDest <- "data/IMPACTData/singleScenario"

catList <- c("catNames.land", "catNames.commod", "catNames.region", "catNames.world")
vars.land <- c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "landUse", "year", "value")

vars.commods <-
  c("PCX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0", "PerCapKCAL_com", "FoodAvailability")
catNames.commod <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "year", "value")

vars.region <- c("GDPX0", "pcGDPX0", "TotalMalnourished",
                 "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")
catNames.region <- c("scenario", "region_code.IMPACT159", "year", "value")

vars.world <- "PWX0"
catNames.world <- c("scenario", "IMPACT_code", "year", "value")
keepYearList <- keyVariable("keepYearList")


#' Title generateResults - send a list of variables with common categories to the
#' function to write out the data
#' @param vars - list of variables to process
#' @param catNames - list of categories common to all variables in var
#'
#' @return
#' @export
generateResults <- function(gdxFileName, gdxFileLoc, vars, catNames, singleScenario, keepYearList){
  for (i in vars) {
    catNames <- catNames[!catNames %in% "scenario"]
    processIMPACT159Data(gdxFileName, gdxFileLoc, varName = i, catNames = catNames, singleScenario, keepYearList)
  }
}

#' Title processIMPACT159Data - read in from an IMPACT gdx file and write out rds and excel files for a single param
#' @param gdxFileName - name of the IMPACT gdx file
#' @param varName - name of the IMPACT parameter to write out
#' @param catNames - types of info about the parameter
#' @return null
#' @export
#'
processIMPACT159Data <- function(gdxFileName, gdxFileLoc, varName, catNames, singleScenario, keepYearList) {
  # dt.regions.all <- getNewestVersion("dt.regions.all")
  # IMPACTgdx <- gdxFileName
  dt <- data.table::as.data.table(gdxrrw::rgdx.param(gdxFileLoc, varName,
                                                     ts = TRUE, names = catNames))
  dt <- data.table::as.data.table(rapply(dt, as.character, classes = "factor", how = "replace"))

  if(!singleScenario == TRUE) {
    dt[scenario %in% c("SSP1-NoCC", "SSP2-GFDL", "SSP2-HGEM","SSP2-HGEM2", "SSP2-IPSL", "SSP2-IPSL2",
                       "SSP2-MIROC", "SSP2-NoCC", "SSP3-NoCC"),
       scenario := paste(scenario, "-REF", sep = "")]
  }

  # dt.temp <- dt.regions.all[,c("region_code.IMPACT159","region_name.IMPACT159"), with = FALSE]
  # data.table::setkey(dt.temp,region_code.IMPACT159)
  # dt.IMPACTregions <- unique(dt.temp)
  # if the data set contains SDN (the old Sudan) data, convert the code to SDP
  if (!varName %in% "PWX0") {
    # this kludge is here because the currently used gdx files have both SDN and SDP
    dt[region_code.IMPACT159 == "SDN", region_code.IMPACT159 := "SDP"]
  }
  dt[,year := paste("X",year, sep = "")]
  dt <- dt[year %in% keepYearList]
  #setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT159, year)
  data.table::setorderv(dt, cols = catNames)
  data.table::setnames(dt, "value", varName)
  # this if statement keeps the region code and name from being added since PW is only for the world
  # if (!varName == "PWX0") {
  # data.table::setkey(dt, region_code.IMPACT159)
  # dt.temp <-
  # merge(dt, dt.IMPACTregions, by = "region_code.IMPACT159", all = TRUE)
  # }
  inDT <- dt
  # this is where dt.FoodAvailability is written out, for example
  outName <- paste("dt", varName, gsub(".gdx", "", gdxFileName), sep = ".")
  if (singleScenario == TRUE) {
    desc <- paste0("gdx data for ", varName)
    cleanup(inDT,outName,fileDest, desc = desc)
  }else{
    desc <- paste0("gdx data for ", varName)
    cleanup(inDT,outName,fileloc("iData"), desc = desc)
  }
}

for (i in c("SSP2-HGEM2-WithGLOBE.gdx", "SSP2-HGEM-WithoutGLOBE.gdx")) {

  # comment out lines below to speed up data crunching.
  # generateResults(vars.land,catNames.land)
  gdxFileLoc <- paste(fileloc("IMPACTRawData"),i, sep = "/")
  generateResults(i, gdxFileLoc, vars = vars.commods, catNames = catNames.commod, singleScenario, keepYearList)
  generateResults(i, gdxFileLoc, vars = vars.region,  catNames = catNames.region, singleScenario, keepYearList)
  # generateResults(gdxFileLoc, vars.world, catNames.world)
}

dt.FoodAvailability.woGlobe <- getNewestVersion("dt.FoodAvailability.SSP2-HGEM-WithoutGLOBE", fileDest)
dt.FoodAvailability.wGlobe <- getNewestVersion("dt.FoodAvailability.SSP2-HGEM2-WithGLOBE", fileDest)
dt.pcGDPX0.woGlobe <- getNewestVersion("dt.pcGDPX0.SSP2-HGEM-WithoutGLOBE", fileDest)
dt.pcGDPX0.wGlobe <- getNewestVersion("dt.pcGDPX0.SSP2-HGEM2-WithGLOBE", fileDest)
dt.PCX0.woGlobe <- getNewestVersion("dt.PCX0.SSP2-HGEM-WithoutGLOBE", fileDest)
dt.PCX0.wGlobe <- getNewestVersion("dt.PCX0.SSP2-HGEM2-WithGLOBE", fileDest)

setkey(dt.FoodAvailability.woGlobe)
setkey(dt.FoodAvailability.wGlobe)
setkey(dt.pcGDPX0.woGlobe)
setkey(dt.pcGDPX0.wGlobe)
setkey(dt.PCX0.woGlobe)
setkey(dt.PCX0.wGlobe)

# first do wo Globe
dt_woGlobe <- merge( dt.FoodAvailability.woGlobe, dt.pcGDPX0.woGlobe, by = c("region_code.IMPACT159", "year"))
dt_woGlobe <- merge(dt_woGlobe, dt.PCX0.woGlobe, by = c("region_code.IMPACT159", "IMPACT_code", "year"))
dt_woGlobe[, budget := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("region_code.IMPACT159", "year")]
dt_woGlobe[, incShare := 100 * budget / pcGDPX0 ]
namesToChange <- c("FoodAvailability", "pcGDPX0", "PCX0", "budget", "incShare")
setnames(dt_woGlobe, old = namesToChange, new = paste0(namesToChange, "_woGlobe"))

# now with Globe
dt_wGlobe <- merge( dt.FoodAvailability.wGlobe, dt.pcGDPX0.wGlobe, by = c("region_code.IMPACT159", "year"))
dt_wGlobe <- merge(dt_wGlobe, dt.PCX0.wGlobe, by = c("region_code.IMPACT159", "IMPACT_code", "year"))
dt_wGlobe[, budget := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("region_code.IMPACT159", "year")]
dt_wGlobe[, incShare := 100 * budget / pcGDPX0 ]
namesToChange <- c("FoodAvailability", "pcGDPX0", "PCX0", "budget", "incShare")
setnames(dt_wGlobe, old = namesToChange, new = paste0(namesToChange, "_wGlobe"))

dt <- merge(dt_woGlobe, dt_wGlobe, by = c("region_code.IMPACT159", "IMPACT_code", "year"))
write.csv(dt, file = "data/IMPACTData/singleScenario/combinedResults.csv")

dt <- as.data.table(read.csv(file = "data/IMPACTData/singleScenario/combinedResults.csv", stringsAsFactors = FALSE))
dt[, X := NULL] # get rid of row numbers
dt <- dt[!region_code.IMPACT159 %in% "SOM",]
dt.50 <- dt[year %in% "X2050",]
dt.50[, c("IMPACT_code", "FoodAvailability_woGlobe", "FoodAvailability_wGlobe", "PCX0_woGlobe", "PCX0_wGlobe","year") := NULL]
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
openxlsx::saveWorkbook(wb = sumStats, file = paste(fileDest, "compareStats.xlsx", sep = "/"))
# low-income removal
# noSom <- temp2[!region_code.IMPACT159 %in% c("SOM", "BDI", "LBR", "CAF", "NER", "RWA"),]

# facet maps of deltas due to use of Globe
cat("\n Working on facet maps")
worldMap <- getNewestVersion("worldMap", fileloc("mData"))

measureVars <- c("pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe", "budget_wGlobe",
                 "incShare_woGlobe", "incShare_wGlobe",
                 "budgetRatio", "incRatio", "incShareRatio")

dt.50.long <- data.table::melt(dt.50,
                               id.vars = "region_code.IMPACT159",
                               variable.name = "metric",
                               measure.vars = measureVars,
                               value.name = "value",
                               variable.factor = FALSE)

dt.50.long.base <- copy(dt.50.long)
dt.50.long.share <- copy(dt.50.long)
#for (i in c("base", "share")) {
  for (i in c( "share")) {
    DT <- copy(dt.50.long)
  if (i %in% "base"){
    DT <- DT[metric %in% c("pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe", "budget_wGlobe",
                           "incShare_woGlobe", "incShare_wGlobe"), ]
    # setnames(DT, old = c("pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe", "budget_wGlobe",
    #                       "incShare_woGlobe", "incShare_wGlobe"),
    #           new = c("per capita income, w/o Globe", "per capita income, with Globe",
    #                   "food budget, w/o Globe", "food budget, with Globe",
    #                   "income share, w/o Globe", "income share, with Globe"))
    legendText <- "Macro metrics range"
    fillLimits <- c(0, 35)
  }
  if (i %in% "share"){
    DT <- DT[metric %in% c("budgetRatio", "incRatio", "incShareRatio"), ]
    DT[metric %in% c("budgetRatio"), metric := "Food budget effect"]
    DT[metric %in% c("incRatio"), metric := "Income effect"]
    DT[metric %in% c("incShareRatio"), metric := "Food budget share of income effect"]
    legendText <- "(percent)"
    fillLimits <- c(-5, 5)
  }
  DT <- countryCodeCleanup(DT) # converts IMPACT region codes to ISO3 codes for largest country in the region
  data.table::setnames(DT, old = "region_code.IMPACT159", new = "id")
  facetColName <- "metric"

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  palette <- myPalette(4)
  #' middle two values shift the palette gradient; the code below give a smooth change
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))
  displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
  fileName <- paste("facetmap", "macroMetrics", "2050", sep = "_")
  graphsListHolder <- list()
  facetMaps(worldMap, DT, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
  print(graphsListHolder)

  b <- breakValues
  f <- fillLimits
  p <- palette
  d <- DT
  #d[, (n) := factor(get(n), levels = displayOrder)]
  gg <- ggplot(data = d, aes(map_id = region_code.IMPACT159))
  gg <- gg + geom_map(aes(fill = incShareRatio), map = worldMap)
  # gg <- gg + expand_limits(x = worldMap$long, y = worldMap$lat)
  #gg <- gg + facet_wrap(facets = n)
  gg <- gg + theme(legend.position = "bottom")
  gg <- gg +  theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),
                    axis.text.y = element_blank(), strip.text = element_text(family = "Times", face = "plain"))
  gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                  na.value = "grey50", values = b,
                                  guide = "colorbar", limits = f)
  gg
  }

gg <- ggplot(data = dt.50, aes(incRatio, incShareRatio))
gg <- gg + geom_point()
gg <- gg + xlab("Change in per capita income (percent)") + ylab("Change in food budget share of per capita income (percent)")
gg

lmout <- lm(incShareRatio ~  incRatio, dt.50 )
