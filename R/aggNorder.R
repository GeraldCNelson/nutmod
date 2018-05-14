#' Nutrient Modeling Functions for the shiny app
#' @title: "Functions used in aggRun.R and a few other scripts for aggregating and graphing
#' @name aggNorder.R
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, aggregate data, sort data
#' @description
#' This script contains functions that are needed primarily (or perhaps exclusively) in
#' the aggRun.R script. It has functions to aggregate data to arbitrary regions and to sort
#' data ordering to a prespecified order.

#Copyright (C) 2016, 2017 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

# GDP setup -----
library(gridExtra)
library(gplots)

source("R/renameUSAIDscenarios.R")

# get gdxChoice -----
gdxChoice <- getGdxChoice()

#' population data set used for weighting by population -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))

#' create legend grobs for use in multiple graph files -----
updateLegendGrobs <- function(l, aggChoice, legendLoc, mergedVals) {
  # use an arbitrary file to construct the grob. This code modified from aggRun.R
  DTglobal <- getNewestVersion("dt.budgetShare.base", fileloc("resultsDir"))
  DT <- aggNorder(gdxChoice, DTglobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars)
  ylab <- "(percent)"
  cat("\nCreating legend grob for ", aggChoice, ",", l, "and", legendLoc)
  regionCodes <- unique(DT$region_code)
  regionNames <- unique(DT$region_name)
  scenarios <- unique(DT$scenario)
  DT[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  if (aggChoice %in% "WB") regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  scenarioNameOrder <- scenOrder
  DT[, region_name := gsub(" income", "", region_name)]
  DT[, region_name := factor(region_name, levels =  regionNameOrder)]
  DT[, scenario := factor(scenario, levels = scenarioNameOrder)]
  if (gdxChoice %in% "USAID")  DT <- renameUSAIDscenarios(DT)

  #' draw bars to get the legend
  p <- ggplot(DT, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme(legend.position = legendLoc) +
    theme(legend.text = element_text(size = 8, family = "Times", face = "plain")) +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = colorList)
  return(g_legend(p))
}

orderRegions <- function(DT, aggChoice) {
  # order by regions
  if (aggChoice == "WB") {
    regionOrder <- c("lowInc","lowMidInc", "upMidInc","highInc")
    DT[, regionOrder := match(region_code, regionOrder)]
    data.table::setorder(DT, regionOrder)
    DT[, regionOrder := NULL]
  }
  if (aggChoice == "I3regions") {
    #' percap GDP data for ordering by income
    dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
    dt.pcGDPX0 <- dt.pcGDPX0[year %in% c("X2010","X2050"), ]
    #data.table::setkeyv(dt.GDP,c("region_code.IMPACT159"))
    dt.pcGDPX0 <- dt.pcGDPX0[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                             .SDcols = "pcGDPX0", by = c("scenario","region_code.IMPACT159")]
    dt.pcGDPX0growth <- dt.pcGDPX0[year ==  "X2050" & scenario == scenario.base,][,c("scenario","pcGDPX0","year") :=  NULL]
    dt.pcGDPX0.2010.ref <- dt.pcGDPX0[year ==  "X2010" & scenario == scenario.base,][,c("scenario","growthRate","year") :=  NULL]
    DT <- merge(dt.GDP.2010.ref, DT, by = c("region_code.IMPACT159"))
    data.table::setorder(DT, pcGDPX0) # for data tables ordered from low to high 2010 per cap GDP
  }
  return(DT)
}

aggNorder <- function(gdxChoice, DTaggNorder, aggChoice, scenChoice, mergedVals, plotErrorBars) {
  setkey(DTaggNorder, NULL)
  dt.regions <- regionAgg(aggChoice)
  # aggregate to and retain only the relevant regions
  temp <- merge(DTaggNorder, dt.regions, by = "region_code.IMPACT159")
  merged <- merge(temp, dt.pop, by = c("scenario","region_code.IMPACT159","year"))
  #' deal with the budget data
  if ("incShare.PCX0" %in% names(merged)) {
    keepListCol.incShare <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incShare.PCX0", "PopX0")
    keepListCol.pcGDP <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "pcGDPX0", "PopX0")
    dt.incShare <- merged[, (keepListCol.incShare), with = FALSE]
    dt.pcGDP <- merged[, (keepListCol.pcGDP), with = FALSE]
    data.table::setnames(dt.incShare, old = "incShare.PCX0", new = "value")
    data.table::setnames(dt.pcGDP, old = "pcGDPX0", new = "value")
    merged <- dt.incShare
  }
  #' deal with the ShannonDiversity data
  if ("SDnorm" %in% names(merged)) {
    keepListCol.SD <- c("scenario","year", "region_code.IMPACT159",  "region_code", "region_name", "SDnorm", "PopX0")
    merged <- merged[, (keepListCol.SD), with = FALSE]
    data.table::setnames(merged, old = "SDnorm", new = "value")
  }
  if ("MFAD" %in% names(merged)) {
    data.table::setnames(merged, old = "MFAD", new = "value")
  }

  #' aggregation takes place in the next lines of code.
  #' It says create the variable value from the old variable value, averaged by the region
  #' code and year (and other variables, in particular nutrient in some cases) using the popX) value as weights
  #' don't do min, max, and sd if aggChoice = tenregions
  if (plotErrorBars == TRUE) {
    merged <- merged[, min.region := min(value), by = mergedVals]
    merged <- merged[, max.region := max(value), by = mergedVals]
    merged <- merged[, sd.region := sd(value), by = mergedVals]
  }
  merged <- merged[, value := weighted.mean(value, PopX0), by = mergedVals]

  # other potential stats to add
  # lower=quantile(value, .25, na.rm=TRUE),
  # middle=quantile(value, .50, na.rm=TRUE),
  # upper=quantile(value, .75, na.rm=TRUE),

  keepListCol <- c(mergedVals, "region_name", "value", "min.region", "max.region", "sd.region")
  if(aggChoice %in% "tenregions") keepListCol <- c(mergedVals, "region_name", "value")
  if ("nutrient" %in% names(merged))  {
    # set kcalsPerDay.other to zero if it is less than zero.
    merged[nutrient %in% "kcalsPerDay.other" & value < 0, value := 0]
    nutOrder <- c("kcalsPerDay.carbohydrate", "kcalsPerDay.fat", "kcalsPerDay.protein", "kcalsPerDay.other")
    keepListCol <- c(mergedVals, "region_name", "value")
  }
  merged[, setdiff(names(merged), keepListCol) := NULL]
  data.table::setkey(merged, NULL)
  DT <- unique(merged)
  #' keep just the scenario.base scenario for 2010 and rename the scenario to 2010, then delete year column
  DT <- DT[year %in% "X2010" & scenario %in% scenario.base | year %in% "X2050",]
  DT <- DT[year %in% "X2010", scenario := "2010"][, year := NULL]

  #' order of scenario and regions
  if (gdxChoice == "USAID") {
    #    DT <- renameUSAIDscenarios(DT)
    #
    # # this needs to be changed at some point. Operate on scenChoice
    # scenarioList.prodEnhance <- c("REF_NoCC", "MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
    # scenarioList.waterMan <- c("REF_NoCC", "IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
    # scenarioList.addEnhance <- c("REF_NoCC","RPHL", "RMM")
    # scenarioList.comp <- c("REF_NoCC", "COMP", "COMP_NoCC", "COMP_IPSL")
    # keep only needed USAID scenarios
    scenOrder.USAID <- c(scenChoice)
    DT <- DT[scenario %in% scenChoice, ] # only needed for the USAID results
    # order scenarios, first write the number into the number variable scenarioOrder
    DT[, scenarioOrder := match(scenario, scenOrder.USAID)]
    data.table::setorder(DT, scenarioOrder)
    DT[, scenarioOrder := NULL]
  }
  if (gdxChoice == "SSPs") {
    # do manipulations on the gdx data that has the scenarios in scenChoice.
    scenOrder <- c("2010", scenChoice)
    DT <- DT[scenario %in% scenOrder, ] # doesn't need eval-parse because the list is defined inside the function

    #' order by scenarios
    DT[, scenarioOrder := match(scenario, scenOrder)]
    data.table::setorder(DT, scenarioOrder)
    DT[, scenarioOrder := NULL]

    #' order by nutrients
    if ("nutrient" %in% names(DT)) {
      DT[, nutOrder := match(nutrient, nutOrder)]
      data.table::setorder(DT, nutOrder)
      DT[, nutOrder := NULL]
    }
  }
  #' order by regions
  DT <- orderRegions(DT, aggChoice)
  DT <- DT[, region_name := gsub(" plus", "", region_name)]
  return(DT)
}

plotByRegionBar <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, oneLine, colorList, AMDR_hi = NULL, plotErrorBars) {
  cat("\nPlotting bars by region", aggChoice, "for", plotTitle)
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  if (aggChoice %in% "WB") regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  scenarioNameOrder <- scenOrder

  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  if (gdxChoice %in% "USAID")  temp <- renameUSAIDscenarios(temp)

  # if (round(max(temp$value)) - max(temp$value) > 0.08) yRange[2] <- round(max(temp$value) + 1, digits = 1) # will hopefully deal with rare situation. Commented out May 13, 2018
  #' use the standard deviation value to define ymax. Commented out April 3, 2018 because yminmax is now calculated in aggRun.
  # if (plotErrorBars == TRUE) {
  #   yRange[2] <- max(yRange[2], round(max(temp$sd.region + temp$value), digits = 1))
  # }

  #    if (aggChoice %in% "tenregions") temp[, c("min.region", "max.region", "sd.region") := NULL] now done in aggNorder
  #' when all elements of value are the same as the max y range
  temp[, value := round(value, 2)]

  # adjust font size for bars by aggchoice
  if (aggChoice %in% "tenregions") fontsize <- 1.8
  if (aggChoice %in% "WB") fontsize <- 3

   # adjust location of bar label in the bar (yval) for graph type
  if (yLab %in% "(Adequacy ratio)") {yval <- 0.15; roundVal = 2} else {yval <- 0.25; roundVal = 1}

    #' draw bars
  # pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""), width = 7, height = 5.2,)
  p = ggplot(data = temp, aes(x = factor(region_name), y = value, group = scenario)) +
    geom_col(aes(fill = scenario), position = "dodge", color = "black") +
    coord_cartesian(ylim = yRange) +
    theme(legend.position = "none") +
    labs(x = NULL, y = yLab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", face = "plain")) +
    theme(axis.title.y = element_text(family = "Times", face = "plain")) +
  #   geom_text(aes(label = value, y = 1.25), position = position_dodge(0.9), size = 2.25, angle = 90,  color = "white", vjust = "bottom") +
    geom_text(aes(label = formatC( round(value, roundVal), format='f', digits = roundVal), x = factor(region_name), y = yval), position = position_dodge(0.9),
              size = fontsize, angle = 90,   color = "white") + #vjust = "bottom", hjust = 'left', removed April 8, 2018
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times", face = "plain")) +
    ggtitle(plotTitle)

  #' the 'or' part of the if statement means don't draw the line if it's greater than ymax
  if (oneLine == FALSE | oneLine > yRange[2]) {} else {
    p <- p + geom_hline(aes(yintercept = oneLine, color = "black"))
  }
  if (plotErrorBars == TRUE) {
    temp[, yminValue := ifelse(value - sd.region < 0, 0, value - sd.region)]
    temp[, ymaxValue := ifelse(value + sd.region > yRange[2],  yRange[2], value + sd.region)]
    p <- p + geom_errorbar(aes(ymin = yminValue, ymax = ymaxValue), width = .2,
                           position = position_dodge(.9), color = "grey")
  }
  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".pdf"), plot = p,
         width = 7, height = 6)
  # dev.off()

  #' code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)

  formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, gsub("-REF","",scenarios))]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)

  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
}

plotByRegionStackedBar <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, oneLine, colorList) {
  cat("\nPlotting stacked bars by region", aggChoice, "for", plotTitle)
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)

  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  if (aggChoice %in% "WB") regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  scenarioNameOrder <- scenOrder

  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]

  if (gdxChoice %in% "USAID")  temp <- renameUSAIDscenarios(temp)

  #' draw bars
pdfFileName <- paste(fileloc("gDir"),"/", fileName, ".pdf", sep = "")
 # pdf(file = pdfFileName, width = 7, height = 5.2,)
#  if (max(temp$value) - yRange[2] > 0) yRange[2] <- max(temp$value) - commented out April 3, 2018
  p <- ggplot(temp, aes(as.numeric(interaction(scenario,region_name)), y = value, fill = nutrient, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "stack", color = "black", width = .80, group = "scenario") +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, family = "Times", face = "plain")) +
    theme(axis.title.y = element_text(family = "Times", face = "plain")) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times", face = "plain")) +
    ggtitle(plotTitle) +
    labs(y = yLab, x = NULL)

  if (oneLine == FALSE) {} else {p + geom_abline(intercept = oneLine, slope = 0)}
  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".pdf"), plot = p,
         width = 7, height = 6)

  #' code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
  # dev.off()
  formula.wide <- "scenario + nutrient ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, scenarios)]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)

  colsToRound <- names(temp.wide)[3:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", "nutrient", regionCodes))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
}

plotByBoxPlot2050 <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix ){
  cat("\nPlotting boxplot for 2050 by region", aggChoice)
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  if (aggChoice %in% "WB") regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames

  scenarioNameOrder <- scenOrder
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  if (gdxChoice %in% "USAID")  temp <- renameUSAIDscenarios(temp)

  # draw boxplot
  # pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""), width = 7, height = 5.2)
  p <- ggplot(temp, aes(x = region_name, y = value)) +
    geom_boxplot(stat = "boxplot", position = "dodge", color = "black", outlier.shape = NA) + # outlier.shape = NA, gets rid of outlier dots
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", face = "plain")) +
    theme(axis.title.y = element_text(family = "Times", face = "plain")) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times", face = "plain")) +
    ggtitle(plotTitle) +
    ylim(yRange) +
    labs(y = yLab, x = NULL)
  # dev.off()
  #' code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".pdf"), plot = p,
         width = 7, height = 6)
}
#
# plotByRegionLine <- function(dt, fileName, plotTitle, yRange, regionCodes, colorList) {
#   plotTitle <- capwords(plotTitle)
#   dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
#   dt.pcGDPX0.2010.ref <- dt.pcGDPX0[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]
#   temp <- getNewestVersion(dt, fileloc("resultsDir"))
#   temp <- merge(temp, dt.pcGDPX0.2010.ref, by = "region_code.IMPACT159")
#   temp <- temp[region_code.IMPACT159 %in% regionAgg("I3regions")]
#   scenarios <- unique(temp$scenario)
#
#   if (gdxChoice %in% "USAID")  temp <- renameUSAIDscenarios(temp)
#
#   pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""))
#   par(mfrow = c(1,1))
#   legendText <- NULL
#   for (i in 1:length(scenarios)) {
#     if (i == 1) {
#       plot(temp[year %in% "X2050" & scenario == scenarios[i],value], type = "l", col = "green",
#            xlab = "", xaxt = "n", ylab = "share (%)",
#            main = plotTitle, cex.main = 1, ylim = yRange) # common range for requirements share
#       par(new = T)
#     } else {
#       lines(temp[year %in% "X2050" & scenario == scenarios[i],value], col = colorList[i])
#       par(new = F)
#     }
#     legendText <- c(legendText,scenarios[i])
#   }
#   lines(temp[year %in% "X2010" & scenario %in% scenario.base, value], col = "black")
#   legendText <- c(legendText, "2010")
#   # print(legendText)
#   axis(1, at = 1:length(unique(dt.GDP.2010.ref$region_code.IMPACT159)), labels = unique(dt.GDP.2010.ref$region_code.IMPACT159), cex.axis = 0.5, padj = -3)
#   abline(h = 1, lty = 3, lwd = 0.8)
#   legendText <- gsub("-REF", "", legendText)
#   # dev.off()
# }

#' # error bars-----
#' plotByRegionErrorBars <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, oneLine) {
#'   cat("\nPlotting lines with error bars by region ", aggChoice, "for ", plotTitle)
#'   plotTitle <- capwords(plotTitle)
#'   temp <- copy(dt)
#'   regionCodes <- unique(temp$region_code)
#'   regionNames <- unique(temp$region_name)
#'   scenarios <- unique(temp$scenario)
#'   if (gdxChoice == "SSPs") colorList <- c("black", "red", "red3", "red4", "green", "green3", "green4")
#'   if (gdxChoice == "USAID") colorList <- c("black", rainbow(10)[1:length(scenarios) - 1])
#'   legendText <- unique(gsub("-REF", "", scenarios))
#'
#'   temp[, scenario := gsub("-REF", "", scenario)]
#'   regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
#'   scenarioNameOrder <- c("2010", "SSP2-NoCC", "SSP1-NoCC", "SSP3-NoCC", "SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
#'   temp[, region_name := gsub(" income", "", region_name)]
#'   temp[, region_name := factor(region_name, levels =  regionNameOrder)]
#'   temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
#'   temp[, region_name := gsub(" income", "", region_name)]
#'   scenario.econ <- c("SSP2-NoCC", "SSP1-NoCC", "SSP3-NoCC")
#'   scenario.clim <- c("SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
#'   min.econ <- temp[scenario %in% scenario.econ,  min(value), by = c("region_code")]
#'   temp[scenario %in% scenario.econ, value.max.econ := max(value), by = c("region_code")]
#'   temp[scenario %in% scenario.clim, value.min.clim := min(value), by = c("region_code")]
#'   temp[scenario %in% scenario.clim, value.max.clim := max(value), by = c("region_code")]
#'   temp[scenario %in% "2010", value.min.econ := value]
#'   temp[scenario %in% "2010", value.max.econ := value]
#'   temp[scenario %in% "2010", value.min.clim := value]
#'   temp[scenario %in% "2010", value.max.clim := value]
#'   #
#'
#'   #' draw lines
#'   temp2 <- temp[scenario %in% c("2010", "SSP2-NoCC") & region_code %in% "lowInc",]
#'   ggplot(temp2, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
#'     theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", face = "plain")) +
#'     theme(axis.title.y = element_text(family = "Times", face = "plain")) +
#'     geom_line(aes(group = 1)) +
#'     geom_point(size = 2) +
#'     theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times", face = "plain")) +
#'     ggtitle(plotTitle) +
#'     ylim(yRange) +
#'     labs(y = yLab, x = NULL) +
#'     geom_errorbar(aes(ymin = value.min.econ, ymax = value.max.econ),color = "green",
#'                   size = 2, width = 0.2)
#'
#'   #' code to save the plot for future use
#'   graphsListHolder[[fileName]] <- p
#'   assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
#' }

plotByRegionBarAMDR <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, colorList, AMDR_lo, AMDR_hi, graphsListHolder, plotErrorBars) {
  cat("\nPlotting AMDR bars for region ", aggChoice, "for", plotTitle)
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  if (aggChoice %in% "WB") regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  scenarioNameOrder <- scenOrder
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  if (gdxChoice %in% "USAID")  temp <- renameUSAIDscenarios(temp)

  #' draw bars
#  pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""), width = 7, height = 5.2)
  if (round(max(temp$value) - yRange[2]) == 0) yRange[2] <- max(temp$value) # will hopefully deal with rare situation
  #' when all elements of value are the same as the max y range
  p <- ggplot(temp, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", face = "plain")) +
    theme(axis.title.y = element_text(family = "Times", face = "plain")) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times", face = "plain")) +
    ggtitle(plotTitle) +
    labs(y = yLab, x = NULL) +
    geom_hline(aes(yintercept = AMDR_lo,  color = "green")) +
    geom_text( aes(.5, AMDR_lo + .75, label = "Low", color = "green")) +
    geom_hline(aes(yintercept = AMDR_hi,  color = "dark red")) +
    geom_text( aes(.5, AMDR_hi + .75, label = "High", color = "green")) +
    geom_text(aes(x = factor(region_name),  y=0.25, label = round(value, 1)), position = position_dodge(0.9), size = 2.25, angle = 90,  color = "white", hjust = 'left')

   ggsave(filename = paste0(fileloc("gDir"),"/",fileName,".pdf"), plot = p,
         width = 7, height = 6)
 # # dev.off()
  #' code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)

  # save data
  formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, gsub("-REF","",scenarios))]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)

  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  #  textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
}
