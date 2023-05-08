#' Nutrient Modeling Functions for the shiny app
#' @title: "Functions used in aggRun.R and a few other scripts for aggregating and graphing
#' @name aggNorder.R
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, aggregate data, sort data
#' @description {
#' This script contains functions that are needed primarily (or perhaps exclusively) in
#' the aggRun.R script. It has functions to aggregate data to arbitrary regions and to sort
#' data ordering to a prespecified order.
#' }

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
sourceFile <- "aggNorder.R"
description <- "This script contains functions that are needed primarily (or perhaps exclusively) in the aggRun.R script. It has functions to aggregate data to arbitrary regions and to sort data ordering to a prespecified order."

# GDP setup -----
library(gridExtra)
library(gplots)

# get gdxChoice -----
gdxChoice <- "SSPs"

# population data set used for weighting by population -----
# dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))
dt.pop <- readRDS(paste0(fileloc("iData"), "dt.PopX0"))

getRegionOrder <- function(aggChoice, regionNames) {
  regionNameOrder <- character(0)
  if (aggChoice %in% "WB") {regionNameOrder <- c("Low", "Lower middle", "Upper middle", "High")}
  if (aggChoice %in% "AggReg1") {regionNameOrder <- regionNames}
  if (aggChoice %in% "tenregions") {
    codes <- keyVariable("tenregions")
    regionNameOrder <- character(0)
    for (i in codes)  {
      regionNameOrder <- append(regionNameOrder, unique((countryNameLookup(i))))
    }
  }
  
  regionNameOrder <- gsub(" plus", "", regionNameOrder) 
  return(regionNameOrder)
}
# create legend grobs for use in multiple graph files -----
updateLegendGrobs <- function(l, aggChoice, legendLoc, mergedVals, fileToUse) {
  # use an arbitrary file to construct the grob. This code modified from aggRun.R
  DTGlobal <- getNewestVersion(fileToUse, fileloc("resultsDir"))
  DTGlobal[, scenario := gsub("-", "_", scenario)]
  DTGlobal[, scenario := gsub("_REF", "", scenario)] # added for final version of paper Nove 3, 2018
  DT <- aggNorder(gdxChoice, DTaggNorder = DTGlobal, aggChoice, scenChoice = get(l), mergedVals, plotErrorBars) # returns a scenario list with 2010
  ylab <- "(percent)"
  cat("\nCreating legend grob for ", aggChoice, ",", l, "and", legendLoc, "\n")
  regionCodes <- unique(DT$region_code)
  regionNames <- unique(DT$region_name)
  scenarios <- unique(DT$scenario)
  scenOrder <- get(l) # returns a scenario list with X2010
  scenOrder <- gsub("X", "", scenOrder) # gets rid of the X in 2010 so the legend is ordered properly
  regionNameOrder <- getRegionOrder(aggChoice, regionNames)
  scenarioNameOrder <- scenOrder
  DT[, region_name := gsub(" income", "", region_name)]
  DT[, region_name := factor(region_name, levels =  regionNameOrder)]
  DT[, scenario := factor(scenario, levels = scenarioNameOrder)]

  # draw bars to get the legend
  p <- ggplot(DT, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme(legend.position = legendLoc) +
    theme(legend.text = element_text(size = 8, family = fontFamily, face = "plain")) +
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
    # percap GDP data for ordering by income
#    dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
    dt.pcGDPX0 <- readRDS(paste0(fileloc("iData"), "dt.pcGDPX0"))
    dt.pcGDPX0 <- dt.pcGDPX0[year %in% c("X2010","X2050"), ]
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
  DTaggNorder <- DTaggNorder[scenario %in% scenChoice] # added June 4, 2018. Note: includes 2010
  dt.regions <- regionAgg(aggChoice)
  # aggregate to and retain only the relevant regions
  temp <- merge(DTaggNorder, dt.regions, by = "region_code.IMPACT159")
  merged <- merge(temp, dt.pop, by = c("scenario","region_code.IMPACT159","year"))
  # deal with the budget data
  if ("incShare.PCX0" %in% names(merged)) {
    keepListCol.incShare <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incShare.PCX0", "PopX0")
    keepListCol.pcGDP <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "pcGDPX0", "PopX0")
    dt.incShare <- merged[, (keepListCol.incShare), with = FALSE]
    dt.pcGDP <- merged[, (keepListCol.pcGDP), with = FALSE]
    data.table::setnames(dt.incShare, old = "incShare.PCX0", new = "value")
    data.table::setnames(dt.pcGDP, old = "pcGDPX0", new = "value")
    merged <- dt.incShare
  }
  # deal with the ShannonDiversity data
  if ("SDnorm" %in% names(merged)) {
    keepListCol.SD <- c("scenario","year", "region_code.IMPACT159",  "region_code", "region_name", "SDnorm", "PopX0")
    merged <- merged[, (keepListCol.SD), with = FALSE]
    data.table::setnames(merged, old = "SDnorm", new = "value")
  }
  if ("MFAD" %in% names(merged)) {
    data.table::setnames(merged, old = "MFAD", new = "value")
  }
  
  # aggregation takes place in the next lines of code.
  # It says create the variable value from the old variable value, averaged by the region
  # code and year (and other variables, in particular nutrient in some cases) using the popX) value as weights
  # don't do min, max, and sd if aggChoice = tenregions
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
  if(aggChoice %in% c("tenregions") ) keepListCol <- c(mergedVals, "region_name", "value")
  if ("nutrient" %in% names(merged))  {
    # set kcalsPerDay_other to zero if it is less than zero.
    merged[nutrient %in% "kcalsPerDay_other" & value < 0, value := 0]
    nutOrder <- c("kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein", "kcalsPerDay_other")
    keepListCol <- c(mergedVals, "region_name", "value")
  }
  merged[, setdiff(names(merged), keepListCol) := NULL]
  data.table::setkey(merged, NULL)
  DT <- unique(merged)
  # keep just the scenario.base scenario for 2010 and rename the scenario to 2010, then delete year column
  DT <- DT[year %in% "X2010" & scenario %in% scenario.base.CC | year %in% "X2050",]
  DT <- DT[year %in% "X2010", scenario := "2010"][, year := NULL]
  
  # order of scenario and regions
    scenOrder <- scenChoice
    # order by scenarios
    DT[, scenarioOrder := match(scenario, scenOrder)]
    data.table::setorder(DT, scenarioOrder)
    DT[, scenarioOrder := NULL]
    
    # order by nutrients
    if ("nutrient" %in% names(DT)) {
      DT[, nutOrder := match(nutrient, nutOrder)]
      data.table::setorder(DT, nutOrder)
      DT[, nutOrder := NULL]
    }
  
  # order by regions
  DT <- orderRegions(DT, aggChoice)
  DT <- DT[, region_name := gsub(" plus", "", region_name)]
  return(DT)
}

# removed AMDR_hi = NULL Oct 15, 2018
plotByRegionBar <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, oneLine, colorList, graphsListHolder, plotErrorBars, fill = "region_name") { 
  cat("Plotting bars by region", aggChoice, "for", plotTitle, "\n")
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
   regionNameOrder <- getRegionOrder(aggChoice, regionNames)
  scenarioNameOrder <- scenOrder
  if ("2010" %in% scenarios & !"2010" %in% scenarioNameOrder) scenarioNameOrder <- c("2010", scenarioNameOrder)
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]

  temp[, value := round(value, 2)]
  
  # adjust font size for bars by aggchoice
  if (aggChoice %in% c("tenregions") ) fontsize <- 1.5
  if (aggChoice %in% "WB") fontsize <- 2.5
  
  # adjust location of bar label in the bar (yval) for graph type
  if (yLab %in% "(Adequacy ratio)") {roundVal = 2} else { roundVal = 1}
  # next line is supposed to put the label in the bar at 6.5 percent of the distance from the bottom
  # to the top of the bar. Commented out July 11, 2018
  yval <- (yRange[2] - yRange[1]) *.065
  # draw bars
  
  #default values
  nrow = 2
  legendLoc <- "right"
  height = 5
  width = 7
  yDelta <- max(temp$value) - min(temp$value)
  yvalSupplment <- yDelta * .04
  roundVal = 2
  
  if (max(temp$value) > 9) {roundVal = 1} # for text in the bars
  if (max(temp$value) > 99) {roundVal = 0} # for text in the bars
  
    legendLoc <- "none"
    p <- ggplot(data = temp, aes(x = factor(region_name), y = value, group = scenario)) +
      geom_col(aes(fill = scenario), position = "dodge", color = "black") +
      coord_cartesian(ylim = yRange) +
      theme( # remove the vertical grid lines
        panel.grid.major.x = element_blank() ,
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="black", ), 
        panel.background = element_blank(),
        axis.line.y = element_line(colour = 'black', size=0.1, linetype='solid'),
        axis.line.x = element_line(colour = 'black', size=0.1, linetype='solid')
      ) +
      theme(legend.position = legendLoc, legend.text=element_text(size=10)) +
      labs(x = NULL, y = yLab) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, family = fontFamily, face = "plain")) +
      theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
      scale_fill_manual(values = colorList) +
      theme(plot.title = element_text(hjust = 0.5, size = 11, family = fontFamily, face = "plain")) +
      ggtitle(plotTitle) 
     # the 'or' part of the if statement means don't draw the line if it's greater than ymax
    if (oneLine == FALSE | oneLine > yRange[2]) {} else {
      p <- p + geom_hline(aes(yintercept = oneLine, color = "black"), show.legend = FALSE)
    }
    if (plotErrorBars == TRUE) {
      temp[, yminValue := ifelse(value - sd.region < 0, 0, value - sd.region)]
      temp[, ymaxValue := ifelse(value + sd.region > yRange[2],  yRange[2], value + sd.region)]
      p <- p + geom_errorbar(aes(ymin = yminValue, ymax = ymaxValue), width = .2,
                             position = position_dodge(.9), color = "grey")
    }
    p <- p + geom_text(aes(label = formatC( round(value, roundVal), format='f', digits = roundVal),
                           x = factor(region_name), y = yval), position = position_dodge(0.9),
                       size = fontsize, angle = 90, color = "black")

  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".png"), plot = p, device = "png",
         width = width, height = height)
  
  # code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
  
  formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, scenarioNameOrder)]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
  
  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
  return(p)
}

plotByRegionStackedBar <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, oneLine, colorList, graphsListHolder) {
  cat("\nPlotting stacked bars by region", aggChoice, "for", plotTitle, "\n")
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  # adjust font size for bars by aggchoice
  if (aggChoice %in% c("tenregions") ) fontsize <- 1.5
  if (aggChoice %in% "WB") fontsize <- 2.5
  # adjust location of bar label in the bar (yval) for graph type
  if (yLab %in% "(Adequacy ratio)") {roundVal = 2} else { roundVal = 1}
   yval <- (yRange[2] - yRange[1]) *.065
  
  # Next two lines not needed as of Nov 3, 2018
#  temp[, scenario := gsub("-REF", "", scenario)] 
#  scenOrder <- gsub("-REF", "", scenOrder)
  regionNameOrder <- getRegionOrder(aggChoice, regionNames)  
  scenarioNameOrder <- scenOrder
  if ("2010" %in% scenarios & !"2010" %in% scenarioNameOrder) scenarioNameOrder <- c("2010", scenarioNameOrder)
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]

  #default values
  nrow = 2
  legendLoc <- "right"
  height = 5
  width = 7
  yvalSupplment = 0
  
  p <- ggplot(data=temp, aes(scenario, y = value, fill = nutrient, levels =  region_name, position_dodge(preserve = "total"))) +
    geom_bar(stat = "identity", position = "stack", color = "black", width = .80, group = "region_name") +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black", ), 
      panel.background = element_blank(),
      axis.line.y = element_line(colour = 'black', size=0.1, linetype='solid'),
      axis.line.x = element_line(colour = 'black', size=0.1, linetype='solid')
    ) +
    theme(legend.position = legendLoc) +
    theme(legend.title=element_blank()) + 
    labs(x = NULL, y = yLab) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, family = fontFamily, face = "plain")) +
    theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = fontFamily, face = "plain")) +
    scale_fill_manual(values = colorList) +
    ggtitle(plotTitle) +
    facet_wrap(~ region_name, nrow = nrow) +
    theme(panel.spacing = unit(0.1, "lines")) +
    theme(strip.text.x = element_text(face= "bold", size = 11)) # other options colour = "orange", angle = 90
  
  if (oneLine == FALSE) {} else {p + geom_abline(intercept = oneLine, slope = 0)}
  
   height = 6
  cat("height:", height, "\n")
  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".png"), plot = p,
         width = width, height = height)
  
  # code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
  
  formula.wide <- "scenario + nutrient ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, scenarioNameOrder)]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
  
  colsToRound <- names(temp.wide)[3:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", "nutrient", regionCodes))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
}

plotByBoxPlot2050 <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, graphsListHolder) {
  cat("\nPlotting boxplot for 2050 by region", aggChoice, "\n")
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  regionNameOrder <- getRegionOrder(aggChoice, regionNames)  
  
  scenarioNameOrder <- scenOrder
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
 
  # draw boxplot
  p <- ggplot(temp, aes(x = region_name, y = value)) +
    geom_boxplot(stat = "boxplot", position = "dodge", color = "black", outlier.shape = NA) + # outlier.shape = NA, gets rid of outlier dots
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = fontFamily, face = "plain")) +
    theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = fontFamily, face = "plain")) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black", ), 
      panel.background = element_blank(),
      axis.line.y = element_line(colour = 'black', size=0.1, linetype='solid'),
      axis.line.x = element_line(colour = 'black', size=0.1, linetype='solid')
    ) +
    ggtitle(plotTitle) +
    ylim(yRange) +
    labs(y = yLab, x = NULL)
  
  ggsave(file = paste0(fileloc("gDir"),"/",fileName,".png"), plot = p,
         width = 7, height = 6)
  
  # code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
}

plotByRegionBarAMDR <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, colorList, AMDR_lo, AMDR_hi, graphsListHolder, plotErrorBars) {
  print(paste("Plotting AMDR bars for region", aggChoice, "for", plotTitle, sep = " ") )
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  # Next two lines not needed as of Nov 3, 2018
  #  temp[, scenario := gsub("-REF", "", scenario)] 
  #  scenOrder <- gsub("-REF", "", scenOrder)
  regionNameOrder <- getRegionOrder(aggChoice, regionNames)  
  scenarioNameOrder <- scenOrder
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  # adjust location of bar label in the bar (yval) for graph type
  if (yLab %in% "(Adequacy ratio)") {roundVal = 2} else { roundVal = 1}
   yval = 1.5 # controls how far above the y axis bottom the vertical numbers are
  xval = 2.5 # x nudge default for WB
  if (aggChoice %in% "tenregions") {xval = 0.7; fontsize <- 1.5}
  if (aggChoice %in% "WB") fontsize <- 2.5
  
  roundVal = 2
  if (max(temp$value) > 9) {roundVal = 1} # for text in the bars
  if (max(temp$value) > 99) {roundVal = 0} # for text in the bars
  
  # draw bars
  #  pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""), width = 7, height = 5.2)
  if (round(max(temp$value) - yRange[2]) == 0) yRange[2] <- max(temp$value) # will hopefully deal with rare situation
  # when all elements of value are the same as the max y range
  p <- ggplot(temp, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black", ), 
      panel.background = element_blank(),
      axis.line.y = element_line(colour = 'black', size=0.1, linetype='solid'),
      axis.line.x = element_line(colour = 'black', size=0.1, linetype='solid')
    ) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = fontFamily, face = "plain")) +
    theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = fontFamily, face = "plain")) +
    ggtitle(plotTitle) +
    labs(y = yLab, x = NULL) +
    geom_hline(aes(yintercept = AMDR_lo), color = "darkgreen", show.legend = FALSE) +
    geom_label(aes(x = 1, y = AMDR_lo + 2, label = paste0("Lower limit is ", AMDR_lo), 
                   family = fontFamily), fill= "white", color = "black", nudge_x = xval) + # value after AMDR_lo shifts the label up or down
 #   geom_text(aes(.6, AMDR_lo + 2.5, label = "Low", family = fontFamily), color = "black") + # value after AMDR_lo shifts the label up or down
    geom_hline(aes(yintercept = AMDR_hi),  color = "dark red", show.legend = FALSE) +
    geom_label(aes(x =1, y = AMDR_hi + 2, label = paste0("Upper limit is ", AMDR_hi), 
                   family = fontFamily), fill= "white", color = "black", nudge_x = xval)
  # next line adds the vertical numbers
  p <- p + geom_text(aes(label = formatC( round(value, roundVal), format='f', digits = roundVal),
                         x = factor(region_name), y = yval), position = position_dodge(0.9),
                     size = fontsize, angle = 90, color = "black")

  ggsave(filename = paste0(fileloc("gDir"),"/",fileName,".png"), plot = p,
         width = 7, height = 6)
  
  # code to save the plot for future use
  graphsListHolder[[fileName]] <- p
  assign("graphsListHolder", graphsListHolder, envir = .GlobalEnv)
  
  # save data
  formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  # next line probably not needed as of Nov 3, 2018
  temp.wide[, scenarioOrder := match(scenario, gsub("-REF","",scenarios))]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
  
  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  #  textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
  write.csv(temp.wide, file = paste(fileloc("gDir"),"/", fileName, ".csv", sep = ""))
  return(p)
}
