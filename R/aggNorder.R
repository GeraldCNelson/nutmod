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

{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
  source("R/renameUSAIDscenarios.R")}
# GDP setup -----
library(data.table)
library(gridExtra)
library(gplots)
library(ggplot2)

# population for weighting -----
dt.pop <- getNewestVersion("dt.PopX0", fileloc("iData"))

orderRegions <- function(DT, aggChoice) {
  # order by regions
  if (aggChoice == "WB") {
    regionOrder <- c("lowInc","lowMidInc", "upMidInc","highInc")
    DT[, regionOrder := match(region_code, regionOrder)]
    data.table::setorder(DT, regionOrder)
    DT[, regionOrder := NULL]
  }
  if (aggChoice == "I3regions") {
    # percap GDP data for ordering
    dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
    # dt.pcGDPX0 <- dt.pcGDPX0[scenario %in% eval(parse(text = (scenChoice)))[!eval(parse(text = (scenChoice))) %in% "2010"],]
    # dt.pcGDPX0 <- unique(dt.pcGDPX0[,c("IMPACT_code","FoodAvailability","PCX0","PWX0","CSE") :=  NULL])
    dt.pcGDPX0 <- dt.pcGDPX0[year %in% c("X2010","X2050"), ]
    #data.table::setkeyv(dt.GDP,c("region_code.IMPACT159"))
    dt.pcGDPX0 <- dt.pcGDPX0[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                             .SDcols = "pcGDPX0", by = c("scenario","region_code.IMPACT159")]
    dt.pcGDPX0growth <- dt.pcGDPX0[year ==  "X2050" & scenario == scenario.base,][,c("scenario","pcGDPX0","year") :=  NULL]
    dt.pcGDPX0.2010.ref <- dt.pcGDPX0[year ==  "X2010" & scenario == scenario.base,][,c("scenario","growthRate","year") :=  NULL]
    DT <- merge(dt.GDP.2010.ref, DT, by = c("region_code.IMPACT159"))
    data.table::setorder(DT, pcGDPX0) # for data tables ordered from low to high 2010 per cap GDP
  }
  # if (aggChoice == "156") {
  #   DT <- DT[region_code %in% regionCodestenregions,]
  #   # percap GDP data for ordering
  #   data.table::setorder(DT, aggChoice)
  # }
  return(DT)
}

aggNorder <- function(gdxChoice, DTglobal, aggChoice, scenChoice) {
  # print(paste("running aggNorder for ", gdxChoice, " and ", i))
  DT <- getNewestVersion(DTglobal, fileloc("resultsDir"))
  setkey(DT, NULL)
  dt.regions <- regionAgg(aggChoice)
  # aggregate to and retain only the relevant regions
  temp <- merge(DT, dt.regions, by = "region_code.IMPACT159")
  merged <- merge(temp, dt.pop, by = c("scenario","region_code.IMPACT159","year"))
  # merged <- merged[region_code.IMPACT159 %in% region_code, ]
  # deal with the budget data
  if ("incSharePCX0" %in% names(merged)) {
    keepListCol.incShare <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0", "PopX0")
    keepListCol.pcGDP <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "pcGDPX0", "PopX0")
    dt.incShare <- merged[, (keepListCol.incShare), with = FALSE]
    dt.pcGDP <- merged[, (keepListCol.pcGDP), with = FALSE]
    data.table::setnames(dt.incShare, old = "incSharePCX0", new = "value")
    data.table::setnames(dt.pcGDP, old = "pcGDPX0", new = "value")
    merged <- dt.incShare
  }
  # deal with the ShannonDiversity data
  if ("SDnorm" %in% names(merged)) {
    keepListCol.SD <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "SDnorm", "PopX0")
    merged <- merged[, (keepListCol.SD), with = FALSE]
    data.table::setnames(merged, old = "SDnorm", new = "value")
  }
  if ("MFAD" %in% names(merged)) {
    data.table::setnames(merged, old = "MFAD", new = "value")
  }

  #  temp <- temp[, region.budget.share := mean(value), by = c("region_code", "year")]
  # aggregation takes place in the next line of code. It says create the variable value from the old variable value, averaged by the region
  # code and year using the popX) value as weights
  merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
  keepListCol <- c("scenario",  "year", "region_code", "region_name", "value")
  merged <- merged[, (keepListCol), with = FALSE]
  data.table::setkey(merged, NULL)
  DT <- unique(merged)
  #keep just the scenario.base scenario for 2010 and rename the scenario to 2010, then delete year column
  DT <- DT[year == "X2010" & scenario == scenario.base | year == "X2050",]
  DT <- DT[year == "X2010", scenario := "2010"][, year := NULL]

  # order of scenario and regions
  if (gdxChoice == "USAID") {
    DT <- renameUSAIDscenarios(DT)
    scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
    scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
    scenarioList.addEnhance <- c("RPHL", "RMM")
    scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
    # keep only needed USAID scenarios
    scenOrder.USAID <- c("2010", scenChoice)
    DT <- DT[scenario %in% scenOrder.USAID, ] # only needed for the USAID results
    # order scenarios, first write the number into the number variable scenarioOrder
    DT[, scenarioOrder := match(scenario, scenOrder.USAID)]
    data.table::setorder(DT, scenarioOrder)
    DT[, scenarioOrder := NULL]
  }
  if (gdxChoice == "SSPs") {
    # do manipulations on the gdx data that has the scenarios in scenChoice.
    scenOrder <- c("2010", scenChoice)
    DT <- DT[scenario %in% scenOrder, ] # doesn't need eval-parse because the list is defined inside the function

    # order by scenarios
    DT[, scenarioOrder := match(scenario, scenOrder)]
    data.table::setorder(DT, scenarioOrder)
    DT[, scenarioOrder := NULL]
  }
  # order by regions
  DT <- orderRegions(DT, aggChoice)
  DT <- DT[, region_name := gsub(" plus", "", region_name)]
  return(DT)
}

plotByRegionBar <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, scenOrder, oneLine, colorList) {
  print(paste("plotting bars by region", aggChoice, "for", plotTitle))
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)

  # initial attempts to use ggplot
  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  # temp <- temp[order(region_code)]
  if (aggChoice %in% "WB") regionNameOrder <- c("Low income", "Lower middle income", "Upper middle income", "High income")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  scenarioNameOrder <- scenOrder
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  scenario.econ <- c("SSP2-NoCC", "SSP1-NoCC", "SSP3-NoCC")
  scenario.clim <- c("SSP2-HGEM")
  # min.econ <- temp[scenario %in% scenario.econ,  min(value), by = c("region_code")]
  # temp[scenario %in% scenario.econ, value.min.econ := min(value), by = c("region_code")]
  # temp[scenario %in% scenario.econ, value.max.econ := max(value), by = c("region_code")]
  # temp[scenario %in% scenario.clim, value.min.clim := min(value), by = c("region_code")]
  # temp[scenario %in% scenario.clim, value.max.clim := max(value), by = c("region_code")]
  # temp[scenario %in% "2010", value.min.econ := value]
  # temp[scenario %in% "2010", value.max.econ := value]
  # temp[scenario %in% "2010", value.min.clim := value]
  # temp[scenario %in% "2010", value.max.clim := value]
  #
  # draw bars
  pdf(paste("graphics/", fileName,"_", aggChoice, ".pdf", sep = ""), width = 7, height = 5.2, useDingbats=FALSE)
  p <- ggplot(temp, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(plotTitle) +
    ylim(yRange) +
    labs(y = yLab, x = NULL)

  if (oneLine == TRUE) p + geom_abline(intercept = 1, slope = 0)
  print(p)
  # geom_errorbar(aes(ymin = value.min.econ, ymax = value.max.econ), color = "red", size = 2, position = position_dodge(0.9), width = 0.5) +
  # geom_errorbar(aes(ymin = value.min.clim, ymax = value.max.clim), color = "green", size = 2, position = position_dodge(0.2), width = 0.5)
  dev.off()
  formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  temp.wide <- data.table::dcast(
    data = temp,
    formula = formula.wide,
    value.var = "value")
  temp.wide[, scenarioOrder := match(scenario, scenarios)]
  data.table::setorder(temp.wide, scenarioOrder)
  temp.wide[, scenarioOrder := NULL]
  #temp.out <- data.table::copy(temp.wide)
  data.table::setnames(temp.wide, old = regionCodes, new = regionNames)

  colsToRound <- names(temp.wide)[2:length(temp.wide)]
  temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  #  textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
  write.csv(temp.wide, file = paste("graphics/", fileName, "_", aggChoice, ".csv", sep = ""))
  # #draw lines
  # temp2 <- temp[scenario %in% c("2010", "SSP2-NoCC") & region_code %in% "lowInc",]
  # ggplot(temp2, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   geom_line(aes(group = 1)) +
  #   geom_point(size = 2) +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   ggtitle(plotTitle) +
  #   ylim(yRange) +
  #   labs(y = yLab, x = NULL) +
  #   geom_errorbar(aes(ymin = value.min.econ, ymax = value.max.econ), color = "green", size = 2, width = 0.2)


  # ###

  #the use of factor and levels keeps the order of the regions in region_code
  # formula.wide <- "scenario ~ factor(region_code, levels = unique(region_code))"
  # temp.wide <- data.table::dcast(
  #   data = temp,
  #   formula = formula.wide,
  #   value.var = "value")
  # temp.wide[, scenarioOrder := match(scenario, scenarios)]
  # data.table::setorder(temp.wide, scenarioOrder)
  # temp.wide[, scenarioOrder := NULL]
  # #temp.out <- data.table::copy(temp.wide)
  # data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
  # temp.wide[, scenario := gsub("-REF", "", scenario)]
  # temp <- as.data.frame(temp.wide)
  #
  # rownames(temp) <- temp[,1]
  # temp[1] = NULL
  # temp <- data.matrix(temp)
  # # print(temp.wide)
  # #  par(mai=c(2,0.82,0.82,0.42))
  # # pdf(paste("graphics/", fileName,"_", aggChoice, ".pdf", sep = ""), width = 7, height = 5.2)
  # # #layout(matrix(c(1,2)), c(1,1), c(1,3))
  # # #par(mfrow = c(2,1), mai = c(1,1,1,1))
  # # #  mat = matrix(c(1,2))
  # # #  layout(mat, heights = c(5,3))
  # # barlocs <- barplot(temp,  col = colorList, ylim = yRange, xaxt = "n",
  # #                    legend.text = rownames(temp), args.legend =
  # #                      list(cex = .5, x = "bottomright", inset = c(0, -0.3), xpd = TRUE),
  # #                    beside = TRUE, ylab = yLab,  cex.names = .7, las = 2,  srt = 45, main = plotTitle)
  #
  # regionNames <- colnames(temp)
  # text(
  #   colMeans(barlocs),
  #   par("usr")[3] - 0.1, srt = 45, adj = 1.2,
  #   labels = regionNames, xpd = TRUE,  cex = 0.6, cex.axis = 0.6)
  # if (oneLine == TRUE) abline(h = 1, lty = 3, lwd = 0.8)
  # colsToRound <- names(temp.wide)[2:length(temp.wide)]
  # temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  # data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
  # #  textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
  # dev.off()
  # write.csv(temp.wide, file = paste("graphics/", fileName, "_", aggChoice, ".csv", sep = ""))
  # print(paste("Done plotting bars by region ", aggChoice, "for ", plotTitle))
  #cat("\n\n")

}

plotByBoxPlot2050 <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice ){
  print(paste("plotting boxplot for 2050 by region", aggChoice))
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)


  # initial attempts to use ggplot
  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  # temp <- temp[order(region_code)]
  if (aggChoice %in% "WB") regionNameOrder <- c("Low income", "Lower middle income", "Upper middle income", "High income")
  if (aggChoice %in% "AggReg1") regionNameOrder <- regionNames
  if (aggChoice %in% "tenregions") regionNameOrder <- regionNames
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]

  # draw boxplot
  pdf(paste("graphics/", fileName,"_", aggChoice, ".pdf", sep = ""), width = 7, height = 5.2, useDingbats = FALSE)
  p <- ggplot(temp, aes(x = region_name, y = incSharePCX0)) +
    geom_boxplot(stat = "boxplot", position = "dodge", color = "black", outlier.shape = NA) + # outlier.shape = NA, gets rid of outlier dots
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(plotTitle) +
    ylim(yRange) +
    labs(y = yLab, x = NULL)
  print(p)
  dev.off()
}

plotByRegionLine <- function(dt, fileName, plotTitle, yRange, regionCodes, colorList) {
  dt.pcGDPX0 <- getNewestVersionIMPACT("dt.pcGDPX0")
  dt.pcGDPX0.2010.ref <- dt.pcGDPX0[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]
  temp <- getNewestVersion(dt, fileloc("resultsDir"))
  temp <- merge(temp, dt.pcGDPX0.2010.ref, by = "region_code.IMPACT159")
  temp <- temp[region_code.IMPACT159 %in% regionAgg("I3regions")]
  scenarios <- unique(temp$scenario)
  pdf(paste("graphics/", fileName, ".pdf", sep = ""))
  par(mfrow = c(1,1))
  legendText <- NULL
  for (i in 1:length(scenarios)) {
    if (i == 1) {
      plot(temp[year %in% "X2050" & scenario == scenarios[i],value], type = "l", col = "green",
           xlab = "", xaxt = "n", ylab = "share (%)",
           main = plotTitle, cex.main = 1, ylim = yRange) # common range for requirements share
      #      main = nutShortname,cex.main=1, ylim = c(0, round(max(scen.temp$value))))
      par(new = T)
    } else {
      lines(temp[year %in% "X2050" & scenario == scenarios[i],value], col = colorList[i])
      par(new = F)
    }
    legendText <- c(legendText,scenarios[i])
  }
  lines(temp[year %in% "X2010" & scenario %in% scenario.base, value], col = "black")
  legendText <- c(legendText, "2010")
  # print(legendText)
  axis(1, at = 1:length(unique(dt.GDP.2010.ref$region_code.IMPACT159)), labels = unique(dt.GDP.2010.ref$region_code.IMPACT159), cex.axis = 0.5, padj = -3)
  abline(h = 1, lty = 3, lwd = 0.8)
  legendText <- gsub("-REF", "", legendText)
  legend(x = "topright", y = NULL, legend = legendText, bty = "n", pch = 20,
         col = colorList, text.col = "black", cex = .5, pt.cex = .5, pt.lwd = 1,
         y.intersp = .8)
  dev.off()
}

# error bars-----
plotByRegionErrorBars <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, oneLine) {
  print(paste("plotting lines with error bars by region ", aggChoice, "for ", plotTitle))
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  if (gdxChoice == "SSPs") colorList <- c("black", "red", "red3", "red4", "green", "green3", "green4")
  if (gdxChoice == "USAID") colorList <- c("black", rainbow(10)[1:length(scenarios) - 1])
  legendText <- unique(gsub("-REF", "", scenarios))

  # initial attempts to use ggplot
  temp[, scenario := gsub("-REF", "", scenario)]
  # temp <- temp[order(region_code)]
  regionNameOrder <- c("Low income", "Lower middle income", "Upper middle income", "High income")
  scenarioNameOrder <- c("2010", "SSP2-NoCC", "SSP1-NoCC", "SSP3-NoCC", "SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
  scenario.econ <- c("SSP2-NoCC", "SSP1-NoCC", "SSP3-NoCC")
  scenario.clim <- c("SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
  min.econ <- temp[scenario %in% scenario.econ,  min(value), by = c("region_code")]
  temp[scenario %in% scenario.econ, value.max.econ := max(value), by = c("region_code")]
  temp[scenario %in% scenario.clim, value.min.clim := min(value), by = c("region_code")]
  temp[scenario %in% scenario.clim, value.max.clim := max(value), by = c("region_code")]
  temp[scenario %in% "2010", value.min.econ := value]
  temp[scenario %in% "2010", value.max.econ := value]
  temp[scenario %in% "2010", value.min.clim := value]
  temp[scenario %in% "2010", value.max.clim := value]
  #

  #draw lines
  temp2 <- temp[scenario %in% c("2010", "SSP2-NoCC") & region_code %in% "lowInc",]
  ggplot(temp2, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line(aes(group = 1)) +
    geom_point(size = 2) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(plotTitle) +
    ylim(yRange) +
    labs(y = yLab, x = NULL) +
    geom_errorbar(aes(ymin = value.min.econ, ymax = value.max.econ),color = "green",
                  size = 2, width = 0.2)
}

# nutStackedBarGraph
# plotByRegionBar <- function(dt, fileName, title, yLab, yRange, aggChoice) {
#   print(paste("plotting bars by region", aggChoice, "for", title))
#   temp <- copy(dt)
#   regionCodes <- unique(temp$region_code)
#   regionNames <- unique(temp$region_name)
#   scenarios <- unique(temp$scenario)
#   if (gdxChoice == "SSPs") colList <- c("black", "red", "red2", "red4", "green", "green2", "green4")
#   if (gdxChoice == "USAID") colList <- c("black", rainbow(10)[1:length(scenarios) - 1])
#   legendText <- unique(gsub("-REF", "", scenarios))
#   #the use of factor and levels keeps the order of the regions in region_code
#   formula.wide <- "scenario ~ factor(region_code, levels=unique(region_code))"
#   temp.wide <- data.table::dcast(
#     data = temp,
#     formula = formula.wide,
#     value.var = "value")
#
#   # order the scenarios
#   temp.wide[, scenarioOrder := match(scenario, scenarios)]
#   data.table::setorder(temp.wide, scenarioOrder)
#   temp.wide[, scenarioOrder := NULL]
#   data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
#
#   # remove "-REF" from the base scenarios
#   temp.wide[, scenario := gsub("-REF", "", scenario)]
#
#   #convert to data frame to make the matrix setup easier
#   temp <- as.data.frame(temp.wide)
#
#   # use first column for row names and delete it
#   rownames(temp) <- temp[,1]
#   temp[1] = NULL
#   temp <- data.matrix(temp)
#
#   #create the pdf of the graph
#   browser()
#   pdf(paste("graphics/", fileName, ".", aggChoice, ".pdf", sep = ""))
#   #Create barplots with the barplot(height) function, where height is a vector or matrix.
#   #If height is a vector, the values determine the heights of the bars in the plot.
#   #If height is a matrix and the option beside=FALSE then each bar of the plot corresponds to a column of height,
#   # with the values in the column giving the heights of stacked “sub-bars”.
#   #If height is a matrix and beside=TRUE, then the values in each column are juxtaposed rather than stacked.
#   # Include option names.arg=(character vector) to label the bars. The option horiz=TRUE to create a
#   # horizontal barplot.
#
#   # mat = matrix(c(1,2))
#   # layout(mat, heights = c(6,3))
#   # barplot(nuts.matrix.transpose, main = barTitleMain, xlab = "Years", names.arg = gsub("X", "",years),
#   #         col = colList, border = NA)
#
#   barplot(temp,  col = colList, ylim = yRange, xpd = FALSE,
#           legend.text = rownames(temp), args.legend = list(cex = .5, x = "topright"),
#           beside = TRUE, ylab = yLab,  cex.names = .7, las = 2,  main = title)
#   colsToRound <- names(temp.wide)[2:length(temp.wide)]
#   temp.wide[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
#   data.table::setnames(temp.wide, old = names(temp.wide), new = c("scenario", regionCodes))
#   textplot(temp.wide, cex = 0.6, valign = "top", show.rownames = FALSE, mai = c(.5, .5, .5, .5))
#   dev.off()
#   write.csv(temp.wide, file = paste("graphics/", fileName, ".", aggChoice, ".csv", sep = ""))
#   print(paste("Done plotting bars by region", aggChoice, "for", title))
#   print(" ")
#
# # reqType <- "kcal_ratios"
# # temp <- nutReqDataPrep(reqType, countryCode, scenarioName, years)
# #
# # nutListShort <- temp[[2]]
# # inputData <- temp[[1]]
# # nuts.matrix <- as.matrix(inputData[year %in% (years), 2:ncol(inputData), with = FALSE])
# # nuts.matrix.transpose <- t(nuts.matrix)
# # colList <- c("grey","green","red", "blue")
# # barTitleMain <- sprintf("Ratio of macronutrients in energy intake for %s \n SSP scenario: %s, climate model: %s, experiment %s",
# #                         countryNameLookup(countryCode), SSP, climModel, experiment)
# # # mainTitle = "Share of macronutrients in total energy consumption"
# # # subTitle <- paste("Country:" , country,
# # #                   ", Climate model:", climModel,
# # #                   "\nSocioeconomic scenario:", SSP,
# # #                   ", Experiment:", experiment, sep = " ")
# # barplot(nuts.matrix.transpose, main = barTitleMain, xlab = "Years", names.arg = gsub("X", "",years),
# #         col = colList, border = NA)
# # legendText <- gsub("_g", "", nutListShort)
# # legend("bottomright", legend = legendText, text.col = "black", cex = .6, pt.cex = .6,
# #        pt.lwd = 1, pch = 20,
# #        col = colList)
#
# }
