#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, region management
#' @title Draw spider diagrams
#' @name xxx
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

library(fmsb)
library(radarchart)
region <- keyVariable("region")

datasetup <- function(reqType,country, SSP, climModel, experiment, years) {

  scenarioName <- paste(SSP, climModel, experiment, sep = "-")
  scenarioListIMPACT <- keyVariable("scenarioListIMPACT")
  errorMessage <- cbind(paste("Your combination of ", scenarioName,
                              "is not allowed. Please choose from one of the following combinations: ", sep = " "),
                        paste(shQuote(as.character(scenarioListIMPACT), type = "sh"), collapse = ", "))
  if (experiment %in% "REF" & !scenarioName %in% c("SSP2-HGEM2-REF", "SSP2-IPSL2-REF", "SSP2-NoCC-REF")) {
    stop(errorMessage)
  }
  if (!experiment %in% "REF" & !scenarioName %in% scenarioListIMPACT) {
    stop(errorMessage)
  }
  # need to do this to match up with what the experiment names are in the data files
  if (experiment %in% "IRREXP-WUE2") {experiment = "IRREXP_WUE2"}
  if (experiment %in% "PHL-DEV2") {experiment = "PHL_DEV2"}
  scenarioName <- paste(SSP, climModel, experiment, sep = "-")

  if (reqType == "RDA_macro") {
    reqRatios <- getNewestVersion("RDA.macro.sum.req.ratio", fileloc("resData"))
    reqType <- "RDA, macronutrients"
  }
  if (reqType == "RDA_vits") {
    reqRatios <- getNewestVersion("RDA.vits.sum.req.ratio", fileloc("resData"))
    reqType <- "RDA, vitamins"
  }
  if (reqType == "RDA_minrls") {
    reqRatios <- getNewestVersion("RDA.minrls.sum.req.ratio", fileloc("resData"))
    reqType <- "RDA, minerals"
  }
  if (reqType == "EAR") {
    reqRatios <- getNewestVersion("RDA.EAR.sum.req.ratio", fileloc("resData"))
    reqType <- "EAR"
  }
  if (reqType == "UL_vits") {
    reqRatios <- getNewestVersion("UL.vits.sum.req.ratio", fileloc("resData"))
    reqType <- "UL, vitamins"
  }
  if (reqType == "UL_minrls") {
    reqRatios <- getNewestVersion("UL.minrls.sum.req.ratio", fileloc("resData"))
    reqType <- "UL, minerals"
  }

  if (reqType == "kcal_ratios") {
    reqRatios <- getNewestVersion("dt.energy.ratios", fileloc("resData"))
    reqRatios <- reqRatios[!nutrientReq %in% "sugar_g_reqRatio",]
    reqType <- "Share of Kcals"
  }

  idVars <- c("scenario", "SSP","climate_model", "experiment", region, "nutrientReq")
  measureVars <- keyVariable("keepYearList")
  reqRatios.long <- data.table::melt(
    data = reqRatios,  id.vars = idVars, measure.vars = measureVars, variable.name = "year",
    value.name = "value", variable.factor = FALSE)

  formula.ratios <- paste("scenario + SSP + climate_model + experiment + ", region, "  +  year ~ nutrientReq")
  reqRatios.wide <- data.table::dcast(
    reqRatios.long,
    formula = formula.ratios,
    value.var = "value")

  nutList <- unique(reqRatios$nutrientReq)
  nutListShort <- gsub("_reqRatio","",nutList)
  nutListShort <- gsub("vit_","vit ",nutListShort)
  nutListShort <- gsub("_µg","",nutListShort)
  nutListShort <- gsub("_mg","",nutListShort)
  nutListShort <- gsub("_rae"," rae",nutListShort)
  nutListShort <- gsub("_g"," ",nutListShort)
  nutListShort <- gsub("totalfiber","total fiber",nutListShort)

  i <- country
  j <- SSP
  k <- climModel
  m <- experiment
  l <- years

  #l <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
  reqRatios.wide.nuts <- reqRatios.wide[region_code.IMPACT159 %in% i &
                                          SSP %in% j &
                                          climate_model %in% k &
                                          experiment == m &
                                          year %in% l,
                                        c( "year",nutList), with = FALSE]
  data.table::setnames(reqRatios.wide.nuts,old = names(reqRatios.wide.nuts),
                       new = gsub("_reqRatio","",names(reqRatios.wide.nuts)))

  # include the requirement ratio as a row in calculating the col mins and maxs
  reqRatioRow <- as.list(c("Req",rep(1,ncol(reqRatios.wide.nuts) - 1)))
  temp <- rbind(reqRatios.wide.nuts, reqRatioRow)
  changeCols <- colnames(temp)[2:ncol(temp)]
  temp[is.nan(get(changeCols)),  (changeCols) := 0, with = FALSE]
  colMins <- as.list(c("Min", rep.int(0,ncol(reqRatios.wide.nuts) - 1)))
  temp[, (changeCols) := lapply(.SD, as.numeric), .SDcols = changeCols]
  # next few lines to get maximum ratio value for all the nutrients
  temp[, nmax := max(.SD), .SDcols = colnames(temp)[2:ncol(temp)]]
  cMax <- round(temp[1,nmax])
  temp[,nmax := NULL]
  colMaxs <- as.list(c("Max",rep.int(cMax,ncol(reqRatios.wide.nuts) - 1)))

  temp <- rbind(colMaxs, colMins, reqRatioRow, reqRatios.wide.nuts)
  temp[, (changeCols) := lapply(.SD, as.numeric), .SDcols = changeCols]
  temp <- temp[,(changeCols) := round(.SD,2), .SDcols = changeCols]
  return(list(temp, nutListShort))
}

chartTitleCleanup <- function(reqType) {
  if (reqType == "RDA_macro")  {fullName <- "RDA, macronutrients"}
  if (reqType == "RDA_vits")   {fullName <- "RDA, vitamins"}
  if (reqType == "RDA_minrls") {fullName <- "RDA, minerals"}
  if (reqType == "EAR")        {fullName <- "Estimated average requirements"}
  if (reqType == "UL_minrls") {fullName <- "Upper limit, minerals"}
  if (reqType == "UL_vits") {fullName <- "Upper limit, vitamins"}
  if (reqType == "kcal_ratios") {fullName <- "nutrient share in energy intake"}
  return(fullName)
}

nutSpiderGraph <- function(reqType, country, SSP, climModel, experiment, years) {
  temp <- datasetup(reqType,country, SSP, climModel, experiment, years)
  nutListShort <- temp[[2]]
  inputData <- temp[[1]]
  head(inputData)
  chartTitle <- chartTitleCleanup(reqType)

  #temp1 <- rbind(colMins, colMaxs, reqRatioRow, reqRatios.wide.nuts)
  colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
  colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )
  lineType <- c(3, 1, 1, 1)
  radarchart(inputData[,!1, with = FALSE], axistype = 2,
             title = chartTitle,
             vlabels = nutListShort,
             seg = 3,
             #custom polygon
             pcol = colors_border, plwd = 1, plty = lineType,
             #custom the grid
             cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
             #custom labels
             vlcex = 0.8
  )
  legend(x = 1.2, y = -.55, legend = gsub("X","",inputData[3:nrow(inputData),years]), bty = "n", pch = 20,
         col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
         y.intersp = .8)
}

#nutSpiderGraph(reqType,country, scenario, climModel)

#reqType choices are RDA_macro, RDA_vits, RDA_minrls, EAR, UL _vits, UL_minrls, kcal_ratios
country = "KEN"
SSP = "SSP2" # the only choice at the moment
climModel = "HGEM2" # choices are "HGEM"  "HGEM2" "IPSL"  "IPSL2" "NoCC"
experiment = "REF" # choices are "HiNARS2", "HiREFF2", "HiYld2", "IRREXP2", "IRREXP-WUE2", "LoYld2", "RegYld2", "SWHC2", "REF", PHL-DEV2
years <- c("X2010","X2030","X2050")
# note: the combination of SSP, climModel, and experiment must come from the following list
# SSP2-HGEM-HiNARS2     SSP2-HGEM-HiREFF2     SSP2-HGEM-HiYld2      SSP2-HGEM-IRREXP_WUE2
# SSP2-HGEM-IRREXP2     SSP2-HGEM-LoYld2      SSP2-HGEM-PHL-DEV2    SSP2-HGEM-RegYld2
# SSP2-HGEM-SWHC2       SSP2-HGEM2            SSP2-IPSL-IRREXP-WUE2 SSP2-IPSL-IRREXP2
# SSP2-IPSL-SWHC2       SSP2-IPSL2            SSP2-NoCC-IRREXP-WUE2 SSP2-NoCC-IRREXP2
# SSP2-NoCC-SWHC2       SSP2-NoCC

plot.new()
par(mar = c(1, 1, 5, 1)) #decrease default margin
layout(matrix(1:6, ncol = 2, byrow = TRUE)) #draw 4 plots to device

nutSpiderGraph("RDA_macro", country, SSP, climModel, experiment, years)
nutSpiderGraph("RDA_vits", country, SSP, climModel, experiment, years)
nutSpiderGraph("RDA_minrls", country, SSP, climModel, experiment, years)
nutSpiderGraph("UL_minrls", country, SSP, climModel, experiment, years)
nutSpiderGraph("UL_vits", country, SSP, climModel, experiment, years)
nutSpiderGraph("kcal_ratios", country, SSP, climModel, experiment, years)
titleText <- sprintf("Ratio of consumption to requirement for %s \n SSP scenario: %s, climate model: %s, experiment %s",
                     countryNameLookup(country), SSP, climModel, experiment)
title(paste(titleText), outer = TRUE, line = -2)

# nutStackedBarGraph
reqType <- "kcal_ratios"
temp <- datasetup(reqType,country, SSP, climModel, experiment, years)
nutListShort <- temp[[2]]
inputData <- temp[[1]]
nuts.matrix <- as.matrix(inputData[year %in% (l),2:ncol(reqRatios.wide.nuts), with = FALSE])
temp <- t(nuts.matrix)
colList <- c("grey","green","red")
barTitleMain <- sprintf("Ratio of macronutrients in energy intake for %s \n SSP scenario: %s, climate model: %s, experiment %s",
                        countryNameLookup(country), SSP, climModel, experiment)
# mainTitle = "Share of macronutrients in total energy consumption"
# subTitle <- paste("Country:" , country,
#                   ", Climate model:", climModel,
#                   "\nSocioeconomic scenario:", SSP,
#                   ", Experiment:", experiment, sep = " ")
barplot(temp, main = barTitleMain, xlab = "Years", names.arg = gsub("X", "",l),
        col = colList, border = NA)
legendText <- gsub("_g", "", nutListShort)
legend("topleft", legend = legendText, text.col = "black", cex = .6, pt.cex = .6,
       pt.lwd = 1, pch = 20,
       col = colList)

# reqRatioDelta

