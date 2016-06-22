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
nutSpiderGraph <- function(reqType,country, scenario, climModel) {
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
    reqType <- "Share of Kcals"
  }

  idVars <- c("scenario","climate_model", region, "nutrientReq")
  measureVars <- keyVariable("keepYearList")
  reqRatios.long <- data.table::melt(
    data = reqRatios,  id.vars = idVars, measure.vars = measureVars, variable.name = "year",
    value.name = "value", variable.factor = FALSE)
  reqRatios.wide <- data.table::dcast.data.table(
    reqRatios.long,
    scenario + climate_model + region_code.IMPACT159 +  year ~ nutrientReq,
    value.var = "value")

  nutList <- names(reqRatios.wide)[5:ncol(reqRatios.wide)]
  nutListShort <- gsub("_reqRatio","",nutList)
  nutListShort <- gsub("vit_","vit ",nutListShort)
  nutListShort <- gsub("_µg","",nutListShort)
  nutListShort <- gsub("_mg","",nutListShort)
  nutListShort <- gsub("_rae"," rae",nutListShort)
  nutListShort <- gsub("_g"," ",nutListShort)

  i <- country
  j <- scenario
  k <- climModel
  #l <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
  l <- c("X2010","X2030","X2050")
  reqRatios.wide.nuts <- reqRatios.wide[region_code.IMPACT159 == i & scenario == j & climate_model == k & year %in% l, c("year",nutList), with = FALSE]
  data.table::setnames(reqRatios.wide.nuts,old = names(reqRatios.wide.nuts), new = gsub("_reqRatio","",names(reqRatios.wide.nuts)))

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
  temp <- temp[,(changeCols) := round(.SD,1), .SDcols = changeCols]

  #colMaxs <- temp[, lapply(.SD, max, na.rm=TRUE)] [,year := "Max" ]

  # par(mar = c(1, 1, 2, 1)) #decrease default margin
  # layout(matrix(1:4, ncol = 2)) #draw 4 plots to device
  # #loop over rows to draw them, add 1 as max and 0 as min for each var
  # chartTitle <- paste("Share of RDA\nmacro requirements for",i, j,  k,"\n", l, sep = " ")
  # lapply(1:5, function(m) {
  #   radarchart(rbind(colMins, colMaxs, reqRatios.wide.nuts[m,]),
  #              title = chartTitle[m],
  #              vlabels = nutListShort,
  #              vlcex = .75,
  #              palcex = .75)
  # })
  chartTitle <- paste(reqType,"for\n", i, j, k, sep = " ")
  #temp1 <- rbind(colMins, colMaxs, reqRatioRow, reqRatios.wide.nuts)
  colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
  colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )
  lineType <- c(3, 1, 1, 1)
  radarchart(temp[,!1, with = FALSE], axistype = 2,
             title = chartTitle,
             vlabels = nutListShort,
             seg = cMax,
             #custom polygon
             pcol = colors_border, plwd = 1, plty = lineType,
             #custom the grid
             cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
             #custom labels
             vlcex = 0.8
  )
  legend(x = 1.1, y = -.5, legend = gsub("X","",temp[3:nrow(temp),year]), bty = "n", pch = 20,
         col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
         y.intersp = .8)
}

#nutSpiderGraph(reqType,country, scenario, climModel)

#reqType choices are RDA_macro, RDA_vits, RDA_minrls, EAR, UL _vits, UL_minrls, kcal_ratios
country = "USA"
scenario = "SSP2"
climModel = "GFDL"
plot.new()
par(mar = c(1, 1, 2, 1)) #decrease default margin
layout(matrix(1:6, ncol = 2)) #draw 4 plots to device
title(paste("Ratio of consumption to requirement for\n",
            "country: ", country,
            ", economic and population scenario: ", scenario,
            ", climate model: ", climModel, sep = ""))

nutSpiderGraph("RDA_macro", country, scenario, climModel)
nutSpiderGraph("RDA_vits", country, scenario, climModel)
nutSpiderGraph("RDA_minrls", country, scenario, climModel)
nutSpiderGraph("UL_minrls", country, scenario, climModel)
nutSpiderGraph("UL_vits", country, scenario, climModel)
nutSpiderGraph("kcal_ratios", country, scenario, climModel)
