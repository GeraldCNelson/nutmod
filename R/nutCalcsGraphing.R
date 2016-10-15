#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2015-2016 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

#' @description To be added

#' @include nutrientModFunctions.R
#' @include nutrientCalcFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

# region <- keyVariable("region")
reqsList <- keyVariable("reqsListPercap")
# exclude the following from the graphing
deleteList <- c("req.EAR.percap","req.UL.vits.percap",
                "req.UL.minrls.percap", "req.AMDR.hi","req.AMDR.lo")

scenario <- "SSP2-GFDL" # for testing
scenarioResults <- function(scenario) {}
for (req in reqsList) {
  #req <- "req.UL.vits.percap" # for testing
  #get just nutrient list from req
  temp <- gsub("req.","",req); reqShortName <- gsub(".percap","",temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resultsDir"))
  # get per capita consumption of each nutrient
  #dt.nuts.sum <- getNewestVersion("all.sum",fileloc("resultsDir"))

  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]

  #set up initial worksheets for the ith set of requirements -----
  tmp <- f.createGeneralWorksheet()
  # this structure is needed to get two items out of the function
  #wbGeneral sets up a workbook with some basic information
  wbGeneral <- tmp[[1]]
  # df.wbInfoGeneral is a data frame
  df.wbInfoGeneral <- tmp[[2]]
  #add a worksheet describing the nutrients in this requirement
  openxlsx::addWorksheet(wbGeneral, sheetName = reqShortName)
  openxlsx::writeData(
    wbGeneral,
    dt.nutsReqPerCap,
    sheet = reqShortName,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE)

  f.addStyle(wbGeneral, numStyle, dt.nutsReqPerCap, sheetName = reqShortName)
  openxlsx::setColWidths(wbGeneral, sheet = reqShortName, cols = 1:ncol(dt.nutsReqPerCap), widths = "auto")
  df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
    c(reqShortName, paste("metadata on nutrients included in ", reqShortName))

  # add data to the spreadsheet -----
  for (j in 1:length(nutList)) {
    # keep only columns relevant to the current nutrient
    data.table::setkey(dt.food.agg)
    keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "food_group_code", "staple_code",
                     "foodAvailpDay",names(dt.food.agg)[grepl((nutList[j]),names(dt.food.agg))])
    dt.temp.nut <- dt.food.agg[, keepListCol, with = FALSE]
    for (k in c("staple","FG","all")) {
      keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "food_group_code", "staple_code",
                       "foodAvailpDay", names(dt.temp.nut)[grepl(k,names(dt.temp.nut))])
      dt.temp.group <- dt.temp.nut[, keepListCol, with = FALSE]
      # add the results to the spreadsheet
      sheetName <- paste(nutList[j], k, sep = ".")
      openxlsx::addWorksheet(wbGeneral, sheetName)
      openxlsx::writeData(wbGeneral, dt.temp.group, sheet = sheetName,
        startRow = 1, startCol = 1, rowNames = FALSE, withFilter = TRUE)
      openxlsx::setColWidths(
        wbGeneral, sheet = sheetName, cols = 1, widths = "auto", ignoreMergedCells = FALSE)

      f.addStyle(wbGeneral, numStyle, dt.temp.group, sheetName)

      # code to create a graph of the nutrient ratios for each of the nutrients
      # in the current requirements list for each scenario
      dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
      scenarioListIMPACT <- dt.scenarioListIMPACT$scenario
      for (scenario.name in scenarioListIMPACT) {
        #  nutrient.name <- sub("^(.*)_.*$", "\\1", nutList[j])
        nutrient.name <- nutList[j]
        # temp <-
        #   dt.food.ratio.long[eval(data.table::like(nutrient, "ratio")) & eval(data.table::like(nutrient, nutrient.name)) &
        #                        scenario %in% scenario.name, ]

        dt.temp <- data.table::copy(dt.temp.nut[scenario == scenario.name,])
        #ggplot wants the x axis to be numeric
        dt.temp[,year := as.numeric(gsub("X", "", year))]
        plotTitle <- paste("Requirement share, ", nutrient.name, ", ", scenario.name,sep = "")
        gElements <- paste("color = 'black', size = 9, vjust = 0.5")
        gg <- ggplot2::ggplot(data = dt.temp,
                              mapping = eval(ggplot2::aes(x = year, y = nut_req,  color = get(region)))) +
          eval(ggplot2::geom_line()) +
          eval(ggplot2::theme(
            axis.text.x = eval(ggplot2::element_text(gElements)),
            axis.text.y = eval(ggplot2::element_text(gElements)),
            axis.title.y = eval(ggplot2::element_text(gElements)),
            legend.text = eval(ggplot2::element_text(color = "black", size = 7, vjust = 0.5)),
            plot.title =  eval(ggplot2::element_text(color = "black", face = "bold", size = 11, hjust = 0.5, vjust = 1)),
            panel.background = eval(ggplot2::element_blank()),
            panel.border = eval(ggplot2::element_rect(linetype = "solid", colour = "black", fill = NA)),
            legend.position = "bottom",
            legend.title = eval(ggplot2::element_blank()),
            legend.key = eval(ggplot2::element_rect(fill = "white")),
            legend.background = eval(ggplot2::element_rect(fill = NA)))) +
          eval(ggplot2::xlab("year")) + eval(ggplot2::ylab(paste("share of",nutList[j] ,"requirement"))) +
          eval(ggplot2::ggtitle(plotTitle))  +
          eval(ggplot2::guides(color = eval(ggplot2::guide_legend(nrow = 5, byrow = TRUE))))

        # add plot to the spreadsheet
        plotSheet <- paste(nutrient.name,scenario.name, sep = ".")
        print(plotSheet)
        openxlsx::addWorksheet(wbGeneral, sheetName = plotSheet)
        print(gg) # the figure needs to be showing for the next step to work
        openxlsx::insertPlot(wbGeneral, sheet = plotSheet, width = 9, height = 5, units = "in", startRow = 5, fileType = "png")

        df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
          c(nutList[j], paste("Ratio results for ", nutrient.name))
        # cMax.all <- data.table::as.data.table(colMax(dt.food.agg$folate_µg.ratio.all))
        # cMin.all <- data.table::as.data.table(colMin(dt.food.reqs.ratio))
        temp.cMax <- paste(k, "req.ratio.cMax", sep = ".")
        temp.cMin <- paste(k, "req.ratio.cMin", sep = ".")
        temp.cMax <- getNewestVersion("temp.cMax", fileloc("resultsDir"))
        temp.cMin <- getNewestVersion("temp.cMin", fileloc("resultsDir"))

        #write the rows that have the max and min values for the current nutrient
        # eval parse needed here to get the which to run. uggh.
        # This displays the row num of the row with the max/min value
        # maxRowNum <- which(dt.food.ratio[, eval(parse(text = colnames(cMax)[j]))] ==
        #                      cMax.food[1, eval(parse(text = colnames(cMax.food)[j]))])
        # minRowNum <- which(dt.food.ratio[, eval(parse(text = colnames(cMin)[j]))] ==
        #                      cMin.food[1, eval(parse(text = colnames(cMin.food)[j]))])
        openxlsx::writeData(wbGeneral, temp.cMax[,1], sheet = plotSheet,
                            startRow = 1, startCol = 2, rowNames = FALSE, withFilter = FALSE)
        openxlsx::writeData(wbGeneral,
                            paste("Country and year with max value for the ",
                                  nutrient.name, " ratio", sep = ""), sheet = plotSheet,
                            startRow = 1, startCol = 1, rowNames = FALSE, withFilter = FALSE)
        openxlsx::writeData(wbGeneral, temp.cMin[,1], sheet = plotSheet,
                            startRow = 3, startCol = 2, rowNames = FALSE, withFilter = FALSE)
        openxlsx::writeData(wbGeneral,
                            paste("Country and year with min value for the ",
                                  nutrient.name, " ratio", sep = ""), sheet = plotSheet,
                            startRow = 3, startCol = 1, rowNames = FALSE, withFilter = FALSE)
        openxlsx::addStyle(wbGeneral, sheet = plotSheet, style = numStyle, rows = 1:4,
                           cols = 2:ncol(dt.food.tot), gridExpand = TRUE)
        openxlsx::setColWidths(wbGeneral, sheet = plotSheet,
                               cols = 1:ncol(dt.food.tot[minRowNum, ]), widths = "auto", ignoreMergedCells = FALSE)
        df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
          c(plotSheet,
            paste("Max/min rows and graph of results for ", nutrient.name))
      }
    }
    f.finalizeWB(wbGeneral, df.wbInfoGeneral, req)
  }
}
