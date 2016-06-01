#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2015 Gerald C. Nelson, except where noted

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
if (!exists("getNewestVersion", mode = "function")) {
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}
options(warn = 1) # can be deleted after development is finished. This changes warnings to errors and stops execution.
# choose a grouping of countries -----
region <- keyVariable("region")
reqList <-
  c(
    "req.EAR.percap",
    "req.RDA.vits.percap" ,
    "req.RDA.minrls.percap",
    "req.RDA.macro.percap",
    "req.UL.vits.percap",
    "req.UL.minrls.percap"
  )

req <- "req.UL.vits.percap" # for testing
#get just nutrient list from req
temp <- gsub("req.","",req)
reqShortName <- gsub(".percap","",temp)
temp <- paste("food.agg.", reqShortName, sep = "")
dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
# get per capita consumption of each nutrient
#dt.nuts.sum <- getNewestVersion("all.sum",fileloc("resData"))

dt.nutsReqPerCap <- getNewestVersion(req)
# get list of nutrients from dt.nutsReqPerCap for the req set of requirements
nutList <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]

# # now prepare for creating rds and excel files (if they are not too big);
# # one each for all food items, staples, and food groups ------
#
# # individual food function
# f.ratios.all <- function(region,dt.food.agg,nutList){
#   basicKey <- c("scenario",region,"year")
#   sumKey <-   c(basicKey,"IMPACT_code")
#
#   # the total daily consumption of each nutrient
#   nutList.sum.all <-       paste(nutList, "sum.all", sep = ".")
#   # the ratio of daily consumption of each nutrient to the total consumption
#   nutList.ratio.all <-     paste(nutList, "ratio.all", sep = ".")
#   # the ratio of daily consumption of each nutrient by the nutrient requirement
#   nutList.req.ratio.all <- paste(nutList, "req.ratio.all", sep = ".")
#   # the list of columns to keep for each group of data tables
#   keepListCol.sum.all <-       c(basicKey,nutList.sum.all)
#   keepListCol.ratio.all <-     c(sumKey,nutList.ratio.all)
#   keepListCol.req.ratio.all <- c(sumKey,nutList.req.ratio.all)
#
#   # create the data table and remove unneeded columns
#   dt.all.sum <- unique(dt.food.agg[,       keepListCol.sum.all, with =  FALSE])
#   dt.all.ratio <- unique(dt.food.agg[,     keepListCol.ratio.all, with =  FALSE])
#   dt.all.req.ratio <- unique(dt.food.agg[, keepListCol.req.ratio.all, with =  FALSE])
#   #reshape the results to get years in columns
#   dt.all.sum.long <- data.table::melt(
#     dt.all.sum,   id.vars = basicKey, measure.vars = nutList.sum.all, variable.name = "nutrient",
#     value.name = "nut_share", variable.factor = FALSE)
#   dt.all.ratio.long <- data.table::melt(
#     dt.all.ratio, id.vars = sumKey, measure.vars = nutList.ratio.all, variable.name = "nutrient",
#     value.name = "nut_share", variable.factor = FALSE)
#   dt.all.req.ratio.long <- data.table::melt(
#     dt.all.req.ratio,
#     id.vars = sumKey,
#     measure.vars = nutList.req.ratio.all,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE)
#   formula.sum.all <- paste("scenario + ", region, " + nutrient  ~ year")
#   dt.all.sum.wide <- data.table::dcast.data.table(
#     data = dt.all.sum.long,
#     formula = formula.sum.all,
#     value.var = "nut_share",
#     variable.factor = FALSE)
#
#   formula.ratio.all <- paste("scenario + ", region, "  + nutrient + IMPACT_code  ~ year")
#
#   dt.all.ratio.wide <- data.table::dcast(
#     data = dt.all.ratio.long,
#     formula = formula.ratio.all,
#     value.var = "nut_share")
#   dt.all.req.ratio.wide <- data.table::dcast(
#     data = dt.all.req.ratio.long,
#     formula = formula.ratio.all,
#     value.var = "nut_share")
#   inDT <- dt.all.sum.wide
#   outName <- "all.sum"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- dt.all.ratio.wide
#   outName <- "all.ratio"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- dt.all.req.ratio.wide
#   outName <- "all.req.ratio"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- data.table::as.data.table(colMax(dt.all.req.ratio.wide))
#   outName <- "all.req.ratio.cMax"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- data.table::as.data.table(colMin(dt.all.req.ratio.wide))
#   outName <- "all.req.ratio.cMin"
#   cleanup(inDT,outName, fileloc("resData"))
# }
#
# # foodGroup function
# f.ratios.FG <- function(region,dt.food.agg,nutList) {
#   basicKey <-     c("scenario",region,"year")
#   foodGroupKey <- c(basicKey, "food.group.code")
#   nutList.sum.foodGroup <-       paste(nutList, "sum.foodGroup", sep = ".")
#   nutList.ratio.foodGroup <-     paste(nutList, "ratio.foodGroup", sep = ".")
#   nutList.req.ratio.foodGroup <- paste(nutList, "req.ratio.foodGroup", sep = ".")
#   keepListCol.sum.foodGroup <-       c(foodGroupKey,nutList.sum.foodGroup)
#   keepListCol.ratio.foodGroup <-     c(foodGroupKey,nutList.ratio.foodGroup)
#   keepListCol.req.ratio.foodGroup <- c(foodGroupKey,nutList.req.ratio.foodGroup)
#   dt.foodGroup.sum <- unique(dt.food.agg[,       keepListCol.sum.foodGroup, with =  FALSE])
#   dt.foodGroup.ratio <- unique(dt.food.agg[,     keepListCol.ratio.foodGroup, with =  FALSE])
#   dt.foodGroup.req.ratio <- unique(dt.food.agg[, keepListCol.req.ratio.foodGroup, with =  FALSE])
#   #reshape the results to get years in columns
#   dt.foodGroup.sum.long <- data.table::melt(
#     dt.foodGroup.sum,
#     id.vars = foodGroupKey,
#     measure.vars = nutList.sum.foodGroup,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.foodGroup.ratio.long <- data.table::melt(
#     dt.foodGroup.ratio,
#     id.vars = foodGroupKey,
#     measure.vars = nutList.ratio.foodGroup,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.foodGroup.req.ratio.long <- data.table::melt(
#     dt.foodGroup.req.ratio,
#     id.vars = foodGroupKey,
#     measure.vars = nutList.req.ratio.foodGroup,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE
#   )
#   formula.foodGroup <- paste("scenario + ",region," + nutrient + food.group.code ~ year")
#   dt.foodGroup.sum.wide <- data.table::dcast.data.table(
#     data = dt.foodGroup.sum.long,
#     formula = formula.foodGroup,
#     value.var = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.foodGroup.ratio.wide <- data.table::dcast.data.table(
#     data = dt.foodGroup.ratio.long,
#     formula = formula.foodGroup,
#     value.var = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.foodGroup.req.ratio.wide <- data.table::dcast.data.table(
#     data = dt.foodGroup.req.ratio.long,
#     formula = formula.foodGroup,
#     value.var = "nut_share",
#     variable.factor = FALSE
#   )
#   inDT <- dt.foodGroup.sum.wide
#   outName <- "foodGroup.sum"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- dt.foodGroup.ratio.wide
#   outName <- "foodGroup.ratio"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- dt.foodGroup.req.ratio.wide
#   outName <- "foodGroup.req.ratio"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- data.table::as.data.table(colMax(dt.foodGroup.req.ratio.wide))
#   outName <- "foodGroup.req.ratio.cMax"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- data.table::as.data.table(colMin(dt.foodGroup.req.ratio.wide))
#   outName <- "foodGroup.req.ratio.cMin"
#   cleanup(inDT,outName, fileloc("resData"))
# }
#
# # staples function
# r.ratios.staples <- function(region,dt.food.agg,nutList) {
#   basicKey <-  c("scenario", region, "year")
#   stapleKey <- c(basicKey, "staple.code")
#   # the total daily consumption of each staple
#   nutList.sum.staple <-       paste(nutList, "sum.staple", sep = ".")
#   # the ratio of daily consumption of each nutrient for each staple to the total consumption
#   nutList.ratio.staple <-     paste(nutList, "ratio.staple", sep = ".")
#   nutList.req.ratio.staple <- paste(nutList, "req.ratio.staple", sep = ".")
#   keepListCol.sum.staple <-       c(stapleKey,nutList.sum.staple)
#   keepListCol.ratio.staple <-     c(stapleKey,nutList.ratio.staple)
#   keepListCol.req.ratio.staple <- c(stapleKey,nutList.req.ratio.staple)
#   dt.staples.sum <- unique(dt.food.agg[,       keepListCol.sum.staple, with =  FALSE])
#   dt.staples.ratio <- unique(dt.food.agg[,     keepListCol.ratio.staple, with =  FALSE])
#   dt.staples.req.ratio <- unique(dt.food.agg[, keepListCol.req.ratio.staple, with =  FALSE])
#   #reshape the results to get years in columns
#   dt.staples.sum.long <- data.table::melt(
#     dt.staples.sum,
#     id.vars = stapleKey,
#     measure.vars = nutList.sum.staple,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.staples.ratio.long <- data.table::melt(
#     dt.staples.ratio,
#     id.vars = stapleKey,
#     measure.vars = nutList.ratio.staple,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.staples.req.ratio.long <- data.table::melt(
#     dt.staples.req.ratio,
#     id.vars = stapleKey,
#     measure.vars = nutList.req.ratio.staple,
#     variable.name = "nutrient",
#     value.name = "nut_share",
#     variable.factor = FALSE
#   )
#   formula.staple <- paste("scenario + ",region," + nutrient + staple.code ~ year")
#   dt.staples.sum.wide <- data.table::dcast.data.table(
#     data = dt.staples.sum.long,
#     formula = formula.staple,
#     value.var = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.staples.ratio.wide <- data.table::dcast.data.table(
#     data = dt.staples.ratio.long,
#     formula = formula.staple,
#     value.var = "nut_share",
#     variable.factor = FALSE
#   )
#   dt.staples.req.ratio.wide <- data.table::dcast.data.table(
#     data = dt.staples.req.ratio.long,
#     formula = formula.staple,
#     value.var = "nut_share",
#     variable.factor = FALSE
#   )
#   inDT <- dt.staples.sum.wide
#   outName <- "staples.sum"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- dt.staples.ratio.wide
#   outName <- "staples.ratio"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- dt.staples.req.ratio.wide
#   outName <- "staples.req.ratio"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- data.table::as.data.table(colMax(dt.staples.req.ratio.wide))
#   outName <- "staples.req.ratio.cMax"
#   cleanup(inDT,outName, fileloc("resData"))
#
#   inDT <- data.table::as.data.table(colMin(dt.staples.req.ratio.wide))
#   outName <- "staples.req.ratio.cMin"
#   cleanup(inDT,outName, fileloc("resData"))
# }

# graphics code below -----

cMax.all <- getNewestVersion("all.req.ratio.cMax", fileloc("resData"))
cMin.all <- getNewestVersion("all.req.ratio.cMin", fileloc("resData"))

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

f.addStyle(wbGeneral, numStyle, dt.nutsReqPerCap, sheetName)
# openxlsx::addStyle(
#   wbGeneral,
#   sheet = reqShortName,
#   style = numStyle,
#   rows = 1:nrow(dt.nutsReqPerCap) + 1,
#   cols = 2:ncol(dt.nutsReqPerCap),
#   gridExpand = TRUE)

openxlsx::setColWidths(
  wbGeneral,
  sheet = reqShortName,
  cols = 1:ncol(dt.nutsReqPerCap),
  widths = "auto")

df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
  c(reqList[i], paste("metadata on nutrients included in ", reqShortName))

# add data to the spreadsheet -----
for (j in 1:length(nutList)) {
  # add an info sheet for the jth nutrient in the ith set of requirements
  temp <- dt.food.ratio.wide[nutrient %in% nutList.ratio[j],]
  # add the results to the spreadsheet
  openxlsx::addWorksheet(wbGeneral, sheetName = nutList[j])
  openxlsx::writeData(
    wbGeneral,
    temp,
    sheet = nutList[j],
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    withFilter = TRUE
  )
  openxlsx::setColWidths(
    wbGeneral,
    sheet = nutList[j],
    cols = 1,
    widths = "auto",
    ignoreMergedCells = FALSE
  )

  f.addStyle(wbGeneral, numStyle, temp, nutList[j])
  #   openxlsx::addStyle(
  # wbGeneral,
  # sheet = nutList[j],
  # style = numStyle,
  # rows = 1:nrow(temp),
  # cols = 2:ncol(temp),
  # gridExpand = TRUE
  #  )

  # code to create a graph of the nutrient ratios for each of the nutrients
  # in the current requirements list for a single scenario
  for (k in 1:length(IMPACTscenarioList)) {
    scenario.name <- IMPACTscenarioList[k]
    #  nutrient.name <- sub("^(.*)_.*$", "\\1", nutList[j])
    nutrient.name <- nutList[j]
    # temp <-
    #   dt.food.ratio.long[eval(data.table::like(nutrient, "ratio")) & eval(data.table::like(nutrient, nutrient.name)) &
    #                        scenario %in% scenario.name, ]
    dt.temp <- data.table::copy(dt.food.ratio.long[nutrient == nutrient.name & scenario == scenario.name,])
    #ggplot wants the x axis to be numeric
    dt.temp[,year := as.numeric(gsub("X", "", year))]
    plotTitle <- paste("Requirement share, ", nutrient.name, ", ", scenario.name,sep = "")
    gElements <- paste("color = 'black', size = 9, vjust = 0.5")
    gg <- ggplot2::ggplot(data = dt.temp,
                          mapping = eval(ggplot2::aes(x = year, y = nut_req,  color = get(region)))) +
      eval(ggplot2::geom_line()) +
      eval(ggplot2::theme(
        axis.text.x = eval(ggplot2::element_text(
          gElements
        )),
        axis.text.y = eval(ggplot2::element_text(
          gElements
        )),
        axis.title.y = eval(ggplot2::element_text(
          gElements
        )),
        legend.text = eval(ggplot2::element_text(
          color = "black", size = 7, vjust = 0.5
        )),
        plot.title = eval(ggplot2::element_text(
          color = "black", face = "bold", size = 11, hjust = 0.5, vjust = 1
        )),
        panel.background = eval(ggplot2::element_blank()),
        panel.border = eval(ggplot2::element_rect(
          linetype = "solid",
          colour = "black",
          fill = NA
        )),
        legend.position = "bottom",
        legend.title = eval(ggplot2::element_blank()),
        legend.key = eval(ggplot2::element_rect(fill = "white")),
        legend.background = eval(ggplot2::element_rect(fill = NA))
      )) +
      eval(ggplot2::xlab("year")) + eval(ggplot2::ylab(paste("share of",nutList[j] ,"requirement"))) +
      eval(ggplot2::ggtitle(plotTitle))  +
      eval(ggplot2::guides(color = eval(ggplot2::guide_legend(nrow = 5, byrow = TRUE))))

    # add plot to the spreadsheet
    plotSheet <- paste(nutrient.name,scenario.name, sep = ".")
    print(plotSheet)
    openxlsx::addWorksheet(wbGeneral, sheetName = plotSheet)
    print(gg) #needs to be showing for the next step to work
    openxlsx::insertPlot(
      wbGeneral,
      sheet = plotSheet, width = 9, height = 5, units = "in", startRow = 5,
      fileType = "png"
    )

    df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
      c(nutList[j], paste("Ratio results for ", nutrient.name))
    cMax.all <- data.table::as.data.table(colMax(dt.food.agg$folate_µg.ratio.all))
    cMin.all <- data.table::as.data.table(colMin(dt.food.reqs.ratio))

    #write the rows that have the max and min values for the current nutrient
    # eval parse needed here to get the which to run. uggh.
    # This displays the row num of the row with the max/min value
    maxRowNum <-
      which(dt.food.ratio[, eval(parse(text = colnames(cMax)[j]))] ==
              cMax.food[1, eval(parse(text = colnames(cMax.food)[j]))])
    minRowNum <-
      which(dt.food.ratio[, eval(parse(text = colnames(cMin)[j]))] ==
              cMin.food[1, eval(parse(text = colnames(cMin.food)[j]))])
    openxlsx::writeData(
      wbGeneral,
      dt.food.ratio[maxRowNum, ],
      sheet = plotSheet,
      startRow = 1,
      startCol = 2,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::writeData(wbGeneral,
                        paste("Country and year with max value for the ",
                              nutrient.name, " ratio", sep = ""), sheet = plotSheet, startRow = 1,
                        startCol = 1, rowNames = FALSE, withFilter = FALSE)
    openxlsx::writeData(wbGeneral, dt.food.ratio[minRowNum, ],
                        sheet = plotSheet,
                        startCol = 2, startRow = 3, rowNames = FALSE, withFilter = FALSE)
    openxlsx::writeData(wbGeneral,
                        paste("row with min value for the ", nutrient.name,
                              " ratio", sep = ""), sheet = plotSheet,
                        startCol = 1, startRow = 3,
                        rowNames = FALSE, withFilter = FALSE)
    openxlsx::addStyle(wbGeneral, sheet = plotSheet, style = numStyle, rows = 1:4,
                       cols = 2:ncol(dt.food.tot), gridExpand = TRUE)
    openxlsx::setColWidths(wbGeneral, sheet = plotSheet,
                           cols = 1:ncol(dt.food.tot[minRowNum, ]), widths = "auto", ignoreMergedCells = FALSE)
    df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
      c(plotSheet,
        paste("Max/min rows and graph of results for ", nutrient.name))
  }
}
f.finalizeWB(wbGeneral, df.wbInfoGeneral, reqList[i])

