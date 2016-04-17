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

#Copyright (C) 2015 Gerald C. Nelson, except where noted
#Important code contributions from Brendan Power.

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

#' @include nutrientModFunctions.R
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

# Read in all data first and standardize variable names -----
# Read in IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
# this should not be necessary
dt.IMPACTfood <- dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList"),]
dt.IMPACTfood <- dt.IMPACTfood[!get(region) == "GRL",] # neither should this
# get the list of scenarios in the IMPACT data for use below
IMPACTscenarioList <- unique(dt.IMPACTfood$scenario)
#IMPACTscenarioList <- IMPACTscenarioList[1] # just for testing. !!!XXX

# read in nutrients data and optionally apply cooking retention values -----
dt.nuts <- cookingRet("yes")
# get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
dt.nuts <- dt.nuts[!IMPACT_code %in% deleteListRow,]

# calculate the share of per capita income spent on IMPACT commodities
# do only if the data are mostly by country; ie the IMPACT3 regions
if (region == "region_code.IMPACT3") {budgetShare(dt.IMPACTfood,region)}

keepListCol <- c("scenario", "IMPACT_code", region, "FoodAvailability","year")
dt.IMPACTfood <- dt.IMPACTfood[, keepListCol, with = FALSE]

# convert food availability from per year to per day
dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")][,FoodAvailability := NULL]

# reqList is a list of the requirements types. Each has a different set of nutrients. These are a subset
# of what are in the nutrients requirements tables from IOM. They are the nutrients common to
# both the IOM and nutrient content lookup spreadsheet

# the .percap data are for a representative consumer

reqList <-
  c(
    "req.EAR.percap",
    "req.RDA.vits.percap" ,
    "req.RDA.minrls.percap",
    "req.RDA.macro.percap",
    "req.UL.vits.percap",
    "req.UL.minrls.percap"
  )
reqList <- reqList[1] # just for testing!!! XXX
#IMPACTscenarioList <- "SSP2-MIROC" # just for testing!!! XXX
req <- "req.EAR.percap" # just for testing!!! XXX

generateResults <- function(req,dt.IMPACTfood,IMPACTscenarioList,dt.nuts,region) {
  # use dt.food only in the function
  dt.food <- data.table::copy(dt.IMPACTfood)
  dt.food <- dt.food[scenario %in% IMPACTscenarioList,]
  # read in the nutrient requirements data that for a representative consumer -----
  # Note that these are for SSP categories and thus vary by SSP category and year for each region
  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]
  #nutList <- nutList[3:4] # Here just for testing. !!! be sure to comment out!!!XXX

  # dt.nutsReqPerCap has values for the 5 SSP scenarios. To align with the IMPACT data we need to
  # add the climate model name to the SSP scenario name (SSP1 - 5).
  # add copies of dt.nutsReqPerCap for each of the climate models
  # this for loop adds copies of the nutrient requirements for each climate model used. May end up
  # with more than needed because it has all 5 SSP scenarios.
  dt.temp <- data.table::copy(dt.nutsReqPerCap[FALSE,])
  for (i in IMPACTscenarioList) {
    climModel <- gsub(substr((i),1,5),"",i) # get climate model abbrev
    print(i)
    temp.nuts <- data.table::copy(dt.nutsReqPerCap)
    temp.nuts[,scenario := paste(scenario,climModel,sep = "-")]
    dt.temp <- rbind(dt.temp, temp.nuts)
  }
  # keep just the nutrient requirements scenarios that are in the IMPACT data
  #  and the nutrients in nutList. And reduce rows to just IMPACT scenarios
  keepListCol <- c("scenario",region,"year",nutList)
  dt.nutsReqPerCap <- dt.temp[,keepListCol, with = FALSE]
  dt.nutsReqPerCap <- dt.nutsReqPerCap[scenario %in% IMPACTscenarioList,]

  # reduce calculations to just the nutrients in nutList
  keepListCol <- c("IMPACT_code","food.group.code","staple.code",nutList)
  # use the data table dt.nutrients only in the function
  dt.nutrients <- data.table::copy(dt.nuts[,keepListCol, with = FALSE])
  # convert nutrients (in 100 grams of food) to nutrients per kg of food -----
  dt.nutrients[, (nutList) := lapply(.SD, function(x) (x * 10)), .SDcols = nutList]

  #combine the food availability info with the nutrients for each of the IMPACT commodities
  data.table::setkey(dt.nutrients, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario",region,"IMPACT_code","year" ))
  dt.foodnNuts <-  merge(dt.food,dt.nutrients, by = "IMPACT_code", all = TRUE)

  # create name lists for use in operations below
  # the product of daily consumption by nutrient content for each commodity
  nutList.Q <-   paste(nutList, "Q", sep = ".")

  # the total daily consumption of each nutrient
  nutList.sum.all <- paste(nutList, "sum.all", sep = ".")
  # the ratio of daily consumption of each nutrient to the total consumption
  nutList.ratio.all <- paste(nutList, "ratio.all", sep = ".")
  # the ratio of daily consumption of each nutrient by the nutrient requirement
  nutList.req.ratio.all <- paste(nutList, "req.ratio.all", sep = ".")

  # the total daily consumption of each staple
  nutList.sum.staple <- paste(nutList, "sum.staple", sep = ".")
  # the ratio daily consumption of each nutrient for each staple to the total consumption
  nutList.ratio.staple <- paste(nutList, "ratio.staple", sep = ".")
  # the ratio daily consumption of each nutrient for each staple by the nutrient requirement
  nutList.req.ratio.staple <- paste(nutList, "req.ratio.staple", sep = ".")

   # the total daily consumption of each food group
  nutList.sum.foodGroup <- paste(nutList, "sum.foodGroup", sep = ".")
  # the ratio daily consumption of each nutrient for each foodGroup to the total consumption
  nutList.req.ratio.foodGroup <- paste(nutList, "req.ratio.foodGroup", sep = ".")
  # the ratio daily consumption of each nutrient for each foodGroup by the nutrient requirement
  nutList.ratio.foodGroup <- paste(nutList, "ratio.foodGroup", sep = ".")

  # multiply the food item by the nutrients it contains and copy into a table called dt.food.prod
  dt.food.agg <- data.table::copy(dt.foodnNuts[, (nutList.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutList][,(nutList) := NULL])

 # calculate sums and ratios, for all food items, by staples, and by food groups -----
  # these keys are used to determine what is summed over or ratio made with
  allKey <-       c("scenario", region, "year")
  sumKey <-       c("scenario", region, "year","IMPACT_code")
  stapleKey <-    c("scenario", region, "year", "staple.code")
  foodGroupKey <- c("scenario", region, "year", "food.group.code")

  # first sum
    ## individual nutrients from all commodities
   data.table::setkeyv(dt.food.agg,allKey)
   dt.food.agg <- dt.food.agg[, (nutList.sum.all) := lapply(.SD, sum), .SDcols = nutList.Q,
                              by = eval(data.table::key(dt.food.agg))]

  ## individual nutrients by staples
  data.table::setkeyv(dt.food.agg,stapleKey)
  dt.food.agg <- dt.food.agg[, (nutList.sum.staple) := lapply(.SD, sum), .SDcols = nutList.Q,
                             by = eval(data.table::key(dt.food.agg))]

  ## individual nutrients by food group
  data.table::setkeyv(dt.food.agg,foodGroupKey)
  dt.food.agg <- dt.food.agg[, (nutList.sum.foodGroup) := lapply(.SD, sum), .SDcols = nutList.Q,
                             by = eval(data.table::key(dt.food.agg))]

  # now calculate ratios of nutrient by group to total consumption of the nutrient
  # nutrient from each food item to the total
  #dt.food.ratio <- data.table::copy(dt.all.sum[,(nutList.ratio) := dt.all.sum[[nutList.Q]] / dt.all.sum[[nutList.sum]]])

  #  ratio of nutrient from each food item to the total
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.ratio.all[k] := get(nutList.Q[k]) / get(nutList.sum.all[k])]
  }
  #  ratio of nutrient from each staple item to the total
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.ratio.staple[k] := get(nutList.sum.staple[k]) / get(nutList.sum.all[k])]
  }
  #  ratio of nutrient from each food group item to the total
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.ratio.foodGroup[k] := get(nutList.sum.foodGroup[k]) / get(nutList.sum.all[k])]
  }

  # now do ratios with nutrient requirements
  setkeyv(dt.food.agg,allKey)
  setkeyv(dt.nutsReqPerCap,allKey)
  dt.food.agg <- merge(dt.food.agg,dt.nutsReqPerCap, by = allKey, all = TRUE)
oldOrder <- names(dt.food.agg)
moveitems <- c(sumKey,"food.group.code","staple.code","foodAvailpDay",nutList)
remainder <- oldOrder[!oldOrder %in% moveitems]
  data.table::setcolorder(dt.food.agg,c(moveitems,remainder))
  #  ratio of nutrient from each food item to the requirement
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.req.ratio.all[k] := get(nutList.Q[k]) / get(nutList[k])]
  }
  #  ratio of nutrient from each staple item to the requirement
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.req.ratio.staple[k] := get(nutList.sum.staple[k]) / get(nutList[k])]
  }
  #  ratio of nutrient from each food group item to the requirement
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.req.ratio.foodGroup[k] := get(nutList.sum.foodGroup[k]) / get(nutList[k])]
  }

  inDT <- dt.food.agg

  reqShortName <- substring(req,5,7)
  outName <- paste("food.agg.",reqShortName,sep = "")
  cleanup(inDT, outName,fileloc("resData"))
}
# end of generateResults function

  # now prepare for creating excel files; one each for all, staples, and food groups ------
  sumKey <-       c("scenario", region, "year","IMPACT_code")
  stapleKey <-    c("scenario", region, "year", "staple.code")
  foodGroupKey <- c("scenario", region, "year", "food.group.code")

  keepListCol.all <- c(sumKey,nutList.ratio.all)
  keepListCol.staples <- c(stapleKey,nutList.ratio.staples)
  keepListCol.foodGroup <- c(foodGroupKey,nutList.ratio.foodGroup)

  dt.all.ratio <- unique(dt.food.agg[, keepListCol.all, with =  FALSE])
  dt.staples.ratio <- unique(dt.food.agg[, keepListCol.staples, with =  FALSE])
  dt.foodGroup.ratio <- unique(dt.food.agg[, keepListCol.foodGroup, with =  FALSE])

  # these functions return the maximum or minimum in every row
  colMax <- function(dataIn) {
    lapply(dataIn, max, na.rm = TRUE)
  }
  colMin <- function(dataIn) {
    lapply(dataIn, min, na.rm = TRUE)
  }
  cMax.all <- data.table::as.data.table(colMax(dt.food.agg))
  cMin.all <- data.table::as.data.table(colMin(dt.food.agg))

  #reshape the results to get years in columns
  setkeyv(dt.food.agg,sumKey)
  dt.food.agg.all.long <- data.table::melt(
    dt.food.agg,
    id.vars = sumKey,
    measure.vars = nutList.ratio.all,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.food.ratio.wide <- data.table::dcast.data.table(
    data = dt.food.ratio.long,
    formula = scenario + get(region) + nutrient ~ year,
    value.var = "nut_req")

  #set up initial worksheets for the ith set of requirements -----
  tmp <- f.createGeneralWorksheet()
  # this structure is needed to get two data frames out of the function
  #wbGeneral sets up a workbook with some basic information
  wbGeneral <- tmp[[1]]
  # df.wbInfoGeneral is a data frame
  df.wbInfoGeneral <- tmp[[2]]
  reqListShort <- gsub(".percap", "", req)

  #add a worksheet describing the nutrients in this requirement
  openxlsx::addWorksheet(wbGeneral, sheetName = reqListShort)
  openxlsx::writeData(
    wbGeneral,
    dt.nutsReqPerCap,
    sheet = reqListShort,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE)

  openxlsx::addStyle(
    wbGeneral,
    sheet = reqListShort,
    style = numStyle,
    rows = 1:nrow(dt.nutsReqPerCap) + 1,
    cols = 2:ncol(dt.nutsReqPerCap),
    gridExpand = TRUE)

  openxlsx::setColWidths(
    wbGeneral,
    sheet = reqListShort,
    cols = 1:ncol(dt.nutsReqPerCap),
    widths = "auto")

  df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
    c(reqList[i], paste("metadata on nutrients included in ", reqListShort))

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
    openxlsx::addStyle(
      wbGeneral,
      sheet = nutList[j],
      style = numStyle,
      rows = 1:nrow(temp),
      cols = 2:ncol(temp),
      gridExpand = TRUE
    )

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
      gg <- ggplot2::ggplot(data = dt.temp,
                            mapping = eval(ggplot2::aes(x = year, y = nut_req,  color = get(region)))) +
        eval(ggplot2::geom_line()) +
        eval(ggplot2::theme(
          axis.text.x = eval(ggplot2::element_text(
            color = "black",
            angle = 0,
            size = 9,
            vjust = 0.5
          )),
          axis.text.y = eval(ggplot2::element_text(
            color = "black",
            size = 9,
            vjust = 0.5
          )),
          axis.title.y = eval(ggplot2::element_text(
            color = "black",
            size = 9,
            vjust = 0.5
          )),
          legend.text = eval(ggplot2::element_text(
            color = "black",
            size = 7,
            vjust = 0.5
          )),
          plot.title = eval(ggplot2::element_text(
            color = "black",face = "bold",size = 11,hjust = 0.5,vjust = 1
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
      #    plotSheet <- paste("plot_", nutrient.name, sep = "")
      plotSheet <- paste(nutrient.name,scenario.name, sep = ".")
      print(plotSheet)
      openxlsx::addWorksheet(wbGeneral, sheetName = plotSheet)
      print(gg) #needs to be showing for the next step to work
      openxlsx::insertPlot(
        wbGeneral,
        sheet = plotSheet,
        width = 9,
        height = 5,
        fileType = "png",
        units = "in",
        startRow = 5
      )
      df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
        c(nutList[j], paste("Ratio results for ", nutrient.name))
      cMax.all <- data.table::as.data.table(colMax(dt.food.agg$folate_µg.ratio.all))
      cMin.all <- data.table::as.data.table(colMin(dt.food.reqs.ratio))

      #write the rows that have the max and min values for the current nutrient
      # eval parse needed here to get the which to run. uggh. This displays the row num of the row with the max/min value
      maxRowNum <-
        which(dt.food.ratio[, eval(parse(text = colnames(cMax)[j]))] == cMax.food[1, eval(parse(text = colnames(cMax.food)[j]))])
      minRowNum <-
        which(dt.food.ratio[, eval(parse(text = colnames(cMin)[j]))] == cMin.food[1, eval(parse(text = colnames(cMin.food)[j]))])
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
}

# for (i in 1:length(reqList)) {
#   generateResults(reqList[i],dt.food,IMPACTscenarioList, dt.nuts,region)
# }
generateResults(reqList[1],dt.food,IMPACTscenarioList, dt.nuts,region)
