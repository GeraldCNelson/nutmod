#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro -------------------------------------------------------------------

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

# choose a grouping of countries -----
region <- keyVariable("region")
# Read in IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
dt.IMPACTfood <- dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList"),] # this should not be necessary
dt.IMPACTfood <- dt.IMPACTfood[!get(region) == "GRL",] # neither should this

# calculate the share of per capita income spent on IMPACT commodities
budgetShare(dt.IMPACTfood,region)

# get the list of scenarios in the IMPACT data for use below
IMPACTscenarioList <- unique(dt.IMPACTfood$scenario)

keepListCol <- c("scenario", "IMPACT_code", region, "FoodAvailability","year")
dt.food <- dt.IMPACTfood[, keepListCol, with = FALSE]
# convert food availability from per year to per day
dt.food[, foodAvailpDay:= FoodAvailability / keyVariable("DinY")][,FoodAvailability:= NULL]

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

generateResults <- function(req,dt.food,IMPACTscenarioList) {
  # read in the requirements for the representative consumer in each country for this set
  # note that these change over time and SSP scenario with the change in age and gender composition
  dt.nutsReqPerCap <- getNewestVersion(req)
  #get list of nutrients from the requirement df
  nutNames <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]
  # remove the .fin part of the name so the nutrient names in the requirements
  # corresponds to the nutrient in the dt.nutrients looksup table
  nutList <- gsub(".fin","",nutNames)
  data.table::setnames(dt.nutsReqPerCap, old = names(dt.nutsReqPerCap), new = c("scenario",region,"year",nutList))

  # read in nutrients data and optionally apply cooking retention values -----
  dt.nutrients <- cookingRet("yes")
  # get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
  dt.nutrients <- dt.nutrients[!IMPACT_code %in% deleteListRow,]

  keepListCol <- c("IMPACT_code","food.group.code","staple.code",nutList)
  dt.nutrients <- dt.nutrients[,keepListCol, with = FALSE]
  # convert nutrients (in 100 grams of food) to nutrients per kg of food -----
  dt.nutrients[, (nutList) := lapply(.SD, function(x) (x * 10)), .SDcols = nutList]

  # convert dt.nutsReqPerCap scenario names. Leave just SSP1 to 5.
  dt.nutsReqPerCap[,scenario := substr(scenario,1,4)]
  # dt.nutsReqPerCap has values for the 5 SSP scenarios. To align with the IMPACT data we need to
  # add the climate model name to the SSP scenario name (SSP1 - 5).
  # add copies of dt.nutsReqPerCap for each of the climate models
  # this for loop adds copies of the nutrient requirements for each climate model used. May end up
  # with more than needed because it has all 5 SSP scenarios.
  dt.temp <- dt.nutsReqPerCap[FALSE,]
  for (i in unique(dt.food$scenario)) {
    climModel <- gsub(substr((i),1,5),"",i)
    SSPNum <- substr((i),1,4)
    print(i)
    temp.nuts <- data.table::copy(dt.nutsReqPerCap)
    temp.nuts[,scenario:= paste(scenario,climModel,sep="-")]
    dt.temp <- rbind(dt.temp, temp.nuts)
  }
  # keep just the nutrient requirements scenarios that are in the IMPACT data
  dt.nutsReqPerCap <- dt.temp[scenario %in% IMPACTscenarioList,]

  #combine the food availability info with the nutrients for each of the IMPACT commodities
  data.table::setkey(dt.nutrients, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario",region,"IMPACT_code","year" ))
  dt.foodnNuts <-  merge(dt.food,dt.nutrients, by = "IMPACT_code", all = TRUE)

  #temp <- merge(dt.food,dt.nutsReqPerCap, by = c("scenario",region,"year"), all = TRUE)
  nutList.Q <-   paste(nutList, "Q", sep = ".")
  nutList.sum <- paste(nutList, "sum", sep = ".")

  # multiply the food item by the nutrients it contains and copy into a table called dt.food.sum
  dt.food.sum <- data.table::copy(dt.foodnNuts[, (nutList.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutList][,(nutList) := NULL])

  #sum individual nutrients across all commodities
  allKey <-    c("scenario", region, "year")
  data.table::setkeyv(dt.food.sum,allKey)
  dt.food.sum <- dt.food.sum[, (nutList.sum) := lapply(.SD, sum), .SDcols = nutList.Q,
                             by = eval(data.table::key(dt.food.sum))][,(nutList.Q) := NULL]
  dt.food.sum <-
    unique(dt.food.sum[, c("scenario", region, "year", nutList.sum), with =  FALSE])

  # do staples
  stapleKey <-    c("scenario", region, "staple.code", "year")
  dt.staples.sum <- data.table::copy(dt.foodnNuts)
  data.table::setkeyv(dt.staples.sum,stapleKey)
  dt.staples.sum <- dt.staples.sum[, (nutList.sum) := lapply(.SD, sum), .SDcols = nutList.Q,
                                   by = eval(data.table::key(dt.staples.sum))][,(nutList.Q) := NULL]
  dt.staples.sum <-
    unique(dt.staples.sum[, c(stapleKey,nutList.sum), with = FALSE])

  # do food groups
  foodGroupkey <- c("scenario", region, "food.group.code", "year")
  dt.foodGroup.sum <- data.table::copy(dt.foodnNuts)
  data.table::setkeyv(dt.foodGroup.sum,foodGroupkey)
  dt.foodGroup.sum <- dt.foodGroup.sum[, (nutList.sum) := lapply(.SD, sum), .SDcols = nutList.Q,
                                       by = eval(data.table::key(dt.foodGroup.sum))][,(nutList.Q) := NULL]
  dt.foodGroup.sum <-
    unique(dt.foodGroup.sum[, c(foodGroupkey,nutList.sum), with = FALSE])

  # genFoodGroupResults(dt.foodnNuts,region,nutList.sum)
  # genStapleResults(dt.foodnNuts,region,nutList.sum)

  #dt.reqs.long <- getNewestVersion(req)

  data.table::setkeyv(dt.nutsReqPerCap, allKey)
  data.table::setkeyv(dt.food.sum, allKey)
  dt.food.tot <- dt.nutsReqPerCap[dt.food.sum]
  nutList.ratio <- paste(nutList, "ratio", sep = ".")
  #nutList.fin <- paste(nutList,".fin",sep="")
  #loop through all the nutrients in this requirement set and calculate the ratio of consumption to requirement

  # for (j in 1:length(nutList)) {
  #   #calculate the share of the requirement from IMPACT commodities
  #   numerator <- nutList[j]
  #   dt.food.tot[, (nutList.ratio[j]) := eval(parse(text = nutList[j])) /
  #                 eval(parse(text = nutList.sum[j]))]
  # }
  #temp <- dt.food.tot[,(nutList.ratio) := .(nutList)/.(nutList.sum)]
  for(j in nutList){
    nutListSum <- paste(j,".sum",sep="")
    data.table::set(dt.food.tot, i=NULL, j=j, value= dt.food.tot[[j]]/dt.food.tot[[nutListSum]])
  }
  keepListCol <- c("scenario",region,"year",nutList)
  dt.food.ratio <- dt.food.tot[, keepListCol, with = FALSE]
  colMax <- function(dataIn)
    lapply(dataIn, max, na.rm = TRUE)
  colMin <- function(dataIn)
    lapply(dataIn, min, na.rm = TRUE)
  cMax <- data.table::as.data.table(colMax(dt.food.ratio))
  cMin <- data.table::as.data.table(colMin(dt.food.ratio))

  # dt.maxRows = dt.food.tot[0]
  # rbind[dt.maxRows, dt.food.tot[maxRowNum,]]

  inDT <- dt.food.ratio
  outName <- paste(req, ".ratio",sep = "")
  cleanup(inDT, outName,fileloc("resData"))

  #reshape the results to get years in columns
  dt.food.ratio.long <- data.table::melt(
    dt.food.ratio,
    id.vars = c("scenario", region, "year"),
    measure.vars = c(nutList),
    variable.name = "nutrient",
    value.name = "nut_req",
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
  reqListShort <-gsub(".percap","",req)

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
    temp <- dt.food.ratio.wide[nutrient %in% nutList.ratio[j]]
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
    scenario.name <- unique(dt.IMPACTfood$scenario)
    nutrient.name <- sub("^(.*)_.*$", "\\1", nutList[j])
    temp <-
      dt.food.ratio.long[eval(data.table::like(nutrient, "ratio")) & eval(data.table::like(nutrient, nutrient.name)) &
                           scenario %in% scenario.name, ]
    #ggplot wants the x axis to be numeric
    temp[,year:= as.numeric(gsub("X", "", year))]
    gg <- ggplot2::ggplot(data = temp,
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
      eval(ggplot2::ggtitle(paste(
        "Cons. share of require., ", nutrient.name, ", ", unique(temp$scenario),sep = ""))) +
      eval(ggplot2::guides(color = eval(ggplot2::guide_legend(nrow = 5, byrow = TRUE))))
    # add plot to the spreadsheet
    plotSheet <- paste("plot_", nutrient.name, sep = "")
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

    #write the rows that have the max and min values for the current nutrient
    # eval parse needed here to get the which to run. uggh. This displays the row num of the row with the max/min value
    maxRowNum <-
      which(dt.food.ratio[, eval(parse(text = colnames(cMax)[j]))] == cMax[1, eval(parse(text = colnames(cMax)[j]))])
    minRowNum <-
      which(dt.food.ratio[, eval(parse(text = colnames(cMin)[j]))] == cMin[1, eval(parse(text = colnames(cMin)[j]))])
    openxlsx::writeData(
      wbGeneral,
      dt.food.ratio[maxRowNum, ],
      sheet = plotSheet,
      startRow = 1,
      startCol = 2,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::writeData(wbGeneral, paste("Country and year with max value for the ",
                                         nutrient.name, " ratio", sep = ""), sheet = plotSheet, startRow = 1,
                        startCol = 1, rowNames = FALSE, withFilter = FALSE)
    openxlsx::writeData(wbGeneral, dt.food.ratio[minRowNum, ], sheet = plotSheet,
                        startRow = 3, startCol = 2, rowNames = FALSE, withFilter = FALSE)
    openxlsx::writeData(wbGeneral, paste("row with min value for the ", nutrient.name,
                                         " ratio", sep = ""), sheet = plotSheet, startRow = 3, startCol = 1,
                        rowNames = FALSE, withFilter = FALSE)
    openxlsx::addStyle(wbGeneral, sheet = plotSheet, style = numStyle, rows = 1:4,
                       cols = 2:ncol(dt.food.tot), gridExpand = TRUE)
    openxlsx::setColWidths(wbGeneral, sheet = plotSheet, cols = 1:ncol(dt.food.tot[minRowNum,
                                                                                   ]), widths = "auto", ignoreMergedCells = FALSE)
    df.wbInfoGeneral[(nrow(df.wbInfoGeneral) + 1), ] <-
      c(plotSheet,
        paste("Max/min rows and graph of results for ", nutrient.name))
  }

  f.finalizeWB(wbGeneral, df.wbInfoGeneral, reqList[i])
}

for (i in 1:length(reqList)) {
  generateResults(reqList(i))
}
