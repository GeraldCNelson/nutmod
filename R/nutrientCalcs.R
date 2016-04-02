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

#' @description This script reads in IMPACT nutrient lookup table, does some manipulations of the data,
#' and writes out results to an excel spreadsheet

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
if (!exists("getNewestVersion", mode = "function")) {
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  }

# Read in data ----------
IMPACTfood <- fileNameList("IMPACTfood")
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")

# read in nutrients data ----

df.nutrients <- getNewestVersion("df.nutrients")
dt.nutrients <- data.table::as.data.table(df.nutrients)

#get cooking retention values data frame
df.cookretn <- getNewestVersion("cookingRet")

# a switch to turn on or off the use of the cooking retention values
#' @param applyCookingRetention
applyCookingRetention <- keyVariable("useCookingRetnValues")
if (applyCookingRetention == "yes") {
  for (i in 1:length(cookretn.cols)) {
    nutrientName <-
      substr(x = cookretn.cols[i], 1, nchar(cookretn.cols[i]) - 3)
    nutColName <- paste ("nutrients$", nutrientName, sep = "")
    #  print(nutColName)
    nutRetentColName <-
      paste ("nutrients$", cookretn.cols[i], sep = "")
    #  print(nutRetentColName)
    temp <-
      as.data.frame(eval(parse(text = nutRetentColName)) * eval(parse(text = nutColName)) /
                      100)
    colnames(temp) <- nutrientName
    dt.nutrients[, nutrientName] <- temp
  }
}

#generated in SSPPopExtract.R because it is adjusted to the SSP age and gender groups
req.EAR <- getNewestVersion("req.EAR")
req.RDA.vits <- getNewestVersion("req.RDA.vits")
req.RDA.minrls <- getNewestVersion("req.RDA.minrls")
req.RDA.macro <- getNewestVersion("req.RDA.macro")
req.UL.vitsFile <- getNewestVersion("req.UL.vitsFile")
req.UL.minrls <- getNewestVersion("req.UL.minrls")

# calculate the share of per capita income spent on IMPACT commodities
data.table::setkey(dt.IMPACTfood, "scenario", "region", "year")
dt.IMPACTfood[, budget.Pw := sum(food * Pw / 365 / 1000), by = key(dt.IMPACTfood)]
dt.IMPACTfood[, budget.Pcon := sum(food * Pc * (1 - CSE) / 365 / 1000), by = key(dt.IMPACTfood)]
dt.IMPACTfood[, budget.Pc := sum(food * Pc / 365 / 1000), by = key(dt.IMPACTfood)]
dt.budget <-
  unique(dt.IMPACTfood[, c(key(dt.IMPACTfood), "budget.Pw", "budget.Pc", "budget.Pcon", "pcGDP"), with =
                  F])

#incomeShare <-join(t1.pcGDP,budget)
dt.budget[, incSharePw := budget.Pw / ((pcGDP * 1000) / 365)]
dt.budget[, incSharePc := budget.Pc / ((pcGDP * 1000) / 365)]
dt.budget[, incSharePcon := budget.Pcon / ((pcGDP * 1000) / 365)]

#nutrient stuff
#include only columns that are needed; IMPACT code and nutrient names
#choices are
# - all - all relevant nutrients, 23
# - macro - "energy", "protein", "fat", "carbohydrate", "fiber", "sugar"
# - minerals - "calcium", "iron", "potassium", "sodium", "zinc"
# - vitamins - "vitamin_c", "thiamin",	"riboflavin",	"niacin", "vitamin_b6",	"folate", "vitamin_b12",
#      "vitamin_a_RAE", 	"vitamin_e", "vitamin_d2_3"
# - fattyAcids - "ft_acds_tot_sat", "ft_acds_plyunst"
# next line is where the choice is made
# short.name <- c("vitamins")
# nut.list <- eval(parse(text = short.name))
# includes <- c("IMPACT_code", nut.list)
# nuts.reduced <- nutrients[, (names(nutrients) %in% includes)]

# data.table::setkey(dt.IMPACTfood,"IMPACT_code")
# temp <- as.data.table(foodGroupsInfo)
# data.table::setkey(temp,"IMPACT_code")
# dt.d2 <- dt.IMPACTfood[temp]

keepList <- c("scenario", "IMPACT_code", "region", "year", "food")
dt.food <- dt.IMPACTfood[, names(dt.IMPACTfood) %in% keepList, with = FALSE]
#convert from annual availability to daily availability. 'food' variable is in kgs/person/year
dt.food[, food := food / 365]
data.table::setkey(dt.food, "IMPACT_code")
#a list of the requirements types. Each has a different set of nutrients. These are a subset
# of what are in the nutrients requirements tables from IOM. They are the nutrients common to
# both the IOM and nutrient content lookup spreadsheet

reqList <-
  c(
    "req.EAR",
    "req.RDA.vits" ,
    "req.RDA.minrls",
    "req.RDA.macro",
    "req.UL.vits",
    "req.UL.minrls"
  )

for (i in 1:length(reqList)) {
  #get list of nutrients from the requirement df
  nut.list <-
    unique(eval(parse(text = paste(
      reqList[i], "$nutrient", sep = ""
    ))))
  #nuts.reduced <- nutrients[, (names(nutrients) %in% nut.list)]
  #keep only nutrients in the requirements being worked on
  dt.nuts.reduced <-
    dt.nutrients[, c("IMPACT_code", nut.list, "staple.code", "food.group.code"), with = FALSE]
  data.table::setkey(dt.nuts.reduced, "IMPACT_code")

  dt.d3 <-  dt.food[dt.nuts.reduced]
  # get sum of nutrient intake by food group
  data.table::setkey(dt.d3, "scenario", "region", "food.group.code", "year")
  nut.list.Q <- paste(nut.list, "Q", sep = ".")
  nut.list.Q.sum <- paste(nut.list, "Q.sum", sep = ".")
  #splitting up these operations seems to make them faster than doing it in one go
  #note - * 10 is need because nutrients are per 100 g and food is in kgs
  temp2 <-
    dt.d3[, (nut.list.Q) := lapply(.SD, function(x)
      (x * dt.d3[['food']] * 10)), .SDcols = nut.list]
  temp3 <-
    temp2[, (nut.list.Q.sum) := lapply(.SD, sum), .SDcols = nut.list.Q, by = key(dt.d3)]
  dt.food.group.sum <-
    unique(temp3[, c("scenario",
                     "region",
                     "food.group.code",
                     "year",
                     nut.list.Q.sum), with = F])

  # get sum of nutrient intake by staple
  data.table::setkey(dt.d3, "scenario", "region", "staple.code", "year")
  temp3 <-
    temp2[, (nut.list.Q.sum) := lapply(.SD, sum), .SDcols = nut.list.Q, by = key(dt.d3)]
  dt.staples.sum <-
    unique(temp3[, c("scenario", "region", "staple.code", "year", nut.list.Q.sum), with =
                   F])

  # get sum of nutrient intake by country
  data.table::setkey(dt.d3, "scenario", "region", "year")
  temp3 <-
    temp2[, (nut.list.Q.sum) := lapply(.SD, sum), .SDcols = nut.list.Q, by = key(dt.d3)]
  dt.food.sum <-
    unique(temp3[, c("scenario", "region", "year", nut.list.Q.sum), with =
                   F])

  # convert the requirements dt from wide (years as columns) to long (add year and value columns)
  dt.reqs.long <- melt(
    eval(parse(text = reqList[i])),
    id.vars = c("region", "nutrient"),
    measure.vars = keepYearList,
    variable.name = "year",
    value.name = "nut_req"
  )
  dt.reqs.long[, year := as.character(year)]
  # .. and then back to wide with nutrients as columns, to match the structure of the sum DTs.
  dt.reqs <-
    dcast.data.table(dt.reqs.long, region + year ~ nutrient, value.var = "nut_req")

  data.table::setkey(dt.reqs, "region", "year")
  data.table::setkey(dt.food.sum, "region", "year", "scenario")
  dt.food.tot <- dt.reqs[dt.food.sum]
  nut.list.ratio <- paste(nut.list, "ratio", sep = ".")

  #loop through all the nutrients in this requirement set and calculate the ratio of consumption to requirement
  for (j in 1:length(nut.list)) {
    #calculate the share of the requirement from IMPACT commodities
    dt.food.tot[, (nut.list.ratio[j]) := eval(parse(text = nut.list[j])) /
                  eval(parse(text = nut.list.Q.sum[j]))]
  }

  dt.food.ratio <- dt.food.tot[, nut.list.ratio, with = FALSE]
  colMax <- function(data)
    lapply(data, max, na.rm = TRUE)
  colMin <- function(data)
    lapply(data, min, na.rm = TRUE)
  cMax <- as.data.table(colMax(dt.food.ratio))
  cMin <- as.data.table(colMin(dt.food.ratio))

  # dt.maxRows = dt.food.tot[0]
  # rbind[dt.maxRows, dt.food.tot[maxRowNum,]]
  #
  saveRDS(dt.food.tot,
          file = paste("results/", reqList[i], ".results.", Sys.Date(), ".rds", sep =
                         ""))

  #reshape the results to get years in columns
  dt.food.tot.long <- melt(
    dt.food.tot,
    id.vars = c("scenario", "region", "year"),
    measure.vars = c(nut.list, nut.list.Q.sum, nut.list.ratio),
    variable.name = "nutrient",
    value.name = "nut_req"
  )
  # dt.food.tot.long[,nutrient:= as.character(nutrient)]

  dt.food.tot.wide <-
    dcast.data.table(dt.food.tot.long,
                     scenario + region + nutrient ~ year,
                     value.var = "nut_req")
  dt.food.tot.wide[, nutrient := as.character(nutrient)]

  #set up initial worksheets in the spreadsheet for the ith set of requirements
  tmp <- f.createGeneralWorksheet()
  # this structure is needed to get two data frames out of the function
  wbGeneral <- tmp[[1]]
  wbInfoGeneral <- tmp[[2]]

  #add a worksheet describing the nutrients in this requirement
  openxlsx::addWorksheet(wbGeneral, sheetName = reqList[i])
  openxlsx::writeData(
    wbGeneral,
    eval(parse(text = reqList[i])),
    sheet = reqList[i],
    startRow = 1,
    startCol = 1,
    rowNames = FALSE
  )
  openxlsx::addStyle(
    wbGeneral,
    sheet = reqList[i],
    style = numStyle,
    rows = 1:nrow(eval(parse(text = reqList[i]))) + 1,
    cols = 2:ncol(eval(parse(text = reqList[i]))),
    gridExpand = TRUE
  )
  openxlsx::setColWidths(
    wbGeneral,
    sheet = reqList[i],
    cols = 1:ncol(eval(parse(text = reqList[i]))),
    widths = "auto"
  )
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
    c(reqList[i], paste("metadata on nutrients included in ", reqList[i]))

  # add data to the spreadsheet -----
  for (j in 1:length(nut.list)) {
    # add an info sheet for the jth nutrient in the ith set of requirements
    temp <- dt.food.tot.wide[nutrient %in% nut.list.ratio[j]]
    # add the results to the spreadsheet
    openxlsx::addWorksheet(wbGeneral, sheetName = nut.list[j])
    openxlsx::writeData(
      wbGeneral,
      temp,
      sheet = nut.list[j],
      startRow = 1,
      startCol = 1,
      rowNames = FALSE,
      withFilter = TRUE
    )
    openxlsx::setColWidths(
      wbGeneral,
      sheet = nut.list[j],
      cols = 1,
      widths = "auto",
      ignoreMergedCells = FALSE
    )
    openxlsx::addStyle(
      wbGeneral,
      sheet = nut.list[j],
      style = numStyle,
      rows = 1:nrow(temp),
      cols = 2:ncol(temp),
      gridExpand = TRUE
    )

    # code to create a graph of the nutrient ratios for each of the nutrients in the current requirements list for a single scenario
    scenario.name <- "SSP2-GFDL"
    nutrient.name <- sub("^(.*)_.*$", "\\1", nut.list[j])
    temp <-
      dt.food.tot.long[nutrient %like% "ratio" &
                         nutrient %like% nutrient.name & scenario %in% scenario.name, ]
    temp$year <- gsub("X", "", temp$year)
    temp$year <- as.numeric(temp$year)
    gg <- ggplot2::ggplot(data = temp,
                 aes(x = year, y = nut_req,  color = region)) + geom_line() +
      theme(
        axis.text.x = element_text(
          color = "black",
          angle = 0,
          size = 9,
          vjust = 0.5
        ),
        axis.text.y = element_text(
          color = "black",
          size = 9,
          vjust = 0.5
        ),
        axis.title.y = element_text(
          color = "black",
          size = 9,
          vjust = 0.5
        ),
        legend.text = element_text(
          color = "black",
          size = 7,
          vjust = 0.5
        ),
        plot.title = element_text(
          color = "black",
          face = "bold",
          size = 11,
          hjust = 0.5,
          vjust = 1
        ),
        panel.background = element_blank(),
        panel.border = element_rect(
          linetype = "solid",
          colour = "black",
          fill = NA
        ),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = NA)
      ) +
      xlab("year") + ylab(nutrient.name) +
      ggtitle(paste(
        "Cons. share of require., ",
        nutrient.name,
        ", ",
        unique(temp$scenario),
        sep = ""
      )) +
      guides(color = guide_legend(nrow = 5, byrow = TRUE))
    # add plot to the spreadsheet
    plotSheet <- paste("plot_", nutrient.name, sep = "")
    print(plotSheet)
    openxlsx::addWorksheet(wbGeneral, sheetName = plotSheet)
    print(gg) #needs to be showing for the next step to work
    insertPlot(
      wbGeneral,
      sheet = plotSheet,
      width = 9,
      height = 5,
      fileType = "png",
      units = "in",
      startRow = 5
    )
    wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
      c(nut.list[j], paste("Ratio results for ", nutrient.name))

    #write the rows that have the max and min values for the current nutrient
    # eval parse needed here to get the which to run. uggh. This displays the row num of the row with the max/min value
    maxRowNum <-
      which(dt.food.ratio[, eval(parse(text = colnames(cMax)[j]))] == cMax[1, eval(parse(text = colnames(cMax)[j]))])
    minRowNum <-
      which(dt.food.ratio[, eval(parse(text = colnames(cMin)[j]))] == cMin[1, eval(parse(text = colnames(cMin)[j]))])
    openxlsx::writeData(
      wbGeneral,
      dt.food.tot[maxRowNum, ],
      sheet = plotSheet,
      startRow = 1,
      startCol = 2,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::writeData(
      wbGeneral,
      paste(
        "Country and year with max value for the ",
        nutrient.name,
        " ratio",
        sep = ""
      ),
      sheet = plotSheet,
      startRow = 1,
      startCol = 1,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::writeData(
      wbGeneral,
      dt.food.tot[minRowNum, ],
      sheet = plotSheet,
      startRow = 3,
      startCol = 2,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::writeData(
      wbGeneral,
      paste("row with min value for the ", nutrient.name, " ratio", sep = ""),
      sheet = plotSheet,
      startRow = 3,
      startCol = 1,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::addStyle(
      wbGeneral,
      sheet = plotSheet,
      style = numStyle,
      rows = 1:4,
      cols = 2:ncol(dt.food.tot),
      gridExpand = TRUE
    )
    openxlsx::setColWidths(
      wbGeneral,
      sheet = plotSheet,
      cols = 1:ncol(dt.food.tot[minRowNum, ]),
      widths = "auto",
      ignoreMergedCells = FALSE
    )
    wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
      c(plotSheet,
        paste("Max/min rows and graph of results for ", nutrient.name))

  }
  #----------------------------

  short.name <- reqList[i]
  f.finalizeWB(wbGeneral, wbInfoGeneral, short.name)

}
