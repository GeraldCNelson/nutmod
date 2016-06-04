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
#' @include workbookFunctions.R
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
# dt.IMPACTfood <- dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList"),]
# get the list of scenarios in the IMPACT data for use below
IMPACTscenarioList <- keyVariable("scenarioListIMPACT")
#IMPACTscenarioList <- IMPACTscenarioList[1] # just for testing. !!!XXX

# read in nutrients data and optionally apply cooking retention values -----
dt.nuts <- cookingRet("yes")
# # get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
if (keyVariable("fixFish") == TRUE) {
deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
dt.nuts <- dt.nuts[!IMPACT_code %in% deleteListRow,]
}

# calculate the share of per capita income spent on IMPACT commodities
# do only if the data are mostly by country; ie the IMPACT159 regions
if (region == "region_code.IMPACT159") {budgetShare(dt.IMPACTfood,region)}

keepListCol <- c("scenario", "IMPACT_code", region, "FoodAvailability", "year")
dt.IMPACTfood <- dt.IMPACTfood[, keepListCol, with = FALSE]
# get rid of duplicate rows, caused by getting rid of GDP column
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood <- unique(dt.IMPACTfood)
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
#reqList <- reqList[4] # just for testing!!! XXX
#IMPACTscenarioList <- "SSP2-MIROC" # just for testing!!! XXX
#req <- "req.EAR.percap" # just for testing!!! XXX

generateResults <- function(req,dt.IMPACTfood,IMPACTscenarioList,dt.nuts,region) {
  # use dt.food only in the function
  dt.food <- data.table::copy(dt.IMPACTfood)
  print(paste("loading dt.IMPACT.food for ", req, sep = ""))
  print(proc.time())
 # str(dt.food)
  dt.food <- dt.food[scenario %in% IMPACTscenarioList,]
  # read in the nutrient requirements data  for a representative consumer -----
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
  print(paste("multiplying dt.nutrients by 10 for ", req, sep = ""))
  print(proc.time())

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
  nutList.sum.staples <- paste(nutList, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutList.ratio.staples <- paste(nutList, "ratio.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple by the nutrient requirement
  nutList.req.ratio.staples <- paste(nutList, "req.ratio.staple", sep = ".")

  # the total daily consumption of each food group
  nutList.sum.foodGroup <- paste(nutList, "sum.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup to the total consumption
  nutList.req.ratio.foodGroup <- paste(nutList, "req.ratio.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup by the nutrient requirement
  nutList.ratio.foodGroup <- paste(nutList, "ratio.foodGroup", sep = ".")

  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
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
  print(paste("summing nutrient for all commodities for ", req, sep = ""))
  print(proc.time())

  ## individual nutrients by staples
  data.table::setkeyv(dt.food.agg,stapleKey)
  dt.food.agg <- dt.food.agg[, (nutList.sum.staples) := lapply(.SD, sum), .SDcols = nutList.Q,
                             by = eval(data.table::key(dt.food.agg))]

  print(paste("summing by food group ", req, sep = ""))
  print(proc.time())

  ## individual nutrients by food group
  data.table::setkeyv(dt.food.agg,foodGroupKey)
  dt.food.agg <- dt.food.agg[, (nutList.sum.foodGroup) := lapply(.SD, sum), .SDcols = nutList.Q,
                             by = eval(data.table::key(dt.food.agg))]

  # now calculate ratios of nutrient by group to total consumption of the nutrient
  # nutrient from each food item to the total
  #dt.food.ratio <- data.table::copy(dt.all.sum[,(nutList.ratio) := dt.all.sum[[nutList.Q]] / dt.all.sum[[nutList.sum]]])

  print(paste("calculating nutrient share ratios ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each food item to the total
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.ratio.all[k] := get(nutList.Q[k]) / get(nutList.sum.all[k])]
  }
  #  ratio of nutrient from each staple item to the total
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.ratio.staples[k] := get(nutList.sum.staples[k]) / get(nutList.sum.all[k])]
  }
  #  ratio of nutrient from each food group item to the total
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.ratio.foodGroup[k] := get(nutList.sum.foodGroup[k]) / get(nutList.sum.all[k])]
  }

  # now do ratios with nutrient requirements
  print(paste("calculating nutrient requirement ratios for ", req, sep = ""))
  print(proc.time())

  data.table::setkeyv(dt.food.agg,allKey)
  data.table::setkeyv(dt.nutsReqPerCap,allKey)
  dt.food.agg <- merge(dt.food.agg,dt.nutsReqPerCap, by = allKey, all = TRUE)
  oldOrder <- names(dt.food.agg)
  moveitems <- c(sumKey,"food.group.code","staple.code","foodAvailpDay",nutList)
  remainder <- oldOrder[!oldOrder %in% moveitems]
  data.table::setcolorder(dt.food.agg,c(moveitems,remainder))
  #  ratio of nutrient from each food item to the requirement
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.req.ratio.all[k] := get(nutList.Q[k]) / get(nutList[k])]
  }

  print(paste("finished with ratio for each food item ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each staple item to the requirement
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.req.ratio.staples[k] := get(nutList.sum.staples[k]) / get(nutList[k])]
  }
  print(paste("finished with ratio for the staple/non staple categories ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each food group item to the requirement
  for (k in 1:length(nutList)) {
    dt.food.agg[,nutList.req.ratio.foodGroup[k] := get(nutList.sum.foodGroup[k]) / get(nutList[k])]
  }
  print(paste("finished with ratio for the food group categories ", req, sep = ""))
  print(proc.time())
  flush.console()
  inDT <- dt.food.agg

  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)
  outName <- paste("food.agg.",reqShortName,sep = "")
  cleanup(inDT, outName,fileloc("resData"), "csv")
}
# end of generateResults function

for (i in 1:length(reqList)) {
  generateResults(reqList[i],dt.IMPACTfood,IMPACTscenarioList, dt.nuts,region)
  print(paste("Done with ", reqList[i], ". ", length(reqList) - i," sets of requirements to go.", sep = ""))
}
