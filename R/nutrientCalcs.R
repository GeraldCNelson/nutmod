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
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
if (!exists("getNewestVersion", mode = "function")) {
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}

# Read in all data first and standardize variable names -----
# Read in IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
#scenarioListIMPACT <- unique(dt.IMPACTfood$scenario)
# dt.IMPACTfood <- dt.IMPACTfood[!region_code.IMPACT159 %in% c("GRL","SDN")]
# this should not be necessary
# dt.IMPACTfood <- dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTfoodCommodList"),]
# get the list of scenarios in the IMPACT data for use below
scenarioListIMPACT <- keyVariable("scenarioListIMPACT")

#scenarioListIMPACT <- scenarioListIMPACT[1] # just for testing. !!!XXX

# read in nutrients data and optionally apply cooking retention values -----
switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

# calculate the share of per capita income spent on IMPACT commodities
budgetShare(dt.IMPACTfood)

keepListCol <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "FoodAvailability", "year")
dt.IMPACTfood <- dt.IMPACTfood[, keepListCol, with = FALSE]
# get rid of duplicate rows, caused by getting rid of GDP column
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood <- unique(dt.IMPACTfood)
# convert food availability from per year to per day
dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")][,FoodAvailability := NULL]

# reqsListPercap is a list of the requirements types. Each has a different set of nutrients. These are a subset
# of what are in the nutrients requirements tables from IOM. They are the nutrients common to
# both the IOM and nutrient content lookup spreadsheet

# the .percap data are for a representative consumer. They are generated in dataManagement.SSP

reqsListPercap <- keyVariable("reqsListPercap")
#reqPercap <- reqsListPercap[4] # just for testing!!! XXX
#scenarioListIMPACT <- "SSP2-MIROC" # just for testing!!! XXX
req <- "req.EAR.percap" # just for testing!!! XXX

 generateResults <- function(req,dt.IMPACTfood,scenarioListIMPACT,dt.nutrients) {
  # use dt.food only in the function
  dt.food <- data.table::copy(dt.IMPACTfood)
  print(paste("loading dt.IMPACT.food for ", req, sep = ""))
  print(proc.time())
  dt.food <- dt.food[scenario %in% scenarioListIMPACT,]
  # read in the nutrient requirements data  for a representative consumer -----
  # Note that these are for SSP categories and thus vary by SSP category and year for each region
  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutListReq <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]
  #nutListReq <- nutListReq[3:4] # Here just for testing. !!! be sure to comment out!!!XXX

  # dt.nutsReqPerCap has values for the 5 SSP scenarios (but no climate change or experiment effects.
  # To align with the IMPACT data we need to
  # add the climate model and experiment names to the SSP scenario name (SSP1 - 5).
  # add copies of dt.nutsReqPerCap for each of the climate models
  # This for loop adds copies of the nutrient requirements for each climate model used.
  dt.temp <- data.table::copy(dt.nutsReqPerCap[FALSE,])
  scenarioListIMPACT <- as.character(scenarioListIMPACT)
  for (i in scenarioListIMPACT) {
    SSPName <- unlist(strsplit(i, "-"))[1] # get SSP abbrev
    climModel <- unlist(strsplit(i, "-"))[2] # get climate model abbrev
    experiment <- unlist(strsplit(i, "-"))[3] # get experiment abbrev
    # if (is.na(experiment)) {experiment <- "REF"}
    # print(experiment)
    # may need to add the RCP column later. Currently it's not included in the scenario name.
    temp.nuts <- data.table::copy(dt.nutsReqPerCap)
    temp.nuts <- temp.nuts[scenario == SSPName, ]
    # if (is.na(experiment)) {
    #   temp.nuts[,scenario := paste(scenario,climModel, sep = "-")]
    # } else {
    #   temp.nuts[,scenario := paste(scenario,climModel, experiment, sep = "-")]
    # }
    temp.nuts[,scenario := paste(SSPName, climModel, experiment, sep = "-")]
    dt.temp <- rbind(dt.temp, temp.nuts)
  }
  # keep just the nutrient requirements scenarios that are in the IMPACT data
  #  and the nutrients in nutListReq. And reduce rows to just IMPACT scenarios
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", nutListReq)
  dt.nutsReqPerCap <- dt.temp[,keepListCol, with = FALSE]
  dt.nutsReqPerCap <- dt.nutsReqPerCap[scenario %in% scenarioListIMPACT,]

  # reduce calculations to just the nutrients in nutListReq
  keepListCol <- c("IMPACT_code","food_group_code","staple_code",nutListReq)
  # use the data table dt.nutrients only in the function
  dt.nutrients <- data.table::copy(dt.nutrients[,keepListCol, with = FALSE])
  # convert nutrients (in 100 grams of food) to nutrients per kg of food -----
  dt.nutrients[, (nutListReq) := lapply(.SD, function(x) (x * 10)), .SDcols = nutListReq]
  print(proc.time())

  #combine the food availability info with the nutrients for each of the IMPACT commodities
  data.table::setkey(dt.nutrients, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  dt.foodnNuts <-  merge(dt.food,dt.nutrients, by = "IMPACT_code", all = TRUE)

  # create name lists for use in operations below
  # the product of daily consumption by nutrient content for each commodity
  nutListReq.Q <-   paste(nutListReq, "Q", sep = ".")

  # the total daily consumption of each nutrient
  nutListReq.sum.all <- paste(nutListReq, "sum.all", sep = ".")
  # the ratio of daily consumption of each nutrient to the total consumption
  nutListReq.ratio.all <- paste(nutListReq, "ratio.all", sep = ".")
  # the ratio of daily consumption of each nutrient by the nutrient requirement
  nutListReq.req.ratio.all <- paste(nutListReq, "req.ratio.all", sep = ".")

  # the total daily consumption of each staple
  nutListReq.sum.staples <- paste(nutListReq, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutListReq.ratio.staples <- paste(nutListReq, "ratio.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple by the nutrient requirement
  nutListReq.req.ratio.staples <- paste(nutListReq, "req.ratio.staple", sep = ".")

  # the total daily consumption of each food group
  nutListReq.sum.foodGroup <- paste(nutListReq, "sum.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup to the total consumption
  nutListReq.req.ratio.foodGroup <- paste(nutListReq, "req.ratio.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup by the nutrient requirement
  nutListReq.ratio.foodGroup <- paste(nutListReq, "ratio.foodGroup", sep = ".")

  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  dt.food.agg <- data.table::copy(dt.foodnNuts[, (nutListReq.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq])
  #[,(nutListReq) := NULL])
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  # calculate sums and ratios, for all food items, by staples, and by food groups -----
  # these keys are used to determine what is summed over or ratio made with
  allKey <-       c("scenario", "region_code.IMPACT159", "year")
  sumKey <-       c("scenario", "region_code.IMPACT159", "year","IMPACT_code")
  stapleKey <-    c("scenario", "region_code.IMPACT159", "year", "staple_code")
  foodGroupKey <- c("scenario", "region_code.IMPACT159", "year", "food_group_code")

  # first sum
  ## individual nutrients from all commodities
  data.table::setkeyv(dt.food.agg,allKey)
  dt.food.agg <- dt.food.agg[, (nutListReq.sum.all) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg))]
  print(paste("summing nutrient for all commodities for ", req, sep = ""))
  print(proc.time())

  ## individual nutrients by staples
  data.table::setkeyv(dt.food.agg,stapleKey)
  dt.food.agg <- dt.food.agg[, (nutListReq.sum.staples) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg))]

  print(paste("summing by food group ", req, sep = ""))
  print(proc.time())

  ## individual nutrients by food group -----
  data.table::setkeyv(dt.food.agg,foodGroupKey)
  dt.food.agg <- dt.food.agg[, (nutListReq.sum.foodGroup) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg))]
  data.table::setkey(dt.food.agg, NULL)
  dt.food.agg <- unique(dt.food.agg)
  # now calculate ratios of nutrient by group to total consumption of the nutrient
  # nutrient from each food item to the total
  #dt.food.ratio <- data.table::copy(dt.all.sum[,(nutListReq.ratio) := dt.all.sum[[nutListReq.Q]] / dt.all.sum[[nutListReq.sum]]])

  print(paste("calculating nutrient share ratios ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each food item to the total
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.ratio.all[k] := get(nutListReq.Q[k]) / get(nutListReq.sum.all[k])]
  }
  #  ratio of nutrient from each staple item to the total
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.ratio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.sum.all[k])]
  }
  #  ratio of nutrient from each food group item to the total
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.ratio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.sum.all[k])]
  }

  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))
  #xxxxx
  # now do ratios with nutrient requirements
  print(paste("calculating nutrient requirement ratios for ", req, sep = ""))
  print(proc.time())

  # change nutrient names in dt.nutsReqPerCap so they differ from those in nutListReq
  nutListReq.Req <- paste(nutListReq,"req", sep = ".")
  data.table::setnames(dt.nutsReqPerCap, old = nutListReq, new = nutListReq.Req)
  data.table::setkeyv(dt.food.agg,allKey)
  data.table::setkeyv(dt.nutsReqPerCap,allKey)
  # temp <- merge(dt.food.agg,dt.nutsReqPerCap, by = c(allKey,nutListReq), all.x = TRUE)
  dt.food.agg <- merge(dt.food.agg,dt.nutsReqPerCap, by = c(allKey), all.x = TRUE)
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  #  ratio of nutrient from each food item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.req.ratio.all[k] := get(nutListReq.Q[k]) / get(nutListReq.Req[k])]
  }

  print(paste("finished with ratio for each food item ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each staple item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.req.ratio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.Req[k])]
  }
  print(paste("finished with ratio for the staple/non staple categories ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each food group item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.req.ratio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.Req[k])]
  }
  print(paste("finished with requirement ratio for the food group categories ", req, sep = ""))
  print(proc.time())
  inDT <- dt.food.agg

  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)
  outName <- paste("food.agg.",reqShortName,sep = "")
  cleanup(inDT, outName,fileloc("resultsDir"), "csv")
}
# end of generateResults function

generateSum <- function(dt.IMPACTfood,scenarioListIMPACT) {
  print("Creating sum for all nutrients")
  #print(proc.time())
  dt.food <- data.table::copy(dt.IMPACTfood)
  dt.food <- dt.food[scenario %in% scenarioListIMPACT,]

  # read in nutrients data and optionally apply cooking retention values -----
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

  nutListReq <- names(dt.nutrients)[2:(ncol(dt.nutrients))]
  deleteList <- c("food_group_code", "staple_code")
  nutListReq <- nutListReq[!nutListReq %in% deleteList]
  nutListReq.Q <-   paste(nutListReq, "Q", sep = ".")
  nutListReq.sum.all <- paste(nutListReq, "sum.all", sep = ".")
  nutListReq.sum.staple <- paste(nutListReq, "sum.staple", sep = ".")

  print("Multiplying dt.nutrients by 10 so in same units as IMPACT commodities")
  dt.nutrients[, (nutListReq) := lapply(.SD, function(x) (x * 10)), .SDcols = nutListReq]
 data.table::setkey(dt.nutrients, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  dt.foodnNuts <-  merge(dt.food, dt.nutrients, by = "IMPACT_code", all = TRUE)
  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  dt.food.agg <- dt.foodnNuts[, (nutListReq.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq][,(nutListReq) := NULL]

  print("summing nutrient for all commodities and staples")
  allKey <-    c("scenario", "region_code.IMPACT159", "year")
  stapleKey <- c(allKey, "staple_code")

  # do sum of all first
  dt.food.agg.all <- data.table::copy(dt.food.agg)
  data.table::setkeyv(dt.food.agg.all, allKey)
  dt.food.agg.all <- dt.food.agg.all[, (nutListReq.sum.all) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg.all))][,(nutListReq.Q) := NULL]
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  dt.food.agg.all[,(deleteListCol) := NULL]
  inDT <- unique(dt.food.agg.all)
  outName <- "dt.nutrients.sum.all"
  cleanup(inDT,outName, fileloc("resultsDir"))

  # do staples next
  dt.food.agg.staples <- data.table::copy(dt.food.agg)
  data.table::setkeyv(dt.food.agg.staples, stapleKey)
  dt.food.agg.staples <- dt.food.agg.staples[, (nutListReq.sum.staple) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                                     by = eval(data.table::key(dt.food.agg.staples))][,(nutListReq.Q) := NULL]
  print(proc.time())
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "food_group_code")
  dt.food.agg.staples[,(deleteListCol) := NULL]
  inDT <- unique(dt.food.agg.staples)
  outName <- "dt.nutrients.sum.staples"
  cleanup(inDT,outName, fileloc("resultsDir"))
}
# run the generateResults script -----
for (i in 1:length(reqsListPercap)) {
  generateResults(reqsListPercap[i],dt.IMPACTfood,scenarioListIMPACT, dt.nutrients)
  print(paste("Done with ", reqsListPercap[i], ". ", length(reqsListPercap) - i," sets of requirements to go.", sep = ""))
}

# run the generateSum script -----
generateSum(dt.IMPACTfood, scenarioListIMPACT)
