#' Nutrient calculations
#' @title "First round of calculations to generate final results"
#' @keywords results calculations
#' @name nutrientCalcs.R
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @description To be added
#Copyright (C) 2016, 2017 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.


#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
source("R/nutrientModFunctions.R")

sourceFile <- "nutrientCalcs.R"
createScriptMetaData()
gdxChoice <- getGdxChoice()
#' get the list of scenarios in the IMPACT data for use below
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)
reqsListPercap <- keyVariable("reqsListPercap")

generateResults.dataPrep <- function(req, dt.foodNnuts, scenarioListIMPACT) {
  #' use dt.food only in the function
  #    dt.food <- data.table::copy(dt.foodNnuts) - commented out Mar 13, 2018. Doesn't appear to be used.
  #    cat("\nLoading dt.foodNnuts for ", req, " for ", suffix, sep = "")

  #' read in nutrient requirements data for a representative consumer -----
  #' Note that these are for SSP age group and gender categories and thus vary by SSP category and year for each region
  dt.nutsReqPerCap <- getNewestVersion(req) # scenarios should just be SSP1, SSP2, etc.

  #' get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutListReq <- names(dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]

  #' list of names for the product of daily availability by nutrient content for each commodity
 # nutListReq <- nutListReq # xxx if this works replace nutListReq.Q with nutListReq. Seems to have so I have done the replacement June 2, 2018

  #nutListReq <- nutListReq[3:4] # Here just for testing. !!! be sure to comment out!!!XXX

  #' dt.nutsReqPerCap has values for the 5 SSP scenarios (but no climate change or experiment effects.
  #' To align with the IMPACT data we need to
  #' - add the climate model and experiment names to the SSP scenario name (SSP1 - 5).
  #' - add copies of dt.nutsReqPerCap for each of the climate models
  #' This for loop adds copies of the nutrient requirements for each climate model used.
  #' create an empty data table with the structure of dt.nutsReqPerCap

   dt.temp <- dt.nutsReqPerCap[FALSE,]

  for (scenName in scenarioListIMPACT) {
    if (gdxChoice %in% "SSPs") {
      SSPName <- unlist(strsplit(scenName, "-"))[1] # get SSP abbrev
      climModel <- unlist(strsplit(scenName, "-"))[2] # get climate model abbrev
      experiment <- unlist(strsplit(scenName, "-"))[3]
    }

     if (gdxChoice %in% "USAIDPriorities") {
       SSPName <- "SSP2"
       climModel <- "HGEM"
       crop <- unlist(strsplit(scenName, "-"))[3]
       experiment <- crop # this extracts the name of the food item and adds c onto the front of it
     }
    #' may need to add the RCP column later. Currently it's not included in the scenario name.
    # if (is.na(experiment)) {experiment <- "REF"}
    # print(experiment)
    temp.nuts <- dt.nutsReqPerCap[scenario %in% SSPName, ]
    # if (is.na(experiment)) {
    # temp.nuts[,scenario := paste(scenario,climModel, sep = "-")]
    # } else {
    # temp.nuts[,scenario := paste(scenario,climModel, experiment, sep = "-")]
    # }
    temp.nuts[, scenario := paste(SSPName, climModel, experiment, sep = "-")]
    dt.temp <- rbind(dt.temp, temp.nuts)
  }

  #' keep just the nutrient requirements scenarios that are in the IMPACT data
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", nutListReq)
dt.nutsReqPerCap <- dt.temp[,keepListCol, with = FALSE]
dt.nutsReqPerCap <- dt.nutsReqPerCap[scenario %in% scenarioListIMPACT]

  #' zinc and iron adjustments moved to dataManagementFoodNnts - Mar 30, 2018
  #' reduce calculations to just the nutrients in nutListReq
  #' plus those needed for iron and zinc bioavailability,
  #' "phytate_mg", "vit_c_mg", "energy_kcal", "protein_g".

  # keepListCol <- c("IMPACT_code","food_group_code","staple_code",nutListReq) commented out June 2, 2018 because doesn't seem to be used

  #' keep extra columns around for iron and zinc bioavailability calculations
  # if ("req_RDA.minrls_percap" %in% req) keepListCol <-
  #   c(keepListCol, "phytate_mg", "energy_kcal", "vit_c_mg", "protein_g")

  #' #' use the data table dt.nuts only in the function
  #' dt.nuts <- data.table::copy(dt.nutrients.adj)
  #' dt.nuts <- dt.nuts[,(keepListCol), with = FALSE]
  #'
  #' #' combine the food availability info with the nutrients for each of the IMPACT commodities
  #' data.table::setkey(dt.nuts, IMPACT_code)
  #' data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  #' dt.foodnNuts <- merge(dt.food, dt.nuts, by = "IMPACT_code", all = TRUE)

  #' multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  #' dt.food.agg.[req] is what eventually gets stored and used later
  #' # Next step not necessary now because the nutrients in foodNnuts are already multiplied by foodAvailpDay. March 29, 2018
  # dt.food.agg <- data.table::copy(dt.foodNnuts[, (nutListReq) := lapply(.SD, function(x)
  #   (x * dt.foodNnuts[['foodAvailpDay']])), .SDcols = nutListReq])
  dt.food.agg <- data.table::copy(dt.foodNnuts)
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  # create name lists for use in operations below -----
  #' total daily availability of each nutrient
  nutListReq.sum.all <- paste(nutListReq, "sum_all", sep = "_")
  #' ratio of daily availability of each nutrient from each commodity to the total availability
  nutListreqRatio.all <- paste(nutListReq, "ratio_all", sep = "_")
  #' ratio of daily availability of each nutrient to the nutrient requirement
  nutListReq.reqRatio.all <- paste(nutListReq, "reqRatio_all", sep = "_")

  #' total daily consumption of each staple
  nutListReq.sum.staples <- paste(nutListReq, "sum_staple", sep = "_")
  #' ratio of daily consumption of each nutrient for each staple to the total consumption
  nutListreqRatio.staples <- paste(nutListReq, "ratio_staple", sep = "_")
  #' ratio of daily consumption of each nutrient for each staple by the nutrient requirement
  nutListReq.reqRatio.staples <- paste(nutListReq, "reqRatio_staple", sep = "_")

  #' total daily consumption of each food group
  nutListReq.sum.foodGroup <- paste(nutListReq, "sum_foodGroup", sep = "_")
  #' ratio of daily consumption of each nutrient for each foodGroup to the total consumption
  nutListReq.reqRatio.foodGroup <- paste(nutListReq, "reqRatio_foodGroup", sep = "_")
  #' ratio of daily consumption of each nutrient for each foodGroup by the nutrient requirement
  nutListreqRatio.foodGroup <- paste(nutListReq, "ratio_foodGroup", sep = "_")
  #' calculate sums and ratios, for all food items, by staples, and by food groups -----
  #' these keys are used to determine what is summed over or ratio made with
  allKey <- c("scenario", "region_code.IMPACT159", "year")
  sumKey <- c("scenario", "region_code.IMPACT159", "year","IMPACT_code")
  stapleKey <- c("scenario", "region_code.IMPACT159", "year", "staple_code")
  foodGroupKey <- c("scenario", "region_code.IMPACT159", "year", "food_group_code")

  #' AMDR are lo and hi ranges for fat, carbohydrate and protein as percent of total kcals;
  #' if statement here excludes AMDR calcs which are done below
  if (!req %in% c("req_AMDR_hi_percap", "req_AMDR_lo_percap")) {
    #' first sum individual nutrients from all commodities
    data.table::setkeyv(dt.food.agg, allKey)
    dt.food.agg <- dt.food.agg[, (nutListReq.sum.all) := lapply(.SD, sum), .SDcols = nutListReq,
                               by = eval(data.table::key(dt.food.agg))]
    #' individual nutrients by staples
    data.table::setkeyv(dt.food.agg,stapleKey)
    dt.food.agg <- dt.food.agg[, (nutListReq.sum.staples) := lapply(.SD, sum), .SDcols = nutListReq,
                               by = eval(data.table::key(dt.food.agg))]

    cat("\nSumming by staples ", req, sep = "")

    # individual nutrients by food group -----
    data.table::setkeyv(dt.food.agg,foodGroupKey)
    dt.food.agg <- dt.food.agg[, (nutListReq.sum.foodGroup) := lapply(.SD, sum), .SDcols = nutListReq,
                               by = eval(data.table::key(dt.food.agg))]
    data.table::setkey(dt.food.agg, NULL)
    dt.food.agg <- unique(dt.food.agg) # doesn't appear to be necessary but leaving in for now June 3, 2018

    #' now calculate ratios of nutrient by group to total consumption of the nutrient
    #' nutrient from each food item to the total

    cat("\nCalculating nutrient share ratios for ", req, sep = "")

    # combining next three for loops into 1 June 3, 2018
    #' ratio of nutrient from each food item to the total
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListreqRatio.all[k] := get(nutListReq[k]) / get(nutListReq.sum.all[k])]
    #' ratio of nutrient from each staple item to the total
      dt.food.agg[,nutListreqRatio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.sum.all[k])]
      #' ratio of nutrient from each food group  to the total
      dt.food.agg[,nutListreqRatio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.sum.all[k])]
    }

    leadingCols <- c("scenario", "region_code.IMPACT159",
                     "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
    laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
    data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))
  }

  # set up AMDRs -----
  if (req %in% c("req_AMDR_hi_percap", "req_AMDR_lo_percap")) {
    #' calculate ratio of kcals from nutrient to total kcals. Multiply by 100 so its in same units as the AMDR values

    #' use different source for dt.food.agg for AMDRs
    dt.nutrients.kcals <- getNewestVersion(paste("dt.nutrients_kcals", suffix, sep = "."), fileloc("resultsDir"))

    dt.food.agg <- data.table::copy(dt.nutrients.kcals)
    # next two lines added to potentially correct a 'length' problem. March 14, 2018
    deleteListRows <- c("caffeine_mg", "cholesterol_mg", "ft_acds_tot_trans_g")
    dt.food.agg <- dt.food.agg[!nutrient %in% deleteListRows]
    formula.wide <- paste("scenario + region_code.IMPACT159 + year ~nutrient")
    dt.food.agg <- data.table::dcast(
      data = dt.food.agg,
      formula = formula.wide,
      value.var = "value"
    )

    #' the .share variables are the percent of the macronutrient kcals in total kcals
    dt.food.agg[, `:=`(
      fat_g.kcalpercent = 100 * kcalsPerDay_fat / kcalsPerDay_tot,
      protein_g.kcalpercent = 100 * kcalsPerDay_protein / kcalsPerDay_tot,
      carbohydrate_g.kcalpercent = 100 * kcalsPerDay_carbohydrate / kcalsPerDay_tot
    )]
    macroKcalShare <- c("carbohydrate_g.kcalpercent", "fat_g.kcalpercent", "protein_g.kcalpercent")
    dt.food.agg[, c("kcalsPerDay_fat", "kcalsPerDay_protein", "kcalsPerDay_carbohydrate", "kcalsPerDay_tot") := NULL]
  }
  #' now do ratios with nutrient requirements

  #' change nutrient names in dt.nutsReqPerCap so they differ from those in nutListReq
  nutListReq.Req <- paste(nutListReq,"req", sep = "_")
  data.table::setnames(dt.nutsReqPerCap, old = nutListReq, new = nutListReq.Req)
  data.table::setkeyv(dt.food.agg, allKey)
  data.table::setkeyv(dt.nutsReqPerCap, allKey)
  dt.food.agg <- merge(dt.food.agg, dt.nutsReqPerCap, by = c(allKey), all.x = TRUE)
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  if (req %in% c("req_AMDR_hi_percap", "req_AMDR_lo_percap")) {
    leadingCols <- c("scenario", "region_code.IMPACT159",
                     "year")
  }
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  #' ratio of nutrient from each food item to the requirement
  for (k in 1:length(nutListReq)) {
    # cat("\nnutlistreq: ", nutListReq) #xxx
    # cat("\nk is  ", k)
    # cat("\nNames dt.food.agg : ", sort(names(dt.food.agg)), "\n")

    if (req %in% c("req_AMDR_hi_percap", "req_AMDR_lo_percap")) {
      dt.food.agg[,nutListReq.reqRatio.all[k] := get(macroKcalShare[k]) / get(nutListReq.Req[k])]
    }else{
      dt.food.agg[,nutListReq.reqRatio.all[k] := get(nutListReq[k]) / get(nutListReq.Req[k])]
    }

  }
  cat("\nFinished with ratio for each food item ", req, sep = "")

  if (!req %in% c("req_AMDR_hi_percap", "req_AMDR_lo_percap")) { #' because staples and food groups are not relevant for AMDR
    #' ratio of nutrient from each staple item to the requirement
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListReq.reqRatio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.Req[k])]
    }
    cat("\nFinished with ratio for the staple/non staple categories ", req, sep = "")
    #' ratio of nutrient from each food group item to the requirement
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListReq.reqRatio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.Req[k])]
    }
    cat("\nFinished with requirement ratio for the food group categories ", req, sep = "")
  }

  inDT <- dt.food.agg
  temp <- gsub("req_","",req)
  reqShortName <- gsub(".percap","",temp)
  outName <- paste("food_agg_",reqShortName, ".", suffix, sep = "")
  desc <- paste("adequacy ratios - all, by food groups, and by staples", reqShortName)
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
}
# end of generateResults function

for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  cat("\nWorking on", suffix, "-----\n")
  dt.foodNnuts <- getNewestVersion(paste("dt.foodNnuts", suffix, sep = "."), fileloc("resultsDir"))
  dt.foodNnuts <- dt.foodNnuts[scenario %in% scenarioListIMPACT]

  dt.nutrients_sum_all <- getNewestVersion(paste("dt.nutrients_sum_all", suffix, sep = "."), fileloc("resultsDir"))

  #' reqsListPercap is a list of the requirements types. Each has a different set of nutrients. These are a subset
  #' of what are in the nutrients requirements tables from IOM. They are the nutrients common to
  #' both the IOM and nutrient content lookup spreadsheet. Some are in physical units (eg. gms; others, especially AMDR are in percent of total energy)
  #' the .percap data are for a representative consumer. They are generated in dataManagement.SSP

  #reqPercap <- reqsListPercap[4] # just for testing!!! XXX
  #scenarioListIMPACT <- "SSP2-MIROC" # just for testing!!! XXX
  #req <- "req_RDA.minrls_percap" # just for testing!!! XXX

  #' run generateResults script -----
  for (i in 1:length(reqsListPercap)) {
    generateResults.dataPrep(reqsListPercap[i],dt.foodNnuts, scenarioListIMPACT)
    cat("\nDone with ", reqsListPercap[i], ". ", length(reqsListPercap) - i," sets of requirements to go.\n", sep = "")
  }
}
finalizeScriptMetadata(metadataDT, sourceFile)
