#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

#' @description Has functions for cooking retention and budget share calculations
#'

cookingRetFishCorrect <- function(switch.useCookingRetnValues, switch.fixFish) {
  dt.nutrients <- getNewestVersion("dt.nutrients", fileloc("mData"))
  # dt.nutrients[,c("zinc_mg", "iron_mg") := NULL] #remove non bioavailable values
  # dt.bioavail_zinc <- getNewestVersion("dt.bioavail_zinc", fileloc("resultsDir"))
  # dt.bioavail_iron <- getNewestVersion("dt.bioavail_iron", fileloc("resultsDir"))
  # keepListCol.zinc <- c("scenario", "region_code.IMPACT159", "year", "zinc_mg")
  # keepListCol.iron <- c("scenario", "region_code.IMPACT159", "year", "iron_mg")
  # dt.bioavail_zinc <- dt.bioavail_zinc[,(keepListCol.zinc), with = FALSE]
  # dt.bioavail_iron <- dt.bioavail_iron[,(keepListCol.iron), with = FALSE]
  # dt.nutrients.temp <- dt.bioavail_iron[,c("scenario", "region_code.IMPACT159", "year")]
  # dt.nutrients.temp.zinc <- cbind(dt.nutrients.temp, dt.nutrients)
  #
  # dt.nutrients <- merge(dt.nutrients, dt.bioavail_iron, by = c("scenario", "region_code.IMPACT159", "year"))
  # dt.nutrients <- merge(dt.nutrients, dt.bioavail_zinc, by = c("scenario", "region_code.IMPACT159", "year"))

  cols.cookretn <- names(dt.nutrients)[grep("_cr",names(dt.nutrients))]
  colsNotToMultiply <- c("IMPACT_code" ,"usda_code","Long_Desc", "food_group_code",
                         "staple_code", "Ref_Desc", "retentioncode_aus", "RetnDesc", cols.cookretn, "phytate_source",
                         "IMPACT_conversion", "edible_share", "food_group_code", "staple_code")

  nutrients.list <- names(dt.nutrients)[!(names(dt.nutrients) %in% colsNotToMultiply)]
  # dt.nutrients is in nutrient per 100 grams of the edible portion
  # reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
  dt.nutrients[ , (nutrients.list) := lapply(.SD, `*`, IMPACT_conversion / 100), .SDcols = nutrients.list]
  # reduce nutrient amount by conversion of all items to edible share
  dt.nutrients[ , (nutrients.list) := lapply(.SD, `*`, edible_share / 100), .SDcols = nutrients.list]

  # drop columns that are not needed.
  deleteListCol <- c("edible_share", "IMPACT_conversion")
  dt.nutrients[, (deleteListCol) := NULL]

  # use cooking retention values if TRUE -----
  if (switch.useCookingRetnValues == "TRUE") {
    # get cooking retention values. Update: commented out because dt.nutrients now has crs in it (11/25/2017)
    # dt.cookRetn <- getNewestVersion("dt.cookingRet")
    # data.table::setkey(dt.nutrients,IMPACT_code)
    # data.table::setkey(dt.cookRetn,IMPACT_code)
    # dt.temp <- dt.nutrients[dt.cookRetn]

    for (i in 1:length(cols.cookretn)) {
      nutrientName <-
        substr(x = cols.cookretn[i], 1, nchar(cols.cookretn[i]) - 3)
      nutRetName <- cols.cookretn[i]
      # multiply amount of nutrient times the cooking retention value (in percent) and divide by 100 to get to share
      dt.nutrients[,(nutrientName) := eval(parse(text = nutrientName)) *
                     eval(parse(text = nutRetName)) / 100]
    }
    dt.nutrients <- dt.nutrients[,(c(cols.cookretn)) := NULL]
  }
  # fix fish if TRUE -----
  if (switch.fixFish == "TRUE")  {
    deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
    dt.nutrients <- dt.nutrients[!IMPACT_code %in% deleteListRow,]
  }
  # convert to nutrients per kg of food
  colsToMultiply <- names(dt.nutrients)[!names(dt.nutrients) %in% colsNotToMultiply]
  dt.nutrients[, (colsToMultiply) := lapply(.SD, function(x) (x * 10)), .SDcols = colsToMultiply]
  return(dt.nutrients)
}

#' Title budgetShare
#' calculate the share of per capita income spent on IMPACT commodities
#' writes out data table to the results directory
#' @param dt.IMPACTfood
#' @param region - the grouping of countries to aggregate to
#' @return null
#' @export
budgetShareNpriceGrowth <- function(dt.IMPACTfood) {
  # prices are in 2005 dollars per metric ton
  # pcGDP is in 1000 2005 dollars
  # 'FoodAvailability' variable is in kgs/person/year. DinY is days in year
  dt.temp <- data.table::copy(dt.IMPACTfood)
  # data.table::setkeyv(dt.temp, c("scenario", "region_code.IMPACT159", "year"))
  # budget is in 1000 2005 dollars
  # dt.temp[, budget.PWX0 := (sum(FoodAvailability * PWX0 / 1000 )) / 1000, by = eval(data.table::key(dt.temp))]
  # dt.temp[, budget.PCX0 := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = eval(data.table::key(dt.temp))]
  dt.temp[, budget.PWX0 := (sum(FoodAvailability * PWX0 / 1000 )) / 1000, by = c("scenario", "region_code.IMPACT159", "year")]
  dt.temp[, budget.PCX0 := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = c("scenario", "region_code.IMPACT159", "year")]
  # data.table::setkey(dt.temp, budget.PWX0) is this necessary?
  # dt.budget <- dt.temp[!duplicated(budget.PCX0),]
  dt.budget <- data.table::copy(dt.temp)
  deleteListCol <- c("IMPACT_code", "FoodAvailability", "foodAvailpDay","PCX0","PWX0")
  dt.budget[,(deleteListCol) := NULL]
  dt.budget <- unique(dt.budget)
  # at world prices -----
  dt.budget[, incSharePWX0 := 100 * budget.PWX0 / pcGDPX0 ]
  # at domestic prices -----
  dt.budget[, incSharePCX0 := 100 * budget.PCX0 / pcGDPX0 ]
  data.table::setkeyv(dt.budget, c("scenario", "region_code.IMPACT159", "year"))
  inDT <- dt.budget
  outName <- "dt.budgetShare"
  cleanup(inDT,outName,fileloc("resultsDir"))

  # get world price change from 2010 to 2050 by food groups
  dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo", fileloc("mData"))
  dt.foodGroupsInfo[, c("description", "food_group_assignment", "food_groups", "food_group_codes", "staple_category") := NULL]

  dt.temp <- merge(dt.foodGroupsInfo, dt.temp, by = "IMPACT_code")
  deleteListRow <- c("c_aqan","c_aqpl", "c_beer", "c_Crust", "c_FrshD", "c_FshOil", "c_Mllsc", "c_ODmrsl", "c_OMarn",
                     "c_OPelag", "c_spirits", "c_wine")
  keepListYears <- c("X2010", "X2050")
  dt.temp <- dt.temp[!IMPACT_code %in% deleteListRow & year %in% keepListYears]
  keepListCol <- c("scenario","IMPACT_code", "FoodAvailability", "food_group_code", "staple_code", "year", "PWX0")
  dt.temp <- dt.temp[,(keepListCol), with = FALSE]
  dt.temp <- unique(dt.temp)

  dt.temp <- dt.temp[, growthRatePW :=  lapply(.SD, function(x)((x/data.table::shift(x))^(1/(2050 - 2010)) - 1) * 100),
                     .SDcols = "PWX0", by = c("scenario","IMPACT_code")]

  dt.temp <- dt.temp[year %in% "X2050"]
  dt.temp[, c("year", "PWX0") := NULL]

  formula.wide.scen <- "IMPACT_code + FoodAvailability + food_group_code + staple_code ~ scenario"
  dt.temp.wide.scen <- data.table::dcast(
    data = dt.temp,
    formula = formula.wide.scen,
    value.var = "growthRatePW")

  formula.wide.FG <- "IMPACT_code+ FoodAvailability + staple_code + scenario ~ food_group_code"
  dt.temp.wide.FG <- data.table::dcast(
    data = dt.temp,
    formula = formula.wide.FG,
    value.var = "growthRatePW")

  formula.wide.staple <- "IMPACT_code+ FoodAvailability + food_group_code + scenario ~ staple_code"
  dt.temp.wide.FG <- data.table::dcast(
    data = dt.temp,
    formula = formula.wide.staple,
    value.var = "growthRatePW")

  #calculate weighted average price growth rate by food groups
  dt.temp[, growthRateAve.FG := weighted.mean(growthRatePW, FoodAvailability), by = c("scenario", "food_group_code")]
  #calculate weighted average price growth rate by food groups
  dt.temp[, growthRateAve.staple := weighted.mean(growthRatePW, FoodAvailability), by = c("scenario", "staple_code")]

  dt.temp.FG <- data.table::copy(dt.temp)
  deleteListCol <- c("IMPACT_code","growthRatePW", "FoodAvailability")
  dt.temp.FG <- unique(dt.temp.FG[, c(deleteListCol, "staple_code", "growthRateAve.staple") := NULL])

  dt.temp.staple <- data.table::copy(dt.temp)
  dt.temp.staple <- unique(dt.temp.staple[, c(deleteListCol, "food_group_code", "growthRateAve.FG") := NULL])

  formula.wide.staple.scen <- "staple_code  ~ scenario"
  dt.temp.staple.wide.scen <- data.table::dcast(
    data = dt.temp.staple,
    formula = formula.wide.staple.scen,
    value.var = "growthRateAve.staple")

  inDT <- dt.temp.staple.wide.scen
  outName <- "dt.priceGrowth.staple.wide"
  cleanup(inDT, outName, fileloc("resultsDir"), "xlsx")

  formula.wide.FG.scen <- "food_group_code  ~ scenario"
  dt.temp.FG.wide.scen <- data.table::dcast(
    data = dt.temp.FG,
    formula = formula.wide.FG.scen,
    value.var = "growthRateAve.FG")

  inDT <- dt.temp.FG.wide.scen
outName <- "dt.priceGrowth.FG.wide"
cleanup(inDT, outName, fileloc("resultsDir"), "xlsx")
}
