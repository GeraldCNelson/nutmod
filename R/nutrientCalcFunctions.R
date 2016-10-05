
#' #' Title genFoodGroupResults
#' #'
#' #' @param dt.foodnNuts the data table with per cap daily consumption and nutrient content for each commodity
#' #' @param region the region grouping
#' #' @param nutList.Q nutList.sum the list of nutrients with content of each food item
#' #' @return null. Writes out files with statistics by food groups
#' #' @export
#' genFoodGroupResults <- function(dt.foodnNuts,region,nutList.Q,dt.nutsReqPerCap) {
#'
#'   #  sum nutrient intake by food group -----
#'   foodGroupkey <- c("scenario", region, "food_group_code", "year")
#'   data.table::setkeyv(dt.foodnNuts, foodGroupkey)
#'   dt.foodGroup.sum <- dt.foodnNuts[, lapply(.SD, sum, na.rm=TRUE),
#'                                    by = eval(data.table::key(dt.foodnNuts)) , .SDcols = nutList.Q]
#'
#'   dt.foodGroup.sum <-
#'     unique(dt.foodGroup.sum[, c(foodGroupkey,nutList.Q), with = FALSE])
#'   # get output into year columns
#'   dt.foodGroup.melt <- data.table::melt(
#'     dt.foodGroup.sum,
#'     id.vars = foodGroupkey,
#'     measure.vars = c(nutList.Q),
#'     variable.name = "nutrient",
#'     value.name = "nut_req",
#'     variable.factor = FALSE
#'   )
#'   dt.temp<- data.table::dcast.data.table(dt.foodGroup.melt,
#'                                          scenario + get(region) + food_group_code + nutrient ~ year,
#'                                          value.var = "nut_req")
#'   data.table::setnames(dt.temp,old = names(dt.temp), gsub(".Q.sum","",names(dt.temp)))
#'   inDT <- dt.temp
#'   outName <- "food.group"
#'   cleanup(inDT,outName,fileloc("resultsDir"))
#' }
#'
#' #' Title genStapleResults
#' #'
#' #' @param dt.foodnNuts the data table with per cap daily consumption and nutrient content for each commodity
#' #' @param region the region grouping
#' #' @param nutList.Q.sum the list of nutrients that have sums
#' #' @return null. Writes out files with statistics by staples
#' #' @export
#' genStapleResults <- function(dt.foodnNuts,region,nutList.Q, dt.nutsReqPerCap) {
#'   # sum nutrient intake by staple -----
#'     stapleKey <-    c("scenario", region, "staple_code", "year")
#'     data.table::setkeyv(dt.foodnNuts,stapleKey)
#'   dt.staples.sum <- dt.foodnNuts[, lapply(.SD, sum, na.rm=TRUE),
#'                                  by = eval(data.table::key(dt.foodnNuts)), .SDcols = nutList.sum]
#'   dt.staples.sum <-
#'     unique(dt.staples.sum[, c(stapleKey,nutList.sum), with = FALSE])
#'   # get output into year columns
#'   dt.staples.melt <- data.table::melt(
#'     dt.staples.sum,
#'     id.vars = c("scenario", region, "staple_code", "year"),
#'     measure.vars = c(nutList.sum),
#'     variable.name = "nutrient",
#'     value.name = "nut_req",
#'     variable.factor = FALSE
#'   )
#'   dt.temp<- data.table::dcast.data.table(dt.staples.melt,
#'                                          scenario + get(region) + staple_code + nutrient ~ year,
#'                                          value.var = "nut_req")
#'  # data.table::setnames(dt.temp,old = names(dt.temp), new = gsub(".Q","",names(dt.temp)))
#'
#'   inDT <- dt.temp
#'   outName <- "staples"
#'   cleanup(inDT,outName,fileloc("resultsDir"))
#'
#'   # now do share of requirements
#'   data.table::setkeyv(dt.staples.sum,c("staple_code","scenario", region,"year"))
#'   data.table::setkeyv(dt.nutsReqPerCap,c("scenario", region, "year"))
#'   dt.temp <- dt.staples.sum[dt.nutsReqPerCap]
#' }

cookingRetFishCorrect <- function(switch.useCookingRetnValues, switch.fixFish) {
  # dt.nutrients is in nutrient per 100 grams of the edible portion
  dt.nutrients <- getNewestVersion("dt.nutrients")

  # use cooking retention values if TRUE -----
  if (switch.useCookingRetnValues == "TRUE") {
    # get cooking retention values
    dt.cookRetn <- getNewestVersion("dt.cookingRet")
    data.table::setkey(dt.nutrients,IMPACT_code)
    data.table::setkey(dt.cookRetn,IMPACT_code)
    dt.temp <- dt.nutrients[dt.cookRetn]

    nutrientsWcookingRet <- names(dt.cookRetn)[2:length(dt.cookRetn)]
    for (i in 1:length(nutrientsWcookingRet)) {
      nutrientName <-
        substr(x = nutrientsWcookingRet[i], 1, nchar(nutrientsWcookingRet[i]) - 3)
      nutRetName <- nutrientsWcookingRet[i]
      dt.temp[,(nutrientName) := eval(parse(text = nutrientName)) *
                eval(parse(text = nutRetName))]
    }
    dt.nutrients <- dt.temp[,(c("composite_code",nutrientsWcookingRet)) := NULL]
  }
# fix fish if TRUE -----
  if (switch.fixFish == "TRUE")  {
      deleteListRow <- c("c_Shrimp", "c_Tuna", "c_Salmon")
      dt.nutrients <- dt.nutrients[!IMPACT_code %in% deleteListRow,]
  }
  return(dt.nutrients)
}

#' Title budgetShare
#' calculate the share of per capita income spent on IMPACT commodities
#' writes out data table to the results directory
#' @param dt.IMPACTfood
#' @param region - the grouping of countries to aggregate to
#' @return null
#' @export
budgetShare <- function(dt.IMPACTfood) {
  # prices are in 2005 dollars per metric ton
  # pcGDP is in 1000 2005 dollars
  # 'FoodAvailability' variable is in kgs/person/year. DinY is days in year
  dt.temp <- data.table::copy(dt.IMPACTfood)
  data.table::setkeyv(dt.temp, c("scenario", "region_code.IMPACT159", "year"))
  # budget is in 1000 2005 dollars
  dt.temp[, budget.PWX0 := (sum(FoodAvailability * PWX0 / 1000 )) / 1000, by = eval(data.table::key(dt.temp))]
  dt.temp[, budget.PCX0 := (sum(FoodAvailability * PCX0 / 1000 )) / 1000, by = eval(data.table::key(dt.temp))]
  data.table::setkey(dt.temp, budget.PWX0)
  dt.budget <- dt.temp[!duplicated(budget.PCX0),]
  deleteListCol <- c("IMPACT_code", "FoodAvailability","PCX0","PWX0","CSE")
  dt.budget[,(deleteListCol) := NULL]
  # at world prices -----
  dt.budget[, incSharePWX0 := 100 * budget.PWX0 / pcGDPX0 ]
  # at domestic prices -----
  dt.budget[, incSharePCX0 := 100 * budget.PCX0 / pcGDPX0 ]
  data.table::setkeyv(dt.budget, c("scenario", "region_code.IMPACT159", "year"))
  inDT <- dt.budget
  outName <- "dt.budgetShare"
  cleanup(inDT,outName,fileloc("resultsDir"))
}
