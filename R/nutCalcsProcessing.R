#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2015 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
#   for more details at http://www.gnu.org/licenses/.

#' @description To be added

#' @include nutrientModFunctions.R
#' @include nutrientCalcFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}

# choose a grouping of countries -----
region <- keyVariable("region")
reqList <- keyVariable("reqsList")


# req <- "req.EAR" # for testing
#get just nutrient list from req
# temp <- gsub("req.", "", req)
# reqShortName <- gsub(".percap", "", temp)
# ##### This probably needs to be changed
# temp <- paste("food.agg.", reqShortName, sep = "")
# dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
# # get per capita consumption of each nutrient
# #dt.nuts.sum <- getNewestVersion("all.sum", fileloc("resData"))
#
# dt.nutsReqPerCap <- getNewestVersion(req)
# # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
# nutList <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]

# individual food function
req <- "req.EAR" # - for testing purposes
f.ratios.all <- function(region, req){
  print(paste("------ working on all for ", req))
  reqShortName <- gsub("req.", "", req)
#  reqShortName <- gsub(".percap", "", temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
  scenarioComponents <- c("SSP", "climate_model", "experiment", "RCP")

  dt.food.agg[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", , fixed=TRUE)]

  dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "."))

  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <- c(scenarioComponents, "RCP", region, "year")
  sumKey <-  c(basicKey, "IMPACT_code")

  # the total daily consumption of each nutrient
  nutList.sum.all <-    paste(nutList, "sum.all", sep = ".")
  # the ratio of daily consumption of each nutrient to the total consumption
  nutList.ratio.all <-   paste(nutList, "ratio.all", sep = ".")
  # the ratio of daily consumption of each nutrient by the nutrient requirement
  nutList.req.ratio.all <- paste(nutList, "req.ratio.all", sep = ".")
  # the ratio of daily consumption of each nutrient by the nutrient requirement
  nutList.req.ratio.all <- paste(nutList, "req.ratio.all", sep = ".")

  # the list of columns to keep for each group of data tables
  keepListCol.sum.all <-    c(basicKey, nutList.sum.all)
  keepListCol.ratio.all <-   c(sumKey, nutList.ratio.all)
  keepListCol.req.ratio.all <- c(sumKey, nutList.req.ratio.all)

  # create the data table and remove unneeded columns
  dt.all.sum <- dt.food.agg[,keepListCol.sum.all, with = FALSE]
  data.table::setkey(dt.all.sum)
  dt.all.sum <- unique(dt.all.sum)
  dt.all.ratio <- dt.food.agg[,   keepListCol.ratio.all, with = FALSE]
  data.table::setkey(dt.all.ratio)
  dt.all.ratio <- unique(dt.all.ratio)
  dt.all.req.ratio <- dt.food.agg[, keepListCol.req.ratio.all, with = FALSE]
  data.table::setkey(dt.all.req.ratio)
  dt.all.req.ratio <- unique(dt.all.req.ratio)

  # calculate the ratio of nutrient consumption for all commodities to the requirement
   dt.sum.copy <- data.table::copy(dt.all.sum)
  dt.nuts.temp <- dt.nutsReqPerCap[scenario %in% unique(dt.sum.copy$scenario),]
  temp <- merge(dt.sum.copy,dt.nuts.temp, by = c("scenario", "region_code.IMPACT159", "year"), all.x = TRUE)
  nutListSum <- as.vector(paste(nutList,".sum.all", sep = ""))
  nutListReqRatio <- as.vector(paste(nutList,"_reqRatio", sep = ""))
  nutListVector <- as.vector(nutList)
  # explained at http://stackoverflow.com/questions/37802687/r-data-table-divide-list-of-columns-by-a-second-list-of-columns
  temp[, (nutListReqRatio) := Map(`/`, mget(nutListSum), mget(nutListVector))]
  keepListCol <- c("scenario", "climate_model", region, "year",  nutListReqRatio)
  dt.sum.req.ratio <- temp[, keepListCol, with = FALSE]

  #reshape the results to get years in columns
  dt.all.sum.long <- data.table::melt(
    dt.all.sum,  id.vars = basicKey, measure.vars = nutList.sum.all, variable.name = "nutrient",
    value.name = "nut_share", variable.factor = FALSE)

  dt.sum.req.ratio.long <- data.table::melt(
    dt.sum.req.ratio,
    id.vars = c("scenario", "climate_model" ,"region_code.IMPACT159", "year"),
    measure.vars = nutListReqRatio, variable.name = "nutrientReq",
    value.name = "req_share", variable.factor = FALSE)

  dt.all.ratio.long <- data.table::melt(
    dt.all.ratio, id.vars = sumKey,
    measure.vars = nutList.ratio.all,
    variable.name = "nutrient",
    value.name = "nut_share", variable.factor = FALSE)

  dt.all.req.ratio.long <- data.table::melt(
    dt.all.req.ratio,
    id.vars =  c("scenario", region, "year", "IMPACT_code"),
    measure.vars = nutList.req.ratio.all,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE)

  formula.sum.all <- paste("scenario + ", region, " + nutrient ~ year")
  dt.all.sum.wide <- data.table::dcast.data.table(
    data = dt.all.sum.long,
    formula = formula.sum.all,
    value.var = "nut_share",
    variable.factor = FALSE)

  formula.sum.req.all <- paste("scenario + climate_model + ", region, " + nutrientReq ~ year")
  dt.sum.req.ratio.wide <- data.table::dcast.data.table(
    data = dt.sum.req.ratio.long,
    formula = formula.sum.req.all,
    value.var = "req_share",
    variable.factor = FALSE)

  formula.ratio.all <- paste("scenario + ", region, " + nutrient + IMPACT_code ~ year")
  data.table::setkey(dt.all.ratio.long)
  dt.all.ratio.wide <- data.table::dcast(
    data = dt.all.ratio.long,
    formula = formula.ratio.all,
    value.var = "nut_share")

  dt.all.req.ratio.wide <- data.table::dcast(
    data = dt.all.req.ratio.long,
    formula = formula.ratio.all,
    value.var = "nut_share")

  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)

  inDT <- dt.all.sum.wide
  outName <- paste(reqShortName, "all.sum", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- dt.sum.req.ratio.wide
  outName <- paste(reqShortName, "sum.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- dt.all.ratio.wide
  outName <- paste(reqShortName, "all.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- dt.all.req.ratio.wide
  outName <- paste(reqShortName, "all.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- data.table::as.data.table(colMax(dt.all.req.ratio.wide))
  outName <- "all.req.ratio.cMax"
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- data.table::as.data.table(colMin(dt.all.req.ratio.wide))
  outName <- "all.req.ratio.cMin"
  cleanup(inDT, outName, fileloc("resData"))
}

# foodGroup function
f.ratios.FG <- function(region, req) {
  print(paste("------ working on foood groups for", req))
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
  dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "."))
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  scenarioComponents <- c("SSP", "climate_model", "experiment", "RCP")
  foodGroupKey <- c(basicKey, "food.group.code")
  # nutList.sum.foodGroup <-    paste(nutList, "sum.foodGroup", sep = ".")
  nutList.ratio.foodGroup <-   paste(nutList, "ratio.foodGroup", sep = ".")
  nutList.req.ratio.foodGroup <- paste(nutList, "req.ratio.foodGroup", sep = ".")
  # keepListCol.sum.foodGroup <-    c(foodGroupKey, nutList.sum.foodGroup)
  keepListCol.ratio.foodGroup <-   c(foodGroupKey, nutList.ratio.foodGroup)
  keepListCol.req.ratio.foodGroup <- c(foodGroupKey, nutList.req.ratio.foodGroup)
  # dt.foodGroup.sum <- unique(dt.food.agg[,    keepListCol.sum.foodGroup, with = FALSE])
  dt.foodGroup.ratio <- dt.food.agg[,   keepListCol.ratio.foodGroup, with = FALSE]
  data.table::setkey(dt.foodGroup.ratio)
  dt.foodGroup.ratio <- unique(dt.foodGroup.ratio)
  dt.foodGroup.req.ratio <- dt.food.agg[, keepListCol.req.ratio.foodGroup, with = FALSE]
  data.table::setkey(dt.foodGroup.req.ratio)
  dt.foodGroup.req.ratio <- unique(dt.foodGroup.req.ratio)
  #reshape the results to get years in columns
  # dt.foodGroup.sum.long <- data.table::melt(
  #  dt.foodGroup.sum,
  #  id.vars = foodGroupKey,
  #  measure.vars = nutList.sum.foodGroup,
  #  variable.name = "nutrient",
  #  value.name = "nut_share",
  #  variable.factor = FALSE
  # )
  dt.foodGroup.ratio.long <- data.table::melt(
    dt.foodGroup.ratio,
    id.vars = foodGroupKey,
    measure.vars = nutList.ratio.foodGroup,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.foodGroup.req.ratio.long <- data.table::melt(
    dt.foodGroup.req.ratio,
    id.vars = foodGroupKey,
    measure.vars = nutList.req.ratio.foodGroup,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  formula.foodGroup <- paste("scenario + ", region, " + nutrient + food.group.code ~ year")
  # dt.foodGroup.sum.wide <- data.table::dcast.data.table(
  #  data = dt.foodGroup.sum.long,
  #  formula = formula.foodGroup,
  #  value.var = "nut_share",
  #  variable.factor = FALSE
  # )
  dt.foodGroup.ratio.wide <- data.table::dcast.data.table(
    data = dt.foodGroup.ratio.long,
    formula = formula.foodGroup,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.foodGroup.req.ratio.wide <- data.table::dcast.data.table(
    data = dt.foodGroup.req.ratio.long,
    formula = formula.foodGroup,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)

  inDT <- dt.foodGroup.ratio.wide
  outName <- paste(reqShortName, "FG.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- dt.foodGroup.req.ratio.wide
  outName <- paste(reqShortName, "FG.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- data.table::as.data.table(colMax(dt.foodGroup.req.ratio.wide))
  outName <- "FG.req.ratio.cMax"
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- data.table::as.data.table(colMin(dt.foodGroup.req.ratio.wide))
  outName <- "FG.req.ratio.cMin"
  cleanup(inDT, outName, fileloc("resData"))
}

# staples function
f.ratios.staples <- function(region, req) {
  print(paste("------ working on staples", req))
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
  dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "."))
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  scenarioComponents <- c("SSP", "climate_model", "experiment", "RCP")
  basicKey <- c(scenarioComponents, region, "year")
  stapleKey <- c(basicKey, "staple.code")
  # the total daily consumption of each staple
  nutList.sum.staple <-    paste(nutList, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutList.ratio.staple <-   paste(nutList, "ratio.staple", sep = ".")
  nutList.req.ratio.staple <- paste(nutList, "req.ratio.staple", sep = ".")
  keepListCol.sum.staple <-    c(stapleKey, nutList.sum.staple)
  keepListCol.ratio.staple <-   c(stapleKey, nutList.ratio.staple)
  keepListCol.req.ratio.staple <- c(stapleKey, nutList.req.ratio.staple)
  dt.staples.sum <- dt.food.agg[,    keepListCol.sum.staple, with = FALSE]
  data.table::setkey(dt.staples.sum)
  dt.staples.sum <- unique(dt.staples.sum)
  dt.staples.ratio <- dt.food.agg[,   keepListCol.ratio.staple, with = FALSE]
  data.table::setkey(dt.staples.ratio)
  dt.staples.ratio <- unique(dt.staples.ratio)
  dt.staples.req.ratio <- dt.food.agg[, keepListCol.req.ratio.staple, with = FALSE]
  data.table::setkey(dt.staples.req.ratio)
  dt.staples.req.ratio <- unique(dt.staples.req.ratio)
  #reshape the results to get years in columns
  dt.staples.sum.long <- data.table::melt(
    dt.staples.sum,
    id.vars = stapleKey,
    measure.vars = nutList.sum.staple,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.ratio.long <- data.table::melt(
    dt.staples.ratio,
    id.vars = stapleKey,
    measure.vars = nutList.ratio.staple,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.req.ratio.long <- data.table::melt(
    dt.staples.req.ratio,
    id.vars = stapleKey,
    measure.vars = nutList.req.ratio.staple,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  formula.staple <- paste("scenario + ", region, " + nutrient + staple.code ~ year")
  dt.staples.sum.wide <- data.table::dcast.data.table(
    data = dt.staples.sum.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.ratio.wide <- data.table::dcast.data.table(
    data = dt.staples.ratio.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.req.ratio.wide <- data.table::dcast.data.table(
    data = dt.staples.req.ratio.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)

  inDT <- dt.staples.sum.wide
  outName <- paste(reqShortName, "staples.sum", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- dt.staples.ratio.wide
  outName <- paste(reqShortName, "staples.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- dt.staples.req.ratio.wide
  outName <- paste(reqShortName, "staples.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- data.table::as.data.table(colMax(dt.staples.req.ratio.wide))
  outName <- "staples.req.ratio.cMax"
  cleanup(inDT, outName, fileloc("resData"))

  inDT <- data.table::as.data.table(colMin(dt.staples.req.ratio.wide))
  outName <- "staples.req.ratio.cMin"
  cleanup(inDT, outName, fileloc("resData"))
}

for (i in reqList) {
  # req <- "req.UL.vits.percap" # for testing

  # get per capita consumption of each nutrient
  # dt.nuts.sum <- getNewestVersion("all.sum", fileloc("resData"))
  f.ratios.all(region, i)
  f.ratios.staples(region, i)
  f.ratios.FG(region, i)
}

# fats, etc share of total kcals ------
# source of conversion http://www.convertunits.com/from/joules/to/calorie+[thermochemical]
# fat 37kJ/g - 8.8432122371 kCal
fatKcals <- 8.8432122371
# protein 17kJ/g - 4.0630975143 kCal
proteinKcals <- 4.0630975143
# carbs 16kJ/g) - 3.8240917782
carbsKcals <- 3.8240917782
# 1 kJ = 0.23900573614 thermochemical /food calorie (kCal)
# 1 Kcal = 4.184 kJ
dt.nutSum <- getNewestVersion("dt.nutrients.sum", fileloc("resData"))
nutList <- names(dt.nutSum)[4:ncol(dt.nutSum)]
nutListShort <- gsub(".sum.all","",nutList)
data.table::setnames(dt.nutSum, old = c(nutList), new = c(nutListShort))

basicInfo <- c("scenario", region,"year")
macro <- c("energy_kcal", "protein_g", "carbohydrate_g", "totalfiber_g", "sugar_g", "fat_g" )
minrls <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "sodium_g")
vits <- c("niacin_mg", "riboflavin_mg", "folate_µg", "thiamin_mg",
          "vit_a_rae_µg", "vit_b12_µg", "vit_b6_mg", "vit_c_mg",
          "vit_d_μg", "vit_e_mg", "vit_k_µg")
ftyAcids <-  c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
              "cholesterol_mg", "ft_acds_tot_trans_g" )

keepListCol <- c(basicInfo, macro)
dt.nutSum <- dt.nutSum[,keepListCol, with = FALSE]
macroKcals <- c("protein_g", "carbohydrate_g", "sugar_g", "fat_g")
nutList.kcals <- paste(macroKcals,".kcal", sep = "")
nutList.ratio <- paste(macroKcals,"_reqRatio", sep = "")

dt.nutSum[, protein_g.kcal := protein_g * proteinKcals][, fat_g.kcal := fat_g * fatKcals][, sugar_g.kcal := sugar_g * carbsKcals][, carbohydrate_g.kcal := carbohydrate_g * carbsKcals]
dt.nutSum[, (c("protein_g", "carbohydrate_g", "totalfiber_g", "sugar_g", "fat_g" )) := NULL]
# Note. sum.kcals differs from energy_kcal because alcohol is not included in carbohydrates. Maybe other reasons too
dt.nutSum[, sum.kcals := protein_g.kcal + fat_g.kcal + carbohydrate_g.kcal]
dt.nutSum[, diff.kcals := dt.nutSum$energy_kcal - dt.nutSum$sum.kcals]
dt.nutSum[, (nutList.ratio) := lapply(.SD, "/", dt.nutSum$energy_kcal), .SDcols = (nutList.kcals)]
keepListCol <- c("scenario", region, "year", "protein_g.ratio", "fat_g.ratio", "carbohydrate_g.ratio")
dt.nutSum[, keepListCol, with = FALSE]
scenarioComponents <- c("SSP", "climate_model", "experiment", "RCP")
basicKey <- c(scenarioComponents, region, "year")
dt.nutSum.long <- data.table::melt(
  dt.nutSum, id.vars = basicKey,
  measure.vars = nutList.ratio,
  variable.name = "nutrientReq",
  value.name = "nut_ratio", variable.factor = FALSE)

formula.sum.all <- paste("scenario + ", region, " + nutrientReq ~ year")
dt.nutSum.wide <- data.table::dcast.data.table(
  data = dt.nutSum.long,
  formula = formula.sum.all,
  value.var = "nut_ratio",
  variable.factor = FALSE)

inDT <- dt.nutSum.wide
outName <- "dt.energy.ratios"
cleanup(inDT, outName, fileloc("resData"))
