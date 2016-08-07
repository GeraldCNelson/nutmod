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
# #region <- keyVariable("region")

reqList <- keyVariable("reqsList")

#get just nutrient list from req
# temp <- gsub("req.", "", req)
# reqShortName <- gsub(".percap", "", temp)
# ##### This probably needs to be changed
# temp <- paste("food.agg.", reqShortName, sep = "")
# dt.food.agg <- getNewestVersion(temp, fileloc("resultsDir"), "csv")
# # get per capita consumption of each nutrient
# #dt.nuts.sum <- getNewestVersion("all.sum", fileloc("resultsDir"), "csv")
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
  dt.food.agg <- getNewestVersion(temp, fileloc("resultsDir"))
  cols.all <- names(dt.food.agg)[grep(".all", names(dt.food.agg))]
  cols.staple <- names(dt.food.agg)[grep(".staple", names(dt.food.agg))]
  cols.foodGroup <- names(dt.food.agg)[grep(".foodGroup", names(dt.food.agg))]
  deleteListCol <- c(cols.staple, cols.foodGroup)
  dt.food.agg[, (deleteListCol) := NULL]
  dt.food.agg[, RCP := "RCP8.5"]
  scenarioComponents <- c("SSP", "climate_model", "experiment") # RCP added above
  dt.food.agg[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
  dt.food.agg[is.na(experiment), experiment := "REF"]

  dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "."))

  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <- c("scenario", scenarioComponents, "RCP", "region_code.IMPACT159", "year")
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
  dt.nuts.temp <- dt.nutsReqPerCap[scenario %in% unique(dt.sum.copy$SSP),]
  temp <- merge(dt.sum.copy,dt.nuts.temp, by.x = c("SSP", "region_code.IMPACT159", "year"),
                by.y = c("scenario", "region_code.IMPACT159", "year"),
                all.x = TRUE)
  nutListSum <- as.vector(paste(nutList,".sum.all", sep = ""))
  nutListReqRatio <- as.vector(paste(nutList,"_reqRatio", sep = ""))
  nutListVector <- as.vector(nutList)
  # the R code is explained at http://stackoverflow.com/questions/37802687/r-data-table-divide-list-of-columns-by-a-second-list-of-columns
  temp[, (nutListReqRatio) := Map(`/`, mget(nutListSum), mget(nutListVector))]
  keepListCol <- c(basicKey, nutListReqRatio)
  dt.sum.req.ratio <- temp[, keepListCol, with = FALSE]

  #reshape the results to get years in columns
  dt.all.sum.long <- data.table::melt(
    dt.all.sum,  id.vars = basicKey,
    measure.vars = nutList.sum.all, variable.name = "nutrient",
    value.name = "nut_share", variable.factor = FALSE)

  dt.sum.req.ratio.long <- data.table::melt(
    dt.sum.req.ratio,
    id.vars = basicKey,
 #   measure.vars = nutListReqRatio, variable.name = "nutrientReq",
    measure.vars = nutListReqRatio, variable.name = "nutrient",
    value.name = "req_share", variable.factor = FALSE)

  dt.all.ratio.long <- data.table::melt(
    dt.all.ratio, id.vars = sumKey,
    measure.vars = nutList.ratio.all,
    variable.name = "nutrient",
    value.name = "nut_share", variable.factor = FALSE)

  dt.all.req.ratio.long <- data.table::melt(
    dt.all.req.ratio,
    id.vars =  sumKey,
    measure.vars = nutList.req.ratio.all,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE)

  formula.sum.all <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrient ~ year")

  dt.all.sum.wide <- data.table::dcast(
    data = dt.all.sum.long,
    formula = formula.sum.all,
    value.var = "nut_share",
    variable.factor = FALSE)

 # formula.reqRatio <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrientReq ~ year")
  formula.reqRatio <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrient ~ year")

  dt.sum.req.ratio.wide <- data.table::dcast(
    data = dt.sum.req.ratio.long,
    formula = formula.reqRatio,
    value.var = "req_share",
    variable.factor = FALSE)

  formula.all.ratio <- paste("scenario + SSP + climate_model + experiment + RCP + IMPACT_code + region_code.IMPACT159 + nutrient ~ year")

  data.table::setkey(dt.all.ratio.long)
  dt.all.ratio.wide <- data.table::dcast(
    data = dt.all.ratio.long,
    formula = formula.all.ratio,
    value.var = "nut_share")

  dt.all.req.ratio.wide <- data.table::dcast(
    data = dt.all.req.ratio.long,
    formula = formula.all.ratio,
    value.var = "nut_share")

  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)

  inDT <- dt.all.sum.wide
  outName <- paste(reqShortName, "all.sum", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- dt.sum.req.ratio.wide
  outName <- paste(reqShortName, "sum.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- dt.all.ratio.wide
  outName <- paste(reqShortName, "all.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- dt.all.req.ratio.wide
  outName <- paste(reqShortName, "all.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- data.table::as.data.table(colMax(dt.all.req.ratio.wide))
  outName <- "all.req.ratio.cMax"
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- data.table::as.data.table(colMin(dt.all.req.ratio.wide))
  outName <- "all.req.ratio.cMin"
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")
}

# foodGroup function
f.ratios.FG <- function(region, req) {
  print(paste("------ working on food groups for", req))
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resultsDir"))
  cols.all <- names(dt.food.agg)[grep(".all", names(dt.food.agg))]
  cols.staple <- names(dt.food.agg)[grep(".staple", names(dt.food.agg))]
  cols.foodGroup <- names(dt.food.agg)[grep(".foodGroup", names(dt.food.agg))]
  deleteListCol <- c(cols.staple, cols.all)
  dt.food.agg[, (deleteListCol) := NULL]
  dt.food.agg[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.food.agg[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.food.agg[, RCP := "RCP8.5"]
  scenarioComponents <- c("SSP", "climate_model", "experiment") # RCP added above
  dt.food.agg[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
  dt.food.agg[is.na(experiment), experiment := "REF"]
  dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "."))

  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <- c("scenario", scenarioComponents, "RCP", "region_code.IMPACT159", "year")
  foodGroupKey <- c(basicKey, "food_group_code")
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

  formula.foodGroup <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrient+ food_group_code ~ year")

  dt.foodGroup.ratio.wide <- data.table::dcast(
    data = dt.foodGroup.ratio.long,
    formula = formula.foodGroup,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.foodGroup.req.ratio.wide <- data.table::dcast(
    data = dt.foodGroup.req.ratio.long,
    formula = formula.foodGroup,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)

  inDT <- dt.foodGroup.ratio.wide
  outName <- paste(reqShortName, "FG.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- dt.foodGroup.req.ratio.wide
  outName <- paste(reqShortName, "FG.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- data.table::as.data.table(colMax(dt.foodGroup.req.ratio.wide))
  outName <- "FG.req.ratio.cMax"
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- data.table::as.data.table(colMin(dt.foodGroup.req.ratio.wide))
  outName <- "FG.req.ratio.cMin"
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")
}

# staples function
f.ratios.staples <- function(region, req) {
  print(paste("------ working on staples for", req))
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resultsDir"))
  cols.all <- names(dt.food.agg)[grep(".all", names(dt.food.agg))]
  cols.staple <- names(dt.food.agg)[grep(".staple", names(dt.food.agg))]
  cols.foodGroup <- names(dt.food.agg)[grep(".foodGroup", names(dt.food.agg))]
  deleteListCol <- c(cols.all, cols.foodGroup)
  dt.food.agg[, (deleteListCol) := NULL]
  dt.food.agg[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.food.agg[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.food.agg[, RCP := "RCP8.5"]
  scenarioComponents <- c("SSP", "climate_model", "experiment") # RCP added above
  dt.food.agg[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
  dt.food.agg[is.na(experiment), experiment := "REF"]
  dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "."))

  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <- c("scenario", scenarioComponents, "RCP", "region_code.IMPACT159", "year")
  stapleKey <- c(basicKey, "staple_code")
  # the total daily consumption of each staple
  nutList.sum.staple <-    paste(nutList, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutList.ratio.staple <-   paste(nutList, "ratio.staple", sep = ".")
  nutList.req.ratio.staple <- paste(nutList, "req.ratio.staple", sep = ".")
  keepListCol.sum.staple <-    c(stapleKey, nutList.sum.staple)
  keepListCol.ratio.staple <-   c(stapleKey, nutList.ratio.staple)
  keepListCol.req.ratio.staple <- c(stapleKey, nutList.req.ratio.staple)

  dt.staples.sum <- dt.food.agg[,    keepListCol.sum.staple, with = FALSE]
  data.table::setkey(dt.staples.sum, NULL)
  dt.staples.sum <- unique(dt.staples.sum)
  dt.staples.ratio <- dt.food.agg[,   keepListCol.ratio.staple, with = FALSE]
  data.table::setkey(dt.staples.ratio, NULL)
  dt.staples.ratio <- unique(dt.staples.ratio)
  dt.staples.req.ratio <- dt.food.agg[, keepListCol.req.ratio.staple, with = FALSE]
  data.table::setkey(dt.staples.req.ratio, NULL)
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

  formula.staple <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrient+ staple_code ~ year")

  dt.staples.sum.wide <- data.table::dcast(
    data = dt.staples.sum.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.ratio.wide <- data.table::dcast(
    data = dt.staples.ratio.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.req.ratio.wide <- data.table::dcast(
    data = dt.staples.req.ratio.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  reqShortName <- gsub("req.", "", req)
  #reqShortName <- gsub(".percap", "", temp)

  inDT <- dt.staples.sum.wide
  outName <- paste(reqShortName, "staples.sum", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- dt.staples.ratio.wide
  outName <- paste(reqShortName, "staples.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- dt.staples.req.ratio.wide
  outName <- paste(reqShortName, "staples.req.ratio", sep = ".")
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- data.table::as.data.table(colMax(dt.staples.req.ratio.wide))
  outName <- "staples.req.ratio.cMax"
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")

  inDT <- data.table::as.data.table(colMin(dt.staples.req.ratio.wide))
  outName <- "staples.req.ratio.cMin"
  cleanup(inDT, outName, fileloc("resultsDir"), "csv")
}

for (i in reqList) {

  f.ratios.all(region_code.IMPACT159, i)
  f.ratios.staples(region_code.IMPACT159, i)
  f.ratios.FG(region_code.IMPACT159, i)
}

# # fats, etc share of total kcals ------
# # source of conversion http://www.convertunits.com/from/joules/to/calorie+[thermochemical]
# # fat 37kJ/g - 8.8432122371 kCal
# fatKcals <- 8.8432122371
# # protein 17kJ/g - 4.0630975143 kCal
# proteinKcals <- 4.0630975143
# # carbs 16kJ/g) - 3.8240917782
# carbsKcals <- 3.8240917782
# # 1 kJ = 0.23900573614 thermochemical /food calorie (kCal)
# # 1 Kcal = 4.184 kJ
# # alcoholic beverages need to have ethanol energy content included
# # assumptions
# #  beer - 4% ethanol
# #  wine - 12% ethanol
# # spirits - 47% ethanol
# # ethanol contributes 6.9 kcal/g

# reminder dt.IMPACTfood has the annual consumption of a commodity. So long as used for ratios this is ok.
dt.IMPACTfood <- getNewestVersion("dt.IMPACTfood", fileloc("iData"))
deleteListCol <- c("pcGDPX0", "PCX0", "PWX0", "CSE")
dt.IMPACTfood[,(deleteListCol) := NULL]
# dt.alc <- dt.IMPACTfood[IMPACT_code %in% keyVariable("IMPACTalcohol_code"),]
# keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "FoodAvailability")
# dt.alc <- dt.alc[,keepListCol, with = FALSE]
# # dt.alc[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
# # dt.alc[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
# #dt.alc[, RCP := "RCP8.5"]
# scenarioComponents <- c("SSP", "climate_model", "experiment")
# dt.alc[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
# dt.alc[is.na(experiment), experiment := "REF"]
# ethanolkcals <- 6.9
# ethanol.beer <- .04
# ethanol.wine <- .12
# ethanol.spirits <- .47
# formula.wide <- paste("scenario + region_code.IMPACT159 + year ~ IMPACT_code")
# dt.alc.wide <- data.table::dcast(data = dt.alc,
#                                  formula = formula.wide,
#                                  value.var = "FoodAvailability")
# dt.alc.wide[, ethanol.kcal := c_beer * ethanolkcals * ethanol.beer +
#                               c_wine * ethanolkcals * ethanol.wine +
#                            c_spirits * ethanolkcals * ethanol.spirits]
# deleteListCol <- c("c_beer","c_spirits","c_wine")
# dt.alc.wide[, (deleteListCol) := NULL]

# now get the nutrient values
dt.nutrients <- getNewestVersion("dt.nutrients")

dt.nutSum <- getNewestVersion("dt.nutrients.sum", fileloc("resultsDir"))
#dt.nutSum[, RCP := "RCP8.5"]
scenarioComponents <- c("SSP", "climate_model", "experiment")
dt.nutSum[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
dt.nutSum[is.na(experiment), experiment := "REF"]

basicInfo <- c("scenario",  "SSP", "climate_model", "experiment","RCP", "region_code.IMPACT159", "year")
macro <- c("energy_kcal", "protein_g", "carbohydrate_g", "totalfiber_g", "sugar_g", "fat_g" )
minrls <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "sodium_g", "zinc_mg")
vits <- c("niacin_mg", "riboflavin_mg", "folate_µg", "thiamin_mg",
          "vit_a_rae_µg", "vit_b12_µg", "vit_b6_mg", "vit_c_mg",
          "vit_d_μg", "vit_e_mg", "vit_k_µg")
ftyAcids <-  c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
               "cholesterol_mg", "ft_acds_tot_trans_g" )
othr <- c("caffeine_mg")
nutListShort <- c(macro, minrls, vits, ftyAcids, othr)
nutList <- paste(nutListShort,".sum.all", sep = "")
data.table::setnames(dt.nutSum, old = c(nutList), new = c(nutListShort))
data.table::setcolorder(dt.nutSum, c(basicInfo,nutListShort))
keepListCol <- c(basicInfo, macro)
dt.nutSum <- dt.nutSum[,keepListCol, with = FALSE]

# needed to keep original kcals number around
data.table::setnames(dt.nutSum, old = "energy_kcal", new = "energy.kcal")
macroKcals <- c("energy", "protein_g", "carbohydrate_g", "sugar_g", "fat_g", "ethanol")
nutList.kcals <- paste(macroKcals,".kcal", sep = "")
nutList.ratio <- paste(macroKcals,"_share", sep = "")

# calc kcals from macro nutrients -----
dt.nutSum[, protein_g.kcal := protein_g * proteinKcals][, fat_g.kcal := fat_g * fatKcals][, sugar_g.kcal := sugar_g * carbsKcals][, carbohydrate_g.kcal := carbohydrate_g * carbsKcals]
 deleteListCol <- c("protein_g", "carbohydrate_g", "totalfiber_g", "sugar_g", "fat_g" )
dt.nutSum[, (deleteListCol) := NULL]

# add alcohol kcals
dt.nutSum <- merge(dt.nutSum, dt.alc.wide, by = c("scenario", "region_code.IMPACT159", "year"))

# Note. sum.kcals differs from energy_kcal because alcohol is not included in carbohydrates. Maybe other reasons too
dt.nutSum[, sum.kcals := protein_g.kcal + fat_g.kcal + carbohydrate_g.kcal + ethanol.kcal]
dt.nutSum[, diff.kcals := dt.nutSum$energy.kcal - dt.nutSum$sum.kcals]
dt.nutSum[, (nutList.ratio) := lapply(.SD, "/", dt.nutSum$energy_kcal), .SDcols = (nutList.kcals)]
keepListCol <- c(basicInfo, nutList.ratio)
dt.nutSum <- dt.nutSum[, keepListCol, with = FALSE]

scenarioComponents <- c("SSP", "climate_model", "experiment")
basicKey <- c("scenario", scenarioComponents, "RCP", "region_code.IMPACT159", "year")

dt.nutSum.long <- data.table::melt(
  dt.nutSum, id.vars = basicKey,
  measure.vars = nutList.ratio,
#  variable.name = "nutrientReq",
  variable.name = "nutrient",
  value.name = "nut_ratio",
  variable.factor = FALSE)

#formula.sum.all <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrientReq ~ year")
formula.sum.all <- paste("scenario + SSP + climate_model + experiment + RCP + region_code.IMPACT159 + nutrient ~ year")

dt.nutSum.wide <- data.table::dcast(
  data = dt.nutSum.long,
  formula = formula.sum.all,
  value.var = "nut_ratio",
  variable.factor = FALSE)

inDT <- dt.nutSum.wide
outName <- "dt.energy.ratios"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# staple energy shares -----
dt.staple.shares <- dt.nutSum.wide[]

# Shannon diversity ratio -----
#SD = - sum(s_i * ln(s_i)
keepListCol <- c("scenario","region_code.IMPACT159", "year", "IMPACT_code", "FoodAvailability")
dt.SDfood <- dt.IMPACTfood[,keepListCol, with = FALSE]
dt.SDfood[,foodQ.sum := sum(FoodAvailability), by = c("scenario","region_code.IMPACT159", "year")]
dt.SDfood[,foodQ.ratio := FoodAvailability/foodQ.sum]
dt.SDfood[,lnfoodQ.ratio := foodQ.ratio * log(foodQ.ratio)]
dt.SDfood[is.na(lnfoodQ.ratio),lnfoodQ.ratio := 0]
dt.SDfood[,SD := -sum(lnfoodQ.ratio), by = c("scenario","region_code.IMPACT159", "year")]
keepListCol <- c("scenario","region_code.IMPACT159", "year", "SD")
dt.SDfood <- unique(dt.SDfood[, keepListCol, with = FALSE])
foodList <- unique(dt.IMPACTfood$IMPACT_code)
dt.SDfood[, SDnorm := SD * 100/log(length(foodList))]

inDT <- dt.SDfood
outName <- "dt.shannonDiversity"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")


