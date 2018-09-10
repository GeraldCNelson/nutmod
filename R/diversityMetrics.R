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

#' @description Calculated diversity and nutrient benefit metrics.
#'
library(data.table)
# library(maps)

#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
source("R/nutrientModFunctions.R")
sourceFile <- "diversityMetrics.R"
createScriptMetaData()

keepYearList <- keyVariable("keepYearList")

# Read IMPACT food and nutrient content data ------------------------------

for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 1) {switch.vars <- FALSE;  switch.fortification <- FALSE; suffix = "base"}
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}

  dt.foodNnuts <- getNewestVersion(paste("dt.foodNnuts", suffix, sep = "."), fileloc("resultsDir"))
  dt.foodNnuts <- dt.foodNnuts[year %in% keepYearList, ]
  data.table::setkey(dt.foodNnuts)
  dt.foodNnuts <- unique(dt.foodNnuts)

  #' nutrient categories
  macroNutrients <- keyVariable("macronutrients")
  macroNutrients.noFat <- macroNutrients[!macroNutrients %in% "fat_g"]
  vitamins <- keyVariable("vitamins")
  minerals <- keyVariable("minerals")
  energy <- keyVariable("energy")
  addedSugar <- keyVariable("addedSugar")
  fattyAcids <- keyVariable("fattyAcids")
  other <- keyVariable("other")

  #nutrients with cooking retention values
  # cookingretention <- keyVariable("cookingretention")

  # dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
  # keepListCol <- c(macroNutrients, vitamins, minerals, fattyAcids)
  # dt.nutrientNames_Units <- dt.nutrientNames_Units[,(keepListCol), with = FALSE]

  # Shannon diversity ratio ---------------
  #SD = - sum(s_i * ln(s_i)
  dt.SDfood <- data.table::copy(dt.foodNnuts)

  # ratio of quantity of individual food item to total quantity of food available
  #dt.foodQratio <- data.table::copy(dt.SDfood)
  dt.SDfood[,foodQ.ratio := foodAvailpDay/foodQ.sum]
  #dt.foodQratio[,c("foodAvailpDay","foodQ.sum") := NULL]

  #Shannon diversity calcs
  dt.SDfood[,lnfoodQ.ratio := foodQ.ratio * log(foodQ.ratio)]
  dt.SDfood[is.nan(lnfoodQ.ratio),lnfoodQ.ratio := 0]
  dt.SDfood[,SD := -sum(lnfoodQ.ratio), by = c("scenario","region_code.IMPACT159", "year")]
  foodList <- unique(dt.SDfood$IMPACT_code)
  keepListCol <- c("scenario","region_code.IMPACT159", "year", "SD")
  dt.SDfood[, setdiff(names(dt.SDfood), keepListCol) := NULL]
  dt.SDfood <- unique(dt.SDfood)
  dt.SDfood[, SDnorm := SD * 100/log(length(foodList))]
  inDT <- dt.SDfood
  outName <- paste("dt.shannonDiversity", suffix, sep = ".")
  desc <- "Shannon diversity index"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  #' MFAD calculations ---------------
  #' mfad = ((sum over i from 1 to n)((sum over j from 1 to n) of d_ij/n)
  #' d_ij = sum over k from 1 to K(i_k - j_k)^2
  #' K is number of nutrients,
  #' N is number of food items, i and j are different food items
  #' f_i proportion of ith food item in the diet; not  used in MFAD
  # keepListCol <- c("IMPACT_code", macroNutrients, vitamins, minerals, fattyAcids)
  # dt.nutrients.adj <- dt.nutrients.adj[, (keepListCol), with = FALSE]
  # nutlist <- names(dt.nutrients.adj)[!names(dt.nutrients.adj) %in% "IMPACT_code"]
  # nutlist <- c(macroNutrients, vitamins, minerals, fattyAcids)
  # nutlist <- nutlist[!nutlist %in% c("fat_g", "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g")]

  #' for nutrient distance measures such as in the MFAD, all nutrients must be divided by their RDA
  #' this is the adequacy ratio.
  #' get ratios for individual food items
  dt.ratio.macro <- getNewestVersion(paste("reqRatio_all_RDA_macro", suffix, sep = "."), fileloc("resultsDir"))
  dt.ratio.vits <- getNewestVersion(paste("reqRatio_all_RDA_vits", suffix, sep = "."), fileloc("resultsDir"))
  dt.ratio.minrls <- getNewestVersion(paste("reqRatio_all_RDA_minrls", suffix, sep = "."), fileloc("resultsDir"))

  #' get ratios for sum of all food items
  dt.ratio.macro.sum <- getNewestVersion(paste("reqRatio_sum_RDA_macro", suffix, sep = "."), fileloc("resultsDir"))
  dt.ratio.vits.sum <- getNewestVersion(paste("reqRatio_sum_RDA_vits", suffix, sep = "."), fileloc("resultsDir"))
  dt.ratio.minrls.sum <- getNewestVersion(paste("reqRatio_sum_RDA_minrls", suffix, sep = "."), fileloc("resultsDir"))

  #' combine the req ratios for all macro, vits, and minerals that have a req ratio
  dt.ratio <- data.table::rbindlist(list(dt.ratio.macro, dt.ratio.vits, dt.ratio.minrls))
  dt.ratio.sum <- data.table::rbindlist(list(dt.ratio.macro.sum, dt.ratio.vits.sum, dt.ratio.minrls.sum))

  # remove files because I'm having memory problems
  rm( list = c("dt.ratio.macro", "dt.ratio.vits", "dt.ratio.minrls", "dt.ratio.macro.sum", "dt.ratio.vits.sum", "dt.ratio.minrls.sum"))

  dt.ratio <- dt.ratio[year %in% keepYearList, ]
  dt.ratio.sum <- dt.ratio.sum[year %in% keepYearList, ]
  dt.ratio[, nutrient := gsub(".reqRatio.all", "", nutrient)]
  nutList <- unique(dt.ratio$nutrient)
  regionList <- unique(dt.ratio$region_code.IMPACT159)
  commodityList <- unique(dt.ratio$IMPACT_code)

  # make columns for each of the nutrients at the individual food item level
  formula.wide <- paste("scenario + region_code.IMPACT159 + year +  IMPACT_code ~ nutrient")
  dt.adequateRatio.nuts <- data.table::dcast(
    data = dt.ratio,
    formula = formula.wide,
    value.var = "value"
  )
  dt.adequateRatio.nuts[, (names(dt.adequateRatio.nuts)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.adequateRatio.nuts)]

  #' make columns for each of the nutrients at the country level
  formula.wide <- paste("scenario + region_code.IMPACT159 + year ~ nutrient")
  dt.adequateRatio.nuts.sum <- data.table::dcast(
    data = dt.ratio.sum,
    formula = formula.wide,
    value.var = "value"
  )
  dt.adequateRatio.nuts.sum[, (names(dt.adequateRatio.nuts.sum)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.adequateRatio.nuts.sum)]

  #' make columns for each of the IMPACT commodities
  formula.wide <- paste("scenario + region_code.IMPACT159 + year + nutrient ~ IMPACT_code")
  dt.adequateRatio.commods <- data.table::dcast(
    data = dt.ratio,
    formula = formula.wide,
    value.var = "value"
  )
  dt.adequateRatio.commods[, (names(dt.adequateRatio.commods)) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = names(dt.adequateRatio.commods)]

  # do by nutrients
  # # MFAD is calculated on one of the triangles of the distance matrix. Since it is symmetrical, we can
  # # sum over the whole matrix and divide by 2. .N is the number of food items. It varies by country.
  dt.MFAD <- data.table::copy(dt.adequateRatio.nuts)
  dt.MFAD[, `:=`(MFAD = sum(dist(.SD)) / (2 * .N)),
          by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList]
  keepListCol.MFAD <- c("scenario", "region_code.IMPACT159", "year", "MFAD" )
  dt.MFAD[, setdiff(names(dt.MFAD), keepListCol.MFAD) := NULL]
  dt.MFAD <- unique(dt.MFAD)
  data.table::setnames(dt.MFAD, old = "MFAD", new = "value")

  #' scale to 0 to 100 range
  dt.MFAD[, value := 100 * (value - min(value)) / (max(value) - min(value)),
          by = c("scenario", "year")]

  #RAOqe calcs -----
  #' dt.foodQratio - ratio of individual food item weight to total weight of daily availability
  #' dt.adequateRatio.nuts - ratio of nutrient availability in a food to the nutrient requirement

  DT <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "foodAvailpDay", "foodQ.sum")
  DT[, setdiff(names(DT), keepListCol) := NULL]

  dt.RAOqe <- merge(dt.adequateRatio.nuts, DT,
                    by = c("scenario", "year", "region_code.IMPACT159", "IMPACT_code"))
  dt.RAOqe[,foodQ.ratio := foodAvailpDay/foodQ.sum]
  dt.RAOqe[, foodAvailpDay := NULL]

  #' See http://stackoverflow.com/questions/41112062/calculate-raos-quadratic-entropy for an explanation
  #' and also the crossProdExperiments.R code
  dt.RAOqe[, RAOqe := c(crossprod(foodQ.ratio, as.matrix(dist(.SD)) %*% foodQ.ratio)) / 2,
           by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList]

  keepListCol.RAOqe <- c("scenario", "region_code.IMPACT159", "year", "RAOqe" )
  dt.RAOqe[, setdiff(names(dt.RAOqe), keepListCol.RAOqe) := NULL]
  dt.RAOqe <- unique(dt.RAOqe)
  data.table::setnames(dt.RAOqe, old = "RAOqe", new = "value")

  #' scale RAOqe to 0 to 100 range for each scenario and year
  median.2010 <- median(dt.RAOqe[year %in% "X2010", value])
  dt.RAOqe[, value := 100 - (100 * exp((value/median.2010) * log(0.5)))]

  #' for testing
  # temp <- dt.RAOqe[scenario %in% "SSP1-NoCC-REF" & region_code.IMPACT159 %in% "USA" & year %in% "X2010",]
  # temp[, RAOqe := c(crossprod(foodQ.ratio, as.matrix(dist(.SD)) %*% foodQ.ratio)) / 2, .SDcols = nutList]
  #
  # temp[, c("scenario", "year", "region_code.IMPACT159", "IMPACT_code", "foodQ.ratio") := NULL]
  # temp.matrix <- as.matrix(dist(temp))

  inDT <- dt.MFAD
  outName <- paste("dt.MFAD", suffix, sep = ".")
  desc <- "MFAD index"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  inDT <- dt.RAOqe
  outName <- paste("dt.RAOqe", suffix, sep = ".")
  desc <- "Rao's quadratic entroy index"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  # nutrient balance score calculations ---------------
  #' qualifying nutrients from PLOS paper
  #' Water", "protein_g",  "totalfiber_g", "Calcium, Ca", "Iron, Fe", "Magnesium, Mg", "Phosphorus, P",
  #' "Potassium, K", "Zinc, Zn", "Vitamin C, total ascorbic acid", "Thiamin", "Riboflavin", "Niacin",
  #' "Vitamin B-6", "Folate, DFE", "Vitamin B-12", "Vitamin A, RAE", "Vitamin A, IU", "Vitamin E (alpha-tocopherol)",
  #' "Vitamin D (D2 + D3)", "Vitamin D", "Vitamin K (phylloquinone)", "Pantheoic Acid", "Linolenic Acid",
  #' "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)

  # qualifying nutrients -----
  nutrients.qual <- c("protein_g", "totalfiber_g", minerals, vitamins)
  nutrients.qual.missing <- c( "Vitamin A, IU", "Vitamin D IU",  "Pantheoic Acid", "Linolenic Acid",
                               "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)")
  #nutrients.disqual <- c("fat_g",  addedSugar, "ft_acds_tot_sat_g",  "ft_acds_tot_trans_g", "cholesterol_mg")

  # disqualifying nutrients -----
  nutrients.disqual <- c("sugar", "ft_acds_tot_sat", "ethanol")

  # nutrients.other <- c("phytate_mg", "kcals.fat", "kcals.protein", "kcals.carbohydrate", "kcals.sugar",
  #                      "kcals.ethanol", "caffeine_mg", "ft_acds_plyunst_g", "ft_acds_mono_unsat_g")
  #' daily Maximal Reference Values (MRV). The specific disqualifying nutrients
  #' and the MRVs used were: total fats (<35% of energy); saturated fats (<10% of dietary energy),
  #' cholesterol (<300 mg), trans fats (<1%) and sodium (<2300 mg), all as specified in the Dietary
  #' Guidelines for Americans [22], as well as total sugar (<25% of dietary energy, based on recommendations
  #' from the Institute of Medicine, USA [23]). Alcohol (ethyl) was not included as a disqualifying nutrient

  # MRV values -----
  mrv.sugar <- .10 # share of dietary energy
  #mrv.fat.tot <- .35 # share of dietary energy
  mrv.ft_acds_tot_sat <- .10 # share of dietary energy
  # mrv.fat.trans <- .01 # share of dietary energy
  # mrv.cholesterol <- 300 # mg

  kcalRef <- 2000

  #' as of March 24, 2017, the MRV for ethanol is now done as part of the nutrient requirements calculations.
  #' mrv.ethanol <- 20 * ethanolKcals #20 is in gm; convert kcals. source is https://en.wikipedia.org/wiki/Recommended_maximum_intake_of_alcoholic_beverages#Daily_maximum_drinks_.28no_weekly_limits_recommended

  Nq <- length(nutrients.qual)
  Nd <- length(nutrients.disqual)

  # calculate qi for each food ---------------
  #' ratio qualifying nutrient intake to requirement can't be greater than 1 for NBS. Keep only qualifying nutrients
  dt.ratio.adj <- dt.ratio[nutrient %in% nutrients.qual,] #
  rm(dt.ratio)  # because it is really big and may be causing memory availability problems

  #' qi.adj is for use in the NBS; capped at 1
  dt.ratio.adj[value > 1, qi.adj := 1][value <= 1, qi.adj := value]
  data.table::setnames(dt.ratio.adj, old = "value", new = "qi")
  dt.ratio.sum.adj <- dt.ratio.sum[nutrient %in% nutrients.qual,]
  dt.ratio.sum.adj[value > 1, qi.adj := 1][value <= 1, qi.adj := value]
  data.table::setnames(dt.ratio.sum.adj, old = "value", new = "qi")

  # # get the amount of kcals per day for each food, by scenario and country
  # dt.nutrients.kcals <- getNewestVersion("dt.nutrients.kcals", fileloc("resultsDir"))
  #
  # keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "kcalsPerCommod", "foodAvailpDay",
  #                  "kcalsPerDay_tot", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein", "kcalsPerDay_other", "kcalsPerDay_ethanol",
  #                  "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat")
  # dt.nutrients.kcals <- dt.nutrients.kcals[year %in% keepYearList, (keepListCol), with = FALSE]

  # get rid of Somalia data in dt.nutrients.kcals
  # dt.nutrients.kcals <- dt.nutrients.kcals[!region_code.IMPACT159 %in% "SOM"]

  # create food group share of kcals ---------------
  dt.foodGroupLookUp <- data.table::copy(dt.foodNnuts)
  # keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "kcalsPerCommod", "kcalsPerDay_tot", "food_group_code",
  #                  "kcalsPerDay_carbohydrate", "kcalsPerDay_fat", "kcalsPerDay_protein", "kcalsPerDay_other", "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat")
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "kcalsPerCommod", "kcalsPerDay_tot", "food_group_code")
  dt.foodGroupLookUp[, setdiff(names(dt.foodGroupLookUp), keepListCol) := NULL]
  dt.foodGroupLookUp <- unique(dt.foodGroupLookUp)
  dt.foodGroupLookUp[,value := sum(kcalsPerCommod), by = c("scenario", "region_code.IMPACT159", "year", "food_group_code")]
  # convert to percent
  dt.foodGroupLookUp[,value := 100 * value / kcalsPerDay_tot]
   dt.foodGroupLookUp[, c("IMPACT_code", "kcalsPerCommod", "kcalsPerDay_tot") := NULL]
  dt.foodGroupLookUp <- unique(dt.foodGroupLookUp)

  inDT <- dt.foodGroupLookUp
  outName <- paste("dt.KcalShare.foodgroup", suffix, sep = ".")
  desc <- "Share of total kcals from each food group"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  # back to qi calculation
  # combine kcal data in the data table with qi and qi.adj for the nutrients each food
  dt.qi <- merge(dt.ratio.adj, dt.foodNnuts, by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year" ))

  # get just kcals per day for each country -----
  dt.kcalsInfo.region <- data.table::copy(dt.foodNnuts)
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "kcalsPerDay_tot")
  dt.kcalsInfo.region[, setdiff(names(dt.kcalsInfo.region), keepListCol) := NULL]
  dt.kcalsInfo.region <- unique(dt.kcalsInfo.region)

  # combine the qi ratio for each nutrient from all food items with the kcals
  dt.qi.sum <- merge(dt.ratio.sum.adj, dt.kcalsInfo.region, by = c("scenario", "region_code.IMPACT159", "year" ))

  # calculate QI for each for each food item, by scenario and country ---------------
  dt.qi[, QI := (sum(qi) / Nq) * (kcalRef / kcalsPerCommod ),
        by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]

  # dt.qi[, qi.ave := (sum(qi) / Nq),
  #       by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]

  dt.qi[is.na(QI), QI := 0]
  keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "QI")
  dt.qi[, setdiff(names(dt.qi), keepListCol) := NULL]
  inDT <-  unique(dt.qi)
  outName <- paste("dt.indexQual", suffix, sep = ".")
  desc <- "QI index"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)
  dt.qi <- NULL # because it is really big and may be causing memory availability problems
  dt.ratio.adj <- NULL # because it is really big and may be causing memory availability problems
  # # calculate QIcomposite ---------------
  # dt.qi[, QI.comp := sum(QI * kcalsPerCommod / kcalsPerDay_tot), by = c("scenario", "year", "region_code.IMPACT159")]
  # keepListCol.QIcomp <- c("scenario", "region_code.IMPACT159",  "year", "QI.comp")
  # dt.QIcomp <- dt.qi[, (keepListCol.QIcomp), with = FALSE]
  # dt.QIcomp <- unique(dt.QIcomp)
  # data.table::setnames(dt.QIcomp, old = "QI.comp", new = "value")
  # inDT <-  dt.QIcomp
  # outName <- "dt.compQI"
  # desc <- "Composite QI
  # cleanup(inDT, outName, fileloc("resultsDir"), "csv", desc = desc)

  # # calculate nutrient balance for individual commodities ---------------
  # dt.qi[, NB := (sum(qi.adj) / Nq) * 100,
  #       by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]
  # keepListCol.NB <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "NB")
  # dt.NB <- dt.qi[, (keepListCol.NB), with = FALSE]
  # dt.NB <- unique(dt.NB)
  # inDT <-  dt.NB
  # outName <- "dt.nutBal.commods"
  # desc <- "Nutrient balance index for individual countries
  # cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  DT <- data.table::copy(dt.foodNnuts)
  #keep columns that are needed
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "kcalsPerDay_tot", "kcalsPerDay_carbohydrate","kcalsPerDay_fat", "kcalsPerDay_protein",
                   "kcalsPerDay_other", "kcalsPerDay_ethanol", "kcalsPerDay_sugar", "kcalsPerDay_ft_acds_tot_sat")
  #  deleteListCol <- names(DT)[!names(DT) %in% keepListCol]
  DT[, setdiff(names(DT), keepListCol) := NULL]
  DT <- unique(DT)

  nutrients.disqual_kcals <- paste0("kcals.",nutrients.disqual)

  # #set up data for the stacked bar charts for calorie sources.
  # dt.kcals <- data.table::copy(dt.nutrients.kcals)
  # dt.kcals <- unique(dt.kcals)
  # kcalsSources <- c("kcalsPerDay_other", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat",  "kcalsPerDay_protein")
  # dt.kcals.long <- data.table::melt(
  #   data = dt.kcals,
  #   id.vars = c("scenario", "region_code.IMPACT159", "year"),
  #   measure.vars = kcalsSources,
  #   variable.name = "nutrient",
  #   value.name = "value",
  #   variable.factor = FALSE
  # )

  # inDT <- dt.kcals.long
  # outName <- "dt.kcals.values"
  # cleanup(inDT, outName, fileloc("resultsDir"))

  # calculate nutrient to MRV ratio for all disqualifying nutrients -----
  di <- as.vector(paste0("di.",nutrients.disqual[!nutrients.disqual %in% "ethanol"]))
  nutrients.disqual.MRV <- as.vector(paste0("mrv.", nutrients.disqual[!nutrients.disqual %in% "ethanol"]))
  nutrients.disqual.kcals <- as.vector(paste0("kcalsPerDay_",nutrients.disqual[!nutrients.disqual %in% "ethanol"]))
  # see http://stackoverflow.com/questions/42471840/r-divide-a-list-or-vector-of-columns-by-a-list-or-vector-of-constants for source
  #dt.nutrients.kcals[, (di) := Map(`/`, mget(nutrients.disqual.kcals), mget(nutrients.disqual.MRV, envir = .GlobalEnv))]

  # do calculation for nutrients whose MRV is based on share of kcals
  for (l in seq_along(di)) {
    set(DT, i = NULL, j = di[l], value = (DT[[nutrients.disqual.kcals[l]]] / DT[['kcalsPerDay_tot']]) /
          get(nutrients.disqual.MRV[l]))
  }

  # times 100 to put it on 0 to 100 scale; April 29, 2017, remove the * 100

  for (j in di) {
    #  set(dt.nutrients.kcals, i = NULL, j = j, value = dt.nutrients.kcals[[j]] * 100)
    set(DT, i = NULL, j = j, value = DT[[j]])
  }

  #' do ethanol separately because it is about a specific numbers of grams (20 per day) rather than share of total
  dt.mrv.ethanol <- getNewestVersion(paste("reqRatio_sum_MRVs", suffix, sep = "."), fileloc("resultsDir"))
  DT <- merge(dt.mrv.ethanol, DT, by = c("scenario",  "region_code.IMPACT159", "year" ))
  # dt.nutrients.kcals[, di.ethanol := value * 100] #April 29, 2017, remove the * 100
  DT[, di.ethanol := value ]
  deleteListCol <- c("nutrient", "value")
  DT[, (deleteListCol) := NULL ]

  # now add ethanol back to the di list
  di <- c(di, "di.ethanol")

  keepListCol <- c("scenario", "region_code.IMPACT159", "year", di)
  DT[, setdiff(names(DT), keepListCol) := NULL]

  # use standard nutrient names
  newNames <- gsub("di.", "", di)
  newNames <- paste0(newNames, "_g")
  data.table::setnames(DT, old = di, new = newNames)

  DT.long <- data.table::melt(
    data = DT,
    id.vars = c("scenario", "region_code.IMPACT159", "year"),
    measure.vars = newNames,
    variable.name = "nutrient",
    value.name = "value",
    variable.factor = FALSE
  )

  inDT <- unique(DT.long)
  outName <- paste("dt.MRVRatios", suffix, sep = ".")
  desc <- "MRV ratios"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  # keepListCol <- c("scenario", "year", "region_code.IMPACT159",  nutrients.disqual.kcals, di)
  # dt.nutrients.kcals <- dt.nutrients.kcals[, (keepListCol), with = FALSE]
  # dt.di <- data.table::melt(
  #   data = dt.nutrients.kcals,
  #   id.vars = c( "scenario", "region_code.IMPACT159", "year"),
  #   measure.vars = c(paste0("kcalsPerDay_", nutrients.disqual)),
  #   variable.name = "nutrient",
  #   value.name = "di",
  #   variable.factor = FALSE
  # )
  # dt.di <- merge(dt.di, dt.kcalsInfo, by = c( "IMPACT_code", "scenario", "region_code.IMPACT159", "year"))

  # dt.di[, DI := (sum(di) / Nd) * (kcalRef / kcalsPerCommod ),
  #       by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]
  #
  # dt.di[is.na(DI), DI := 0]
  # keepListCol.DI <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "DI")
  # dt.DI <- dt.di[, (keepListCol.DI), with = FALSE]
  # dt.DI <- unique(dt.DI)
  # inDT <-  dt.DI
  # outName <- "dt.indexDisqual"
  # disqualifying index
  # cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  # calculate di.comp -----
  DT[, DI.comp := rowSums(.SD), .SDcols = (newNames)]
  DT[, DI.comp :=  DI.comp / Nd]
  keepListCol.DIcomp <- c( "scenario", "region_code.IMPACT159", "year", "DI.comp")
  DT[, setdiff(names(DT), keepListCol.DIcomp) := NULL]
  DT <- unique(DT)
  data.table::setnames(DT, old = "DI.comp", new = "value")

  # scale compDI to 0 to 100 range for each scenario and year
  median.2010 <- median(DT[year %in% "X2010", value])
  DT[, value := 100 - (100 * exp((value/median.2010) * log(0.5)))]
  inDT <-  DT
  outName <- paste("dt.compDI", suffix, sep = ".")
  desc <- "Composite disqualifying index"
  cleanup(inDT, outName, fileloc("resultsDir"), desc = desc)

  # calculate NBS as if only one commodity is consumed ---------------
  dt.qi.sum[, value := (sum(qi.adj) / Nq) * 100,
            by = c( "scenario", "region_code.IMPACT159", "year") ]
  keepListCol.NB.sum <- c("scenario", "region_code.IMPACT159",  "year", "value")
  dt.qi.sum[, setdiff(names(dt.qi.sum), keepListCol.NB.sum) := NULL]
   dt.qi.sum <- unique(dt.qi.sum)
  inDT <-  dt.qi.sum
  outName <- paste("dt.nutBalScore", suffix, sep = ".")
  desc <- "Nutrient balance score"
  cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
}
finalizeScriptMetadata(metadataDT, sourceFile)
sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
