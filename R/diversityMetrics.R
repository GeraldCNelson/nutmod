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
#if (!exists("getNewestVersion", mode = "function"))
{
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}

keepYearList <- keyVariable("keepYearList")

# Read IMPACT food data ------------------------------
dt.foodNnuts <- getNewestVersion("dt.foodNnuts", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "foodAvailpDay", "foodQ.sum")
dt.foodNnuts <- dt.foodNnuts[year %in% keepYearList, ]
dt.foodNnuts <- dt.foodNnuts[, (keepListCol), with = FALSE]
data.table::setkey(dt.foodNnuts)
dt.foodNnuts <- unique(dt.foodNnuts)

#nutrient categories
macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g")
macroNutrients.noFat <- c("protein_g", "carbohydrate_g",  "totalfiber_g")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol", "kcals.carbohydrate")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")
others <- c("caffeine_mg", "phytate_mg", "cholesterol_mg")

#nutrients with cooking retention values
cookingretention <- c( "thiamin_mg_cr" , "vit_b12_µg_cr", "riboflavin_mg_cr", "niacin_mg_cr", "vit_b6_mg_cr", "calcium_mg_cr",
                       "iron_mg_cr", "folate_µg_cr",  "potassium_g_cr", "magnesium_mg_cr", "phosphorus_mg_cr",
                       "vit_a_rae_µg_cr", "vit_c_mg_cr", "vit_e_mg_cr", "zinc_mg_cr" )

# read in nutrients data and optionally apply cooking retention values ---------------
# switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
# switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
# #dt.nutrients.adj is per kg of the edible portion and after cooking losses are applied)
# dt.nutrients.adj <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

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
dt.SDfood <- unique(dt.SDfood[, keepListCol, with = FALSE])
dt.SDfood[, SDnorm := SD * 100/log(length(foodList))]
inDT <- dt.SDfood
outName <- "dt.shannonDiversity"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# MFAD calculations ---------------
#mfad = ((sum over i from 1 to n)((sum over j from 1 to n) of d_ij/n)
#d_ij = sum over k from 1 to K(i_k - j_k)^2
# K is number of nutrients,
# N is number of food items, i and j are different food items
# f_i proportion of ith food item in the diet; not  used in MFAD
# keepListCol <- c("IMPACT_code", macroNutrients, vitamins, minerals, fattyAcids)
# dt.nutrients.adj <- dt.nutrients.adj[, (keepListCol), with = FALSE]
# nutlist <- names(dt.nutrients.adj)[!names(dt.nutrients.adj) %in% "IMPACT_code"]
# nutlist <- c(macroNutrients, vitamins, minerals, fattyAcids)
# nutlist <- nutlist[!nutlist %in% c("fat_g", "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g")]

# for nutrient distance measures such as in the MFAD, all nutrients must be divided by their RDA
# this is the adequacy ratio.
#get ratios for individual food items
dt.ratio.macro <- getNewestVersion("RDA.macro_all_reqRatio", fileloc("resultsDir"))
dt.ratio.vits <- getNewestVersion("RDA.vits_all_reqRatio", fileloc("resultsDir"))
dt.ratio.minrls <- getNewestVersion("RDA.minrls_all_reqRatio", fileloc("resultsDir"))

#get ratios for sum of all food items
dt.ratio.macro.sum <- getNewestVersion("RDA.macro_sum_reqRatio", fileloc("resultsDir"))
dt.ratio.vits.sum <- getNewestVersion("RDA.vits_sum_reqRatio", fileloc("resultsDir"))
dt.ratio.minrls.sum <- getNewestVersion("RDA.minrls_sum_reqRatio", fileloc("resultsDir"))

# combine the req ratios for all macro, vits, and minerals that have a req ratio
dt.ratio <- data.table::rbindlist(list(dt.ratio.macro, dt.ratio.vits, dt.ratio.minrls))
dt.ratio.sum <- data.table::rbindlist(list(dt.ratio.macro.sum, dt.ratio.vits.sum, dt.ratio.minrls.sum))
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
dt.adequateRatio.nuts[is.na(dt.adequateRatio.nuts)] <- 0

# make columns for each of the nutrients at the country level
formula.wide <- paste("scenario + region_code.IMPACT159 + year ~ nutrient")
dt.adequateRatio.nuts.sum <- data.table::dcast(
  data = dt.ratio.sum,
  formula = formula.wide,
  value.var = "value"
)
dt.adequateRatio.nuts.sum[is.na(dt.adequateRatio.nuts.sum)] <- 0

# # # make columns for each of the IMPACT commodities
formula.wide <- paste("scenario + region_code.IMPACT159 + year + nutrient ~ IMPACT_code")
dt.adequateRatio.commods <- data.table::dcast(
  data = dt.ratio,
  formula = formula.wide,
  value.var = "value"
)
dt.adequateRatio.commods[is.na(dt.adequateRatio.commods)] <- 0

# do by nutrients
# # MFAD is calculated on one of the triangles of the distance matrix. Since it is symmetrical, we can
# # sum over the whole matrix and divide by 2. .N is the number of food items. It varies by country.
dt.MFAD <- data.table::copy(dt.adequateRatio.nuts)
system.time(dt.MFAD[, `:=`(MFAD = sum(dist(.SD)) / (2 * .N)),
                    by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList])
keepListCol.MFAD <- c("scenario", "region_code.IMPACT159", "year", "MFAD" )
dt.MFAD <- unique(dt.MFAD[, (keepListCol.MFAD), with = FALSE])
data.table::setnames(dt.MFAD, old = "MFAD", new = "value")

# scale to 0 to 100 range
dt.MFAD[, value := 100 * (value - min(value)) / (max(value) - min(value)),
        by = c("scenario", "year")]

#RAOqe calcs -----
# dt.foodQratio - ratio of individual food item weight to total weight of daily availability
# dt.adequateRatio.nuts - ratio of nutrient availability in a food to the nutrient requirement
dt.RAOqe <- merge(dt.adequateRatio.nuts, dt.foodNnuts,
                  by = c("scenario", "year", "region_code.IMPACT159", "IMPACT_code"))
dt.RAOqe[,foodQ.ratio := foodAvailpDay/foodQ.sum]
dt.RAOqe[, foodAvailpDay := NULL]

# See http://stackoverflow.com/questions/41112062/calculate-raos-quadratic-entropy for an explanation
# and also the crossProdExperiments.R code
dt.RAOqe[, RAOqe := c(crossprod(foodQ.ratio, as.matrix(dist(.SD)) %*% foodQ.ratio)) / 2,
         by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList]

keepListCol.RAOqe <- c("scenario", "region_code.IMPACT159", "year", "RAOqe" )
dt.RAOqe <- unique(dt.RAOqe[, (keepListCol.RAOqe), with = FALSE])
data.table::setnames(dt.RAOqe, old = "RAOqe", new = "value")

# scale RAOqe to 0 to 100 range for each scenario and year
median.2010 <- median(dt.RAOqe[year %in% "X2010", value])
dt.RAOqe[, value := 100 - (100 * exp((value/median.2010) * log(0.5)))]


# # for testing
# temp <- dt.RAOqe[scenario %in% "SSP1-NoCC-REF" & region_code.IMPACT159 %in% "USA" & year %in% "X2010",]
# temp[, RAOqe := c(crossprod(foodQ.ratio, as.matrix(dist(.SD)) %*% foodQ.ratio)) / 2, .SDcols = nutList]
#
# temp[, c("scenario", "year", "region_code.IMPACT159", "IMPACT_code", "foodQ.ratio") := NULL]
# temp.matrix <- as.matrix(dist(temp))

inDT <- dt.MFAD
outName <- "dt.MFAD"
cleanup(inDT, outName, fileloc("resultsDir"))

inDT <- dt.RAOqe
outName <- "dt.RAOqe"
cleanup(inDT, outName, fileloc("resultsDir"))

# nutrient balance score calculations ---------------
#qualifying nutrients from PLOS paper
# Water", "protein_g",  "totalfiber_g", "Calcium, Ca", "Iron, Fe", "Magnesium, Mg", "Phosphorus, P",
# "Potassium, K", "Zinc, Zn", "Vitamin C, total ascorbic acid", "Thiamin", "Riboflavin", "Niacin",
# "Vitamin B-6", "Folate, DFE", "Vitamin B-12", "Vitamin A, RAE", "Vitamin A, IU", "Vitamin E (alpha-tocopherol)",
# "Vitamin D (D2 + D3)", "Vitamin D", "Vitamin K (phylloquinone)", "Pantheoic Acid", "Linolenic Acid",
# "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)

# qualifying nutrients -----
nutrients.qual <- c("protein_g", "totalfiber_g", minerals, vitamins)
nutrients.qual.missing <- c( "Vitamin A, IU", "Vitamin D IU",  "Pantheoic Acid", "Linolenic Acid",
                             "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)")
#nutrients.disqual <- c("fat_g",  addedSugar, "ft_acds_tot_sat_g",  "ft_acds_tot_trans_g", "cholesterol_mg")

# disqualifying nutrients -----
nutrients.disqual <- c("sugar", "ft_acds_tot_sat", "ethanol")

# nutrients.other <- c("phytate_mg", "kcals.fat", "kcals.protein", "kcals.carbohydrate", "kcals.sugar",
#                      "kcals.ethanol", "caffeine_mg", "ft_acds_plyunst_g", "ft_acds_mono_unsat_g")
#daily Maximal Reference Values (MRV) for that nutrient. The specific disqualifying nutrients
#and the MRVs used were: total fats (<35% of energy); saturated fats (<10% of dietary energy),
#cholesterol (<300 mg), trans fats (<1%) and sodium (<2300 mg), all as specified in the Dietary
#Guidelines for Americans [22], as well as total sugar (<25% of dietary energy, based on recommendations
#from the Institute of Medicine, USA [23]). Alcohol (ethyl) was not included as a disqualifying nutrient

# MRV values -----
mrv.sugar <- .10 # share of dietary energy
#mrv.fat.tot <- .35 # share of dietary energy
mrv.ft_acds_tot_sat <- .10 # share of dietary energy
# mrv.fat.trans <- .01 # share of dietary energy
# mrv.cholesterol <- 300 # mg

kcalRef <- 2000

# as of March 24, 2017, the MRV for ethanol is now done as part of the nutrient requirements calculations.
# mrv.ethanol <- 20 * ethanolKcals #20 is in gm; convert kcals. source is https://en.wikipedia.org/wiki/Recommended_maximum_intake_of_alcoholic_beverages#Daily_maximum_drinks_.28no_weekly_limits_recommended

switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#dt.nutrients.adj is per kg of the raw product after IMPACT conversion and edible portion adjustments applied)
dt.nutrients.adj <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish) # used only for disqualifying nutrients

#kcals calculation
keepListCol <- c("IMPACT_code", "energy_kcal", "kcals.fat_g","kcals.carbohydrate_g", "kcals.protein_g",
                 "kcals.ethanol_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g")
Nq <- length(nutrients.qual)
Nd <- length(nutrients.disqual)

# calculate qi for each food ---------------
# ratio qualifying nutrient intake to requirement can't be greater than 1 for NBS. Keep only qualifying nutrients
dt.ratio.adj <- dt.ratio[nutrient %in% nutrients.qual,]

# qi.adj is for use in the NBS; capped at 1
dt.ratio.adj[value > 1, qi.adj := 1][value <= 1, qi.adj := value]
data.table::setnames(dt.ratio.adj, old = "value", new = "qi")
dt.ratio.sum.adj <- dt.ratio.sum[nutrient %in% nutrients.qual,]
dt.ratio.sum.adj[value > 1, qi.adj := 1][value <= 1, qi.adj := value]
data.table::setnames(dt.ratio.sum.adj, old = "value", new = "qi")

# get the amount of kcals per day for each food, by scenario and country
dt.nutrients.kcals <- getNewestVersion("dt.nutrients.kcals", fileloc("resultsDir"))


keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "kcalsPerCommod", "foodAvailpDay",
                 "kcalsPerDay.tot", "kcalsPerDay.carbohydrate", "kcalsPerDay.fat", "kcalsPerDay.protein", "kcalsPerDay.other", "kcalsPerDay.ethanol",
                 "kcalsPerDay.sugar", "kcalsPerDay.ft_acds_tot_sat")
dt.nutrients.kcals <- dt.nutrients.kcals[year %in% keepYearList, (keepListCol), with = FALSE]

# get rid of Somalia data in dt.nutrients.kcals
# dt.nutrients.kcals <- dt.nutrients.kcals[!region_code.IMPACT159 %in% "SOM"]

# create nonstaple share of kcals ---------------
dt.foodGroupLU <- getNewestVersion("dt.foodGroupsInfo")
keepListCol <- c("IMPACT_code", "staple_code",  "food_group_code" )
dt.foodGroupLU <- dt.foodGroupLU[, (keepListCol), with = FALSE]
dt.foodGroupLU <- merge(dt.nutrients.kcals, dt.foodGroupLU, by = "IMPACT_code")
dt.stapleLookup <- data.table::copy(dt.foodGroupLU)
dt.stapleLookup <- dt.stapleLookup[, food_group_code := NULL]
dt.stapleLookup <- unique(dt.stapleLookup)
dt.stapleLookup[,value := sum(kcalsPerCommod) / kcalsPerDay.tot, by = c("scenario", "region_code.IMPACT159", "year", "staple_code")]
dt.stapleLookup[, c("IMPACT_code", "kcalsPerCommod", "kcalsPerDay.tot") := NULL]
dt.stapleLookup <- unique(dt.stapleLookup)
dt.stapleLookup <- dt.stapleLookup[staple_code %in% "nonstaple",]

# convert to percent
dt.stapleLookup[,value := value * 100]
inDT <- dt.stapleLookup
outName <- "dt.KcalShare.nonstaple"
cleanup(inDT, outName, fileloc("resultsDir"))

# create food group share of kcals ---------------
dt.foodGroupLookUp <- dt.foodGroupLU[, staple_code := NULL]
dt.foodGroupLookUp <- unique(dt.foodGroupLookUp)
dt.foodGroupLookUp[, value := sum(kcalsPerCommod) / kcalsPerDay.tot, by = c("scenario", "region_code.IMPACT159", "year", "food_group_code")]
dt.foodGroupLookUp[, c("IMPACT_code", "kcalsPerCommod", "kcalsPerDay.tot") := NULL]
dt.foodGroupLookUp <- unique(dt.foodGroupLookUp)

# convert to percent
dt.foodGroupLookUp[,value := value * 100]
inDT <- dt.foodGroupLookUp
outName <- "dt.KcalShare.foodgroup"
cleanup(inDT, outName, fileloc("resultsDir"))

# back to qi calculation
# combine kcal data in the data table with qi and qi.adj for the nutrients each food
dt.qi <- merge(dt.ratio.adj, dt.nutrients.kcals, by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year" ))

# get just kcals per day for each country -----
dt.kcalsInfo.region <- dt.nutrients.kcals[, c("scenario", "region_code.IMPACT159", "year", "kcalsPerDay.tot"), with = FALSE]
dt.kcalsInfo.region <- unique(dt.kcalsInfo.region)

# combine the qi ratio for each nutrient from all food items with the kcals
dt.qi.sum <- merge(dt.ratio.sum.adj, dt.kcalsInfo.region, by = c("scenario", "region_code.IMPACT159", "year" ))

# calculate QI for each for each food item, by scenario and country ---------------
dt.qi[, QI := (sum(qi) / Nq) * (kcalRef / kcalsPerCommod ),
      by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]

# dt.qi[, qi.ave := (sum(qi) / Nq),
#       by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]

dt.qi[is.na(QI), QI := 0]
keepListCol.QI <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "QI")
dt.QI <- dt.qi[, (keepListCol.QI), with = FALSE]
dt.QI <- unique(dt.QI)
inDT <-  dt.QI
outName <- "dt.indexQual"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# # calculate QIcomposite ---------------
# dt.qi[, QI.comp := sum(QI * kcalsPerCommod / kcalsPerDay.tot), by = c("scenario", "year", "region_code.IMPACT159")]
# keepListCol.QIcomp <- c("scenario", "region_code.IMPACT159",  "year", "QI.comp")
# dt.QIcomp <- dt.qi[, (keepListCol.QIcomp), with = FALSE]
# dt.QIcomp <- unique(dt.QIcomp)
# data.table::setnames(dt.QIcomp, old = "QI.comp", new = "value")
# inDT <-  dt.QIcomp
# outName <- "dt.compQI"
# cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# # calculate nutrient balance for individual commodities ---------------
# dt.qi[, NB := (sum(qi.adj) / Nq) * 100,
#       by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]
# keepListCol.NB <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "NB")
# dt.NB <- dt.qi[, (keepListCol.NB), with = FALSE]
# dt.NB <- unique(dt.NB)
# inDT <-  dt.NB
# outName <- "dt.nutBal.commods"
# cleanup(inDT, outName, fileloc("resultsDir"), "csv")

#keep columns that are needed
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "kcalsPerDay.tot", "kcalsPerDay.carbohydrate",
                 "kcalsPerDay.fat", "kcalsPerDay.protein", "kcalsPerDay.other", "kcalsPerDay.ethanol", "kcalsPerDay.sugar", "kcalsPerDay.ft_acds_tot_sat")

dt.nutrients.kcals <- dt.nutrients.kcals[, (keepListCol), with = FALSE]
dt.nutrients.kcals <- unique(dt.nutrients.kcals)

nutrients.disqual_kcals <- paste0("kcals.",nutrients.disqual)

#set up data for the stacked bar charts for calorie sources.
dt.kcals <- data.table::copy(dt.nutrients.kcals)
dt.kcals <- unique(dt.kcals)
kcalsSources <- c("kcalsPerDay.other", "kcalsPerDay.carbohydrate", "kcalsPerDay.fat",  "kcalsPerDay.protein")
dt.kcals.long <- data.table::melt(
  data = dt.kcals,
  id.vars = c("scenario", "region_code.IMPACT159", "year"),
  measure.vars = kcalsSources,
  variable.name = "nutrient",
  value.name = "value",
  variable.factor = FALSE
)

inDT <- dt.kcals.long
outName <- "dt.kcals.values"
cleanup(inDT, outName, fileloc("resultsDir"))

# #kcalsAll <- c(kcalsSources)
# kcalsShare <- paste0(kcalsSources, "_share")
# # calculate ratio of energy to kcals per day for all items in sumlist
# dt.nutrients.kcals[, (kcalsShare) := lapply(.SD, "/", kcalsPerDay.tot/100), .SDcols =  (kcalsSources)] # divide by 100 to get to percent
# dt.nutrients.kcals <- unique(dt.nutrients.kcals)
# inDT <- dt.nutrients.kcals
# outName <- "dt.nutrients.kcals"
# cleanup(inDT, outName, fileloc("mData"))

# calculate nutrient to MRV ratio for all disqualifying nutrients -----
di <- as.vector(paste0("di.",nutrients.disqual[!nutrients.disqual %in% "ethanol"]))
nutrients.disqual.MRV <- as.vector(paste0("mrv.", nutrients.disqual[!nutrients.disqual %in% "ethanol"]))
nutrients.disqual.kcals <- as.vector(paste0("kcalsPerDay.",nutrients.disqual[!nutrients.disqual %in% "ethanol"]))
# see http://stackoverflow.com/questions/42471840/r-divide-a-list-or-vector-of-columns-by-a-list-or-vector-of-constants for source
#dt.nutrients.kcals[, (di) := Map(`/`, mget(nutrients.disqual.kcals), mget(nutrients.disqual.MRV, envir = .GlobalEnv))]

# do calculation for nutrients whose MRV is based on share of kcals
for (l in seq_along(di)) {
  set(dt.nutrients.kcals, i = NULL, j = di[l], value = (dt.nutrients.kcals[[nutrients.disqual.kcals[l]]] / dt.nutrients.kcals[['kcalsPerDay.tot']]) /
        get(nutrients.disqual.MRV[l]))
}

# times 100 to put it on 0 to 100 scale; April 29, 2017, remove the * 100

for (j in di) {
#  set(dt.nutrients.kcals, i = NULL, j = j, value = dt.nutrients.kcals[[j]] * 100)
  set(dt.nutrients.kcals, i = NULL, j = j, value = dt.nutrients.kcals[[j]])
}

# do ethanol separately because it is about a specific numbers of grams (20 per cay) rather than share of total
# ethanol MRV ratio is now created earlier
dt.mrv.ethanol <- getNewestVersion("MRVs_sum_reqRatio", fileloc("resultsDir"))
dt.nutrients.kcals <- merge(dt.mrv.ethanol, dt.nutrients.kcals, by = c("scenario",  "region_code.IMPACT159", "year" ))
# dt.nutrients.kcals[, di.ethanol := value * 100] #April 29, 2017, remove the * 100
dt.nutrients.kcals[, di.ethanol := value ]
deleteListCol <- c("nutrient", "value")
dt.nutrients.kcals[, (deleteListCol) := NULL ]

# now add ethanol back to the di list
di <- c(di, "di.ethanol")
dt.nutrients.kcals <- unique(dt.nutrients.kcals)
temp <- data.table::copy(dt.nutrients.kcals)
keeplistCol <- c("scenario", "region_code.IMPACT159", "year", di)
temp <- temp[, (keeplistCol), with = FALSE]

# use standard nutrient names
newNames <- gsub("di.", "", di)
newNames <- paste0(newNames, "_g")
data.table::setnames(temp, old = di, new = newNames)

temp.long <- data.table::melt(
  data = temp,
  id.vars = c("scenario", "region_code.IMPACT159", "year"),
  measure.vars = newNames,
  variable.name = "nutrient",
  value.name = "value",
  variable.factor = FALSE
)

inDT <- temp.long
outName <- "dt.MRVRatios"
cleanup(inDT, outName, fileloc("resultsDir"))

# keepListCol <- c("scenario", "year", "region_code.IMPACT159",  nutrients.disqual.kcals, di)
# dt.nutrients.kcals <- dt.nutrients.kcals[, (keepListCol), with = FALSE]
# dt.di <- data.table::melt(
#   data = dt.nutrients.kcals,
#   id.vars = c( "scenario", "region_code.IMPACT159", "year"),
#   measure.vars = c(paste0("kcalsPerDay.", nutrients.disqual)),
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
# cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate DIcomposite ---------------
# add alcohol back to the list
nutrients.disqual.kcals <- (paste0("di.",nutrients.disqual))
# calculate di.comp -----
dt.nutrients.kcals[, DI.comp := rowSums(.SD), .SDcols = (nutrients.disqual.kcals)]
dt.nutrients.kcals[, DI.comp :=  DI.comp / Nd]
keepListCol.DIcomp <- c( "scenario", "region_code.IMPACT159", "year", "DI.comp")
dt.nutrients.kcals <- dt.nutrients.kcals[, (keepListCol.DIcomp), with = FALSE]
dt.nutrients.kcals <- unique(dt.nutrients.kcals)
data.table::setnames(dt.nutrients.kcals, old = "DI.comp", new = "value")

# scale compDI to 0 to 100 range for each scenario and year
median.2010 <- median(dt.nutrients.kcals[year %in% "X2010", value])
dt.nutrients.kcals[, value := 100 - (100 * exp((value/median.2010) * log(0.5)))]
inDT <-  dt.nutrients.kcals
outName <- "dt.compDI"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate NBS as if only one commodity is consumed ---------------
dt.qi.sum[, value := (sum(qi.adj) / Nq) * 100,
          by = c( "scenario", "region_code.IMPACT159", "year") ]
keepListCol.NB.sum <- c("scenario", "region_code.IMPACT159",  "year", "value")
dt.NB.sum <- dt.qi.sum[, (keepListCol.NB.sum), with = FALSE]
dt.NB.sum <- unique(dt.NB.sum)
inDT <-  dt.NB.sum
outName <- "dt.nutBalScore"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")
