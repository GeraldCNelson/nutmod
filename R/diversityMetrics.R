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

keepYearList <- c("X2010","X2050")

# Read IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "FoodAvailability")
dt.IMPACTfood <- dt.IMPACTfood[year %in% keepYearList, ]
dt.IMPACTfood <- dt.IMPACTfood[, (keepListCol), with = FALSE]
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood <- unique(dt.IMPACTfood)

# convert food availability from per year to per day
dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")][,FoodAvailability := NULL]
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
cookingretention <- c( "thiamin_mg_cr" , "vit_b12_µg_cr", "riboflavin_mg_cr", "niacin_mg_cr", "vit_b6_mg_cr", "calcium_mg_cr",
                       "iron_mg_cr", "folate_µg_cr",  "potassium_g_cr", "magnesium_mg_cr", "phosphorus_mg_cr",
                       "vit_a_rae_µg_cr", "vit_c_mg_cr", "vit_e_mg_cr", "zinc_mg_cr" )

# read in nutrients data and optionally apply cooking retention values -----
# switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
# switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
# #dt.nutrients.adj is per kg of the edible portion and after cooking losses are applied)
# dt.nutrients.adj <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

# dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
# keepListCol <- c(macroNutrients, vitamins, minerals, fattyAcids)
# dt.nutrientNames_Units <- dt.nutrientNames_Units[,(keepListCol), with = FALSE]

# Shannon diversity ratio -----
#SD = - sum(s_i * ln(s_i)
dt.SDfood <- data.table::copy(dt.IMPACTfood)
dt.SDfood[,foodQ.sum := sum(foodAvailpDay), by = c("scenario","region_code.IMPACT159", "year")]
dt.SDfood[,foodQ.ratio := foodAvailpDay/foodQ.sum]
# ratio of quantity of individual food item to total quantity of food available
dt.foodQratio <- data.table::copy(dt.SDfood)
dt.foodQratio[,c("foodAvailpDay","foodQ.sum") := NULL]
dt.SDfood[,lnfoodQ.ratio := foodQ.ratio * log(foodQ.ratio)]
dt.SDfood[is.nan(lnfoodQ.ratio),lnfoodQ.ratio := 0]
dt.SDfood[,SD := -sum(lnfoodQ.ratio), by = c("scenario","region_code.IMPACT159", "year")]
keepListCol <- c("scenario","region_code.IMPACT159", "year", "SD")
dt.SDfood <- unique(dt.SDfood[, keepListCol, with = FALSE])
foodList <- unique(dt.IMPACTfood$IMPACT_code)
dt.SDfood[, SDnorm := SD * 100/log(length(foodList))]
inDT <- dt.SDfood
outName <- "dt.shannonDiversity"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# MFAD calculations -----
#mfad = ((sum over i from 1 to n)((sum over j from 1 to n) of d_ij/n)
#d_ij = sum over k from 1 to K(i_k - j_k)^2
# K is number of nutrients,
# N is number of food items, i and j are different food items
# f_i proportion of ith food item in the diet; not  used in MFAD
# keepListCol <- c("IMPACT_code", macroNutrients, vitamins, minerals, fattyAcids)
# dt.nutrients.adj <- dt.nutrients.adj[, (keepListCol), with = FALSE]
# nutlist <- names(dt.nutrients.adj)[!names(dt.nutrients.adj) %in% "IMPACT_code"]
# nutlist <- nutlist[!nutlist %in% c("fat_g", "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g")]

# for nutrient distance measures such as in the MFAD, all nutrients must be divided by their RDA
# this is the adequacy ratio.
dt.ratio.macro <- getNewestVersion("RDA.macro_all_reqRatio", fileloc("resultsDir") )
dt.ratio.vits <- getNewestVersion("RDA.vits_all_reqRatio", fileloc("resultsDir") )
dt.ratio.minrls <- getNewestVersion("RDA.minrls_all_reqRatio", fileloc("resultsDir") )

# remove iron and zinc from dt.ratio.minrls because the ratios are not based on PR
dt.ratio.minrls <- dt.ratio.minrls[!nutrient %in% c("iron_mg.reqRatio.all", "zinc_mg.reqRatio.all"),]

# add the PR versions of the req ratios
dt.ratio.PR.iron <- getNewestVersion("PR.iron_all_reqRatio", fileloc("resultsDir") )
dt.ratio.PR.zinc <- getNewestVersion("PR.zinc_all_reqRatio", fileloc("resultsDir") )

# combine the req ratios for all macro, vits, and minerals that have a req ratio
dt.ratio <- data.table::rbindlist(list(dt.ratio.macro, dt.ratio.vits, dt.ratio.minrls, dt.ratio.PR.iron, dt.ratio.PR.zinc))
dt.ratio <- dt.ratio[year %in% keepYearList, ]
dt.ratio[, nutrient := gsub(".reqRatio.all", "", nutrient)]
nutList <- unique(dt.ratio$nutrient)
regionList <- unique(dt.ratio$region_code.IMPACT159)
commodityList <- unique(dt.ratio$IMPACT_code)

# make columns for each of the nutrients
formula.wide <- paste("scenario + region_code.IMPACT159 + year +  IMPACT_code ~ nutrient")
dt.adequateRatio.nuts <- data.table::dcast(
  data = dt.ratio,
  formula = formula.wide,
  value.var = "value"
)
dt.adequateRatio.nuts[is.na(dt.adequateRatio.nuts)] <- 0

# # make columns for each of the IMPACT commodities
formula.wide <- paste("scenario + region_code.IMPACT159 + year + nutrient ~ IMPACT_code")
dt.adequateRatio.commods <- data.table::dcast(
  data = dt.ratio,
  formula = formula.wide,
  value.var = "value"
)
dt.adequateRatio.commods[is.na(dt.adequateRatio.commods)] <- 0

# do by nutrients
# MFAD is calculated on one of the triangles of the distance matrix. Since it is symmetrical, we can
# sum over the whole matrix and divide by 2. .N is the number of food items. It varies by country.
numVits <- 17
numCommods <- 58
dt.MFAD <- data.table::copy(dt.adequateRatio.nuts)
system.time(dt.MFAD[, `:=` (MFAD = sum(dist(.SD)) / (2 * .N)),
                    by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList])
keepListCol.MFAD <- c("scenario", "region_code.IMPACT159", "year", "MFAD" )
dt.MFAD <- unique(dt.MFAD[, (keepListCol.MFAD), with = FALSE])
data.table::setnames(dt.MFAD, old = "MFAD", new = "value")

# scale to 0 to 100 range
dt.MFAD[, value := 100 * (value - min(value)) / (max(value) - min(value)),
        by = c("scenario", "year")]

dt.RAOqe <- merge(dt.adequateRatio.nuts, dt.foodQratio,
                  by = c("scenario", "year", "region_code.IMPACT159", "IMPACT_code"))

# See http://stackoverflow.com/questions/41112062/calculate-raos-quadratic-entropy for an explanation
dt.RAOqe[, RAOqe := c(crossprod(foodQ.ratio, as.matrix(dist(.SD)) %*% foodQ.ratio)) / 2,
                     by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList]

keepListCol.RAOqe <- c("scenario", "region_code.IMPACT159", "year", "RAOqe" )
dt.RAOqe <- unique(dt.RAOqe[, (keepListCol.RAOqe), with = FALSE])
data.table::setnames(dt.RAOqe, old = "RAOqe", new = "value")

# scale to 0 to 100 range
dt.RAOqe[, value := 100 * (value - min(value)) / (max(value) - min(value)),
         by = c("scenario", "year")]

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

# #do by commods
# system.time(dt.adequateRatio.commods[, `:=` (MFAD = sum(dist(.SD)) / .N),
#                                   by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = commodityList])
#
# temp <- dt.adequateRatio.nuts[, dist(.SD),
#                          by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList]

# keepListCol <- c("scenario", "region_code.IMPACT159", "year", "MFAD" )
# dt.MFAD.commods <- unique(dt.adequateRatio.nuts[, (keepListCol), with = FALSE])

# nutrient balance score -----
#qualifying nutrients
# Water", "protein_g",  "totalfiber_g", "Calcium, Ca", "Iron, Fe", "Magnesium, Mg", "Phosphorus, P",
# "Potassium, K", "Zinc, Zn", "Vitamin C, total ascorbic acid", "Thiamin", "Riboflavin", "Niacin",
# "Vitamin B-6", "Folate, DFE", "Vitamin B-12", "Vitamin A, RAE", "Vitamin A, IU", "Vitamin E (alpha-tocopherol)",
# "Vitamin D (D2 + D3)", "Vitamin D", "Vitamin K (phylloquinone)", "Pantheoic Acid", "Linolenic Acid",
# "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)

nutrients.qual <- c("protein_g", "totalfiber_g", minerals, vitamins)
nutrients.qual.missing <- c( "Vitamin A, IU", "Vitamin D IU",  "Pantheoic Acid", "Linolenic Acid",
                             "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)")
nutrients.disqual <- c("fat_g",  addedSugar, "ft_acds_tot_sat_g",  "ft_acds_tot_trans_g", "cholesterol_mg")
Nd <- length(nutrients.disqual) + 1 # plus 1 is because of ethanol, added below
nutrients.other <- c("phytate_mg", "kcals.fat", "kcals.protein", "kcals.carbohydrate", "kcals.sugar",
                     "kcals.ethanol", "caffeine_mg", "ft_acds_plyunst_g", "ft_acds_mono_unsat_g")
#daily Maximal Reference Values (MRV) for that nutrient. The specific disqualifying nutrients
#and the MRVs used were: total fats (<35% of energy); saturated fats (<10% of dietary energy),
#cholesterol (<300 mg), trans fats (<1%) and sodium (<2300 mg), all as specified in the Dietary
#Guidelines for Americans [22], as well as total sugar (<25% of dietary energy, based on recommendations
#from the Institute of Medicine, USA [23]). Alcohol (ethyl) was not included as a disqualifying nutrient

kcalRef <- 2000 #kcals
mrv.sugar.tot <- 25 #percent of dietary energy
mrv.fat.tot <- 35 #percent of dietary energy
mrv.fat.sat <- 10 #percent of dietary energy
mrv.fat.trans <- 1 #percent of dietary energy
mrv.cholesterol <- 300 #mg
mrv.ethanol <- 20 #gm. source is https://en.wikipedia.org/wiki/Recommended_maximum_intake_of_alcoholic_beverages#Daily_maximum_drinks_.28no_weekly_limits_recommended.29

# kcals per gram
fatKcals <- 8.8432122371
proteinKcals <- 4.0630975143
carbsKcals <- 3.8240917782
ethanolKcals <- 6.9

# percent of ethanol in each beverage. Not needed here at the moment
ethanol.beer.percent <- .04
ethanol.wine.percent <- .12
ethanol.spirits.percent <- .47

# quantity of ethanol in 1 kg of beverage (Same units as foodAvailpDay)
ethanol.beer <- ethanol.beer.percent * 1000
ethanol.wine <- ethanol.wine.percent * 1000
ethanol.spirits <- ethanol.spirits.percent * 1000

switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#dt.nutrients.adj is per kg of the raw product after IMPACT conversion and edible portion adjustments applied)
dt.nutrients.adj <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)
dt.nutrients.qual <- dt.nutrients.adj[,c("IMPACT_code", nutrients.qual), with = FALSE]
dt.nutrients.disqual <- dt.nutrients.adj[,c("IMPACT_code", nutrients.disqual), with = FALSE]
# Nq <- length(dt.nutrients.qual) - 1
Nq <- length(nutrients.qual)
Nd <- length(nutrients.disqual)
dt.kcal.lookup <- dt.nutrients.adj[, c("IMPACT_code", "energy_kcal")]

# calculate qi for each food -----
# ratio qualifying nutrient intake to requirement can't be greater than 1. Keep only qualifying nutrients
dt.ratio.adj <- dt.ratio[value > 1, qi.adj := 1][value <= 1, qi.adj := value][nutrient %in% nutrients.qual,]
data.table::setnames(dt.ratio.adj, old = "value", new = "qi")

# get the amount of kcals per day for each food, by scenario and country
temp <- merge(dt.IMPACTfood, dt.kcal.lookup, by = "IMPACT_code")
dt.kcalsInfo <- temp[,kcalsPerCommod := foodAvailpDay * energy_kcal]
dt.kcalsInfo[, kcalsPerDay := sum(kcalsPerCommod), by = c("scenario",  "year", "region_code.IMPACT159")]
dt.kcalsInfo <- dt.kcalsInfo[year %in% keepYearList, c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "kcalsPerCommod", "kcalsPerDay")]

dt.qi <- merge(dt.ratio.adj, dt.kcalsInfo, by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year" ))

# calculate QI for each for each food item, by scenario and country -----
dt.qi[,QI := (sum(qi) / Nq) * (kcalRef / kcalsPerCommod ),
      by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]
keepListCol.QI <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "QI")
dt.QI <- dt.qi[, (keepListCol.QI), with = FALSE]
dt.QI <- unique(dt.QI)
inDT <-  dt.QI
outName <- "dt.qualIndex"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate QIcomposite -----
dt.qi[, QI.comp := sum(QI * kcalsPerCommod / kcalsPerDay), by = c("scenario", "year", "region_code.IMPACT159")]
keepListCol.QIcomp <- c("scenario", "region_code.IMPACT159",  "year", "QI.comp")
dt.QIcomp <- dt.qi[, (keepListCol.QIcomp), with = FALSE]
dt.QIcomp <- unique(dt.QIcomp)
inDT <-  dt.QIcomp
outName <- "dt.QIcomp"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate nutrient balance -----
dt.qi[, NB := (sum(qi.adj) / Nq) * 100,
      by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]
keepListCol.NB <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "NB")
dt.NB <- dt.qi[, (keepListCol.NB), with = FALSE]
dt.NB <- unique(dt.NB)
inDT <-  dt.NB
outName <- "dt.nutBal"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate di for each food -----
temp <- merge(dt.IMPACTfood, dt.nutrients.disqual, by = "IMPACT_code")
# calculate ethanol for each food
temp[IMPACT_code %in% "c_beer", ethanol_g := ethanol.beer]
temp[IMPACT_code %in% "c_wine", ethanol_g := ethanol.wine]
temp[IMPACT_code %in% "c_spirits", ethanol_g := ethanol.spirits]
temp[is.na(ethanol_g), ethanol_g := 0]

#create di values for each disqualifying nutrient.
temp[, `:=`(fat_g = (fat_g * foodAvailpDay) * (fatKcals / mrv.fat.tot),
            sugar_g = (sugar_g * foodAvailpDay * carbsKcals) / mrv.sugar.tot,
            ft_acds_tot_sat_g  = ft_acds_tot_sat_g * foodAvailpDay * fatKcals / mrv.fat.sat,
            # ft_acds_mono_unsat_g  = ft_acds_mono_unsat_g * foodAvailpDay * fatKcals /,
            # ft_acds_plyunst_g  = ft_acds_plyunst_g * foodAvailpDay * fatKcals,
            ft_acds_tot_trans_g  = (ft_acds_tot_trans_g * foodAvailpDay * fatKcals) / mrv.fat.trans,
            cholesterol_mg = cholesterol_mg  * foodAvailpDay / mrv.cholesterol,
            ethanol_g = ethanol_g  * foodAvailpDay / mrv.ethanol
)]

dt.di <- data.table::melt(
  data = temp,
  id.vars = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year"),
  measure.vars = c(nutrients.disqual, "ethanol_g"),
  variable.name = "nutrient",
  value.name = "di",
  variable.factor = FALSE
)
dt.di <- merge(dt.di, dt.kcalsInfo, by = c( "IMPACT_code", "scenario", "region_code.IMPACT159", "year"))

dt.di[, DI := (sum(di) / Nd) * (kcalRef / kcalsPerCommod ),
      by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ]
keepListCol.QI <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "DI")
dt.DI <- dt.di[, (keepListCol.QI), with = FALSE]
dt.DI <- unique(dt.DI)
inDT <-  dt.DI
outName <- "dt.disqualIndex"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate DIcomposite -----
dt.di[, DI.comp := sum(DI * kcalsPerCommod / kcalsPerDay), by = c("scenario", "year", "region_code.IMPACT159")]
keepListCol.DIcomp <- c("scenario", "region_code.IMPACT159",  "year", "DI.comp")
dt.DIcomp <- dt.di[, (keepListCol.DIcomp), with = FALSE]
dt.DIcomp <- unique(dt.DIcomp)
inDT <-  dt.DIcomp
outName <- "dt.DIcomp"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calc NBC as if only one commodity is consumed -----

dt.nuts.sum <- getNewestVersion("dt.nutrients.sum.all", fileloc("resultsDir"))
dt.nuts.sum <- dt.nuts.sum[nutrient %in% nutrients.qual, ]
dt.nuts.sum <- dt.nuts.sum[nutrient %in% "energy_kcal" & year %in% keepYearList,][,nutrient := NULL]

# convert value to the energy ratio needed in the Qualifying index
dt.nutSum.kcal <- dt.nuts.sum[,kcalRatio := value/kcalRef][,value := NULL]

formula.wide <- paste("scenario + region_code.IMPACT159 + year +  IMPACT_code ~ nutrient")
dt.ratio.qual.wide <- data.table::dcast(
  data = dt.ratio.adj,
  formula = formula.wide,
  value.var = "qi.adj"
)

dt.ratio.qual.wide <- merge(dt.ratio.qual.wide, dt.nutSum.kcal,
                            by = c("scenario", "region_code.IMPACT159",  "year"))

formula.wide <- paste("scenario + region_code.IMPACT159 + year ~ nutrient")
dt.nutSum.wide <- data.table::dcast(
  data = dt.nutSum,
  formula = formula.wide,
  value.var = "value"
)

dt.nutSum.wide <- dt.nutSum.wide[year %in% yearList,]
dt.nutSum.wide[, kcal.ratio := (kcals.carbohydrate + kcals.ethanol + kcals.fat + kcals.protein)/Ed]
dt.nutSum.wide[,c("kcals.carbohydrate", "kcals.ethanol", "kcals.fat", "kcals.protein") := NULL]

dt.nutBal <- merge(dt.nutreqholder, dt.nutSum.wide, by = c("scenario", "region_code.IMPACT159", "year"))

dt.nutBal[, (nutnames.ratio) := Map(`/`, mget(as.vector(nutnames)), mget(as.vector(nutnames.req)))]
dt.adequateRatio <- dt.temp[,c(headCols, nutnames.ratio), with = FALSE]


