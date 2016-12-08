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
# library(spam)
# library(ape)
# library(permute)
# library(lattice)
# library(ade4)
# library(geometry)
# library(magic)
# library(abind)
# #library(fields) # needed for rdist function; not used now
# # library(diverse)
# library(vegan)
# library(FD)
library(picante)
#
# library(Matrix) # isSymmetric
# library(matrixcalc) # is.positive.definite
# library(expm) # sqrtm


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
dt.foodQratio <- dt.SDfood[,c("foodAvailpDay","foodQ.sum") := NULL]
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

# combine the req ratios for all macro, vits, and minerals tha have a req ratio
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
#dt.adequateRatio.nuts[is.na(dt.adequateRatio.nuts)] <- 0

# # make columns for each of the IMPACT commodities
# formula.wide <- paste("scenario + region_code.IMPACT159 + year + nutrient ~ IMPACT_code")
# dt.adequateRatio.commods <- data.table::dcast(
#   data = dt.ratio,
#   formula = formula.wide,
#   value.var = "value"
# )
# dt.adequateRatio.commods[is.na(dt.adequateRatio.commods)] <- 0

# do by nuts
# MFAD is calculated on one of the triangles of the distance matrix. Since it is symmetrical, we can
# sum over the whole matrix and divide by 2. .N is the number of food items. It varies by country.
system.time(dt.adequateRatio.nuts[, `:=` (MFAD = sum(dist(.SD)) / (2 * .N)),
                                  by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList])
system.time(dt.adequateRatio.nuts[, `:=` (RAOd = raoD(.SD)$total ),
                                  by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList])
keepListCol.MFAD <- c("scenario", "region_code.IMPACT159", "year", "MFAD", "RAOd" )
dt.MFAD <- unique(dt.adequateRatio.nuts[, (keepListCol.MFAD), with = FALSE])

inDT <- dt.MFAD
outName <- "dt.MFAD"
cleanup(inDT, outName, fileloc("resultsDir"))

keepListCol.RaoD <- c("scenario", "region_code.IMPACT159", "year", "MFAD", "RAOd" )
dt.RaoD <- unique(dt.adequateRatio.nuts[, (keepListCol.RaoD), with = FALSE])
inDT <- dt.RaoD
outName <- "dt.RaoD"
cleanup(inDT, outName, fileloc("resultsDir"))

# #do by commods
# system.time(dt.adequateRatio.commods[, `:=` (MFAD = sum(dist(.SD)) / .N),
#                                   by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = commodityList])
#
# temp <- dt.adequateRatio.nuts[, dist(.SD),
#                          by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = nutList]

# keepListCol <- c("scenario", "region_code.IMPACT159", "year", "MFAD" )
# dt.MFAD.commods <- unique(dt.adequateRatio.nuts[, (keepListCol), with = FALSE])

# test of rdist and dist.
#   They both seem to do the same thing at least at my basic level of use so use dist.
# each row is a nutrient, each column is a food
# cols.commods <- dt.adequateRatio.commods[year %in% "X2010" &
#                                            scenario %in% "SSP1-NoCC-REF" &
#                                            region_code.IMPACT159 %in% "USA", c(commodityList), with = FALSE]
# distance.commods <- dist(cols.commod) # this is a matrix that is the number of nutrients on a side
#
# # each row is a food, each column is a nutrient
# cols.nuts <- dt.adequateRatio.nuts[year %in% "X2010" &
#                                      scenario %in% "SSP1-NoCC-REF" &
#                                      region_code.IMPACT159 %in% "USA", c("IMPACT_code",nutList), with = FALSE]
# distance.nuts <- dist(cols.nuts) # this is a matrix that is the number of foods on a side

# End of test code

# qualifying nutrient balance score -----
#qualifying nutrients
# Water", "protein_g",  "totalfiber_g", "Calcium, Ca", "Iron, Fe", "Magnesium, Mg", "Phosphorus, P",
# "Potassium, K", "Zinc, Zn", "Vitamin C, total ascorbic acid", "Thiamin", "Riboflavin", "Niacin",
# "Vitamin B-6", "Folate, DFE", "Vitamin B-12", "Vitamin A, RAE", "Vitamin A, IU", "Vitamin E (alpha-tocopherol)",
# "Vitamin D (D2 + D3)", "Vitamin D", "Vitamin K (phylloquinone)", "Pantheoic Acid", "Linolenic Acid",
# "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)
nutrients.qual <- c("protein_g", "totalfiber_g", minerals, vitamins)
nutrients.qual.missing <- c( "Vitamin A, IU", "Vitamin D IU",  "Pantheoic Acid", "Linolenic Acid",
                             "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)")
nutrients.disqual <- c("fat_g",  addedSugar, fattyAcids, "cholesterol_mg")
Nd <- length(nutrients.disqual)
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

# kcals per gram
fatKcals <- 8.8432122371
proteinKcals <- 4.0630975143
carbsKcals <- 3.8240917782
ethanolKcals <- 6.9

# # percent of ethanol in each beverage. Not needed here at the moment
# ethanol.beer <- .04
# ethanol.wine <- .12
# ethanol.spirits <- .47

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


# calculate the qi for each food
# ratio qualifying nutrient intake to requirement can't be greater than 1. Keep only qualifing nutrients
dt.ratio.adj <- dt.ratio[value > 1, qi.adj := 1][value <= 1, qi.adj := value][nutrient %in% nutrients.qual,]
data.table::setnames(dt.ratio.adj, old = "value", new = "qi")

# get the amount of kcals in each food
temp <- merge(dt.IMPACTfood, dt.kcal.lookup, by = "IMPACT_code")
dt.kcalsPerCommod <- temp[,kcalsPerCommod := foodAvailpDay * energy_kcal]
dt.kcalsPerCommod <- dt.kcalsPerCommod[year %in% keepYearList, c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "kcalsPerCommod")]

dt.qi <- merge(dt.ratio.adj, dt.kcalsPerCommod, by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year" ))
dt.qi[kcalsPerCommod > 0, QI := (sum(qi) / Nq) * (kcalRef /  kcalsPerCommod),
      by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ][kcalsPerCommod == 0, QI := 0]
keepListCol.QI <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "QI")
inDT <-       dt.qi[, (keepListCol.DI), with = FALSE]
outName <- "dt.qualIndex"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# calculate the di for each food
temp <- merge(dt.IMPACTfood, dt.nutrients.disqual, by = "IMPACT_code")
temp <- merge(temp, dt.kcalsPerCommod, by = c( "IMPACT_code", "scenario", "region_code.IMPACT159", "year"))

#create di values for each disqualifying nutrient
temp [kcalsPerCommod > 0, `:=` (fat_g = (fat_g * foodAvailpDay* fatKcals / kcalsPerCommod) / mrv.fat.tot ,
              sugar_g = (sugar_g * foodAvailpDay * carbsKcals / kcalsPerCommod) / mrv.sugar.tot,
              ft_acds_tot_sat_g  = ft_acds_tot_sat_g * foodAvailpDay * fatKcals / kcalsPerCommod / mrv.fat.sat,
              # ft_acds_mono_unsat_g  = ft_acds_mono_unsat_g * foodAvailpDay * fatKcals /,
              # ft_acds_plyunst_g  = ft_acds_plyunst_g * foodAvailpDay * fatKcals,
              ft_acds_tot_trans_g  = (ft_acds_tot_trans_g * foodAvailpDay * fatKcals / kcalsPerCommod) / mrv.fat.trans,
              cholesterol_mg = cholesterol_mg  * foodAvailpDay / mrv.cholesterol
)]

temp [kcalsPerCommod == 0, `:=` (fat_g = 0 ,
                                sugar_g = 0,
                                ft_acds_tot_sat_g  = 0,
                                # ft_acds_mono_unsat_g  = ft_acds_mono_unsat_g * foodAvailpDay * fatKcals /,
                                # ft_acds_plyunst_g  = ft_acds_plyunst_g * foodAvailpDay * fatKcals,
                                ft_acds_tot_trans_g  = 0,
                                cholesterol_mg = cholesterol_mg  * foodAvailpDay / mrv.cholesterol
)]
deleteListCol <- c( "foodAvailpDay")
temp[, (deleteListCol) := NULL]

dt.DI <- temp[kcalsPerCommod > 0, DI := (sum(fat_g + sugar_g + ft_acds_tot_sat_g + ft_acds_tot_trans_g + cholesterol_mg) / Nd) * (kcalRef /  kcalsPerCommod),
      by = c("IMPACT_code", "scenario", "region_code.IMPACT159", "year") ][kcalsPerCommod == 0, DI := 0]
keepListCol.DI <- c("IMPACT_code", "scenario", "region_code.IMPACT159",  "year", "DI")
inDT <- dt.DI[, (keepListCol.DI), with = FALSE]
outName <- "dt.disqualIndex"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")


                   # this is the quantity of nutrient from all commodities consumed
                   dt.nuts.sum <- getNewestVersion("dt.nutrients.sum.all", fileloc("resultsDir"))
                   dt.nuts.sum <- dt.nuts.sum[nutrient %in% "energy_kcal" & year %in% keepYearList,][,nutrient := NULL]


                   # convert value to the energy ratio needed in the Qualifying index
                   dt.nutSum.kcal <- dt.nuts.sum[,kcalRatio := kcalRef/value][,value := NULL]

                   formula.wide <- paste("scenario + region_code.IMPACT159 + year +  IMPACT_code ~ nutrient")
                   dt.ratio.qual.wide <- data.table::dcast(
                     data = dt.ratio.adj,
                     formula = formula.wide,
                     value.var = "value"
                   )

                   dt.ratio.qual.wide <- merge(dt.ratio.qual.wide, dt.nutSum.kcal, by = c("scenario", "region_code.IMPACT159",  "year", "IMPACT_code"))

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
                   # qualifying index
                   # the ratio of each qualifying nutrient contained in 2000 kcal of a given food relative
                   # to its Dietary Reference Intake (DRI) value.


