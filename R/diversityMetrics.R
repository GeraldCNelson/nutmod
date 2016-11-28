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
library(maps)
library(spam)
library(ape)
library(permute)
library(lattice)
library(ade4)
library(geometry)
library(magic)
library(abind)
library(fields) # needed for rdist function
# library(diverse)
library(vegan)
library(FD)
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

# Read IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "FoodAvailability")
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
switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#dt.nutrients is per 100 gm of the raw product (ie before edible portion is applied)
dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))
keepListCol <- c(macroNutrients, vitamins, minerals, fattyAcids)
dt.nutrientNames_Units <- dt.nutrientNames_Units[,(keepListCol), with = FALSE]

# Shannon diversity ratio -----
#SD = - sum(s_i * ln(s_i)
dt.SDfood <- data.table::copy(dt.IMPACTfood)
dt.SDfood[,foodQ.sum := sum(foodAvailpDay), by = c("scenario","region_code.IMPACT159", "year")]
dt.SDfood[,foodQ.ratio := foodAvailpDay/foodQ.sum]
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
keepListCol <- c("IMPACT_code", macroNutrients, vitamins, minerals, fattyAcids)
dt.nutrients <- dt.nutrients[, (keepListCol), with = FALSE]
nutlist <- names(dt.nutrients)[!names(dt.nutrients) %in% "IMPACT_code"]
nutlist <- nutlist[!nutlist %in% c("fat_g", "ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g")]

# for nutrient distance measures such as in the MFAD, all nutrients must be divided by their RDA
reqsListPercap <- keyVariable("reqsListPercap") # this line here to remind that there are other items in the reqsList
keepListReqs <- c("req.RDA.vits.percap", "req.RDA.minrls.percap", "req.RDA.macro.percap",
                  "req.PR.iron.percap", "req.PR.zinc.percap")

# get initial setup of dt.nutreqholder from the first of the reqs list
dt.nutreqholder <- getNewestVersion(keepListReqs[1], fileloc("mData"))

#now merge in the rest of the reqs
for (i in 2:length(keepListReqs)) {
  temp <- getNewestVersion(keepListReqs[i], fileloc("mData"))
  dt.nutreqholder <- merge(dt.nutreqholder, temp, by = c("scenario", "region_code.IMPACT159", "year"))
}
dt.nutreqholder[,c("iron_mg.x", "zinc_mg.x") := NULL] # get rid of iron and zinc ratio NOT based on phys response
setnames(dt.nutreqholder, old = c("iron_mg.y", "zinc_mg.y"), new = c("iron_mg", "zinc_mg"))
nutnames <- names(dt.nutreqholder)[!names(dt.nutreqholder) %in% c("scenario", "region_code.IMPACT159", "year") ]
nutnames.req <- paste0(nutnames, ".req" )
setnames(dt.nutreqholder, old = nutnames, new = paste0(nutnames, ".req" ))

# dt.nutreqholder scenarios are just SSPs. Need to convert to IMPACT scenarios
dt.temp <- data.table::copy(dt.nutreqholder[FALSE,])
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)
for (i in scenarioListIMPACT) {
  SSPName <- unlist(strsplit(i, "-"))[1] # get SSP abbrev
  climModel <- unlist(strsplit(i, "-"))[2] # get climate model abbrev
  experiment <- unlist(strsplit(i, "-"))[3] # get experiment abbrev
  # if (is.na(experiment)) {experiment <- "REF"}
  # print(experiment)
  # may need to add the RCP column later. Currently it's not included in the scenario name.
  temp.nuts <- data.table::copy(dt.nutreqholder)
  temp.nuts <- temp.nuts[scenario == SSPName, ]
  # if (is.na(experiment)) {
  #   temp.nuts[,scenario := paste(scenario,climModel, sep = "-")]
  # } else {
  #   temp.nuts[,scenario := paste(scenario,climModel, experiment, sep = "-")]
  # }
  temp.nuts[,scenario := paste(SSPName, climModel, experiment, sep = "-")]
  dt.temp <- rbind(dt.temp, temp.nuts)
}
dt.nutreqholder <- dt.temp

headCols <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code")
nutrients.macro <- getNewestVersion("food.agg.RDA.macro", fileloc("resultsDir"))
keepListCol <- c(headCols, paste0(macroNutrients.noFat, ".Q"))
nutrients.macro <- nutrients.macro[, (keepListCol), with = FALSE]
nutrients.vits <-  getNewestVersion("food.agg.RDA.vits", fileloc("resultsDir"))
keepListCol <- c(headCols, paste0(vitamins, ".Q"))
nutrients.vits <- nutrients.vits[, (keepListCol), with = FALSE]
nutrients.minrls <-  getNewestVersion("food.agg.RDA.minrls", fileloc("resultsDir"))
keepListCol <- c(headCols, paste0(minerals, ".Q"))
nutrients.minrls <- nutrients.minrls[, (keepListCol), with = FALSE]

dt.allnuts <- Reduce(merge,list(nutrients.macro, nutrients.vits, nutrients.minrls))

dt.temp <- merge(dt.allnuts, dt.temp, by = c("scenario", "region_code.IMPACT159", "year"))

# calculate the adequacy ratios, the ratio of the nutrient availability to its requirement
nutnames.ratio <- paste0(nutnames, ".ratio")
dt.temp[, (nutnames.ratio) := Map(`/`, mget(as.vector(paste0(nutnames, ".Q"))), mget(as.vector(nutnames.req)))]
dt.adequateRatio <- dt.temp[,c(headCols, nutnames.ratio), with = FALSE]

#rename from .ratio to just the nutrient name
data.table::setnames(dt.adequateRatio, old = names(dt.adequateRatio), new = gsub(".ratio", "", names(dt.adequateRatio)))
itemlist <- unique(dt.temp$IMPACT_code)

dt.adequateRatio.long <- data.table::melt(
  data = dt.adequateRatio,
  id.vars = c(headCols),
  measure.vars = nutlist,
  variable.name = "nutrient",
  value.name = "value",
  variable.factor = FALSE
)

# make columns for each of the nutrients
formula.wide <- paste("scenario + region_code.IMPACT159 + year +  IMPACT_code ~ nutrient  ")
dt.adequateRatio.nuts <- data.table::dcast(
  data = dt.adequateRatio.long,
  formula = formula.wide,
  value.var = "value"
)
dt.adequateRatio.nuts[is.na(dt.adequateRatio.nuts)] <- 0

# make columns for each of the IMPACT commodities
formula.wide <- paste("scenario + region_code.IMPACT159 + year + nutrient ~ IMPACT_code")
dt.adequateRatio.commods <- data.table::dcast(
  data = dt.adequateRatio.long,
  formula = formula.wide,
  value.var = "value"
)
dt.adequateRatio.commods[is.na(dt.adequateRatio.commods)] <- 0

dt.nutrients.long <- data.table::melt(
  data = dt.nutrients,
  id.vars = "IMPACT_code",
  measure.vars = names(dt.nutrients)[!names(dt.nutrients) == "IMPACT_code"],
  variable.name = "nutrient",
  value.name = "value",
  variable.factor = FALSE
)

formula.wide <- paste("nutrient ~ IMPACT_code")
dt.nutrients.wide <- data.table::dcast(
  data = dt.nutrients.long,
  formula = formula.wide,
  value.var = "value"
)

# distance measure calcs -----
# we need a distance matrix for each country, for each scenario, and for each year in yearList
dt.main <- data.table::copy(dt.adequateRatio.commods)
dt.foodqratio <- data.table::copy(dt.foodQratio)
dt.foodqratio[,c("lnfoodQ.ratio", "SD") := NULL]

formula.wide <- paste("scenario + year + region_code.IMPACT159 ~ IMPACT_code")
dt.foodqratio.wide <- data.table::dcast(
  data = dt.foodqratio,
  formula = formula.wide,
  value.var = "foodQ.ratio"
)
dt.foodqratio.wide[is.na(dt.foodqratio.wide)] <- 0
data.table::setnames(dt.foodqratio.wide, old = itemlist, new = paste0(itemlist,".ratio"))
dt.main <- merge(dt.main, dt.foodqratio.wide, by = c("scenario", "year", "region_code.IMPACT159"))
yearList <- c("X2010","X2050")
dt.main <- dt.main[ year %in% yearList,]
cols <- setdiff(colnames(dt.main), c("scenario", "region_code.IMPACT159", "year", "nutrient", paste0(itemlist,".ratio")))
system.time(dt.main[, `:=` (MFAD = sum(rdist(.SD)) / .N),
                    by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = cols])
# system.time(dt.main[, `:=` (RaoQe = MFAD * .N * ),
#                     by = c("scenario", "year", "region_code.IMPACT159"), .SDcols = cols])
MFADHolder <- unique(dt.main[, c("scenario","year", "region_code.IMPACT159", "MFAD") ])
inDT <- MFADHolder
outName <- "dt.MFAD"
cleanup(inDT, outName, fileloc("resultsDir"), "csv")

# qualifying nutrient balance score
#qualifying nutrients
# Water", "protein_g",  "totalfiber_g", "Calcium, Ca", "Iron, Fe", "Magnesium, Mg", "Phosphorus, P", "Potassium, K", "Zinc, Zn", "Vitamin C, total ascorbic acid", "Thiamin", "Riboflavin", "Niacin", "Vitamin B-6", "Folate, DFE", "Vitamin B-12", "Vitamin A, RAE", "Vitamin A, IU", "Vitamin E (alpha-tocopherol)", "Vitamin D (D2 + D3)", "Vitamin D", "Vitamin K (phylloquinone)", "Pantheoic Acid", "Linolenic Acid", "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)
nutrients.qual <- c("protein_g", "totalfiber_g", minerals, vitamins)
Nq <- length(nutrients.qual)
nutrients.qual.missing <- c("Vitamin A, IU", "Vitamin D IU",  "Pantheoic Acid", "Linolenic Acid", "a-Linolenic Acid", "Choline", "Copper (Cu)", "Fluorine (F)", "Selenium (Se)")
nutrients.disqual <- c("fat_g", "carbohydrate_g", "sodium_mg",  addedSugar, fattyAcids, "caffeine_mg", "cholesterol_mg")
Nd <- length(nutrients.disqual)
nutrients.other <- c("phytate_mg", "kcals.fat", "kcals.protein", "kcals.carbohydrate", "kcals.sugar", "kcals.ethanol")
#daily Maximal Reference Values (MRV) for that nutrient. The specific disqualifying nutrients
#and the MRVs used were: total fats (<35% of energy); saturated fats (<10% of dietary energy),
#cholesterol (<300 mg), trans fats (<1%) and sodium (<2300 mg), all as specified in the Dietary
#Guidelines for Americans [22], as well as total sugar (<25% of dietary energy, based on recommendations
#from the Institute of Medicine, USA [23]). Alcohol (ethyl) was not included as a disqualifying nutrient
dt.nutrients.qual <- dt.nutrients[,c(nutrients.qual), with = FALSE]
Ed <- 2000
dt.nutrients.qual[, (nutrients.qual) := get((nutrients.qual)) * kcalRef/energy_kcal]
dt.req.macro <- getNewestVersion("req.RDA.macro.percap")
dt.req.vits <- getNewestVersion("req.RDA.vits.percap")
dt.req.minrls <- getNewestVersion("req.RDA.minrls.percap")
dt.energy.ratios <- getNewestVersion("dt.energy.ratios", fileloc("resultsDir"))
dt.nutSum <- getNewestVersion("dt.nutrients.sum.all", fileloc("resultsDir"))
dt.kcalSum <- dt.nutSum[nutrient %in% c("kcals.carbohydrate", "kcals.ethanol", "kcals.fat",
                                        "kcals.protein") & year %in% yearList, ]
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




# old code, temporarily stored here ------
# ctyList <- sort(unique(dt.temp$region_code.IMPACT159))
# MFADHolder <- data.table(scenario = character(0), region_code.IMPACT159 = character(0), year = character(0), MFAD = numeric(0), RAOs_QE = numeric(0))
# for (k in yearList) {
#   for (l in scenList) {
#     for (n in ctyList) {
#       dt <- dt.main[ year == k & scenario == l & region_code.IMPACT159 == n,]
#       dt.foodqrat <- dt.foodqratio[ year == k & scenario == l & region_code.IMPACT159 == n,]
#       #      dt[,c("scenario", "region_code.IMPACT159", "year", "IMPACT_code") := NULL]
#       d <- as.data.table(rdist(dt[,5:length(dt)]))
#       data.table::setnames(d, old = names(d), new = unique(dt$IMPACT_code))
#       MFAD <- sum(d)/len.nutlist
#       RaosQE <- raoD(dt[,5:length(dt)])$total
#       newRow <- as.list(c(l,n,k,MFAD,RaosQE))
#       MFADHolder <- rbind(MFADHolder, newRow)
#     }
#   }
# }


#d <- vector(mode = "numeric", length = length(nutlist) * length(itemlist))

# for (i in 1:length(itemlist)) {
#   for (j in 1:length(itemlist)) {
#     for (k in 1:length(nutlist)) {
#     d[k] <- (dt.temp[IMPACT_code == itemlist[i],get(nutlist[k])] - dt.temp[IMPACT_code == itemlist[j],get(nutlist[k])])^2
#     }
#   }
# }

# dt.main[, MFAD := sum(rdist(dt.main[,!(c("scenario", "region_code.IMPACT159", "year", "IMPACT_code"))]))/len.nutlist,
# by = c("scenario", "year", "region_code.IMPACT159")]

#      d <- merge(d, dt.foodqratio, by = )
# for (i in 1:length(nutlist)) {
#   for (j in 1:length(nutlist)) {
#     d <- matrix(data = NA, nrow = length(itemlist), ncol = length(itemlist), dimnames = list(itemlist,itemlist))
#     d[i,j] <- sqrt(sum(dt[, (get(itemlist[i]) - get(itemlist[j]))^2]))
#   }
# }
#      d <- data.table::as.data.table(d)
#      browser()
#      MFAD <- sum(d)/length(nutlist)

#      newRow <- as.list(c(l,n,k,MFAD))
#      print(newRow)
#      MFADHolder <- rbind(MFADHolder, newRow)
#      print(MFADHolder)

# MFADHolder[,MFAD := as.numeric(MFAD)][,RAOs_QE := as.numeric(RAOs_QE)]
# RAOHolder <- MFADHolder[, c("scenario",  "region_code.IMPACT159", "year", "RAOs_QE")]
# MFADHolder <- MFADHolder[, c("scenario",  "region_code.IMPACT159", "year", "MFAD")]
# data.table::setnames(MFADHolder, old = "MFAD", new = "value")
# data.table::setnames(RAOHolder, old = "RAOs_QE", new = "value")
# inDT <- MFADHolder
# outName <- "dt.MFAD"
# cleanup(inDT, outName, fileloc("resultsDir"), "csv")
# inDT <- RAOHolder
# outName <- "dt.RaoQE"
# cleanup(inDT, outName, fileloc("resultsDir"), "csv")
#
# formula.wide <- paste("region_code.IMPACT159 + year ~ scenario")
# MFADHolder.wide <- data.table::dcast(
#   data = MFADHolder,
#   formula = formula.wide,
#   value.var = "value"
# )
