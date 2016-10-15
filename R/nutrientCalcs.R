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

#' @include nutrientModFunctions.R
#' @include workbookFunctions.R
#' @include nutrientCalcFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}

# Read in all data first and standardize variable names -----
# Read in IMPACT food data ----------
dt.IMPACTfood <- getNewestVersionIMPACT("dt.IMPACTfood")
# get the list of scenarios in the IMPACT data for use below
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
scenarioListIMPACT <- unique(dt.scenarioListIMPACT$scenario)

# read in nutrients data and optionally apply cooking retention values -----
switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
#dt.nutrients is per 100 gm of the raw product (ie before edible portion is applied)
dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

# calculate the share of per capita income spent on IMPACT commodities
budgetShare(dt.IMPACTfood)

keepListCol <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "FoodAvailability", "year")
dt.IMPACTfood <- dt.IMPACTfood[, keepListCol, with = FALSE]
# get rid of duplicate rows, caused by getting rid of GDP column
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood <- unique(dt.IMPACTfood)
# convert food availability from per year to per day
dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")][,FoodAvailability := NULL]

# reqsListPercap is a list of the requirements types. Each has a different set of nutrients. These are a subset
# of what are in the nutrients requirements tables from IOM. They are the nutrients common to
# both the IOM and nutrient content lookup spreadsheet

# the .percap data are for a representative consumer. They are generated in dataManagement.SSP

reqsListPercap <- keyVariable("reqsListPercap")
#reqPercap <- reqsListPercap[4] # just for testing!!! XXX
#scenarioListIMPACT <- "SSP2-MIROC" # just for testing!!! XXX
req <- "req.RDA.minrls.percap" # just for testing!!! XXX

generateResults <- function(req, dt.IMPACTfood, scenarioListIMPACT, dt.nutrients) {
  # use dt.food only in the function
  dt.food <- data.table::copy(dt.IMPACTfood)
  print(paste("loading dt.IMPACT.food for ", req, sep = ""))
  print(proc.time())
  dt.food <- dt.food[scenario %in% scenarioListIMPACT,]
  # read in the nutrient requirements data  for a representative consumer -----
  # Note that these are for SSP categories and thus vary by SSP category and year for each region
  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutListReq <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]
  # list of names for the product of daily availability by nutrient content for each commodity
  nutListReq.Q <-   paste(nutListReq, "Q", sep = ".")

  #nutListReq <- nutListReq[3:4] # Here just for testing. !!! be sure to comment out!!!XXX

  # dt.nutsReqPerCap has values for the 5 SSP scenarios (but no climate change or experiment effects.
  # To align with the IMPACT data we need to
  # - add the climate model and experiment names to the SSP scenario name (SSP1 - 5).
  # - add copies of dt.nutsReqPerCap for each of the climate models
  # This for loop adds copies of the nutrient requirements for each climate model used.
  #create an empty data table with the structure of dt.nutsReqPerCap
  dt.temp <- data.table::copy(dt.nutsReqPerCap[FALSE,])
  for (i in scenarioListIMPACT) {
    SSPName <- unlist(strsplit(i, "-"))[1] # get SSP abbrev
    climModel <- unlist(strsplit(i, "-"))[2] # get climate model abbrev
    experiment <- unlist(strsplit(i, "-"))[3] # get experiment abbrev
    # if (is.na(experiment)) {experiment <- "REF"}
    # print(experiment)
    # may need to add the RCP column later. Currently it's not included in the scenario name.
    temp.nuts <- data.table::copy(dt.nutsReqPerCap)
    temp.nuts <- temp.nuts[scenario == SSPName, ]
    # if (is.na(experiment)) {
    #   temp.nuts[,scenario := paste(scenario,climModel, sep = "-")]
    # } else {
    #   temp.nuts[,scenario := paste(scenario,climModel, experiment, sep = "-")]
    # }
    temp.nuts[,scenario := paste(SSPName, climModel, experiment, sep = "-")]
    dt.temp <- rbind(dt.temp, temp.nuts)
  }

  # keep just the nutrient requirements scenarios that are in the IMPACT data
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", nutListReq)
  dt.nutsReqPerCap <- dt.temp[,keepListCol, with = FALSE][scenario %in% scenarioListIMPACT,]

  # reduce calculations to just the nutrients in nutListReq
  #  and the nutrients in nutListReq plus those needed for iron and zinc bioavailability,
  # "phytate_mg", "vit_c_mg", "energy_kcal", "protein_g".

  keepListCol <- c("IMPACT_code","food_group_code","staple_code",nutListReq)
  if ("req.PR.zinc.percap" %in% req | "req.PR.iron.percap" %in% req) keepListCol <-
    c(keepListCol, "phytate_mg", "energy_kcal", "vit_c_mg", "protein_g")
  # if (req %in% "req.RDA.minrls.percap") keepListCol <- c(keepListCol, "phytate_mg", "energy_kcal", "vit_c_mg", "protein_g")
  # if (req %in% "req.EAR.percap")        keepListCol <- c(keepListCol, "phytate_mg", "energy_kcal")
  # if (req %in% "req.UL.minrls.percap")  keepListCol <- c(keepListCol, "phytate_mg", "energy_kcal", "vit_c_mg", "protein_g")

  # use the data table dt.nuts only in the function
  dt.nuts <- data.table::copy(dt.nutrients)
  dt.nuts <- dt.nuts[,(keepListCol), with = FALSE]
  # convert nutrients (in 100 grams of food) to nutrients per kg of food -----
  dt.nuts[, (nutListReq) := lapply(.SD, function(x) (x * 10)), .SDcols = nutListReq]
  if ("req.PR.zinc.percap" %in% req | "req.PR.iron.percap" %in% req) dt.nuts[, phytate_mg := phytate_mg * 10]
  #combine the food availability info with the nutrients for each of the IMPACT commodities
  data.table::setkey(dt.nuts, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  dt.foodnNuts <-  merge(dt.food, dt.nuts, by = "IMPACT_code", all = TRUE)

  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  dt.food.agg <- data.table::copy(dt.foodnNuts[, (nutListReq.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq])
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  # adjust iron and zinc in the pr reqs for bioavailability
  # if ("iron_mg" %in% nutListReq | "zinc_mg" %in% nutListReq) {
  if ("req.PR.zinc.percap" %in% req | "req.PR.iron.percap" %in% req) {
    # now add phytate, vit c, energy_kcal, and protein_g to the list of nutrients if the requirements include iron and zinc
    nutListReq.bio <- c(nutListReq, "phytate_mg", "vit_c_mg", "energy_kcal", "protein_g")
    #    print(nutListReq.bio)
    #    if (req %in% "req.EAR.percap") nutListReq.bio <- c(nutListReq, "phytate_mg","energy_kcal")
    nutListReq.Q.bio <-   paste(nutListReq.bio, "Q", sep = ".")
    dt.bioavail <- data.table::copy(dt.foodnNuts)
    print(names(dt.bioavail))
    dt.bioavail <- dt.bioavail[, (nutListReq.Q.bio) := lapply(.SD, function(x)
      (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq.bio]
    # leadingCols <- c("scenario", "region_code.IMPACT159",
    #                  "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
    # laggingCols <- names(dt.bioavail)[!names(dt.bioavail) %in% leadingCols]
    # data.table::setcolorder(dt.bioavail, c(leadingCols, laggingCols))

    # do operations over the whole data table; initialize variables

  }
  # work on iron
  if ("req.PR.iron.percap" %in% req) {
    dt.bioavail[, `:=`(kcal.avail = energy_kcal.Q,
                       kcal.cereals_legumes = 0,
                       vit_c.avail_mg = vit_c_mg.Q,
                       iron.raw_mg = iron_mg.Q,
                       iron.heme_mg = 0,
                       iron.nonheme_mg = 0,
                       protein.animal.avail_g = 0,
                       stimsFactor = 0
    )]
    # Heme iron = sum of iron from meats, poultry and fish x 0.40
    # bioavailability of heme iron = Heme iron * 25%
    #  Nonheme iron = sum of all remaining iron (including iron from MPF x 0.60)
    # get iron and protein from fish and meats
    fishNmeats <- c("fish", "meats")
    dt.bioavail[food_group_code %in% fishNmeats, `:=`(
      iron.heme_mg = foodAvailpDay * iron_mg * 0.4 * 0.25,
      iron.nonheme_mg = foodAvailpDay * iron_mg * 0.6,
      protein.animal.avail_g = foodAvailpDay * protein_g)]

    # get ironfrom items other than fish and meats (! means not in)
    dt.bioavail[!food_group_code %in% fishNmeats, `:=`(
      iron.nonheme_mg = foodAvailpDay * iron_mg)]

    # get kcals from cereals and legumes
    dt.bioavail[food_group_code %in% c("cereals", "legumes"), `:=`(
      kcal.cereals_legumes = foodAvailpDay * energy_kcal)]

    # converted below Tea factor = [100% - {((tea intake (kg/d) * 1L/0.00792 kg) + (coffee intake (kg/d) * 1L/0.0442 kg * 1/1.5)) x 1/0.6L * 60%}]
    # dt.food.agg[IMPACT_code == "ccafe", stimsFactor := 100 - (foodAvailpDay * (1/0.00792) *          (1/0.6) * 60)]
    # dt.food.agg[IMPACT_code == "cteas", stimsFactor := 100 - (foodAvailpDay * (1/0.0442) * (1/1.5) * (1/0.6) * 60)]

    dt.bioavail[IMPACT_code == "cteas", stimsFactor := foodAvailpDay * (1/0.00792)]
    dt.bioavail[IMPACT_code == "ccafs", stimsFactor := foodAvailpDay * (1/0.0442) * (1/1.5)]

    keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "iron.raw_mg",
                     "iron.heme_mg",  "iron.nonheme_mg", "kcal.avail", "kcal.cereals_legumes", "vit_c.avail_mg", "protein.animal.avail_g", "stimsFactor")
    dt.bioavail <- dt.bioavail[, (keepListCol), with = FALSE]
    # data.table::setkeyv(dt.food.agg, c("scenario", "region_code.IMPACT159", "year"))
    dt.bioavail[,`:=`(
      sum.iron.raw_mg              = sum(iron.raw_mg),
      sum.iron.heme_mg             = sum(iron.heme_mg),
      sum.iron.nonheme_mg          = sum(iron.nonheme_mg),
      sum.kcal.avail               = sum(kcal.avail),
      sum.kcal.cereals_legumes     = sum(kcal.cereals_legumes),
      sum.protein.animal.avail_g   = sum(protein.animal.avail_g),
      sum.stimsFactor              = (100 - sum(stimsFactor) * (1/0.6) * 60),
      sum.vit_c.avail_mg           = sum(vit_c.avail_mg)),
      by = .(scenario, region_code.IMPACT159, year)]

    dt.bioavail[sum.stimsFactor < 40, sum.stimsFactor := 40]
    deleteListCol <- c("IMPACT_code", "iron.raw_mg",  "iron.heme_mg", "iron.nonheme_mg", "kcal.avail", "kcal.cereals_legumes",
                       "protein.animal.avail_g", "stimsFactor", "vit_c.avail_mg")
    dt.bioavail[, (deleteListCol) := NULL]
    dt.bioavail <- unique(dt.bioavail)

    # adjust for interactions among vitamin c and protein
    #    Note: for protein_g_per_1000kcal > 27, nonhemeBioavail is 15
    #    for protein_g_per_1000kcal 9- 27, nonhemeBioavail is 15, unless vit_c__mg_per_1000kcal >35

    dt.bioavail[,`:=`(
      vit_c__mg_per_1000kcal = 1000 * sum.vit_c.avail_mg/sum.kcal.avail,
      protein_g_per_1000kcal = 1000 * sum.protein.animal.avail_g/sum.kcal.avail)
      ]
    dt.bioavail[,nonhemeBioavail := 15] # starting value; now adjust down
    dt.bioavail[protein_g_per_1000kcal < 9 & vit_c__mg_per_1000kcal <  35,
                nonhemeBioavail := 5]
    dt.bioavail[protein_g_per_1000kcal < 9 & vit_c__mg_per_1000kcal >= 35 & vit_c__mg_per_1000kcal <= 105,
                nonhemeBioavail := 10]
    dt.bioavail[protein_g_per_1000kcal >= 9 & protein_g_per_1000kcal <= 27 & vit_c__mg_per_1000kcal < 35,
                nonhemeBioavail := 10]

    dt.bioavail[, iron_mg := sum.iron.heme_mg + (sum.iron.nonheme_mg*(nonhemeBioavail/100) * (sum.stimsFactor/100))]
    dt.bioavail[, delta_iron_mg := iron_mg - sum.iron.raw_mg]
    dt.bioavail[, bioavailability.iron := 100 * iron_mg/sum.iron.raw_mg]

    inDT <- dt.bioavail
    outName <- "dt.bioavail.iron"
    cleanup(inDT, outName, fileloc("resultsDir"), "csv")
    keepListCol <- c("scenario","region_code.IMPACT159", "year", "bioavailability.iron")
    dt.bioavail <- dt.bioavail[, (keepListCol), with = FALSE]
    # adjust iron in dt.food.agg
    temp <- merge(dt.food.agg, dt.bioavail, by = c("scenario", "region_code.IMPACT159", "year"))
    dt.food.agg <- temp[,iron_mg.Q := iron_mg.Q * bioavailability.iron/100][,c("bioavailability.iron") := NULL]
  }

  # now do zinc
  #source: 1. K. M. Hambidge, L. V. Miller, J. E. Westcott, X. Sheng, N. F. Krebs,
  # Zinc bioavailability and homeostasis. Am. J. Clin. Nutr. 91, 1478S–1483S (2010).
  if ("req.PR.zinc.percap" %in% req) {
    dt.bioavail[, `:=`(zinc.raw_mg = zinc_mg.Q,
                       phytate_mg = phytate_mg.Q
    )]
    keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "zinc.raw_mg", "phytate_mg")
    dt.bioavail <- dt.bioavail[, (keepListCol), with = FALSE]
    # data.table::setkeyv(dt.food.agg, c("scenario", "region_code.IMPACT159", "year"))
    dt.bioavail[,`:=`(
      sum.zinc.raw_mg              = sum(zinc.raw_mg),
      sum.phytate_mg               = sum(phytate_mg)),
      by = .(scenario, region_code.IMPACT159, year)]
    deleteListCol <- c("IMPACT_code", "zinc.raw_mg", "phytate_mg")
    dt.bioavail[, (deleteListCol) := NULL]
    dt.bioavail <- unique(dt.bioavail)
    # This is based on equation 11 of 1. L. V. Miller, N. F. Krebs, K. M. Hambidge,
    # A mathematical model of zinc absorption in humans as a function of dietary zinc and phytate.
    # J. Nutr. 137, 135–41 (2007).
    # Three parameters have been updated in a 2010 paper.
    zincAtomMass  <- 65.38
    phytMolecMass <- 660
    amax2010 = .091; kr2010 = .033; kp2010 = .68
    dt.bioavail[, molarRatio := (sum.phytate_mg/phytMolecMass) / (sum.zinc.raw_mg/zincAtomMass)]
    dt.bioavail[, millernum2010 := amax2010 + (sum.zinc.raw_mg/zincAtomMass) + kr2010 * (1 + (sum.phytate_mg/phytMolecMass)/kp2010)]
    dt.bioavail[, zinc_mg := zincAtomMass * 0.5 * (millernum2010 - sqrt(millernum2010^2 - 4 * amax2010 * (sum.zinc.raw_mg/zincAtomMass)))]
    dt.bioavail[, bioavailability.zinc := 100 * zinc_mg/sum.zinc.raw_mg]

    inDT <- dt.bioavail
    outName <- "dt.bioavail.zinc"
    cleanup(inDT, outName, fileloc("resultsDir"), "csv")
    keepListCol <- c("scenario","region_code.IMPACT159", "year", "bioavailability.zinc")
    dt.bioavail <- dt.bioavail[, (keepListCol), with = FALSE]
    # adjust zinc in dt.food.agg
    temp <- merge(dt.food.agg, dt.bioavail, by = c("scenario", "region_code.IMPACT159", "year"))
    dt.food.agg <- temp[,zinc_mg.Q := zinc_mg.Q * bioavailability.zinc/100][,c("bioavailability.zinc") := NULL]

  }

  # create name lists for use in operations below

  # the total daily availability of each nutrient
  nutListReq.sum.all <- paste(nutListReq, "sum.all", sep = ".")
  # the ratio of daily availability of each nutrient from each commodity to the total availability
  nutListReq.ratio.all <- paste(nutListReq, "ratio.all", sep = ".")
  # the ratio of daily availability of each nutrient to the nutrient requirement
  nutListReq.req.ratio.all <- paste(nutListReq, "req.ratio.all", sep = ".")

  # the total daily consumption of each staple
  nutListReq.sum.staples <- paste(nutListReq, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutListReq.ratio.staples <- paste(nutListReq, "ratio.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple by the nutrient requirement
  nutListReq.req.ratio.staples <- paste(nutListReq, "req.ratio.staple", sep = ".")

  # the total daily consumption of each food group
  nutListReq.sum.foodGroup <- paste(nutListReq, "sum.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup to the total consumption
  nutListReq.req.ratio.foodGroup <- paste(nutListReq, "req.ratio.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup by the nutrient requirement
  nutListReq.ratio.foodGroup <- paste(nutListReq, "ratio.foodGroup", sep = ".")
  # calculate sums and ratios, for all food items, by staples, and by food groups -----
  # these keys are used to determine what is summed over or ratio made with
  allKey <-       c("scenario", "region_code.IMPACT159", "year")
  sumKey <-       c("scenario", "region_code.IMPACT159", "year","IMPACT_code")
  stapleKey <-    c("scenario", "region_code.IMPACT159", "year", "staple_code")
  foodGroupKey <- c("scenario", "region_code.IMPACT159", "year", "food_group_code")

  # # first sum
  # ## individual nutrients from all commodities
  data.table::setkeyv(dt.food.agg, allKey)
  dt.food.agg <- dt.food.agg[, (nutListReq.sum.all) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg))]
  # print(paste("summing nutrient for all commodities for ", req, sep = ""))
  # print(proc.time())

  ## individual nutrients by staples
  data.table::setkeyv(dt.food.agg,stapleKey)
  dt.food.agg <- dt.food.agg[, (nutListReq.sum.staples) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg))]

  print(paste("summing by food group ", req, sep = ""))
  print(proc.time())

  ## individual nutrients by food group -----
  data.table::setkeyv(dt.food.agg,foodGroupKey)
  dt.food.agg <- dt.food.agg[, (nutListReq.sum.foodGroup) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                             by = eval(data.table::key(dt.food.agg))]
  data.table::setkey(dt.food.agg, NULL)
  dt.food.agg <- unique(dt.food.agg)
  # now calculate ratios of nutrient by group to total consumption of the nutrient
  # nutrient from each food item to the total
  #dt.food.ratio <- data.table::copy(dt.all.sum[,(nutListReq.ratio) := dt.all.sum[[nutListReq.Q]] / dt.all.sum[[nutListReq.sum]]])

  print(paste("calculating nutrient share ratios ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each food item to the total
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.ratio.all[k] := get(nutListReq.Q[k]) / get(nutListReq.sum.all[k])]
  }
  #  ratio of nutrient from each staple item to the total
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.ratio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.sum.all[k])]
  }
  #  ratio of nutrient from each food group item to the total
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.ratio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.sum.all[k])]
  }

  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  # now do ratios with nutrient requirements
  print(paste("calculating nutrient requirement ratios for ", req, sep = ""))
  print(proc.time())

  # change nutrient names in dt.nutsReqPerCap so they differ from those in nutListReq
  nutListReq.Req <- paste(nutListReq,"req", sep = ".")
  data.table::setnames(dt.nutsReqPerCap, old = nutListReq, new = nutListReq.Req)
  data.table::setkeyv(dt.food.agg,allKey)
  data.table::setkeyv(dt.nutsReqPerCap,allKey)
  # temp <- merge(dt.food.agg,dt.nutsReqPerCap, by = c(allKey,nutListReq), all.x = TRUE)
  dt.food.agg <- merge(dt.food.agg,dt.nutsReqPerCap, by = c(allKey), all.x = TRUE)
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  #  ratio of nutrient from each food item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.req.ratio.all[k] := get(nutListReq.Q[k]) / get(nutListReq.Req[k])]
  }

  print(paste("finished with ratio for each food item ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each staple item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.req.ratio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.Req[k])]
  }
  print(paste("finished with ratio for the staple/non staple categories ", req, sep = ""))
  print(proc.time())

  #  ratio of nutrient from each food group item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.req.ratio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.Req[k])]
  }
  print(paste("finished with requirement ratio for the food group categories ", req, sep = ""))
  print(proc.time())
  inDT <- dt.food.agg

  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)
  outName <- paste("food.agg.",reqShortName,sep = "")
  cleanup(inDT, outName,fileloc("resultsDir"), "csv")
}
# end of generateResults function

generateSum <- function(dt.IMPACTfood, scenarioListIMPACT) {
  print("Creating sum for all nutrients")
  #print(proc.time())
  dt.food <- data.table::copy(dt.IMPACTfood)
  dt.food <- dt.food[scenario %in% scenarioListIMPACT,]

  # read in nutrients data and optionally apply cooking retention values -----
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  dt.nutrients <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

  nutListReq <- names(dt.nutrients)[2:(ncol(dt.nutrients))]
  deleteList <- c("food_group_code", "staple_code")
  nutListReq <- nutListReq[!nutListReq %in% deleteList]
  nutListReq.Q <-   paste(nutListReq, "Q", sep = ".")
  #  nutListReq.sum.all <- paste(nutListReq, "sum.all", sep = ".")
  nutListReq.sum.all <- paste(nutListReq, sep = ".")
  # nutListReq.sum.staple <- paste(nutListReq, "sum.staple", sep = ".")
  nutListReq.sum.staple <- paste(nutListReq, sep = ".")

  print("Multiplying dt.nutrients by 10 so in same units (kgs) as IMPACT commodities")
  dt.nutrients[, (nutListReq) := lapply(.SD, function(x) (x * 10)), .SDcols = nutListReq]
  data.table::setkey(dt.nutrients, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  dt.foodnNuts <-  merge(dt.food, dt.nutrients, by = "IMPACT_code", all = TRUE)
  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  dt.food.agg <- dt.foodnNuts[, (nutListReq.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq][,(nutListReq) := NULL]

  print("summing nutrient for all commodities and staples")
  allKey <-    c("scenario", "region_code.IMPACT159", "year")
  stapleKey <- c(allKey, "staple_code")

  # do sum of all first
  dt.food.agg.all <- data.table::copy(dt.food.agg)
  data.table::setkeyv(dt.food.agg.all, allKey)
  dt.food.agg.all <- dt.food.agg.all[, (nutListReq.sum.all) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                                     by = eval(data.table::key(dt.food.agg.all))][,(nutListReq.Q) := NULL]
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  dt.food.agg.all[,(deleteListCol) := NULL]
  dt.nut.wide <- unique(dt.food.agg.all)
  dt.nut.long <- data.table::melt(dt.nut.wide,
                                  id.vars = c("scenario","region_code.IMPACT159", "year"),
                                  variable.name = "nutrient",
                                  measure.vars = nutListReq.sum.all,
                                  value.name = "value",
                                  variable.factor = FALSE)

  inDT <- unique(dt.nut.long)

  outName <- "dt.nutrients.sum.all"
  cleanup(inDT,outName, fileloc("resultsDir"))

  # do staples next
  dt.food.agg.staples <- data.table::copy(dt.food.agg)
  data.table::setkeyv(dt.food.agg.staples, stapleKey)
  dt.food.agg.staples <- dt.food.agg.staples[, (nutListReq.sum.staple) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                                             by = eval(data.table::key(dt.food.agg.staples))][,(nutListReq.Q) := NULL]
  print(proc.time())
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "food_group_code")
  dt.food.agg.staples[,(deleteListCol) := NULL]
  dt.nut.sum.staple.wide <- unique(dt.food.agg.staples)
  dt.nut.sum.staple.long <- data.table::melt(dt.nut.sum.staple.wide,
                                             id.vars = c("scenario","region_code.IMPACT159", "staple_code","year"),
                                             variable.name = "nutrient",
                                             measure.vars = nutListReq.sum.staple,
                                             value.name = "value",
                                             variable.factor = FALSE)

  inDT <- dt.nut.sum.staple.long
  outName <- "dt.nutrients.sum.staples"

  # Create non staple share of nutrient dt
  shareFormula <- "scenario + region_code.IMPACT159 + nutrient + year ~ staple_code"
  dt.nut.nonstaple.share.wide <- data.table::dcast(data = dt.nut.sum.staple.long,
                                                   formula = shareFormula,
                                                   value.var = "value")
  dt.nut.nonstaple.share.wide <- dt.nut.nonstaple.share.wide[, value := 100 * nonstaple/(nonstaple + staple) ]
  dt.nut.nonstaple.share.long <- dt.nut.nonstaple.share.wide[,c("nonstaple", "staple") := NULL]
  inDT <- dt.nut.nonstaple.share.long
  outName <- "dt.nutrients.nonstapleShare"
  cleanup(inDT, outName, fileloc("resultsDir"))

}
# run the generateResults script -----
for (i in 1:length(reqsListPercap)) {
  generateResults(reqsListPercap[i],dt.IMPACTfood,scenarioListIMPACT, dt.nutrients)
  print(paste("Done with ", reqsListPercap[i], ". ", length(reqsListPercap) - i," sets of requirements to go.", sep = ""))
}

# run the generateSum script -----
generateSum(dt.IMPACTfood, scenarioListIMPACT)
