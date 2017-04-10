#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2016 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

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
#dt.nutrients.adj is per kg of food
dt.nutrients.adj <- cookingRetFishCorrect(switch.useCookingRetnValues, switch.fixFish)

# calculate the share of per capita income spent on IMPACT commodities
budgetShareNpriceGrowth(dt.IMPACTfood)

keepListCol <- c("scenario", "IMPACT_code", "region_code.IMPACT159", "FoodAvailability", "foodAvailpDay", "year")
dt.IMPACTfood <- dt.IMPACTfood[, keepListCol, with = FALSE]

# get rid of duplicate rows, caused by getting rid of GDP column
data.table::setkey(dt.IMPACTfood)
dt.IMPACTfood <- unique(dt.IMPACTfood)

# convert food availability from per year to per day
# dt.IMPACTfood[, foodAvailpDay := FoodAvailability / keyVariable("DinY")]
dt.IMPACTfood[,FoodAvailability := NULL]

#calculate kcal values -----
keepListCol <- c("IMPACT_code", "energy_kcal", "kcals.fat_g","kcals.carbohydrate_g", "kcals.protein_g",
                 "kcals.ethanol_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g")
dt.nutrients.kcals <- dt.nutrients.adj[, (keepListCol), with = FALSE]
dt.nutrients.kcals <- merge(dt.IMPACTfood, dt.nutrients.kcals, by = "IMPACT_code")
dt.nutrients.kcals[,kcalsPerCommod := foodAvailpDay * energy_kcal]
# add the kcals per day from the sources of kcals
dt.nutrients.kcals[, `:=`(
  kcals.fat = foodAvailpDay * kcals.fat_g,
  kcals.carbohydrate = foodAvailpDay * kcals.carbohydrate_g,
  kcals.protein = foodAvailpDay * kcals.protein_g,
  kcals.ethanol = foodAvailpDay * kcals.ethanol_g,
  kcals.sugar = foodAvailpDay * kcals.sugar_g,
  kcals.ft_acds_tot_sat = foodAvailpDay * kcals.ft_acds_tot_sat_g
)]
dt.nutrients.kcals[, `:=`(
  kcalsPerDay.tot = sum(kcalsPerCommod),
  kcalsPerDay.carbohydrate = sum(kcals.carbohydrate),
  kcalsPerDay.fat = sum(kcals.fat),
  kcalsPerDay.protein = sum(kcals.protein),
  kcalsPerDay.ethanol = sum(kcals.ethanol),
  kcalsPerDay.sugar = sum(kcals.sugar),
  kcalsPerDay.ft_acds_tot_sat = sum(kcals.ft_acds_tot_sat)),
  by = c("scenario",  "year", "region_code.IMPACT159"
  )][,
     kcalsPerDay.other := kcalsPerDay.tot - (kcalsPerDay.carbohydrate + kcalsPerDay.fat + kcalsPerDay.protein)
     ]
DT <- dt.nutrients.kcals
outName <- "dt.nutrients.kcals"
cleanup(DT, outName, fileloc("resultsDir"))

# reqsListPercap is a list of the requirements types. Each has a different set of nutrients. These are a subset
# of what are in the nutrients requirements tables from IOM. They are the nutrients common to
# both the IOM and nutrient content lookup spreadsheet. Some are in physical units (eg. gms; others, especially AMDR are in percent of total energy)

# the .percap data are for a representative consumer. They are generated in dataManagement.SSP

reqsListPercap <- keyVariable("reqsListPercap")
#reqPercap <- reqsListPercap[4] # just for testing!!! XXX
#scenarioListIMPACT <- "SSP2-MIROC" # just for testing!!! XXX
req <- "req.RDA.minrls_percap" # just for testing!!! XXX

generateResults.dataPrep <- function(req, dt.IMPACTfood, scenarioListIMPACT, dt.nutrients.adj) {
  # use dt.food only in the function
  dt.food <- data.table::copy(dt.IMPACTfood)
  print(paste("loading dt.IMPACT.food for ", req, sep = ""))

  # print(proc.time())
  dt.food <- dt.food[scenario %in% scenarioListIMPACT,]

  # read in nutrient requirements data for a representative consumer -----
  # Note that these are for SSP categories and thus vary by SSP category and year for each region
  dt.nutsReqPerCap <- getNewestVersion(req)

  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutListReq <- names(dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]
  # list of names for the product of daily availability by nutrient content for each commodity
  nutListReq.Q <- paste(nutListReq, "Q", sep = ".") # note that these are in percent of total daily kcals for AMDRs

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
    # temp.nuts[,scenario := paste(scenario,climModel, sep = "-")]
    # } else {
    # temp.nuts[,scenario := paste(scenario,climModel, experiment, sep = "-")]
    # }
    temp.nuts[,scenario := paste(SSPName, climModel, experiment, sep = "-")]
    dt.temp <- rbind(dt.temp, temp.nuts)
  }

  # keep just the nutrient requirements scenarios that are in the IMPACT data
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", nutListReq)
  dt.nutsReqPerCap <- dt.temp[,keepListCol, with = FALSE][scenario %in% scenarioListIMPACT,]

  # reduce calculations to just the nutrients in nutListReq
  # and the nutrients in nutListReq plus those needed for iron and zinc bioavailability,
  # "phytate_mg", "vit_c_mg", "energy_kcal", "protein_g".

  keepListCol <- c("IMPACT_code","food_group_code","staple_code",nutListReq)

  # keep extra columns around for iron and zinc bioavailability calculations
  if ("req.RDA.minrls_percap" %in% req) keepListCol <-
    c(keepListCol, "phytate_mg", "energy_kcal", "vit_c_mg", "protein_g")

  # use the data table dt.nuts only in the function
  dt.nuts <- data.table::copy(dt.nutrients.adj)
  dt.nuts <- dt.nuts[,(keepListCol), with = FALSE]
  #combine the food availability info with the nutrients for each of the IMPACT commodities
  data.table::setkey(dt.nuts, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  dt.foodnNuts <- merge(dt.food, dt.nuts, by = "IMPACT_code", all = TRUE)

  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  #dt.food.agg.[req] is what eventually gets stored and used later
  dt.food.agg <- data.table::copy(dt.foodnNuts[, (nutListReq.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq])
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  # adjust iron and zinc  bioavailability amounts to food.agg.minrls
  if ("req.RDA.minrls_percap" %in% req | "req.EAR.percap"  %in% req ) {
    # add phytate(for zinc calculations), vit c, energy_kcal, and protein_g (for iron calculations) to the list of nutrients
    nutListReq.bio <- c(nutListReq, "phytate_mg", "vit_c_mg", "energy_kcal", "protein_g")
    # print(nutListReq.bio)
    # if (req %in% "req.EAR.percap") nutListReq.bio <- c(nutListReq, "phytate_mg","energy_kcal")
    nutListReq.Q.bio <- paste(nutListReq.bio, "Q", sep = ".")
    dt.bioavail <- data.table::copy(dt.foodnNuts)
    #create dt.bioavail and use it instead of food.agg. It is merged back into food agg with just iron or zinc
    dt.bioavail <- dt.bioavail[, (nutListReq.Q.bio) := lapply(.SD, function(x)
      (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq.bio]

    # iron bioavailability -----
    dt.bioavail.iron <- data.table::copy(dt.bioavail)
    dt.bioavail.iron[, `:=`(kcal.avail = energy_kcal.Q,
                            kcal.cereals_legumes = 0,
                            vit_c.avail_mg = vit_c_mg.Q,
                            iron.raw_mg = iron_mg.Q,
                            iron.heme_mg = 0,
                            iron.nonheme_mg = 0,
                            protein.animal.avail_g = 0,
                            stimsFactor = 0
    )]
    # Heme iron = sum of iron from meats, poultry and fish x 0.40
    hemeIronshare <- 0.4
    # bioavailability of heme iron = Heme iron * 25%
    hemeIronBioavail <- 0.25
    # Nonheme iron = sum of all remaining iron (including iron from MPF x 0.60)
    nonhemIronShare <- 0.60
    # get iron and protein from fish and meats
    fishNmeats <- c("fish", "meats")
    dt.bioavail.iron[food_group_code %in% fishNmeats, `:=`(
      iron.heme_mg = foodAvailpDay * iron_mg * hemeIronshare * hemeIronBioavail,
      iron.nonheme_mg = foodAvailpDay * iron_mg * nonhemIronShare,
      protein.animal.avail_g = foodAvailpDay * protein_g)]

    # get ironfrom items other than fish and meats (! means not in)
    dt.bioavail.iron[!food_group_code %in% fishNmeats, `:=`(
      iron.nonheme_mg = foodAvailpDay * iron_mg)]

    # get kcals from cereals and legumes
    dt.bioavail.iron[food_group_code %in% c("cereals", "legumes"), `:=`(
      kcal.cereals_legumes = foodAvailpDay * energy_kcal)]

    # converted below Tea factor = [100% - {((tea intake (kg/d) * 1L/0.00792 kg) + (coffee intake (kg/d) * 1L/0.0442 kg * 1/1.5)) x 1/0.6L * 60%}]
    # dt.food.agg[IMPACT_code == "ccafe", stimsFactor := 100 - (foodAvailpDay * (1/0.00792) * (1/0.6) * 60)]
    # dt.food.agg[IMPACT_code == "cteas", stimsFactor := 100 - (foodAvailpDay * (1/0.0442) * (1/1.5) * (1/0.6) * 60)]
    teaFactor <- 0.00792
    coffeeFactor <- 0.0442
    dt.bioavail.iron[IMPACT_code == "cteas", stimsFactor := foodAvailpDay * (1/teaFactor)]
    dt.bioavail.iron[IMPACT_code == "ccafs", stimsFactor := foodAvailpDay * (1/coffeeFactor) * (1/1.5)]

    keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "iron.raw_mg",
                     "iron.heme_mg", "iron.nonheme_mg", "kcal.avail", "kcal.cereals_legumes", "vit_c.avail_mg", "protein.animal.avail_g", "stimsFactor")
    dt.bioavail.iron <- dt.bioavail.iron[, (keepListCol), with = FALSE]
    # data.table::setkeyv(dt.food.agg, c("scenario", "region_code.IMPACT159", "year"))
    dt.bioavail.iron[,`:=`(
      sum.iron.raw_mg = sum(iron.raw_mg),
      sum.iron.heme_mg = sum(iron.heme_mg),
      sum.iron.nonheme_mg = sum(iron.nonheme_mg),
      sum.kcal.avail = sum(kcal.avail),
      sum.kcal.cereals_legumes = sum(kcal.cereals_legumes),
      sum.protein.animal.avail_g = sum(protein.animal.avail_g),
      sum.stimsFactor = (100 - sum(stimsFactor) * (1/0.6) * 60),
      sum.vit_c.avail_mg = sum(vit_c.avail_mg)),
      by = .(scenario, region_code.IMPACT159, year)]

    dt.bioavail.iron[sum.stimsFactor < 40, sum.stimsFactor := 40]
    deleteListCol <- c("IMPACT_code", "iron.raw_mg", "iron.heme_mg", "iron.nonheme_mg", "kcal.avail", "kcal.cereals_legumes",
                       "protein.animal.avail_g", "stimsFactor", "vit_c.avail_mg")
    dt.bioavail.iron[, (deleteListCol) := NULL]
    dt.bioavail.iron <- unique(dt.bioavail.iron)

    # adjust for interactions among vitamin c and protein
    # Note: for protein_g_per_1000kcal > 27, nonhemeBioavail is 15
    # for protein_g_per_1000kcal 9- 27, nonhemeBioavail is 15, unless vit_c__mg_per_1000kcal >35

    dt.bioavail.iron[,`:=`(
      vit_c__mg_per_1000kcal = 1000 * sum.vit_c.avail_mg/sum.kcal.avail,
      protein_g_per_1000kcal = 1000 * sum.protein.animal.avail_g/sum.kcal.avail)
      ]
    dt.bioavail.iron[,nonhemeBioavail := 15] # starting value; now adjust down
    dt.bioavail.iron[protein_g_per_1000kcal < 9 & vit_c__mg_per_1000kcal < 35,
                     nonhemeBioavail := 5]
    dt.bioavail.iron[protein_g_per_1000kcal < 9 & vit_c__mg_per_1000kcal >= 35 & vit_c__mg_per_1000kcal <= 105,
                     nonhemeBioavail := 10]
    dt.bioavail.iron[protein_g_per_1000kcal >= 9 & protein_g_per_1000kcal <= 27 & vit_c__mg_per_1000kcal < 35,
                     nonhemeBioavail := 10]

    dt.bioavail.iron[, iron_mg := sum.iron.heme_mg + (sum.iron.nonheme_mg*(nonhemeBioavail/100) * (sum.stimsFactor/100))]
    #dt.bioavail.iron[, delta_iron_mg := iron_mg - sum.iron.raw_mg]
    dt.bioavail.iron[, bioavailability.iron := 100 * iron_mg/sum.iron.raw_mg]
    dt.bioavail.iron <- unique(dt.bioavail.iron)
    inDT <- dt.bioavail.iron
    outName <- "dt.bioavail_iron"
    cleanup(inDT, outName, fileloc("resultsDir"), "csv")
    keepListCol <- c("scenario","region_code.IMPACT159", "year", "bioavailability.iron")
    dt.bioavail.iron <- dt.bioavail.iron[, (keepListCol), with = FALSE]
    # adjust iron in dt.food.agg
    temp <- merge(dt.food.agg, dt.bioavail.iron, by = c("scenario", "region_code.IMPACT159", "year"))
    dt.food.agg <- temp[,iron_mg.Q := iron_mg.Q * bioavailability.iron/100][,c("bioavailability.iron") := NULL]

    # zinc bioavailability -----
    dt.bioavail_zinc <- data.table::copy(dt.bioavail)
    dt.bioavail_zinc[, `:=`(zinc.raw_mg = zinc_mg.Q,
                            phytate_mg = phytate_mg.Q
    )]
    keepListCol <- c("IMPACT_code", "scenario", "region_code.IMPACT159", "year", "zinc.raw_mg", "phytate_mg")
    dt.bioavail_zinc <-  dt.bioavail_zinc[, (keepListCol), with = FALSE]
    dt.bioavail_zinc[,`:=`(
      sum.zinc.raw_mg = sum(zinc.raw_mg),
      sum.phytate_mg = sum(phytate_mg)),
      by = .(scenario, region_code.IMPACT159, year)]
    #   deleteListCol <- c("IMPACT_code", "zinc.raw_mg", "phytate_mg")
    deleteListCol <- c("IMPACT_code", "phytate_mg")
    dt.bioavail_zinc[, (deleteListCol) := NULL]
    dt.bioavail_zinc <- unique( dt.bioavail_zinc)
    # This is based on equation 11 of 1. L. V. Miller, N. F. Krebs, K. M. Hambidge,
    # A mathematical model of zinc absorption in humans as a function of dietary zinc and phytate.
    # J. Nutr. 137, 135–41 (2007).
    # amax - maximal absorption of zinc
    # taz - total daily absorbed zinc
    # tdz - total daily dietary zinc
    # tdp - total daily dietary phytate
    # units of above are millimoles per day
    # - kp and kr are the equilibrium dissociation constants of zinc-phytate and zinc-receptor binding, respectively
    zincAtomMass <- 65.38
    phytMolecMass <- 660

    # Three parameters have been updated in a 2010 paper.
    amax2010 = .091; kr2010 = .033; kp2010 = .68 # updated parameters from
    # Reference : Hambidge KM, Miller LV, Westcott JE, Sheng X, Krebs NF. Zinc bioavailability and homeostasis.
    # Am J Clin Nutr. 2010;91:1478S-83S.

    # dt.bioavail[, molarRatio := (sum.phytate_mg/phytMolecMass) / (sum.zinc.raw_mg/zincAtomMass)]
    # millernum2010 -  subset of equation 11 in Miller 2007 that is used twice
    dt.bioavail_zinc[, tdp := sum.phytate_mg/phytMolecMass][, tdz := sum.zinc.raw_mg/zincAtomMass]
    dt.bioavail_zinc[, millernum2010 := amax2010 + tdz + kr2010 * (1 + (tdp / kp2010))]
    dt.bioavail_zinc[, taz := 0.5 * (millernum2010 - sqrt(millernum2010^2 - 4 * amax2010 * tdz))]
    dt.bioavail_zinc[, zinc_mg := taz * zincAtomMass]
    dt.bioavail_zinc[, bioavailability.zinc := 100 * zinc_mg/sum.zinc.raw_mg]
    dt.bioavail_zinc <- unique( dt.bioavail_zinc)
    inDT <-  dt.bioavail_zinc
    outName <- "dt.bioavail_zinc"
    cleanup(inDT, outName, fileloc("resultsDir"), "csv")
    #   keepListCol <- c("scenario","region_code.IMPACT159", "year", "bioavailability.zinc")
    keepListCol <- c("scenario","region_code.IMPACT159", "year", "sum.zinc.raw_mg", "sum.phytate_mg", "bioavailability.zinc")
    dt.bioavail_zinc <-  dt.bioavail_zinc[, (keepListCol), with = FALSE]

    #do some graphing
    dt.regions <- regionAgg("WB")
    scenChoice <- "SSP1-NoCC-REF"
    gyear <- "X2010"
    mainTitle <- paste("Dietary zinc vs dietary phytate;\n ", "scenario - ", scenChoice, ", year - ", gyear, sep = "")
    temp.all <- merge( dt.bioavail_zinc, dt.regions, by = "region_code.IMPACT159")
    pdf(paste(fileloc("gDir"), "/phytatePlot",gyear,".pdf", sep = ""))
    par(mai = c(.8,1,0.8,.5),oma = c(1,1,2,1), mfrow = c(2,2))
    for (i in unique(temp.all$region_code)) {
      gTitle <- paste("Income group - ", i, sep = "")
      temp <- temp.all[region_code %in% i & scenario %in% "SSP1-NoCC-REF" & year %in% "X2050",]
      plot(temp$sum.zinc.raw_mg, temp$sum.phytate_mg, type = "p", main = gTitle,
           xlab = "Dietary zinc (mg)", ylab = "Dietary phytate (mg)", ylim = c(800,8000),
           xlim = c(0,30), pch = 16, cex = .7)
    }
    mtext(mainTitle, outer = TRUE, cex = 1)
    dev.off()
    # get rid of dietary zinc and phytate in dt.bioavail_zinc. Only needed for graphing above
    dt.bioavail_zinc[, c("sum.phytate_mg", "sum.zinc.raw_mg") := NULL]
    dt.bioavail_zinc <- unique(dt.bioavail_zinc)

    # adjust zinc in dt.food.agg
    temp <- merge(dt.food.agg,  dt.bioavail_zinc, by = c("scenario", "region_code.IMPACT159", "year"))
    dt.food.agg <- temp[,zinc_mg.Q := zinc_mg.Q * bioavailability.zinc/100]
    dt.food.agg[,c("bioavailability.zinc") := NULL]
  }
  # end of iron and zinc bioavalability calculations -----

  # create name lists for use in operations below -----
  # the total daily availability of each nutrient
  nutListReq.sum.all <- paste(nutListReq, "sum.all", sep = ".")
  # the ratio of daily availability of each nutrient from each commodity to the total availability
  nutListreqRatio.all <- paste(nutListReq, "ratio.all", sep = ".")
  # the ratio of daily availability of each nutrient to the nutrient requirement
  nutListReq.reqRatio.all <- paste(nutListReq, "reqRatio.all", sep = ".")

  # the total daily consumption of each staple
  nutListReq.sum.staples <- paste(nutListReq, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutListreqRatio.staples <- paste(nutListReq, "ratio.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple by the nutrient requirement
  nutListReq.reqRatio.staples <- paste(nutListReq, "reqRatio.staple", sep = ".")

  # the total daily consumption of each food group
  nutListReq.sum.foodGroup <- paste(nutListReq, "sum.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup to the total consumption
  nutListReq.reqRatio.foodGroup <- paste(nutListReq, "reqRatio.foodGroup", sep = ".")
  # the ratio of daily consumption of each nutrient for each foodGroup by the nutrient requirement
  nutListreqRatio.foodGroup <- paste(nutListReq, "ratio.foodGroup", sep = ".")
  # calculate sums and ratios, for all food items, by staples, and by food groups -----
  # these keys are used to determine what is summed over or ratio made with
  allKey <- c("scenario", "region_code.IMPACT159", "year")
  sumKey <- c("scenario", "region_code.IMPACT159", "year","IMPACT_code")
  stapleKey <- c("scenario", "region_code.IMPACT159", "year", "staple_code")
  foodGroupKey <- c("scenario", "region_code.IMPACT159", "year", "food_group_code")

  # AMDR are lo and hi ranges for fat, carbohydrate and protein as percent of total kcals; if statement excludes AMDR calcs
  if (!req %in% c("req.AMDR_hi_percap", "req.AMDR_lo_percap")) {
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

    print(paste("summing by staples ", req, sep = ""))
    print(proc.time())

    ## individual nutrients by food group -----
    data.table::setkeyv(dt.food.agg,foodGroupKey)
    dt.food.agg <- dt.food.agg[, (nutListReq.sum.foodGroup) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                               by = eval(data.table::key(dt.food.agg))]
    data.table::setkey(dt.food.agg, NULL)
    dt.food.agg <- unique(dt.food.agg)

    # sum foodavail by food group -----
    temp <- dt.food.agg[, c("scenario", "region_code.IMPACT159", "year", "IMPACT_code", "foodAvailpDay","food_group_code") , with = FALSE]
    temp <- temp[year %in% c("X2010", "X2050")]

    #sum and convert to grams
    temp.foodgroup.sum <- temp[, foodavail.foodgroup.sum := sum(foodAvailpDay) * 1000,
                               by = c("scenario", "region_code.IMPACT159","year","food_group_code")]
    temp.foodgroup.sum[, c("IMPACT_code", "foodAvailpDay") := NULL]
    temp.foodgroup.sum <- unique(temp.foodgroup.sum)
    data.table::setnames(temp.foodgroup.sum, old = "foodavail.foodgroup.sum", new = "value")

    inDT <- temp.foodgroup.sum
    outName <- "dt.foodAvail.foodGroup"
    cleanup(inDT, outName, fileloc("resultsDir"), "csv")

    # now calculate ratios of nutrient by group to total consumption of the nutrient
    # nutrient from each food item to the total
    #dt.food.ratio <- data.table::copy(dt.all.sum[,(nutListreqRatio) := dt.all.sum[[nutListReq.Q]] / dt.all.sum[[nutListReq.sum]]])

    print(paste("calculating nutrient share ratios ", req, sep = ""))
    print(proc.time())

    # ratio of nutrient from each food item to the total
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListreqRatio.all[k] := get(nutListReq.Q[k]) / get(nutListReq.sum.all[k])]
    }
    # ratio of nutrient from each staple item to the total
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListreqRatio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.sum.all[k])]
    }
    # ratio of nutrient from each food group item to the total
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListreqRatio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.sum.all[k])]
    }

    leadingCols <- c("scenario", "region_code.IMPACT159",
                     "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
    laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
    data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))
  }

  # set up AMDRs -----
  if (req %in% c("req.AMDR_hi_percap", "req.AMDR_lo_percap")) {
    # calculate ratio of kcals from nutrient to total kcals. Multiply by 100 so its in same units as the AMDR values

    # use different source for dt.food.agg for AMDRs
    dt.food.agg <- data.table::copy(dt.nutrients.kcals)
    keepListCol <- c("scenario", "region_code.IMPACT159", "year", "kcalsPerDay.fat", "kcalsPerDay.protein", "kcalsPerDay.carbohydrate", "kcalsPerDay.tot")
    dt.food.agg <- unique(dt.food.agg[, (keepListCol), with = FALSE])

    # the .Q variables are the percent of the macronutrient kcals in total kcals
    dt.food.agg[, `:=`(
      fat_g.Q = 100 * kcalsPerDay.fat / kcalsPerDay.tot,
      protein_g.Q = 100 * kcalsPerDay.protein / kcalsPerDay.tot,
      carbohydrate_g.Q = 100 * kcalsPerDay.carbohydrate / kcalsPerDay.tot
    )
    ]
    dt.food.agg[, c("kcalsPerDay.fat", "kcalsPerDay.protein", "kcalsPerDay.carbohydrate", "kcalsPerDay.tot") := NULL]
  }
  # now do ratios with nutrient requirements
  print(paste("calculating nutrient requirement ratios for ", req, sep = ""))
  #print(proc.time())

  # change nutrient names in dt.nutsReqPerCap so they differ from those in nutListReq
  nutListReq.Req <- paste(nutListReq,"req", sep = ".")
  data.table::setnames(dt.nutsReqPerCap, old = nutListReq, new = nutListReq.Req)
  data.table::setkeyv(dt.food.agg, allKey)
  data.table::setkeyv(dt.nutsReqPerCap, allKey)
  # temp <- merge(dt.food.agg,dt.nutsReqPerCap, by = c(allKey,nutListReq), all.x = TRUE)
  dt.food.agg <- merge(dt.food.agg, dt.nutsReqPerCap, by = c(allKey), all.x = TRUE)
  leadingCols <- c("scenario", "region_code.IMPACT159",
                   "year", "IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  if (req %in% c("req.AMDR_hi_percap", "req.AMDR_lo_percap")) {
    leadingCols <- c("scenario", "region_code.IMPACT159",
                     "year")
  }
  laggingCols <- names(dt.food.agg)[!names(dt.food.agg) %in% leadingCols]
  data.table::setcolorder(dt.food.agg, c(leadingCols, laggingCols))

  # ratio of nutrient from each food item to the requirement
  for (k in 1:length(nutListReq)) {
    dt.food.agg[,nutListReq.reqRatio.all[k] := get(nutListReq.Q[k]) / get(nutListReq.Req[k])]
  }
  # if (req %in% c("req.AMDR_hi_percap", "req.AMDR_lo_percap")) {
  #   keeplistCol <- c("scenario" ,"region_code.IMPACT159","year", "carbohydrate_g.reqRatio.all", "fat_g.reqRatio.all", "protein_g.reqRatio.all")
  #   dt.food.agg <- dt.food.agg[, (keeplistCol), with = FALSE]
  # }
  print(paste("finished with ratio for each food item ", req, sep = ""))
  print(proc.time())

  if (!req %in% c("req.AMDR_hi_percap", "req.AMDR_lo_percap")) { # because staples and food groups are not relevant for AMDR

    # ratio of nutrient from each staple item to the requirement
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListReq.reqRatio.staples[k] := get(nutListReq.sum.staples[k]) / get(nutListReq.Req[k])]
    }
    print(paste("finished with ratio for the staple/non staple categories ", req, sep = ""))
    print(proc.time())

    # ratio of nutrient from each food group item to the requirement
    for (k in 1:length(nutListReq)) {
      dt.food.agg[,nutListReq.reqRatio.foodGroup[k] := get(nutListReq.sum.foodGroup[k]) / get(nutListReq.Req[k])]
    }
    print(paste("finished with requirement ratio for the food group categories ", req, sep = ""))
    print(proc.time())
  }
  # cap alcohol at 100 gm
  if ("ethanol_g" %in% names(dt.food.agg)) {
    dt.food.agg[ethanol_g.sum.all > 100, ethanol_g.sum.all := 100]
    dt.food.agg[ethanol_g.sum.staple > 100, ethanol_g.sum.staple := 100]
    dt.food.agg[ethanol_g.sum.foodGroup > 100, ethanol_g.sum.foodGroup := 100]
  }
  inDT <- dt.food.agg
  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)
  outName <- paste("food_agg_",reqShortName,sep = "")
  cleanup(inDT, outName,fileloc("resultsDir"), "csv")
}
# end of generateResults function

generateSum <- function(dt.IMPACTfood, scenarioListIMPACT, dt.nutrients.adj) {
  print("Creating sum for all nutrients")
  # get bioavailable shares for iron and zinc
  dt.bioavail_zinc <- getNewestVersion("dt.bioavail_zinc", fileloc("resultsDir"))
  dt.bioavail_iron <- getNewestVersion("dt.bioavail_iron", fileloc("resultsDir"))
  keepListCol.zinc <- c("scenario", "region_code.IMPACT159", "year", "bioavailability.zinc")
  keepListCol.iron <- c("scenario", "region_code.IMPACT159", "year", "bioavailability.iron")
  dt.bioavail_zinc <- dt.bioavail_zinc[,(keepListCol.zinc), with = FALSE]
  dt.bioavail_zinc <- unique(dt.bioavail_zinc)
  dt.bioavail_iron <- dt.bioavail_iron[,(keepListCol.iron), with = FALSE]
  dt.food <- data.table::copy(dt.IMPACTfood)
  dt.food <- dt.food[scenario %in% scenarioListIMPACT,]
  colsNotNutrients <- c("usda_code", "item", "foodAvailRatio",
                        "item_name", "usda_desc", "IMPACT_code", "phytate_source",
                        "Ref_Desc", "RetnDesc", "retentioncode_aus", "food_group_code", "staple_code",
                        "Long_Desc", "phytate_mg")
  nutListReq <- names(dt.nutrients.adj)[!names(dt.nutrients.adj) %in% colsNotNutrients]
  nutListReq.Q <- paste(nutListReq, "Q", sep = ".")
  # nutListReq.sum.all <- paste(nutListReq, "sum.all", sep = ".")
  nutListReq.sum.all <- paste(nutListReq, sep = ".")
  # nutListReq.sum.staple <- paste(nutListReq, "sum.staple", sep = ".")
  nutListReq.sum.staple <- paste(nutListReq, sep = ".")

  data.table::setkey(dt.nutrients.adj, IMPACT_code)
  data.table::setkeyv(dt.food, c("scenario","region_code.IMPACT159","IMPACT_code","year" ))
  dt.foodnNuts <- merge(dt.food, dt.nutrients.adj, by = "IMPACT_code", all = TRUE)
  # multiply the food item by the nutrients it contains and copy into a table called dt.food.agg
  dt.food.agg <- dt.foodnNuts[, (nutListReq.Q) := lapply(.SD, function(x)
    (x * dt.foodnNuts[['foodAvailpDay']])), .SDcols = nutListReq][,(nutListReq) := NULL]

  #convert iron and zinc to bioavailable shares
  dt.food.agg <- merge(dt.food.agg, dt.bioavail_zinc, by = c("scenario","region_code.IMPACT159", "year" ))
  dt.food.agg[, zinc_mg.Q := zinc_mg.Q * bioavailability.zinc / 100]
  dt.food.agg <- merge(dt.food.agg, dt.bioavail_iron, by = c("scenario","region_code.IMPACT159", "year" ))
  dt.food.agg[, iron_mg.Q := iron_mg.Q * bioavailability.iron / 100]
  dt.food.agg[, c("bioavailability.zinc", "bioavailability.iron") := NULL]

  print("summing nutrient for all commodities and staples")
  allKey <- c("scenario", "region_code.IMPACT159", "year")
  stapleKey <- c(allKey, "staple_code")

  # do sum of all first
  dt.food.agg.all <- data.table::copy(dt.food.agg)

  data.table::setkeyv(dt.food.agg.all, allKey)
  dt.food.agg.all[, (nutListReq) := lapply(.SD, sum), .SDcols = (nutListReq.Q),
                  by = eval(data.table::key(dt.food.agg.all))]
  dt.food.agg.all[,(nutListReq.Q) := NULL]
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "food_group_code", "staple_code")
  dt.food.agg.all[,(deleteListCol) := NULL]
  dt.nut.wide <- unique(dt.food.agg.all)
  dt.nut.long <- data.table::melt(dt.nut.wide,
                                  id.vars = c("scenario","region_code.IMPACT159", "year"),
                                  variable.name = "nutrient",
                                  measure.vars = nutListReq,
                                  value.name = "value",
                                  variable.factor = FALSE)

  inDT <- unique(dt.nut.long)

  outName <- "dt.nutrients.sum.all"
  cleanup(inDT,outName, fileloc("resultsDir"))

  # do staples next
  dt.food.agg.staples <- data.table::copy(dt.food.agg)
  data.table::setkeyv(dt.food.agg.staples, stapleKey)
  dt.food.agg.staples[, (nutListReq.sum.staple) := lapply(.SD, sum), .SDcols = nutListReq.Q,
                      by = eval(data.table::key(dt.food.agg.staples))][,(nutListReq.Q) := NULL]
  #  print(proc.time())
  deleteListCol <- c("IMPACT_code", "foodAvailpDay", "food_group_code")
  dt.food.agg.staples[,(deleteListCol) := NULL]
  dt.nut.sum.staple.wide <- unique(dt.food.agg.staples)

  # next step gets rid of a lot of extraneous column info
  dt.nut.sum.staple.long <- data.table::melt(dt.nut.sum.staple.wide,
                                             id.vars = c("scenario","region_code.IMPACT159", "staple_code","year"),
                                             variable.name = "nutrient",
                                             measure.vars = nutListReq.sum.staple,
                                             value.name = "value",
                                             variable.factor = FALSE)
  dt.nut.sum.staple.long <- unique(dt.nut.sum.staple.long)
  inDT <- dt.nut.sum.staple.long
  outName <- "dt.nutrients.sum.staples"
  cleanup(inDT,outName, fileloc("resultsDir"))

  # Create non staple share of nutrient dt
  shareFormula <- "scenario + region_code.IMPACT159 + nutrient + year ~ staple_code"
  dt.nut.nonstaple.share.wide <- data.table::dcast(data = dt.nut.sum.staple.long,
                                                   formula = shareFormula,
                                                   value.var = "value")
  dt.nut.nonstaple.share.wide[, value := 100 * nonstaple/(nonstaple + staple) ][is.na(value), value := 0]

  dt.nut.nonstaple.share.long <- dt.nut.nonstaple.share.wide[,c("nonstaple", "staple") := NULL]
  inDT <- dt.nut.nonstaple.share.long
  outName <- "dt.nutrients.nonstapleShare"
  cleanup(inDT, outName, fileloc("resultsDir"))
}

# run generateResults script -----
for (i in 1:length(reqsListPercap)) {
  generateResults.dataPrep(reqsListPercap[i],dt.IMPACTfood,scenarioListIMPACT, dt.nutrients.adj)
  print(paste("Done with ", reqsListPercap[i], ". ", length(reqsListPercap) - i," sets of requirements to go.", sep = ""))
}

# run  generateSum script -----
generateSum(dt.IMPACTfood, scenarioListIMPACT, dt.nutrients.adj)
