
library(shiny)
library(data.table)
library(fmsb) # for the spider charts
library(gdata) # to reorder the scenario names
source("R/nutrientModFunctions.R")
source("R/workbookFunctions.R")
source("R/nutrientCalcFunctions.R")
library(shinythemes)

years <- c("X2010", "X2030", "X2050")
scenarioName <- "SSP2-NoCC-REF"
countryCode <- "USA"
dt.stapleShares <- getNewestVersion("dt.nutrients.sum.staples", fileloc("resultsDir"))

loadSwitchValues()
if (switch.vars <- FALSE & switch.fortification <- FALSE) dt.stapleShares <- getNewestVersion("dt.nutrients.sum.staples", fileloc("resultsDir"))
if (switch.vars <- TRUE & switch.fortification <- FALSE) dt.stapleShares <-  getNewestVersion("dt.nutrients.sum.staplesVar", fileloc("resultsDir"))
if (switch.vars <- TRUE & switch.fortification <- TRUE) dt.stapleShares <-  getNewestVersion("dt.nutrients.sum.staplesVarFort", fileloc("resultsDir"))


temp <- dt.stapleShares[region_code.IMPACT159 == countryCode & scenario == scenarioName & year %in% years,]
macroNutrients <- c("energy_kcal.sum.staple", "protein_g.sum.staple", "fat_g.sum.staple", "carbohydrate_g.sum.staple",
                    "totalfiber_g.sum.staple")
vitamins <- c("vit_c_mg.sum.staple", "thiamin_mg.sum.staple", "riboflavin_mg.sum.staple", "niacin_mg.sum.staple",
              "vit_b6_mg.sum.staple", "folate_µg.sum.staple", "vit_b12_µg.sum.staple", "vit_a_rae_µg.sum.staple",
              "vit_e_mg.sum.staple",  "vit_d_µg.sum.staple", "vit_k_µg.sum.staple")
minerals <- c("calcium_mg.sum.staple",  "iron_mg.sum.staple", "magnesium_mg.sum.staple", "phosphorus_mg.sum.staple",
              "potassium_g.sum.staple", "zinc_mg.sum.staple")
kcals <- c("kcals.fat.sum.staple", "kcals.protein.sum.staple", "kcals.sugar.sum.staple", "kcals.ethanol.sum.staple")
addedSugar <- c("sugar_g.sum.staple")
fattyAcids <- c("ft_acds_tot_sat_g.sum.staple", "ft_acds_mono_unsat_g.sum.staple", "ft_acds_plyunst_g.sum.staple",
                "ft_acds_tot_trans_g.sum.staple")
basicInputs <- c("scenario",  "region_code.IMPACT159",  "year", "staple_code")
deletedNutrients <- c("cholesterol_mg.sum.staple", "caffeine_mg.sum.staple", "energy_kcal.sum.staple")
temp[, (deletedNutrients) := NULL]

idVars <- basicInputs
measureVars <- c(macroNutrients, vitamins, kcals, addedSugar, fattyAcids)
temp.melt <- data.table::melt(temp,
                              id.vars = idVars,
                              variable.name = "nutrient",
                              measure.vars = measureVars,
                              value.name = "value",
                              variable.factor = FALSE)

formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient + year ~ staple_code")
temp.wide <- data.table::dcast(
  data = temp.melt,
  formula = formula.wide,
  value.var = "value")

temp.wide[, nonStapleShare := nonstaple/(nonstaple + staple)]

temp <- temp[,(keepListCol), with = FALSE]
setnames(temp, old = years, new = yearsClean)
nutnames <- cleanupNutrientNames(temp[1:4,nutrient])
colors_in <- c( "yellow", "green", "blue","red" )
barplot(as.matrix(temp[1:4,years, with = FALSE]), main="Share of energy consumption",
        col=colors_in, ylim = c(0,1))
legend(x = "bottomright", y = NULL, legend = nutnames, bty = "n", pch = 20,
       col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
       y.intersp = .8)


