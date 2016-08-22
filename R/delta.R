# calculate differences between base run results in 2050 and 2050 results from
# a. experiment outcomes for given climate scenario, for USAID report
# b. different SSPs for given climate scenario, for research paper
# c. different climate models with SSP2
# base run is SSP2, NoCC
# differences should be of
# a. intake of each nutrient as a percent of the base run outcome; e.g. delta protein/base protein
# b. change in the percentage of the requirement met

#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name delta.R
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
# GDP setup -----
library(data.table)
dt.GDP <- data.table::copy(dt.IMPACTfood)
dt.GDP <- unique(dt.GDP[,c("IMPACT_code","FoodAvailability","PCX0","PWX0","CSE") :=  NULL])
dt.GDP <- dt.GDP[year %in% c("X2010","X2050"), ]
#data.table::setkeyv(dt.GDP,c("region_code.IMPACT159"))
dt.GDP <- dt.GDP[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                 .SDcols = "pcGDPX0", by = c("scenario","region_code.IMPACT159")]
dt.GDP <- dt.GDP[year ==  "X2050",][,year :=  NULL]
dt.GDPonly <- data.table::setorder(dt.GDP[, growthRate := NULL], pcGDPX0)

# all nutrients -----
dt.nutrients.sum <- getNewestVersion("dt.nutrients.sum", fileloc("resultsDir"))
macroNutrients <- c("protein_g.sum.all", "fat_g.sum.all", "carbohydrate_g.sum.all",  "totalfiber_g.sum.all")
vitamins <- c("vits_c_mg.sum.all", "thiamin_mg.sum.all", "riboflavin_mg.sum.all", "niacin_mg.sum.all",
              "vits_b6_mg.sum.all", "folate_µg.sum.all", "vits_b12_µg.sum.all", "vits_a_rae_µg.sum.all",
              "vits_e_mg.sum.all",  "vits_d_μg.sum.all", "vits_k_µg.sum.all")
minerals <- c("calcium_mg.sum.all",  "iron_mg.sum.all", "magnesium_mg.sum.all", "phosphorus_mg.sum.all",
              "potassium_g.sum.all", "sodium_g.sum.all", "zinc_mg.sum.all")
kcals <- c("kcals.fat.sum.all", "kcals.protein.sum.all", "kcals.sugar.sum.all", "kcals.ethanol.sum.all")
addedSugar <- c("sugar_g.sum.all")
fattyAcids <- c("ft_acds_tot_sat_g.sum.all", "ft_acds_mono_unsat_g.sum.all", "ft_acds_plyunst_g.sum.all",
                "ft_acds_tot_trans_g.sum.all")

basicInputs <- c("scenario",  "region_code.IMPACT159",  "year", "staple_code")
deletedNutrients <- c("cholesterol_mg.sum.staple", "caffeine_mg.sum.staple", "energy_kcal.sum.staple")
scenario.base <- "SSP2-NoCC-REF"
scenario.SSP1 <- "SSP1-NoCC-REF"
scenario.SSP3 <- "SSP3-NoCC-REF"
scenario.ClimModel.GFDL <- "SSP2-GFDL-REF"
scenario.ClimModel.IPSL <- "SSP2-IPSL-REF"
scenario.ClimModel.HGEM <- "SSP2-HGEM-REF"
scenarioList <- c(scenario.base, scenario.SSP1, scenario.SSP3, scenario.ClimModel.GFDL, scenario.ClimModel.IPSL, scenario.ClimModel.HGEM)
deleteListCol <- c(scenario.SSP1, scenario.SSP3, scenario.ClimModel.GFDL, scenario.ClimModel.IPSL, scenario.ClimModel.HGEM)
# keep just 2050 and the relevant scenarios
dt.nutrients.sum <- dt.nutrients.sum[year %in% "X2050" &
                                       scenario %in% scenarioList,]
dt.nutrients.sum <- dt.nutrients.sum[,year := NULL]
idVars <- c("scenario", "region_code.IMPACT159")
measureVars <- names(dt.nutrients.sum)[!names(dt.nutrients.sum) %in% idVars]

dt.nutrients.sum.melt <- data.table::melt(dt.nutrients.sum,
                                          id.vars = idVars,
                                          variable.name = "nutrient",
                                          measure.vars = measureVars,
                                          value.name = "value",
                                          variable.factor = FALSE)

# formula.sum.all <- paste("region_code.IMPACT159 + nutrient ~ scenario")
#
# dt.nutrients.sum.wide <- data.table::dcast(
#   data = dt.nutrients.sum.melt,
#   formula = formula.sum.all,
#   value.var = "value",
#   variable.factor = FALSE)

dt.nutrients.sum.melt <- merge(dt.GDPonly, dt.nutrients.sum.melt, by = c("scenario","region_code.IMPACT159"))
data.table::setorder(dt.nutrients.sum.melt, pcGDPX0)

#box plots nutrients -----
temp <- dt.nutrients.sum.melt[,c("region_code.IMPACT159","pcGDPX0") := NULL][nutrient == "energy_kcal.sum.all",]
yrange <- range(temp$value)
png('deleteme.png')
box.test <- boxplot(value ~ scenario, data = temp, range = 1.5,
                    at = c(1, 2.5, 4, 5.5, 7, 8.5),
                    xaxt = 'n',
                    ylim = yrange,
                    col = c('white', 'white smoke', 'gray'))
axis(side = 1, at = c(1, 2.5, 4, 5.5, 7, 8.5), labels = FALSE)
labels <- gsub("-REF","", unique(temp$scenario))
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Per Capita Energy Availability by Scenario')
text(c(1, 2.5, 4, 5.5, 7, 8.5), par("usr")[3] - 100, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
# add scenario labels near the box plots
#text(c(1, 2.5, 4, 5.5, 7, 8.5), c(1, 2.5, 4, 5.5, 7, 8.5), unique(temp$scenario), srt = 45)
dev.off()


# delta climate model effects on nutrients -----
dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.HGEM :=
                                                 100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.IPSL :=
                                                 100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.GFDL :=
                                                 100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# delta SSP effects on nutrients -----
dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.SSP3 :=
                                                 100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.SSP1 :=
                                                 100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.nutrients.sum.wide[,(deleteListCol) :=  NULL]

# macro nutrients effects
temp <- dt.nutrients.sum.wide[nutrient %in% macroNutrients,]

# Shannon diversity ------
dt.shannonDiversity <- getNewestVersion("dt.shannonDiversity", fileloc("resultsDir"))
dt.shannonDiversity <- dt.shannonDiversity[year == "X2050" &
                                             scenario %in% scenarioList,]
deleteListCol <- c("year", "SD")
dt.shannonDiversity[, (deleteListCol) := NULL]
formula.shannonDiversity <- paste("region_code.IMPACT159  ~ scenario")
dt.shannonDiversity.wide <- data.table::dcast(
  data = dt.shannonDiversity,
  formula = formula.shannonDiversity,
  value.var = "SDnorm",
  variable.factor = FALSE)

# delta climate model effects on Shannon diversity -----
dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.HGEM :=
                                                       100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.IPSL :=
                                                       100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.GFDL :=
                                                       100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# delta SSP effects on Shannon diversity -----
dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.SSP3 :=
                                                       100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.SSP1 :=
                                                       100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.shannonDiversity.wide[,(deleteListCol) :=  NULL]

# budget share -----
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
dt.budgetShare <- dt.budgetShare[year ==  "X2050" &
                                   scenario %in% scenarioList,][, year := NULL]
deleteListCol <- c("pcGDPX0", "budget.PWX0", "budget.PCX0", "incSharePWX0")
dt.budgetShare <- dt.budgetShare[, (deleteListCol) := NULL]

formula.budgetShare <- paste("scenario + region_code.IMPACT159  ~ scenario")
dt.budgetShare.wide <- data.table::dcast(
  data = dt.budgetShare,
  formula = formula.budgetShare,
  value.var = "incSharePCX0",
  variable.factor = FALSE)

idVars <- c("scenario", "region_code.IMPACT159")
measureVars <- "incSharePCX0"
dt.budgetShare.melt <- data.table::melt(dt.budgetShare,
                                        id.vars = idVars,
                                        variable.name = "year",
                                        measure.vars = measureVars,
                                        value.name = "value",
                                        variable.factor = FALSE)


# delta climate model effects on budget share -----
dt.budgetShare.wide <- dt.budgetShare.wide[, delta.HGEM :=
                                             100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.budgetShare.wide <- dt.budgetShare.wide[, delta.IPSL :=
                                             100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.budgetShare.wide <- dt.budgetShare.wide[, delta.GFDL :=
                                             100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# delta SSP effects on budget share -----
dt.budgetShare.wide <- dt.budgetShare.wide[, delta.SSP3 :=
                                             100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.budgetShare.wide <- dt.budgetShare.wide[, delta.SSP1 :=
                                             100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
dt.budgetShare.wide[,(deleteListCol) :=  NULL]
library(data.table)
dt.IMPACTfood <- getNewestVersion("dt.IMPACTfood", fileloc("iData"))
deleteListNames <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code")
dt.IMPACTfood[,list(sapply(.SD, min, na.rm = TRUE, USE.NAMES = TRUE), lapply(.SD, mean, na.rm = TRUE, USE.NAMES = TRUE), lapply(.SD, max, na.rm = TRUE, USE.NAMES = TRUE)),
              .SDcols = c(names(dt.IMPACTfood)[!names(dt.IMPACTfood) %in% deleteListNames])]

deltaPlot <- function(dt, var, title, ylabel, regions) {
  if (missing(regions)) {regions <- 156}
  dt.temp <- data.table::copy(dt)
  data.table::setorderv(dt.temp, var)
  junk <- dt.temp[,{
    plot(x = get(var), type = "s", main = title,
         ylab = ylabel,
         xlab = "",
         xaxt = "n",
         cex.lab = 0.8, mgp = c(2.5, 1, 0),
         yaxs = "r")
    abline(h = 0)
    axis(1, at = 1:regions, labels = region_code.IMPACT159, cex.axis = 0.5)}
    ]
}

dt.temp <- data.table::copy(dt.budgetShare.wide)
data.table::setorderv(dt.temp, var)
junk <- dt.temp[,{
  plot(x = get(var), type = "s", main = title,
       ylab = ylabel,
       xlab = "",
       xaxt = "n",
       cex.lab = 0.8, mgp = c(2.5, 1, 0),
       yaxs = "r")
  abline(h = 0)
  axis(1, at = 1:156, labels = region_code.IMPACT159, cex.axis = 0.5)}
  ]


par(mfrow = c(1, 1))
yrange <- range(dt.budgetShare.wide$delta.SSP3, dt.budgetShare.wide$delta.SSP1)
data.table::setorder(dt.budgetShare.wide, delta.SSP1)
plot(dt.budgetShare.wide$delta.SSP1, type = "s", col = "red", ylim = yrange,
     ylab = "Change in IMPACT commodity\nshare of per capita GDP (%)",
     xlab = "",
     xaxt = "n",
     cex.lab = 0.8, mgp = c(2.5, 1, 0),
     yaxs = "r")
lines(dt.budgetShare.wide$delta.SSP3, col = "green")
axis(1, at = 1:156, labels = dt.budgetShare.wide$region_code.IMPACT159, cex.axis = 0.5)
legendText <- c("Delta SSP1", "Delta SSP3")
colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )

legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
       col = colors_in, text.col = "black", cex = .9, pt.cex = .9, pt.lwd = 1,
       y.intersp = .9)

deltaPlot(dt = dt.budgetShare.wide,
          var = "delta.SSP1", title = "Delta SSP1",
          ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
)

deltaPlot(dt = dt.budgetShare.wide,
          var = "delta.SSP3",title = "Delta SSP3",
          ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
)

deltaPlot(dt = dt.budgetShare.wide,
          var = "delta.HGEM", title = "Delta HGEM",
          ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
)
deltaPlot(dt = dt.budgetShare.wide,
          var = "delta.GFDL", title = "Delta GFDL",
          ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
)

# remove SOM because it's expenditure is greater than its per cap income
temp <- data.table::copy(dt.budgetShare.wide)
temp <- temp[!region_code.IMPACT159 ==  "SOM",][, `SSP2-NoCC-REF` :=  `SSP2-NoCC-REF` * 100]
deltaPlot(dt = temp,
          var = "SSP2-NoCC-REF",
          title = "Reference scenario:\nSSP2 with no climate change",
          ylabel = "IMPACT commodity\nshare of per capita GDP (%)",
          regions = 155
)

# macro food group availability -----
RDA.macro.FG.ratio <- getNewestVersion("RDA.macro.FG.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code", "X2050")
RDA.macro.FG.ratio <- RDA.macro.FG.ratio[, keepListCol, with = FALSE]
RDA.macro.FG.ratio <- RDA.macro.FG.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code")
measureVars <- "X2050"
RDA.macro.FG.ratio.melt <- data.table::melt(RDA.macro.FG.ratio,
                                            id.vars = idVars,
                                            variable.name = "year",
                                            measure.vars = measureVars,
                                            value.name = "value",
                                            variable.factor = FALSE)

formula.macro.FG <- paste("region_code.IMPACT159 + year + nutrient +food_group_code ~ scenario")
RDA.macro.FG.ratio.wide <- data.table::dcast(
  data = RDA.macro.FG.ratio.melt,
  formula = formula.macro.FG,
  value.var = "value",
  variable.factor = FALSE)

RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.HGEM :=
                                                     100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.IPSL :=
                                                     100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.GFDL :=
                                                     100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.SSP3 :=
                                                     100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.SSP1 :=
                                                     100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.FG.ratio.wide[,(deleteListCol) :=  NULL]

# minerals food group availability -----
RDA.minrls.FG.ratio <- getNewestVersion("RDA.minrls.FG.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code", "X2050")
RDA.minrls.FG.ratio <- RDA.minrls.FG.ratio[, keepListCol, with = FALSE]
RDA.minrls.FG.ratio <- RDA.minrls.FG.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code")
measureVars <- "X2050"
RDA.minrls.FG.ratio.melt <- data.table::melt(RDA.minrls.FG.ratio,
                                             id.vars = idVars,
                                             variable.name = "year",
                                             measure.vars = measureVars,
                                             value.name = "value",
                                             variable.factor = FALSE)

formula.minrls.FG <- paste("region_code.IMPACT159 + year + nutrient +food_group_code ~ scenario")
RDA.minrls.FG.ratio.wide <- data.table::dcast(
  data = RDA.minrls.FG.ratio.melt,
  formula = formula.minrls.FG,
  value.var = "value",
  variable.factor = FALSE)

RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.HGEM :=
                                                       100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.IPSL :=
                                                       100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.GFDL :=
                                                       100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.SSP3 :=
                                                       100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.SSP1 :=
                                                       100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.minrls.FG.ratio.wide[,(deleteListCol) :=  NULL]

# vitamins food group availability -----
RDA.vits.FG.ratio <- getNewestVersion("RDA.vits.FG.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code", "X2050")
RDA.vits.FG.ratio <- RDA.vits.FG.ratio[, keepListCol, with = FALSE]
RDA.vits.FG.ratio <- RDA.vits.FG.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code")
measureVars <- "X2050"
RDA.vits.FG.ratio.melt <- data.table::melt(RDA.vits.FG.ratio,
                                           id.vars = idVars,
                                           variable.name = "year",
                                           measure.vars = measureVars,
                                           value.name = "value",
                                           variable.factor = FALSE)

formula.vits.FG <- paste("region_code.IMPACT159 + year + nutrient +food_group_code ~ scenario")
RDA.vits.FG.ratio.wide <- data.table::dcast(
  data = RDA.vits.FG.ratio.melt,
  formula = formula.vits.FG,
  value.var = "value",
  variable.factor = FALSE)

RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.HGEM :=
                                                   100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.IPSL :=
                                                   100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.GFDL :=
                                                   100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.SSP3 :=
                                                   100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.SSP1 :=
                                                   100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.FG.ratio.wide[,(deleteListCol) :=  NULL]

# macro adequacy ----
RDA.macro.sum.req.ratio <- getNewestVersion("RDA.macro.sum.req.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "X2050")
RDA.macro.sum.req.ratio <- RDA.macro.sum.req.ratio[, keepListCol, with = FALSE]
RDA.macro.sum.req.ratio <- RDA.macro.sum.req.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
measureVars <- "X2050"
RDA.macro.sum.req.ratio.melt <- data.table::melt(RDA.macro.sum.req.ratio,
                                                 id.vars = idVars,
                                                 variable.name = "year",
                                                 measure.vars = measureVars,
                                                 value.name = "value",
                                                 variable.factor = FALSE)

formula.macro.req <- paste("region_code.IMPACT159 + year + nutrient ~ scenario")
RDA.macro.sum.req.ratio.wide <- data.table::dcast(
  data = RDA.macro.sum.req.ratio.melt,
  formula = formula.macro.req,
  value.var = "value",
  variable.factor = FALSE)

RDA.macro.sum.req.ratio.wide <- RDA.macro.sum.req.ratio.wide[, delta.HGEM :=
                                                               (100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`), by = c("nutrient")]
RDA.macro.sum.req.ratio.wide <- RDA.macro.sum.req.ratio.wide[, delta.IPSL :=
                                                               100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.sum.req.ratio.wide <- RDA.macro.sum.req.ratio.wide[, delta.GFDL :=
                                                               100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.sum.req.ratio.wide <- RDA.macro.sum.req.ratio.wide[, delta.SSP3 :=
                                                               100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.sum.req.ratio.wide <- RDA.macro.sum.req.ratio.wide[, delta.SSP1 :=
                                                               100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.macro.sum.req.ratio.wide[,(deleteListCol) :=  NULL]

# minerals adequacy ----
RDA.minrls.sum.req.ratio <- getNewestVersion("RDA.minrls.sum.req.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "X2050")
RDA.minrls.sum.req.ratio <- RDA.minrls.sum.req.ratio[, keepListCol, with = FALSE]
RDA.minrls.sum.req.ratio <- RDA.minrls.sum.req.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
measureVars <- "X2050"
RDA.minrls.sum.req.ratio.melt <- data.table::melt(RDA.minrls.sum.req.ratio,
                                                  id.vars = idVars,
                                                  variable.name = "year",
                                                  measure.vars = measureVars,
                                                  value.name = "value",
                                                  variable.factor = FALSE)

#box plots nutrients requirement -----
temp <- RDA.vits.sum.req.ratio.melt[,c("region_code.IMPACT159","year") := NULL][nutrient == "vit_e_mg_reqRatio",]
yrange <- range(temp$value)
png('deleteme.png')
box.test <- boxplot(value ~ scenario, data = temp, range = 0,
                    at = c(1, 2, 3, 5, 6, 7),
                    xaxt = 'n',
                    ylim = yrange,
                    col = c('white', 'white smoke', 'gray'))
axis(side = 1, at = c(1, 2, 3, 5, 6, 7), labels = FALSE)
labels <- gsub("-REF","", unique(temp$scenario))
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Availability of Vitamin E as Share of Requirement')
text(c(1, 2, 3, 5, 6, 7), par("usr")[3] - 0.1, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(h = 1, lty = 3, lwd = 0.8)
dev.off()


#box plots -----
yrange <- range(RDA.macro.sum.req.ratio.wide$delta.HGEM,
                RDA.macro.sum.req.ratio.wide$delta.IPSL,
                RDA.macro.sum.req.ratio.wide$delta.SSP1,
                RDA.macro.sum.req.ratio.wide$delta.SSP3)
yrange <- c(-30, 40)
par(mfrow = c(1, 4), mar = c(6,4,2,2), cex = 0.8, lwd = 0.7,las = 2)
plot.new()
plot(NA,
     xlim = c(0,80),
     ylim = yrange,
     xlab = "",
     ylab = "Percent change",
     axes = F)
abline(h = 0,lty = 3,lwd = 0.8)
axis(2,lwd = 0.7,at = -3:3*20)
boxplot(RDA.macro.sum.req.ratio.wide$delta.HGEM, las = 2,outline = T,range = 5,axes = F,
        col = "white smoke",
        #        ylim = yrange,
        xlab = "Delta HGEM",
        add = T)
boxplot(RDA.macro.sum.req.ratio.wide$delta.IPSL, las = 2, outline = T, range = 5, axes = F,
        col = "white smoke",
        #        ylim = yrange,
        xlab = "Delta IPSL",
        add = F)
boxplot(RDA.macro.sum.req.ratio.wide$delta.SSP1, las = 2, outline = T, range = 5, axes = F,
        col = "white smoke",
        #        ylim = yrange,
        xlab = "Delta SSP1",
        add = F)
boxplot(RDA.macro.sum.req.ratio.wide$delta.SSP3, las = 2, outline = T, range = 5, axes = F,
        col = "white smoke",
        #       ylim = yrange,
        xlab = "Delta SSP3",
        add = F)
# mtext("n",side=1,line=1,at=0,cex=0.4,las=1,adj=0)
# mtext("Mean",side=1,line=2,at=0,cex=0.4,las=1,adj=0)
# mtext("SD",side=1,line=3,at=0,cex=0.4,las=1,adj=0)

#dev.off()

# vitamin adequacy -----
RDA.vits.sum.req.ratio <- getNewestVersion("RDA.vits.sum.req.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "X2050")
RDA.vits.sum.req.ratio <- RDA.vits.sum.req.ratio[, keepListCol, with = FALSE]
RDA.vits.sum.req.ratio <- RDA.vits.sum.req.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
measureVars <- "X2050"
RDA.vits.sum.req.ratio.melt <- data.table::melt(RDA.vits.sum.req.ratio,
                                                id.vars = idVars,
                                                variable.name = "year",
                                                measure.vars = measureVars,
                                                value.name = "value",
                                                variable.factor = FALSE)

formula.vits.req <- paste("region_code.IMPACT159 + year + nutrient ~ scenario")
RDA.vits.sum.req.ratio.wide <- data.table::dcast(
  data = RDA.vits.sum.req.ratio.melt,
  formula = formula.vits.req,
  value.var = "value",
  variable.factor = FALSE)

RDA.vits.sum.req.ratio.wide <- RDA.vits.sum.req.ratio.wide[, delta.HGEM :=
                                                             (100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`), by = c("nutrient")]
RDA.vits.sum.req.ratio.wide <- RDA.vits.sum.req.ratio.wide[, delta.IPSL :=
                                                             100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.sum.req.ratio.wide <- RDA.vits.sum.req.ratio.wide[, delta.GFDL :=
                                                             100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.sum.req.ratio.wide <- RDA.vits.sum.req.ratio.wide[, delta.SSP3 :=
                                                             100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.sum.req.ratio.wide <- RDA.vits.sum.req.ratio.wide[, delta.SSP1 :=
                                                             100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
RDA.vits.sum.req.ratio.wide[,(deleteListCol) :=  NULL]

# PCX calcs ----
dt.PCX <- data.table::copy(dt.IMPACTfood)
dt.PCX <- unique(dt.PCX[,c("pcGDPX0","FoodAvailability","PWX0","CSE") :=  NULL])
dt.PCX <- dt.PCX[year %in% c("X2010","X2050"), ]
dt.PCX <- dt.PCX[!PCX0 ==  0,]
dt.PCX <- dt.PCX[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                 .SDcols = "PCX0", by = c("scenario", "IMPACT_code","region_code.IMPACT159")]
dt.PCX <- dt.PCX[year ==  "X2050",][,year :=  NULL]

# plot nutrient ratios by country and scenario
plotCountries <- function(dt, fileName) {
  dt, fileName[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
  dt, fileName[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
  data.table::setorder(dt, fileName,newOrder)

  png(paste(fileName, "png", sep = "."))
  dt.GDPonly.ref <- dt.GDPonly[scenario == "SSP2-NoCC-REF", ][,scenario := NULL]
  data.table::setorder(dt.GDPonly.ref, pcGDPX0)
  scenarios <- unique(dt$scenario)
  nutrients <- unique(dt$nutrient)
  colList <- c("red", "red2", "red4", "green", "green2", "green4")
  par(mfrow = c(round(length(nutrients)/2),2))
  par(mar = c(1,1,1,1) + 0.1)
  for (j in 1:length(nutrients)) {
    legendText <- NULL
    #  print(nutrients[j])
    for (i in 1:length(scenarios)) {
      scen.temp <- data.table::copy(dt)
      scen.temp <- scen.temp[nutrient == nutrients[j] & scenario == scenarios[i],]
      scen.temp <- scen.temp[,c("nutrient", "scenario", "year") := NULL]
      scen.temp <- merge(scen.temp, dt.GDPonly.ref, by = "region_code.IMPACT159")
      data.table::setorder(scen.temp, pcGDPX0)
      nutShortname <- cleanupNutrientNames(nutrients[j])
      nutShortname
      if (i == 1) {
        plot(scen.temp$value, type = "l", col = "green", xlab = "", xaxt = "n",
             main = nutShortname,cex.main=1, ylim = c(0, 4)) # common range for requirements share
        #      main = nutShortname,cex.main=1, ylim = c(0, round(max(scen.temp$value))))
        par(new = T)
      } else {
        lines(scen.temp$value, col = colList[i])
        par(new = F)
      }
      legendText <- c(legendText,scenarios[i])
    }
    # print(legendText)
    axis(1, at = 1:regions, labels = dt.GDPonly.ref$region_code.IMPACT159, cex.axis = 0.5, padj = -3)
    abline(h = 1, lty = 3, lwd = 0.8)
    legendText <- gsub("-REF", "", legendText)
    legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
           col = colList, text.col = "black", cex = .5, pt.cex = .5, pt.lwd = 1,
           y.intersp = .8)
  }
  dev.off()
}

plotCountries(RDA.minrls.sum.req.ratio.melt, "minrls")
plotCountries(RDA.macro.sum.req.ratio.melt, "macroNutrients")
plotCountries(RDA.vits.sum.req.ratio.melt, "vitamins")

# plot budget shares by country and scenario -----
plotBudgetShares <- function(dt, fileName) {
  dt, fileName[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
  dt, fileName[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
  data.table::setorder(dt, fileName,newOrder)
  png(paste(fileName, "png", sep = "."))
  dt.GDPonly.ref <- dt.GDPonly[scenario == "SSP2-NoCC-REF", ][,scenario := NULL]
  data.table::setorder(dt.GDPonly.ref, pcGDPX0)
  scenarios <- unique(dt$scenario)
  colList <- c("red", "red2", "red4", "green", "green2", "green4")
  par(mfrow = c(1,1))
  #  par(mar = c(1,1,1,1) + 0.1)
  legendText <- NULL
  for (i in 1:length(scenarios)) {
    scen.temp <- data.table::copy(dt)
    scen.temp <- scen.temp[scenario == scenarios[i],]
    scen.temp <- scen.temp[,c("scenario", "year") := NULL]
    scen.temp <- merge(scen.temp, dt.GDPonly.ref, by = "region_code.IMPACT159")
    data.table::setorder(scen.temp, pcGDPX0)
    if (i == 1) {
      plot(scen.temp$value, type = "l", col = "green", xlab = "", xaxt = "n",
           main = "Budget Share", cex.main = 1, ylim = c(0, 0.5)) # common range for requirements share
      #      main = nutShortname,cex.main=1, ylim = c(0, round(max(scen.temp$value))))
      par(new = T)
    } else {
      lines(scen.temp$value, col = colList[i])
      par(new = F)
    }
    legendText <- c(legendText,scenarios[i])
  }
  # print(legendText)
  axis(1, at = 1:regions, labels = dt.GDPonly.ref$region_code.IMPACT159, cex.axis = 0.5, padj = -3)
  abline(h = 1, lty = 3, lwd = 0.8)
  legendText <- gsub("-REF", "", legendText)
  legend(x = "topright", y = NULL, legend = legendText, bty = "n", pch = 20,
         col = colList, text.col = "black", cex = .5, pt.cex = .5, pt.lwd = 1,
         y.intersp = .8)
  dev.off()
}

plotBudgetShares(dt.budgetShare.melt, "budgetShare")

# plot Shannon Diversity by country and scenario -----
plotShannonDiv <- function(dt, fileName) {
  dt, fileName[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
  dt, fileName[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
  data.table::setorder(dt, fileName,newOrder)
  png(paste(fileName, "png", sep = "."))
  dt.GDPonly.ref <- dt.GDPonly[scenario == "SSP2-NoCC-REF", ][,scenario := NULL]
  data.table::setorder(dt.GDPonly.ref, pcGDPX0)
  scenarios <- unique(dt$scenario)
  colList <- c("red", "red2", "red4", "green", "green2", "green4")
  par(mfrow = c(1,1))
  #  par(mar = c(1,1,1,1) + 0.1)
  legendText <- NULL
   for (i in 1:length(scenarios)) {
    scen.temp <- data.table::copy(dt)
    scen.temp <- scen.temp[scenario == scenarios[i],]
    scen.temp <- scen.temp[,c("scenario", "year") := NULL]
    scen.temp <- merge(scen.temp, dt.GDPonly.ref, by = "region_code.IMPACT159")
    data.table::setorder(scen.temp, pcGDPX0)
    yrange <- range(scen.temp$SDnorm)
    if (i == 1) {
      plot(scen.temp$SDnorm, type = "l", col = "green", xlab = "", xaxt = "n",
           main = "Shannon Diversity", cex.main = 1, ylim = yrange) # common range for requirements share
      #      main = nutShortname,cex.main=1, ylim = c(0, round(max(scen.temp$value))))
      par(new = T)
    } else {
      lines(scen.temp$value, col = colList[i])
      par(new = F)
    }
    legendText <- c(legendText,scenarios[i])
  }
  # print(legendText)
  axis(1, at = 1:regions, labels = dt.GDPonly.ref$region_code.IMPACT159, cex.axis = 0.5, padj = -3)
  abline(h = 1, lty = 3, lwd = 0.8)
  legendText <- gsub("-REF", "", legendText)
  legend(x = "topright", y = NULL, legend = legendText, bty = "n", pch = 20,
         col = colList, text.col = "black", cex = .5, pt.cex = .5, pt.lwd = 1,
         y.intersp = .8)
  dev.off()
}

plotShannonDiv(dt.shannonDiversity, "ShannonDiversity")


#box plots budget share -----
temp <- copy(dt.budgetShare.melt[,c("region_code.IMPACT159","year") := NULL])
temp[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
temp[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
data.table::setorder(temp,newOrder)
temp[, newOrder := NULL]
yrange <- range(temp$value)
png('budgetShareBoxPlots.png')
box.test <- boxplot(value ~ scenario, data = temp, range = 5,
                    at = c(1, 2, 3, 5, 6, 7),
                    xaxt = 'n',
                    ylim = c(0, 0.4),
                    col = c('white', 'white smoke', 'gray'))
labels <- gsub("-REF","", unique(temp$scenario))
axis(side = 1, at = c(1, 2, 3, 5, 6, 7),labels = FALSE)
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Share of IMPACT food items cost in per capita GDP by scenario')
text(c(1, 2, 3, 5, 6, 7), par("usr")[3] - .025, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
# add scenario labels near the box plots
#text(c(1, 2.5, 4, 5.5, 7, 8.5), c(1, 2.5, 4, 5.5, 7, 8.5), unique(temp$scenario), srt = 45)
dev.off()

#box plots nutrients -----
temp <- copy(dt.nutrients.sum.melt[,c("region_code.IMPACT159","pcGDPX0") := NULL][nutrient == "energy_kcal.sum.all",])
temp[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
temp[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
data.table::setorder(temp,newOrder)
yrange <- range(temp$value)
png('deleteme.png')
box.test <- boxplot(value ~ scenario, data = temp, range = 1.5,
                    at = c(1, 2.5, 4, 5.5, 7, 8.5),
                    xaxt = 'n',
                    ylim = yrange,
                    col = c('white', 'white smoke', 'gray'))
axis(side = 1, at = c(1, 2.5, 4, 5.5, 7, 8.5), labels = FALSE)
labels <- gsub("-REF","", unique(temp$scenario))
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Per Capita Energy Availability by Scenario')
text(c(1, 2.5, 4, 5.5, 7, 8.5), par("usr")[3] - 100, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
# add scenario labels near the box plots
#text(c(1, 2.5, 4, 5.5, 7, 8.5), c(1, 2.5, 4, 5.5, 7, 8.5), unique(temp$scenario), srt = 45)
dev.off()

#box plots Shannon diversity -----
temp <- copy(dt.shannonDiversity[,c("region_code.IMPACT159","year") := NULL])
temp[scenario == "SSP2-NoCC-REF" ,newOrder := 1] [scenario == "SSP1-NoCC-REF" ,newOrder := 2][scenario == "SSP3-NoCC-REF" ,newOrder := 3]
temp[scenario == "SSP2-GFDL-REF" ,newOrder := 4] [scenario == "SSP2-HGEM-REF" ,newOrder := 5][scenario == "SSP2-IPSL-REF" ,newOrder := 6]
data.table::setorder(temp,newOrder)
temp[, newOrder := NULL]
yrange <- range(temp$SDnorm)
png('ShannonDiversityBoxPlots.png')
box.test <- boxplot(SDnorm ~ scenario, data = temp, range = 5,
                    at = c(1, 2, 3, 5, 6, 7),
                    xaxt = 'n',
                    ylim = yrange,
                    col = c('white', 'white smoke', 'gray'))
labels <- gsub("-REF","", unique(temp$scenario))
axis(side = 1, at = c(1, 2, 3, 5, 6, 7),labels = FALSE)
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Shannon Diversity score by scenario')
text(c(1, 2, 3, 5, 6, 7), par("usr")[3] - .025, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
# add scenario labels near the box plots
#text(c(1, 2.5, 4, 5.5, 7, 8.5), c(1, 2.5, 4, 5.5, 7, 8.5), unique(temp$scenario), srt = 45)
dev.off()
