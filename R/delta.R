# calculate differences between base run results in 2050 and 2050 results from
# a. experiment outcomes for given climate scenario, for USAID report
# b. different SSPs for given climate scenario, for research paper
# c. different climate models with SSP2

#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name delta.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
# GDP setup -----
library(data.table)
basicInputs <- c("scenario",  "region_code.IMPACT159",  "year", "staple_code")
deletedNutrients <- c("cholesterol_mg.sum.staple", "caffeine_mg.sum.staple", "energy_kcal.sum.staple")
scenario.base <- "SSP2-NoCC-REF"
scenario.SSP1 <- "SSP1-NoCC-REF"
scenario.SSP3 <- "SSP3-NoCC-REF"
scenario.ClimModel.GFDL <- "SSP2-GFDL-REF"
scenario.ClimModel.IPSL <- "SSP2-IPSL-REF"
scenario.ClimModel.HGEM <- "SSP2-HGEM-REF"
scenarioList <- c(scenario.base, scenario.SSP1, scenario.SSP3, scenario.ClimModel.GFDL, scenario.ClimModel.IPSL, scenario.ClimModel.HGEM)

dt.IMPACTfood <- getNewestVersion("dt.IMPACTfood", fileloc("iData"))
dt.IMPACTfood <- dt.IMPACTfood[scenario %in% scenarioList,]
dt.GDP <- data.table::copy(dt.IMPACTfood)
dt.GDP <- unique(dt.GDP[,c("IMPACT_code","FoodAvailability","PCX0","PWX0","CSE") :=  NULL])
dt.GDP <- dt.GDP[year %in% c("X2010","X2050"), ]
#data.table::setkeyv(dt.GDP,c("region_code.IMPACT159"))
dt.GDP <- dt.GDP[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                 .SDcols = "pcGDPX0", by = c("scenario","region_code.IMPACT159")]
dt.GDP.2010.ref <- dt.GDP[year ==  "X2010" & scenario == scenario.base,][,c("scenario","growthRate","year") :=  NULL]

# all nutrients -----
dt.nutrients.sum <- getNewestVersion("dt.nutrients.sum", fileloc("resultsDir"))
macroNutrients <- c("protein_g.sum.all", "fat_g.sum.all", "carbohydrate_g.sum.all",  "totalfiber_g.sum.all")
vitamins <- c("vits_c_mg.sum.all", "thiamin_mg.sum.all", "riboflavin_mg.sum.all", "niacin_mg.sum.all",
              "vits_b6_mg.sum.all", "folate_µg.sum.all", "vits_b12_µg.sum.all", "vits_a_rae_µg.sum.all",
              "vits_e_mg.sum.all",  "vits_d_µg.sum.all", "vits_k_µg.sum.all")
minerals <- c("calcium_mg.sum.all",  "iron_mg.sum.all", "magnesium_mg.sum.all", "phosphorus_mg.sum.all",
              "potassium_g.sum.all", "zinc_mg.sum.all")
kcals <- c("kcals.fat.sum.all", "kcals.protein.sum.all", "kcals.sugar.sum.all", "kcals.ethanol.sum.all")
addedSugar <- c("sugar_g.sum.all")
fattyAcids <- c("ft_acds_tot_sat_g.sum.all", "ft_acds_mono_unsat_g.sum.all", "ft_acds_plyunst_g.sum.all",
                "ft_acds_tot_trans_g.sum.all")

deleteListCol <- c(scenario.SSP1, scenario.SSP3, scenario.ClimModel.GFDL, scenario.ClimModel.IPSL, scenario.ClimModel.HGEM)
# keep just 2050 and the relevant scenarios
dt.nutrients.sum <- dt.nutrients.sum[year %in% c("X2010","X2050") &
                                       scenario %in% scenario.base,]
idVars <- c("scenario", "region_code.IMPACT159", "year")
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

dt.nutrients.sum.melt <- merge(dt.GDP.2010.ref, dt.nutrients.sum.melt, by = c("region_code.IMPACT159"))
data.table::setorder(dt.nutrients.sum.melt, pcGDPX0)

# # macro nutrients effects
# temp <- dt.nutrients.sum.wide[nutrient %in% macroNutrients,]

# Shannon diversity data ------
dt.shannonDiversity <- getNewestVersion("dt.shannonDiversity", fileloc("resultsDir"))
dt.shannonDiversity <- dt.shannonDiversity[year %in% c("X2010","X2050") & scenario %in% scenarioList,]
deleteListCol <- c("SD")
dt.shannonDiversity[, (deleteListCol) := NULL]
data.table::setnames(dt.shannonDiversity, old = "SDnorm", new = "value")
formula.shannonDiversity <- paste("region_code.IMPACT159 + year  ~ scenario")
dt.shannonDiversity.wide. <- data.table::dcast(
  data = dt.shannonDiversity,
  formula = formula.shannonDiversity,
  value.var = "value",
  variable.factor = FALSE)

# budget share data -----
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
dt.budgetShare <- dt.budgetShare[year %in% c("X2010",  "X2050") &
                                   scenario %in% scenarioList,]
deleteListCol <- c("pcGDPX0", "budget.PWX0", "budget.PCX0", "incSharePWX0")
dt.budgetShare <- dt.budgetShare[, (deleteListCol) := NULL]
data.table::setnames(dt.budgetShare, old = "incSharePCX0", new = "value")
dt.budgetShare[, value := 100 * value]

deleteListNames <- c("scenario", "region_code.IMPACT159", "year", "IMPACT_code")
dt.IMPACTfood[,list(sapply(.SD, min, na.rm = TRUE, USE.NAMES = TRUE), lapply(.SD, mean, na.rm = TRUE, USE.NAMES = TRUE), lapply(.SD, max, na.rm = TRUE, USE.NAMES = TRUE)),
              .SDcols = c(names(dt.IMPACTfood)[!names(dt.IMPACTfood) %in% deleteListNames])]

# macro adequacy data ----
RDA.macro.sum.req.ratio <- getNewestVersion("RDA.macro.sum.req.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "X2010","X2050")
RDA.macro.sum.req.ratio <- RDA.macro.sum.req.ratio[, keepListCol, with = FALSE]
RDA.macro.sum.req.ratio <- RDA.macro.sum.req.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
measureVars <- c("X2010","X2050")
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

# minerals adequacy data ----
RDA.minrls.sum.req.ratio <- getNewestVersion("RDA.minrls.sum.req.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "X2010", "X2050")
RDA.minrls.sum.req.ratio <- RDA.minrls.sum.req.ratio[, keepListCol, with = FALSE]
RDA.minrls.sum.req.ratio <- RDA.minrls.sum.req.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
measureVars <- c("X2010","X2050")

RDA.minrls.sum.req.ratio.melt <- data.table::melt(RDA.minrls.sum.req.ratio,
                                                  id.vars = idVars,
                                                  variable.name = "year",
                                                  measure.vars = measureVars,
                                                  value.name = "value",
                                                  variable.factor = FALSE)

# vitamin adequacy data -----
RDA.vits.sum.req.ratio <- getNewestVersion("RDA.vits.sum.req.ratio", fileloc("resultsDir"))
keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "X2010", "X2050")
RDA.vits.sum.req.ratio <- RDA.vits.sum.req.ratio[, keepListCol, with = FALSE]
RDA.vits.sum.req.ratio <- RDA.vits.sum.req.ratio[scenario %in% scenarioList,]

idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
measureVars <- c( "X2010", "X2050")
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

# nonstaples share of kcals data -----
dt.stapleShare <- getNewestVersion("dt.nutrients.sum.staples", fileloc("resultsDir"))
dt.stapleShare <- dt.stapleShare[year %in% c("X2010",  "X2050") &
                                   scenario %in% scenarioList,]
keepListCol <- c("scenario", "region_code.IMPACT159", "staple_code", "year", "energy_kcal.sum.staple")
dt.stapleShare <- dt.stapleShare
# next line structure is because dt.stapleShare has staple code with nonstaple and staple.
# Shift goes back one row to get nonstaple in numerator
dt.stapleShare <- dt.stapleShare[,value := shift(energy_kcal.sum.staple) / (energy_kcal.sum.staple + shift(energy_kcal.sum.staple)),
                                 by = c("scenario","region_code.IMPACT159", "year")]
keepListCol <- c("scenario", "region_code.IMPACT159", "year", "value")
dt.stapleShare <- dt.stapleShare[,(keepListCol), with = FALSE]
dt.stapleShare <- dt.stapleShare[!is.na(value),][, value := 100 * value]

#box plots vitamins requirement -----
temp <- copy(RDA.vits.sum.req.ratio.melt)
temp <- temp[,c("region_code.IMPACT159") := NULL][nutrient == "vit_e_mg_reqRatio",]
yrange <- range(temp$value)
pdf(paste("graphics/vit e", ".pdf", sep = ""))
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

# PWX growth rate calcs ----
dt.PWX <- data.table::copy(dt.IMPACTfood)
dt.PWX <- dt.PWX[,c("pcGDPX0","FoodAvailability","region_code.IMPACT159", "PCX0","CSE") :=  NULL]
dt.PWX <- unique(dt.PWX)
dt.PWX <- dt.PWX[year %in% c("X2010","X2050"), ]
dt.PWX <- dt.PWX[!PWX0 ==  0,] # gets rid of fish and alcohol that don't have prices
dt.PWX <- dt.PWX[,growthRate :=  lapply(.SD, function(x)((x/shift(x))^(1/(2050 - 2010)) - 1) * 100),
                 .SDcols = "PWX0", by = c("scenario", "IMPACT_code")]
dt.PWX <- dt.PWX[year ==  "X2050",][,year :=  NULL]
dt.foodGroupLU <- getNewestVersion("dt.foodGroupsInfo")
temp <- merge(dt.PWX, dt.foodGroupLU, by = "IMPACT_code")
# food group price calcs -----
keepListCol <- c("IMPACT_code","scenario", "PWX0", "growthRate", "food_group_code", "staple_code")
temp <- temp[,(keepListCol), with = FALSE ]
temp.FGmean <- temp[, FGmean := mean(growthRate), by = c("scenario","food_group_code")]
temp.FGmean <- unique(temp.FGmean[,c("scenario", "food_group_code","FGmean"), with = FALSE])
formula.FG.wide <- paste("scenario  ~ food_group_code")
temp.FGmean.wide <- data.table::dcast(data = temp.FGmean, formula = formula.FG.wide, value.var = "FGmean")
temp <- copy(temp.FGmean.wide)
temp[,scenario := gsub("-REF", "", scenario)]
write.csv(format(temp, digits = 2), paste(fileloc("resultsDir"),"priceGrowthFG.csv", sep = "/"))
# staple group price calcs
temp <- merge(dt.PWX, dt.foodGroupLU, by = "IMPACT_code")
keepListCol <- c("IMPACT_code","scenario", "PWX0", "growthRate", "food_group_code", "staple_code")
temp <- temp[,(keepListCol), with = FALSE ]
temp.stapleMean <- temp[, stapleMean := mean(growthRate), by = c("scenario","staple_code")]
temp.stapleMean <- unique(temp.stapleMean[,c("scenario", "staple_code","stapleMean"), with = FALSE])
formula.staple.wide <- paste("scenario  ~ staple_code")
temp.stapleMean.wide <- data.table::dcast(data = temp.stapleMean, formula = formula.staple.wide, value.var = "stapleMean")
temp <- copy(temp.stapleMean.wide)
temp[,scenario := gsub("-REF", "", scenario)]
write.csv(format(temp, digits = 2), paste(fileloc("resultsDir"),"priceGrowthStaples.csv", sep = "/"))

# plot nutrient ratios by country and scenario
plotNutrients <- function(dt, fileName, regions) {
  if (missing(regions)) {regions <- 156}
  temp <- copy(dt)
  pdf(paste("graphics/", fileName, ".pdf", sep = ""))

  scenarios <- unique(temp$scenario)
  nutrients <- unique(temp$nutrient)
  colList <- c("red", "red2", "red4", "green", "green2", "green4", "black")
  par(mfrow = c(round(length(nutrients)/2),2))
  par(mar = c(1,1,1,1) + 0.1)
  for (j in 1:length(nutrients)) {
    legendText <- NULL
    #  print(nutrients[j])
    for (i in 1:length(scenarios)) {
      #     temp.scen <- data.table::copy(temp)
      #     temp.scen <- temp.scen[nutrient == nutrients[j] & scenario == scenarios[i],]
      #     temp <- temp[,c("nutrient", "scenario") := NULL]
      #     temp <- merge(temp, dt.GDP.2010.ref, by = "region_code.IMPACT159")
      nutShortname <- cleanupNutrientNames(nutrients[j])
      nutShortname
      if (i == 1) {
        plot(temp[year %in% "X2050" & scenario == scenarios[i] & nutrient == nutrients[j],value],
             type = "l", col = "green", xlab = "", xaxt = "n",
             main = nutShortname,cex.main = 1, ylim = c(0, 4)) # common range for requirements share
        #      main = nutShortname,cex.main=1, ylim = c(0, round(max(temp$value))))
        par(new = T)
      } else {
        lines(temp[year %in% "X2050" & scenario == scenarios[i] & nutrient == nutrients[j],value],
              col = colList[i])
        par(new = F)
      }
      legendText <- c(legendText,scenarios[i])
    }
    lines(temp[year %in% "X2010" & scenario %in% scenario.base & nutrient == nutrients[j], value],
          col = "black")
    # print(legendText)
    axis(1, at = 1:regions, labels = dt.GDP.2010.ref$region_code.IMPACT159, cex.axis = 0.5, padj = -3)
    abline(h = 1, lty = 3, lwd = 0.8)
    legendText <- gsub("-REF", "", legendText)
    legendText <- c(legendText, "2010")
    legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
           col = colList, text.col = "black", cex = .5, pt.cex = .5, pt.lwd = 1,
           y.intersp = .8)
  }
  dev.off()
}

plotNutrients(RDA.minrls.sum.req.ratio.melt, "minrls")
plotNutrients(RDA.macro.sum.req.ratio.melt, "macroNutrients")
plotNutrients(RDA.vits.sum.req.ratio.melt, "vitamins")

#box plots budget share -----
boxPlotSingle <- function(dt, fileName, titleText, yRange) {
  temp <- copy(dt)
  temp <- temp[!region_code.IMPACT159 ==  "SOM",]
  colList <- c("white smoke", "red", "red2", "red4", "green", "green2", "green4")

  par(mfrow = c(1,1))
  pdf(paste("graphics/", fileName, ".pdf", sep = ""))
  box.test <- boxplot(value ~ scenario, data = temp, range = 5,
                      at = c(1, 2, 2.8, 3.6, 4.6, 5.4, 6.2),
                      xaxt = 'n',
                      ylim = yRange,
                      col = colList)
  labels <- gsub("-REF", "", unique(temp$scenario))
  axis(side = 1,labels = FALSE)
  title(main = titleText)
  text(c(1, 2, 2.8, 3.6, 4.6, 5.4, 6.2), par("usr")[3] - 2, srt = 45, adj = 1,
       labels = labels, xpd = TRUE)
  mytable <- box.test$stats
  colnames(mytable) <- box.test$names
  rownames(mytable) <- c('min','lower quartile','median','upper quartile','max')
  # need to figure out how to put this table below the box plot
  dev.off()
}
boxPlotSingle(dt  = dt.budgetShare, fileName = "budgetShareBoxes", titleText = "Share of IMPACT food items cost in per capita GDP, \n2010 and 2050 scenarios", yRange = c(0, 50))
boxPlotSingle(dt  = dt.shannonDiversity, fileName = "ShannonDivBoxes", titleText = "Shannon diversity index by scenario", yRange = c(0, 80))
boxPlotSingle(dt  = dt.stapleShare, fileName = "nonStapleBoxes", titleText = "Non-staple share by scenario", yRange = c(0, 80))

#box plots nutrients -----
temp <- copy(dt.nutrients.sum.melt[,c("region_code.IMPACT159","pcGDPX0") := NULL][nutrient == "energy_kcal.sum.all",])
yrange <- range(temp$value)
par(mfrow = c(1,1))
pdf("graphics/nutrientsBoxes.pdf")
box.test <- boxplot(value ~ scenario, data = temp, range = 1.5,
                    at = c(1, 2.5, 4, 5.5, 7, 8.5),
                    xaxt = 'n',
                    ylim = yrange,
                    col = c('white', 'white smoke', 'gray'))
axis(side = 1, at = c(1, 2.5, 4, 5.5, 7, 8.5), labels = FALSE)
labels <- gsub("-REF","", unique(temp$scenario))
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Per capita energy availability by scenario')
text(c(1, 2.5, 4, 5.5, 7, 8.5), par("usr")[3] - 100, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
# add scenario labels near the box plots
#text(c(1, 2.5, 4, 5.5, 7, 8.5), c(1, 2.5, 4, 5.5, 7, 8.5), unique(temp$scenario), srt = 45)
dev.off()

#box plots Shannon diversity -----
temp <- copy(dt.shannonDiversity[,c("region_code.IMPACT159","year") := NULL])
yrange <- range(temp$SDnorm)
fileName <- 'ShannonDiversityBoxPlots'
pdf(paste("graphics/", fileName, ".pdf", sep = ""))
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

plotByRegionLine <- function(dt, fileName, title, yRange, regionCodes) {
  temp <- copy(dt)
  temp <- merge(temp, dt.GDP.2010.ref, by = "region_code.IMPACT159")
  temp <- temp[region_code.IMPACT159 %in% regionCodes]
 scenarios <- unique(temp$scenario)
  pdf(paste("graphics/", fileName, ".pdf", sep = ""))
  colList <- c("red", "red2", "red4", "green", "green2", "green4", "black")
  par(mfrow = c(1,1))
  legendText <- NULL
  for (i in 1:length(scenarios)) {
    if (i == 1) {
      plot(temp[year %in% "X2050" & scenario == scenarios[i],value], type = "l", col = "green",
           xlab = "", xaxt = "n", ylab = "share (%)",
           main = title, cex.main = 1, ylim = yRange) # common range for requirements share
      #      main = nutShortname,cex.main=1, ylim = c(0, round(max(scen.temp$value))))
      par(new = T)
    } else {
      lines(temp[year %in% "X2050" & scenario == scenarios[i],value], col = colList[i])
      par(new = F)
    }
    legendText <- c(legendText,scenarios[i])
  }
  lines(temp[year %in% "X2010" & scenario %in% scenario.base, value], col = "black")
  legendText <- c(legendText, "2010")
  # print(legendText)
  axis(1, at = 1:length(unique(dt.GDP.2010.ref$region_code.IMPACT159)), labels = unique(dt.GDP.2010.ref$region_code.IMPACT159), cex.axis = 0.5, padj = -3)
  abline(h = 1, lty = 3, lwd = 0.8)
  legendText <- gsub("-REF", "", legendText)
  legend(x = "topright", y = NULL, legend = legendText, bty = "n", pch = 20,
         col = colList, text.col = "black", cex = .5, pt.cex = .5, pt.lwd = 1,
         y.intersp = .8)
  dev.off()
}
#
# plotByRegionBar <- function(dt, fileName, title, yLab, yRange, regionList) {
#   temp <- copy(dt)
#   regionCodes <- regionList[, region_code]
#   regionNames <- regionList[, region_name]
#   regionNames <- gsub(" plus", "", regionNames)
#   temp <- temp[region_code %in% regionCodes]
#   colList <- c("black", "red", "red2", "red4", "green", "green2", "green4")
#   # par(mfrow = c(1,1))
#   # for (i in regionCodes) {
#   legendText <- unique(gsub("-REF", "", temp$scenario))
#   temp <- temp[year == "X2010" & scenario == scenario.base |
#                  year == "X2050" ,][year == "X2010", scenario := "2010"]
#   scenarios <- unique(temp$scenario)
#   pdf(paste("graphics/", fileName, ".pdf", sep = ""))
#   formula.wide <- "scenario + year ~ region_code"
#   temp.wide <- data.table::dcast(
#     data = temp,
#     formula = formula.wide,
#     value.var = "value")
#   temp.wide[, year := NULL]
#    data.table::setnames(temp.wide, old = regionCodes, new = regionNames)
#   regionNames <- sort(regionNames)
# #  data.table::setcolorder(temp.wide, c("scenario", regionNames))
#   temp.wide <- as.data.frame(temp.wide)
#   rownames(temp.wide) <-  temp.wide[,1]
#   temp.wide[1] = NULL
#   temp.wide <- data.matrix(temp.wide)
#  # print(temp.wide)
#   barplot(temp.wide,  col = colList, ylim = yRange,
#                legend = gsub("-REF", "", rownames(temp.wide)), args.legend = list(cex = .7),
#                beside = TRUE, ylab = yLab,  cex.names = .7, las = 2,  main = title)
#   dev.off()
# }

# plot Shannon Diversity by country and scenario -----
plotByRegionLine(dt.shannonDiversity, "ShannonDiversity", "Shannon Diversity", yRange = c(20, 80), regionCodes156)
plotByRegionBar(dt.shannonDiversity, "ShannonDiversity", "Shannon Diversity", "percent", yRange = c(0, 80), dt.region11List)
# plot budget shares by country and scenario -----
plotByRegionLine(dt.budgetShare, "budgetShare", "Income share of IMPACT food budget, 2010 and 2050 scenarios", yRange = c(0, 50), regionCodes156)
plotByRegionBar(dt.budgetShare, "budgetShare", "Income share of IMPACT food budget, 2010 and 2050 scenarios", "percent", yRange = c(0, 50), dt.region11List)
# plot nonstaple shares by country and scenario -----
plotByRegionLine(dt.stapleShare, "stapleShare", title = "Non-staple share of energy availability, 2010 and 2050 scenarios", yRange = c(0, 100), regionCodes156)
temp <- data.table::copy(dt.stapleShare)
data.table::setnames(temp, old = c("region_code.IMPACT159", "region_name.IMPACT159"), new = c("region_code", "region_name"))
plotByRegionBar(temp, "stapleShare", "Nonstaple share of energy", "percent", yRange = c(0, 100), dt.region11List)

# plot nutrient shares by country and scenario
plotByRegionLine(RDA.minrls.sum.req.ratio.melt, "minrls", "Minerals availablity share of requirement", yRange = c(0, 4), regionCodes156)

# aggregation code -----
# temp <- merge(dt.budgetShare,dt.region_codes.WB, by = "region_code.IMPACT159")
# temp <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
# #temp <- temp[, region.budget.share := mean(value), by = c("region_code.AggReg2", "year")]
# temp <- temp[, value := weighted.mean(value, PopX0), by = c("scenario","region_code", "year")]
# keepListCol <- c("scenario", "year", "region_code", "region_name", "value")
# temp <- unique(temp[, (keepListCol), with = FALSE])
# # data.table::setnames(temp, old = c("region_code.AggReg2", "region_name.AggReg2", "region.budget.share.w"),
# #                      new = c("region_code", "region_name", "value"))
# dt.regionList <- unique(dt.region_codes.WB[, region_code.IMPACT159 := NULL])
# data.table::setkey(dt.regionList, region_name)
# plotByRegionBar(temp, "budgetShareWB", "Income share of IMPACT food budget, 2010 and 2050 scenarios", "percent", yRange = c(0, 70), dt.regionList)

plotAggRegionBar <- function(dataDT, regionCodesDT, fileName, titleText, ylab, yRange, aggType) {
  # tbls <- list(dataDT,regionCodesDT,dt.pop.2010.ref)
  # lapply(tbls, function(i) setkey(i, "region_code.IMPACT159"))
  # merged <- Reduce(function(...) merge(..., all = T), tbls)
  temp <- merge(dataDT,regionCodesDT, by = "region_code.IMPACT159")
  merged <- merge(temp, dt.pop.2010.ref, by = "region_code.IMPACT159")
#  temp <- temp[, region.budget.share := mean(value), by = c("region_code", "year")]
  merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
  keepListCol <- c("scenario",  "year", "region_code", "region_name", "value")
  merged <- unique(merged[, (keepListCol), with = FALSE])
  dt.regionList <- data.table::copy(regionCodesDT)
  dt.regionList <- unique(dt.regionList[, region_code.IMPACT159 := NULL])
  if (aggType == "WB") {
     merged[region_code == "lowInc" ,newOrder := 1] [region_code == "lowMidInc" ,newOrder := 2][region_code == "upMidInc" ,newOrder := 3]
   merged[region_code == "highInc" ,newOrder := 4]
  dt.regionList[region_code == "lowInc" ,newOrder := 1] [region_code == "lowMidInc" ,newOrder := 2][region_code == "upMidInc" ,newOrder := 3]
  dt.regionList[region_code == "highInc" ,newOrder := 4]
  data.table::setorder(dt.regionList,newOrder)
  dt.regionList[, newOrder := NULL]
  } else {
  data.table::setkey(dt.regionList, region_name)
  }
  #print(names(merged))
  plotByRegionBar(merged, fileName, titleText, "percent", yRange, dt.regionList)
  #print(dt.regionList)
}

plotAggRegionBar(dataDT = dt.budgetShare, regionCodesDT = dt.region_codes.WB, fileName =  "budgetShareWB",
                 titleText = "Income share of IMPACT food budget, 2010 and 2050 scenarios",
                 ylab = "percent", yRange = c(0, 70), aggType = "WB")
plotAggRegionBar(dataDT = dt.stapleShare, regionCodesDT = dt.region_codes.WB, fileName =  "stapleShareWB",
                 titleText = "Non-staple shares of energy availability, 2010 and 2050 scenarios",
                 ylab = "percent", yRange = c(0, 70), aggType = "WB")
plotAggRegionBar(dataDT = dt.shannonDiversity, regionCodesDT = dt.region_codes.WB, fileName =  "ShannonDiversityWB",
                 titleText = "Shannon Diversity Index, 2010 and 2050 scenarios",
                 ylab = "percent", yRange = c(0, 70), aggType = "WB")
# old code ------
# #box plots nutrients
# temp <- dt.nutrients.sum.melt[,c("region_code.IMPACT159","pcGDPX0") := NULL][nutrient == "energy_kcal.sum.all",]
# yrange <- range(temp$value)
# pdf(paste("graphics/", fileName, ".pdf", sep = ""))
# box.test <- boxplot(value ~ scenario, data = temp, range = 1.5,
#                     at = c(1, 2.5, 4, 5.5, 7, 8.5),
#                     xaxt = 'n',
#                     ylim = yrange,
#                     col = c('white', 'white smoke', 'gray'))
# axis(side = 1, at = c(1, 2.5, 4, 5.5, 7, 8.5), labels = FALSE)
# labels <- gsub("-REF","", unique(temp$scenario))
# #    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
# #   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
# title('Per Capita Energy Availability by Scenario')
# text(c(1, 2.5, 4, 5.5, 7, 8.5), par("usr")[3] - 100, srt = 45, adj = 1,
#      labels = labels, xpd = TRUE)
# # add scenario labels near the box plots
# #text(c(1, 2.5, 4, 5.5, 7, 8.5), c(1, 2.5, 4, 5.5, 7, 8.5), unique(temp$scenario), srt = 45)
# dev.off()

# old FG code -----
# macro food group availability
# RDA.macro.FG.ratio <- getNewestVersion("RDA.macro.FG.ratio", fileloc("resultsDir"))
# keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code", "X2050")
# RDA.macro.FG.ratio <- RDA.macro.FG.ratio[, keepListCol, with = FALSE]
# RDA.macro.FG.ratio <- RDA.macro.FG.ratio[scenario %in% scenarioList,]
#
# idVars <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code")
# measureVars <- "X2050"
# RDA.macro.FG.ratio.melt <- data.table::melt(RDA.macro.FG.ratio,
#                                             id.vars = idVars,
#                                             variable.name = "year",
#                                             measure.vars = measureVars,
#                                             value.name = "value",
#                                             variable.factor = FALSE)
#
# formula.macro.FG <- paste("region_code.IMPACT159 + year + nutrient +food_group_code ~ scenario")
# RDA.macro.FG.ratio.wide <- data.table::dcast(
#   data = RDA.macro.FG.ratio.melt,
#   formula = formula.macro.FG,
#   value.var = "value",
#   variable.factor = FALSE)
#
# RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.HGEM :=
#                                                      100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.IPSL :=
#                                                      100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.GFDL :=
#                                                      100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.SSP3 :=
#                                                      100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.macro.FG.ratio.wide <- RDA.macro.FG.ratio.wide[, delta.SSP1 :=
#                                                      100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.macro.FG.ratio.wide[,(deleteListCol) :=  NULL]
#
# # minerals food group availability
# RDA.minrls.FG.ratio <- getNewestVersion("RDA.minrls.FG.ratio", fileloc("resultsDir"))
# keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code", "X2050")
# RDA.minrls.FG.ratio <- RDA.minrls.FG.ratio[, keepListCol, with = FALSE]
# RDA.minrls.FG.ratio <- RDA.minrls.FG.ratio[scenario %in% scenarioList,]
#
# idVars <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code")
# measureVars <- "X2050"
# RDA.minrls.FG.ratio.melt <- data.table::melt(RDA.minrls.FG.ratio,
#                                              id.vars = idVars,
#                                              variable.name = "year",
#                                              measure.vars = measureVars,
#                                              value.name = "value",
#                                              variable.factor = FALSE)
#
# formula.minrls.FG <- paste("region_code.IMPACT159 + year + nutrient +food_group_code ~ scenario")
# RDA.minrls.FG.ratio.wide <- data.table::dcast(
#   data = RDA.minrls.FG.ratio.melt,
#   formula = formula.minrls.FG,
#   value.var = "value",
#   variable.factor = FALSE)
#
# RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.HGEM :=
#                                                        100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.IPSL :=
#                                                        100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.GFDL :=
#                                                        100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.SSP3 :=
#                                                        100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.minrls.FG.ratio.wide <- RDA.minrls.FG.ratio.wide[, delta.SSP1 :=
#                                                        100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.minrls.FG.ratio.wide[,(deleteListCol) :=  NULL]
#
# # vitamins food group availability
# RDA.vits.FG.ratio <- getNewestVersion("RDA.vits.FG.ratio", fileloc("resultsDir"))
# keepListCol <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code", "X2050")
# RDA.vits.FG.ratio <- RDA.vits.FG.ratio[, keepListCol, with = FALSE]
# RDA.vits.FG.ratio <- RDA.vits.FG.ratio[scenario %in% scenarioList,]
#
# idVars <- c("scenario", "region_code.IMPACT159", "nutrient", "food_group_code")
# measureVars <- "X2050"
# RDA.vits.FG.ratio.melt <- data.table::melt(RDA.vits.FG.ratio,
#                                            id.vars = idVars,
#                                            variable.name = "year",
#                                            measure.vars = measureVars,
#                                            value.name = "value",
#                                            variable.factor = FALSE)
#
# formula.vits.FG <- paste("region_code.IMPACT159 + year + nutrient +food_group_code ~ scenario")
# RDA.vits.FG.ratio.wide <- data.table::dcast(
#   data = RDA.vits.FG.ratio.melt,
#   formula = formula.vits.FG,
#   value.var = "value",
#   variable.factor = FALSE)
#
# RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.HGEM :=
#                                                    100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.IPSL :=
#                                                    100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.GFDL :=
#                                                    100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.SSP3 :=
#                                                    100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.vits.FG.ratio.wide <- RDA.vits.FG.ratio.wide[, delta.SSP1 :=
#                                                    100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# RDA.vits.FG.ratio.wide[,(deleteListCol) :=  NULL]
# more old delta code -----

# delta climate model effects on nutrients
# dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.HGEM :=
#                                                  100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.IPSL :=
#                                                  100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.GFDL :=
#                                                  100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# # delta SSP effects on nutrients
# dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.SSP3 :=
#                                                  100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.nutrients.sum.wide <- dt.nutrients.sum.wide[, delta.SSP1 :=
#                                                  100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.nutrients.sum.wide[,(deleteListCol) :=  NULL]

# delta climate model effects on Shannon diversity
# dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.HGEM :=
#                                                        100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.IPSL :=
#                                                        100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.GFDL :=
#                                                        100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# # delta SSP effects on Shannon diversity
# dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.SSP3 :=
#                                                        100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.shannonDiversity.wide <- dt.shannonDiversity.wide[, delta.SSP1 :=
#                                                        100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.shannonDiversity.wide[,(deleteListCol) :=  NULL]
# # delta climate model effects on budget share
# dt.budgetShare.wide <- dt.budgetShare.wide[, delta.HGEM :=
#                                              100 * (`SSP2-HGEM-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.budgetShare.wide <- dt.budgetShare.wide[, delta.IPSL :=
#                                              100 * (`SSP2-IPSL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.budgetShare.wide <- dt.budgetShare.wide[, delta.GFDL :=
#                                              100 * (`SSP2-GFDL-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# # delta SSP effects on budget share
# dt.budgetShare.wide <- dt.budgetShare.wide[, delta.SSP3 :=
#                                              100 * (`SSP3-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.budgetShare.wide <- dt.budgetShare.wide[, delta.SSP1 :=
#                                              100 * (`SSP1-NoCC-REF` - `SSP2-NoCC-REF`)/`SSP2-NoCC-REF`]
# dt.budgetShare.wide[,(deleteListCol) :=  NULL]

# deltaPlot <- function(dt, var, title, ylabel, regions) {
#   if (missing(regions)) {regions <- 156}
#   dt.temp <- data.table::copy(dt)
#   data.table::setorderv(dt.temp, var)
#   junk <- dt.temp[,{
#     plot(x = get(var), type = "s", main = title,
#          ylab = ylabel,
#          xlab = "",
#          xaxt = "n",
#          cex.lab = 0.8, mgp = c(2.5, 1, 0),
#          yaxs = "r")
#     abline(h = 0)
#     axis(1, at = 1:regions, labels = region_code.IMPACT159, cex.axis = 0.5)}
#     ]
# }
#
# dt.temp <- data.table::copy(dt.budgetShare.wide)
# data.table::setorderv(dt.temp, var)
# junk <- dt.temp[,{
#   plot(x = get(var), type = "s", main = title,
#        ylab = ylabel,
#        xlab = "",
#        xaxt = "n",
#        cex.lab = 0.8, mgp = c(2.5, 1, 0),
#        yaxs = "r")
#   abline(h = 0)
#   axis(1, at = 1:156, labels = region_code.IMPACT159, cex.axis = 0.5)}
#   ]
#
#
# par(mfrow = c(1, 1))
# yrange <- range(dt.budgetShare.wide$delta.SSP3, dt.budgetShare.wide$delta.SSP1)
# data.table::setorder(dt.budgetShare.wide, delta.SSP1)
# plot(dt.budgetShare.wide$delta.SSP1, type = "s", col = "red", ylim = yrange,
#      ylab = "Change in IMPACT commodity\nshare of per capita GDP (%)",
#      xlab = "",
#      xaxt = "n",
#      cex.lab = 0.8, mgp = c(2.5, 1, 0),
#      yaxs = "r")
# lines(dt.budgetShare.wide$delta.SSP3, col = "green")
# axis(1, at = 1:156, labels = dt.budgetShare.wide$region_code.IMPACT159, cex.axis = 0.5)
# legendText <- c("Delta SSP1", "Delta SSP3")
# colors_border <- c(  "black", rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9) )
# colors_in <- c( "black", rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4) )
#
# legend(x = "bottomright", y = NULL, legend = legendText, bty = "n", pch = 20,
#        col = colors_in, text.col = "black", cex = .9, pt.cex = .9, pt.lwd = 1,
#        y.intersp = .9)
#
# deltaPlot(dt = dt.budgetShare.wide,
#           var = "delta.SSP1", title = "Delta SSP1",
#           ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
# )

# deltaPlot(dt = dt.budgetShare.wide,
#           var = "delta.SSP3",title = "Delta SSP3",
#           ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
# )
#
# deltaPlot(dt = dt.budgetShare.wide,
#           var = "delta.HGEM", title = "Delta HGEM",
#           ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
# )
# deltaPlot(dt = dt.budgetShare.wide,
#           var = "delta.GFDL", title = "Delta GFDL",
#           ylabel = "Change in IMPACT commodity\nshare of per capita GDP (%)"
# )

# remove SOM because it's expenditure is greater than its per cap income
# temp <- data.table::copy(dt.budgetShare.wide)
# temp <- temp[!region_code.IMPACT159 ==  "SOM",][, `SSP2-NoCC-REF` :=  `SSP2-NoCC-REF` * 100]
# deltaPlot(dt = temp,
#           var = "SSP2-NoCC-REF",
#           title = "Reference scenario:\nSSP2 with no climate change",
#           ylabel = "IMPACT commodity\nshare of per capita GDP (%)",
#           regions = 155
# )
#box plots, deltas
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
