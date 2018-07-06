#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Copyright (C) 2018 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.
#'
#' @name dataManagementVars.R
#' @description Replaces base crop varieties with country-specific varieties and does some graphing
#' @name dataPrep.SingleScenario.R

source("R/nutrientModFunctions.R")
#source("R/aggNorder.R") # is this needed? Commented out April 11, 2018
sourceFile <- "dataManagementVars.R" # would be better to figure out a way to get this automatically.
createScriptMetaData()

gdxSwitchCombo <- read.csv(file = paste0(getwd(), "/results/gdxInfo.csv"), header = TRUE, stringsAsFactors = FALSE)


#get the country crop variety lookup data table
library(readxl)
dt.countryCropVariety <- as.data.table(read_excel("data-raw/NutrientData/countryCropVariety.xlsx", na = "NA"))
#' delete country names in first row
dt <- dt.countryCropVariety[-1]
temp <- sapply(dt, function(x)all(is.na(x))) # find all columns that have no country-specific varieties
ctyWspecVarieties <- names(dt)[temp == FALSE]
# get rid of the column names "IMPACT_code" "usda_code"
ctyWspecVarieties <- ctyWspecVarieties[3:length(ctyWspecVarieties)]

dt.nutrients.sum.all.base <- getNewestVersion("dt.nutrients.sum.all.base", fileloc("resultsDir"))
dt.nutrients.sum.all.var <- getNewestVersion("dt.nutrients.sum.all.var", fileloc("resultsDir"))
if (gdxSwitchCombo[3] == 3) dt.nutrients.sum.all.varFort <- getNewestVersion("dt.nutrients.sum.all.varFort", fileloc("resultsDir"))

#' compare results with base and country-specific vars
dt.nutrients.sum.var <- dt.nutrients.sum.all.var[region_code.IMPACT159 %in% ctyWspecVarieties]
dt.nutrients.sum.base <- dt.nutrients.sum.all.base[region_code.IMPACT159 %in% ctyWspecVarieties]
setnames(dt.nutrients.sum.var, old = "value", new = "valueVar")
setnames(dt.nutrients.sum.base, old = "value", new = "valueBase")
temp <- merge(dt.nutrients.sum.var, dt.nutrients.sum.base)
# delete irrelevant nutrients
deleteListNuts <- c("kcalsPerDay_other", "ethanol_g", "kcalsPerDay_ethanol", "kcals.ethanol_g",
                    "kcalsPerDay_ft_acds_tot_sat", "kcalsPerDay_protein", "kcalsPerDay_sugar",
                    "kcals.protein_g", "kcals.sugar_g", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat",
                    "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g",
                    "kcals.carbohydrate_g", "kcals.fat_g", "kcals.ft_acds_tot_sat_g")
temp <- temp[!nutrient %in% deleteListNuts]

temp[, diff := valueVar - valueBase]
temp[, diffRatio := 100 * diff/valueBase]
temp.small <- temp[scenario %in% "SSP2-NoCC-REF" & year %in% "X2010",]

macronutrients <- keyVariable("macronutrients")
vitamins <- keyVariable("vitamins")
minerals <- keyVariable("minerals")

for (i in c("macronutrients", "vitamins", "minerals")) {
  dt <- temp.small[nutrient %in% eval(parse(text = i)),]
  p <- ggplot(data = dt, aes(x = region_code.IMPACT159, y = diffRatio, group = nutrient, color = nutrient)) +
    xlab("Country") +
    ylab("(percent)") +
    #  scale_y_continuous() +
    theme_bw() +
    #  ggtitle(sprintf("%s\n egg %s", gasinTitle, eggName)) +
    ggtitle("Difference between variety-specific and base results") +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))+
    theme(legend.title=element_blank())+
    geom_bar(aes(fill=nutrient),   # fill depends on cond2
             stat="identity",
             colour="black",    # Black outline for all
             position=position_dodge()) # Put bars side-by-side instead of stacked)
  #   print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
  print(p)
}

if (gdxSwitchCombo[3] == 3) {
  #' compare results with country-specific vars with and without fortification

  # get list of countries that have fortification
  dt.fortValues <- getNewestVersion("dt.fortValues", fileloc("uData"))
  ctyWFort <- sort(unique(dt.fortValues$region_code.IMPACT159))

  dt.nutrients.sum.var <- dt.nutrients.sum.all.varFort[region_code.IMPACT159 %in% ctyWFort]
  dt.nutrients.sum.base <- dt.nutrients.sum.all.var[region_code.IMPACT159 %in% ctyWFort]
  setnames(dt.nutrients.sum.var, old = "value", new = "valueVar")
  setnames(dt.nutrients.sum.base, old = "value", new = "valueBase")
  temp <- merge(dt.nutrients.sum.var, dt.nutrients.sum.base)

  # dt.nutrients.sumVar <- copy(dt.nutrients.sum.allVarFort)
  # setnames(dt.nutrients.sumVar, old = "value", new = "valueVar")
  #
  # dt.nutrients.sum.base <- copy(dt.nutrients.sum.allVar)
  # setnames(dt.nutrients.sum.base, old = "value", new = "valueBase")
  # temp <- merge(dt.nutrients.sumVar, dt.nutrients.sum.base)
  # # delete irrelevant nutrients
  # deleteListNuts <- c("kcalsPerDay_other", "ethanol_g", "kcalsPerDay_ethanol", "kcals.ethanol_g",
  #                     "kcalsPerDay_ft_acds_tot_sat", "kcalsPerDay_protein", "kcalsPerDay_sugar",
  #                     "kcals.protein_g", "kcals.sugar_g", "kcalsPerDay_carbohydrate", "kcalsPerDay_fat",
  #                     "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_sat_g",
  #                     "kcals.carbohydrate_g", "kcals.fat_g", "kcals.ft_acds_tot_sat_g")
  temp <- temp[!nutrient %in% deleteListNuts]

  temp[, diff := valueVar - valueBase]
  temp[, diffRatio := 100 * diff/valueBase]
  temp.small <- temp[scenario %in% "SSP2-NoCC-REF" & year %in% "X2010",]
  macronutrients <- keyVariable("macronutrients")
  vitamins <- keyVariable("vitamins")
  minerals <- keyVariable("minerals")

  for (i in c("macronutrients", "vitamins", "minerals")) {
    dt <- temp.small[nutrient %in% eval(parse(text = i)),]
    p <- ggplot(data = dt, aes(x = region_code.IMPACT159, y = diffRatio, group = nutrient, color = nutrient)) +
      xlab("Country") +
      ylab("(percent)") +
      #  scale_y_continuous() +
      theme_bw() +
      #  ggtitle(sprintf("%s\n egg %s", gasinTitle, eggName)) +
      ggtitle("Difference between fortified and unfortified results") +
      theme(plot.title = element_text(hjust = 0.5)) + # center title
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow = 3, byrow = TRUE))+
      theme(legend.title=element_blank())+
      geom_bar(aes(fill=nutrient),   # fill depends on cond2
               stat="identity",
               colour="black",    # Black outline for all
               position=position_dodge()) # Put bars side-by-side instead of stacked)
    #   print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
    print(p)
  }
}

finalizeScriptMetadata(metadataDT, sourceFile)
