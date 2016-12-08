#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Calculate nutrient deltas across scenarios
#' @name aggRun.R
#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
  source("R/aggNorder.R")}
library(data.table)
# gdxChoice values are either SSPs or USAID
# DTglobal choices are
# with one output
# - dt.budgetShare, dt.shannonDiversity
# with multiple nutrients
# - dt.nutrients.sum.all, RDA_macro.sum.reqRatio, RDA_minrls.sum.reqRatio, RDA_vits.sum.reqRatio
# - dt.nutrients.nonstapleShare, dt.foodGroupsInfo, dt.energy.ratios
# aggChoices are I3regions, tenregions, AggReg1, AggReg2, twoEconGroup, WB
dt.metadata <- getNewestVersion("dt.metadata", fileloc("resultsDir"))
gdxFileName <- dt.metadata[file_description %in% "IMPACT demand data in gdx form", file_name_location]
aggChoiceListBarChart <- c("tenregions", "WB", "AggReg1") # missing AggReg2 and  "2EconGroup
multipleNutsFileList <- c("dt.nutrients.sum.all",
                          "RDA.macro_sum_reqRatio",
                          "RDA.minrls_sum_reqRatio",
                          "RDA.vits_sum_reqRatio",
                          "dt.nutrients.nonstapleShare",
                          "PR.zinc_sum_reqRatio",
                          "PR.iron_sum_reqRatio") # "dt.energy.ratios" not included
multipleNutsListShortName <- c("nutrients.avail",
                               "macro_reqRatio",
                               "minrls_reqRatio",
                               "vits_reqRatio",
                               "nutrients.nonstaples.share",
                               "zinc_bioavail_reqRatio",
                               "iron_bioavail_reqRatio",
                               "foodAvail.foodGroup")
#nutrients grouping
macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")

# scenChoices for the USAID gdx are scenarioList.prodEnhance, scenarioList.waterMan, scenarioList.addEnhance, scenarioList.comp
# scenChoice for SSPs is scenOrder.SSPs
if (gdxFileName == "Micronutrient-Inputs-07252016.gdx") {
  gdxChoice <- "SSPs"
  scenario.base <- "SSP2-NoCC-REF"
  scenOrder <- c("2010", "SSP2-NoCC-REF", "SSP1-NoCC-REF", "SSP3-NoCC-REF", "SSP2-GFDL-REF",
                 "SSP2-IPSL-REF", "SSP2-HGEM-REF")
  scenChoiceList <- "scenOrder"
  scenChoice.name <- "SSP"
}

if (gdxFileName == "Micronutrient-Inputs-USAID.gdx") {
  gdxChoice <- "USAID"
  scenario.base <- "SSP2-NoCC-NA"
  # USAID scenario combo choices
  prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
  waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
  addEnhance <- c("RPHL", "RMM")
  comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")

  #USAID scenario list choice
  scenChoiceList <- c("prodEnhance", "waterMan", "addEnhance", "comp")
  #  scenChoice <- get(scenChoice.name)
}

#for testing
#aggChoice <- "WB"; DTglobal <- "dt.shannonDiversity"

# dt.pop.2010.ref <- dt.pop[year ==  "X2010" & scenario == scenario.base,][,c("scenario","year") :=  NULL]
dt.nutrientNames_Units <- getNewestVersion("dt.nutrientNames_Units", fileloc("mData"))

for (l in scenChoiceList) {
  for (i in aggChoiceListBarChart) {
    # Shannon Diversity -----
    print(paste("Working on bar chart for Shannon Diversity for", i))
    SD.out <- aggNorder(gdxChoice, DTglobal = "dt.shannonDiversity", aggChoice = i, get(l))
    plotByRegionBar(dt = SD.out, fileName = "ShannonDiversity", plotTitle = "Shannon Diversity", yLab = "(percent)", yRange = c(0, 80), aggChoice = i)

    print(paste("Working on bar chart for budget share for", i))
    budgetShare.out <- aggNorder(gdxChoice, DTglobal = "dt.budgetShare", aggChoice = i, get(l))
    plotByRegionBar(dt = budgetShare.out, fileName = "budgetShare", plotTitle = "IMPACT food budget share of per capita income",
                    yLab = "(percent)", yRange = c(0, 50), aggChoice = i)
    print(paste("Done with bar chart for budget share for", i))
    # MFAD -----
    print(paste("Working on bar chart for MFAD for", i))
    MFAD.out <- aggNorder(gdxChoice, DTglobal = "dt.MFAD", aggChoice = i, get(l))
    plotByRegionBar(dt = MFAD.out, fileName = "MFAD", plotTitle = "Modified Functional Attribute Diversity",
                    yLab = "(percent)", yRange = c(0, 120), aggChoice = i)
    print(paste("Done with bar chart for MFAD for", i))
    # RAOd -----
    print(paste("Working on bar chart for RaosD for", i))
    RaoD.out <- aggNorder(gdxChoice, DTglobal = "dt.RaoD", aggChoice = i, get(l))
    plotByRegionBar(dt = RaoD.out, fileName = "RaoD", plotTitle = "Rao's quadratic entropy",
                    yLab = "(percent)", yRange = c(0, 1), aggChoice = i)
    print(paste("Done with bar chart for Raos QE for", i))
    # food availability -----
    print(paste("Working on bar chart for food availability for", i))
    foodAvail.out <- aggNorder(gdxChoice, DTglobal = "dt.foodAvail.foodGroup", aggChoice = i, get(l))
    plotByRegionBar(dt = foodAvail.out, fileName = "foodAvail.foodGroup", plotTitle = "Food availability by food group",
                    yLab = "(grams)", yRange = c(0, 100), aggChoice = i)
    print(paste("Done with bar chart for food availability for", i))

    # # qualifying index -----
    # print(paste("Working on bar chart for qualifing index for", i))
    # foodAvail.out <- aggNorder(gdxChoice, DTglobal = "dt.qualIndex", aggChoice = i, get(l))
    # plotByRegionBar(dt = foodAvail.out, fileName = "qualIndex", plotTitle = "Qualifying Index",
    #                 yLab = "", yRange = c(0, 100), aggChoice = i)
    # print(paste("Done with bar chart for qualifying index for", i))
    #
    # # disqualifying index -----
    # print(paste("Working on bar chart for qualifing index for", i))
    # foodAvail.out <- aggNorder(gdxChoice, DTglobal = "dt.disqualIndex", aggChoice = i, get(l))
    # plotByRegionBar(dt = foodAvail.out, fileName = "disqualIndex", plotTitle = "Disqualifying Index",
    #                 yLab = "", yRange = c(0, 100), aggChoice = i)
    # print(paste("Done with bar chart for disqualifying index for", i))


  }

  # food groups -----
  print(paste("Working on dt.foodAvail.foodGroup"))
  temp.in <- getNewestVersion("dt.foodAvail.foodGroup", fileloc("resultsDir"))
  temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

  #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
  temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
                       year == "X2050",][year == "X2010", scenario := "2010"]
  for (i in aggChoiceListBarChart) {
    for (j in unique(temp.in$food_group_code)) {
      #     for (j in c(macroNutrients, vitamins, minerals)) {
      FG.shortName <- j
      FG.longName <- cleanupNutrientNames(j)
      units <- "g"
      DT <- data.table::copy(temp.in)
      if (units == "g") units <- "grams"
      DT <- DT[food_group_code %in% j,]
      DT[, food_group_code := NULL]
      dt.regions <- regionAgg(i)
      # aggregate to and retain only the relevant regions
      merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
      #      merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code")]
      keepListCol <- c("scenario", "region_code", "region_name", "value")
      DT <- unique(merged[, (keepListCol), with = FALSE])
      if (gdxChoice == "USAID") DT <- renameUSAIDscenarios(DT)

      DT <- DT[scenario %in% get(l), ]
      #     if (gdxChoice == "SSPs") {
      # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
      # DT <- DT[scenario %in% get(l), ]
      # DT[, scenarioOrder := match(scenario, scenOrder.SSPs)]
      # data.table::setorder(DT, scenarioOrder)
      # DT[, scenarioOrder := NULL]
      # #      }
      if (gdxChoice == "USAID")  {
        # scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
        # scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
        # scenarioList.addEnhance <- c("RPHL", "RMM")
        # scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
        #       scenario.base <- "REF_HGEM"

        # keep only needed USAID scenarios
        scenOrder <- c("2010", l)
        DT <- DT[scenario %in% get(l), ]
        scenChoice.name <- l
      }

      # order scenarios, first write the number into the number variable scenarioOrder
      DT[, scenarioOrder := match(scenario, scenOrder)]
      data.table::setorder(DT, scenarioOrder)
      DT[, scenarioOrder := NULL]

      DT <- orderRegions(DT, i)
      nutTitle <- paste("Average daily availability of ", tolower(FG.longName), sep = "")
      ylab = paste("(",units,")",sep = "")
      plotByRegionBar(dt = DT, fileName = paste(j, "foodAvail_foodGroup", sep = "_"), plotTitle = nutTitle, yLab = ylab, yRange = NULL, aggChoice = i)
      print(j)
    }
  }
  print(paste("Done with food groups"))
  print(" ", quote = FALSE)

  # several nutrients -----
  for (k in 1:length(multipleNutsFileList)) {
    print(paste("Working on multiple nut file", multipleNutsFileList[k]))
    temp.in <- getNewestVersion(multipleNutsFileList[k], fileloc("resultsDir"))
    temp.in <- merge(temp.in, dt.pop, by = c("scenario","region_code.IMPACT159", "year"))

    #keep just the scenario.base scenario for 2010 and rename the scenario to 2010
    temp.in <- temp.in[year == "X2010" & scenario == scenario.base |
                         year == "X2050",][year == "X2010", scenario := "2010"]
    for (i in aggChoiceListBarChart) {
      for (j in unique(temp.in$nutrient)) {
        #     for (j in c(macroNutrients, vitamins, minerals)) {
        nutshortName <- cleanupNutrientNames(j)
        # if (j %in% c("kcals.ethanol", "kcals.fat", "kcals.carbohydrate","kcals.protein", "kcals.sugar")) {
        #     nutlongName <- "energy"
        #     units <- "kilocalories"
        # } else {
        nutlongName <- dt.nutrientNames_Units[1, (j)]
        units <- dt.nutrientNames_Units[2, (j)]
        # }
        DT <- data.table::copy(temp.in)
        #      DT[, nutrient := cleanupNutrientNames(nutrient)]
        if (units == "g") units <- "grams"
        DT <- DT[nutrient %in% j,]
        DT[, nutrient := NULL]
        dt.regions <- regionAgg(i)
        # aggregate to and retain only the relevant regions; region code is the code for the region
        merged <- merge(DT, dt.regions, by = "region_code.IMPACT159")
        merged <- merged[, value := weighted.mean(value, PopX0), by = c("scenario", "region_code", "year")]
        keepListCol <- c("scenario", "region_code", "region_name", "value")
        DT <- unique(merged[, (keepListCol), with = FALSE])
        if (gdxChoice == "USAID") DT <- renameUSAIDscenarios(DT)

        DT <- DT[scenario %in% get(l), ]
        #     if (gdxChoice == "SSPs") {
        # do manipulations on the gdx data that has 3 SSP scenarios and 3 climate change scenarios.
        # DT <- DT[scenario %in% get(l), ]
        # DT[, scenarioOrder := match(scenario, scenOrder.SSPs)]
        # data.table::setorder(DT, scenarioOrder)
        # DT[, scenarioOrder := NULL]
        # #      }
        if (gdxChoice == "USAID")  {
          # scenarioList.prodEnhance <- c("MED", "HIGH", "HIGH_NARS", "HIGH_RE", "REGION")
          # scenarioList.waterMan <- c("IX", "IX_WUE", "ISW", "IX_WUE_NoCC", "IX_IPSL", "ISW_NoCC", "ISW_IPSL")
          # scenarioList.addEnhance <- c("RPHL", "RMM")
          # scenarioList.comp <- c("COMP", "COMP_NoCC", "COMP_IPSL")
          #       scenario.base <- "REF_HGEM"

          # keep only needed USAID scenarios
          scenOrder <- c("2010", l)
          DT <- DT[scenario %in% get(l), ]
          scenChoice.name <- l
        }

        # order scenarios, first write the number into the number variable scenarioOrder
        DT[, scenarioOrder := match(scenario, scenOrder)]
        data.table::setorder(DT, scenarioOrder)
        DT[, scenarioOrder := NULL]
        #     }

        DT <- orderRegions(DT, i)
        reqRatioFiles <- c( "RDA.macro_sum_reqRatio", "RDA.minrls_sum_reqRatio", "RDA.vits_sum_reqRatio",
                            "PR.zinc_sum_reqRatio", "PR.iron_sum_reqRatio")
        if (multipleNutsFileList[k] == "dt.nutrients.sum.all")  {
          nutTitle <- paste("Average daily availability of ", tolower(nutlongName), sep = "")
          ylab = paste("(",units,")",sep = "")
        }
        if (multipleNutsFileList[k] %in% reqRatioFiles)  {
          nutTitle <- paste("Adequacy ratio, ", nutshortName, sep = "")
          ylab = NULL
        }

        if (multipleNutsFileList[k] == "dt.nutrients.nonstapleShare")  {
          nutTitle <- paste("Non-staple share of ", nutshortName, " availability", sep = "")
          ylab = "(percent)"
        }
        plotByRegionBar(dt = DT, fileName = paste(j, multipleNutsListShortName[k], sep = "_"),
                        plotTitle = nutTitle, yLab = ylab, yRange = NULL, aggChoice = i)
      }
    }
    print(paste("Done with ", multipleNutsFileList[k]))
    print(" ", quote = FALSE)
  }
} # end of scenChoiceList loop

#this is not working right now
# plotByRegionLine("dt.shannonDiversity", "ShannonDiversity", "Shannon Diversity", yRange = c(20, 80), "I3regions")

# write out zip files of the data files in resultsDir
for (i in multipleNutsFileList) {
  print(paste("writing zip file for", i))
  inDT <- getNewestVersion(i, fileloc("resultsDir"))
  write.csv(inDT, file = gzfile(paste("graphics/",i,".csv.zip", sep = "")))
}

#create zip file of all the graphics outputs for a set of scenarios
graphs.fileNames <- list.files(path = "graphics")
zip(zipfile = paste(fileloc("gDir"),"/", scenChoice.name,".zip", sep = ""), files = paste(fileloc("gDir"),"/", graphs.fileNames, sep = ""))

# code to produce a table of req requirements results -----
# list of potential nutrients to add to the table
macroNutrients <- c("protein_g", "fat_g", "carbohydrate_g",  "totalfiber_g")
vitamins <- c("vit_c_mg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
              "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg",
              "vit_e_mg",  "vit_d_µg", "vit_k_µg")
minerals <- c("calcium_mg",  "iron_mg", "magnesium_mg", "phosphorus_mg",
              "potassium_g", "zinc_mg")
kcals <- c("kcals.fat", "kcals.protein", "kcals.sugar", "kcals.ethanol")
addedSugar <- c("sugar_g")
fattyAcids <- c("ft_acds_tot_sat_g", "ft_acds_mono_unsat_g", "ft_acds_plyunst_g",
                "ft_acds_tot_trans_g")

nutlistmacro <- c("totalfiber_g")
nutlistminrls <- c("calcium_mg", "potassium_g")
nutlistvits <- c("folate_µg", "riboflavin_mg", "vit_a_rae_µg","vit_b12_µg", "vit_e_mg",  "vit_d_µg", "vit_k_µg")
nutlistbioavail <- c("iron_mg_iron", "zinc_mg_zinc")

temp <- data.table::data.table(scenario = character(0),
                               lowInc = numeric(0), lowMidInc = numeric(0), upMidInc = numeric(0), highInc = numeric(0),
                               DlowInc = numeric(0), DlowMidInc = numeric(0), DupMidInc = numeric(0), DhighInc = numeric(0))
incCats <- c("lowInc", "lowMidInc", "upMidInc", "highInc")
DincCats <- c("DlowInc", "DlowMidInc", "DupMidInc", "DhighInc")
scen2050list <- c("SSP2-GFDL", "SSP2-IPSL", "SSP2-HGEM")
for (i in nutlistmacro) {
  fileName <- paste(i, "macro_reqRatio_WB", sep = "_")
  fileIn <- data.table::fread(paste("graphics/", fileName, ".csv", sep = ""), select = 2:6)
  for (j in scen2050list) {
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == "SSP2-NoCC", get(incCats[k])]
      fileIn[scenario == j, DincCats[k] := (get(incCats[k]) - baseVal)/baseVal]
    }
  }
  category <- data.table::data.table(scenario = i,
                                     lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
                                     DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  temp <- rbind(temp,category)
  temp <- rbind(temp, fileIn)
}

for (i in nutlistminrls) {
  fileName <- paste(i, "minrls_reqRatio_WB", sep = "_")
  fileIn <- data.table::fread(paste("graphics/", fileName, ".csv", sep = ""), select = 2:6)
  for (j in scen2050list) {
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == "SSP2-NoCC", get(incCats[k])]
      fileIn[scenario == j, DincCats[k] := (get(incCats[k]) - baseVal)/baseVal]
    }
  }
  category <- data.table::data.table(scenario = i,
                                     lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
                                     DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  temp <- rbind(temp,category)
  temp <- rbind(temp, fileIn)
}

for (i in nutlistbioavail) {
  fileName <- paste(i, "bioavail_reqRatio_WB", sep = "_")
  fileIn <- data.table::fread(paste("graphics/", fileName, ".csv", sep = ""), select = 2:6)
  for (j in scen2050list) {
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == "SSP2-NoCC", get(incCats[k])]
      fileIn[scenario == j, DincCats[k] := (get(incCats[k]) - baseVal)/baseVal]
    }
  }
  category <- data.table::data.table(scenario = i,
                                     lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
                                     DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  temp <- rbind(temp,category)
  temp <- rbind(temp, fileIn)
}

for (i in nutlistvits) {
  fileName <- paste(i, "vits_reqRatio_WB", sep = "_")
  fileIn <- data.table::fread(paste("graphics/", fileName, ".csv", sep = ""), select = 2:6)
  for (j in scen2050list) {
    for (k in 1:length(incCats)) {
      baseVal <- fileIn[scenario == "SSP2-NoCC", get(incCats[k])]
      fileIn[scenario == j, DincCats[k] := (get(incCats[k]) - baseVal)/baseVal]
    }
  }
  category <- data.table::data.table(scenario = i,
                                     lowInc = NA, lowMidInc = NA, upMidInc = NA, highInc = NA,
                                     DlowInc = NA, DlowMidInc = NA, DupMidInc = NA, DhighInc = NA)
  temp <- rbind(temp,category)
  temp <- rbind(temp, fileIn)
}

write.csv(temp, file = paste("graphics/reqTable.csv", sep = ""))

# box plots
dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("resultsDir"))
dt.regions <- regionAgg("WB")
# aggregate to and retain only the relevant regions
dt.budgetShare <- merge(dt.budgetShare, dt.regions, by = "region_code.IMPACT159")
keepListCol.incShare <- c("scenario","year", "region_code.IMPACT159", "region_code", "region_name", "incSharePCX0")
dt.budgetShare <- dt.budgetShare[, (keepListCol.incShare), with = FALSE]
scenario.base <- "SSP2-NoCC-REF"
dt.budgetShare <- dt.budgetShare[year == "X2010" & scenario == scenario.base |
                                   year == "X2050",]
dt.budgetShare <- dt.budgetShare[year == "X2010", scenario := "2010"][, year := NULL]
# get rid of Somalia because it's budget share is 500 * per cap income
dt.budgetShare <- dt.budgetShare[!region_code.IMPACT159 == "SOM",]
temp <- dt.budgetShare
WBregionLevels <- c("lowInc", "lowMidInc", "upMidInc", "highInc")
temp[,region_code := factor(region_code, levels = WBregionLevels)]
# sort by the region codes
temp <- temp[order(region_code),]
yrange <- range(temp$incSharePCX0)
pdf(paste("graphics/budgetShareBoxPlot", ".pdf", sep = ""))
box.test <- boxplot(incSharePCX0 ~ region_code, data = temp, range = 0,
                    #                    at = c(1, 2, 4, 5, 6),
                    xaxt = 'n',
                    ylim = yrange,
                    ylab = "(percent)",
                    col = c('white', 'white smoke', 'gray'))
axis(side = 1,
     at = c(1, 2, 3, 4),
     labels = FALSE)
labels <- gsub("-REF","", unique(temp$region_name))
#    labels = unique(temp$scenario), srt=45, adj=1, xpd=TRUE,
#   line = 0.5, lwd = 0, cex.lab = 0.5, cex.axis = 0.6)
title('Expenditures Share of Per Capita Income, 2050\nAll countries, all scenarios')
text(
  c(1, 2, 3, 4),
  par("usr")[3] - 0.1, srt = 45, adj = 1.2,
  labels = labels, xpd = TRUE,  cex = 0.6, cex.axis = 0.6)
# abline(h = 1, lty = 3, lwd = 0.8)
dev.off()

source("R/aggregateResults.R") # is this still necessary?
