#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
# Intro -------------------------------------------------------------------

# Copyright (C) 2015 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.  This program is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details at
# http://www.gnu.org/licenses/.  Contributors to the work include
# Brendan Power (for coding assistance), Joanne E. Arsenault (for
# constructing the nutrient requirements worksheet) and Joanne E.
# Arsenault, Malcolm Reilly, Jessica Bogard, and Keith Lividini (for
# nutrition expertise)

#' @description - This script calculates nutrient requirements for SSP age group categories
#' The source of the requirements is
#' @source \url{http://www.nal.usda.gov/fnic/DRI/DRI_Tables/recommended_intakes_individuals.pdf}
#' The requirements are imported in dataPrep.nutrientData.R and saved as dt.nutrients

#' @include nutrientModFunctions.R
#if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
options(encoding = "UTF-8")

# nutrients spreadsheet; otherwise the next line is fine
#' @param dt.nutrients - list of nutrient data generated in dataPrep.nutrientData.R
dt.nutrients <- getNewestVersion("dt.nutrients")

#' @param allFoodGroups - list of all food groups
allFoodGroups <- unique(dt.nutrients$food_group_code)

# Read in and set up the nutrient requirements data -----

# getSheetNames(DRIFileName)
#' @param req.metadata - meta data for the nutrient requirements
reqsFile <- fileNameList("DRIs")
req.metadata <- openxlsx::read.xlsx(reqsFile, sheet = "Reference", colNames = FALSE)
format(req.metadata, justify = c("left"))

#' @param req.EAR - Estimated Average Requirements (EAR) data
req.EAR <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "EAR"))
keepListCol.reqs <- names(req.EAR)[!names(req.EAR) %in% c("status_group", "age_group")]
req.EAR <- req.EAR[, (keepListCol.reqs), with = FALSE]
reqsList <- "req.EAR"

#' @param req.RDA.vits - Recommended Daily Allowance (RDA) data for vitamins
req.RDA.vits <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "RDAorAI_vitamins"))
keepListCol.reqs <- names(req.RDA.vits)[!names(req.RDA.vits) %in% c("status_group", "age_group")]
req.RDA.vits <- req.RDA.vits[, (keepListCol.reqs), with = FALSE]
reqsList <- c(reqsList,"req.RDA.vits")

#' @param req.RDA.minrls - Recommended Daily Allowance (RDA) data for minerals
req.RDA.minrls <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "RDAorAI_minerals"))
keepListCol.reqs <- names(req.RDA.minrls)[!names(req.RDA.minrls) %in% c("status_group", "age_group")]
req.RDA.minrls <- req.RDA.minrls[, (keepListCol.reqs), with = FALSE]

#remove the requirements for iron and zinc that are not based on physiological requirements
deleteListCols = c("iron_mg", "zinc_mg")
req.RDA.minrls[, (deleteListCols) := NULL]

# add PR based iron and zinc requirements to the reqs data
req.PR.iron <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "PR_iron"))
keepListCol.reqs <- names(req.PR.iron)[!names(req.PR.iron) %in% c("status_group", "age_group")]
req.PR.iron <- req.PR.iron[, (keepListCol.reqs), with = FALSE]
req.PR.zinc <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "PR_zinc"))
keepListCol.reqs <- names(req.PR.zinc)[!names(req.PR.zinc) %in% c("status_group", "age_group")]
req.PR.zinc <- req.PR.zinc[, (keepListCol.reqs), with = FALSE]
req.RDA.minrls <- merge(req.RDA.minrls, req.PR.iron, by = c("ageGenderCode"))
req.RDA.minrls <- merge(req.RDA.minrls, req.PR.zinc, by = c("ageGenderCode"))
#reqsList <- c(reqsList,"req.PR.iron", "req.PR.zinc"). Not needed because these are the only requirements now.
reqsList <- c(reqsList,"req.RDA.minrls")

#' @param req.RDA.macro - Recommended Daily Allowance (RDA) data for macro nutrients
req.RDA.macro <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1, colNames = TRUE,
                      sheet = "RDAorAI_macro"))
keepListCol.reqs <- names(req.RDA.macro)[!names(req.RDA.macro) %in% c("status_group", "age_group")]
req.RDA.macro <- req.RDA.macro[, (keepListCol.reqs), with = FALSE]
reqsList <- c(reqsList,"req.RDA.macro")

#' @param req.UL.vits - Recommended Upper Limit (UL) data for vitamins
req.UL.vits <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "UL_vitamins"))
keepListCol.reqs <- names(req.UL.vits)[!names(req.UL.vits) %in% c("status_group", "age_group")]
req.UL.vits <- req.UL.vits[, (keepListCol.reqs), with = FALSE]
reqsList <- c(reqsList,"req.UL.vits")

#' @param req.UL.minrls - Recommended Upper Limit (UL) data for minerals
req.UL.minrls <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "UL_minerals"))
keepListCol.reqs <- names(req.UL.minrls)[!names(req.UL.minrls) %in% c("status_group", "age_group")]
req.UL.minrls <- req.UL.minrls[, (keepListCol.reqs), with = FALSE]

reqsList <- c(reqsList,"req.UL.minrls")

#' @param req.AMDR.hi - Acceptable Macronutrient Distribution Range, hi version
req.AMDR.hi <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "AMDR_hi"))
keepListCol.reqs <- names(req.AMDR.hi)[!names(req.AMDR.hi) %in% c("status_group", "age_group")]
req.AMDR.hi <- req.AMDR.hi[, (keepListCol.reqs), with = FALSE]

#' @param req.AMDR.lo - Acceptable Macronutrient Distribution Range, lo version
req.AMDR.lo <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE,
                      sheet = "AMDR_lo"))
keepListCol.reqs <- names(req.AMDR.lo)[!names(req.AMDR.lo) %in% c("status_group", "age_group")]
req.AMDR.lo <- req.AMDR.lo[, (keepListCol.reqs), with = FALSE]
reqsList <- c(reqsList,"req.AMDR.hi", "req.AMDR.lo")

# create lists of nutrients common to the food nutrient and  requirements lists ------
# keep only the common nutrients in the req list---
# i is the name of the requirement; j is a copy of the requirement data table
# also combine groups of children and for pregnant and lactating women
# children calcs for nutrient needs
# SSPF0_4 <- Inf0_0.5*(1/6) + Inf0.5_1*(1/6 + Chil1_3)*(4/6)
# SSPM0_4 <- Inf0_0.5*(1/6) + Inf0.5_1*(1/6 + Chil1_3)*(4/6)
# 0 to 4 is four years. Divide into 8 6-month intervals.
# chldrn.male <- j[j$ageGenderCode %in% Chil0_3, ]
# chldrn.male.SSP <-
#   chldrn.male[chldrn.male$ageGenderCode == "Inf0_0.5",
#               2:length(temp)] * 1 / 8 +
#   chldrn.male[chldrn.male$ageGenderCode == "Inf0.5_1",
#               2:length(chldrn.male)] * 1 / 8 +
#   chldrn.male[chldrn.male$ageGenderCode == "Chil1_3",
#               2:length(chldrn.male)] * 6 / 8
# chldrn.male.SSP$ageGenderCode <- "SSPM0_4"

# # female children are the same as male children
# chldrn.female.SSP <- chldrn.male.SSP
# chldrn.female.SSP$ageGenderCode <- "SSPF0_4"
# j <-
#   dplyr::bind_rows(list(j, chldrn.male.SSP, chldrn.female.SSP))

# calculations needed for the pregnant and lactating women results ----
# SSPF15_49 <- (F14_18 + F19_30 + F31_50)/3
# SSPLact <- (Lact14_18 + Lact19_30 + Lact31_50)/3
# SSPPreg <- (Preg14_18 + Preg19_30 + Preg31_50)/3
dt.ageGroupLU <- data.table::as.data.table(openxlsx::read.xlsx(fileNameList("SSP_DRI_ageGroupLU")))

# for loop to do recalculations -----
for (i in reqsList) {
commonListName <- paste("common", gsub("req.", "", i), sep = ".")
  temp <- intersect(sort(colnames(dt.nutrients)), sort(colnames(eval(parse(text = i)))))
  assign(commonListName, temp)
  j <- eval(parse(text = i))
  # Note: EARS include Reference_weight_kg. This is removed in the next line of code.
  j <- j[, c("ageGenderCode", temp), with = FALSE]
  j[is.na(j)] = 0
  # now do the adjustment for the differences in infant age groupd
  nutList <- names(j)[2:ncol(j)]
  req.transpose <- data.table::as.data.table(data.table::dcast(data.table::melt(j, id.vars = "ageGenderCode", measure.vars = nutList, variable.name = "nutrient"), nutrient ~ ageGenderCode))

  req.transpose[, Chil0_3 :=  1/6 * (Inf0_0.5 * 1 + Inf0.5_1 * 1 + Chil1_3 * 4)]
  req.transpose[, LactTot := (Lact14_18 + Lact19_30 + Lact31_50)/3]
  req.transpose[, PregTot := (Preg14_18 + Preg19_30 + Preg31_50)/3]
  req.transpose[, SSPF15_49 := (F14_18 + F19_30 + F31_50)/3 ]
  deleteListCol <- c("Inf0_0.5", "Inf0.5_1", "Chil1_3")
  req.transpose[, (deleteListCol) := NULL]
  ageGroupList <- colnames(req.transpose)[2:ncol(req.transpose)]
  #move Chil0_3 to the beginning of the data table
  ageGroupList <- ageGroupList[ageGroupList != "Chil0_3"]
  data.table::setcolorder(req.transpose, c("nutrient", "Chil0_3", ageGroupList))
  j <- data.table::dcast(data.table::melt(req.transpose, id.vars = "nutrient", variable.name = "ageGenderCode"), ageGenderCode ~ nutrient)
  j <- merge(j, dt.ageGroupLU, by.y = "dri", by.x = "ageGenderCode")
  j[, ageGenderCode := NULL]
  data.table::setnames(j, old = "ssp", new = "ageGenderCode")
  data.table::setcolorder(j, c("ageGenderCode", nutList))
    # assign (i, j) give the original req name to j
  assign(paste(i,"ssp", sep = "_"), j)
  inDT <- j
  outName <- paste(i,"ssp", sep = "_")
  cleanup(inDT,outName,fileloc("mData"))
}

