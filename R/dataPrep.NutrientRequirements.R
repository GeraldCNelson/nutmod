#' @title calculate nutrient requirements for SSP age group categories
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

source("R/nutrientModFunctions.R")

#' @description {
#' This script calculates nutrient requirements for SSP age group categories.
#' The source of the requirements is http://www.nal.usda.gov/fnic/DRI/DRI_Tables/recommended_intakes_individuals.pdf.
#' }

sourceFile <- "dataPrep.NutrientRequirements.R"
description <- "This script calculates nutrient requirements for SSP age group categories. The source of the requirements is http://www.nal.usda.gov/fnic/DRI/DRI_Tables/recommended_intakes_individuals.pdf."
createScriptMetaData()

options(encoding = "UTF-8")

# nutrients spreadsheet for the base runs. This should be fine because this script just needs the list of nutrients.
# better to use .var because base will be going away Aug 17, 2018
dt.nutrients <- getNewestVersion("dt.nutrients.var", fileloc("iData"))

# Read in and set up the nutrient requirements data -----

# getSheetNames(DRIFileName). Not used elsewhere so commenting it out. Mar 17, 2018
#' @param req.metadata - meta data for the nutrient requirements
reqsFile <- fileNameList("DRIs")
req.metadata <- openxlsx::read.xlsx(reqsFile, sheet = "Reference", colNames = FALSE)
format(req.metadata, justify = c("left"))

#' @param req_EAR - Estimated Average Requirements (EAR) data
req_EAR <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "EAR"))
req_EAR[, c("status_group", "age_group") := NULL]

#' @param req_RDA_vits - Recommended Daily Allowance (RDA) data for vitamins
req_RDA_vits <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "RDAorAI_vitamins"))
req_RDA_vits[, c("status_group", "age_group") := NULL]

#' @param req_RDA_minrls - Recommended Daily Allowance (RDA) data for minerals
req_RDA_minrls <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "RDAorAI_minerals"))
req_RDA_minrls[, c("status_group", "age_group") := NULL]

#remove the requirements for iron and zinc that are not based on physiological requirements (PR)
req_RDA_minrls[, c("iron_mg", "zinc_mg") := NULL]

# add PR based iron and zinc requirements to the reqs data
req_PR_iron <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "PR_iron"))
req_PR_iron[, c("status_group", "age_group") := NULL]
req_PR_zinc <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "PR_zinc"))
req_PR_zinc[, c("status_group", "age_group") := NULL]
req_RDA_minrls <- merge(req_RDA_minrls, req_PR_iron, by = c("ageGenderCode"))
req_RDA_minrls <- merge(req_RDA_minrls, req_PR_zinc, by = c("ageGenderCode"))

#' @param req_RDA_macro - Recommended Daily Allowance (RDA) data for macro nutrients
req_RDA_macro <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1, colNames = TRUE, sheet = "RDAorAI_macro"))
req_RDA_macro[, c("status_group", "age_group") := NULL]

#' @param req_UL.vits - Recommended Upper Limit (UL) data for vitamins
req_UL_vits <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "UL_vitamins"))
req_UL_vits[, c("status_group", "age_group") := NULL]

#' @param req_UL.minrls - Recommended Upper Limit (UL) data for minerals
req_UL_minrls <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "UL_minerals"))
req_UL_minrls[, c("status_group", "age_group") := NULL]

#' @param req_AMDR_hi - Acceptable Macronutrient Distribution Range, high level
req_AMDR_hi <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "AMDR_hi"))
req_AMDR_hi[, c("status_group", "age_group") := NULL]

#' @param req_AMDR_lo - Acceptable Macronutrient Distribution Range, low level
req_AMDR_lo <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1,  colNames = TRUE, sheet = "AMDR_lo"))
req_AMDR_lo[, c("status_group", "age_group") := NULL]

#' @param req.MRV.alcohol - maximum recommended consumption of alcohol. The only MRV in the file is alcohol
req_MRVs <-
  data.table::as.data.table(openxlsx::read.xlsx(reqsFile, startRow = 1, colNames = TRUE, sheet = "MRVs"))
req_MRVs[, c("status_group", "age_group") := NULL]

reqsList <- keyVariable("reqsList")

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

# calculations needed for the pregnant and lactating women results ----
# SSPF15_49 <- (F14_18 + F19_30 + F31_50)/3
# SSPLact <- (Lact14_18 + Lact19_30 + Lact31_50)/3
# SSPPreg <- (Preg14_18 + Preg19_30 + Preg31_50)/3
dt.ageGroupLU <- data.table::as.data.table(openxlsx::read.xlsx(fileNameList("SSP_DRI_ageGroupLU")))

# for loop to do recalculations -----
# this loop assigns requirements by the age groups used in the SSP data
for (i in reqsList) {
  commonListName <- paste("common", gsub("req.", "", i), sep = ".")
  dt <- eval(parse(text = i))
  nutList <- intersect(sort(colnames(dt.nutrients)), sort(colnames(dt)))
  assign(commonListName, nutList)
  # Note: EARS include Reference_weight_kg. This is removed in the next line of code.
  deleteListCol <- names(dt)[!names(dt) %in% c("ageGenderCode", nutList)]
  if (length(deleteListCol) > 0) dt[, (deleteListCol) := NULL]
  dt[is.na(dt)] = 0
  # now do the adjustment for the differences in infant age group
  # Note combo of melt and dcast. Hmmm, when I ran this on Oct 7, 2018, I had to add data.table:: in front of dcast. It was already in front of melt
  req.transpose <- data.table::dcast(data.table::melt(dt,
                                          id.vars = "ageGenderCode",
                                          measure.vars = nutList,
                                          variable.name = "nutrient"),
                         nutrient ~ ageGenderCode)
  #create new groups to convert the requirements data so they align with the age and gender groups in the SSP data
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
  inDT <- j
  outName <- paste(i,"ssp", sep = "_")
  desc <- paste0("SSP data for ", i)
  cleanup(inDT,outName,fileloc("mData"), desc = desc)
}
finalizeScriptMetadata(metadataDT, sourceFile)
# sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
