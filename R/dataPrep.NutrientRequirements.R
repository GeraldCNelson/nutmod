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
#' The requirements are imported in dataPrep.nutrientData.R and saved as df.nutrients

#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function"))
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
options(encoding = "UTF-8")

# nutrients spreadsheet; otherwise the next line is fine
#' @param nutrients - list of nutrient data generated in dataPrep.nutrientData.R
nutrients <- getNewestVersion("dt.nutrients")

#' @param allFoodGroups - list of all food groups
allFoodGroups <- unique(nutrients$food.group.code)

# Read in and set up the nutrient requirements data -----

# getSheetNames(DRIFileName)
#' @param req.metadata - meta data for the nutrient requirements
reqsFile <- fileNameList("DRIs")
req.metadata <- openxlsx::read.xlsx(reqsFile, sheet = "Reference", colNames = FALSE)
format(req.metadata, justify = c("left"))

#' @param req.EAR - Estimated Average Requirements (EAR) data
req.EAR <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "EAR",
    startRow = 1,
    cols = 3:24,
    colNames = TRUE
  )
reqsList <- "req.EAR"

#' @param req.RDA.vits - Recommended Daily Allowance (RDA) data for vitamins
req.RDA.vits <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "RDAorAI_vitamins",
    startRow = 1,
    cols = 3:17,
    colNames = TRUE
  )
reqsList <- c(reqsList,"req.RDA.vits")

#' @param req.RDA.minrls - Recommended Daily Allowance (RDA) data for minerals
req.RDA.minrls <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "RDAorAI_minerals",
    startRow = 1,
    cols = 3:18,
    colNames = TRUE
  )
reqsList <- c(reqsList,"req.RDA.minrls")

#' @param req.RDA.macro - Recommended Daily Allowance (RDA) data for macro nutrients
req.RDA.macro <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "RDAorAI_macro",
    startRow = 1,
    cols = 3:10,
    colNames = TRUE
  )
reqsList <- c(reqsList,"req.RDA.macro")

#' @param req.UL.vits - Recommended Upper Limit (UL) data for vitamins
req.UL.vits <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "UL_vitamins",
    startRow = 1,
    cols = 3:18,
    colNames = TRUE
  )
reqsList <- c(reqsList,"req.UL.vits")

#' @param req.UL.minrls - Recommended Upper Limit (UL) data for minerals
req.UL.minrls <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "UL_minerals",
    startRow = 1,
    cols = 3:22,
    colNames = TRUE
  )
reqsList <- c(reqsList,"req.UL.minrls")

#' @param req.AMDR.hi - Acceptable Macronutrient Distribution Range, hi version
req.AMDR.hi <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "AMDR_hi",
    startRow = 1,
    cols = 3:13,
    colNames = TRUE
  )

#' @param req.AMDR.lo - Acceptable Macronutrient Distribution Range, lo version
req.AMDR.lo <-
  openxlsx::read.xlsx(
    reqsFile,
    sheet = "AMDR_lo",
    startRow = 1,
    cols = 3:13,
    colNames = TRUE
  )

reqsList <- c(reqsList,"req.AMDR.hi", "req.AMDR.lo")

# make lists of nutrients common to the food nutrient list and the
# requirements list ---
common.EAR <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.EAR)))
common.RDA.vits <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.RDA.vits)))
common.RDA.minrls <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.RDA.minrls)))
common.RDA.macro <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.RDA.macro)))
common.UL.vits <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.UL.vits)))
common.UL.minrls <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.UL.minrls)))
common.AMDR.hi <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.AMDR.hi)))
common.AMDR.lo <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.AMDR.lo)))


# keep only the nutrients common to the dt.nutrients and the list of nutrients covered in a nutrient requirements list
req.EAR <- req.EAR[, c("ageGenderCode", common.EAR)]
req.RDA.vits <- req.RDA.vits[, c("ageGenderCode", common.RDA.vits)]
req.RDA.minrls <-
  req.RDA.minrls[, c("ageGenderCode", common.RDA.minrls)]
req.RDA.macro <-
  req.RDA.macro[, c("ageGenderCode", common.RDA.macro)]
req.UL.vits <- req.UL.vits[, c("ageGenderCode", common.UL.vits)]
req.UL.minrls <-
  req.UL.minrls[, c("ageGenderCode", common.UL.minrls)]
req.AMDR.hi <- req.AMDR.hi[, c("ageGenderCode", common.AMDR.hi)]
req.AMDR.lo <- req.AMDR.lo[, c("ageGenderCode", common.AMDR.lo)]

#' naming conventions ---------- M - Male, F- Female, X - children of
#' both genders,
#' Preg - pregnant,
#' Lact - lactating
#' nonPL- women in the 15-49 age group that are neither pregnant or lactating

# assuming the population statistics have the number of children 5-9
# years and the EAR is for 4-8 years… you could make a new population
# estimate for 4-8 years by taking 20% of the population for 0-4 year
# olds (this is the number of 4 year olds) plus 80% of the population
# of 5-9 year olds (this is number of 5-8 year olds) – and then adjust
# all the others in a similar manner.
df.ageGroupLU <- openxlsx::read.xlsx(fileNameList("SSP_DRI_ageGroupLU"))

children <- c("Inf0_0.5", "Inf0.5_1", "Chil1_3")

reqsList <- keyVariable("reqsList")
  c(
    "req.EAR",
    "req.RDA.vits",
    "req.RDA.minrls",
    "req.RDA.macro",
    "req.UL.vits",
    "req.UL.minrls"
  )
for (j in reqsList) {
  temp2 <- eval(parse(text = j))
  temp2[is.na(temp2)] <- 0
  #males
  for (i in 1:nrow(df.ageGroupLU)) {
    temp <- temp2[temp2$ageGenderCode == df.ageGroupLU[i, 2], ]
    temp[1, 1] <- df.ageGroupLU[i, 1]
    # print(temp[1,1])
    temp2 <- rbind(temp2, temp)
  }
  #females
  for (i in 1:nrow(df.ageGroupLU)) {
    temp <- temp2[temp2$ageGenderCode == df.ageGroupLU[i, 4], ]
    temp[1, 1] <- df.ageGroupLU[i, 3]
    # print(temp[1,1])
    temp2 <- rbind(temp2, temp)
  }
  # children calcs for nutrient needs SSPF0_4 <- Inf0_0.5*(1/6) +
  # Inf0.5_1*(1/6 + Chil1_3)*(4/6) SSPM0_4 <- Inf0_0.5*(1/6) +
  # Inf0.5_1*(1/6 + Chil1_3)*(4/6) 0 to 4 is four years. Divide into 8 6
  # month intervals.
  chldrn.male <- temp2[temp2$ageGenderCode %in% children, ]
  chldrn.male.SSP <-
    chldrn.male[chldrn.male$ageGenderCode == "Inf0_0.5",
    2:length(temp)] * 1 / 8 +
    chldrn.male[chldrn.male$ageGenderCode == "Inf0.5_1",
    2:length(chldrn.male)] * 1 / 8 +
    chldrn.male[chldrn.male$ageGenderCode == "Chil1_3",
    2:length(chldrn.male)] * 6 / 8
  chldrn.male.SSP$ageGenderCode <- "SSPM0_4"

  # female children are the same as male children
  chldrn.female.SSP <- chldrn.male.SSP
  chldrn.female.SSP$ageGenderCode <- "SSPF0_4"
  temp2 <-
    dplyr::bind_rows(list(temp2, chldrn.male.SSP, chldrn.female.SSP))

  # calculations needed for the pregnant and lactating women results ----
  # SSPF15_49 <- (F14_18 + F19_30 + F31_50)/3 SSPLact <- (Lact14_18 +
  # Lact19_30 + Lact31_50)/3 SSPPreg <- (Preg14_18 + Preg19_30 +
  # Preg31_50)/3

  preg.potent <-
    temp2[temp2$ageGenderCode %in% c("F14_18", "F19_30",
                                     "F31_50"), ]
  preg.potent <-
    preg.potent[preg.potent$ageGenderCode == "F14_18", 2:length(preg.potent)] /
    3 +
    preg.potent[preg.potent$ageGenderCode == "F19_30", 2:length(preg.potent)] /
    3 +
    preg.potent[preg.potent$ageGenderCode == "F31_50", 2:length(preg.potent)] /
    3
  preg.potent$ageGenderCode <- "SSPF15_49"

  lact <-
    temp2[temp2$ageGenderCode %in% c("Lact14_18", "Lact19_30",
                                     "Lact31_50"), ]
  lact <-
    lact[lact$ageGenderCode == "Lact14_18", 2:length(lact)] / 3 +
    lact[lact$ageGenderCode == "Lact19_30", 2:length(lact)] / 3 +
    lact[lact$ageGenderCode == "Lact31_50", 2:length(lact)] / 3
  lact$ageGenderCode <- "SSPLact"

  preg <-
    temp2[temp2$ageGenderCode %in% c("Preg14_18", "Preg19_30", "Preg31_50"), ]
  preg <-
    preg[preg$ageGenderCode == "Preg14_18", 2:length(preg)] / 3 +
    preg[preg$ageGenderCode == "Preg19_30", 2:length(preg)] / 3 +
    preg[preg$ageGenderCode == "Preg31_50", 2:length(preg)] / 3
  preg$ageGenderCode <- "SSPPreg"

  temp2 <-
    as.data.frame(dplyr::bind_rows(list(temp2, preg.potent, lact, preg)))

  # delete extraneous rows
  temp2 <-
    temp2[(
      !temp2$ageGenderCode %in% df.ageGroupLU[, 2] &
        !temp2$ageGenderCode %in% df.ageGroupLU[, 4] &
        !temp2$ageGenderCode %in% children &
        !temp2$ageGenderCode %in% c("Preg14_18", "Preg19_30", "Preg31_50") &
        !temp2$ageGenderCode %in%
        c("Lact14_18", "Lact19_30", "Lact31_50")
    ), ]
  newDFname <- paste(j, ".ssp", sep = "")
  nutlistname <- paste()
  assign(newDFname, temp2)
  nutlistname <- paste(j, ".nutlist", sep = "")
  nutlist <- colnames(temp2[, 2:length(temp2)])
  assign(nutlistname, nutlist)
  inDT <- eval(parse(text = newDFname))
  print(newDFname)
  outName <- newDFname
  cleanup(inDT,outName,fileloc("mData"))
}

