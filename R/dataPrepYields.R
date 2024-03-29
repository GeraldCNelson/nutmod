#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords nutrient data,
#' @title Import and process DSSAT yield data
#' @name dataPrepYields.R
#' @include nutrientModFunctions.R

#' @keywords DSSAT, yield, data
#' \description{
#' This script imports DSSAT yield data generated by Ricky Robertson
#' }

#Copyright (C) 22018 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.

# column label explanation
# CCCOM: commodity name (like rice, sorg, etc.)
# FPU: FPU code name
# IRRF: irrigated vs. rainfed code
# YRS: approximate year number this represents
# co2_level: assumed atmospheric CO2 ppm
# area: SPAM area for this crop/water within the FPU under consideration
# yield: area-weighted average yield we came up with (i think IMPACT does its own computation of this, so this is mainly for convenience and double checking)
# prod: total "production" based on modeled yield multiplied by SPAM area
# 
# the remaining ones have to do with how we tried to deal with technology change when we were more obsessed with that.
# 
# adopt_area: area within the FPU where adoption of the new technology results in a higher yield
# post_adopt_yield: yield for adopters that are in locations benefitting from adoption
# post_adopt_prod: total production number for adopters that are in locations benefitting from adoption
# pre_adopt_yield: yield with the original "technology" on areas where adoption would be beneficial (these are those who have not adopted for whatever reason)
# pre_adopt_prod: total production with the original "technology" on areas where adoption would be beneficial (these are those who have not adopted for whatever reason)
# nonadopt_area: area within the FPU where adoption of the new technology results in a LOWER or EQUAL yield and hence would not be worthwhile
# nonadopt_yield: yield with the original "technology" on areas where adoption would NOT help
# nonadopt_prod: total production with the original "technology" on areas where adoption would NOT help
# RF: what you think, rainfed yields on rainfed areas as defined by SPAM
# IR: what you thing, irrigated yields on irrigated areas as defined by SPAM
# RF_cross: rainfed yields, but using the irrigated areas as the weights
# IR_cross: irrigated yields, but using the rainfed areas as the weights
# 
# the "cross" values were implemented to provide a few more fallbacks for IMPACT when it encounters mismatches between what crops it thinks need to be grown in an FPU
# and what SPAM thinks. in this case "crop" is includes both the directly modeled crops and the composites put together to proxy for climate change in the non-directly
# modeled crops. there are/were a very few places where maybe we needed some irrigated something-or-other, but there wasn't any irrigated area in the directly modeled crops
# that could be used to proxy. by using these "cross" values, IMPACT has more options that are based on something remotely reasonable when building the shifters before having
# to throw up its hands and purely speculate.
crops <- c("maiz", "grnd", "pota", "rice", "sorg", "soyb", "whea")
yearchoices <- c("2000", "2041_2070", "2071-2099" )
CO2Concentration <- c("379", "421", "478", "443", "487", "539", "541", "670", "936")
climModel <- c("noCC", "miroc", "hadgem2", "gfdl", "ipsl", "noresm1")
rcpVals <- c("rcp2p6", "rcp4p5", "rcp6p0", "rcp8.5")
  source("R/nutrientModFunctions.R")

sourceFile <- "dataPrepYields.R"
description <- "This script imports DSSAT yield data generated by Ricky Robertson"
createScriptMetaData()

fileloc.2000_2050 <- paste0(getwd(), "/data-raw/yields/yields_2000_2050/ftest_base_and_2055_fpu_aggregations")
files.2000_2050 <- as.data.table(list.files(fileloc.2000_2050))
files.2000_2050[, V1 := gsub("___", "__", V1)]
files.2000_2050[, V1 := gsub("base_2000", "base_2000__future_rcp8p5_2041_2070", V1)]
files.2000_2050[, cropModel := tstrsplit(V1, "__", fixed = TRUE, keep = c(1))]
files.2000_2050[, co2 := tstrsplit(V1, "__", fixed = TRUE, keep = c(2))]
files.2000_2050[, crop := tstrsplit(V1, "__", fixed = TRUE, keep = c(3))]
files.2000_2050[, techScheme := tstrsplit(V1, "__", fixed = TRUE, keep = c(4))]
files.2000_2050[, climate := tstrsplit(V1, "__", fixed = TRUE, keep = c(5))]
files.2000_2050[, otherInfo := tstrsplit(V1, "__", fixed = TRUE, keep = c(6))]
files.2000_2050[, otherInfo2 := tstrsplit(V1, "__", fixed = TRUE, keep = c(7))]
files.2000_2050[, TIME.notes.txt := tstrsplit(V1, "__", fixed = TRUE, keep = c(8))]
files.2000_2050[, TIME.notes.txt := gsub("notes.txt", "notes", TIME.notes.txt)]
files.2000_2050[, notes := tstrsplit(TIME.notes.txt, ".", fixed = TRUE, keep = c(2))]

files.2000_2050[, c("cropModel", "techScheme", "TIME.notes.txt") := NULL]
files.2000_2050 <- files.2000_2050[climate %in% c("base_2000", "hadgem2_es"),]
files.2000_2050 <- files.2000_2050[otherInfo %in% "future_rcp8p5_2041_2070" & notes %in% "txt",]
files.2000_2050 <- files.2000_2050[crop %in% "maiz" & otherInfo2 %in% "fixedmes" | 
                                   crop %in% "rice" & otherInfo2 %in% "boring" |
                                    crop %in% "soyb" & otherInfo2 %in% "static" |
                                   crop %in% c("grnd", "pota", "sorg", "whea"), ]
#reverse the changes to the file names
files.2000_2050[, V1 := gsub("base_2000__future_rcp8p5_2041_2070", "base_2000", V1 )]
files.2000_2050[, V1 := gsub("boring__", "boring___", V1 )]
files.2000_2050[, V1 := gsub("static__", "static___", V1 )]
files.2000_2050[, V1 := gsub("fixedmes__", "fixedmes___", V1 )]
fileListFinal.2000_2050 <- files.2000_2050$V1

fileloc.2071_2090 <- paste0(getwd(), "/data-raw/yields/yields_2080/narrow_cross_goericofmetz_19sep18")
files.2071_2090 <- as.data.table(list.files(fileloc.2071_2090))
files.2071_2090[, V1 := gsub("___", "__", V1)]
files.2071_2090[, V1 := gsub("base_2000", "base_2000__future_rcp8p5_2041_2070", V1)]
files.2071_2090[, cropModel := tstrsplit(V1, "__", fixed = TRUE, keep = c(1))]
files.2071_2090[, co2 := tstrsplit(V1, "__", fixed = TRUE, keep = c(2))]
files.2071_2090[, crop := tstrsplit(V1, "__", fixed = TRUE, keep = c(3))]
files.2071_2090[, techScheme := tstrsplit(V1, "__", fixed = TRUE, keep = c(4))]
files.2071_2090[, climate := tstrsplit(V1, "__", fixed = TRUE, keep = c(5))]
files.2071_2090[, otherInfo := tstrsplit(V1, "__", fixed = TRUE, keep = c(6))]
files.2071_2090[, otherInfo2 := tstrsplit(V1, "__", fixed = TRUE, keep = c(7))]
files.2071_2090[, TIME.notes.txt := tstrsplit(V1, "__", fixed = TRUE, keep = c(8))]
files.2071_2090[, TIME.notes.txt := gsub("notes.txt", "notes", TIME.notes.txt)]
files.2071_2090[, notes := tstrsplit(TIME.notes.txt, ".", fixed = TRUE, keep = c(2))]
files.2071_2090 <- files.2071_2090[otherInfo %in% "future_rcp8p5_2071_2099" & notes %in% "txt" & 
                                     climate %in% c("base_2000", "hadgem2_es"),]
files.2071_2090 <- files.2071_2090[crop %in% "maiz" & otherInfo2 %in% "fixedmes" | 
                                     crop %in% c("grnd", "pota", "sorg", "whea") |
                                     crop %in% "rice" & techScheme %in% "normal" |
                                     crop %in% "soyb" & otherInfo2 %in% "static", ]
#reverse the changes to the file names
files.2071_2090[, V1 := gsub("base_2000__future_rcp8p5_2041_2070", "base_2000", V1 )]
files.2071_2090[, V1 := gsub("boring__", "boring___", V1 )]
files.2071_2090[, V1 := gsub("static__", "static___", V1 )]
files.2071_2090[, V1 := gsub("fixedmes__", "fixedmes___", V1 )]
fileListFinal.2071_2090 <- files.2071_2090$V1

# read in one of the files to create a template to combine data files together
filetest <-  paste0(getwd(), "/data-raw/yields/yields_2000_2050/ftest_base_and_2055_fpu_aggregations/", fileListFinal.2000_2050[1])
temp <- as.data.table(read_csv(filetest, col_names=TRUE, cols(
  CCCOM = col_character(),
  FPU = col_character(),
  IRRF = col_character(),
  YRS = col_integer(),
  co2_level = col_integer(),
  area = col_double(),
  yield = col_double(),
  prod = col_double(),
  adopt_area = col_double(),
  post_adopt_yield = col_integer(),
  post_adopt_prod = col_double(),
  pre_adopt_yield = col_integer(),
  pre_adopt_prod = col_double(),
  nonadopt_area = col_double(),
  nonadopt_yield = col_double(),
  nonadopt_prod = col_double()
)))

finalFile.2071_2090 <- finalFile.2000_2050 <- temp[0,]
  for (i in 1:length(fileListFinal.2071_2090)) {
    dtname <-  paste0(getwd(), "/data-raw/yields/yields_2080/narrow_cross_goericofmetz_19sep18/", fileListFinal.2071_2090[i])
    dt <- read_csv(dtname, col_names=TRUE, cols(
      CCCOM = col_character(),
      FPU = col_character(),
      IRRF = col_character(),
      YRS = col_integer(),
      co2_level = col_integer(),
      area = col_double(),
      yield = col_double(),
      prod = col_double(),
      adopt_area = col_double(),
      post_adopt_yield = col_integer(),
      post_adopt_prod = col_double(),
      pre_adopt_yield = col_integer(),
      pre_adopt_prod = col_double(),
      nonadopt_area = col_double(),
      nonadopt_yield = col_double(),
      nonadopt_prod = col_double()))
    finalFile.2071_2090 <- rbind(finalFile.2071_2090, dt)
}
            
for (i in 1:length(fileListFinal.2000_2050)) {
  dtname <-  paste0(getwd(), "/data-raw/yields/yields_2000_2050/ftest_base_and_2055_fpu_aggregations/", fileListFinal.2000_2050[i])
  dt <- read_csv(dtname, col_names=TRUE, cols(
    CCCOM = col_character(),
    FPU = col_character(),
    IRRF = col_character(),
    YRS = col_integer(),
    co2_level = col_integer(),
    area = col_double(),
    yield = col_double(),
    prod = col_double(),
    adopt_area = col_double(),
    post_adopt_yield = col_integer(),
    post_adopt_prod = col_double(),
    pre_adopt_yield = col_integer(),
    pre_adopt_prod = col_double(),
    nonadopt_area = col_double(),
    nonadopt_yield = col_double(),
    nonadopt_prod = col_double()))
  finalFile.2000_2050 <- rbind(finalFile.2000_2050, dt)
}

finalFile <- rbind(finalFile.2000_2050, finalFile.2071_2090)

# deleted cols -   "adopt_area", "post_adopt_yield", "post_adopt_prod", "pre_adopt_yield",
# "pre_adopt_prod", "nonadopt_area", "nonadopt_yield", "nonadopt_prod"
keepListCol <- c("CCCOM", "FPU", "IRRF", "YRS", "co2_level", "yield", "area", "prod")
finalFile[, setdiff(names(finalFile), keepListCol) := NULL]
finalFile <- finalFile[IRRF %in% c("IR", "RF"), ]
setnames(finalFile, old = c("CCCOM", "YRS"), new = c("cropCode", "year"))
finalFile[, year := as.character(year)]
inDT <- finalFile
outName <- "yieldData"
desc <- "Yield data by FPU for selected IMPACT crops, 2005, 2050, and 2080"
cleanup(inDT, outName, fileloc("mData"), desc = desc)
finalizeScriptMetadata(metadataDT, sourceFile)
