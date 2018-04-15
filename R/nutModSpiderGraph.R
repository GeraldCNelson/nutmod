#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, region management
#' @title Draw spider diagrams
#' @name xxx
#' @include nutrientModFunctions.R
source("R/nutrientModFunctions.R")

#library(fmsb)
# region <- keyVariable("region")

#reqType choices are RDA_macro, RDA_vits, RDA_minrls, EAR, UL _vits, UL_minrls, kcal_ratios
country = "NGA"
SSP = "SSP2" # the only choice at the moment
climModel = "HGEM" # choices are "HGEM" "IPSL"  "IPSL2" "NoCC"
experiment = "REF" # choices are "HiNARS2", "HiREFF2", "HiYld2", "IRREXP2", "IRREXP-WUE2", "LoYld2", "RegYld2", "SWHC2", "REF", PHL-DEV2
years <- c("X2010","X2030","X2050")
# note: the combination of SSP, climModel, and experiment must come from the following list
# SSP2-HGEM-HiNARS2     SSP2-HGEM-HiREFF2     SSP2-HGEM-HiYld2      SSP2-HGEM-IRREXP_WUE2
# SSP2-HGEM-IRREXP2     SSP2-HGEM-LoYld2      SSP2-HGEM-PHL-DEV2    SSP2-HGEM-RegYld2
# SSP2-HGEM-SWHC2       SSP2-HGEM            SSP2-IPSL-IRREXP-WUE2 SSP2-IPSL-IRREXP2
# SSP2-IPSL-SWHC2       SSP2-IPSL2            SSP2-NoCC-IRREXP-WUE2 SSP2-NoCC-IRREXP2
# SSP2-NoCC-SWHC2       SSP2-NoCC

plot.new()
par(mar = c(1, 1, 5, 1)) #decrease default margin
layout(matrix(1:6, ncol = 2, byrow = TRUE)) #draw 4 plots to device

nutSpiderGraph("RDA_macro", country, SSP, climModel, experiment, years)
nutSpiderGraph("RDA_vits", country, SSP, climModel, experiment, years)
nutSpiderGraph("RDA_minrls", country, SSP, climModel, experiment, years)
nutSpiderGraph("UL_minrls", country, SSP, climModel, experiment, years)
nutSpiderGraph("UL_vits", country, SSP, climModel, experiment, years)
nutSpiderGraph("kcal_ratios", country, SSP, climModel, experiment, years)
titleText <- sprintf("Ratio of consumption to requirement for %s \n SSP scenario: %s, climate model: %s, experiment %s",
                     countryNameLookup(country), SSP, climModel, experiment)
title(paste(titleText), outer = TRUE, line = -2)

# nutStackedBarGraph
#Create barplots with the barplot(height) function, where height is a vector or matrix.
#If height is a vector, the values determine the heights of the bars in the plot.
#If height is a matrix and the option beside=FALSE then each bar of the plot corresponds to a column of height, w
#ith the values in the column giving the heights of stacked “sub-bars”.
#If height is a matrix and beside=TRUE, then the values in each column are juxtaposed rather than stacked. Include option names.arg=(character vector) to label the bars. The option horiz=TRUE to createa a horizontal barplot.
plot.new()
reqType <- "kcal_ratios"
temp <- reqRatiodatasetup(reqType,country, SSP, climModel, experiment, years)
nutListShort <- temp[[2]]
inputData <- temp[[1]]
nuts.matrix <- as.matrix(inputData[year %in% (years),2:ncol(inputData), with = FALSE])
nuts.matrix.transpose <- t(nuts.matrix)
colList <- c("grey","green","red", "blue")
barTitleMain <- sprintf("Ratio of macronutrients in energy intake for %s \n SSP scenario: %s, climate model: %s, experiment %s",
                        countryNameLookup(country), SSP, climModel, experiment)
# mainTitle = "Share of macronutrients in total energy consumption"
# subTitle <- paste("Country:" , country,
#                   ", Climate model:", climModel,
#                   "\nSocioeconomic scenario:", SSP,
#                   ", Experiment:", experiment, sep = " ")
barplot(nuts.matrix.transpose, main = barTitleMain, xlab = "Years", names.arg = gsub("X", "",years),
        col = colList, border = NA)
legendText <- gsub("_g", "", nutListShort)
legend("bottomright", legend = legendText, text.col = "black", cex = .6, pt.cex = .6,
       pt.lwd = 1, pch = 20,
       col = colList)

# reqRatioDelta

