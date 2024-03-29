#' @title Nutrient calcs processing
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2015 - 2018 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
#   for more details at http://www.gnu.org/licenses/.

#' @description {
#' Calculate various adequacy ratios. More description needed
#' }

source("R/nutrientModFunctions.R")

sourceFile <- "nutCalcsProcessing.R"
description <- "Calculate various adequacy ratios. More description needed"
createScriptMetaData()

reqList <- keyVariable("reqsList")
#reqsToDelete <- c( "req_EAR", "req_UL_vits", "req_UL_minrls", "req_AMDR_hi", "req_AMDR_lo")
reqsToDelete <- c( "req_EAR", "req_AMDR_hi", "req_AMDR_lo" ) #, "req_UL_vits", "req_UL_minrls")
reqList <- reqList[!reqList %in% reqsToDelete]
AMDRs <-  c("req_AMDR_hi", "req_AMDR_lo")

for (switchloop in getSwitchChoice()) {
  switch.useCookingRetnValues <- keyVariable("switch.useCookingRetnValues")
  switch.fixFish <- keyVariable("switch.fixFish") #get rid of nutrient info for shrimp, tuna, and salmon because they are not currently in the FBS data
  if (switchloop == 2) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  if (switchloop == 3) {switch.vars <- TRUE;  switch.fortification <- TRUE; suffix = "varFort"}
  if (switchloop == 4) {switch.vars <- TRUE;  switch.fortification <- FALSE; suffix = "var"}
  
  #do AMDRs as a special case
  for (i in AMDRs) {
    cat("\n------ working on ", i, "\n")
    reqShortName <- gsub("req_", "", i)
    temp <- paste("food_agg_", reqShortName, ".", suffix, sep = "")
    DT <- getNewestVersion(temp, fileloc("resultsDir"))
    basicKey <- c("scenario", "region_code.IMPACT159", "year")
    
    DT.long <- data.table::melt(
      DT,
      id.vars = basicKey,
      measure.vars =  c("carbohydrate_g_reqRatio_all", "fat_g_reqRatio_all" , "protein_g_reqRatio_all"),
      variable.name = "nutrient",
      value.name = "value", variable.factor = FALSE)
    
    DT.long[, nutrient := gsub("_reqRatio_all", "", nutrient)]
    inDT <- unique(DT.long)
    outName <- paste("reqRatio_sum_", reqShortName, ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("Adequacy ratios for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
  }
  
  # individual food function -----
  # This is called from a for loop down about 200 lines of code
  f.ratios.all <- function(dt.food_agg.master){
    dt.food_agg <- data.table::copy(dt.food_agg.master)
    keepListCol <- c(mainCols, cols.all)
    dt.food_agg[, setdiff(names(dt.food_agg), keepListCol) := NULL]
    sumKey <- c(basicKey, "IMPACT_code")
    # the total daily consumption of each nutrient
    nutList_sum_all <-    paste(nutList, "sum_all", sep = "_")
    # the ratio of daily consumption of each nutrient to the total consumption
    nutList_ratio_all <-   paste(nutList, "ratio_all", sep = "_")
    # the ratio of daily consumption of each nutrient by the nutrient requirement
    nutList_reqRatio_all <- paste(nutList, "reqRatio_all", sep = "_")
    
    # the list of columns to keep for each group of data tables
    keepListCol_sum_all <-    c(basicKey, nutList_sum_all)
    keepListCol_ratio_all <-   c(sumKey, nutList_ratio_all)
    keepListCol_reqRatio_all <- c(sumKey, nutList_reqRatio_all)
    
    # create the data table and remove unneeded columns
    dt.all_sum <- dt.food_agg[,keepListCol_sum_all, with = FALSE]
    setkey(dt.all_sum)
    dt.all_sum <- unique(dt.all_sum)
    
    dt.all_ratio <- dt.food_agg[,   keepListCol_ratio_all, with = FALSE]
    setkey(dt.all_ratio)
    dt.all_ratio <- unique(dt.all_ratio)
    
    dt.all_reqRatio <- dt.food_agg[, keepListCol_reqRatio_all, with = FALSE]
    setkey(dt.all_reqRatio)
    dt.all_reqRatio <- unique(dt.all_reqRatio)
    
    # calculate the ratio of nutrient consumption for all commodities to the requirement
    # # dt.sum.copy <- data.table::copy(dt.all_sum)
    #  # scenarioComponents code needed because nutsReqPerCap are only available with scenario as SSPs
    #  scenarioComponents <- c("SSP", "climate_model", "experiment")
    #  dt.sum[, (scenarioComponents) := data.table::tstrsplit(scenario, "-", fixed = TRUE)]
    #  dt.nuts.temp <- dt.nutsReqPerCap[scenario %in% unique(dt.sum$SSP),]
    #  temp <- merge(dt.sum,dt.nuts.temp, by.x = c("SSP", "region_code.IMPACT159", "year"),
    #                by.y = c("scenario", "region_code.IMPACT159", "year"),
    #                all.x = TRUE)
    #  temp[,SSP := NULL]
    nutListSum <- as.vector(paste(nutList,"_sum_all", sep = ""))
    nutListReqRatio <- as.vector(paste(nutList,"_reqRatio", sep = ""))
    nutListReq <- as.vector(paste(nutList,"_req", sep = ""))
    # note: an alternative to the Map code below is simply to sum (using ethanol as an example) ethanol_g_reqRatio.all by scenario year region
    # the R code is explained at http://stackoverflow.com/questions/37802687/r-data-table-divide-list-of-columns-by-a-second-list-of-columns
    dt.food_agg[, (nutListReqRatio) := Map(`/`, mget(nutListSum), mget(nutListReq))]
    keepListCol <- c(basicKey, nutListReqRatio)
    dt.sum_reqRatio <- unique(dt.food_agg[, keepListCol, with = FALSE])
    
    dt.all_sum.long <- data.table::melt(
      dt.all_sum,
      id.vars = basicKey,
      measure.vars = nutList_sum_all,
      variable.name = "nutrient",
      value.name = "value", variable.factor = FALSE)
    dt.all_sum.long[, nutrient := gsub("_sum_all", "",nutrient)]
    
    dt.sum_reqRatio_long <- data.table::melt(
      dt.sum_reqRatio,
      id.vars = basicKey,
      measure.vars = nutListReqRatio,
      variable.name = "nutrient",
      value.name = "value", variable.factor = FALSE)
    dt.sum_reqRatio_long[, nutrient := gsub("_reqRatio", "",nutrient)]
    
    dt.all_ratio.long <- data.table::melt(
      dt.all_ratio, id.vars = sumKey,
      measure.vars = nutList_ratio_all,
      variable.name = "nutrient",
      #    value.name = "nut_share", variable.factor = FALSE)
      value.name = "value", variable.factor = FALSE)
    dt.all_ratio.long[, nutrient := gsub("_ratio_all", "", nutrient)]
    
    dt.all_reqRatio_long <- data.table::melt(
      dt.all_reqRatio,
      id.vars =  sumKey,
      measure.vars = nutList_reqRatio_all,
      variable.name = "nutrient",
      value.name = "value", variable.factor = FALSE)
    dt.all_reqRatio_long[, nutrient := gsub("_reqRatio_all", "",nutrient)]
    
    reqShortName <- gsub("req.", "", req)
    
    inDT <- unique(dt.all_sum.long)
    outName <- paste("all_sum_", reqShortName, ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("All sum for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
    # add column to indicate nutrient type. Added Aug 27, 2018
    dt.sum_reqRatio_long[, nutrientType := reqShortName]
    inDT <- dt.sum_reqRatio_long
    inDT[, nutrient := gsub("_reqRatio", "", nutrient)]
    inDT <- unique(inDT)
    outName <- paste("reqRatio_sum_", reqShortName, ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("Adequacy ratios by country for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
    
    # used in dt.diversityMetrics.R
    inDT <- unique(dt.all_ratio.long)
    outName <- paste("ratio_all_" ,reqShortName, ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("'All ratios by food item for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
    
    inDT <- dt.all_reqRatio_long
    inDT[, nutrient := gsub("_reqRatio_all", "", nutrient)]
    inDT <- unique(inDT)
    #    outName <- paste(reqShortName, "_all_reqRatio", ".", suffix, sep = "")
    outName <- paste("reqRatio_all_", reqShortName, ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("Adequacy ratios by food item for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
  }
  
  # foodGroup function -----
  f.ratios.FG <- function(dt.food_agg.master){
    dt.food_agg <- data.table::copy(dt.food_agg.master)
    keepListCol <- c(mainCols, cols.foodGroup)
    dt.food_agg[, setdiff(names(dt.food_agg), keepListCol) := NULL]
    foodGroupKey <- c(basicKey, "food_group_code")
    nutList_ratio_foodGroup <-   paste(nutList, "ratio_foodGroup", sep = "_")
    nutList_reqRatio_foodGroup <- paste(nutList, "reqRatio_foodGroup", sep = "_")
    keepListCol_ratio_foodGroup <-   c(foodGroupKey, nutList_ratio_foodGroup)
    keepListCol_reqRatio_foodGroup <- c(foodGroupKey, nutList_reqRatio_foodGroup)
    dt.foodGroup_ratio <- dt.food_agg[,   keepListCol_ratio_foodGroup, with = FALSE]
    data.table::setkey(dt.foodGroup_ratio)
    dt.foodGroup.ratio <- unique(dt.foodGroup_ratio)
    dt.foodGroup_reqRatio <- dt.food_agg[, keepListCol_reqRatio_foodGroup, with = FALSE]
    data.table::setkey(dt.foodGroup_reqRatio)
    dt.foodGroup_reqRatio <- unique(dt.foodGroup_reqRatio)
    
    dt.foodGroup.ratio.long <- data.table::melt(
      dt.foodGroup.ratio,
      id.vars = foodGroupKey,
      measure.vars = nutList_ratio_foodGroup,
      variable.name = "nutrient",
      value.name = "value",
      variable.factor = FALSE
    )
    
    dt.foodGroup_reqRatio_long <- data.table::melt(
      dt.foodGroup_reqRatio,
      id.vars = foodGroupKey,
      measure.vars = nutList_reqRatio_foodGroup,
      variable.name = "nutrient",
      value.name = "value",
      variable.factor = FALSE
    )
    
    reqShortName <- gsub("req.", "", req)
    
    # commented out March 27, 2018 because not used elsewhere
    # inDT <- unique(dt.foodGroup.ratio.long)
    # outName <- paste(reqShortName, "_FGratio", ".", suffix, sep = "")
    # desc <- paste0("Food group ratio for ", reqShortName)
    # cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
    
    inDT <- unique(dt.foodGroup_reqRatio_long)
    outName <- paste(reqShortName, "_FG_reqRatio", ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("Food group adequacy ratio for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
  }
  
  # staples function
  f.ratios.staples <- function(dt.food_agg.master){
    dt.food_agg <- data.table::copy(dt.food_agg.master)
    keepListCol <- c(mainCols, cols.staple)
    dt.food_agg[, setdiff(names(dt.food_agg), keepListCol) := NULL]
    
    stapleKey <- c(basicKey, "staple_code")
    
    # the total daily consumption of each staple
    nutList_sum_staple <-    paste(nutList, "sum_staple", sep = "_")
    
    # the ratio of daily consumption of each nutrient for each staple to the total consumption
    nutList_ratio_staple <-   paste(nutList, "ratio_staple", sep = "_")
    nutList_reqRatio_staple <- paste(nutList, "reqRatio_staple", sep = "_")
    keepListCol_sum_staple <-    c(stapleKey, nutList_sum_staple)
    keepListCol_ratio_staple <-   c(stapleKey, nutList_ratio_staple)
    keepListCol_reqRatio_staple <- c(stapleKey, nutList_reqRatio_staple)
    
    dt.staples_sum <- dt.food_agg[,    keepListCol_sum_staple, with = FALSE]
    data.table::setkey(dt.staples_sum, NULL)
    dt.staples_sum <- unique(dt.staples_sum)
    dt.staples_ratio <- dt.food_agg[,   keepListCol_ratio_staple, with = FALSE]
    data.table::setkey(dt.staples_ratio, NULL)
    dt.staples_ratio <- unique(dt.staples_ratio)
    dt.staples_reqRatio <- dt.food_agg[, keepListCol_reqRatio_staple, with = FALSE]
    data.table::setkey(dt.staples_reqRatio, NULL)
    dt.staples_reqRatio <- unique(dt.staples_reqRatio)
    
    #reshape the results to get years in columns
    dt.staples_sum.long <- data.table::melt(
      dt.staples_sum,
      id.vars = stapleKey,
      measure.vars = nutList_sum_staple,
      variable.name = "nutrient",
      value.name = "value",
      variable.factor = FALSE
    )
    
    dt.staples_ratio.long <- data.table::melt(
      dt.staples_ratio,
      id.vars = stapleKey,
      measure.vars = nutList_ratio_staple,
      variable.name = "nutrient",
      value.name = "value",
      variable.factor = FALSE
    )
    
    dt.staples_reqRatio_long <- data.table::melt(
      dt.staples_reqRatio,
      id.vars = stapleKey,
      measure.vars = nutList_reqRatio_staple,
      variable.name = "nutrient",
      value.name = "value",
      variable.factor = FALSE
    )
    
    reqShortName <- gsub("req.", "", req)
    # commented out March 27, 2018 because not used elsewhere
    # inDT <- unique(dt.staples_sum.long)
    # outName <- paste(reqShortName, "_staples_sum", ".", suffix, sep = "")
    # desc <- paste0("Staples sum for ", reqShortName)
    # cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
    #
    # inDT <- unique(dt.staples_ratio.long)
    # outName <- paste(reqShortName, "_staples_ratio", ".", suffix, sep = "")
    # desc <- paste0("Staples ratio for ", reqShortName)
    # cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
    
    #  inDT <- dt.staples_reqRatio_wide
    inDT <- unique(dt.staples_reqRatio_long)
    outName <- paste(reqShortName, "_staples_reqRatio", ".", suffix, sep = "")
    cat("outName:", outName, "\n")
    desc <- paste0("Staples adequacy ratio for ", reqShortName)
    cleanup(inDT, outName, fileloc("resultsDir"),  desc = desc)
  }
  cat("\n", suffix, "data")
  
  for (req in reqList) {
    cat("\n------ working on ", req, "\n")
    reqShortName <- gsub("req.", "", req) # results in names like "RDA_vits
    temp <- paste("food_agg_", reqShortName, ".", suffix, sep = "")
    dt.food_agg.master <- getNewestVersion(temp, fileloc("resultsDir"))
    dt.food_agg.master[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
    dt.food_agg.master[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
    dt.nutsReqPerCap <- getNewestVersion(paste(req,"percap",sep = "_"), fileloc("mData"))
    
    nutList <- names(dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
    basicKey <- c("scenario", "region_code.IMPACT159", "year")
    cols.all <- names(dt.food_agg.master)[grep("_all", names(dt.food_agg.master))]
    cols.staple <- names(dt.food_agg.master)[grep("_staple", names(dt.food_agg.master))]
    cols.foodGroup <- names(dt.food_agg.master)[grep("_foodGroup", names(dt.food_agg.master))]
    mainCols <- names(dt.food_agg.master)[!names(dt.food_agg.master) %in% c(cols.all,cols.staple,cols.foodGroup)]
    
    # run the ratios functions -----
    f.ratios.all(dt.food_agg.master)
    f.ratios.staples(dt.food_agg.master)
    f.ratios.FG(dt.food_agg.master)
  }
}

finalizeScriptMetadata(metadataDT, sourceFile)
