#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------
#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

#' @description To be added

#' @include nutrientModFunctions.R
#' @include nutrientCalcFunctions.R
if (!exists("getNewestVersion", mode = "function")) {
  source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")
}
options(warn = 1) # can be deleted after development is finished. This changes warnings to errors and stops execution.
# choose a grouping of countries -----
region <- keyVariable("region")
reqList <-
  c(
    "req.EAR.percap",
    "req.RDA.vits.percap" ,
    "req.RDA.minrls.percap",
    "req.RDA.macro.percap",
    "req.UL.vits.percap",
    "req.UL.minrls.percap"
  )

req <- "req.UL.vits.percap" # for testing
#get just nutrient list from req
temp <- gsub("req.","",req)
reqShortName <- gsub(".percap","",temp)
temp <- paste("food.agg.", reqShortName, sep = "")
dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
# get per capita consumption of each nutrient
dt.nuts.sum <- getNewestVersion("all.sum",fileloc("resData"))

dt.nutsReqPerCap <- getNewestVersion(req)
# get list of nutrients from dt.nutsReqPerCap for the req set of requirements
nutList <- names( dt.nutsReqPerCap)[4:length(names( dt.nutsReqPerCap))]

# now prepare for creating rds and excel files (if they are not too big);
# one each for all food items, staples, and food groups ------

# individual food function
f.ratios.all <- function(region,dt.food.agg,req){
  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <- c("scenario",region,"year")
  sumKey <-   c(basicKey,"IMPACT_code")

  # the total daily consumption of each nutrient
  nutList.sum.all <-       paste(nutList, "sum.all", sep = ".")
  # the ratio of daily consumption of each nutrient to the total consumption
  nutList.ratio.all <-     paste(nutList, "ratio.all", sep = ".")
  # the ratio of daily consumption of each nutrient by the nutrient requirement
  nutList.req.ratio.all <- paste(nutList, "req.ratio.all", sep = ".")
  # the list of columns to keep for each group of data tables
  keepListCol.sum.all <-       c(basicKey,nutList.sum.all)
  keepListCol.ratio.all <-     c(sumKey,nutList.ratio.all)
  keepListCol.req.ratio.all <- c(sumKey,nutList.req.ratio.all)

  # create the data table and remove unneeded columns
  dt.all.sum <- unique(dt.food.agg[,       keepListCol.sum.all, with =  FALSE])
  dt.all.ratio <- unique(dt.food.agg[,     keepListCol.ratio.all, with =  FALSE])
  dt.all.req.ratio <- unique(dt.food.agg[, keepListCol.req.ratio.all, with =  FALSE])
  #reshape the results to get years in columns
  dt.all.sum.long <- data.table::melt(
    dt.all.sum,   id.vars = basicKey, measure.vars = nutList.sum.all, variable.name = "nutrient",
    value.name = "nut_share", variable.factor = FALSE)
  dt.all.ratio.long <- data.table::melt(
    dt.all.ratio, id.vars = sumKey, measure.vars = nutList.ratio.all, variable.name = "nutrient",
    value.name = "nut_share", variable.factor = FALSE)
  dt.all.req.ratio.long <- data.table::melt(
    dt.all.req.ratio,
    id.vars = sumKey,
    measure.vars = nutList.req.ratio.all,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE)
  formula.sum.all <- paste("scenario + ", region, " + nutrient  ~ year")
  dt.all.sum.wide <- data.table::dcast.data.table(
    data = dt.all.sum.long,
    formula = formula.sum.all,
    value.var = "nut_share",
    variable.factor = FALSE)

  formula.ratio.all <- paste("scenario + ", region, "  + nutrient + IMPACT_code  ~ year")

  dt.all.ratio.wide <- data.table::dcast(
    data = dt.all.ratio.long,
    formula = formula.ratio.all,
    value.var = "nut_share")
  dt.all.req.ratio.wide <- data.table::dcast(
    data = dt.all.req.ratio.long,
    formula = formula.ratio.all,
    value.var = "nut_share")

  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)

  inDT <- dt.all.sum.wide
  outName <- paste(reqShortName,"sum.all", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))

  inDT <- dt.all.ratio.wide
  outName <- paste(reqShortName,"ratio.all", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))

  inDT <- dt.all.req.ratio.wide
  outName <- paste(reqShortName,"req.ratio.all", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))
}

# foodGroup function
f.ratios.FG <- function(region,dt.food.agg,req) {
  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <-     c("scenario",region,"year")
  foodGroupKey <- c(basicKey, "food.group.code")
  #  nutList.sum.foodGroup <-       paste(nutList, "sum.foodGroup", sep = ".")
  nutList.ratio.foodGroup <-     paste(nutList, "ratio.foodGroup", sep = ".")
  nutList.req.ratio.foodGroup <- paste(nutList, "req.ratio.foodGroup", sep = ".")
  # keepListCol.sum.foodGroup <-       c(foodGroupKey,nutList.sum.foodGroup)
  keepListCol.ratio.foodGroup <-     c(foodGroupKey,nutList.ratio.foodGroup)
  keepListCol.req.ratio.foodGroup <- c(foodGroupKey,nutList.req.ratio.foodGroup)
  #  dt.foodGroup.sum <- unique(dt.food.agg[,       keepListCol.sum.foodGroup, with =  FALSE])
  dt.foodGroup.ratio <- unique(dt.food.agg[,     keepListCol.ratio.foodGroup, with =  FALSE])
  dt.foodGroup.req.ratio <- unique(dt.food.agg[, keepListCol.req.ratio.foodGroup, with =  FALSE])
  #reshape the results to get years in columns
  # dt.foodGroup.sum.long <- data.table::melt(
  #   dt.foodGroup.sum,
  #   id.vars = foodGroupKey,
  #   measure.vars = nutList.sum.foodGroup,
  #   variable.name = "nutrient",
  #   value.name = "nut_share",
  #   variable.factor = FALSE
  # )
  dt.foodGroup.ratio.long <- data.table::melt(
    dt.foodGroup.ratio,
    id.vars = foodGroupKey,
    measure.vars = nutList.ratio.foodGroup,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.foodGroup.req.ratio.long <- data.table::melt(
    dt.foodGroup.req.ratio,
    id.vars = foodGroupKey,
    measure.vars = nutList.req.ratio.foodGroup,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  formula.foodGroup <- paste("scenario + ",region," + nutrient + food.group.code ~ year")
  # dt.foodGroup.sum.wide <- data.table::dcast.data.table(
  #   data = dt.foodGroup.sum.long,
  #   formula = formula.foodGroup,
  #   value.var = "nut_share",
  #   variable.factor = FALSE
  # )
  dt.foodGroup.ratio.wide <- data.table::dcast.data.table(
    data = dt.foodGroup.ratio.long,
    formula = formula.foodGroup,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.foodGroup.req.ratio.wide <- data.table::dcast.data.table(
    data = dt.foodGroup.req.ratio.long,
    formula = formula.foodGroup,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)

  inDT <- dt.foodGroup.ratio.wide
  outName <- paste(reqShortName,"ratio.foodGroup", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))

  inDT <- dt.foodGroup.req.ratio.wide
  outName <- paste(reqShortName,"req.ratio.foodGroup", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))
}

# staples function
r.ratios.staples <- function(region,dt.food.agg,req) {
  dt.nutsReqPerCap <- getNewestVersion(req)
  # get list of nutrients from dt.nutsReqPerCap for the req set of requirements
  nutList <- names( dt.nutsReqPerCap)[4:length(names(dt.nutsReqPerCap))]
  basicKey <-  c("scenario", region, "year")
  stapleKey <- c(basicKey, "staple.code")
  # the total daily consumption of each staple
  nutList.sum.staple <-       paste(nutList, "sum.staple", sep = ".")
  # the ratio of daily consumption of each nutrient for each staple to the total consumption
  nutList.ratio.staple <-     paste(nutList, "ratio.staple", sep = ".")
  nutList.req.ratio.staple <- paste(nutList, "req.ratio.staple", sep = ".")
  keepListCol.sum.staple <-       c(stapleKey,nutList.sum.staple)
  keepListCol.ratio.staple <-     c(stapleKey,nutList.ratio.staple)
  keepListCol.req.ratio.staple <- c(stapleKey,nutList.req.ratio.staple)
  dt.staples.sum <- unique(dt.food.agg[,       keepListCol.sum.staple, with =  FALSE])
  dt.staples.ratio <- unique(dt.food.agg[,     keepListCol.ratio.staple, with =  FALSE])
  dt.staples.req.ratio <- unique(dt.food.agg[, keepListCol.req.ratio.staple, with =  FALSE])
  #reshape the results to get years in columns
  dt.staples.sum.long <- data.table::melt(
    dt.staples.sum,
    id.vars = stapleKey,
    measure.vars = nutList.sum.staple,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.ratio.long <- data.table::melt(
    dt.staples.ratio,
    id.vars = stapleKey,
    measure.vars = nutList.ratio.staple,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.req.ratio.long <- data.table::melt(
    dt.staples.req.ratio,
    id.vars = stapleKey,
    measure.vars = nutList.req.ratio.staple,
    variable.name = "nutrient",
    value.name = "nut_share",
    variable.factor = FALSE
  )
  formula.staple <- paste("scenario + ",region," + nutrient + staple.code ~ year")
  dt.staples.sum.wide <- data.table::dcast.data.table(
    data = dt.staples.sum.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.ratio.wide <- data.table::dcast.data.table(
    data = dt.staples.ratio.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  dt.staples.req.ratio.wide <- data.table::dcast.data.table(
    data = dt.staples.req.ratio.long,
    formula = formula.staple,
    value.var = "nut_share",
    variable.factor = FALSE
  )
  temp <- gsub("req.","",req)
  reqShortName <- gsub(".percap","",temp)

  inDT <- dt.staples.sum.wide
  outName <- paste(reqShortName,"staples.sum", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))

  inDT <- dt.staples.ratio.wide
  outName <- paste(reqShortName,"staples.ratio", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))


  inDT <- dt.staples.req.ratio.wide
  outName <- paste(reqShortName,"staples.req.ratio", sep = ".")
  cleanup(inDT,outName, fileloc("resData"))
}

for (i in reqList) {
  # req <- "req.UL.vits.percap" # for testing
  #get just nutrient list from req
  print(paste("working on", i))
  temp <- gsub("req.","",i)
  reqShortName <- gsub(".percap","",temp)
  temp <- paste("food.agg.", reqShortName, sep = "")
  dt.food.agg <- getNewestVersion(temp, fileloc("resData"))
  # get per capita consumption of each nutrient
  # dt.nuts.sum <- getNewestVersion("all.sum",fileloc("resData"))
  f.ratios.FG(region,dt.food.agg,i)
}
