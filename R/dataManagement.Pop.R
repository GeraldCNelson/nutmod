#' @title "Management of SSP Population Data"
#' @keywords data management, SSP, population data
#' @name dataManagement.Pop.R
#' @description
#' This script reads in  cleaned up population data written by dataPrep.SSP.R for the SSP populaton data 
#' or the population data set for the African Ag Futures work prepared in dataPrep.UNscenarios.R 
#' and does manipulations to prepare it for later use
#' to align the SSP population data with the nutrient requirements age and gender structure data
#' Creates the following files
#'   req.EAR.ssp - data/req.EAR.percap.rds
#'   req.RDA.vits.ssp - data/req.RDA.vits.percap.rds
#'   req.RDA.minrls.ssp - data/req.RDA.minrls.percap.rds
#'   req.RDA.macro.ssp - data/req.RDA.macro.percap.rds
#'   req.UL.vits.ssp - data/req.UL.vits.percap.rds
#'   req.UL.minrls.ssp - data/req.UL.minrls.percap.rds"
#'   req.MRVs.ssp - data/req.MRVs.percap.rds" # added March 24, 2017

#' @source \url{https://tntcat.ac.at/SspDb/download/common/SspDb_country_data_2013-06-12.csv.zip}
#Copyright (C) 2015 - 2017 Gerald C. Nelson, except where noted

#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#   GNU General Public License for more details at http://www.gnu.org/licenses/.

source("R/nutrientModFunctions.R")

sourceFile <- "dataManagement.Pop.R"
createScriptMetaData()
keepYearList <- keyVariable("keepYearList")

# Read in the cleaned up population data ----
if (!gdxChoice %in% "AfricanAgFutures") {
  dt.pop <- getNewestVersion("dt.SSPPopClean", fileloc("uData"))
}else{
  dt.pop <- getNewestVersion("dt.pop.AfrAgFutures", fileloc("uData"))
}

#' do this to remove year 0, which was needed for the fish and alcohol calculations
dt.pop <- dt.pop[year %in% keepYearList,]

# Extract lists of the scenarios, regions, and data variables ------------------------------------
# dt.SSP.scen <- data.table::copy(dt.pop)
# # start process of creating a separate list for pregnant and lactating (P/L) women----
# this list is for females who could be pregnant and lactating and have for the most part
# identical nutrient needs if they are not P/L

#  Estimate the number of pregnant women and lactating women -----
# the estimate is based on the number of children aged 0 to 4.
#sum boys and girls 0 to 4

# convert rows of age gender groups to columns to make the addition process straightforward
formula.wide <- paste("scenario + region_code.IMPACT159 + year ~ ageGenderCode")
dt.pop.wide <- data.table::dcast(
  data = dt.pop,
  formula = formula.wide,
  value.var = "value")

ageColsToSum <- c("F15_19", "F20_24", "F25_29", "F30_34", "F35_39", "F40_44", "F45_49") #ages for which women can be pregnant or lactating
dt.pop.wide[, F15_49 := rowSums(.SD), .SDcols = ageColsToSum]
dt.pop.wide <- unique(dt.pop.wide)

#pregnant and lactating women are a constant share of kids 0 to 4. This is a *kludge*!!!
share.preg <- 0.2
share.lact <- 0.2
dt.pop.wide[, Preg := (F0_4 + M0_4) * share.preg][,Lact := (F0_4 + M0_4) * share.lact][,nonPL := F15_49 - Preg - Lact]
# data.table::setkey(dt.SSP.scen.wide)
# get rid of temporary column
deleteListCol <- c("F15_49")
dt.pop.wide[,(deleteListCol) := NULL]
# the next step makes F15_49 be only women of that age who are not PL. The sum of F15_49, Preg, Lact is all women in this group
data.table::setnames(dt.pop.wide, old = "nonPL",new = "F15_49") 
idVarsPop <- c("scenario", "region_code.IMPACT159", "year" )
measureVarsPop <- names(dt.pop.wide)[!names(dt.pop.wide) %in% idVarsPop]
dt.pop <- data.table::melt(dt.pop.wide,
                           id.vars = idVarsPop,
                           variable.name = "ageGenderCode",
                           measure.vars = measureVarsPop,
                           value.name = "value",
                           variable.factor = FALSE)

#change names of age groups; prepend SSP because the DRI requirements have been converted to SSP age groups and the reqsSSP files use these age group names (eg. SSPM15_19)
 dt.pop[, ageGenderCode := paste("SSP", ageGenderCode, sep = "")]
data.table::setnames(dt.pop, old = "value",new = "pop.value")

#' Title repCons
#' Short for Representative Consumer
#' @param dt.pop - data table with population data for one scenario (at least for now)
#' @param nutReqName - the short name of a nutrients requirement dataframe, eg. req_EAR.ssp
#' that has been converted to SSP (and UN?) age and gender groups
#' @param common.nut - list of nutrients common to the food nutrient list and the
#' requirements list
#' @param ageRowsToSum - a list of the female age groups that could potentially be pregnant.
#' It is included to so these individual rows can be deleted. They are replaced with
#' preg, lact, and nonPL, which should sum to the total of the list in ageRowsToSum
#' @param region - code for the region over which the operations should be done; e.g., region_code.IMPACT159
#' @return
#' @export
# repCons generates the age and gender specific nutrient requirements for a representative consumer as they change over time.
repCons <- function(dt.pop, nutReqName) {
  dt.nutReq <- getNewestVersion(nutReqName)
  common.nut <- names(dt.nutReq)[!names(dt.nutReq) %in% "ageGenderCode"] # get just the names of the nutrients in the nutReqName file
  dt.temp <- merge(dt.pop, dt.nutReq, by = "ageGenderCode", all.y = TRUE)
  keyValues <- c("scenario", "region_code.IMPACT159", "year", "ageGenderCode","pop.value")
  data.table::setkeyv(dt.temp,keyValues)
  # multiply the number of people in an age and gender group by the nutritional requirements of that group
  dt.temp[, (paste(common.nut, "prod", sep = "_")) :=
            lapply(.SD, function(x) x * dt.temp$pop.value), .SDcols = (common.nut)][,(common.nut) := NULL]
  keyValues <- c("scenario", "region_code.IMPACT159", "year")
  data.table::setkeyv(dt.temp, keyValues)
  reqlist <- c(paste(common.nut, "prod", sep = "_"), "pop.value")
  # sum the nutritional requirements for all groups
  dt.temp[, (paste(reqlist, "sum", sep = "_")) := lapply(.SD, sum), by =
            keyValues, .SDcols = c(reqlist)][, c(reqlist) := NULL]
  dt.temp.sum <- unique(dt.temp[, c("scenario","region_code.IMPACT159", "year",
                                    paste(reqlist, "sum", sep = "_")), with = FALSE])
  
  sumlist <- paste(common.nut, "prod_sum", sep = "_")
  finlist <- paste(common.nut, "fin", sep = "_")
  #divide every element in sumlist by pop.value and put in corresponding variable in finlist
  # cat("\n\n", names(dt.temp.sum))
  dt.temp.sum[,(finlist) := lapply(.SD, "/", dt.temp.sum$pop.value_sum), .SDcols = sumlist]
  dt.temp.sum[, c("pop.value_sum", sumlist) := NULL]
  # change column names back to just nutrient names
  data.table::setnames(dt.temp.sum,old = finlist, new = common.nut)
  dt.temp.melt <- data.table::melt(
    dt.temp.sum,
    id.vars = c("scenario","region_code.IMPACT159", "year"),
    measure.vars = common.nut,
    variable.name = "nutrient",
    variable.factor = FALSE,
    value.name = "value"
  )
  
  inDT <- dt.temp.sum
  outName <- paste(gsub("_ssp","",nutReqName),"percap",sep = "_")
  desc <- paste0("Per capita requirement of ", nutReqName, " by specific population scenario")
  cleanup(inDT,outName,fileloc("mData"), desc = desc) # changed to uData June 5, 2018; changed to mData Oct 5, 2018 because of combine Un and pop data
  
  temp <- data.table::dcast(
    dt.temp.melt,
    scenario + region_code.IMPACT159 + nutrient ~ year,
    fun = NULL,
    value.var = "value")
  return(temp)
}

#nutrient requirements are calculated in dataPrep.nutrientRequirements.R.
reqsSSP <- keyVariable("reqsListSSP")
#The list of nutrients for each is common.EAR, common.RDA.vits, common.RDA.minrls, common.RDA.macro, common.UL.vits,
#    common.UL.minrls, common.AMDR
common <- keyVariable("commonList")
# code that creates and writes out the nutrient requirements files
# set up workbook wbGeneral -----
wbGeneral <- openxlsx::createWorkbook()

#create a variable, creationInfo, with info on creator, date, model version, etc.
creationInfo <-
  ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo,
                      paste("Creator:", keyVariable("userName")))
creationInfo <- rbind(creationInfo,
                      paste("Date of file creation:", Sys.Date()))
#creationInfo <- rbind(creationInfo, paste("IMPACT data:", IMPACTfileName))
nutrientFileName <- fileNameList("nutrientFileName")
creationInfo <- rbind(creationInfo,
                      paste("Nutrient data:", nutrientFileName))
DRIFileName <- fileNameList("DRIFileName")
creationInfo <- rbind(creationInfo,
                      paste("Nutrient requirements data:", DRIFileName))
SSPdataZip <- fileNameList("SSPdataZip")
creationInfo <- rbind(creationInfo,
                      paste("SSP.regions data:", SSPdataZip))
openxlsx::addWorksheet(wb = wbGeneral, sheetName = "creation_Info")
# write the creationInfo variable to a worksheet called creation_Info
openxlsx::writeData(
  wb = wbGeneral,
  x = creationInfo,
  sheet = "creation_Info",
  startRow = 1, startCol = 1,
  rowNames = FALSE, colNames = FALSE
)
#Set up df wbInfoGeneral for common worksheet names and descriptions -----
wbInfoGeneral <-
  data.frame(sheet_Name = character(),sheet_Desc = character(),stringsAsFactors = FALSE)

wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
  c("creation_Info","Information on creator, date, model version, etc.")

wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
  c("Sheet names", "Description of sheet contents")

#add a worksheet called IMPACTBaselist to the workbook wb = wbGeneral, with info on the commodities
# in the base set and their nutrients
dt.nutrients <- getNewestVersion("dt.nutrients.var", fileloc("iData")) # put var in the file name Oct 4, 2018

openxlsx::addWorksheet(wb = wbGeneral, sheetName = "IMPACTBaselist")
#commodityNames <- cbind(nutrients[c("Name","IMPACT_code")])
openxlsx::writeData(
  wb = wbGeneral,
  x = dt.nutrients,
  sheet = "IMPACTBaselist",
  startRow = 1,
  startCol = 1
)
openxlsx::addStyle(
  wb = wbGeneral,
  sheet = "IMPACTBaselist",
  style = numStyle,
  rows = 2:nrow(dt.nutrients),
  cols = 3:(ncol(dt.nutrients)),
  gridExpand = TRUE
)

# for loop for the nutrient requirements worksheets ----
for (i in 1:length(reqsSSP)) {
  dt.temp.internal <-  repCons(dt.pop, reqsSSP[i]) #, ageRowsToSum)
  data.table::setkeyv(dt.temp.internal, c("nutrient", "region_code.IMPACT159"))
  
  dt.name <- paste(reqsSSP[i], "percap", sep = "_")
  #print(dt.name)
  openxlsx::addWorksheet(wb = wbGeneral, sheetName = dt.name)
  openxlsx::addStyle(
    wb = wbGeneral, sheet = dt.name, style = numStyle, rows = 2:nrow(dt.temp.internal),
    cols = 3:(ncol(dt.temp.internal)),
    gridExpand = TRUE
  )
  openxlsx::writeData(
    wb = wbGeneral,
    x = dt.temp.internal,
    sheet = dt.name,
    startRow = 1, startCol = 1, rowNames = FALSE, colNames = TRUE
  )
  nutrientList <- paste(unique(dt.temp.internal$nutrient), collapse = ", ")
  sheetDesc <- paste("nutrients included: ", nutrientList)
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
    c(dt.name, sheetDesc)
  
  dt.temp.melt <- data.table::melt(dt.temp.internal,
                                   id.vars = c("region_code.IMPACT159", "nutrient"),
                                   measure.vars = keepYearList,
                                   value.name = "value",
                                   variable.factor = FALSE)
}

#add sheet with info about each of the worksheets
openxlsx::addWorksheet(wb = wbGeneral, sheetName = "sheetInfo")
openxlsx::writeData(
  wb = wbGeneral,
  x = wbInfoGeneral,
  sheet = "sheetInfo",
  startRow = 1,
  startCol = 1,
  rowNames = FALSE,
  colNames = FALSE
)
openxlsx::addStyle(
  wb = wbGeneral,
  sheet = "sheetInfo",
  style = textStyle,
  rows = 1:nrow(wbInfoGeneral),
  cols = 1:(ncol(wbInfoGeneral)),
  gridExpand = TRUE
)
openxlsx::setColWidths(wb = wbGeneral,
                       sheet = "sheetInfo",
                       cols = 1:2,
                       widths = 20)

#move sheetInfo worksheet from the last to the first
temp <- 2:length(names(wbGeneral)) - 1
temp <- c(length(names(wbGeneral)), temp)
openxlsx::worksheetOrder(wbGeneral) <- temp

if( exists("gdxChoice")) {
  xcelOutFileName <-
    paste("results/",gdxChoice, "/nut.requirements.", Sys.Date(), ".xlsx", sep = "")
  openxlsx::saveWorkbook(wb = wbGeneral, xcelOutFileName, overwrite = TRUE)
}

finalizeScriptMetadata(metadataDT, sourceFile)
# sourcer <- clearMemory(sourceFile, gdxChoice) # removes everything in memory and sources the sourcer function
