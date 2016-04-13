# Intro -------------------------------------------------------------------
#This script reads in the Shared Socioeconomic Profiles population data and does some manipulations
#to align the SSP population data with the nutrient requirements age and gender structure data
#Creates the following files
# req.EAR.ssp - results/req.EAR.percap.rds
# req.RDA.vits.ssp - results/req.RDA.vits.percap.rds
# req.RDA.minrls.ssp - results/req.RDA.minrls.percap.rds
# req.RDA.macro.ssp - results/req.RDA.macro.percap.rds
# req.UL.vits.ssp - results/req.UL.vits.percap.rds
# req.UL.minrls.ssp - results/req.UL.minrls.percap.rds"

#' @source \url{https://tntcat.ac.at/SspDb/download/common/SspDb_country_data_2013-06-12.csv.zip}
#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

#' @include nutrientModFunctions.R
#' @include workBookFunctions.R
if (!exists("getNewestVersion", mode = "function")) {
  source("R/nutrientModFunctions.R")
  source("R/workBookFunctions.R")
}
keepYearList <- keyVariable("keepYearList")
# Read in the cleaned up SSP population data ----
dt.SSPPop <- getNewestVersion("dt.SSPPopClean")
dt.SSPPop <- dt.SSPPop[year %in% keepYearList,]

data.table::setkey(dt.SSPPop, ISO_code)
dt.regionsLU <- data.table::data.table(getNewestVersion("df.regions.all"), key = "ISO_code")
dt.SSPPop <- dt.SSPPop[dt.regionsLU]
# Choose the region to aggregate to -----
region <- keyVariable("region")
keepListCol <- c("scenario","ageGenderCode","year", "value",region)
dt.SSPPop <- dt.SSPPop[, keepListCol, with = FALSE]
dt.SSPPop <- dt.SSPPop[!is.na(scenario),]
data.table::setkeyv(dt.SSPPop, c("scenario", region,"ageGenderCode","year"))
dt.SSP.regions <- dt.SSPPop[, sum(value), by = eval(data.table::key(dt.SSPPop))]
data.table::setnames(dt.SSP.regions, old = "V1", new = "value")

# Extract lists of the scenarios, regions, and data variables ------------------------------------
# scenarios <- unique(dt.SSP.regions$scenario)
# scen <- scenarios[2] #"SSP2_v9_130115"
# #mdl <- "IIASA-WiC POP"
# dt.SSP.scen <- dt.SSP.regions[scenario == scen, ]
# above is commented out. Want to include all scenarios
dt.SSP.scen <- dt.SSP.regions
# start process of creating a separate list for pregnant and lactating (P/L) women----
#this list is for females who could be pregnant and lactating and have for the most part
#identical nutrient needs if they are not P/L
ageRowsToSum <- c(
  "F15_19", "F20_24", "F25_29", "F30_34", "F35_39", "F40_44", "F45_49")
ageRowsToSumSSP <- paste("SSP",ageRowsToSum,sep="")

#pull out the relevant rows
dt.SSP.scen.F15_49 <- dt.SSP.scen[ageGenderCode %in% ageRowsToSum,]
data.table::setkeyv(dt.SSP.scen.F15_49,c("scenario", region,"year"))
#sum the relevant rows (females aged 15-49 as those that could be pregnant or lactating) by region
dt.SSP.scen.F15_49.sum <- dt.SSP.scen.F15_49[, sum(value), by = eval(data.table::key(dt.SSP.scen.F15_49))]
data.table::setnames(dt.SSP.scen.F15_49.sum, old = "V1", new = "F15_49")

#get rid of now extraneous rows in dt.SSP.scen.pop so no double counting
dt.SSP.scen <-  dt.SSP.scen[!ageGenderCode %in% ageRowsToSum, ]
# #add the dt.SSP.regionsF15_49 row to dt.SSP.scen.pop
# dt.SSP.scen <- rbind(dt.SSP.scen, dt.SSP.scen.F15_49.sum)
# #sort by region
# dt.SSP.scen <- dt.SSP.scen[order(region_code.IMPACT3), ]

#check to see if population totals are the same. Uncomment to test
# dt.tmp <- dt.SSP.scen
# data.table::setkeyv(dt.tmp, c("scenario", region,"year"))
# pop.temp <- dt.tmp[, sum(value), by = key(dt.tmp)]
# data.table::setnames(pop.temp, old = "V1", new = "value")
# dt.SSP.pop.tot <- getNewestVersion("dt.SSP.pop.tot")
# #check with AFG and for scenario == SSP3
# dt.SSP.pop.tot.AFG <- dt.SSP.pop.tot[ISO_code == "AFG" & scenario == "SSP3_v9_130115",value]
# pop.temp.AFG <- pop.temp[region_code.IMPACT3 == "AFG" & scenario == "SSP3_v9_130115",value]
# temp <- dt.SSP.pop.tot.AFG - pop.temp.AFG
# summary(temp) #the differences should be very small

#now estimate the number of pregnant women and lactating women and add them
#the estimate is based on the number of children aged 0 to 4.
#sum boys and girls 0 to 4
MF_0_4 <- c("F0_4", "M0_4")
#keepListCol <- c("scenario",region,"ageGenderCode","year","value")
#create a temporary data frame with just those rows
dt.MF_0_4 <- dt.SSP.scen[ageGenderCode %in% MF_0_4,]
data.table::setkeyv(dt.MF_0_4,c("scenario", region,"year"))
dt.MF_0_4.sum <- dt.MF_0_4[, sum(value), by = eval(data.table::key(dt.MF_0_4))]
data.table::setnames(dt.MF_0_4.sum, old = "V1", new = "MF_0_4")

#pregnant and lactating women are a constant share of kids 0 to 4. This is a *kludge*!!!
share.preg <- 0.2
share.lact <- 0.2
# temp.preg <-
#   as.data.frame(dt.kids.sum[, lapply(.SD, function(x)
#     x * share.preg), by = "region"])
dt.MF_0_4.sum[,Preg := MF_0_4 * share.preg][,Lact := MF_0_4 * share.lact]
data.table::setkey(dt.MF_0_4.sum)
data.table::setkey(dt.SSP.scen.F15_49.sum)
dt.temp <- cbind(dt.MF_0_4.sum,F15_49 = dt.SSP.scen.F15_49.sum$F15_49)
data.table::setkey(dt.temp)
deleteListCol <- c("MF_0_4","F15_49")
dt.temp[,nonPL := F15_49 - Preg - Lact][,(deleteListCol):=NULL]
data.table::setnames(dt.temp,old = "nonPL",new="F15_49")
idVarsPop <- c("scenario",region,"year" )
measureVarsPop <- c("Preg","Lact","F15_49")
dt.temp.melt <- data.table::melt(dt.temp,
                                 id.vars = idVarsPop,
                                 variable.name = "ageGenderCode",
                                 measure.vars = measureVarsPop,
                                 value.name = "value",
                                 variable.factor = FALSE)

#add new rows to dt.SSP.scen and rename
dt.pop <- rbind(dt.SSP.scen, dt.temp.melt)
#change names of age groups; prepend SSP
# somehow this next line of code adds SSP to the front of all the ageGenderCode s
dt.pop[, ageGenderCode := stringi::stri_replace_all_fixed("SSPM", "M", ageGenderCode)]
data.table::setnames(dt.pop,old = "value",new = "pop.value")

# dt.pop[, ageGenderCode := lapply(dt.pop[,ageGenderCode,with=FALSE],
#                            function(x) gsub(" ", "_", x))]
data.table::setkeyv(dt.SSP.scen,c(region,"year"))
dt.popTot <- dt.SSP.scen[, sum(value), by = eval(data.table::key(dt.SSP.scen))]
data.table::setnames(dt.popTot, old = "V1", new = "pop.tot")
#a temporary line of code for testing
nutReqName <- "req.UL.minrls.ssp"
#common.nut <- getNewestVersion()

#' Title repCons
#' Short for Representative Consumer
#' @param dt.pop - data table with population data for one scenario (at least for now)
#' @param nutReq - the short name of a nutrients requirement dataframe, eg. req.EAR.ssp
#' that has been converted to SSP age and gender groups
#' @param common.nut - list of nutrients common to the food nutrient list and the
#' requirements list
#' @param ageRowsToSum - a list of the female age groups that could potentially be pregnant.
#' It is included to so these individual rows can be deleted. They are replaced with
#' preg, lact, and nonPL, which should sum to the total of the list in ageRowsToSum
#' @param region - code for the region over which the operations should be done; e.g., region_code.IMPACT3
#' @return
#' @export
repCons <- function(dt.pop, nutReqName,ageRowsToSum,region) {
  # remove the nutrient requirements for the female age groups in ageRowsToSum
  # because they are already in dt.SSP.regionsF15_49
  nutReq <- getNewestVersion(nutReqName)
  dt.nutReq <- data.table::as.data.table(nutReq[!(nutReq$ageGenderCode %in% ageRowsToSumSSP), ])
  common.nut <- names(dt.nutReq)[2:length(dt.nutReq)]
  data.table::setkey(dt.pop, ageGenderCode)
  data.table::setkey(dt.nutReq, ageGenderCode)
  # dt.temp <- cbind(dt.pop,dt.nutReq)
  dt.temp <- merge(dt.pop,dt.nutReq, all = TRUE)
  keyValues <- c("scenario", region, "year", "ageGenderCode","pop.value")
  data.table::setkeyv(dt.temp,keyValues)
  # multiply the number of people be age and gender group by the nutritional requirements of that group
  dt.temp[, (paste(common.nut, "prod", sep = ".")) := lapply(.SD, function(x)
    x * dt.temp$pop.value), .SDcols = (common.nut)][,(common.nut):=NULL]
  keyValues <- c("scenario", region, "year")
  data.table::setkeyv(dt.temp, keyValues)
  reqlist <- c(paste(common.nut, "prod", sep = "."),"pop.value")
  # sum the nutritional requirements for all groups
  dt.temp[, (paste(reqlist, "sum", sep = ".")) := lapply(.SD, sum), by =
            keyValues, .SDcols = c(reqlist)][, c(reqlist) := NULL]
 # c("scenario",region, "year", "ageGenderCode"), .SDcols = c(reqlist)]
  dt.temp.sum <- unique(dt.temp[, c("scenario",region, "year",
                                    paste(reqlist, "sum", sep = ".")), with = FALSE])

  # data.table::setkeyv(dt.pop, c(region, "year"))
  # dt.popTot <- dt.pop[, sum(pop.value), by = eval(data.table::key(dt.pop))]
  # data.table::setnames(dt.popTot, old = "V1", new = "pop.tot")
  # data.table::setkeyv(dt.popTot, c(region, "year"))
  # data.table::setkeyv(dt.temp.sum, c(region, "year"))
  # dt.temp <- dt.popTot[dt.temp.sum]

  sumlist <- paste(common.nut, "prod.sum", sep = ".")
  finlist <- paste(common.nut, "fin", sep = ".")
  #divide every element in sumlist by pop.value and put in corresponding variable in finlist
  dt.temp.sum[,(finlist):=lapply(.SD,"/",dt.temp.sum$pop.value.sum),.SDcols=sumlist]
  dt.temp.sum[, c("pop.value.sum", sumlist) := NULL]
  dt.temp.melt <- data.table::melt(
    dt.temp.sum,
    id.vars = c("scenario",region, "year"),
    measure.vars = finlist,
    variable.name = "nutrient",
    variable.factor = FALSE,
    value.name = "value"
  )
  inDT <- dt.temp.sum
  outName <- paste(gsub(".ssp","",nutReqName),"percap",sep = ".")
  cleanup(inDT,outName,fileloc("mData"))
  temp <- data.table::dcast.data.table(
    dt.temp.melt,
    scenario + region_code.IMPACT3 + nutrient ~ year,
    fun = NULL,
    value.var = "value")
  return(temp)
}
# #convert every 5 years data to every year. this is not working so commented out.
# data.table::setkey(dt.temp.dcast,"region","nutrient")
# yrs <- seq(2010,2050,5)
# lin.interp <- function(y,yrs) predict(lm(y~yrs),data.frame(yrs=min(yrs):max(yrs)))
# dt.temp.dcast[, paste0("X",min(yrs):max(yrs)):= apply(.SD,FUN = lin.interp, yrs, MARGIN = 1), .SDcols = yearList, key=dt.temp.dcast]
#
# p1 <- as.data.frame(apply(temp.preg[,2:4],1,lin.interp,yrs))
# names(p1) <- paste0("X",min(yrs):max(yrs))

#nutrient requirements are calculated in dataPrep.nutrientRequirements.R.
reqs <- keyVariable("reqSSP")
#The list of nutrients for each is common.EAR, common.RDA.vits, common.RDA.minrls, common.RDA.macro, common.UL.vits,
#        common.UL.minrls, common.AMDR
common <-
  c( "common.EAR", "common.RDA.vits", "common.RDA.minrls", "common.RDA.macro", "common.UL.vits","common.UL.minrls")
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
EARFileName <- fileNameList("EARFileName")
creationInfo <- rbind(creationInfo,
                      paste("Nutrient requirements data:", EARFileName))
SSPdataZip <- fileNameList("SSPdataZip")
creationInfo <- rbind(creationInfo,
                      paste("SSP.regions data:", SSPdataZip))
openxlsx::addWorksheet(wb = wbGeneral, sheetName = "creation_Info")
# write the creationInfo variable to a worksheet called creation_Info
openxlsx::writeData(
  wb = wbGeneral,
  x = creationInfo,
  sheet = "creation_Info",
  startRow = 1,
  startCol = 1,
  rowNames = FALSE,
  colNames = FALSE
)
#Set up df wbInfoGeneral for common worksheet names and descriptions -----
wbInfoGeneral <-
  data.frame(sheet_Name = character(),sheet_Desc = character(),stringsAsFactors = FALSE)

wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
  c("creation_Info","Information on creator, date, model version, etc.")

wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
  c("Sheet names", "Description of sheet contents")

#add a worksheet called IMPACTCommdlist to the workbook wb = wbGeneral, with info on the commodities and nutrients
temp <- getNewestVersion("df.nutrients")
openxlsx::addWorksheet(wb = wbGeneral, sheetName = "IMPACTCommdlist")
#commodityNames <- cbind(nutrients[c("Name","IMPACT_code")])
openxlsx::writeData(
  wb = wbGeneral,
  x = temp,
  sheet = "IMPACTCommdlist",
  startRow = 1,
  startCol = 1
)
openxlsx::addStyle(
  wb = wbGeneral,
  sheet = "IMPACTCommdlist",
  style = numStyle,
  rows = 2:nrow(temp),
  cols = 3:(ncol(temp)),
  gridExpand = TRUE
)

# for loop for the nutrient requirements worksheets ----
for (i in 1:length(reqs)) {
  #nutReq <- paste(reqs[i], "dt.SSP.regions", sep = ".")
  #nutReq <- eval(parse(text = nutReq))
  dt.temp.internal <-
    repCons(dt.pop, reqs[i],ageRowsToSum,region)
  data.table::setkeyv(dt.temp.internal, c("nutrient", region))

  dt.temp.internal$nutrient <-
    gsub(".fin", "", dt.temp.internal$nutrient)

  dt.name <- paste(reqs[i], "percap", sep = ".")
  #print(dt.name)
  openxlsx::addWorksheet(wb = wbGeneral, sheetName = dt.name)
  openxlsx::addStyle(
    wb = wbGeneral,
    sheet = dt.name,
    style = numStyle,
    rows = 2:nrow(dt.temp.internal),
    cols = 3:(ncol(dt.temp.internal)),
    gridExpand = TRUE
  )
  openxlsx::writeData(
    wb = wbGeneral,
    x = dt.temp.internal,
    sheet = dt.name,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = TRUE
  )
  nutrientList <- paste(unique(dt.temp.internal$nutrient), collapse=", ")
  sheetDesc <- paste("nutrients included: ", nutrientList)
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
    c(dt.name, sheetDesc)

  dt.temp.melt <- data.table::melt(dt.temp.internal,
                                   id.vars = c(region, "nutrient"),
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

xcelOutFileName <-
  paste("results/nut.requirements", Sys.Date(), ".xlsx", sep = "")
openxlsx::saveWorkbook(wb = wbGeneral, xcelOutFileName, overwrite = TRUE)

