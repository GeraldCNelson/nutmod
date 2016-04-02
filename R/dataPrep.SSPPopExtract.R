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
# Read in the cleaned up SSP population data -------------------------------------

dt.SSPPop <- getNewestVersion("dt.SSPPopClean")
dt.SSPPop <- dt.SSPPop[year %in% keepYearList,]

data.table::setkey(dt.SSPPop, ISO_code)
dt.regionsLU <- data.table::as.data.table(getNewestVersion("df.regions.all"))
data.table::setkey(dt.regionsLU, ISO_code)
dt.SSPPop <- dt.SSPPop[dt.regionsLU]
# Choose the region to aggregate to. The current (as of Feb 17, 2016 are "IMPACT3" and "IMPACT115" )
regionChoice <- "IMPACT3"
regionChoiceCode <- paste("region_code",regionChoice,sep=".")
regionChoiceName <- paste("region_name",regionChoice,sep=".")
keepListCol <- c("scenario","ageGenderCode","year", "value",regionChoiceCode)
dt.SSPPop <- dt.SSPPop[, keepListCol, with = FALSE]

# dt.SSPPop.melt <- melt(dt.SSPPop,
#                        id.vars = c("scenario", "ISO_code", regionChoiceCode,"ageGenderCode"),
#                        variable.name = "year",
#                        measure.vars = keepYearList)
# #data.table::setkey(dt.SSPPop.melt)
# data.table::setkeyv(dt.SSPPop.melt,
#         c("scenario","ISO_code","ageGenderCode","year",
#           regionChoiceCode))

# dt.SSP.regions <-
#   dt.SSPPop[, lapply(.SD, sum), by = c("scenario", regionChoiceCode, regionChoiceName),
#                   .SDcols = keepYearList]
dt.SSPPop <- dt.SSPPop[!is.na(scenario),]
data.table::setkeyv(dt.SSPPop, c("scenario", regionChoiceCode,"ageGenderCode","year"))
dt.SSP.regions <- dt.SSPPop[, sum(value), by = eval(data.table::key(dt.SSPPop))]
data.table::setnames(dt.SSP.regions, old = "V1", new = "value")

# end of testing
# # SSP regions
# for (j in 1:length(plusRegions)) { # loop through all the plus regions in IMPACT
#   ctyList <- eval(parse(text = plusRegions[j]))
#   for (i in 1:length(ctyList)) { #look at all the country names in a plus region to make sure they are a IIASA country
#     if(!(ctyList[i] %in% regions.SSP)) { #identify countries not in IIASA list
#       print(paste(ctyList[i],"from", plusRegions[j], "is not in IIASA countries"))
#     }
#     else {
#       ctyListNew <- c(ctyListNew,ctyList[i])
#     }
#   }
#   print(paste(ctyListNew, " is in ctyListNew for", plusRegions[j]))
#   #construct column names by adding the df name to the front of the cty name.
#   ctyList <- gsub("^","pop.wide$",ctyListNew)
#
#   #convert the list into an expression that sums across the cty columns that are in the IIASA data,
#   # and that make up the IMPACT region
#   pop.wide$tmp <- eval(parse(text = paste(ctyList, sep = "", collapse=" + ")))
#   income.wide$tmp <- eval(parse(text = paste(ctyList, sep = "", collapse=" + ")))
#   #Give the tmp column its correct name
#   names(pop.wide)[names(pop.wide)=="tmp"] <- plusRegions[j]
#   names(income.wide)[names(income.wide)=="tmp"] <- plusRegions[j]
#   #Now remove the countries that make up the IMPACT plus region
#   #ctyListNew.as.c <- c(paste(ctyListNew, sep = "\", collapse=" , "
#   pop.wide <- pop.wide[,!(names(pop.wide) %in% ctyListNew)]
#   income.wide <- income.wide[,!(names(income.wide) %in% ctyListNew)]
#
# }
# Extract lists of the scenarios, regions, and data variables ------------------------------------
scenarios <- unique(dt.SSP.regions$scenario)
scen <- scenarios[2] #"SSP2_v9_130115"
#mdl <- "IIASA-WiC POP"
dt.SSP.scen <- dt.SSP.regions[scenario == scen, ]

# start process of creating a separate list for pregnant and lactating (P/L) women----
#this list is for females who could be pregnant and lactating and have for the most part
#identical nutrient needs if they are not P/L
ageRowsToSum <- c(
  "F15_19",
  "F20_24",
  "F25_29",
  "F30_34",
  "F35_39",
  "F40_44",
  "F45_49"
)
ageRowsToSumSSP <- paste("SSP",ageRowsToSum,sep="")

#pull out the relevant rows
dt.SSP.scen.F15_49 <- dt.SSP.scen[ageGenderCode %in% ageRowsToSum,]
data.table::setkeyv(dt.SSP.scen.F15_49,c("scenario", regionChoiceCode,"year"))
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
# data.table::setkeyv(dt.tmp, c("scenario", regionChoiceCode,"year"))
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
#keepListCol <- c("scenario",regionChoiceCode,"ageGenderCode","year","value")
#create a temporary data frame with just those rows
dt.MF_0_4 <- dt.SSP.scen[ageGenderCode %in% MF_0_4,]
data.table::setkeyv(dt.MF_0_4,c("scenario", regionChoiceCode,"year"))
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
idVarsPop <- c("scenario",regionChoiceCode,"year" )
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
data.table::setkeyv(dt.SSP.scen,c(regionChoiceCode,"year"))
dt.popTot <- dt.SSP.scen[, sum(value), by = eval(data.table::key(dt.SSP.scen))]
data.table::setnames(dt.popTot, old = "V1", new = "pop.tot")
#a temporary line of code for testing
nutReq <- getNewestVersion("req.EAR.ssp")
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
#' @param regionChoiceCode - code for the region over which the operations should be done; e.g., region_code.IMPACT3
#' @return
#' @export
repCons <- function(dt.pop, nutReq,ageRowsToSum,regionChoiceCode) {
  # remove the nutrient requirements for the female age groups in ageRowsToSum
  # because they are already in dt.SSP.regionsF15_49
  dt.nutReq <- data.table::as.data.table(nutReq[!(nutReq$ageGenderCode %in% ageRowsToSumSSP), ])
  temp <- names(dt.nutReq)
  common.nut <- temp[2:length(temp)]
  data.table::setkey(dt.pop, ageGenderCode)
  data.table::setkey(dt.nutReq, ageGenderCode)
 # dt.temp <- cbind(dt.pop,dt.nutReq)
  dt.temp <- merge(dt.pop,dt.nutReq, all = TRUE)
  # multiply the number of people be age and gender group by the nutritional requirements of that group
  dt.temp[, (paste(common.nut, "prod", sep = ".")) := lapply(.SD, function(x)
    x * dt.temp$pop.value), .SDcols = c(common.nut)]
  data.table::setkeyv(dt.temp, c("scenario",regionChoiceCode, "year"))
  reqlist <- paste(common.nut, "prod", sep = ".")
  # sum the nutritional requirements for all groups
  dt.temp[, (paste(common.nut, "sum", sep = ".")) := lapply(.SD, sum), by =
            c(regionChoiceCode, "year"), .SDcols = c(reqlist)]
  #remove the .prod columns
  dt.temp[, c(reqlist) := NULL]
   dt.temp.sum <-
    unique(dt.temp[, c(regionChoiceCode, "year", paste(common.nut, "sum", sep = ".")), with =
                     FALSE])

  # dt.pop <- dt.SSP.scen.tot
  # dt.pop <- melt(
  #   dt.pop,
  #   id.vars = "region",
  #   measure.vars = yearList,
  #   variable.name = "year",
  #   variable.factor = FALSE,
  #   value.name = "pop.tot"
  # )

  data.table::setkeyv(dt.pop, c(regionChoiceCode, "year"))
  dt.popTot <- dt.pop[, sum(pop.value), by = eval(data.table::key(dt.pop))]
  data.table::setnames(dt.popTot, old = "V1", new = "pop.tot")
  data.table::setkeyv(dt.popTot, c(regionChoiceCode, "year"))
  data.table::setkeyv(dt.temp.sum, c(regionChoiceCode, "year"))
  dt.temp <- dt.popTot[dt.temp.sum]

  sumlist <- paste(common.nut, "sum", sep = ".")
  finlist <- paste(common.nut, "fin", sep = ".")
  #divide every element in sumlist by pop.value and put in corresponding variable in finlist
  dt.temp[,(finlist):=lapply(.SD,"/",dt.temp$pop.tot),.SDcols=sumlist]
  dt.temp[, c("pop.tot", sumlist) := NULL]
  dt.temp.melt <- data.table::melt(
    dt.temp,
    id.vars = c(regionChoiceCode, "year"),
    measure.vars = finlist,
    variable.name = "nutrient",
    variable.factor = FALSE,
    value.name = "value"
  )
  temp <- data.table::dcast.data.table(
    dt.temp.melt,
    region_code.IMPACT3 + nutrient ~ year,
    fun = NULL,
    value.var = "value")
  return(temp)
  # #convert every 5 years data to every year. this is not working so commented out.
  # data.table::setkey(dt.temp.dcast,"region","nutrient")
  # yrs <- seq(2010,2050,5)
  # lin.interp <- function(y,yrs) predict(lm(y~yrs),data.frame(yrs=min(yrs):max(yrs)))
  # dt.temp.dcast[, paste0("X",min(yrs):max(yrs)):= apply(.SD,FUN = lin.interp, yrs, MARGIN = 1), .SDcols = yearList, key=dt.temp.dcast]
  #
  # p1 <- as.data.frame(apply(temp.preg[,2:4],1,lin.interp,yrs))
  # names(p1) <- paste0("X",min(yrs):max(yrs))
}

#nutrient requirements are calculated in EARfoodGroupCSEloading.R. the next line is a list as of April 1, 2016
reqs <-
  c("req.EAR.ssp", "req.RDA.vits.ssp","req.RDA.minrls.ssp", "req.RDA.macro.ssp","req.UL.vits.ssp", "req.UL.minrls.ssp"
  )
#The list of nutrients for each is common.EAR, common.RDA.vits, common.RDA.minrls, common.RDA.macro, common.UL.vits,
#        common.UL.minrls, common.AMDR
common <-
  c( "common.EAR", "common.RDA.vits", "common.RDA.minrls", "common.RDA.macro", "common.UL.vits","common.UL.minrls"
  )
# code that creates and writes out the nutrient requirements files
# first set up a workbook, wb = wbGeneral, to hold the information
wbGeneral <- openxlsx::createWorkbook()

#create a variable, creationInfo, with info on creator, date, model version, etc.
creationInfo <-
  ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo, paste("Creator:", keyVariable("userName")))
creationInfo <-
  rbind(creationInfo, paste("Date of file creation:", Sys.Date()))
#creationInfo <- rbind(creationInfo, paste("IMPACT data:", IMPACTfileName))
nutrientFileName <- fileNameList("nutrientFileName")
creationInfo <-
  rbind(creationInfo, paste("Nutrient data:", nutrientFileName))
EARFileName <- fileNameList("EARFileName")
creationInfo <-
  rbind(creationInfo,
        paste("Nutrient requirements data:", EARFileName))
SSPdataZip <- fileNameList("SSPdataZip")
creationInfo <-
  rbind(creationInfo,
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
#Set up a dataframe, wbInfoGeneral, to collect common worksheet names and descriptions.
wbInfoGeneral <-
  data.frame(
    sheet_Name = character(),
    sheet_Desc = character(),
    stringsAsFactors = FALSE
  )
#openxlsx::class(wbInfoGeneral$sheet_Name) <- 'hyperlink'
wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
  c("creation_Info",
    "Information on creator, date, model version, etc.")

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
# wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
#   c("IMPACTCommdList",
#     "IMPACT commodities and their nutrient content")
# #add a worksheet called MetaDataNutrnts to the workbook wb = wbGeneral
# openxlsx::addWorksheet(wb = wbGeneral, sheetName = "MetaDataNutrnts")
# openxlsx::writeData(
#   wb = wbGeneral,
#   x = req.metadata,
#   sheet = "MetaDataNutrnts",
#   startRow = 1,
#   startCol = 1,
#   rowNames = FALSE,
#   colNames = FALSE
# )
# wbInfoGeneral[(nrow(wbInfoGeneral) + 1), ] <-
#   c("MetaDataNutrnts", "Information about the requirements sources")

# for loop to write out the nutrient requirements files ----
for (i in 1:length(reqs)) {
  nutReq <- getNewestVersion(reqs[i])
  #nutReq <- paste(reqs[i], "dt.SSP.regions", sep = ".")
  #nutReq <- eval(parse(text = nutReq))
  dt.temp.internal <-
    repCons(dt.pop, nutReq,ageRowsToSum,regionChoiceCode)
  data.table::setkeyv(dt.temp.internal, c("nutrient", regionChoiceCode))

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

  assign(dt.name, dt.temp.internal)
  #  setnames(dt.temp.internal,dt.temp.internal,dt.name)
 # fname <- paste("results/", dt.name,Sys.Date(), ".rds", sep = "")
  #print(fname)

  inDT <- eval(parse(text = dt.name))
  outName <- dt.name
  cleanupResults(inDT,outName)
  #save(eval(parse(text = dt.name)), file = fname)
  #fileEncoding = "Windows-1252"
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

#I think everything from here on down is extraneous, but I'm keeping it for now, just commented out.
#
#
# for (req.type in type.list) {
#   for (nutrient in req.type) {
#     dt.nut <- as.data.table(EARs[grep(nutrient,EARs$NutCode),])
#     print(nutrient)
#     dt.nut[,nutNames.Units:=NULL]
#     dt.nut.melt <- melt(dt.nut,variable.name = "ageGenderCode", id.vars = "NutCode",
#                         value.name = "nut.value", stringsAsFactors = FALSE)
#     dt.nut.melt[,ageGenderCode:=as.character(ageGenderCode)]
#     dt.nut.melt[,NutCode:=NULL]
#     data.table::setkey(dt.nut.melt,"ageGenderCode")
#     #merge dt.pop and dt.nut.melt
#     dt.pop.nut <- dt.pop[dt.nut.melt]
#     data.table::setkey(dt.pop.nut,"region","year")
#     dt.pop.nut[,val:=sum(pop.value*nut.value),by=key(dt.pop.nut)]
#     xx <- unique(dt.pop.nut[,c("region","year","val"),with=F])
#     dt.regionYear <- dt.regionYear[xx] #tot amount of nutrient needed by all (sum of nutrient needed by age group over all age groups)
#     dt.regionYear[,percapval:=val/pop.tot,by=key(dt.regionYear)]
#     #  data.table::setkey(dt.regionYear,"region","year")
#     setnames(dt.regionYear,"val",nutrient)
#     setnames(dt.regionYear,"percapval",paste(nutrient,"_percap",sep=""))
#   }
# }
# temp <- melt(dt.regionYear,id.vars=c("region","year"),measure.vars = c("energy_percap", "protein_percap", "fat_percap",
#                                                                        "carbohydrate_percap", "fiber_percap",
#                                                                        "calcium_percap", "iron_percap", "magnesium_percap",  "phosphorus_percap",
#                                                                        "potassium_percap",     "sodium_percap","zinc_percap",
#                                                                        "vit_c_percap",    "thiamin_percap",  "riboflavin_percap","niacin_percap", "vit_b6_percap", "folate_percap",
#                                                                        "vit_b12_percap", "vit_a_IU_percap", "vit_e_percap", "vit_d_percap"),
#              variable.name = "nutrient", value.name = "percapEAR")
#
# temp[,nutrient:=as.character(nutrient)]
# temp.wide <- dcast(temp, region +  nutrient ~ year, value.var = "percapEAR")
# # energy_percap + protein_percap + fat_percap +
# # carbohydrate_percap + fiber_percap +
# #   calcium_percap + iron_percap + magnesium_percap +  phosphorus_percap +
# #   potassium_percap +     sodium_percap +zinc_percap +
# #   vit_c_percap +    thiamin_percap +  riboflavin_percap +niacin_percap + vit_b6_percap + folate_percap +
# #   vit_b12_percap + vit_a_IU_percap + vit_e_percap + vit_d_percap
# write.csv(dt.regionYear, file = "results/regionyear.csv")
# write.csv(temp.wide, file = "results/repConsumerByYear.csv")
#
#
# #convert every 5 years data to every year
# for (i in seq(2010,2050,5)) temp.wide[[paste0("X",i)]] <- rowMeans(dt.SSP.regionsDat.NCAR[,paste0("X",c(i-5,i+5))])
#

# # get the regions used by IMPACT
# regions.IMPACT <- openxlsx::read.xlsx(IMPACTregionsFileName, colNames = TRUE, sheet = 1)
#
# # make sure that the regions used by both IMPACT and IIASA are included
# regions.combined <- merge(regions.IMPACT,regions, by="CTY", all = TRUE)
#
# # reorder IIASA population data so the country codes are the columns
#
# pop.wide <- spread(mpop,REGION,value)
