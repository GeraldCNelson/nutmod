#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, region alignment
# Intro -------------------------------------------------------------------
#' @description
#' # Intro -------------------------------------------------------------------
#' This script contains functions to align regional aggregations from country data and writes them out to
#' @param dt.regions.all.rds It contains
#' @param ISO_code - official ISO 3 digit code for all countries - 249 countries
#' @param region_code.SSP - the SSP region for each ISO code (either an ISO 3 code or NA) - 194 countries
#' @param region_code.IMPACT115 - the 3 digit IMPACT115 region code for each ISO code
#' @param region_name.IMPACT115 - the name of the IMPACT115 region for each ISO code
#' @param region_code.IMPACT159 - the 3 digit IMPACT159 region code for each ISO code
#' @param region_name.IMPACT159 - the name of the IMPACT159 region for each ISO code
#' @param regions.IMPACT115 - all 115 regions in the 115 region version of IMPACT
#' @param regions.IMPACT115.plus - the 20 regions in the 115 region version of IMPACT that are aggregates of individual 'countries'
#' @param regions.IMPACT159 - all 157 regions in the IMPACT159 version of IMPACT (2015)
#' @param regions.IMPACT159.plus - the 20 regions in the IMPACT159 version of IMPACT that are aggregates of individual 'countries'
#' The 3 digit country codes are based on the ISO 3166 standard, accessed Nov 2015.
#' @source \url{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}

#' naming conventions
#' ISO_code - a 3 letter ISO code
#' country_name - a descriptive name for the country
#' region_members - one or more country codes that make up the region
#' region_name - a descriptive name for the region. Identical to the country name for regions that have only one country
#' IMPACT159 includes all the countries in the ISO list, either individually or in a plus region
#' SSP doesn't include the 56 countries in missingList.SSP. These are bunch of really small things.

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
source("R/nutrientModFunctions.R")
library(readxl)
sourceFile <- "dataPrep.regions.R"
createScriptMetaData()

# update Aug 28, 2016. Most of the code below has been superceded by the spreadsheet IMPACT regions update Aug 28, 2016
# It is now mainly to be used to add new regions.
#only run in the dataPrep.regions.R script
# source("R/createIMPACT159Regions.R")

regionsLookup <- read_excel(fileNameList("regionsLookup"), sheet = "dt.regions.all", na = "NA",
                                          col_types = c("text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text"))
dt.regions.all <- data.table::as.data.table(regionsLookup)

inDT <- dt.regions.all
data.table::setorder(inDT, ISO_code)
outName <- "dt.regions.all"
desc <- "A lookup table for all the ways countries can be classified"
cleanup(inDT,outName,fileloc("uData"), desc = desc)
finalizeScriptMetadata(metadataDT, sourceFile)



