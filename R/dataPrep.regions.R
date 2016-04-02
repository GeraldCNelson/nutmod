#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, region alignment
# Intro -------------------------------------------------------------------
#' @description
#' # Intro -------------------------------------------------------------------
#' This script contains functions to align regional aggregations from country data and writes them out to
#' @param df.regions.all.rds It contains
#' @param ISO_code - official ISO 3 digit code for all countries - 249 countries
#' @param region_code.SSP - the SSP region for each ISO code (either an ISO 3 code or NA) - 194 countries
#' @param region_code.IMPACT115 - the 3 digit IMPACT115 region code for each ISO code
#' @param region_name.IMPACT115 - the name of the IMPACT115 region for each ISO code
#' @param region_code.IMPACT3 - the 3 digit IMPACT3 region code for each ISO code
#' @param region_name.IMPACT3 - the name of the IMPACT3 region for each ISO code
#' @param regions.IMPACT115 - all 115 regions in the 115 region version of IMPACT
#' @param regions.IMPACT115.plus - the 20 regions in the 115 region version of IMPACT that are aggregates of individual 'countries'
#' @param regions.IMPACT3 - all 157 regions in the IMPACT3 version of IMPACT (2015)
#' @param regions.IMPACT3.plus - the 20 regions in the IMPACT3 version of IMPACT that are aggregates of individual 'countries'
#' The 3 digit country codes are based on the ISO 3166 standard, accessed Nov 2015.
#' @source \url{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}

#' naming conventions
#' ISO_code - a 3 letter ISO code
#' country_name - a descriptive name for the country
#' region_members - one or more country codes that make up the region
#' region_name - a descriptive name for the region. Identical to the country name for regions that have only one country
#' IMPACT3 includes all the countries in the ISO list, either individually or in a plus region
#' SSP doesn't include the 56 countries in missingList.SSP. These are bunch of really small things.

#Copyright (C) 2015 Gerald C,Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
#' @include nutrientModFunctions.R
if (!exists("getNewestVersion", mode = "function")) {source("R/nutrientModFunctions.R")}

IMPACTfish <-     fileNameList("IMPACTfish")
IMPACTalcohol <-     fileNameList("IMPACTalcohol")
IMPACTstdRegions <- fileNameList("IMPACTstdRegions")
FAOCountryNameCodeLookup <- filelocFBS("FAOCountryNameCodeLookup")
ISOCodes <- filelocFBS("ISOCodes")

# create regions.ISO by reading in all ISO country codes ----
#' @param regions.ISO - ISO codes
regions.ISO <- openxlsx::read.xlsx(ISOCodes)
colnames(regions.ISO) <- c("ISO_code", "country_name.ISO")

regions.IMPACT3 <- createIMPACT3Regions()

# Create regions.IMPACT115 and regions.IMPACT115.plus ----

regions115Lookup <- openxlsx::read.xlsx(
  IMPACTfish,
  sheet = "IMPACT 115 Regions",
  cols = 1:4,
  rows = 3:118,
  colNames = TRUE
)

# Break apart the mappingCode variable into its individual countries ---
regions.IMPACT115 <-
  as.data.frame(splitstackshape::cSplit(
    regions115Lookup,
    'mappingCode',
    sep = ".",
    type.convert = FALSE
  ))

regions.IMPACT115$mappingCode_2 <-
  substring(regions.IMPACT115$mappingCode_2,
            2,
            nchar(regions.IMPACT115$mappingCode_2) - 1)
colnames(regions.IMPACT115) <-
  c("region_code.IMPACT115",
    "description",
    "mappingCode_1",
    "region_members")
regions.IMPACT115 <-
  regions.IMPACT115[, c("region_code.IMPACT115", "description", "region_members")]
temp1 <-
  stringi::stri_split_fixed(regions.IMPACT115$description, ";", simplify = TRUE)
colnames(temp1) <- c("region_name.IMPACT115", "region_members")
#get rid of extra "s (double quotes)
temp1 <- gsub('"', "", temp1[, 1:2])
#get rid of extra spaces
temp1 <- gsub(" ", "", temp1[, 1:2])
regions.IMPACT115 <-
  as.data.frame(cbind(temp1[, "region_name.IMPACT115"],
                      regions.IMPACT115[, c("region_code.IMPACT115", "region_members")],
                      stringsAsFactors = FALSE))
colnames(regions.IMPACT115) <-
  c("region_name.IMPACT115",
    "region_code.IMPACT115",
    "region_members")

temp <- data.frame(
  region_code.IMPACT115 = character(0),
  lst1 = character(0),
  region_name.IMPACT115 = character(0),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(regions.IMPACT115)) {
  lst1 <-
    unlist(strsplit(regions.IMPACT115[i, "region_members"], ","))
  regionname <- regions.IMPACT115[i, "region_name.IMPACT115"]
  regioncode <- regions.IMPACT115[i, "region_code.IMPACT115"]
  temp <- rbind(temp, plusCnst(regioncode, lst1, regionname))
}
regions.IMPACT115 <- temp
colnames(regions.IMPACT115) <-
  c("region_code.IMPACT115", "ISO_code", "region_name.IMPACT115")
regions.IMPACT <-
  merge(regions.IMPACT115, regions.IMPACT3, by = "ISO_code")

dt.SSP <- getNewestVersion("dt.SSPPopClean")

#' @param regions.SSP SSP regions
regions.SSP <-
  as.data.frame(sort(unique(dt.SSP$ISO_code)), stringsAsFactors = FALSE) #there are 193 regions

regions.SSP[, 2] <- regions.SSP[, 1]
colnames(regions.SSP) <- c("ISO_code", "region_code.SSP")
regions.all.ISO <-
  merge(regions.ISO, regions.IMPACT, by = "ISO_code", all = TRUE)
regions.all.SSP <-
  merge(regions.SSP, regions.all.ISO, by = "ISO_code", all = TRUE)

# add IMPACT3 standard world regions ----

IMPACTstdRegions <- openxlsx::read.xlsx(IMPACTstdRegions)
colnames(IMPACTstdRegions) <-
  c(
    "region_code.IMPACT3",
    "region_code.IMPACTstandard",
    "region_name.IMPACTstandard",
    "region_name.IMPACT3"
  )
IMPACTstdRegions <-
  IMPACTstdRegions[, c("region_code.IMPACT3", "region_code.IMPACTstandard")]

IMPACT.world.regions.lookup <-
  data.frame(
    region_code.IMPACTstandard =
      c("EAP", "EUR", "FSU", "LAC", "MEN", "NAM", "SAS", "SSA"),
    region_name.IMPACTstandard =
      c(
        "East Asia and Pacific",
        "Europe",
        "Former Soviet Union",
        "Latin America and Caribbean",
        "Middle East and North Africa",
        "North America",
        "South Asia",
        "Africa south of the Sahara"
      )
  )
regions.IMPACTworld <-
  merge(IMPACTstdRegions,
        IMPACT.world.regions.lookup,
        by = "region_code.IMPACTstandard",
        all = TRUE)

# create regions.all ----
regions.all <-
  merge(regions.all.SSP,
        regions.IMPACTworld,
        by = "region_code.IMPACT3",
        all = TRUE)

# Read in the worksheet that has the FAO country code-ISO country name lookup
FBSNameLookup <- openxlsx::read.xlsx(
  FAOCountryNameCodeLookup,
  sheet = 1,
  startRow = 2,
  colNames = FALSE
)
colnames(FBSNameLookup) <- c("Short.name", "Official.name", "ISO_code", "ISO2_code",
                             "UNI_code", "UNDP_code", "FAOSTAT_code", "GAUL_code")
FBSNameLookup$FAOSTAT_code <- as.character(FBSNameLookup$FAOSTAT_code)

regions.all <-
  merge(regions.all,
        FBSNameLookup,
        by = "ISO_code",
        all = TRUE)
newColOrder <- c("ISO_code", "region_code.SSP", "FAOSTAT_code","region_code.IMPACT115",
  "region_code.IMPACT3", "region_code.IMPACTstandard", "country_name.ISO",
  "region_name.IMPACT115", "region_name.IMPACT3", "region_name.IMPACTstandard",
  "Short.name", "Official.name", "ISO2_code", "UNI_code", "UNDP_code",
  "GAUL_code")
regions.all <- regions.all[newColOrder]

# remove a pesky 'country' JEY (Jersey)
deleteListRow <- c("JEY")
regions.all <-
  regions.all[!regions.all$ISO_code %in% deleteListRow, ]
regions.all <- regions.all[order(regions.all$ISO_code), ]

# #rearrange the column order
# regions.all <- regions.all[c(
#   "ISO_code",
#   "region_code.SSP",
#   "region_code.IMPACT115",
#   "region_code.IMPACT3",
#   "region_code.IMPACTstandard",
#   "country_name.ISO",
#   "region_name.IMPACT115",
#   "region_name.IMPACT3",
#   "region_name.IMPACTstandard"
# )]

inDT <- regions.all
outName <- "df.regions.all"
cleanup(inDT,outName)

