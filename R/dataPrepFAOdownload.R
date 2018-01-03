library(FAOSTAT)
{source("R/nutrientModFunctions.R")
  source("R/workbookFunctions.R")
  source("R/nutrientCalcFunctions.R")}
# FAOsearch(3)

# group codes and names; production, trade, food balance, etc.
FAOmetaTable$groupTable

# elements needed for nutrition modeling
# domainCode, elementCode, elementName
# CL or CC, 641, Food supply quantity (tonnes) (tonnes)
# CC or FBS, 645, Food supply quantity (kg/capita/yr) (Kg)

# yearsToDownload <- keyVariable("keepYearList.FBS")
# # make this numeric
# yearsToDownload <- as.numeric(gsub("X", "", yearsToDownload))

yearsToDownload <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)
# FBSCommodityInfo <- filelocFBS("FBSCommodityInfo")
# dt.FBScommodLookup <- data.table::as.data.table(openxlsx::read.xlsx(FBSCommodityInfo,
#                                                                     sheet = 1,
#                                                                     startRow = 1,
#                                                                     cols = 1:7,
#                                                                     colNames = TRUE))
#
# itemCodeforIMPACT <- as.character(dt.FBScommodLookup$item_code)

itemCodeforIMPACT <- as.character(c(2659, 2769, 2768, 2775, 2656, 2765, 2761, 2781, 2782, 2766, 2767, 2762,
                     2764, 2763, 2658, 2657, 2655, 2615, 2513, 2546, 2731, 2735, 2736, 2740,
                     2737, 2630, 2532, 2633, 2744, 2572, 2556, 2516, 2732, 2514, 2848, 2743,
                     2517, 2520, 2515, 2547, 2549, 2680, 2534, 2642, 2551, 2645, 2576, 2562,
                     2616, 2577, 2733, 2531, 2734, 2805, 2558, 2574, 2571, 2573, 2557, 2518,
                     2555, 2614, 2625, 2613, 2612, 2611, 2618, 2619, 2537, 2536, 2542, 2745,
                     2541, 2543, 2533, 2635, 2617, 2620, 2560, 2575, 2561, 2559, 2578, 2579,
                     2582, 2581, 2602, 2640, 2641, 2601, 2605, 2511, 2535, 2570, 2563, 2586,
                     2580, 2899))

#set up query
FAOquery.df = data.frame(varName = c("foodMT"),
                         domainCode = c("CL"),
                         itemCode = itemCodeforIMPACT,
                         elementCode = c(641),
                         stringsAsFactors = FALSE)
## Download the data from FAOSTAT
FAO.lst = with(FAOquery.df,
               getFAOtoSYB(name = varName, domainCode = domainCode,
                           itemCode = itemCode, elementCode = elementCode,
                           useCHMT = TRUE, outputFormat = "wide", yearRange = yearsToDownload))

#FAO.lst$entity[, "arableLand"] = as.numeric(FAO.lst$entity[, "arableLand"])

# getWDI and getWDItoSYB functions ----------------------------------------

## Download World Bank data and meta-data
WB.lst = getWDItoSYB(indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"),
                     name = c("totalPopulation", "GDPUSD"),
                     getMetaData = TRUE, printMetaData = TRUE)

# group code, domain code and domain names (eg. crops, crops processed, live animals, food balance sheets, etc)
FAOmetaTable$domainTable

# #domain code, item codes, item names; eg Apples, Banana, Barley (not they are all first letter capitalized)
# FAOmetaTable$itemTable
#
# # domain code, item code, and item name; eg  Cereals (Rice Milled Eqv) + (Total)
# FAOmetaTable$itemAggTable
#
# # domain code, element code, element name; eg, Gross Production Value (current million US$) (USD), Total Population - Both sexes (1000)
# FAOmetaTable$elementTable
