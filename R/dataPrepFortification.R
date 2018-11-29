#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Copyright (C) 2018 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.
#'
#' @name dataPrepFortification.R
#' @description Adds fortification to selected nutrients
#' @name dataPrep.SingleScenario.R

gdxChoice <- "SSPs"
source("R/nutrientModFunctions.R")
source("R/aggNorder.R") # needed because of breakvalues function. Might want to move it to nutrientModFunctions.R

library(readxl)
library(maps)
library(RColorBrewer)

sourceFile <- "dataPrepFortification.R"
createScriptMetaData()

# create worldmap for later use if it doesn't exist in uData
worldMap <- getNewestVersion("worldMap", fileloc("uData"))
dt.fortValues <- as.data.table(read_excel("data-raw/NutrientData/fortification/cc356-Map_ Number of Nutrients.xlsx",
                                          col_types = c("text", "text", "numeric",
                                                        "text", "text", "numeric", "text",
                                                        "text", "text", "text", "text", "text")))
deleteListCol <- c("Income Status", "Region")
dt.fortValues[, (deleteListCol) := NULL]
setnames(dt.fortValues, old = c("Food Vehicle", "Average Value (mg/kg)", "Nutrient Level Comment", "Original Source"),
                        new = c("Food_Vehicle", "Average_Value_mgPerkg", "Nutrient_Level_Comment", "Original_Source"))
dt.fortValues <- dt.fortValues[!Food_Vehicle %in% "Salt",]
dt.fortValues[
  Nutrient %in% "B6",       Nutrient := "vit_b6_mg"][
    Nutrient %in% "B12",    Nutrient := "vit_b12_µg"][
    Nutrient %in% "Iron",   Nutrient := "iron_mg"][
    Nutrient %in% "Vitamin A", Nutrient := "vit_a_rae_µg"][
    Nutrient %in% "Vitamin D", Nutrient := "vit_d_µg"][
    Nutrient %in% "Zinc",    Nutrient := "zinc_mg"][
    Nutrient %in% "Calcium", Nutrient := "calcium_mg"][
    Nutrient %in% "Niacin",  Nutrient := "niacin_mg"][
    Nutrient %in% "Riboflavin", Nutrient := "riboflavin_mg"][
    Nutrient %in% "Thiamin", Nutrient := "thiamin_mg"][
      Nutrient %in% "Folic Acid", Nutrient := "folate_µg"][
        Nutrient %in% "Vitamin E", Nutrient := "vit_e_mg"]
# nutrients not in the nutrient modeling -  "Iodine", "Fluoride", "Selenium"
dt.fortValues <- dt.fortValues[!Nutrient %in% c("Fluoride", "Iodine", "Selenium"), ]

vegOils <- c("cgdol", "crpol", "csbol", "csfol", "ctool") # omitting palm oil and palm kern oil for now
dt.fortValues[, IMPACT_code := " "]
dt.fortValues[
  Food_Vehicle %in% "Wheat Flour", IMPACT_code := "cwhea"][
  Food_Vehicle %in% "Maize Flour", IMPACT_code := "cmaiz"][
  Food_Vehicle %in% "Rice", IMPACT_code := "crice"][
  Food_Vehicle %in% "Oil", IMPACT_code := "vegOils"]
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("uData"))
# keep only iso_code and region_code.IMPACT159
keepListCol <- c("region_code.IMPACT159", "ISO_code")
dt.regions.all[, setdiff(names(dt.regions.all), keepListCol) := NULL]

dt.fortValues <- merge(dt.fortValues, dt.regions.all, by = "ISO_code")
# deleteListCol <- c("ISO_code", "Country", "Food_Vehicle", "region_code.IMPACT115", "region_name.IMPACT115", "region_code.SSP", "FAOSTAT_code", "region_code.AggReg1",
#                    "region_name.AggReg1", "Official_name", "ISO2_code", "UNI_code",
#                    "UNDP_code", "GAUL_code", "region_name.AggReg2",
#                    "region_code.AggReg2", "region_code.EconGroup", "region_name.EconGroup",
#                    "region_code.EAPgMENg", "region_name.EAPgMENg", "region_code.WB.income", "region_name.WB.income",
#                    "Unit", "Nutrient_Level_Comment", "country_name.ISO", "region_name.IMPACT159", "Source", "Original_Source" ,
#                    "Year")
# dt.fortValues[, (deleteListCol) := NULL]
# code above replaced by next two lines Jun 9, 2018. This is needed so that if additional regions are added to dt.regions but not needed they are automatically removed.

keepListCol <- c("region_code.IMPACT159", "IMPACT_code", "Nutrient", "Average_Value_mgPerkg")
dt.fortValues[, setdiff(names(dt.fortValues), keepListCol) := NULL]
dt.fortValues <- unique(dt.fortValues)
ctyWFort <- sort(unique(dt.fortValues$region_code.IMPACT159))

#' now prepare the data to combine with dt.nutrients
#' all fortification nutrients start out as miligrams per kg of food. The following ones need to be
#' converted to micrograms - "vit_b12_µg", "vit_a_rae_µg", "vit_d_µg", "folate_µg"
dt.fortValues[Average_Value_mgPerkg %in% c("vit_b12_µg", "vit_a_rae_µg", "vit_d_µg", "folate_µg"),
              Average_Value_mgPerkg := Average_Value_mgPerkg * 1000]
# deleteListCol <- c("Unit", "Nutrient_Level_Comment", "country_name.ISO", "region_name.IMPACT159", "Source", "Original_Source" ,
#                    "Year")
#dt.fortValues[, (deleteListCol) := NULL]

# pull out just the rows with oil fortification and get them in the right form
dt.vegOils <- dt.fortValues[IMPACT_code %in% "vegOils",]
# add new columns for the veg oil IMPACT code and assign the nutrient value to each of them
dt.vegOils[,(vegOils) := Average_Value_mgPerkg]
deleteListCol <- c("IMPACT_code", "Average_Value_mgPerkg")
dt.vegOils[, (deleteListCol) := NULL]
dt.vegOils.long <- data.table::melt(
  data = dt.vegOils,
  id.vars = c("Nutrient", "region_code.IMPACT159"),
  measure.vars = vegOils,
  variable.name = "IMPACT_code",
  value.name = "value",
  variable.factor = FALSE
)
# formula.wide <- "region_code.IMPACT159 + IMPACT_code ~ Nutrient"
# dt.vegOils.wide <- data.table::dcast(
#   data = dt.vegOils.long,
#   formula = formula.wide,
#   value.var = "value")
# }
#   for (j in names(dt.vegOils.wide)) set(dt.vegOils.wide,which(is.na(dt.vegOils.wide[[j]])),j,0)

dt.fortValues <- dt.fortValues[!IMPACT_code %in% "vegOils",]
setnames(dt.fortValues, old = "Average_Value_mgPerkg", new = "value")
dt.fortValues = rbind(dt.fortValues, dt.vegOils.long)
dt.fortValues <- unique(dt.fortValues) # gets rid of some duplicates where a region such as the Caribbean has the same values for multiple countries
# RAP is the set of countries on the Arabian Peninsula. They have different fortification amounts for iron and folate
# folate is either 1.5 or 1.75. I choose 1.5 for now.
# ifron is 30,60, or 120. I choose 30 for now. (Feb 3, 2018)
dt.fortValues <- dt.fortValues[!Nutrient %in% "folate_µg.fort" & !value == 1.75,]
dt.fortValues <- dt.fortValues[!Nutrient %in% "iron_mg.fort" & !value == 60 & !value == 120,]
#' the nutrient values in dt.nutrients are in 100gm of the food item. The values from the raw data are per kg of food. So convert to 100 gm
dt.fortValues[, value := value/10]

# this seems to be not needed for the use of dt.fortValues in dataPrepUSDANuts.R so commenting it out May 31, 2018
# formula.wide <- "Nutrient + IMPACT_code ~ region_code.IMPACT159"
# dt.fortValues.wide <- data.table::dcast(
#   data = dt.fortValues,
#   formula = formula.wide,
#   value.var = "value")
#
# inDT <- dt.fortValues.wide
inDT <- dt.fortValues
outName <- "dt.fortValues"
desc <- "Fortification values by country, nutrient units per 100 gm of food"
cleanup(inDT, outName, fileloc("uData"), desc = desc)

# facet maps -----
cat("\n Working on facet maps of where fortification occurs; Saved to fileloc('gDir')\n")
worldMap <- getNewestVersion("worldMap", fileloc("uData"))

legendText <- "Nutrient quantity (mg/kg)"
fillLimits <- c(0, 100) # calcium values are 1000s of mg/kg; iron and vit e are 100s
breakValues <- generateBreakValues(fillLimits = fillLimits, decimals = 0)
#' middle two values shift the palette gradient; the code below give a smooth change
# fillRange <- fillLimits[2] - fillLimits[1]
# breakValues <- scales::rescale(c(fillLimits[1], fillLimits[1] + fillRange/3, fillLimits[1] + fillRange/1.5, fillLimits[2]))

DT <- countryCodeCleanup(dt.fortValues) # converts IMPACT region codes to ISO3 codes for largest country in the region
data.table::setnames(DT, old = c("region_code.IMPACT159"), new = c("id"))
facetColName <- "Nutrient"

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
palette <- myPalette(4)
displayOrder <- sort(unique(DT[, get(facetColName)])) # default - alphabetically sorted
fileName <- paste("facetmap", "fortify", "2050", sep = "_")
# graphsListHolder <- list()
# facetMaps(worldMap, DTfacetMap, fileName, legendText, fillLimits, palette, facetColName, graphsListHolder, breakValues, displayOrder)
# print(graphsListHolder)

b <- breakValues
f <- fillLimits
p <- palette
d <- DT
n <- facetColName
#d[, (n) := factor(get(n), levels = displayOrder)]
gg <- ggplot(data = d, aes(map_id = id))
gg <- gg + geom_map(aes(fill = value), map = worldMap)
#gg <- gg + geom_polygon(data=worldMap, aes(x=long, y=lat), colour='black', fill=NA)
# code below commented out June 1, 2018 because facet map seems to plot fine without it.
# gg <- gg + borders("world", colour = "grey80")
# gg <- gg + expand_limits(x = worldMap$long, y = worldMap$lat)
# gg <- gg + facet_wrap(facets = n)
# gg <- gg + theme(legend.position = "bottom")
# gg <- gg +  theme(axis.ticks = element_blank(),axis.title = element_blank(), axis.text.x = element_blank(),
#                   axis.text.y = element_blank(), strip.text = element_text(family = fontFamily, face = "plain"))
# # gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
# #                                 na.value = "grey50", values = b,
# #                                 guide = "colorbar", limits = f)
# gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
#                                 na.value = "grey50", # break values removed June 1, 2018
#                                 guide = "colorbar", limits = f)
# print(gg)
ggsave(file = paste0(fileloc("gDir"),"/",fileName,".png"), plot = gg,
       width = 9, height = 6)

finalizeScriptMetadata(metadataDT, sourceFile)
