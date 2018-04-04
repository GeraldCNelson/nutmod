nutChoice <- c("minrls", "vits", "macro")
# fileList <- paste("dt.food_agg", nutChoice, sep = ".")
minrlsList <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
vitsList <- c("folate_µg", "niacin_mg", "riboflavin_mg",
              "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
              "vit_b12_µg", "vit_c_mg", "vit_d_µg",  "vit_e_mg", "vit_k_µg")
macroList <- c("carbohydrate_g", "protein_g",  "totalfiber_g")
fatList <- "fat_g"
prefix <- c("scenario", "region_code.IMPACT159", "year", "food_group_code" )

for (i in nutChoice) {
  fileName <- paste("food_agg_RDA", i, sep = ".")
  dt <- getNewestVersion(fileName, fileloc("resultsDir"))
  nutList <- get(paste(i, "List", sep = ""))

  keepListNuts.fg <- paste(nutList, "sum.foodGroup", sep = ".")
  keepListCol <- c(prefix,keepListNuts.fg)
  dt <- dt[, (keepListCol), with = FALSE]
  dt <- unique(dt)
  inDT <- dt
  outName <- paste("nutAgg_FG_", i, sep = "")
}

