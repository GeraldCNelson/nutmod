
dt.food_agg.minrls <- getNewestVersion("food_agg_RDA.minrls", fileloc("resultsDir"))
keepListNuts <- c("vit_e_mg", "folate_µg", "iron_mg", "zinc_mg", "potassium_g", "riboflavin_mg", "vit_a_rae_µg", "vit_k_µg")
keepListNuts.minrls <- c( "iron_mg", "zinc_mg", "potassium_g", "calcium_mg")
keepListNuts.fg <- paste(keepListNuts.minrls, "sum.foodGroup", sep = ".")
#keepListNames <- names(dt.food_agg.minrls)[grep("sum.foodGroup",names(dt.food_agg.minrls))]
prefix <- c("scenario", "region_code.IMPACT159", "year", "food_group_code" )
keepListCol <- c(prefix,keepListNuts.fg)
DT <- dt.food_agg.minrls[, (keepListCol), with = FALSE]
DT <- unique(DT)
DT <- DT[year %in% c("X2010", "X2050")]

write.csv(DT, "minrls.csv")

dt.food_agg.vits <- getNewestVersion("food_agg_RDA.vits", fileloc("resultsDir"))
keepListNuts.vits <- c( "vit_e_mg", "folate_µg", "riboflavin_mg", "vit_a_rae_µg", "vit_k_µg")
keepListNuts.fg <- paste(keepListNuts.vits, "sum.foodGroup", sep = ".")
#keepListNames <- names(dt.food_agg.minrls)[grep("sum.foodGroup",names(dt.food_agg.minrls))]
prefix <- c("scenario", "region_code.IMPACT159", "year", "food_group_code" )
keepListCol <- c(prefix,keepListNuts.fg)
DT <- dt.food_agg.vits[, (keepListCol), with = FALSE]
DT <- unique(DT)
DT <- DT[year %in% c("X2010", "X2050")]

write.csv(DT, "vits.csv")
