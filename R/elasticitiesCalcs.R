# elasticity calculations of various kinds
{source("R/nutrientModFunctions.R")}
dt.IncElasticities <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/IMPACTdata/elasticities.xlsx", sheet = "IncomeDemandElasticity"))
dt.ownPriceDemandElasticities <- data.table::as.data.table(openxlsx::read.xlsx("data-raw/IMPACTdata/elasticities.xlsx", sheet = "OwnPriceDemandElasticity"))
keeplistYears <- c("yrs2010", "yrs2050")
dt.IncElasticities <- dt.IncElasticities[, c("CTY", "C",keeplistYears), with = FALSE]
dt.ownPriceDemandElasticities <- dt.ownPriceDemandElasticities[, c("CTY", "C",keeplistYears), with = FALSE]

# calculate ratio of meat to fruit and meat to veggie elasticities
meat <- "cbeef"
veggie <- "cvege"
fruit <- "ctemf"
dt.ownPriceDemandElasticities <- dt.ownPriceDemandElasticities[C %in% c(meat,veggie,fruit),]
dt.long <- data.table::melt(
  data = dt.ownPriceDemandElasticities,
  id.vars = c("CTY", "C"),
  measure.vars = keeplistYears,
  variable.name = "year",
  value.name = "value",
  variable.factor = FALSE
)

formula.wide <- paste("CTY + year  ~ C")

dt.wide <- data.table::dcast(
    data = dt.long,
    formula = formula.wide,
    value.var = "value"
  )

dt.wide[, ratio.veggie := cvege/cbeef][, ratio.fruit := ctemf/cbeef]

temp.2010 <- dt.wide[year %in% "yrs2010",]
temp.2050 <- dt.wide[year %in% "yrs2050",]
plot(temp.2010$ratio.veggie,temp.2010$ratio.fruit, xlim=c(0,4), ylim=c(0,11))
plot(temp.2050$ratio.veggie,temp.2050$ratio.fruit, xlim=c(0,4), ylim=c(0,11))
