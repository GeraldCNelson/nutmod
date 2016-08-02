
# now aggregate population to the region, if it is more aggregated than IMPACT159
if (!region %in% names(dt.pop)) {
  # add value.sum - sum of population over each region and pop.share - share of each country's population in the total of its region
  # See http://stackoverflow.com/questions/25204859/error-while-merging-data-frames-using-data-table-package
  keepListCol <- c(region, "region_code.IMPACT159")
  dt.regions.all <- dt.regions.all[, keepListCol, with = FALSE]
  data.table::setkey(dt.regions.all)
  dt.regions.all <- unique(dt.regions.all)
  dt.temp <- merge(dt.pop,dt.regions.all, by = "region_code.IMPACT159")
  keepListCol <- c(names(dt.pop),region)
  dt.pop <- dt.temp[,keepListCol, with = FALSE]
  data.table::setkeyv(dt.pop, c("scenario", region, "year"))
  dt.pop[,pop.sum := sum(value), by = eval(data.table::key(dt.pop))][,pop.share := value/pop.sum]
}
