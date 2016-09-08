# #convert every 5 years data to every year. this is not working so commented out.
# data.table::setkey(dt.temp.dcast,"region","nutrient")
# yrs <- seq(2010,2050,5)
# lin.interp <- function(y,yrs) predict(lm(y~yrs),data.frame(yrs = min(yrs):max(yrs)))
# dt.temp.dcast[, paste0("X",min(yrs):max(yrs)) :=  apply(.SD,FUN = lin.interp, yrs, MARGIN = 1), .SDcols = yearList, key = dt.temp.dcast]
#
# p1 <- as.data.frame(apply(temp.preg[,2:4],1,lin.interp,yrs))
# names(p1) <- paste0("X",min(yrs):max(yrs))
