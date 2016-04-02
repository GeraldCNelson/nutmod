# A 'subroutine' of nutrientCalcs.R
# Creates and populates several worksheets of an excel file that holds modeling results
#create styles to format the worksheets
source("workBookFunctions.R")

#spreadsheet to output budget share
tmp <- f.createGeneralWorksheet()
wbIncShare <- tmp[[1]]
wbInfIncShare <- tmp[[2]]
tmp.out <-
  by(incomeShare, incomeShare[, c("scenario")], f.write.incShare.sheet, wbIncShare)
tmp <-
  as.data.frame(matrix(unlist(tmp.out), nrow = length(tmp.out), byrow = TRUE))
colnames(tmp) <- colnames(wbInfIncShare)
wbInfIncShare <- rbind(wbInfIncShare, tmp)
f.finalizeWB(wbIncShare, wbInfIncShare, nut.name = "IncShare")

#spreadsheet to output nutrition quantities
tmp <- f.createGeneralWorksheet()
wbNut <- tmp[[1]]
wbInfNut <- tmp[[2]]
tmp.out <-
  by(nutShare, nutShare[, c("food.group.code", "nutrient", "scenario")], f.write.nut.sheet, wbNut)
tmp <-
  as.data.frame(matrix(unlist(tmp.out), nrow = length(tmp.out), byrow = TRUE))
colnames(tmp) <- colnames(wbInfNut)
wbInfNut <- rbind(wbInfNut, tmp)
f.finalizeWB(wbNut, wbInfNut, short.name)

#write out spreadsheet for nutrient consumption summary
tmp <- f.createGeneralWorksheet()
wbGeneral <- tmp[[1]]
wbInfoGeneral <- tmp[[2]]
tmp.out <-
  by(nutShareTot, nutShareTot[, c("nutrient", "scenario")], f.write.nut.sum.sheet, wbNutsum)
tmp <-
  as.data.frame(matrix(unlist(tmp.out), nrow = length(tmp.out), byrow = TRUE))
colnames(tmp) <- colnames(wbInfsum)
wbInfIncShare <- rbind(wbInfsum, tmp)
f.finalizeWB(
  wb = wbNutsum,
  wbInf = wbInfsum,
  nut.name = paste(short.name, "Sum", sep = "_")
)
