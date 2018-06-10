source("R/nutrientModFunctions.R")
metadata.list <- list.files(path = "data/", pattern = "metadata")
metadata.list <- metadata.list[!metadata.list %in% metadata.list[grep("metadataTot", metadata.list)]]
metadata.tot <<- data.table(outName = character(0), sourcecode = character(0), destDir = character(0), desc = character(0), colNames = character(0))
for (i in metadata.list) {
  metadata.tot <- rbind(metadata.tot, read.csv(paste0("data/", i)))
}
inDT <- metadata.tot
outName <- "dt.metadataTot"
desc <- "Metadata for all the files created"
# next line is to keep cleanup happy
metadataDT <<- data.table(outName = character(0), sourcecode = character(0), destDir = character(0), desc = character(0), colNames = character(0))
cleanup(inDT, outName, destDir = "data/", "csv", desc = desc)

