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
#' @description {
#'  Generates and writes out the metadata file
#'  }
source("R/nutrientModFunctions.R")
sourceFile <- "dataPrep.metadata.R"
description <- "Generates and writes out the metadata file."


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
cleanup(inDT, outName, destDir = "documentation, "csv", desc = desc)

