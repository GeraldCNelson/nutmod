#' Nutrient Modeling Functions for the shiny app
#' title: "Global functions needed to make the nutrientModeling shiny app work"
#' @keywords utilities, nutrient data management functions, shiny app
#' @name global.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @description
#' This script contains functions that are needed across the shiny app. It has duplicates of key functions
#' from nutrientModFunctions.R. The shiny specific scripts are towards the end.

#Copyright (C) 2015-2018 Gerald C,Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

ggRadar2 <- function (data, mapping = NULL, rescale = TRUE, legend.position = "top", 
                      colour = "red", alpha = 0.3, size = 3, ylim = NULL, scales = "fixed", 
                      use.label = FALSE, interactive = FALSE, nrow = 1, ...) 
{
  data = as.data.frame(data)
  (groupname = setdiff(names(mapping), c("x", "y")))
  groupname
  mapping
  length(groupname)
  if (length(groupname) == 0) {
    groupvar <- NULL
  }
  else {
    groupvar = getMapping(mapping, groupname)
  }
  groupvar
  facetname <- colorname <- NULL
  if ("facet" %in% names(mapping)) {
    facetname <- getMapping(mapping, "facet")
  }
  (colorname = setdiff(groupvar, facetname))
  if ((length(colorname) == 0) & !is.null(facetname)) 
    colorname <- facetname
  data = num2factorDf(data, groupvar)
  (select = sapply(data, is.numeric))
  if ("x" %in% names(mapping)) {
    xvars = getMapping(mapping, "x")
    xvars
    if (length(xvars) < 3) 
      warning("At least three variables are required")
  }
  else {
    xvars = colnames(data)[select]
  }
  (xvars = setdiff(xvars, groupvar))
  if (rescale) 
    data = rescale_df(data, groupvar)
  temp = sjlabelled::get_label(data)
  cols = ifelse(temp == "", colnames(data), temp)
  if (is.null(groupvar)) {
    id = newColName(data)
    data[[id]] = 1
    longdf = reshape2::melt(data, id.vars = id, measure.vars = xvars)
  }
  else {
    cols = setdiff(cols, groupvar)
    longdf = reshape2::melt(data, id.vars = groupvar, measure.vars = xvars)
  }
  temp = paste0("ddply(longdf,c(groupvar,'variable'),summarize,mean=mean(value,na.rm=TRUE))")
  df = eval(parse(text = temp))
  colnames(df)[length(df)] = "value"
  df
  groupvar
  if (is.null(groupvar)) {
    id2 = newColName(df)
    df[[id2]] = "all"
    id3 = newColName(df)
    df[[id3]] = 1:nrow(df)
    df$tooltip = paste0(df$variable, "=", round(df$value, 
                                                1))
    df$tooltip2 = paste0("all")
    p <- ggplot(data = df, aes_string(x = "variable", y = "value", 
                                      group = 1)) + geom_polygon_interactive(aes_string(tooltip = "tooltip2"), 
                                                                             colour = colour, fill = colour, alpha = alpha) + 
      geom_point_interactive(aes_string(data_id = id3, 
                                        tooltip = "tooltip"), colour = colour, size = size)
  }
  else {
    if (!is.null(colorname)) {
      id2 = newColName(df)
      df[[id2]] = df[[colorname]]
    }
    id3 = newColName(df)
    df[[id3]] = 1:nrow(df)
    df$tooltip = paste0(groupvar, "=", df[[colorname]], "<br>", 
                        df$variable, "=", round(df$value, 1))
    df$tooltip2 = paste0(groupvar, "=", df[[colorname]])
    p <- ggplot(data = df, aes_string(x = "variable", y = "value", 
                                      colour = colorname, fill = colorname, group = colorname)) + 
      geom_polygon_interactive(aes_string(tooltip = "tooltip2"), 
                               alpha = alpha) + geom_point_interactive(aes_string(data_id = id3, 
                                                                                  tooltip = "tooltip"), size = size)
  }
  p
  if (!is.null(facetname)) {
    formula1 = as.formula(paste0("~", facetname))
    p <- p + facet_wrap(formula1, scales = scales, nrow = 1)
  }
  p <- p + xlab("") + ylab("") + theme(legend.position = legend.position)
  if (use.label) 
    p <- p + scale_x_discrete(labels = cols)
  p <- p + coord_radar()
  if (!is.null(ylim)) 
    p <- p + expand_limits(y = ylim)
  p
  if (interactive) {
    tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
    hover_css = "r:4px;cursor:pointer;stroke-width:6px;"
    selected_css = "fill:#FF3333;stroke:black;"
    p <- ggiraph(code = print(p), tooltip_extra_css = tooltip_css, 
                 tooltip_opacity = 0.75, zoom_max = 10, hover_css = hover_css, 
                 selected_css = selected_css)
  }
  p
}

generateBreakValues <- function(fillLimits, decimals) {
  fillRange <- fillLimits[2] - fillLimits[1]
  breakValue.low <- round(fillLimits[1], digits = decimals)
  breakValue.high <- round(fillLimits[2], digits = decimals)
  breakValues <- round(c(breakValue.low, breakValue.low + fillRange/3, breakValue.low + fillRange/1.5, breakValue.high), digits = decimals)
  return(breakValues)
}

fileloc <- function(variableName) {
  gdxChoice <- paste0(getwd(), "/data/gdxInfo.csv")
  rawData <- "data-raw"
  mData <- "data"
  gDir <- "graphics"
  iData <- mData
  nutData <- "data-raw/NutrientData"
  resultsTop <- "results"
  return(eval(parse(text = variableName)))
}

#' getNewestVersion
#'
#' @param fileShortName The substantive (first) part of the file name.
#' @return The most recent file.
#'
#' @export
getNewestVersion <- function(fileShortName, directory, fileType) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  if (missing(fileType)) {fileType <- "rds"}
  
  # see
  # http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  # regExp <- paste("(?=^", fileShortName, ")(?=.*rawData$)", sep = "")
  #  regExp <- paste("(?=^", fileShortName, ")(?=.*", fileType, "$)", sep = "")
  # stringChar <- unlist(strsplit(list.files(mData)[1], ".", fixed = TRUE))
  # # this is still a potential problem. If the first file in the directory doesn't have the same date
  # dateOfFirst <- stringChar[length(stringChar) - 1]
  # tailLength <- 15 # to remove data and the period and csv or rds
  # if (fileType == "xlsx") tailLength <- 16 # for xlsx files
  # fillIn <- paste('.{', tailLength, '}$', sep = "")
  fileShortNameTest <- paste(fileShortName,"_2", sep = "") # this should get rid of the multiple files problem
  filesofFileType <- list.files(mData)[grep(fileType,list.files(mData))]
  
  # Note: added grepteststring, changed grep to grepl and fixed to FALSE June 14, 2018
  greptestString <- paste0("^", fileShortNameTest)
  fileLongName <- filesofFileType[grepl(greptestString, filesofFileType, fixed = FALSE)]
  if (length(fileLongName) == 0) {
    cat("\nCan't find ", fileShortName, " in  directory ", mData, "\n")
    stop(sprintf("\nThere is no file  '%s' in directory %s, \n", fileShortName, mData))
  }
  if (length(fileLongName) > 1) {
    cat("\nTwo versions of ", fileShortName, " in  directory ", mData, "\n")
    stop(sprintf("\nTwo or more files with  '%s' in their names in directory %s, \n", fileShortName, mData))
  }
  outFile = readRDS(paste(mData, fileLongName, sep = "/"))
  return(outFile)
}

removeOldVersions <- function(fileShortName,dir) {
  removeFcn <- function(regExp) {
    oldVersionList <-
      grep(regExp,
           list.files(dir),
           value = TRUE,
           perl = TRUE)
    if (length(oldVersionList) > 0) {
      file.remove(paste(dir, oldVersionList, sep = "/"))
    }
  }
  # remove .rawData versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*rawData$)", sep = "")
  removeFcn(regExp)
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  removeFcn(regExp)
  regExp <- paste("(?=^", fileShortName, ")(?=.*xlsx$)", sep = "")
  removeFcn(regExp)
  # remove .csv versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*csv$)", sep = "")
  removeFcn(regExp)
}

#capitalize words
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if (strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

cleanupScenarioNames <- function(dt.ptemp) {
  # replaces - with _ and removes 2 from a couple of USAID scenario
  dt.ptemp[, scenario := gsub("IRREXP-WUE2", "IRREXP_WUE2", scenario)]
  dt.ptemp[, scenario := gsub("PHL-DEV2", "PHL_DEV2", scenario)]
  dt.ptemp[, scenario := gsub("HGEM2", "HGEM", scenario)]
  dt.ptemp[, scenario := gsub("IPSL2", "IPSL", scenario)]
  return(dt.ptemp)
}

# cleanupNutrientNames <- function(nutList) {
#   nutList <- gsub("food_group_code", "Food group", nutList)
#   nutList <- gsub("_g.reqRatio","",nutList)
#   nutList <- gsub("reqRatio","",nutList)
#   nutList <- gsub("vit_","Vitamin ",nutList)
#   nutList <- gsub("_µg","",nutList)
#   nutList <- gsub("_mg","",nutList)
#   nutList <- gsub("_rae"," rae",nutList)
#   nutList <- gsub("_g","",nutList)
#   nutList <- gsub("totalfiber","total fiber",nutList)
#   nutList <- gsub(".ratio.foodGroup","",nutList)
#   nutList <- gsub(".sum.all","",nutList)
#   nutList <- gsub("_sum.all","",nutList)
#   nutList <- gsub("rootsNPlantain","Roots\nand plantain",nutList)
#   nutList <- gsub("nutsNseeds","Nuts\nand seeds",nutList)
#   nutList <- gsub("beverages","Beverages,\nother",nutList)
#   nutList <- gsub("alcohol","Beverages,\nalcoholic",nutList)
#   nutList <- gsub("NonAlcoholic Beveragesic Beverages","Alcoholic beverages",nutList)
#   nutList <- gsub("ft_acds_tot_sat", "Saturated fat", nutList)
#   nutList <- gsub("_g_AMDR", "", nutList)
#   nutList <- gsub("foodgroup_code", "Food group", nutList)
#   return(nutList)
# }

cleanupNutrientNames <- function(nutList) {
  nutList <- gsub("_g_reqRatio","",nutList)
  nutList <- gsub("reqRatio","",nutList)
  nutList <- gsub("vit_","Vitamin ",nutList)
  nutList <- gsub("_µg","",nutList)
  nutList <- gsub("_mg","",nutList)
  nutList <- gsub("_g","",nutList)
  nutList <- gsub("totalfiber","total fiber",nutList)
  nutList <- gsub(".ratio.foodGroup","",nutList)
  nutList <- gsub(".sum.all","",nutList)
  nutList <- gsub("rootsNPlantain","roots and plantain",nutList)
  nutList <- gsub("nutsNseeds","nuts and oilseeds",nutList)
  nutList <- gsub("alcohol","alcoholic beverages",nutList)
  nutList <- gsub("beverages","nonalcoholic beverages",nutList)
  nutList <- gsub("alcoholic nonalcoholic beverages","alcoholic beverages",nutList)
  nutList <- gsub("ft_acds_tot_sat", "saturated fat", nutList)
  nutList <- gsub("_g_AMDR", "", nutList)
  nutList <- gsub("Vitamin b", "Vitamin B", nutList)
  nutList <- gsub("Vitamin c", "Vitamin C", nutList)
  nutList <- gsub("Vitamin d", "Vitamin D", nutList)
  nutList <- gsub("Vitamin e", "Vitamin E", nutList)
  nutList <- gsub("Vitamin k", "Vitamin K", nutList)
  nutList <- TC(nutList)
  nutList <- gsub("A_rae","A (RAE)",nutList)
  return(nutList)
}

cleanupNutrientNamesFacetGraph <- function(nutList) {
  nutList <- capwords(nutList)
  nutList <- gsub("_g.reqRatio","",nutList)
  nutList <- gsub("reqRatio","",nutList)
  nutList <- gsub("Vit_","Vitamin ",nutList)
  nutList <- gsub("_µg"," (µg)",nutList)
  nutList <- gsub("_mg"," (mg)",nutList)
  nutList <- gsub("_rae"," RAE",nutList)
  nutList <- gsub("_g"," (g)",nutList)
  nutList <- gsub("totalfiber","total fiber",nutList)
  nutList <- gsub(".ratio.foodGroup","",nutList)
  # nutList <- gsub("_share","",nutList)
  nutList <- gsub(".sum.all","",nutList)
  nutList <- gsub("_sum.all","",nutList)
  nutList <- gsub("RootsNPlantain","Roots and plantain",nutList)
  nutList <- gsub("NutsNseeds","Nuts and oilseeds",nutList)
  nutList <- gsub("Beverages","Beverages, other",nutList)
  nutList <- gsub("Alcohol","Beverages, alcoholic",nutList)
  nutList <- gsub("alcoholic nonalcoholic beverages","alcoholic beverages",nutList)
  nutList <- gsub("Ft_acds_tot_sat", "saturated fat", nutList)
  nutList <- gsub("_g_AMDR", "", nutList)
  
  return(nutList)
}



countryNameLookup <- function(countryCode, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  if (!countryCode %in% dt.regions.all$region_code.IMPACT159) {
    stop(sprintf("The country code you entered (%s) is not in the lookup table", countryCode))
  } else {
    countryName <- dt.regions.all[region_code.IMPACT159 == countryCode,region_name.IMPACT159]
    return(countryName)
  }
}
countryCodeLookup <- function(countryName, directory) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  if (!countryName %in% countryNames) {
    stop(sprintf("The country name you entered (%s) is not in the lookup table", countryName))
  } else {
    #' this next step is somewhat kludgy. EG there are four countries represented in CHM
    countryCode <- unique(dt.regions.all[region_name.IMPACT159 == countryName, region_code.IMPACT159])
    return(countryCode)
  }
}

countryCodeCleanup <- function(dt) {
  #converts IMPACT code to ISO3 code for largest country in the region.
  dt <- dt[region_code.IMPACT159 %in% "FRP", region_code.IMPACT159 := "FRA"]
  dt <- dt[region_code.IMPACT159 %in% "CHM", region_code.IMPACT159 := "CHN"]
  dt <- dt[region_code.IMPACT159 %in% "CHP", region_code.IMPACT159 := "CHE"]
  dt <- dt[region_code.IMPACT159 %in% "DNP", region_code.IMPACT159 := "DNK"]
  dt <- dt[region_code.IMPACT159 %in% "FNP", region_code.IMPACT159 := "FIN"]
  dt <- dt[region_code.IMPACT159 %in% "ITP", region_code.IMPACT159 := "ITA"]
  dt <- dt[region_code.IMPACT159 %in% "MOR", region_code.IMPACT159 := "MAR"]
  dt <- dt[region_code.IMPACT159 %in% "SPP", region_code.IMPACT159 := "ESP"]
  dt <- dt[region_code.IMPACT159 %in% "UKP", region_code.IMPACT159 := "GBR"]
  dt <- dt[region_code.IMPACT159 %in% "BLX", region_code.IMPACT159 := "BEL"]
  dt <- dt[region_code.IMPACT159 %in% "SDP", region_code.IMPACT159 := "SDN"]
  dt <- dt[region_code.IMPACT159 %in% "RAP", region_code.IMPACT159 := "ARE"]
  dt <- dt[region_code.IMPACT159 %in% "GSA", region_code.IMPACT159 := "SUR"]
  dt <- dt[region_code.IMPACT159 %in% "CRB", region_code.IMPACT159 := "TTO"]
  dt <- dt[region_code.IMPACT159 %in% "OSA", region_code.IMPACT159 := "SIN"]
  dt <- dt[region_code.IMPACT159 %in% "BLT", region_code.IMPACT159 := "LTU"]
  dt <- dt[region_code.IMPACT159 %in% "OBN", region_code.IMPACT159 := "SRB"]
  dt <- dt[region_code.IMPACT159 %in% "OAO", region_code.IMPACT159 := "CPV"]
  dt <- dt[region_code.IMPACT159 %in% "OIO", region_code.IMPACT159 := "MDV"]
  dt <- dt[region_code.IMPACT159 %in% "OPO", region_code.IMPACT159 := "WSM"]
  return(dt)
}

library(scales)
# old version replaced with new version Nov 1, 2018

facetMaps <- function(mapFile, DTfacetMap, legendText, fillLimits, palette, facetColName,
                      breakValues, displayOrder) {
  numLimits = 4
  bb <- generateBreakValues(fillLimits = fillLimits, numLimits = numLimits, decimals = 0)
  b <- rescale(bb, to = c(0,1))
  f <- fillLimits
  p <- palette
  n <- facetColName
  displayOrder <- gsub("X", "", displayOrder)
  d <- data.table::copy(DTfacetMap)
  d[, (n) := factor(get(n), levels = displayOrder)]
  keepListCol <- c("id", facetColName, "value")
  if (!isTRUE(all.equal(sort(names(d)), sort(keepListCol)))) {d[, setdiff(names(d), keepListCol) := NULL]} # this format added Oct 30, 2018
  d <- unique(d)
  gg <- ggplot(data = d, aes(map_id = id))
  gg <- gg + geom_map(aes(fill = value), map = mapFile, color=NA, size = 0.5) # "#ffffff" is white
  gg <- gg + expand_limits(x = mapFile$long, y = mapFile$lat)
  gg <- gg + ggthemes::theme_map()
  gg <- gg + facet_wrap(facets = n)
  gg <- gg + theme(legend.position = "bottom")
  gg <- gg +  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text.x = element_blank(),
                    axis.text.y = element_blank(), strip.text = element_text(family = fontFamily, face = "plain", size = 12))
  
  gg <- gg + scale_fill_gradientn(colors = p, name = legendText,
                                  na.value = "grey50",
                                  guide = "colorbar") #, values = bb, breaks = f, limits = f, labels = f, aesthetics = "fill")
  
  return(gg) # If this is returned and not captured by gg <- xxx then it appears in the plot window of rstudio
  
}

# # use this function to get a tablegrob. Which can then be placed in a plot. This is done in aggNorder.R
g_legend <- function(plot.in) {
  tmp <- ggplot2::ggplot_gtable(ggplot_build(plot.in))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

# code specifically for shiny app -----
years <- c("X2010", "X2030", "X2050")
yearsClean <- gsub("X", "", years)
fontFamily <- "Helvetica Neue"
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
dt.scenarioListIMPACT[,scenario := gsub("-REF", "", scenario)]
dt.scenarioListIMPACT[, scenario := gsub("-", "_", scenario)]
scenarioNames <- unique(dt.scenarioListIMPACT$scenario)
scenarioNames <- scenarioNames[!scenarioNames %in% c( "SSP2-IPSL", "SSP2-MIROC", "SSP2-GFDL")]

scenarioList <- scenarioNames # added Aug 27, 2018 because scenarioList now used in the app. Might want to change this
resultFileLookup <- getNewestVersion("resultFileLookup", fileloc("mData"))
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("mData"))
dt.regions.all[, region_name.IMPACT159 := gsub(" plus", "", region_name.IMPACT159)] # used to get to country code
dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo", fileloc("mData"))
countryNamesPlus <- sort(unique(dt.regions.all$region_name.IMPACT159))
countryNames <- gsub(" plus", "", countryNamesPlus)

# development files -----
"dt.metadata" <- getNewestVersion("dt.metadataTot", fileloc("mData"))
"dt.IMPACTgdxParams" <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
FGreqChoices <- c("macro nutrients", "minerals", "vitamins")
staplesReqChoices <- c("energy","macro nutrients", "minerals", "vitamins")
initialCountryName <- "Afghanistan"
initialCountryCode <- countryCodeLookup(initialCountryName, fileloc("mData"))

spiderGraphData <- function(countryName, scenarioName, dt, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  dt <- dt[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
  dt[, year := gsub("X", "", year)]
  formula.wide <- sprintf("scenario + region_code.IMPACT159 + year ~ %s", displayColumnName)
  dt <- dcast(data = dt, formula = formula.wide, value.var = "value")
  return(dt)
}

spiderGraphData2 <- function(countryName, dt, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  dt <- dt[region_code.IMPACT159 %in% countryCode,]
  dt[, year := gsub("X", "", year)]
  formula.wide <- sprintf("scenario + region_code.IMPACT159 + nutrientType + year ~ %s", displayColumnName)
  dt <- dcast(data = dt, formula = formula.wide, value.var = "value")
  return(dt)
}

graphData <- function(countryName, scenarioName, dt, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  dt[, year := gsub("X", "", year)]
  dt <- dt[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
  if (displayColumnName %in% "nutrient") dt[, nutrient := cleanupNutrientNames(nutrient)]
  if (displayColumnName %in% "food_group_code") dt[, food_group_code := cleanupNutrientNames(food_group_code)]
  return(dt)
}

spiderGraphOutput <- function(spiderData, scenarioName) {
  countryCode <- unique(spiderData$region_code.IMPACT159)
  countryName <- countryNameLookup(countryCode)
  spiderData[, region_code.IMPACT159 := NULL]
  #  data.table::setnames(spiderData, old = names(spiderData), new = capwords(names(spiderData)))
  titleText <- paste("Country: ", countryName)
  p <- ggRadar2(data = spiderData, mapping = aes(colour = year, facet = "scenario"), nrow = 1, #, facet = "nutrientType"
                rescale = FALSE, interactive = FALSE, size = 1,
                legend.position = "bottom")
  p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                           face = "plain")) + ggtitle(titleText)
  p <- p + theme(axis.text = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p + theme(legend.text = element_text(size = 10, family = fontFamily, face = "plain"))
  return(p)
}

loadNresize <- function(dt) {
  temp <- getNewestVersion(dt, fileloc("mData"))
  temp <- (temp[year %in% years])
  temp[, scenario := gsub("-REF", "", scenario)] # added Aug 9, 2018
  temp[, scenario := gsub("-", "_", scenario)]
  temp <- temp[scenario %in% scenarioNames]
  assign(dt, temp, envir = .GlobalEnv) # this puts the data sets into the global environment
  return(temp) # changed to temp Aug 9, 2018
}

load_data <- function(dataSetsToLoad) {
  #' load data that are not year or scenario specific; these are handled in the observe code in the server
  #' development files
  dt.metadata <- getNewestVersion("dt.metadataTot", fileloc("mData"))
  dt.IMPACTgdxParams <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
  
  withProgress(message = 'Loading data', value = 0, {
    #' Number of times we'll go through the loop
    n <- length(dataSetsToLoad)
    
    for (i in 1:n) {
      #' load the data
      dt <- loadNresize(dataSetsToLoad[i])
      assign(dataSetsToLoad[i], dt)
      #' Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Loading file", i, "of", n))
    }
  })
  
  shinyjs::hide("loading_page", anim = FALSE, animType = "fade", time = 0.5)
  shinyjs::show("mainTabsetPanel", anim = TRUE, animType = "fade", time = 0.5)
}

# replaced and edited Nov 1, 2018
plotByRegionBarAMDR <- function(dt, fileName, plotTitle, yLab, yRange, aggChoice, suffix, scenOrder, colorList, AMDR_lo, AMDR_hi, graphsListHolder, plotErrorBars) {
  plotTitle <- capwords(plotTitle)
  temp <- copy(dt)
  regionCodes <- unique(temp$region_code)
  regionNames <- unique(temp$region_name)
  scenarios <- unique(temp$scenario)
  temp[, scenario := gsub("-REF", "", scenario)]
  scenOrder <- gsub("-REF", "", scenOrder)
  regionNameOrder <- getRegionOrder(aggChoice, regionNames)
  scenarioNameOrder <- scenOrder
  temp[, region_name := gsub(" income", "", region_name)]
  temp[, region_name := factor(region_name, levels =  regionNameOrder)]
  temp[, scenario := factor(scenario, levels = scenarioNameOrder)]
   # adjust location of bar label in the bar (yval) for graph type
  if (yLab %in% "(Adequacy ratio)") {roundVal = 2} else { roundVal = 1}
  yval = 1.5 # controls how far above the y axis bottom the vertical numbers are
  fontsize <- 2.5
  
  #' draw bars
  #  pdf(paste(fileloc("gDir"),"/", fileName, ".pdf", sep = ""), width = 7, height = 5.2)
  if (round(max(temp$value) - yRange[2]) == 0) yRange[2] <- max(temp$value) # will hopefully deal with rare situation
  #' when all elements of value are the same as the max y range
  p <- ggplot(temp, aes(x = region_name, y = value, fill = scenario, order = c("region_name") )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black" ),
      panel.background = element_blank()
    ) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = fontFamily, face = "plain")) +
    theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
    scale_fill_manual(values = colorList) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = fontFamily, face = "plain")) +
    ggtitle(plotTitle) +
    labs(y = yLab, x = NULL) +
    geom_hline(aes(yintercept = AMDR_lo), color = "darkgreen", show.legend = FALSE) +
    geom_label(aes(x = .6, y = AMDR_lo + 2, label = "Lower limit", family = fontFamily), fill= "white", color = "black", nudge_x = 0.25) + # value after AMDR_lo shifts the label up or down
    #   geom_text(aes(.6, AMDR_lo + 2.5, label = "Low", family = fontFamily), color = "black") + # value after AMDR_lo shifts the label up or down
    geom_hline(aes(yintercept = AMDR_hi),  color = "dark red", show.legend = FALSE) +
    geom_label(aes(x = .6, y = AMDR_hi + 2, label = "Upper limit"), nudge_x = 0.25, fill= "white", color = "black")
  # next line adds the vertical numbers
  p <- p + geom_text(aes(label = formatC( round(value, roundVal), format='f', digits = roundVal),
                         x = factor(region_name), y = yval), position = position_dodge(0.9),
                     size = fontsize, angle = 90, color = "black")  # + vjust = "bottom", hjust = 'left', commented out April 8, 2018
  
  ggsave(filename = paste0(fileloc("gDir"),"/",fileName,".png"), plot = p,
         width = 7, height = 6)
  return(p)
}


load_data_special <- function(data_name){
  print("exists(data_name)")
  print(exists(data_name))
  print("is.data.table(data_name")
  print(is.data.table(get(data_name)))
  print(head(get(data_name)))
  if (!exists((data_name))) {
    print("exists(data_name)2")
    print(exists(data_name))
    withProgress(message = 'Loading data',  {
      temp <- getNewestVersion(data_name, fileloc("mData"))
      temp <- (temp[year %in% years])
      temp[, scenario := gsub("-REF", "", scenario)] # added Aug 9, 2018
      temp[, scenario := gsub("-", "_", scenario)]
      temp <- temp[scenario %in% scenarioNames]
      assign(data_name, temp, envir = .GlobalEnv) # this puts the data sets into the global environment
      return(temp)
      incProgress(1)}
    )
  }
}

pivotWideToWideYear <- function(inData) {
  dt <- data.table::copy(inData)
  namelist <- names(dt)[!names(dt) %in% "nutrientType"]
  idVars <- c("scenario", "region_code.IMPACT159", "year")
  measureVars <- namelist[!namelist %in% idVars]
  dt.long  <- data.table::melt(
    data = dt,
    id.vars = idVars,
    measure.vars = measureVars,
    variable.name = "nutrient_foodGroup",
    value.name = "value",
    variable.factor = FALSE
  )
  dt.long <- dt.long[!value %in% NA,] # added Nov 8, 2018. This is here only to deal with situation when all nutrients are in one file.
  formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient_foodGroup  ~ year")
  dt.wide <- data.table::dcast(
    data = dt.long,
    formula = formula.wide,
    value.var = "value")
  return(dt.wide)
}

facetGraphData <- function(countryName, scenarioName, inData, facetColumnName, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "food_group_code"
  if (missing(facetColumnName)) facetColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  dt <- data.table::copy(inData)
  dt <- dt[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
  newOrder <- c(names(dt)[!names(dt) %in% facetColumnName], facetColumnName)
  data.table::setcolorder(dt,neworder = newOrder)
  return(dt)
}

facetGraphOutput <- function(inData, facetColumnName, displayColumnName, foodGroupNames, foodGroupNamesNoWrap) {
  dt <- data.table::copy(inData)
  scenarioName <- unique(dt$scenario)
  countryCode <- unique(dt$region_code.IMPACT159)
  countryName <- countryNameLookup(countryCode)
  dt[,c("scenario", "region_code.IMPACT159") := NULL]
  if (facetColumnName %in% "nutrient") dt[, nutrient := capwords(cleanupNutrientNamesFacetGraph(nutrient))]
  if (displayColumnName %in% "food_group_code") {
    for (i in 1:length(foodGroupNames)) {
      dt[food_group_code == foodGroupNames[i], food_group_code := foodGroupNamesNoWrap[i]]
    }
  }
  
  titleText <- paste("Country: ", countryName, "Scenario: ", scenarioName)
  facetColumnName <- capwords(facetColumnName)
  p <- ggplot(data = dt, mapping = aes(fill = factor(year), x = food_group_code, y = value)) +
    scale_fill_discrete(name = "Year") +
    geom_col(position = "dodge") + facet_wrap(~nutrient, scales = "free_x", ncol = 3) +
    coord_flip() # + theme(plot.margin = unit(c(1, 1, 1, 1), "null"))
  p <- p + theme(plot.title = element_text(hjust = 0.5, size = 10, family = fontFamily,
                                           face = "plain")) + ggtitle(titleText)
  p <- p + theme(axis.text.x = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p + theme(axis.text.y = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p + theme(axis.title.y = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p + theme(legend.text = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p + theme(legend.title = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p + theme(strip.text.x = element_text(size = 10, family = fontFamily, face = "plain"))
  p <- p +  xlab("food group") + ylab(NULL)
  return(p)
}

barGraphData <- function(countryName, inData) {
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  dt <- data.table::copy(inData)
  dt <- dt[region_code.IMPACT159 %in% countryCode,]
  AMDRNuts <- c("scenario", "year", "region_code.IMPACT159", "carbohydrate_g.kcalpercent", "fat_g.kcalpercent", "protein_g.kcalpercent")
  deleteListCol <- names(dt)[!names(dt)  %in% AMDRNuts]
  dt[, (deleteListCol) := NULL]
  dt <- dt[scenario %in% scenarioNames, ]
  dt <- unique(dt)
  return(dt)
}

plotByRegionBarAMDRinShiny <- function(barData, yLab) {
  temp <- data.table::copy(barData)
  countryCode <- unique(temp$region_code.IMPACT159)
  countryName <- countryNameLookup(countryCode)
  temp <- temp[region_code.IMPACT159 %in% countryCode, ]
  plotTitle <- paste("AMDR plots for ", countryName, sep = "")
  temp  <- data.table::melt(
    data = temp,
    id.vars = c("scenario", "region_code.IMPACT159", "year"),
    measure.vars = c("carbohydrate_g.kcalpercent", "fat_g.kcalpercent", "protein_g.kcalpercent"),
    variable.name = "nutrient",
    value.name = "value",
    variable.factor = FALSE
  )
  
  temp[, nutrient := gsub("_g.kcalpercent", "", nutrient)]
  AMDR_hi.carbohydrate <- 65
  AMDR_hi.fat <- 35
  AMDR_hi.protein <- 30
  AMDR_lo.carbohydrate <- 45
  AMDR_lo.fat <- 25
  AMDR_lo.protein <- 10
  int_hi <- c(AMDR_hi.carbohydrate, AMDR_hi.fat, AMDR_hi.protein)
  int_lo = c(AMDR_lo.carbohydrate, AMDR_lo.fat, AMDR_lo.protein)
  slope <- c(0,0,0)
  nutrient = c("carbohydrate", "fat", "protein")
  ref_hi <- data.frame(int_hi, slope, nutrient)
  ref_lo <- data.frame(int_lo, slope, nutrient)
  
  #select color explicitly
  scenOrder <- scenarioNames
  paletteChoice <- "OrRd" #choices are described in the help for RcolorBrewer
  
  #put here so its easy to see alignment of colors and bars
  colorsNeeded <- length(scenarioNames)
  colorList <- c("#000000", brewer.pal(colorsNeeded, paletteChoice))
  
  # a kludge to make the climate scenario green (#2ca25f)
  colorList[3] <- "#2CA25F"
  yval = 1.5 # controls how far above the y axis bottom the vertical numbers are
  fontsize <- 2.5
  
  p <- 
    ggplot(temp, aes(x = year, y = value, fill = scenario )) +
    geom_bar_interactive(stat = "identity", position = "dodge") +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black" ),
      panel.background = element_blank()
    ) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = fontFamily, face = "plain")) +
    theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
    scale_fill_manual(values = colorList) +
    scale_y_continuous(limits = c(0,80)) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, family = fontFamily, face = "plain")) +
    ggtitle(plotTitle) +
    labs(y = yLab, x = NULL) +
    geom_abline(data = ref_hi, aes(intercept = int_hi, slope = slope), color = "red", size = 1) +
    geom_abline(data = ref_lo, aes(intercept = int_lo, slope = slope), color = "darkgreen", size = 1) +
    geom_label(data = ref_lo, aes(x = .6, y = int_lo + 2, label = paste0("Lower limit is ", int_lo), family = fontFamily), fill= "white", color = "black", nudge_x = 1.25, nudge_y = 0.25) + # value after AMDR_lo shifts the label up or down
    geom_label(data = ref_hi, aes(x = .6, y = int_hi + 2, label = paste0("Upper limit is ", int_hi)), nudge_x = 1.25, nudge_y = 0.25, fill= "white", color = "black") +
     facet_wrap( ~ nutrient, scales = "fixed")
  return(p)
}

