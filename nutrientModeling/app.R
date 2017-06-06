# To make sure data and script files are up to date, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio
library(shiny)
library(shinyjs)
library(rsconnect)
library(data.table)
# library(dplyr) # to do %>%
library(dtplyr)
library(shinythemes)
library(ggiraphExtra) #to do the interactive spider graphs. As of May 27, 2017, needs to be installed with devtools::install_github('cardiomoon/ggiraphExtra')
#library(markdown)
require(ggiraph)
library(RColorBrewer)
library(shiny.router) # so you can link to a specific page
source("global.R") # load all the background functions
options(repos = c(CRAN = "https://cran.rstudio.com"))

#' files for the development tab section
FGreqChoices <- c("macro nutrients", "minerals", "vitamins")
staplesReqChoices <- c("energy","macro nutrients", "minerals", "vitamins")
initialCountryName <- "India"
initialScenarioName <- "SSP2-HGEM-REF"
initialCountryCode <- countryCodeLookup(initialCountryName, fileloc("mData"))
userCountryChoice <- initialCountryName # until user chooses something different in the first tab
userScenarioChoice <- initialScenarioName # until user chooses something different in the first tab
# rsconnect::setAccountInfo(name = 'nutrientmodeling',
#                           token = '3E8A9773C46C19C6EF42EE20C8C65BF0',
#                           secret = 'hk4UOGDwRKr5Pkw2hKMzSxcRqL0GRsoU67shiwR/')
# rsconnect::deployApp(appDir = paste(getwd(),"nutrientModeling", sep = "/"))

facetGraphData <- function(countryName, scenarioName, inData, facetColumnName, displayColumnName) {
  if (missing(displayColumnName)) displayColumnName <- "food_group_code"
  if (missing(facetColumnName)) facetColumnName <- "nutrient"
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  DT <- data.table::copy(inData)
  DT <- DT[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
  # formula.wide <- sprintf("scenario + region_code.IMPACT159 + year + %s ~ %s", facetColumnName, displayColumnName)
  # DT <- data.table::dcast(data = DT, formula = formula.wide, value.var = "value")
  newOrder <- c(names(DT)[!names(DT) %in% facetColumnName], facetColumnName)
  data.table::setcolorder(DT,neworder = newOrder)
  return(DT)
}

facetGraphOutput <- function(inData, facetColumnName, displayColumnName, scenarioName, countryName) {
  DT <- data.table::copy(inData)
  DT <- DT[scenario %in% scenarioName]
  # inData[, scenario := gsub("-REF", "", scenario)]
  DT[,c("scenario", "region_code.IMPACT159") := NULL]
  #  names.new <- cleanupNutrientNamesFacetGraph(names(inData))
  #  data.table::setnames(inData, old = names(inData), new = capwords(names.new))
  if (facetColumnName %in% "nutrient") inData[, nutrient := capwords(cleanupNutrientNamesFacetGraph(nutrient))]
  if (displayColumnName %in% "food_group_code") {
    DT[, food_group_code := cleanupNutrientNames(food_group_code)]
  }

  titleText <- paste("Country: ", countryName, "Scenario: ", scenarioName)
  facetColumnName <- capwords(facetColumnName)
  p <- ggplot(data = DT, mapping = aes(fill = factor(year), x = food_group_code, y = value)) +
    scale_fill_discrete(name = "Year") +
    geom_col(position = "dodge") + facet_wrap(~nutrient, scales = "free_x", ncol = 3) +
    coord_flip() + theme(plot.margin = unit(c(1, 1, 1, 1), "null"))

  # p <- ggRadar(data = spiderData, mapping = aes_string(colour = colourName, facet = facetColumnName),
  #              rescale = FALSE, interactive = FALSE, size = 2, scales = "fixed",
  #              legend.position = "right")
  p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                           face = "plain")) + ggtitle(titleText)
  p <- p + theme(axis.text.x = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p + theme(axis.text.y = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p + theme(axis.title.y = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p + theme(legend.text = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p + theme(legend.title = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p + theme(strip.text.x = element_text(size = 12, family = fontFamily, face = "plain"))
  p <- p +  xlab("food group") + ylab(NULL)
  return(p)
}

barGraphData <- function(countryName, inData) {
  countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  DT <- data.table::copy(inData)
  DT <- DT[region_code.IMPACT159 %in% countryCode,]
  AMDRNuts <- c("scenario", "year", "region_code.IMPACT159", "carbohydrate_g.Q", "fat_g.Q", "protein_g.Q")
  deleteListCol <- names(DT)[!names(DT)  %in% AMDRNuts]
  DT[, (deleteListCol) := NULL]
  DT <- DT[scenario %in% scenarioNames, ]
  DT <- unique(DT)
  return(DT)
}

plotByRegionBarAMDRinShiny <- function(barData, yLab) {
  temp <- data.table::copy(barData)
  countryName <- countryNameLookup(unique(temp$region_code.IMPACT159), fileloc("mData"))
  plotTitle <- paste("AMDR plots for ", countryName, sep = "")
  temp  <- data.table::melt(
    data = temp,
    id.vars = c("scenario", "region_code.IMPACT159", "year"),
    measure.vars = c("carbohydrate_g.Q", "fat_g.Q", "protein_g.Q"),
    variable.name = "nutrient",
    value.name = "value",
    variable.factor = FALSE
  )
  #  scenOrder <- gsub("-REF", "", scenOrder)
  temp[, nutrient := gsub("_g.Q", "", nutrient)]
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

  # draw bars
  #  if (round(max(temp$value) - yRange[2]) == 0) yRange[2] <- max(temp$value) # will hopefully deal with rare situation
  # when all elements of value are the same as the max y range

  p <- ggplot(temp, aes(x = year, y = value, fill = scenario )) +
    geom_bar_interactive(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colorList) +
    facet_wrap( ~ nutrient, scales = "fixed") +
    #    scale_y_continuous(name = yLab) +
    # needs to be geom_line. geom_hline shows up on all facets
    geom_abline(data = ref_hi, aes(intercept = int_hi, slope = slope), color = "red", size = 1) +
    geom_abline(data = ref_lo, aes(intercept = int_lo, slope = slope), color = "green", size = 1) +
    theme(strip.text.x = element_text(family = fontFamily, face = "plain")) +
    ylab(yLab)

  p = p + theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = fontFamily, face = "plain")) +
    theme(axis.title.y = element_text(family = fontFamily, face = "plain")) +
    theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily, face = "plain")) +
    ggtitle(plotTitle)
  return(p)
}

# Define UI -----
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  title = "Nutrient modeling",

  useShinyjs(debug = TRUE),
  div(
    id = "loading_page",
    #    h1("Loading...")
    includeHTML("www/introText.html")
  ),
  hidden( # used to make the tabs not appear until the data sets are finished loading
    div(id = "mainTabsetPanel",
        tabsetPanel(
          # Introduction tab panel -----
          tabPanel(title = "Introduction",
                   sidebarLayout(
                     sidebarPanel(width = 2,
                                  includeHTML("www/countryChoice.html"),
                                  selectizeInput(inputId = "userCountryName", label = "Choose a country", choices = countryNames, selected = NULL),
                                  selectizeInput(inputId = "userScenarioName", label = "Choose a scenario", choices = scenarioNames, selected = initialScenarioName)),
                     mainPanel(includeHTML("www/introText.html")))),
          # Affordability tab panel ------
          tabPanel(title = "Affordability",
                   sidebarLayout(
                     sidebarPanel(width = 2,
                                  selectizeInput(inputId = "affordabilityCountryName", label = "Choose a country", choices = countryNames),
                                  downloadButton("downloadData.afford", "Download data")),
                     mainPanel(titlePanel("Affordability of the current diet"),
                               includeHTML("www/affordabilityText.html"),
                               tableOutput("affordabilityTable")))),

          # availability tab panel ------
          tabPanel(title = "Availability",
                   sidebarLayout(
                     sidebarPanel(width = 2,
                                  selectizeInput(inputId = "availabilityCountryName", label = "Choose a country", choices = countryNames),
                                  selectizeInput(inputId = "availabilityScenarioName", label = "Choose a scenario (see definition in glossary)", choices = scenarioNames),
                                  downloadButton("downloadData.avail", "Download data")),
                     mainPanel(titlePanel("Average daily availability by food group"),
                               includeHTML("www/availabilityText.html"),
                               ggiraphOutput("availabilitySpiderGraphP1"),
                               tableOutput("availabilityTableP1")))),

          # Adequacy tab panel ------
          tabPanel(title = "Under- and over nutrition",
                   tabsetPanel(
                     # adequacy tab panel -----
                     tabPanel(title = "Adequacy ratios",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "adequacyCountryName", label = "Choose a country", choices = countryNames),
                                             selectizeInput(inputId = "adequacyScenarioName", label = "Choose a scenario (see definition in glossary)", choices = scenarioNames),
                                             downloadButton("downloadData.adequacy.macro", "Download macro data"),
                                             downloadButton("downloadData.adequacy.vits", "Download vitamin data"),
                                             downloadButton("downloadData.adequacy.minrls", "Download minerals data"),
                                             downloadButton("downloadData.energyRat", "Download energy share data"),
                                             downloadButton("downloadData.energyQ", "Download kcal data")

                                ),
                                mainPanel(
#                                  titlePanel("Dietary adequacy"),
                                          includeHTML("www/adequacyText.html"),
                                          fluidRow(
                                            column(width = 6, ggiraphOutput("adequacySpiderGraphP1")),
                                            column(width = 6, ggiraphOutput("adequacySpiderGraphP2"))),
                                          fluidRow(
                                            column(width = 6, tableOutput("adequacyTableP1")),
                                            column(width = 6, tableOutput("adequacyTableP2"))),
                                          fluidRow(
                                            column(width = 12), ggiraphOutput("adequacySpiderGraphP3")),
                                          fluidRow(
                                            column(width = 12, tableOutput("adequacyTableP3"))),
                                          fluidRow(
                                            column(width = 6, ggiraphOutput("energyQuantityBarPlot")),
                                            column(width = 6, ggiraphOutput("energyShareBarPlot"))),
                                          fluidRow(
                                            column(width = 6, tableOutput("energyQuantityTable")),
                                            column(width = 6, tableOutput("energyShareTable")))))),

                     # AMDR tab panel -----
                     tabPanel(title = "Acceptable Macronutrient Distribution Range (AMDR)",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "AMDRCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.AMDR", "Download data")),
                                #                     mainPanel(tableOutput("affordabilityTable")))),
                                mainPanel(
#                                  titlePanel("Acceptable Macronutrient Distribution Range"),
                                          includeHTML("www/AMDRText.html"),
                                          fluidRow(column(width = 12, ggiraphOutput("AMDRbarGraphP1"))),
                                          fluidRow(column(width = 12, tableOutput("AMDRTableP1")))))),

                     # nutrient balance tab panel -----
                     tabPanel(title = "Nutrient Balance Score",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "nutbalCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.nutbal", "Download data")),
                                mainPanel(
#                                  titlePanel("Nutrient balance score"),
                                          includeHTML("www/nutbalGraphText.html"),
                                          tableOutput("nutbalTableP1")))),

                     # MRV tab panel -----
                     tabPanel(title = "Maximum Recommended Intake",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "MRVCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.MRV", "Download data")),
                                mainPanel(
#                                  titlePanel("Maximum Recommended Intake"),
                                          includeHTML("www/MRVText.html"),
                                          fluidRow(
                                            column(width = 4, tableOutput("MRVTableP2")),
                                            column(width = 4, tableOutput("MRVTableP1")),
                                            column(width = 4, tableOutput("MRVTableP3")))))))),

          # Diversity tab panel, with tabset ------
          tabPanel("Dietary diversity",
                   tabsetPanel(
                     # Shannon diversity tab panel ------
                     tabPanel(title = "Shannon diversity index",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "diversityCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.ShannonDiversity", "Download data")),
                                mainPanel(
#                                  titlePanel("Shannon diversity index"),
                                          includeHTML("www/shannonDiversityText.html"),
                                          tableOutput("diversityTable")))),

                     # Nut diversity by food group tab panel ------

                     tabPanel(title = "Nutrient diversity by food group",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "FGcountryName", label = "Choose a country", choices = countryNames),
                                             selectizeInput(inputId = "FGscenarioName", label = "Choose a scenario", choices = scenarioNames),
                                             selectizeInput(inputId = "nutrientGroup", label = "Choose a nutrient group", choices = c("macronutrients", "vitamins", "minerals")),
                                             downloadButton("downloadData.nutDiversity", "Download data")),
                                mainPanel(
#                                  titlePanel("Nutrient diversity by food group"),
                                          includeHTML("www/foodGroupSpiderGraphText.html"),
                                          plotOutput("NutDiverFGspiderGraphP1", height = "800px"),
                                          tableOutput("NutDiverFGTable"),
                                          includeHTML("www/nutrientDescription.html")))),

                     # nonstaple energy share tab panel ------
                     tabPanel(title = "Nonstaple share of dietary energy",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "nonstapleEnergyShareCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.nonStapleShare", "Download data")),
                               mainPanel(
# titlePanel("Nonstaple share of dietary energy"),
                                          includeHTML("www/nonStapleShareGraphText.html"),
                                          tableOutput("nonStapleEnergyShareTable")))),

                     # Rao's QE tab panel ------
                     tabPanel(title = "Rao's quadratic entropy metric",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "RaosQECountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.RaoQE", "Download data")),
                                mainPanel(
#                                  titlePanel("Rao's quadratic entropy metric"),
                                          includeHTML("www/RaosQEGraphText.html"),
                                          tableOutput("RaosQETable")))))),

          # glossary tab panel ------
          tabPanel(title = "Glossary",
                   mainPanel(
                     includeHTML("www/glossaryText.html"))),
          # data and developer information tabs with tabset -----
          tabPanel(
            "Data and Developer Info",
            tabsetPanel(
              # Metadata tab panel -----
              tabPanel(
                title = "Metadata",
                mainPanel(
                  "Information for developers", " ",
                  fluidRow(column(width = 12, div(tableOutput("metadataTable"), style = "font-size:80%", family = fontFamily))))),
              # IMPACT metadata tab panel -----
              tabPanel(title = "IMPACT metadata",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(tableOutput("IMPACTmetadataTable"), style = "font-size:80%", family = fontFamily))))),
              # Foodgroup lookup panel -----
              tabPanel(title = "Food group lookup table",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(tableOutput("IMPACTfoodgroupTable"), style = "font-size:80%", family = fontFamily))))),
              # nutrient lookup -----
              tabPanel(title = "Nutrient lookup table",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(tableOutput("nutrientLookup"), style = "font-size:80%", family = fontFamily))))),
              # File documentation -----
              tabPanel(title = "File documentation",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(tableOutput("fileDocumentation"), style = "font-size:80%", family = fontFamily))))))),
          # Acknowledgements -----
          tabPanel(title = "Acknowledgements",
                   mainPanel(
                     includeHTML("www/acknowledgementsText.html")))
        )
    )
  )
)

server <- function(input, output, session) {

  load_data() # load most of the data. Big files can be loaded elsewhere

  #not sure this is the best place for this. Need to get crude out of budget share. Just do once
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "pcGDPX0", "budget.PCX0", "incShare.PCX0")
  deleteListCol <- names(dt.budgetShare)[!names(dt.budgetShare) %in% keepListCol]
  dt.budgetShare[, (deleteListCol) := NULL]
  colsToRound <- c("pcGDPX0", "budget.PCX0", "incShare.PCX0")
  dt.budgetShare[, (colsToRound) := round(.SD, 2), .SDcols = colsToRound]
  formula.budgetShare <- paste("scenario + region_code.IMPACT159  ~ year")
  dt.budgetShare.wide <- data.table::dcast(
    dt.budgetShare,
    formula = formula.budgetShare,
    value.var = c("pcGDPX0", "budget.PCX0", "incShare.PCX0"))

  # set up data download

  # foodAfford reactive -----
  data.afford <- reactive({
    countryName <- input$affordabilityCountryName
    DT <- data.table::copy(dt.budgetShare.wide)
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    DT <- DT[region_code.IMPACT159 %in% countryCode, ]
    DT[, scenario := factor(scenario, levels = scenarioNames)]
    DT[, scenario := gsub("-REF", "", scenario)]
    oldNames <- names(DT)
    newNames <- gsub("pc", "Per Capita ", oldNames)
    newNames <- gsub(".PCX0_X", "\n", newNames)
    newNames <- gsub("X0_X", "\n", newNames)
    newNames <- gsub("incShare", "Share of income", newNames)
    newNames <- gsub("budget", "Food expenditures", newNames)
    newNames <- gsub("region_code.IMPACT159", "country code", newNames)
    setnames(DT, old = oldNames, new = newNames)
    DT
  })

  # country name choice observer -----
  observe({
    countryName <- input$userCountryName
    # Can also set the label and select items
    updateSelectInput(session, "affordabilityCountryName", selected = countryName)
    updateSelectInput(session, "availabilityCountryName", selected = countryName)
    updateSelectInput(session, "adequacyCountryName", selected = countryName)
    updateSelectInput(session, "nutbalCountryName", selected = countryName)
    updateSelectInput(session, "AMDRCountryName", selected = countryName)
    updateSelectInput(session, "diversityCountryName", selected = countryName)
    updateSelectInput(session, "nonstapleEnergyShareCountryName", selected = countryName)
    updateSelectInput(session, "MRVCountryName", selected = countryName)
    updateSelectInput(session, "RaosQECountryName", selected = countryName)
    updateSelectInput(session, "FGcountryName", selected = countryName)

    # scenario choice observer -----
    scenarioName <- input$userScenarioName
    # Can also set the label and select items
    updateSelectInput(session, "availabilityScenarioName", selected = scenarioName)
    updateSelectInput(session, "adequacyScenarioName", selected = scenarioName)
    updateSelectInput(session, "FGscenarioName", selected = scenarioName)
  })

  # # userCountryChoice reactive -----
  # userCountryChoice <- reactive({
  #   countryName <- input$userCountryName
  #   countryName
  # })
  #
  # # userScenarioChoice reactive -----
  # userScenarioChoice <- reactive({
  #   scenarioName <- input$userScenarioName
  #   scenarioName
  # })

  data.foodAvail <- reactive({
    countryName <- input$availabilityCountryName
    scenarioName <- input$availabilityScenarioName
    DT <- data.table::copy(dt.foodAvail.foodGroup)
    spiderData <- spiderGraphData(countryName, scenarioName, DT, displayColumnName = "food_group_code")
  })

  # food adequacy.macro reactive -----
  data.adequacy.macro <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- RDA.macro_sum_reqRatio
    DT <- data.table::copy(reqType)
    DT[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, DT, displayColumnName = "nutrient")
  })

  # food adequacy.vits reactive -----
  data.adequacy.vits <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- RDA.vits_sum_reqRatio
    DT <- data.table::copy(reqType)
    DT[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, DT, displayColumnName = "nutrient")
  })

  # food adequacy.minrls reactive -----
  data.adequacy.minrls <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- RDA.minrls_sum_reqRatio
    DT <- data.table::copy(reqType)
    DT[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, DT, displayColumnName = "nutrient")
  })

  # food adequacy.macro reactive -----
  data.energy.quantity <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListRow <- c("kcalsPerDay.other", "kcalsPerDay.carbohydrate",
                     "kcalsPerDay.fat", "kcalsPerDay.protein")
    DT <- data.table::copy(dt.nutrients.kcals)
    DT <- DT[nutrient %in% keepListRow,]
    DT[, value := round(value, 1)][, nutrient := gsub("kcalsPerDay.", "", nutrient)]
    spiderData <- graphData(countryName, scenarioName, DT, displayColumnName = "nutrient")
  })

  # energy.share reactive -----
  data.energy.share <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListRow <- c("kcalsPerDay.other_share", "kcalsPerDay.carbohydrate_share",
                     "kcalsPerDay.fat_share", "kcalsPerDay.protein_share")
    DT <- data.table::copy(dt.nutrients.kcals)
    DT <- DT[scenario %in% scenarioName & nutrient %in% keepListRow,]
    DT[, value := round(value, 1)][, nutrient := gsub("kcalsPerDay.", "", nutrient)][, nutrient := gsub("_share", "", nutrient)]
    spiderData <- graphData(countryName, scenarioName, DT, displayColumnName = "nutrient")
  })

  # nonStaple.share reactive -----
  data.nonStaple.share <- reactive({
    countryName <- input$nonstapleEnergyShareCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    DT <- dt.KcalShare.nonstaple[region_code.IMPACT159 == countryCode,]
    DT[, value := round(value, 2)]
    DT
  })

  # MRV reactive -----
  data.MRV <- reactive({
    countryName <- input$MRVCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    DT <- data.table::copy(dt.MRVRatios)
    DT <- DT[region_code.IMPACT159 %in% countryCode ,]
    DT[, value := round(value, 2)]
    DT[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
    DT
  })

  # AMDR reactive -----
  data.AMDR <- reactive({
    countryName <- input$AMDRCountryName
    barData <- barGraphData(countryName, inData = food_agg_AMDR_hi)
    barData[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
  })

  # nutAvail reactive -----
  data.nutAvail <- reactive({
    countryName <- input$FGcountryName
    scenarioName <- input$FGscenarioName
    reqName <- input$nutrientGroup
    load_data_special("dt.nutrients.sum.FG") # checks to see if data already loaded. If not, loads it
    if (reqName == "macronutrients") nutrientGroup <- c("carbohydrate_g", "protein_g",  "totalfiber_g", "fat_g")
    if (reqName == "minerals") nutrientGroup <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
    if (reqName == "vitamins") nutrientGroup <- c("folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
                                                  "vit_b12_µg", "vit_c_mg", "vit_d_µg",  "vit_e_mg", "vit_k_µg")
    inData <- data.table::copy(dt.nutrients.sum.FG)
    inData[, year := gsub("X", "", year)]
    inData <- inData[nutrient %in% nutrientGroup]
    displayColumnName <- "food_group_code" # all these food groups are included in each spidergraph
    facetColumnName <- "nutrient" # one spider graph per facetColumnName
    spiderData <- facetGraphData(countryName, scenarioName, inData, facetColumnName, displayColumnName)
  })

  # nutBalScore reactive -----
  data.nutBalScore <- reactive({
    countryName <- input$nutbalCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    DT <- data.table::copy(dt.nutBalScore)
    DT <- DT[region_code.IMPACT159 == countryCode,]
    DT[, value := round(value, 2)]
    DT[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]

    formula.wide <- paste("scenario + region_code.IMPACT159 ~ year")
    spiderData <- data.table::dcast(
      data = DT,
      formula = formula.wide,
      value.var = "value")
  })

  # Shannon reactive -----
  data.ShannonDiv <- reactive({
    countryName <- input$diversityCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    DT <- data.table::copy(dt.shannonDiversity)
    DT <- DT[region_code.IMPACT159 == countryCode,]
    keepListCol <-  c("scenario","region_code.IMPACT159", "year", "SD", "SDnorm")
    DT <- DT[, keepListCol, with = FALSE]
    colsToRound <- c("SD", "SDnorm")
    DT[, (colsToRound) := round(.SD, 2), .SDcols = colsToRound]
    # reorder scenarios in temp to be the same order as scenarioNames
    DT[, scenario := factor(scenario, levels = scenarioNames)]
    DT[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
    formula.ShannonDiv <- paste("region_code.IMPACT159 + year ~ scenario")
    spiderData <- data.table::dcast(
      data = DT,
      formula = formula.ShannonDiv,
      value.var = c("SD", "SDnorm"))
  })

  # RaosQE reactive -----
  data.RaosQE <- reactive({
    countryName <- input$RaosQECountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    DT <- data.table::copy(dt.RAOqe)
    DT <- DT[region_code.IMPACT159 == countryCode,]
    DT[, value := round(value, 2)]
    DT[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]

    formula.wide <- paste("scenario + region_code.IMPACT159 ~ year")
    spiderData <- data.table::dcast(
      data = DT,
      formula = formula.wide,
      value.var = "value")
  })

  # affordabilityTable -----
  output$affordabilityTable <- renderTable({
    spiderDataTable <- data.table::copy(data.afford())
    spiderDataTable
  }, include.rownames = FALSE, width = "800" )

  output$downloadData.afford <- downloadHandler(
    filename = function() { paste("afford_", "_", input$affordabilityCountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.afford(), file)}
  )

  # availability graph server side -----
  output$availabilitySpiderGraphP1 <- renderggiraph({
    spiderData <- data.table::copy(data.foodAvail())
    scenarioName <- unique(spiderData$scenario)
    spiderData[, region_code.IMPACT159 := NULL]
    p <- spiderGraphOutput(spiderData, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = .5)
  })

  # availability table server side -----
  output$availabilityTableP1 <- renderTable({
    spiderData <- data.table::copy(data.foodAvail())
    spiderData[, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(spiderData, old = "region_code.IMPACT159", new = "country code")
    spiderData
  }, width = "10" )

  output$downloadData.avail <- downloadHandler(
    filename = function() { paste("foodavail_", "_", input$availabilityCountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.foodAvail(), file)}
  )

  # adequacy graphs server side -----
  output$adequacySpiderGraphP1 <- renderggiraph({
    spiderData <- data.table::copy(data.adequacy.macro())
    scenarioName <- unique(spiderData$scenario)
    spiderData[, region_code.IMPACT159 := NULL]
    spiderData[, scenario := gsub("-REF", "", scenario)]

    p <- spiderGraphOutput(spiderData, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  output$adequacySpiderGraphP2 <- renderggiraph({
    spiderData <- data.table::copy(data.adequacy.vits())
    scenarioName <- unique(spiderData$scenario)
    spiderData[, region_code.IMPACT159 := NULL]
    spiderData[, scenario := gsub("-REF", "", scenario)]

    p <- spiderGraphOutput(spiderData, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  output$adequacySpiderGraphP3 <- renderggiraph({
    DT <- data.table::copy(data.adequacy.minrls())
    scenarioName <- unique(DT$scenario)
    DT[, region_code.IMPACT159 := NULL]
    DT[, scenario := gsub("-REF", "", scenario)]

    p <- spiderGraphOutput(DT, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = .5)
  })

  # adequacy tables server side -----
  output$adequacyTableP1 <- renderTable({
    spiderData <- data.table::copy(data.adequacy.macro())
    spiderData[, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(spiderData, old = "region_code.IMPACT159", new = "country code")
    spiderData
  }, include.rownames = FALSE, width = "400")

  output$downloadData.adequacy.macro <- downloadHandler(
    filename = function() { paste("adeq_macro_", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.adequacy.macro(), file)}
  )

  output$adequacyTableP2 <- renderTable({
    spiderData <- data.table::copy(data.adequacy.vits())
    spiderData[, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(spiderData, old = "region_code.IMPACT159", new = "country code")
    spiderData
  }, include.rownames = FALSE, width = "400")

  output$downloadData.adequacy.vits <- downloadHandler(
    filename = function() { paste("adeq_vits_", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.adequacy.vits(), file)}
  )

  output$adequacyTableP3 <- renderTable({
    spiderData <- data.table::copy(data.adequacy.minrls())
    spiderData[, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(spiderData, old = "region_code.IMPACT159", new = "country code")
    spiderData
  }, include.rownames = FALSE, width = "400")

  output$downloadData.adequacy.minrls <- downloadHandler(
    filename = function() { paste("adeq_minrls_", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.adequacy.minrls(), file)}
  )

  # adequacy AMDR graph server side ------
  output$AMDRbarGraphP1 <- renderggiraph({
    DT <- data.table::copy(data.AMDR())
    p <- plotByRegionBarAMDRinShiny(DT, yLab = "(percent)")
    ggiraph(code = print(p), zoom_max = 1, width = .5)
  })

  # adequacy AMDR table server side ------
  output$AMDRTableP1 <- renderTable({
    DT <- data.table::copy(data.AMDR())
    colsToRound <- c("carbohydrate_g.Q", "fat_g.Q", "protein_g.Q")
    DT[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    oldnames <- c("region_code.IMPACT159", "carbohydrate_g.Q", "fat_g.Q", "protein_g.Q")
    newnames <- c("country code","carbohydrate", "protein", "fat")
    data.table::setnames(DT, old = oldnames, new = newnames)
    DT
  }, include.rownames = FALSE, width = "600")

  output$downloadData.AMDR <- downloadHandler(
    filename = function() { paste("AMDR_", input$AMDRCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.AMDR(), file)}
  )

  # adequacy nutrient balance table server side ------
  output$nutbalTableP1 <- renderTable({
    spiderData <- data.nutBalScore()
    data.table::setnames(spiderData, old = "region_code.IMPACT159", new = "country code")
  }, include.rownames = FALSE, width = "400")

  output$downloadData.nutbal <- downloadHandler(
    filename = function() { paste("nutbal_", input$nutbalCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nutBalScore(), file)}
  )

  # adequacy MRV table server side ------
  output$MRVTableP1 <- renderTable({
    spiderData <- data.table::copy(data.MRV())
    formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient ~ year")
    spiderData <- data.table::dcast(
      data = spiderData,
      formula = formula.wide,
      value.var = "value")
    spiderData
    data.table::setnames(spiderData, old = "region_code.IMPACT159", new = "country code")
  }, include.rownames = FALSE, width = "600")

  output$downloadData.MRV <- downloadHandler(
    filename = function() { paste("MRV", input$MRVCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.MRV(), file)}
  )

  # energy ratio bar chart -----
  output$energyShareBarPlot <- renderggiraph({
    DT <- data.table::copy(data.energy.share())
    scenarioName <- unique(DT$scenario)
    countryCode <- unique(DT$region_code.IMPACT159)
    countryName <- countryNameLookup((unique(DT$region_code.IMPACT159)))
    # colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    titleText <- paste("Share of total kilocalories by macronutrient\n", "Country:", countryName,"Scenario:", scenarioName)
    yLab <- "(percent)"
    p <- ggplot(DT, aes(x = year, y = value, tooltip = value, fill = nutrient, order = c("region_name") )) +
      geom_bar_interactive(stat = "identity") +
      theme(axis.title.y = element_text(family = fontFamily, face = "plain"))
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                             face = "plain")) + ggtitle(titleText)
    p <- p + theme(axis.text = element_text(size = 12, family = fontFamily, face = "plain"))
    p <- p + theme(legend.text = element_text(size = 12, family = fontFamily, face = "plain")) +
      labs(y = yLab, x = NULL)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  # energy quantity bar chart -----
  output$energyQuantityBarPlot <- renderggiraph({
    DT <- data.table::copy(data.energy.quantity())
    scenarioName <- unique(DT$scenario)
    countryCode <- unique(DT$region_code.IMPACT159)
    countryName <- countryNameLookup(countryCode)
    # colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    titleText <- paste("Average daily availability of kilocalories\n","Country:", countryName,", Scenario:", scenarioName)
    yLab <- "(kcals)"
    p <- ggplot(DT, aes(x = year, y = value, tooltip = value, fill = nutrient, order = c("region_name") )) +
      geom_bar_interactive(stat = "identity") +
      theme(axis.title.y = element_text(family = fontFamily, face = "plain"))
    # scale_y_continuous(limits = yRange) +
    # scale_fill_manual(values = colorList) +
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                             face = "plain")) + ggtitle(titleText)
    p <- p + theme(axis.text = element_text(size = 12, family = fontFamily, face = "plain"))
    p <- p + theme(legend.text = element_text(size = 12, family = fontFamily, face = "plain")) +
      labs(y = yLab, x = NULL)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  # energy ratio table -----
  output$energyShareTable <- renderTable({
    DT <- data.table::copy(data.energy.share())
    DT <- unique(DT)
    formula.wide <- paste("scenario + year + region_code.IMPACT159 ~ nutrient")
    DT <- data.table::dcast(data = DT, formula = formula.wide, value.var = "value")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "country code")
    DT[, scenario := gsub("-REF", "", scenario)]
    DT
  }, include.rownames = FALSE, width = "400")

  output$downloadData.energyRat <- downloadHandler(
    filename = function() { paste("energyRatio", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.energy.share(), file)}
  )

  # energy quantity table -----
  output$energyQuantityTable <- renderTable({
    DT <- data.table::copy(data.energy.quantity())
    DT <- unique(DT)
    formula.wide <- paste("scenario + year + region_code.IMPACT159 ~ nutrient")
    DT <- data.table::dcast(data = DT, formula = formula.wide, value.var = "value")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "country code")
    DT[, scenario := gsub("-REF", "", scenario)]
    DT
  }, include.rownames = FALSE, width = "400")

  output$downloadData.energyQ <- downloadHandler(
    filename = function() { paste("energyQ", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.energy.quantity(), file)}
  )

  # nonstaple share diversity metrics -----
  output$nonStapleEnergyShareTable <- renderTable({
    #   countryName <- input$nonstapleEnergyShareCountryName
    #   scenarioName <- input$stapleScenarioName
    #   countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    #
    #   DT <- dt.KcalShare.nonstaple[region_code.IMPACT159 == countryCode,]
    #   DT[, value := round(value, 2)]
    #    temp[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
    DT <- data.table::copy(data.nonStaple.share())
    formula.wide <- paste("year + region_code.IMPACT159 ~ scenario")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "country code")
    DT
  }, include.rownames = FALSE, width = "400")

  output$downloadData.nonStapleShare <- downloadHandler(
    filename = function() { paste("nonStapleShare_", input$nonstapleEnergyShareCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nonStaple.share(), file)}
  )

  # Rao's QE metric -----
  output$RaosQETable <- renderTable({
    DT <- data.table::copy(data.RaosQE())
    formula.wide <- paste("year + region_code.IMPACT159 ~ scenario")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "country code")
    DT
  }, include.rownames = FALSE, width = "400")

  output$downloadData.RaoQE <- downloadHandler(
    filename = function() { paste("RaosQE_", input$input$RaosQECountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.RaosQE(), file)}
  )

  # Shannon diversityTable -----
  output$diversityTable <- renderTable({
    DT <- data.table::copy(data.ShannonDiv())
    formula.wide <- paste("year + region_code.IMPACT159 ~ scenario")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "country code")
    oldNames <- names(DT)
    newNames <- gsub("_", " \n", oldNames)
    setnames(DT, old = oldNames, new = newNames)
    DT
  }, include.rownames = FALSE, width = "400")

  output$downloadData.ShannonDiversity <- downloadHandler(
    filename = function() { paste("Shannon_", input$diversityCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.ShannonDiv(), file)}
  )

  # nutrient avail, food group horizontal graphs -----
  output$NutDiverFGspiderGraphP1 <- renderPlot({
    DT <- data.table::copy(data.nutAvail())
    scenarioName <- unique(DT$scenario)
    countryCode <- unique(DT$region_code.IMPACT159)
    countryName <- countryNameLookup(countryCode)
    displayColumnName <- "food_group_code" # all these food groups are included in each spidergraph
    facetColumnName <- "nutrient" # one spider graph per facetColumnName
    p <- facetGraphOutput(inData = DT, facetColumnName, displayColumnName, scenarioName, countryName)
    p
  })

  # nutrient diversity FG Table -----
  output$NutDiverFGTable <- renderTable({
    DT <- data.table::copy(data.nutAvail())
    displayColumnName <- "food_group_code" # all these food groups are included in each spidergraph
    facetColumnName <- "nutrient" # one spider graph per facetColumnName
    formula.wide <- sprintf("scenario + region_code.IMPACT159 + year + %s ~ %s", facetColumnName, displayColumnName)
    DT <- data.table::dcast(data = DT, formula = formula.wide, value.var = "value")
    data.table::setnames(DT, old = "region_code.IMPACT159", new = "country code")
    names.new <- cleanupNutrientNames(names(DT))
    if (facetColumnName %in% "nutrient") DT[, nutrient := capwords(cleanupNutrientNames(nutrient))]
    data.table::setnames(DT, old = names(DT), new = capwords(names.new))
    DT
  }, include.rownames = FALSE, width = "400")

  output$downloadData.nutDiversity <- downloadHandler(
    filename = function() { paste("nutDiv_", "_", input$FGcountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nutAvail(), file)}
  )

  # metadataTable ------
  output$metadataTable <- renderTable({
    dt.metadata <- getNewestVersion("dt.metadata", fileloc("mData"))
    dt.metadata
  })

  # IMPACTmetadataTable ------
  output$IMPACTmetadataTable <- renderTable({
    dt.IMPACTgdxParams <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
    dt.IMPACTgdxParams
  }, include.rownames = FALSE)

  # IMPACTfoodgroupLookupTable ------
  output$IMPACTfoodgroupTable <- renderTable({
    data.table::setnames(dt.foodGroupsInfo, old = names(dt.foodGroupsInfo), new = gsub("_", " ", names(dt.foodGroupsInfo)))
    dt.foodGroupsInfo[,1:5]
  }, include.rownames = FALSE, width = "400")

  # nutrient lookup -----
  output$nutrientLookup <- renderTable({
    getNewestVersion("dt.nutrients.adj", fileloc("mData"))
  }, include.rownames = FALSE, width = "400")

  # file documentation -----
  output$fileDocumentation <- renderTable({
    getNewestVersion("resultFileLookup", fileloc("mData"))
  }, include.rownames = FALSE, width = "400")
}

# Run the application -----
shinyApp(ui = ui, server = server)

