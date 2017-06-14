# To make sure data and script files are up to date, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio
library(shiny)
library(shinyjs)
library(rsconnect)
library(data.table)
library(dplyr) # to do %>%
library(dtplyr)
library(DT) # needs to come after library(shiny)
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

dataSetsToLoad <- c(
  #' affordability data
  "dt.budgetShare",
  #' availability data
  "dt.foodAvail.foodGroup",
  #    "dt.nutrients.sum.all", now called when needed

  #' adequacy data
  "dt.nutrients.kcals",
  "food_agg_AMDR_hi",
  "RDA.macro_sum_reqRatio",
  "RDA.vits_sum_reqRatio",
  "RDA.minrls_sum_reqRatio",
  "dt.nutBalScore",
  "dt.MRVRatios",
  #     "dt.nutrients.sum.FG", # move to special
  # "RDA.vits_FG_reqRatio",
  # "RDA.minrls_FG_reqRatio",

  #' diversity data
  "dt.shannonDiversity",
  "dt.KcalShare.nonstaple",
  "dt.RAOqe"
)
# use for big data sets
dataSetsToLoad.supplemental <- c("dt.nutrients.sum.FG")

dataSetsToLoad.desc <- c("Budget share", "Food availability by food group", "Kilocalorie availability", "AMDR ratios", "Adequacy, macro",
                         "Adequacy, vitamins", "Adequacy, minerals", "Nutrient balance score", "Maximum recommended intake score", "Shannon diversity", "Share of kcals, nonstaples",
                         "Rao's QE")
dataSetsToLoad.desc.supplemental <- c("Nutrient availability by food group")

datasetsToLoad.complete <- c(dataSetsToLoad, dataSetsToLoad.supplemental)
datasetsToLoad.desc.complete <- c(dataSetsToLoad.desc, dataSetsToLoad.desc.supplemental)

#' foodGroupNames and foodGroupNamesNoWrap must align
foodGroupNames <- c("alcohol", "beverages", "cereals", "dairy", "eggs", "fish", "fruits", "meats", "nutsNseeds",
                    "oils", "pulses", "rootsNPlantain", "sweeteners", "vegetables")
foodGroupNamesNoWrap <- c("Beverages, alcoholic", "Beverages, nonalcoholic", "Cereals", "Dairy", "Eggs", "Fish", "Fruits", "Meats", "Nuts and seeds",
                          "Oils", "Pulses", "Roots and plantain", "Sweeteners", "Vegetables")
# Define UI -----
ui <- fluidPage(
  theme = shinytheme("sandstone"),
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
                               DT::dataTableOutput("affordabilityTable")))),

          # availability tab panel ------
          tabPanel(title = "Availability",
                   sidebarLayout(
                     sidebarPanel(width = 2,
                                  selectizeInput(inputId = "availabilityCountryName", label = "Choose a country", choices = countryNames),
                                  selectizeInput(inputId = "availabilityScenarioName", label = "Choose a scenario (see definition in glossary)", choices = scenarioNames),
                                  downloadButton("downloadData.avail", "Download data")),
                     mainPanel(titlePanel("Average daily availability by food group"),
                               includeHTML("www/availabilityText.html"),
                               ggiraphOutput("availabilitySpiderGraphP1", height = "150px"),
                               DT::dataTableOutput("availabilityTableP1")))),

          # Adequacy tab panel ------
          tabPanel(title = "Under- and over nutrition",
                   tabsetPanel(
                     # adequacy tab panel -----
                     tabPanel(title = "Adequacy ratios",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "adequacyCountryName", label = "Choose a country", choices = countryNames),
                                             selectizeInput(inputId = "adequacyScenarioName", label = "Choose a scenario (see definition in glossary)", choices = scenarioNames),
                                             downloadButton("downloadData.adequacy.macro", "Macro data"),
                                             downloadButton("downloadData.adequacy.vits", "Vitamin data"),
                                             downloadButton("downloadData.adequacy.minrls", "Minerals data"),
                                             downloadButton("downloadData.energyRat", "Energy share data"),
                                             downloadButton("downloadData.energyQ", "Kcal data")

                                ),
                                mainPanel(
                                  #                                  titlePanel("Dietary adequacy"), , width = "100%", height = "300px"
                                  includeHTML("www/adequacyText.html"),
                                  fluidRow(
                                    column(width = 6, ggiraphOutput("adequacySpiderGraphP1", height = "200px")),
                                    column(width = 6, ggiraphOutput("adequacySpiderGraphP2", height = "200px"))),
                                  fluidRow(
                                    column(width = 6, ggiraphOutput("adequacySpiderGraphP3", height = "200px"))),
                                  fluidRow(
                                    column(width = 6, ggiraphOutput("energyQuantityBarPlot")),
                                    column(width = 6, ggiraphOutput("energyShareBarPlot"))),
                                  fluidRow(
                                    column(width = 12, DT::dataTableOutput("adequacyTableP1"))),
                                  fluidRow(
                                    column(width = 12, DT::dataTableOutput("adequacyTableP2"))),
                                  fluidRow(
                                    column(width = 12, DT::dataTableOutput("adequacyTableP3"))),
                                  fluidRow(
                                    column(width = 12, DT::dataTableOutput("energyQuantityTable"))),
                                  fluidRow(
                                    column(width = 12, DT::dataTableOutput("energyShareTable")))))),

                     # AMDR tab panel -----
                     tabPanel(title = "Acceptable Macronutrient Distribution Range (AMDR)",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "AMDRCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.AMDR", "Download data")),
                                mainPanel(
                                  #                                  titlePanel("Acceptable Macronutrient Distribution Range"),
                                  includeHTML("www/AMDRText.html"),
                                  fluidRow(column(width = 12, ggiraphOutput("AMDRbarGraphP1"))),
                                  fluidRow(column(width = 12, DT::dataTableOutput("AMDRTableP1")))))),

                     # nutrient balance tab panel -----
                     tabPanel(title = "Nutrient Balance Score",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "nutbalCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.nutbal", "Download data")),
                                mainPanel(
                                  #                                  titlePanel("Nutrient balance score"),
                                  includeHTML("www/nutbalGraphText.html"),
                                  DT::dataTableOutput("nutbalTableP1")))),

                     # MRV tab panel -----
                     tabPanel(title = "Maximum Recommended Intake",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "MRVCountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.MRV", "Download data")),
                                mainPanel(
                                  #                                  titlePanel("Maximum Recommended Intake"),
                                  includeHTML("www/MRVText.html"),
                                  DT::dataTableOutput("MRVTableP1")))))),

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
                                  DT::dataTableOutput("diversityTable")))),

                     # Nut availability by food group tab panel ------
                     tabPanel(title = "Nutrient availability by food group",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "FGcountryName", label = "Choose a country", choices = countryNames),
                                             selectizeInput(inputId = "FGscenarioName", label = "Choose a scenario", choices = scenarioNames),
                                             selectizeInput(inputId = "nutrientGroup", label = "Choose a nutrient group", choices = c("vitamins", "minerals", "macronutrients")),
                                             downloadButton("downloadData.nutAvailFG", "Download data")),
                                mainPanel(
                                  #                                  titlePanel("Nutrient diversity by food group"),
                                  includeHTML("www/foodGroupSpiderGraphText.html"),
                                  uiOutput("plot.NutAvailFGbarGraphP1"),
                                  #                                plotOutput("NutAvailFGbarGraphP1", height = nutheight, width = "100%"),
                                  DT::dataTableOutput("NutAvailFGTable"),
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
                                  DT::dataTableOutput("nonStapleEnergyShareTable")))),

                     # Rao's QE tab panel ------
                     tabPanel(title = "Rao's quadratic entropy metric",
                              sidebarLayout(
                                sidebarPanel(width = 2,
                                             selectizeInput(inputId = "RaosQECountryName", label = "Choose a country", choices = countryNames),
                                             downloadButton("downloadData.RaoQE", "Download data")),
                                mainPanel(
                                  includeHTML("www/RaosQEGraphText.html"),
                                  DT::dataTableOutput("RaosQETable")))))),

          # glossary tab panel ------
          tabPanel(title = "Glossary",
                   mainPanel(
                     includeHTML("www/glossaryText.html"))),

          # data review and download tab panel ------
          tabPanel(title = "View and download full data sets",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                                  selectizeInput("dataset.full", "Choose a dataset:",
                                                 choices = datasetsToLoad.desc.complete),
                                  downloadButton('downloadData.full', 'Download'),
                                  includeHTML("www/downloadFullText.html")
                     ),
                     mainPanel(DT::dataTableOutput('table')))),

          # data and developer information tabs with tabset -----
          tabPanel(
            "Data and Developer Info",
            tabsetPanel(
              # Metadata tab panel -----
              tabPanel(
                title = "Metadata",
                mainPanel(
                  "Information for developers", " ",
                  fluidRow(column(width = 12, div(DT::dataTableOutput("metadataTable"), style = "font-size:80%", family = fontFamily))))),
              # IMPACT metadata tab panel -----
              tabPanel(title = "IMPACT metadata",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(DT::dataTableOutput("IMPACTmetadataTable"), style = "font-size:80%", family = fontFamily))))),
              # Foodgroup lookup panel -----
              tabPanel(title = "Food group lookup table",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(DT::dataTableOutput("IMPACTfoodgroupTable"), style = "font-size:80%", family = fontFamily))))),
              # nutrient lookup -----
              tabPanel(title = "Nutrient lookup table",
                       mainPanel(
                         "Information for developers", " ",
                         downloadButton("downloadData.nutrients.adj", "Download data"),
                         fluidRow(column(width = 12, div(DT::dataTableOutput("nutrientLookup"), style = "font-size:80%", family = fontFamily))))),
              # File documentation -----
              tabPanel(title = "File documentation",
                       mainPanel(
                         "Information for developers", " ",
                         fluidRow(column(width = 12, div(DT::dataTableOutput("fileDocumentation"), style = "font-size:80%", family = fontFamily))))))),
          # Acknowledgements -----
          tabPanel(title = "Acknowledgements",
                   mainPanel(
                     includeHTML("www/acknowledgementsText.html")))
        )
    )
  )
)

server <- function(input, output, session) {

  load_data(dataSetsToLoad) # load most of the data. Big files can be loaded elsewhere

  #not sure this is the best place for this. Need to get crude out of budget share. Just do once
  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "pcGDPX0", "budget.PCX0", "incShare.PCX0")
  deleteListCol <- names(dt.budgetShare)[!names(dt.budgetShare) %in% keepListCol]
  dt.budgetShare[, (deleteListCol) := NULL]
  colsToRound <- c("pcGDPX0", "budget.PCX0", "incShare.PCX0")
  dt.budgetShare[, (colsToRound) := round(.SD, 2), .SDcols = colsToRound]
  new.names <- c("Per Capita GDP", "Food expenditures", "Share of income")
  data.table::setnames(dt.budgetShare, old = colsToRound, new = new.names)
  dt.budgetShare[, year := gsub("X", "", year)]
  dt.budgetShare.long <- data.table::melt(
    data = dt.budgetShare,
    id.vars = c("scenario", "region_code.IMPACT159", "year"),
    measure.vars = new.names,
    variable.name = "Variable",
    value.name = "value",
    variable.factor = FALSE
  )
  formula.budgetShare <- paste("scenario + region_code.IMPACT159 + Variable  ~ year")
  dt.budgetShare.wide <- data.table::dcast(
    dt.budgetShare.long,
    formula = formula.budgetShare,
    value.var = "value")

  # set up data download

  # foodAfford reactive -----
  data.afford <- reactive({
    countryName <- input$affordabilityCountryName
    dt <- data.table::copy(dt.budgetShare.wide)
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- dt[region_code.IMPACT159 %in% countryCode, ]
    dt[, scenario := factor(scenario, levels = scenarioNames)]
    dt[, scenario := gsub("-REF", "", scenario)]
    # oldNames <- names(dt)
    # newNames <- gsub("pc", "Per Capita ", oldNames)
    # newNames <- gsub(".PCX0_X", "\n", newNames)
    # newNames <- gsub("X0_X", "\n", newNames)
    # newNames <- gsub("incShare", "Share of income", newNames)
    # newNames <- gsub("budget", "Food expenditures", newNames)
    # newNames <- gsub("region_code.IMPACT159", "country code", newNames)
    # setnames(dt, old = oldNames, new = newNames)
    dt
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

  # food availability reactive -----
  data.foodAvail <- reactive({
    countryName <- input$availabilityCountryName
    scenarioName <- input$availabilityScenarioName
    dt <- data.table::copy(dt.foodAvail.foodGroup)
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "food_group_code")
  })

  # food adequacy.macro reactive -----
  data.adequacy.macro <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- RDA.macro_sum_reqRatio
    dt <- data.table::copy(reqType)
    dt[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # food adequacy.vits reactive -----
  data.adequacy.vits <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- RDA.vits_sum_reqRatio
    dt <- data.table::copy(reqType)
    dt[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # food adequacy.minrls reactive -----
  data.adequacy.minrls <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- RDA.minrls_sum_reqRatio
    dt <- data.table::copy(reqType)
    dt[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # energy.quantity reactive -----
  data.energy.quantity <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListRow <- c("kcalsPerDay.other", "kcalsPerDay.carbohydrate",
                     "kcalsPerDay.fat", "kcalsPerDay.protein")
    dt <- data.table::copy(dt.nutrients.kcals)
    dt <- dt[nutrient %in% keepListRow,]
    dt[, value := round(value, 1)][, nutrient := gsub("kcalsPerDay.", "", nutrient)]
    spiderData <- graphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # energy.share reactive -----
  data.energy.share <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListRow <- c("kcalsPerDay.other_share", "kcalsPerDay.carbohydrate_share",
                     "kcalsPerDay.fat_share", "kcalsPerDay.protein_share")
    dt <- data.table::copy(dt.nutrients.kcals)
    dt <- dt[scenario %in% scenarioName & nutrient %in% keepListRow,]
    dt[, value := round(value, 1)][, nutrient := gsub("kcalsPerDay.", "", nutrient)][, nutrient := gsub("_share", "", nutrient)]
    spiderData <- graphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # nonStaple.share reactive -----
  data.nonStaple.share <- reactive({
    countryName <- input$nonstapleEnergyShareCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- dt.KcalShare.nonstaple[region_code.IMPACT159 == countryCode,]
    dt[, value := round(value, 2)]
    dt[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
    dt
  })

  # MRV reactive -----
  data.MRV <- reactive({
    countryName <- input$MRVCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- data.table::copy(dt.MRVRatios)
    dt <- dt[region_code.IMPACT159 %in% countryCode ,]
    dt[, value := round(value, 2)]
    dt[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
    dt
  })

  # AMDR reactive -----
  data.AMDR <- reactive({
    countryName <- input$AMDRCountryName
    barData <- barGraphData(countryName, inData = food_agg_AMDR_hi)
    barData[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]
  })

  # nutAvail reactive -----
  data.nutAvailFG <- reactive({
    countryName <- input$FGcountryName
    scenarioName <- input$FGscenarioName
    reqName <- input$nutrientGroup
    load_data_special("dt.nutrients.sum.FG") # checks to see if data already loaded. If not, load it
    if (reqName %in% "macronutrients") nutrientGroup <- c("carbohydrate_g", "protein_g",  "totalfiber_g", "fat_g")
    if (reqName %in% "minerals") nutrientGroup <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
    if (reqName %in% "vitamins") nutrientGroup <- c("folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
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
    dt <- data.table::copy(dt.nutBalScore)
    dt <- dt[region_code.IMPACT159 == countryCode,]
    dt[, value := round(value, 2)]
    dt[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]

    formula.wide <- paste("scenario + region_code.IMPACT159 ~ year")
    spiderData <- data.table::dcast(
      data = dt,
      formula = formula.wide,
      value.var = "value")
  })

  # Shannon reactive -----
  data.ShannonDiv <- reactive({
    countryName <- input$diversityCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- data.table::copy(dt.shannonDiversity)
    keepListCol <-  c("scenario","region_code.IMPACT159", "year", "SD", "SDnorm")
    dt <- dt[, keepListCol, with = FALSE]
    dt <- dt[region_code.IMPACT159 == countryCode,][, year := gsub("X", "", year)]
    colsToRound <- c("SD", "SDnorm")
    dt[, (colsToRound) := round(.SD, 2), .SDcols = colsToRound]

    idVars <- c("scenario", "region_code.IMPACT159", "year")
    measureVars <- names(dt)[!names(dt) %in% idVars]
    dt.long  <- data.table::melt(
      data = dt,
      id.vars = idVars,
      measure.vars = measureVars,
      variable.name = "Variable",
      value.name = "value",
      variable.factor = FALSE)

    formula.wide <- paste("scenario + region_code.IMPACT159 + Variable ~ year")
    dt.wide <- data.table::dcast(
      data = dt.long,
      formula = formula.wide,
      value.var = "value")

    # reorder scenarios in temp to be the same order as scenarioNames
    dt.wide[, scenario := factor(scenario, levels = scenarioNames)]
    dt.wide[, scenario := gsub("-REF", "", scenario)]
    # formula.ShannonDiv <- paste("region_code.IMPACT159 + scenario ~ year")
    # spiderData <- data.table::dcast(
    #   data = dt,
    #   formula = formula.ShannonDiv,
    #   value.var = c("SD", "SDnorm"))
  })

  # RaosQE reactive -----
  data.RaosQE <- reactive({
    countryName <- input$RaosQECountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- data.table::copy(dt.RAOqe)
    dt <- dt[region_code.IMPACT159 == countryCode,]
    dt[, value := round(value, 2)]
    dt[, scenario := gsub("-REF", "", scenario)][, year := gsub("X", "", year)]

    formula.wide <- paste("scenario + region_code.IMPACT159 ~ year")
    spiderData <- data.table::dcast(
      data = dt,
      formula = formula.wide,
      value.var = "value")
  })

  # full data set download reactive -----
  datasetInput <- reactive({
    # look up number of location of input value in the datasets name list
    # switch(input$dataset.full,
    #        datasetsToLoad.desc.complete[match(input$dataset.full, datasetsToLoad.desc.complete)] = datasetsToLoad.complete[match(input$dataset.full, datasetsToLoad.desc.complete)])
    get(datasetsToLoad.complete[match(input$dataset.full, datasetsToLoad.desc.complete)])
  })

  # affordabilityTable -----
  output$affordabilityTable <- DT::renderDataTable({
    dt <- data.table::copy(data.afford())
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.afford <- downloadHandler(
    filename = function() {paste("afford_", "_", input$affordabilityCountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.afford(), file)}
  )

  # availability graph server side -----
  output$availabilitySpiderGraphP1 <- renderggiraph({
    dt <- data.table::copy(data.foodAvail())
    scenarioName <- unique(dt$scenario)
    dt[, region_code.IMPACT159 := NULL]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = .75)
  })

  # dt.prod.crops.long <- data.table::melt(
  #   data = dt.prod.crops,
  #   id.vars = names(dt.prod.crops)[!names(dt.prod.crops) %in% keepListYears.composites],
  #   measure.vars = keepListYears.composites,
  #   variable.name = "year",
  #   value.name = "Production",
  #   variable.factor = FALSE
  # )
  #

  # availability table server side -----
  output$availabilityTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.foodAvail())
    dt <- pivotWideToWideYear(dt)
    nutrient <- "food group"
    data.table::setnames(dt, old = "nutrient_foodGroup", new = nutrient) # new depends on whether dt is for food groups or nutrients
    dt[, scenario := gsub("-REF", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.avail <- downloadHandler(
    filename = function() {paste("foodavail_", "_", input$availabilityCountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.foodAvail(), file)}
  )

  # adequacy graphs server side -----
  output$adequacySpiderGraphP1 <- renderggiraph({
    dt <- data.table::copy(data.adequacy.macro())
    scenarioName <- unique(dt$scenario)
    dt[, region_code.IMPACT159 := NULL][, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  output$adequacySpiderGraphP2 <- renderggiraph({
    dt <- data.table::copy(data.adequacy.vits())
    scenarioName <- unique(dt$scenario)
    dt[, region_code.IMPACT159 := NULL][, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  output$adequacySpiderGraphP3 <- renderggiraph({
    dt <- data.table::copy(data.adequacy.minrls())
    scenarioName <- unique(dt$scenario)
    dt[, region_code.IMPACT159 := NULL][, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, width = 1)
  })

  # adequacy tables server side -----
  output$adequacyTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.adequacy.macro())
    dt <- pivotWideToWideYear(dt)
    nutrient <- "nutrient"
    data.table::setnames(dt, old = "nutrient_foodGroup", new = nutrient) # new depends on whether dt is for food groups or nutrients
    dt[, scenario := gsub("-REF", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.adequacy.macro <- downloadHandler(
    filename = function() {paste("adeq_macro_", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.adequacy.macro(), file)}
  )

  output$adequacyTableP2 <- DT::renderDataTable({
    dt <- data.table::copy(data.adequacy.vits())
    dt <- pivotWideToWideYear(dt)
    nutrient <- "nutrient"
    data.table::setnames(dt, old = "nutrient_foodGroup", new = nutrient) # new depends on whether dt is for food groups or nutrients
    dt[, scenario := gsub("-REF", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.adequacy.vits <- downloadHandler(
    filename = function() {paste("adeq_vits_", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.adequacy.vits(), file)}
  )

  output$adequacyTableP3 <- DT::renderDataTable({
    dt <- data.table::copy(data.adequacy.minrls())
    dt <- pivotWideToWideYear(dt)
    nutrient <- "nutrient"
    data.table::setnames(dt, old = "nutrient_foodGroup", new = nutrient) # new depends on whether dt is for food groups or nutrients
    dt[, scenario := gsub("-REF", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.adequacy.minrls <- downloadHandler(
    filename = function() {paste("adeq_minrls_", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.adequacy.minrls(), file)}
  )

  # energy ratio bar chart -----
  output$energyShareBarPlot <- renderggiraph({
    dt <- data.table::copy(data.energy.share())
    scenarioName <- unique(dt$scenario)
    countryCode <- unique(dt$region_code.IMPACT159)
    countryName <- countryNameLookup((unique(dt$region_code.IMPACT159)))
    # colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    titleText <- paste("Share of total kilocalories by macronutrient\n", "Country: ", countryName,"Scenario: ", scenarioName)
    yLab <- "(percent)"
    p <- ggplot(dt, aes(x = year, y = value, tooltip = value, fill = nutrient, order = c("region_name") )) +
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
    dt <- data.table::copy(data.energy.quantity())
    scenarioName <- unique(dt$scenario)
    countryCode <- unique(dt$region_code.IMPACT159)
    countryName <- countryNameLookup(countryCode)
    # colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    titleText <- paste("Average daily availability of kilocalories\n","Country:", countryName,", Scenario:", scenarioName)
    yLab <- "(kcals)"
    p <- ggplot(dt, aes(x = year, y = value, tooltip = value, fill = nutrient, order = c("region_name") )) +
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

  # # energy table -----
  # output$energyTable <- DT::renderDataTable({
  #   dt.long.share <- data.table::copy(data.energy.share())
  #   dt.long.quantity <- data.table::copy(data.energy.quantity())
  #   dt.long <- cbind(dt.long.quantity, dt.long.share)
  #   formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient ~ year")
  #   dt <- data.table::dcast(
  #     data = dt.long,
  #     formula = formula.wide,
  #     value.var = "value")
  #   nutrient <- "share of total energy"
  #   data.table::setnames(dt, old = "nutrient", new = nutrient) # new depends on whether dt is for food groups or nutrients
  #   dt[, scenario := gsub("-REF", "", scenario)]
  #   # formula.wide <- paste("scenario + region_code.IMPACT159 + ", nutrient, " ~ year")
  #   # dt <- data.table::dcast(data = dt, formula = formula.wide, value.var = "value")
  #   colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
  #   dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
  #   data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
  #   dt[, scenario := gsub("-REF", "", scenario)]
  #   data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
  #   dt <- DT::datatable(dt, rownames = FALSE, options = list(dom = 't', ordering = F))
  #   dt
  # })

  # energy share table -----
  output$energyShareTable <- DT::renderDataTable({
    dt.long <- data.table::copy(data.energy.share())
    formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient ~ year")
    dt <- data.table::dcast(
      data = dt.long,
      formula = formula.wide,
      value.var = "value")
    nutrient <- "share of total energy"
    data.table::setnames(dt, old = "nutrient", new = nutrient) # new depends on whether dt is for food groups or nutrients
    dt[, scenario := gsub("-REF", "", scenario)]
    # formula.wide <- paste("scenario + region_code.IMPACT159 + ", nutrient, " ~ year")
    # dt <- data.table::dcast(data = dt, formula = formula.wide, value.var = "value")
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    dt[, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.energyRat <- downloadHandler(
    filename = function() {paste("energyRatio", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.energy.share(), file)}
  )

  # energy quantity table -----
  output$energyQuantityTable <- DT::renderDataTable({
    dt.long <- data.table::copy(data.energy.quantity())
    formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient ~ year")
    dt <- data.table::dcast(
      data = dt.long,
      formula = formula.wide,
      value.var = "value")
    nutrient <- "Kilocalories"
    data.table::setnames(dt, old = "nutrient", new = nutrient) # new depends on whether dt is for food groups or nutrients
    dt[, scenario := gsub("-REF", "", scenario)]
    # formula.wide <- paste("scenario + region_code.IMPACT159 + ", nutrient, " ~ year")
    # dt <- data.table::dcast(data = dt, formula = formula.wide, value.var = "value")
    # colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,1), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    dt[, scenario := gsub("-REF", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.energyQ <- downloadHandler(
    filename = function() {paste("energyQ", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.energy.quantity(), file)}
  )

  # adequacy AMDR graph server side ------
  output$AMDRbarGraphP1 <- renderggiraph({
    dt <- data.table::copy(data.AMDR())
    p <- plotByRegionBarAMDRinShiny(dt, yLab = "(percent)")
    ggiraph(code = print(p), zoom_max = 1, width = .75)
  })

  # adequacy AMDR table server side ------
  output$AMDRTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.AMDR())
    oldnames <- c("carbohydrate_g.Q", "fat_g.Q", "protein_g.Q")
    newnames <- c("carbohydrate", "protein", "fat")
    data.table::setnames(dt, old = oldnames, new = newnames)
    #    print(dt)
    dt <- pivotWideToWideYear(dt)
    colsToRound <- c("2010", "2030", "2050")
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]

    oldnames <- c("region_code.IMPACT159", "nutrient_foodGroup")
    newnames <- c("Country Code", "Nutrient")
    data.table::setnames(dt, old = oldnames, new = newnames)
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.AMDR <- downloadHandler(
    filename = function() {paste("AMDR_", input$AMDRCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.AMDR(), file)}
  )

  # adequacy nutrient balance table server side ------
  output$nutbalTableP1 <- DT::renderDataTable({
    dt <- data.nutBalScore()
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year")]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', ordering = F))
    dt
  })

  output$downloadData.nutbal <- downloadHandler(
    filename = function() {paste("nutbal_", input$nutbalCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nutBalScore(), file)}
  )

  # adequacy MRV table server side ------
  output$MRVTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.MRV())
    dt[,value := round(value, 2)]
    formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient ~ year")
    dt <- data.table::dcast(
      data = dt,
      formula = formula.wide,
      value.var = "value")
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    #    dt[, nutrient := capwords(nutrient)]
    dt[, nutrient := capwords(cleanupNutrientNames(nutrient))]
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 15, dom = 't', ordering = F))
    dt
  })

  output$downloadData.MRV <- downloadHandler(
    filename = function() {paste("MRV", input$MRVCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.MRV(), file)}
  )

  # nonstaple share diversity metrics -----
  output$nonStapleEnergyShareTable <- DT::renderDataTable({
    dt <- data.table::copy(data.nonStaple.share())
    formula.wide <- paste("scenario + region_code.IMPACT159 ~ year")
    dt <- data.table::dcast(
      data = dt,
      formula = formula.wide,
      value.var = "value")

    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year")]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.nonStapleShare <- downloadHandler(
    filename = function() {paste("nonStapleShare_", input$nonstapleEnergyShareCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nonStaple.share(), file)}
  )

  # Rao's QE metric -----
  output$RaosQETable <- DT::renderDataTable({
    dt <- data.table::copy(data.RaosQE())
    formula.wide <- paste("year + region_code.IMPACT159 ~ scenario")
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year")]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.RaoQE <- downloadHandler(
    filename = function() {paste("RaosQE_", input$input$RaosQECountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.RaosQE(), file)}
  )

  # Shannon diversityTable -----
  output$diversityTable <- DT::renderDataTable({
    dt <- data.table::copy(data.ShannonDiv())
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", "Variable")]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    #    formula.wide <- paste("year + region_code.IMPACT159 ~ scenario")
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    oldNames <- names(dt)
    newNames <- gsub("_", " \n", oldNames)
    setnames(dt, old = oldNames, new = newNames)
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.ShannonDiversity <- downloadHandler(
    filename = function() {paste("Shannon_", input$diversityCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.ShannonDiv(), file)}
  )

  # nutrient avail, FG horizontal graphs -----
  output$NutAvailFGbarGraphP1 <- renderPlot({
    dt <- data.table::copy(data.nutAvailFG())
    #    print(head(dt))
    displayColumnName <- "food_group_code" # all these food groups are included in each spidergraph
    facetColumnName <- "nutrient" # one spider graph per facetColumnName
    p <- facetGraphOutput(inData = dt, facetColumnName, displayColumnName, foodGroupNames, foodGroupNamesNoWrap)
    p
  }, height = "auto")

  output$plot.NutAvailFGbarGraphP1 <- renderUI({
    if (input$nutrientGroup == "vitamins") plotHeight = "800px"
    if (input$nutrientGroup == "macro") plotHeight = "500px"
    if (input$nutrientGroup == "minerals") plotHeight = "500px"
    plotOutput("NutAvailFGbarGraphP1", width = "100%", height = plotHeight)
  })

  # nutrient diversity FG Table -----
  output$NutAvailFGTable <- DT::renderDataTable({
    dt <- data.table::copy(data.nutAvailFG())
    #    print(unique(dt$food_group_code))
    displayColumnName <- "food_group_code" # all these food groups are included in each spidergraph
    facetColumnName <- "nutrient" # one bar graph per facetColumnName
    dt[, value := round(value, 2)]
    formula.wide <- sprintf("scenario + region_code.IMPACT159 + %s + %s ~ year", facetColumnName, displayColumnName)
    dt <- data.table::dcast(data = dt, formula = formula.wide, value.var = "value")
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    names.new <- cleanupNutrientNames(names(dt))
    dt[, scenario := gsub("-REF", "", scenario)]
    if (facetColumnName %in% "nutrient") dt[, nutrient := capwords(cleanupNutrientNames(nutrient))]
    data.table::setnames(dt, old = names(dt), new = capwords(names.new))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloaddata.nutAvailFG <- downloadHandler(
    filename = function() {paste("nutDiv_", "_", input$FGcountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nutAvailFG(), file)}
  )

  #view and download data ------
  output$table <- DT::renderDataTable({
    dt <- datasetInput()
    currentDT <- datasetsToLoad.complete[match(input$dataset.full, datasetsToLoad.desc.complete)]
    valueDTNames <- c( "dt.foodAvail.foodGroup", "dt.nutrients.kcals",
                       "RDA.macro_sum_reqRatio",  "RDA.vits_sum_reqRatio", "RDA.minrls_sum_reqRatio",
                       "dt.nutBalScore", "dt.MRVRatios", "dt.KcalShare.nonstaple", "dt.RAOqe")
    if (!currentDT %in% valueDTNames) {
      dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
    }else{
      dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25)) %>% formatRound('value', 3)
    }
  })

  output$downloadData.full <- downloadHandler(
    filename = function() { paste(input$dataset.full, "_", Sys.Date(), '.gz', sep = '') },

    content = function(file) {
      write.csv(datasetInput(), file = gzfile(file))
      #   zip(zipfile = file, files = datasetInput())
      #
      # fs <- c()
      # tmpdir <- tempdir()
      # setwd(tempdir())
      # path <- paste0(testFileName)
      # fs <- c(fs, path)
      # write(4, path)
      # print(fs)
      # zip(zipfile = file, files = fs)
    }
    #    content = function(file) {
    #      zip(zipfile = file, files = datasetInput())
    # #     write.csv(datasetInput(), file = gzfile(file))
    #    },
    # contentType = "application/zip"
  )

  # metadataTable ------
  output$metadataTable <- DT::renderDataTable({
    dt.metadata <- getNewestVersion("dt.metadata", fileloc("mData"))
    dt.metadata
  })

  # IMPACTmetadataTable ------
  output$IMPACTmetadataTable <- DT::renderDataTable({
    dt.IMPACTgdxParams <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
    dt.IMPACTgdxParams
  })

  # IMPACTfoodgroupLookupTable ------
  output$IMPACTfoodgroupTable <- DT::renderDataTable({
    data.table::setnames(dt.foodGroupsInfo, old = names(dt.foodGroupsInfo), new = gsub("_", " ", names(dt.foodGroupsInfo)))
    dt.foodGroupsInfo[,1:5]
  })

  # nutrient lookup -----
  output$nutrientLookup <- DT::renderDataTable({
    dt <- getNewestVersion("dt.nutrients.adj", fileloc("mData"))
    colsToRound <- c("phytate_mg", "protein_g", "fat_g", "carbohydrate_g", "totalfiber_g", "energy_kcal",
                     "calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg", "vit_c_mg", "thiamin_mg", "riboflavin_mg",
                     "niacin_mg", "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg", "vit_e_mg", "vit_d_µg", "vit_k_µg", "sugar_g", "ft_acds_tot_sat_g",
                     "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g", "ethanol_g", "caffeine_mg", "cholesterol_mg",
                     "kcals.fat_g", "kcals.protein_g", "kcals.carbohydrate_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g", "kcals.ethanol_g")
    dt[, (colsToRound) := round(.SD, 3), .SDcols = colsToRound]
  })
  output$downloadData.nutrients.adj <- downloadHandler(
    filename = function() {paste("dt.nutrients.adj_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(dt.nutrients.adj, file)}
  )
  # file documentation -----
  output$fileDocumentation <- DT::renderDataTable({
    getNewestVersion("resultFileLookup", fileloc("mData"))
  })
  session$onSessionEnded(stopApp)
}

# Run the application -----
shinyApp(ui = ui, server = server)


