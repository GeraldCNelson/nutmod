# To use this, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(rsconnect)
library(data.table)
library(xtable)
library(fmsb) # for the spider charts
library(gdata) # to reorder the scenario names
library(dplyr)
source("global.R")
options(repos = c(CRAN = "http://cran.rstudio.com"))

rsconnect::setAccountInfo(name = 'nutrientmodeling', token = 'D0257883A5409984C3DC39101FAACA2E', secret = 'xxBEXU7/AvSa7/10LHFCU7uzXVCHiHP+T7Q8fJDB')
# rsconnect::deployApp()
dt.regions <- getNewestVersion("dt.regions.all",fileloc("mData"))
countryNames <- sort(unique(dt.regions$region_name.IMPACT159))
scenarioNames <- sort(keyVariable("scenarioListIMPACT"))
refScenarios <- c("SSP2-NoCC", "SSP2-HGEM", "SSP2-IPSL")
scenarioNames <- c(refScenarios, scenarioNames[!scenarioNames %in% refScenarios])
resultFileLookup <- data.table::as.data.table(read.csv("data/ResultFileLookup.csv"))
reqChoices <- as.character(resultFileLookup$reqType)
FGreqChoices <- reqChoices[grep("FG", reqChoices)]

# Define UI

ui <- fluidPage(title = "Nutrient modeling",
                tabsetPanel(
                  # Introduction tab panel -----
                  tabPanel(title = "Introduction",
                           mainPanel(width = "100%",
                                     includeHTML("www/introText.html")
                           )
                  ),
                  # Affordability tab panel ------
                  tabPanel(title = "Affordability",
                           titlePanel("Affordability of the current diet"),
                           wellPanel(
                             includeHTML("www/affordabilityText.html"),
                             helpText("Choose from the drop downs below to see country-specific shares",
                                      "expenditures on food by a representative consumer compared",
                                      "to per capita income."),
                             selectInput(inputId = "countryName",
                                         label = "Choose a country",
                                         choices = countryNames,
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                             #   selectInput(inputId = "scenarioName",
                             #               choices = scenarioNames,
                             #               label = "Choose a scenario",
                             #               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                             # ),
                             mainPanel( width = "100%",
                                        fluidRow(
                                          column(width = 12, div(tableOutput("affordabilityTable"), style = "font-size:80%"))
                                        )
                             )
                           )
                  ),
                  # Adequacy tab panel ------
                  tabPanel(title = "Adequacy",
                           titlePanel("Nutrient intake and requirements"),
                           includeHTML("www/adequacyText.html"),
                           # adequacy tab well panel
                           wellPanel(
                             helpText("Choose from the drop downs below to see country-specific spider graphs",
                                      "of the consumption by a representative consumer compared",
                                      "to the nutrient requirement. Draft results, not for citation or use, contact nelson.gerald.c@gmail.com for further info"),
                             selectInput(inputId = "adequacyCountryName",
                                         label = "Choose a country",
                                         choices = countryNames,
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                             selectInput(inputId = "adequacyScenarioName",
                                         choices = scenarioNames,
                                         label = "Choose a scenario",
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                           ),
                           # Adequacy tab main panel -----
                           mainPanel(width = "100%",
                                     #      tags$head(tags$style("#adequacyTableP1 table {background-color: red; }", media = "screen", type = "text/css")),
                                     fluidRow(
                                       column(width = 6, plotOutput("adequacySpiderGraphP1", height = "200px")),
                                       column(width = 6, plotOutput("adequacySpiderGraphP2", height = "200px"))
                                     ),
                                     fluidRow(
                                       column(width = 6, div(tableOutput("adequacyTableP1"), style = "font-size:60%")),
                                       column(width = 6, div(tableOutput("adequacyTableP2"), style = "font-size:60%"))
                                     ),
                                     fluidRow(
                                       column(width = 6, plotOutput("adequacySpiderGraphP3", height = "200px")),
                                       column(width = 6, plotOutput("adequacySpiderGraphP4", height = "200px"))
                                     ),
                                     fluidRow(
                                       column(width = 6, div(tableOutput("adequacyTableP3"), style = "font-size:60%")),
                                       column(width = 6, div(tableOutput("adequacyTableP4"), style = "font-size:60%"))
                                     ),
                                     fluidRow(
                                       column(width = 6, plotOutput("adequacySpiderGraphP5", height = "200px")),
                                       column(width = 6)
                                     ),
                                     fluidRow(
                                       column(width = 6, div(tableOutput("adequacyTableP5"), style = "font-size:60%")),
                                       column(width = 6)
                                     )
                           )
                  ),
                  # Diversity tab panel ------
                  tabPanel(title = "Dietary diversity",
                           titlePanel("Dietary diversity measures "),
                           wellPanel(
                             includeHTML("www/diversityText.html"),
                             helpText("Choose from the drop downs below to see country-specific shares",
                                      "expenditures on food by a representative consumer compared",
                                      "to per capita income."),
                             selectInput(inputId = "diversityCountryName",
                                         label = "Choose a country",
                                         choices = countryNames,
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                             #   selectInput(inputId = "scenarioName",
                             #               choices = scenarioNames,
                             #               label = "Choose a scenario",
                             #               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                             # ),
                             # Diversity tab main panel -----
                           ),
                           mainPanel( width = "100%",
                                      fluidRow(
                                        column(width = 12, div(tableOutput("diversityTable"), style = "font-size:80%"))
                                      )
                           )
                  ),
                  # Food group tab panel ------
                  tabPanel(title = "Food group diversity",
                           titlePanel("Nutrient diversity by food groups"),
                           includeHTML("www/foodGroupSpiderGraphText.html"),
                           wellPanel(
                             helpText("Choose from the drop downs below to see country-specific spider graphs",
                                      "of nutrient sources by food group"),
                             selectInput(inputId = "FGcountryName",
                                         label = "Choose a country",
                                         choices = countryNames,
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                             selectInput(inputId = "FGscenarioName",
                                         choices = scenarioNames,
                                         label = "Choose a scenario",
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                             selectInput(inputId = "nutrientGroup",
                                         choices = FGreqChoices,
                                         label = "Choose a nutrient group",
                                         selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                           ),
                           # Show some spidergraphs and underlying data tables
                           mainPanel( width = "100%",
                                      #      tags$head(tags$style("#adequacyTableP1 table {background-color: red; }", media = "screen", type = "text/css")),
                                      fluidRow(
                                        column(width = 12, plotOutput("NutDiverFGspiderGraphP1", height = "1000px"))
                                      )
                           )
                  ),
                  # ,
                  # column(width = 6, plotOutput("NutDiverFGspiderGraphP2", height = "200px"))
                  # fluidRow(
                  #   column(width = 6, div(tableOutput("NutDiverFGspiderTableP1"), style = "font-size:60%")),
                  #   column(width = 6, div(tableOutput("NutDiverFGspiderTableP2"), style = "font-size:60%"))
                  # ),
                  # fluidRow(
                  #   column(width = 6, plotOutput("NutDiverFGspiderGraphP3", height = "200px")),
                  #   column(width = 6, plotOutput("NutDiverFGspiderGraphP4", height = "200px"))
                  # ),
                  # fluidRow(
                  #   column(width = 6, div(tableOutput("NutDiverFGspiderTableP3"), style = "font-size:60%")),
                  #   column(width = 6, div(tableOutput("NutDiverFGspiderTableP4"), style = "font-size:60%"))
                  # ),
                  # fluidRow(
                  #   column(width = 6, plotOutput("NutDiverFGspiderGraphP5", height = "200px")),
                  #   column(width = 6)
                  # ),
                  # fluidRow(
                  #   column(width = 6, div(tableOutput("NutDiverFGspiderTableP5"), style = "font-size:60%")),
                  #   column(width = 6)
                  # )

                  # Metadata tab panel -----
                  tabPanel(title = "Metadata",
                           mainPanel(width = "100%",
                                     "Information for developers",
                                     " ",

                                     fluidRow(
                                       column(width = 12, div(tableOutput("metadataTable"), style = "font-size:80%"))
                                     )
                           )
                  ),
                  # IMPACT metadata tab panel -----
                  tabPanel(title = "IMPACT metadata",
                           mainPanel(width = "100%",
                                     "Information for developers",
                                     " ",
                                     fluidRow(
                                       column(width = 12, div(tableOutput("IMPACTmetadataTable"), style = "font-size:80%"))
                                     )
                           )
                  )
                )
)

server <- function(input, output) {
  years <- c("X2010", "X2030", "X2050")
  yearsClean <- gsub("X", "", years)
  # adequacy server side -----
  output$adequacySpiderGraphP1 <- renderPlot(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA.macro"
      nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
    })

  output$adequacyTableP1 <- renderTable(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA.macro"
      temp <- as.data.table(nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData")))
      namelist <- colnames(temp)
      temp <- temp[4:nrow(temp)][,year := yearsClean]
      setcolorder(temp, c("year", namelist))
    }, include.rownames = FALSE)

  output$adequacySpiderGraphP2 <- renderPlot(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA.vits"
      temp  <- nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
    })

  output$adequacyTableP2 <- renderTable(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA.vits"
      temp <- nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
      namelist <- colnames(temp)
      temp <- temp[4:nrow(temp)][,year := yearsClean]
      setcolorder(temp, c("year", namelist))
    }, include.rownames = FALSE)


  output$adequacySpiderGraphP3 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.minrls"
    #   years <- c("X2010","X2030","X2050")
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP3 <- renderTable(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA.minrls"
      temp <- nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
      namelist <- colnames(temp)
      temp <- temp[4:nrow(temp)][,year := yearsClean]
      setcolorder(temp, c("year", namelist))
    }, include.rownames = FALSE)


  output$adequacySpiderGraphP4 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL.minrls"
    #   years <- c("X2010","X2030","X2050")
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP4 <- renderTable(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "UL.minrls"
      temp <- nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
      namelist <- colnames(temp)
      temp <- temp[4:nrow(temp)][,year := yearsClean]
      setcolorder(temp, c("year", namelist))
    }, include.rownames = FALSE)


  output$adequacySpiderGraphP5 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL.vits"
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyspiderTableP5 <- renderTable(
    {
      countryName <- input$adequacyCountryName
      scenarioName <- input$adequacyScenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "UL.vits"
      temp <- nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
      namelist <- colnames(temp)
      temp <- temp[4:nrow(temp)][,year := yearsClean]
      setcolorder(temp, c("year", namelist))
    }, include.rownames = FALSE)


  # food group diversity spider graphs -----

  output$NutDiverFGspiderGraphP1 <- renderPlot(
    {
      countryName <- input$FGcountryName
      reqType <- input$nutrientGroup
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      nutshareSpiderGraph(reqType, countryCode, input$FGscenarioName, years, fileloc("mData"))
    })
  #
  #   output$NutDiverFGspiderTableP1 <- renderTable(
  #     {
  #       countryName <- input$countryName
  #       countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #       reqType <- "FG.macro"
  #       temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #       temp2 <- as.data.frame(temp[1])
  #       temp2[4:nrow(temp2),]
  #     })
  #
  #   output$NutDiverFGspiderGraphP2 <- renderPlot(
  #     {
  #       countryName <- input$countryName
  #       countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #       reqType <- "FG.vits"
  #       #    years <- c("X2010", "X2030", "X2050")
  #       nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #     })
  #
  #   output$NutDiverFGspiderTableP2 <- renderTable(
  #     {
  #       countryName <- input$countryName
  #       countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #       reqType <- "FG.vits"
  #       temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #       temp2 <- as.data.frame(temp[1])
  #       temp2[4:nrow(temp2),]
  #     })
  #
  #   output$NutDiverFGspiderGraphP3 <- renderPlot({
  #     countryName <- input$countryName
  #     countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #     reqType <- "FG.minrls"
  #     #   years <- c("X2010","X2030","X2050")
  #     nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #   })
  #
  #   output$NutDiverFGspiderTableP3 <- renderTable(
  #     {
  #       countryName <- input$countryName
  #       countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #       reqType <- "FG.minrls"
  #       temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #       temp2 <- as.data.frame(temp[1])
  #       temp2[4:nrow(temp2),]
  #     })
  #
  #   output$NutDiverFGspiderGraphP4 <- renderPlot({
  #     countryName <- input$countryName
  #     countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #     reqType <- "FG.minrls.UL"
  #     #   years <- c("X2010","X2030","X2050")
  #     nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #   })
  #
  #   output$NutDiverFGspiderTableP4 <- renderTable(
  #     {
  #       countryName <- input$countryName
  #       countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #       reqType <- "FG.minrls.UL"
  #       temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #       temp2 <- as.data.frame(temp[1])
  #       temp2[4:nrow(temp2),]
  #     })
  #
  #   output$NutDiverFGspiderGraphP5 <- renderPlot({
  #     countryName <- input$countryName
  #     countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #     reqType <- "FG.vits.UL"
  #     #   years <- c("X2010","X2030","X2050")
  #     nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #   })
  #
  #   output$NutDiverFGspiderTableP5 <- renderTable(
  #     {
  #       countryName <- input$countryName
  #       countryCode <- countryCodeLookup(countryName, fileloc("mData"))
  #       reqType <- "FG.vits.UL"
  #       temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  #       temp2 <- as.data.frame(temp[1])
  #       temp2[4:nrow(temp2),]
  #     })

  # affordabilityTable -----
  output$affordabilityTable <- renderTable(
    {
      countryName <- input$countryName
      #scenarioName <- input$scenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      dt.budgetShare <- getNewestVersion("dt.budgetShare", fileloc("mData"))
      keepListCol <-  c("scenario", "year", "pcGDPX0", "budget.PCX0", "incSharePCX0")
      #      temp <- dt.budgetShare[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName &
      temp <- dt.budgetShare[region_code.IMPACT159 == countryCode &
                               year %in% years, keepListCol, with = FALSE]
      temp[,incSharePCX0 := incSharePCX0*100]
      setnames(temp, old = c("pcGDPX0", "budget.PCX0", "incSharePCX0"), new = c("Per capita GDP","Budget", "Expend. share (%)"))
      # reorder scenarios in temp to be the same order as scenarioNames
      temp$scenario <- reorder.factor(temp$scenario, new.order = scenarioNames)
      temp <- temp %>% arrange(scenario)
      temp
    })

  # diversityTable -----
  output$diversityTable <- renderTable(
    {
      countryName <- input$diversityCountryName
      #scenarioName <- input$scenarioName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      dt.shannonDiversity <- getNewestVersion("dt.shannonDiversity", fileloc("mData"))
      keepListCol <-  c("scenario", "year", "SD", "SDnorm")
      temp <- dt.shannonDiversity[region_code.IMPACT159 == countryCode &
                                    year %in% years, keepListCol, with = FALSE]
      # reorder scenarios in temp to be the same order as scenarioNames
      temp$scenario <- reorder.factor(temp$scenario, new.order = scenarioNames)
      temp <- temp %>% arrange(scenario)
      temp
    })

  # metadataTable ------
  output$metadataTable <- renderTable(
    {
      metaData <- getNewestVersion("metaData", fileloc("mData"))
      metaData
    })

  # IMPACTmetadataTable ------
  output$IMPACTmetadataTable <- renderTable(
    {


      metaData <- getNewestVersion("dt.IMPACTmetaData", fileloc("mData"))
      metaData
    })
}


# Run the application
shinyApp(ui = ui, server = server)
# extra code
# sidebarLayout(
#   sidebarPanel(
#     helpText("Choose from the drop downs below to see the spider graphs of the consumption by a representative consumer compared to the nutrient requirement"),
#     selectInput(inputId = "countries",
#                 label = "Choose a country",
#                 choices = countryNames,
#                 selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
#     selectInput(inputId = "climateModels",
#                 choices = climateModelNames,
#                 label = "Choose a climate model",
#                 selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
#     selectInput(inputId = "SSPs",
#                 choices = SSPNames,
#                 label = "Choose an SSP scenario (only one available right now)",
#                 selected = "SSP2", multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
#     selectInput(inputId = "experiments",
#                 choices = experimentNames,
#                 label = "Choose an experiment (REF is the reference case)",
#                 selected = "REF", multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
#   ),

