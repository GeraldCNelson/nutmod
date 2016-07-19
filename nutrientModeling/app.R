# To use this, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(rsconnect)
library(data.table)
library(xtable)
library(fmsb)
source("global.R")
options(repos = c(CRAN = "http://cran.rstudio.com"))

rsconnect::setAccountInfo(name = 'nutrientmodeling', token = 'D0257883A5409984C3DC39101FAACA2E', secret = 'xxBEXU7/AvSa7/10LHFCU7uzXVCHiHP+T7Q8fJDB')
# rsconnect::deployApp()
dt.regions <- getNewestVersion("dt.regions.all",fileloc("mData"))
countryNames <- sort(unique(dt.regions$region_name.IMPACT159))
scenarios <- sort(keyVariable("scenarioListIMPACT"))
refScenarios <- c("SSP2-NoCC", "SSP2-HGEM", "SSP2-IPSL")
scenarioNames <- c(refScenarios, scenarios[!scenarios %in% refScenarios])

# Define UI

ui <- fluidPage(
  # Application title
  titlePanel("Nutrient intake and requirements.  "),
  textOutput("introText"),

  wellPanel(
    helpText("Choose from the drop downs below to see the spider graphs",
             "of the consumption by a representative consumer compared",
             "to the nutrient requirement. Draft results, not for citation or use, contact nelson.gerald.c@gmail.com for further info"),
    selectInput(inputId = "countryName",
                label = "Choose a country",
                choices = countryNames,
                selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
    selectInput(inputId = "scenarioName",
                choices = scenarioNames,
                label = "Choose a scenario",
                selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
  ),

  # Show some spidergraphs and underlying data tables
  mainPanel( width = "100%",
             #      tags$head(tags$style("#spiderTableP1 table {background-color: red; }", media = "screen", type = "text/css")),
             fluidRow(
               column(width = 6, plotOutput("spiderGraphP1", height = "200px")),
               column(width = 6, plotOutput("spiderGraphP2", height = "200px"))
             ),
             fluidRow(
               column(width = 6, div(tableOutput("spiderTableP1"), style = "font-size:60%")),
               column(width = 6, div(tableOutput("spiderTableP2"), style = "font-size:60%"))
             ),
             fluidRow(
               column(width = 6, plotOutput("spiderGraphP3", height = "200px")),
               column(width = 6, plotOutput("spiderGraphP4", height = "200px"))
             ),
             fluidRow(
               column(width = 6, div(tableOutput("spiderTableP3"), style = "font-size:60%")),
               column(width = 6, div(tableOutput("spiderTableP4"), style = "font-size:60%"))
             ),
             fluidRow(
               column(width = 6, plotOutput("spiderGraphP5", height = "200px")),
               column(width = 6)
             ),
             fluidRow(
               column(width = 6, div(tableOutput("spiderTableP5"), style = "font-size:60%")),
               column(width = 6)
             )
  )
)

server <- function(input, output) {
  years <- c("X2010", "X2030", "X2050")
  output$spiderGraphP1 <- renderPlot(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA_macro"
      nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
    })

  output$spiderTableP1 <- renderTable(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA_macro"
      temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
      temp2 <- as.data.frame(temp[1])
      temp2[4:nrow(temp2),]
    })

  output$spiderGraphP2 <- renderPlot(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA_vits"
      #    years <- c("X2010", "X2030", "X2050")
      nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
    })

  output$spiderTableP2 <- renderTable(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA_vits"
      temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
      temp2 <- as.data.frame(temp[1])
      temp2[4:nrow(temp2),]
    })

  output$spiderGraphP3 <- renderPlot({
    countryName <- input$countryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA_minrls"
    #   years <- c("X2010","X2030","X2050")
    nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  })

  output$spiderTableP3 <- renderTable(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "RDA_minrls"
      temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
      temp2 <- as.data.frame(temp[1])
      temp2[4:nrow(temp2),]
    })

  output$spiderGraphP4 <- renderPlot({
    countryName <- input$countryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL_minrls"
    #   years <- c("X2010","X2030","X2050")
    nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  })

  output$spiderTableP4 <- renderTable(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "UL_minrls"
      temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
      temp2 <- as.data.frame(temp[1])
      temp2[4:nrow(temp2),]
    })

  output$spiderGraphP5 <- renderPlot({
    countryName <- input$countryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL_vits"
    #   years <- c("X2010","X2030","X2050")
    nutSpiderGraph(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
  })
  output$spiderTableP5 <- renderTable(
    {
      countryName <- input$countryName
      countryCode <- countryCodeLookup(countryName, fileloc("mData"))
      reqType <- "UL_vits"
      temp <- reqRatiodatasetup(reqType, countryCode, input$scenarioName, years, fileloc("mData"))
      temp2 <- as.data.frame(temp[1])
      temp2[4:nrow(temp2),]
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

