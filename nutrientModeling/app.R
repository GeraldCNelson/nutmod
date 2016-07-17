# To use this, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(rsconnect)
source("global.R")
library(fmsb)
options(repos = c(CRAN = "http://cran.rstudio.com"))

rsconnect::setAccountInfo(name = 'nutrientmodeling', token = 'D0257883A5409984C3DC39101FAACA2E', secret = 'xxBEXU7/AvSa7/10LHFCU7uzXVCHiHP+T7Q8fJDB')
# rsconnect::deployApp()
dt.regions <- getNewestVersion("dt.regions.all",fileloc("mData"))
countryNames <- sort(unique(dt.regions$region_name.IMPACT159))

climateModelNames <- c("NoCC", "HGEM", "IPSL", "IPSL2")
SSPNames <- "SSP2"
RCPNames <- "RCP8.5"
experimentNames <- c("HiNARS2", "HiREFF2", "HiYld2", "IRREXP2", "IRREXP-WUE2", "LoYld2", "RegYld2", "SWHC2", "REF", "PHL-DEV2")

# Define UI

ui <- fluidPage(
  # Application title
  titlePanel("Nutrient intake and requirements "),
  textOutput("introText"),

  # sidebarLayout(
  #   sidebarPanel(
  #     helpText("Choose from the drop downs below to see the spider graphs of the consumption by a representative consumer compared to the nutrient requirment"),
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
  wellPanel(
    helpText("Choose from the drop downs below to see the spider graphs of the consumption by a representative consumer compared to the nutrient requirment"),
    selectInput(inputId = "countries",
                label = "Choose a country",
                choices = countryNames,
                selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
    selectInput(inputId = "climateModels",
                choices = climateModelNames,
                label = "Choose a climate model",
                selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
    selectInput(inputId = "SSPs",
                choices = SSPNames,
                label = "Choose an SSP scenario (only one available right now)",
                selected = "SSP2", multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
    selectInput(inputId = "experiments",
                choices = experimentNames,
                label = "Choose an experiment (REF is the reference case)",
                selected = "REF", multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
  ),
  # actionButton(inputId = "go",
  #              label = "Update"),
  # Show some spidergraphs
  mainPanel(
    fluidRow(
      column(width = 5,plotOutput("spiderGraphP1")),
      column(width = 5,plotOutput("spiderGraphP2"))
    ),
    fluidRow(
      column(width = 5,plotOutput("spiderGraphP3")),
      column(width = 5,plotOutput("spiderGraphP4"))
    ),
    fluidRow(
      column(width = 5,plotOutput("spiderGraphP5")),
      column(width = 5)
    )
  )
)

    server <- function(input, output) {

      output$spiderGraphP1 <- renderPlot({
        countryName <- input$countries
        country <- countryCodeLookup(countryName, "mData")
        years <- c("X2010","X2030","X2050")
        reqType <- "RDA_macro"

        nutSpiderGraph(reqType, country, input$SSPs, input$climateModels, input$experiments, years, fileloc("mData"))
      })

      output$spiderGraphP2 <- renderPlot({
        countryName <- input$countries
        country <- countryCodeLookup(countryName, "mData")
        years <- c("X2010","X2030","X2050")
        reqType <- "RDA_vits"

        nutSpiderGraph(reqType, country, input$SSPs, input$climateModels, input$experiments, years, fileloc("mData"))
      })

      output$spiderGraphP3 <- renderPlot({
        countryName <- input$countries
        country <- countryCodeLookup(countryName, "mData")
        years <- c("X2010","X2030","X2050")
        reqType <- "RDA_minrls"

        nutSpiderGraph(reqType, country, input$SSPs, input$climateModels, input$experiments, years, fileloc("mData"))
      })

      output$spiderGraphP4 <- renderPlot({
        countryName <- input$countries
        country <- countryCodeLookup(countryName, "mData")
        years <- c("X2010","X2030","X2050")
        reqType <- "UL_minrls"

        nutSpiderGraph(reqType, country, input$SSPs, input$climateModels, input$experiments, years, fileloc("mData"))
      })

      output$spiderGraphP5 <- renderPlot({
        countryName <- input$countries
        country <- countryCodeLookup(countryName, "mData")
        years <- c("X2010","X2030","X2050")
        reqType <- "UL_vits"

        nutSpiderGraph(reqType, country, input$SSPs, input$climateModels, input$experiments, years, fileloc("mData"))
      })

    }

    # Run the application
    shinyApp(ui = ui, server = server)

