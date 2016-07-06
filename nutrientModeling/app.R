#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(rsconnect)
source("global.R")
#rsconnect::deployApp("/Users/gcn/Documents/workspace/nutmod/nutrientModeling/")
dt.regions <- getNewestVersion("dt.regions.all",fileloc("mData"))
countryNames <- sort(unique(dt.regions$region_name.IMPACT159))

climateModelNames <- c("HGEM", "HGEM2", "IPSL",  "IPSL2", "NoCC")
SSPNames <- "SSP2"
RCPNames <- "RCP8.5"
experimentNames <- c("HiNARS2", "HiREFF2", "HiYld2", "IRREXP2", "IRREXP-WUE2", "LoYld2", "RegYld2", "SWHC2", "REF", "PHL-DEV2")

# Define UI

ui <- fluidPage(
  # Application title
  titlePanel("Nutrient Modeling Test"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
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
    # Show some spidergraphs
    mainPanel(
      plotOutput("spiderGraph")
    )

  )
)

server <- function(input, output) {

  output$spiderGraph <- renderPlot({
    countryName <- input$countries
    country <- countryCodeLookup(countryName, "mData")
    years <- c("X2010","X2030","X2050")
    reqType <- "RDA_macro"

    nutSpiderGraph(reqType, country, input$SSPs, input$climateModels, input$experiments, years, fileloc("mData"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

