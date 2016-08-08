# To make sure data and script files are up to date, first run copyFilestoNutrientModeling.R
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
library(shinythemes)
options(repos = c(CRAN = "http://cran.rstudio.com"))

rsconnect::setAccountInfo(name = 'nutrientmodeling',
                          token = 'D0257883A5409984C3DC39101FAACA2E',
                          secret = 'xxBEXU7/AvSa7/10LHFCU7uzXVCHiHP+T7Q8fJDB')
# rsconnect::deployApp()
dt.regions <- getNewestVersion("dt.regions.all", fileloc("mData"))
countryNames <- sort(unique(dt.regions$region_name.IMPACT159))
scenarioNames <- sort(keyVariable("scenarioListIMPACT"))
# refScenarios <- c("SSP2-NoCC", "SSP2-HGEM", "SSP2-IPSL")
# scenarioNames <- c(refScenarios, scenarioNames[!scenarioNames %in% refScenarios])
resultFileLookup <-
  data.table::as.data.table(read.csv("data/ResultFileLookup.csv"))
reqChoices <- as.character(resultFileLookup$reqType)
FGreqChoices <- reqChoices[grep("FG", reqChoices)]

# Define UI -----
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  title = "Nutrient modeling",
  tabsetPanel(
    # Introduction tab panel -----
    tabPanel(title = "Introduction",
             mainPanel(
               width = "100%",
               includeHTML("www/introText.html")
             )),
    # Affordability tab panel ------
    tabPanel(
      title = "Affordability",
      titlePanel("Affordability of the current diet"),
      wellPanel(
        includeHTML("www/affordabilityText.html"),
        helpText(
          "Choose from the drop downs below to see country-specific shares",
          "expenditures on food by a representative consumer compared",
          "to per capita income."
        ),
        selectInput(
          inputId = "countryName",
          label = "Choose a country",
          choices = countryNames,
          selected = NULL,
          multiple = FALSE,
          selectize = FALSE,
          width = NULL,
          size = NULL
        ),
        mainPanel(width = "100%",
                  fluidRow(align = "center",
                           column(
                             width = 12, div(tableOutput("affordabilityTable"), style = "font-size:80%")
                           )))
      )
    ),

    # Adequacy tab panel ------
    tabPanel(
      title = "Adequacy",
      titlePanel("Nutrient availability and requirements"),
      includeHTML("www/adequacyText.html"),
      # adequacy tab well panel
      wellPanel(
        # helpText("xxx"),
        selectInput(
          inputId = "adequacyCountryName",
          label = "Choose a country",
          choices = countryNames,
          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL
        ),
        selectInput(
          inputId = "adequacyScenarioName",
          choices = scenarioNames,
          label = "Choose a scenario",
          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL
        )
      ),
      # Adequacy tab main panel -----
      mainPanel(width = "100%",
                fluidRow(align = "center",
                         column(width = 6, plotOutput("adequacySpiderGraphP1", height = "250px")
                         ),
                         column(width = 6, plotOutput("adequacySpiderGraphP2", height = "250px")
                         )),
                fluidRow(align = "center",
                         column(width = 6, div(tableOutput("adequacyTableP1"), style = "font-size:60%")
                         ),
                         column(width = 6, div(tableOutput("adequacyTableP2"), style = "font-size:60%")
                         )),
                fluidRow(align = "center",
                         column(width = 6, plotOutput("adequacySpiderGraphP3", height = "250px")
                         ),
                         column(width = 6, plotOutput("adequacySpiderGraphP4", height = "250px")
                         )),
                fluidRow(align = "center",
                         column(width = 6, div(tableOutput("adequacyTableP3"), style = "font-size:60%")
                         ),
                         column(width = 6, div(tableOutput("adequacyTableP4"), style = "font-size:60%")
                         )),
                fluidRow(align = "center",
                         column(width = 6, plotOutput("adequacySpiderGraphP5", height = "250px")
                         ),
                         column(width = 6, plotOutput("energyRatio", height = "250px")
                         )),
      fluidRow(align = "center",
               column(width = 6, div(tableOutput("adequacyTableP5"), style = "font-size:60%")
               )),
      includeHTML("www/adequacyWeightedRequirement.html")
    )
  ),
  # Diversity tab panel ------
  tabPanel("Dietary diversity metrics",
           tabsetPanel(
             tabPanel(
               title = "Shannon diversity index",
               wellPanel(
                 includeHTML("www/shannonDiversityText.html"),
                 # helpText("xxx"),
                 selectInput(
                   inputId = "diversityCountryName",
                   label = "Choose a country",
                   choices = countryNames,
                   selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL
                 )
               ),
               # Diversity tab main panel -----
               mainPanel(width = "100%",
                         fluidRow(column(
                           width = 12, div(tableOutput("diversityTable"), style = "font-size:80%")
                         )))
             ),
             # Food group tab panel ------
             tabPanel(
               title = "Food group diversity",
               #         titlePanel("Nutrient diversity by food groups"),
               includeHTML("www/foodGroupSpiderGraphText.html"),
               wellPanel(
                 helpText(
                   "Choose from the drop downs below to see country-specific spider graphs",
                   "of nutrient sources by food group."
                 ),
                 selectInput(
                   inputId = "FGcountryName",
                   label = "Choose a country",
                   choices = countryNames,
                   selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL
                 ),
                 selectInput(
                   inputId = "FGscenarioName",
                   choices = scenarioNames,
                   label = "Choose a scenario",
                   selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL
                 ),
                 selectInput(
                   inputId = "nutrientGroup",
                   choices = FGreqChoices,
                   label = "Choose a nutrient group",
                   selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL
                 )
               ),
               # Show some spidergraphs and underlying data tables
               mainPanel(width = "100%",
                         #      tags$head(tags$style("#adequacyTableP1 table {background-color: red; }", media = "screen", type = "text/css")),
                         fluidRow(column(
                           width = 12,
                           plotOutput("NutDiverFGspiderGraphP1", height = "1000px")
                         )))
             )
           )),
  # data and developer information tabs -----
  tabPanel(
    "Data and Developer Info",
    tabsetPanel(
      # Metadata tab panel -----
      tabPanel(
        title = "Metadata",
        mainPanel(width = "100%",
                  "Information for developers",
                  " ",

                  fluidRow(column(
                    width = 12, div(tableOutput("metadataTable"), style = "font-size:80%")
                  )))
      ),
      # IMPACT metadata tab panel -----
      tabPanel(
        title = "IMPACT metadata",
        mainPanel(width = "100%",
                  "Information for developers",
                  " ",
                  fluidRow(column(
                    width = 12, div(tableOutput("IMPACTmetadataTable"), style = "font-size:80%")
                  )))
      ),
      # Foodgroup lookup panel -----
      tabPanel(
        title = "Food group lookup table",
        mainPanel(width = "100%",
                  "Information for developers",
                  " ",
                  fluidRow(column(
                    width = 12, div(tableOutput("IMPACTfoodgroupTable"), style = "font-size:80%")
                  )))
      )
    )
  )
)
)

server <- function(input, output) {
  years <- c("X2010", "X2030", "X2050")
  yearsClean <- gsub("X", "", years)
  dt.energyRatios <- getNewestVersion("dt.energy.ratios", fileloc("mData"))
  keepYearList <- keyVariable("keepYearList")
  idVars <- c("scenario", "region_code.IMPACT159", "nutrient")
  dt.energyRatios.long <-  data.table::melt(
    dt.energyRatios,
    id.vars = idVars,
    variable.name = "year",
    measure.vars = keepYearList,
    variable.factor = FALSE
  )
  dt.energyRatios.long <- dt.energyRatios.long[year %in% years,]

  # adequacy server side -----
  output$adequacySpiderGraphP1 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.macro"
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP1 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.macro"
    #      print(paste(reqType,countryCode, scenarioName, years, sep = ", "))
    temp <-
      as.data.table(nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
      )
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
    setcolorder(temp, c("year", namelist))
    #      print(temp)
    temp
  }, include.rownames = FALSE)

  output$adequacySpiderGraphP2 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.vits"
    temp  <-
      nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP2 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.vits"
    temp <-
      nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
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

  output$adequacyTableP3 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.minrls"
    temp <-
      nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
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

  output$adequacyTableP4 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL.minrls"
    temp <-
      nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
    setcolorder(temp, c("year", namelist))
  }, include.rownames = FALSE)


  output$adequacySpiderGraphP5 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL.vits"
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP5 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "UL.vits"
    temp <-
      nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
    setcolorder(temp, c("year", namelist))
  }, include.rownames = FALSE)
  # energy ratio bar chart -----
  output$energyRatio <- renderPlot({
    dt.energy.ratios <- getNewestVersion("dt.energy.ratios")
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    temp <- dt.energy.ratios[region_code.IMPACT159 == countryCode & scenario == scenarioName,]
    keepListCol <- c("region_code.IMPACT159", "nutrient", years)
    temp <- temp[,(keepListCol), with = FALSE]
    setnames(temp, old = years, new = yearsClean)
    nutnames <- cleanupNutrientNames(temp[1:4,nutrient])
    colors_in <- c( "yellow", "green", "blue","red" )
    barplot(as.matrix(temp[1:4,yearsClean, with = FALSE]), main = "Share of energy consumption",
            col = colors_in, ylim = c(0,1))
    legend(x = "bottomright", y = NULL, legend = nutnames, bty = "n", pch = 20,
           col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
           y.intersp = .8)

  })

  # food group diversity spider graphs -----
  output$NutDiverFGspiderGraphP1 <- renderPlot({
    countryName <- input$FGcountryName
    reqType <- input$nutrientGroup
    scenarioName <- input$FGscenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    nutshareSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  # affordabilityTable -----
  output$affordabilityTable <- renderTable({
    countryName <- input$countryName
    #scenarioName <- input$scenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    dt.budgetShare <-
      getNewestVersion("dt.budgetShare", fileloc("mData"))
    keepListCol <-
      c("scenario", "year", "pcGDPX0", "budget.PCX0", "incSharePCX0")
    temp <- dt.budgetShare[region_code.IMPACT159 == countryCode &
                             year %in% years, keepListCol, with = FALSE]
    temp[, incSharePCX0 := incSharePCX0 * 100]
    setnames(
      temp,
      old = c("pcGDPX0", "budget.PCX0", "incSharePCX0"),
      new = c("Per capita GDP", "Budget", "Expend. share (%)")
    )
    # reorder scenarios in temp to be the same order as scenarioNames
    temp$scenario <-
      reorder.factor(temp$scenario, new.order = scenarioNames)
    temp <- temp %>% arrange(scenario)
    temp
  }, include.rownames = FALSE)

  # diversityTable -----
  output$diversityTable <- renderTable({
    countryName <- input$diversityCountryName
    #scenarioName <- input$scenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    dt.shannonDiversity <-
      getNewestVersion("dt.shannonDiversity", fileloc("mData"))
    keepListCol <-  c("scenario", "year", "SD", "SDnorm")
    temp <-
      dt.shannonDiversity[region_code.IMPACT159 == countryCode &
                            year %in% years, keepListCol, with = FALSE]
    # reorder scenarios in temp to be the same order as scenarioNames
    temp$scenario <-
      reorder.factor(temp$scenario, new.order = scenarioNames)
    temp <- temp %>% arrange(scenario)
    temp
  }, include.rownames = FALSE)

  # metadataTable ------
  output$metadataTable <- renderTable({
    metaData <- getNewestVersion("metaData", fileloc("mData"))
    metaData
  })

  # IMPACTmetadataTable ------
  output$IMPACTmetadataTable <- renderTable({
    metaData <- getNewestVersion("dt.IMPACTmetaData", fileloc("mData"))
    metaData
  }, include.rownames = FALSE)
  # IMPACTfoodgroupLookupTable ------

  output$IMPACTfoodgroupTable <- renderTable({
    foodGroupLU <-
      getNewestVersion("dt.foodGroupsInfo")
    foodGroupLU
  }, include.rownames = FALSE)
}

# Run the application -----
shinyApp(ui = ui, server = server)

