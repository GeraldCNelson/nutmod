# To make sure data and script files are up to date, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio
library(shiny)
library(shinyjs)
library(rsconnect)
library(data.table)
library(xtable)
#library(fmsb) # for the spider charts
#library(gdata) # to reorder the scenario names
library(dplyr) # to do %>%
library(dtplyr)
library(shinythemes)
library(ggiraphExtra) #to do the interactive spider graphs
#library(markdown)
require(ggiraph)
source("global.R") # load all the background functions
options(repos = c(CRAN = "https://cran.rstudio.com"))

#load data that are not year or scenario specific; these are handled in the observe code in the server
years <- c("X2010", "X2030", "X2050")
yearsClean <- gsub("X", "", years)
dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
scenarioNames <- unique(dt.scenarioListIMPACT$scenario)
scenarioNames <- scenarioNames[!scenarioNames %in% c( "SSP2-IPSL-REF", "SSP2-MIROC-REF", "SSP2-GFDL-REF")]
resultFileLookup <- getNewestVersion("resultFileLookup", fileloc("mData"))
dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("mData"))
dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo", fileloc("mData"))
countryNames <- sort(unique(dt.regions.all$region_name.IMPACT159))
#development files
"dt.metadata" <- getNewestVersion("dt.metadata", fileloc("mData"))
"dt.IMPACTgdxParams" <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
FGreqChoices <- c("macro nutrients", "minerals", "vitamins")
staplesReqChoices <- c("energy","macro nutrients", "minerals", "vitamins")
initialCountryName <- "Afghanistan"
initialCountryCode <- countryCodeLookup(initialCountryName, fileloc("mData"))

# rsconnect::setAccountInfo(name = 'nutrientmodeling',
#                           token = '3E8A9773C46C19C6EF42EE20C8C65BF0',
#                           secret = 'hk4UOGDwRKr5Pkw2hKMzSxcRqL0GRsoU67shiwR/')
# rsconnect::deployApp(appDir = paste(getwd(),"nutrientModeling", sep = "/"))

load_data <- function() {
  #load data that are not year or scenario specific; these are handled in the observe code in the server
  years <- c("X2010", "X2030", "X2050")
  yearsClean <- gsub("X", "", years)
  dt.scenarioListIMPACT <- getNewestVersion("dt.scenarioListIMPACT", fileloc("mData"))
  scenarioNames <- unique(dt.scenarioListIMPACT$scenario)
  scenarioNames <- scenarioNames[!scenarioNames %in% c( "SSP2-IPSL-REF", "SSP2-MIROC-REF", "SSP2-GFDL-REF")]
  resultFileLookup <- getNewestVersion("resultFileLookup", fileloc("mData"))
  dt.regions.all <- getNewestVersion("dt.regions.all", fileloc("mData"))
  dt.foodGroupsInfo <- getNewestVersion("dt.foodGroupsInfo", fileloc("mData"))
  countryNames <- sort(unique(dt.regions.all$region_name.IMPACT159))
  #development files
  "dt.metadata" <- getNewestVersion("dt.metadata", fileloc("mData"))
  "dt.IMPACTgdxParams" <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))

  loadNresize <- function(dt) {
    print(dt)
    temp <- getNewestVersion(dt, fileloc("mData"))
    temp <- (temp[year %in% years])
    temp <- temp[scenario %in% scenarioNames]
    assign(dt, temp, envir = .GlobalEnv)
    return(dt)
  }
  dataSetsToLoad <- c(
    #affordability
    "dt.budgetShare",

    #availability
    "dt.foodAvail.foodGroup",

    #adequacy
    "dt.energy_ratios",
    "RDA.macro_sum_reqRatio",
    "RDA.vits_sum_reqRatio",
    "RDA.minrls_sum_reqRatio",

    #diversity
    "dt.shannonDiversity",
    "dt.KcalShare.nonstaple"
  )
  withProgress(message = 'Loading data', value = 0, {
    # Number of times we'll go through the loop
    n <- length(dataSetsToLoad)

    for (i in 1:n) {
      # load the data
      dt <- loadNresize(dataSetsToLoad[i])
      assign(dataSetsToLoad[i], dt)
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Loading file", i, "of", n))
    }
  })
  hide("loading_page")
  shinyjs::show("mainTabsetPanel")
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
  hidden(
    div(
      tabsetPanel(id = "mainTabsetPanel",
                  # Introduction tab panel -----
                  tabPanel(title = "Introduction",
                           mainPanel(width = "100%",
                                     includeHTML("www/introText.html")
                           )),
                  # Affordability tab panel ------
                  tabPanel(title = "Affordability",
                           titlePanel("Affordability of the current diet"),
                           wellPanel(
                             includeHTML("www/affordabilityText.html"),
                             helpText(
                               "Choose from the drop down below to see country-specific shares ",
                               "of the cost of per capita food availability in per capita income."
                             ),
                             selectInput(
                               inputId = "affordabilityCountryName",
                               label = "Choose a country", choices = countryNames,
                               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)),
                           # affordability tab main panel -----
                           mainPanel(width = "100%",
                                     fluidRow(align = "center",
                                              column(width = 12, div(tableOutput("affordabilityTable"), style = "font-size:80%"))))
                  ),
                  # availability tab panel ------
                  tabPanel(title = "Availability",
                           titlePanel("Availability by food group"),
                           includeHTML("www/availabilityText.html"),
                           wellPanel(
                             helpText("Choose from the drop down below to see average daily availability in grams by food groups."),
                             selectInput(
                               inputId = "availabilityCountryName",
                               label = "Choose a country", choices = countryNames,
                               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                             selectInput(
                               inputId = "availabilityScenarioName",
                               label = "Choose a scenario", choices = scenarioNames,
                               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                           ),
                           # availability tab main panel -----
                           mainPanel(width = "100%",
                                     fluidRow(align = "center",
                                              column(width = 12, plotOutput("availabilitySpiderGraphP1", height = "250px")),
                                              column(width = 12, div(tableOutput("availabilityTableP1"), style = "font-size:65%"))
                                     ))
                  ),
                  # Adequacy tab panel ------
                  tabPanel(title = "Adequacy",
                           titlePanel("Nutrient availability and requirements"),
                           includeHTML("www/adequacyText.html"),
                           # adequacy tab well panel
                           wellPanel(
                             helpText(
                               "Choose from the drop down below to see adequacy."),
                             selectInput(
                               inputId = "adequacyCountryName",
                               label = "Choose a country", choices = countryNames,
                               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                             selectInput(
                               inputId = "adequacyScenarioName",
                               choices = scenarioNames, label = "Choose a scenario",
                               selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)),
                           # Adequacy tab main panel -----
                           mainPanel(width = "100%",
                                     fluidRow(align = "center",
                                              column(width = 6, plotOutput("adequacySpiderGraphP1", height = "250px")),
                                              column(width = 6, plotOutput("adequacySpiderGraphP2", height = "250px"))),
                                     fluidRow(align = "center",
                                              column(width = 6, div(tableOutput("adequacyTableP1"), style = "font-size:65%")),
                                              column(width = 6, div(tableOutput("adequacyTableP2"), style = "font-size:65%"))),
                                     fluidRow(align = "center",
                                              column(width = 6, plotOutput("adequacySpiderGraphP3", height = "250px")),
                                              column(width = 6, plotOutput("adequacySpiderGraphP4", height = "250px"))),
                                     fluidRow(align = "center",
                                              column(width = 6, div(tableOutput("adequacyTableP3"), style = "font-size:65%")),
                                              column(width = 6, div(tableOutput("adequacyTableP4"), style = "font-size:65%"))),
                                     fluidRow(align = "center",
                                              column(width = 6, plotOutput("adequacySpiderGraphP5", height = "250px")),
                                              column(width = 6, plotOutput("energyRatioBarPlot", height = "250px"))),
                                     fluidRow(align = "center",
                                              column(width = 6, div(tableOutput("adequacyTableP5"), style = "font-size:65%")),
                                              column(width = 6, div(tableOutput("energyRatioTable"), style = "font-size:65%"))),
                                     includeHTML("www/adequacyWeightedRequirement.html")
                           )),
                  # Diversity tab panel, with tabset ------
                  tabPanel("Dietary diversity",
                           tabsetPanel(
                             tabPanel(title = "Shannon diversity index",
                                      wellPanel(
                                        includeHTML("www/shannonDiversityText.html"),
                                        # helpText("xxx"),
                                        selectInput(
                                          inputId = "diversityCountryName",
                                          label = "Choose a country", choices = countryNames,
                                          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)),
                                      # Diversity tab main panel -----
                                      mainPanel(width = "100%",
                                                fluidRow(column(width = 12, div(tableOutput("diversityTable"), style = "font-size:80%")))
                                      )),
                             # Food group tab panel ------
                             tabPanel(title = "Food group diversity",
                                      # titlePanel("Nutrient diversity by food groups"),
                                      includeHTML("www/foodGroupSpiderGraphText.html"),
                                      wellPanel(
                                        helpText(
                                          "Choose from the drop downs below to see country-specific spider graphs",
                                          "of nutrient sources by food group."),
                                        selectInput(
                                          inputId = "FGcountryName",
                                          label = "Choose a country",
                                          choices = countryNames,
                                          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                                        selectInput(
                                          inputId = "FGscenarioName",
                                          choices = scenarioNames,
                                          label = "Choose a scenario",
                                          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                                        selectInput(
                                          inputId = "nutrientGroup",
                                          choices = FGreqChoices,
                                          label = "Choose a nutrient group",
                                          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL) ),
                                      # Show some spidergraphs and underlying data tables
                                      mainPanel(width = "100%",
                                                #      tags$head(tags$style("#adequacyTableP1 table {background-color: red; }", media = "screen", type = "text/css")),
                                                fluidRow(column(width = 12, plotOutput("NutDiverFGspiderGraphP1", height = "1000px")))
                                      )),
                             # Staple tab panel ------
                             tabPanel(title = "Nonsttaple share of dietary energy",
                                      includeHTML("www/nonStapleShareGraphText.html"),
                                      wellPanel(
                                        helpText(
                                          "Choose from the drop downs below to see country-specific data",
                                          "on non-staple share of dietary energy."),
                                        selectInput(
                                          inputId = "stapleCountryName",
                                          label = "Choose a country", choices = countryNames,
                                          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL),
                                        selectInput(
                                          inputId = "stapleScenarioName",
                                          choices = scenarioNames,
                                          label = "Choose a scenario",
                                          selected = NULL, multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
                                      ),
                                      # Show a data table
                                      mainPanel(width = "100%",
                                                fluidRow(column(width = 12, div(tableOutput("nonStapleEnergyShareTable"), style = "font-size:80%")))
                                      ))
                           )),
                  # data and developer information tabs with tabset -----
                  tabPanel(
                    "Data and Developer Info",
                    tabsetPanel(
                      # Metadata tab panel -----
                      tabPanel(
                        title = "Metadata",
                        mainPanel(width = "100%",
                                  "Information for developers", " ",
                                  fluidRow(column(width = 12, div(tableOutput("metadataTable"), style = "font-size:80%"))))),
                      # IMPACT metadata tab panel -----
                      tabPanel(
                        title = "IMPACT metadata",
                        mainPanel(width = "100%",
                                  "Information for developers", " ",
                                  fluidRow(column(width = 12, div(tableOutput("IMPACTmetadataTable"), style = "font-size:80%"))))),
                      # Foodgroup lookup panel -----
                      tabPanel(
                        title = "Food group lookup table",
                        mainPanel(width = "100%",
                                  "Information for developers", " ",
                                  fluidRow(column(width = 12, div(tableOutput("IMPACTfoodgroupTable"), style = "font-size:80%"))))),
                      tabPanel(
                        title = "File documentation",
                        mainPanel(width = "100%",
                                  "Information for developers", " ",
                                  fluidRow(column(width = 12, div(tableOutput("fileDocumentation"), style = "font-size:80%")))))
                    )
                  )
      )
    )
  )
)

server <- function(input, output) {

  load_data()
  #   rv <- reactiveValues()
  # rv$setupComplete <- FALSE
  #
  # ## load data in this observe code -----
  # observe({
  #
  #   if (input$btn_data) { # need a different input
  #     loadNresize <- function(dt) {
  #       temp <- getNewestVersion(dt, fileloc("mData"))
  #       temp <- (temp[year %in% years])
  #       temp <- temp[scenario %in% scenarioNames]
  #       return(temp)
  #     }
  #     dataSetsToLoad <- c(
  #       #affordability
  #       "dt.budgetShare",
  #
  #       #availability
  #       "dt.foodAvail.foodGroup",
  #
  #       #adequacy
  #       "dt.energy_ratios",
  #       "RDA.macro_sum_reqRatio",
  #       "RDA.vits_sum_reqRatio",
  #       "RDA.minrls_sum_reqRatio",
  #
  #       #diversity
  #       "dt.shannonDiversity",
  #       "dt.KcalShare.nonstaple",
  #
  #       #development files
  #       "dt.metadata",
  #       "dt.IMPACTgdxParams")
  #
  #     withProgress(message = 'Loading data', value = 0, {
  #       # Number of times we'll go through the loop
  #       n <- length(dataSetsToLoad)
  #
  #       for (i in 1:n) {
  #         # load the data
  #         dt <- loadNresize(dataSetsToLoad[i])
  #         assign(dataSetsToLoad[i], dt)
  #         # Increment the progress bar, and update the detail text.
  #         incProgress(1/n, detail = paste("Loading file", i, "of", n))
  #       }
  #     })
  #
  #     # read in and resize data -----
  #     keepListColFGLU <- c( "IMPACT_code",  "description", "food_group_assignment", "food_group_code", "staple_code" )
  #     dt.foodGroupsInfo <- dt.foodGroupsInfo[, (keepListColFGLU), with = FALSE]
  #
  #     rv$setupComplete <- TRUE
  #   }
  #
  #   ## the conditional panel reads this output
  #   output$setupComplete <- reactive({
  #     return(rv$setupComplete)
  #   })
  #
  #   outputOptions(output, 'setupComplete', suspendWhenHidden = FALSE)
  #
  # })

  formula.energy.ratios <- paste("scenario + region_code.IMPACT159 + nutrient ~ year")
  dt.energy_ratios.wide <- data.table::dcast(
    dt.energy_ratios,
    formula = formula.energy.ratios,
    value.var = "value")
  nutnames <- cleanupNutrientNames(unique(dt.energy_ratios.wide$nutrient))

  keepListCol <- c("scenario", "region_code.IMPACT159", "year", "pcGDPX0", "incSharePCX0")
  formula.budgetShare <- paste("scenario + region_code.IMPACT159  ~ year")
  dt.budgetShare <- dt.budgetShare[, (keepListCol), with = FALSE]
  dt.budgetShare.wide <- data.table::dcast(
    dt.budgetShare,
    formula = formula.budgetShare,
    value.var = c("pcGDPX0", "incSharePCX0"))

  # availability server side -----
  output$availabilitySpiderGraphP1 <- renderPlot({
    # read in foodgroup availabiity data
    dt.foodAvail.foodGroup[, food_group_code := capwords(cleanupNutrientNames(food_group_code))]

    countryName <- input$availabilityCountryName
    scenarioName <- input$availabilityScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    inputData <- data.table::copy(dt.foodAvail.foodGroup)

    #to make the spider graph look better
    names.old <- c("Alcoholic Beverages", "Nonalcoholic Beverages", "Nuts And Seeds", "Roots And Plantain")
    names.new <- c("Alcoholic\nBeverages", "Nonalcoholic\nBeverages", "Nuts and\nSeeds", "Roots and\nPlantain")
    for (i in 1:length(names.old)) {
      inputData[food_group_code %in% names.old[i], food_group_code := names.new[i]]
    }
    formula.wide <- sprintf("scenario + region_code.IMPACT159 + year ~ %s", wideChoice)
    inputData.wide <- data.table::dcast(
      data = inputData,
      formula = formula.wide,
      value.var = "value")
    inputData.wide.cty <- inputData.wide[region_code.IMPACT159 %in% countryCode][,region_code.IMPACT159 := NULL]
    spiderData <- inputData.wide.cty[scenario %in% scenarioName,]
    spiderData[, scenario := NULL]
    titleText <- paste("Scenario: ", scen)
    p <- ggRadar(data = spiderData, mapping = aes(colour = year),
                 rescale = FALSE, interactive = FALSE, use.label = TRUE,
                 legend.position = "right")
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 11, family = "Times",
                                             face = "plain")) + ggtitle(titleText)
    ggiraph(code=print(p))
    #    nutReqSpiderGraph(availData, countryName, scenarioName, years, fileloc("mData"))
  })

  output$availabilityTableP1 <- renderTable({
    countryName <- input$availabilityCountryName
    scenarioName <- input$availabilityScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    #    md_file.md <- knitr::kable(inputData.wide.cty, format = "markdown", digits = 2)
    print(inputData.wide.cty)
    #    test <- includeMarkdown(md_file.md)
    # availData <- "dt.foodAvail.foodGroup"
    # temp <-
    #   nutReqDataPrep(availData, countryCode, scenarioName, years, fileloc("mData"))
    # namelist <- colnames(temp)
    # temp <- temp[4:nrow(temp)][, year := yearsClean]
    # setcolorder(temp, c("year", namelist))
    #, )include.rownames = FALSE)
  } )

  #+++++++++

  # adequacy server side -----
  output$adequacySpiderGraphP1 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.macro_sum_reqRatio"
    #    print(paste(countryName, scenarioName, countryCode))
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP1 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.macro_sum_reqRatio"
    temp <-
      as.data.table(nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
      )
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
    setcolorder(temp, c("year", namelist))
  }, include.rownames = FALSE)

  output$adequacySpiderGraphP2 <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.vits_sum_reqRatio"
    temp  <-
      nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP2 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.vits_sum_reqRatio"
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
    reqType <- "RDA.minrls_sum_reqRatio"
    #   years <- c("X2010","X2030","X2050")
    nutReqSpiderGraph(reqType, countryCode, scenarioName, years, fileloc("mData"))
  })

  output$adequacyTableP3 <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <-
      countryCodeLookup(countryName, fileloc("mData"))
    reqType <- "RDA.minrls_sum_reqRatio"
    temp <-
      nutReqDataPrep(reqType, countryCode, scenarioName, years, fileloc("mData"))
    namelist <- colnames(temp)
    temp <- temp[4:nrow(temp)][, year := yearsClean]
    setcolorder(temp, c("year", namelist))
  }, include.rownames = FALSE)

  # energy ratio bar chart -----
  output$energyRatioBarPlot <- renderPlot({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    temp <- dt.energy_ratios.wide[region_code.IMPACT159 %in% countryCode & scenario %in% scenarioName,]
    colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    # print(as.matrix(temp[1:4,years, with = FALSE]))
    barplot(as.matrix(temp[2:6,years, with = FALSE]), main = "Share of energy consumption",
            col = colors_in, ylim = c(0,1))
    legend(x = "bottomright", y = NULL, legend = nutnames[2:length(nutnames)], bty = "n", pch = 20,
           col = colors_in, text.col = "black", cex = .8, pt.cex = .8, pt.lwd = 1,
           y.intersp = .8)
  })

  output$energyRatioTable <- renderTable({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    temp <- dt.energy_ratios.wide[region_code.IMPACT159 == countryCode & scenario == scenarioName,]
    print(temp)
    keepListCol <- c("region_code.IMPACT159", "nutrient", years)
    temp <- temp[,(keepListCol), with = FALSE]
    setnames(temp, old = c("region_code.IMPACT159",years), new = c("region", yearsClean))
    temp[,region := NULL]
    temp[,nutrient := nutnames]
    temp[1:6]
  }, include.rownames = FALSE)

  # staples diversity metrics -----
  output$nonStapleEnergyShareTable <- renderTable({
    countryName <- input$stapleCountryName
    scenarioName <- input$stapleScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    nutrientGroupStaples <- input$nutrientGroupStaples
    temp <- dt.KcalShare.nonstaple[region_code.IMPACT159 == countryCode,]

    #    print(nutrientGroupStaples)
    print(unique(temp$nutrient))

    temp <- temp[nutrient %in% (nutrientSelector), ]
    idVars <- basicInputs
    measureVars <- nutrientSelector
    print(measureVars)

    formula.wide <- paste("scenario + region_code.IMPACT159 + nutrient + year ~ staple_code")
    temp.wide <- data.table::dcast(
      data = temp,
      formula = formula.wide,
      value.var = "value")
    temp.wide[,c("region_code.IMPACT159") := NULL]
    temp.wide[, year := gsub("X", "", temp.wide$year)]
    temp.wide[, nonStapleShare := nonstaple/(nonstaple + staple)]
    temp.wide[]
    #   temp.melt
  }, include.rownames = FALSE)

  # food group diversity spider graphs -----
  output$NutDiverFGspiderGraphP1 <- renderPlot({
    countryName <- input$FGcountryName
    reqName <- input$nutrientGroup
    if (reqName == "macro nutrients") reqFileName <- "RDA.macro_FG_reqRatio"
    if (reqName == "minerals") reqFileName <- "RDA.minrl_FG_reqRatio"
    if (reqName == "vitamins") reqFileName <- "RDA.vits_FG_reqRatio"
    FGscenarioName <- input$FGscenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    nutshareSpiderGraph(reqFileName, countryCode, FGscenarioName, years, fileloc("mData"))
  })

  # affordabilityTable -----
  output$affordabilityTable <- renderTable({
    countryName <- input$affordabilityCountryName
    #scenarioName <- input$scenarioName xxx
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListCol <- c("scenario", "year", "pcGDPX0", "budget.PCX0", "incSharePCX0")
    temp <- dt.budgetShare.wide[region_code.IMPACT159 %in% countryCode]
    temp[, region_code.IMPACT159 := NULL]
    #    setnames(
    #   temp,
    #   old = c("pcGDPX0", "budget.PCX0", "incSharePCX0"),
    #   new = c("Per capita GDP", "Budget", "Expend. share (%)")
    # )
    # # reorder scenarios in temp to be the same order as scenarioNames
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
    keepListCol <-  c("scenario", "year", "SD", "SDnorm")
    temp <- dt.shannonDiversity[region_code.IMPACT159 == countryCode, keepListCol, with = FALSE]
    # reorder scenarios in temp to be the same order as scenarioNames
    temp$scenario <- reorder.factor(temp$scenario, new.order = scenarioNames)
    temp <- temp %>% arrange(scenario)
    temp
  }, include.rownames = FALSE)

  # metadataTable ------
  output$metadataTable <- renderTable({
    dt.metadata
  })

  # IMPACTmetadataTable ------
  output$IMPACTmetadataTable <- renderTable({
    dt.IMPACTgdxParams
  }, include.rownames = FALSE)

  # IMPACTfoodgroupLookupTable ------

  output$IMPACTfoodgroupTable <- renderTable({
    dt.foodGroupsInfo
  }, include.rownames = FALSE)

  # file documentation
  output$fileDocumentation <- renderTable({
    resultFileLookup
  }, include.rownames = FALSE)
}

# Run the application -----
shinyApp(ui = ui, server = server)

