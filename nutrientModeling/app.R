#' Shiny app for the nutrient model results
#' title: "Shiny app for the nutrient model results"
#' @keywords shiny app
#' @name app.R
#' @author Gerald C. Nelson, \\email{nelson.gerald.c@@gmail.com}
#' @description
#' This script contains the shiny app for the nutrient modeling website

#Copyright (C) 2015-2017 Gerald C,Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
# To make sure data and script files are up to date, first run copyFilestoNutrientModeling.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio
library(shiny)
library(shinyjs)
library(ggplot2)
#library(rsconnect)
library(data.table)
library(dplyr) # to do %>%
library(dtplyr)
library(DT) # needs to come after library(shiny)
library(shinythemes)
# devtools::install_github("strengejacke
# library(sjlabelled)
# library(sjmisc)
library(strengejacke)
library(ggiraphExtra) #to do the interactive spider graphs. As of May 27, 2017, needs to be installed with devtools::install_github('cardiomoon/ggiraphExtra')
#library(markdown)
require(ggiraph)
library(RColorBrewer)
library(shiny.router) # install using devtools::install_github("Appsilon/shiny.router"); using for table of contents
source("global.R") # load all the background functions and install packages if not already done
#note install packages feature in global.R is commented out
options(repos = c(CRAN = "https://cran.rstudio.com"))
gdxChoice <- paste0(getwd(), "/data/gdxInfo.csv")

# router <- make_router(
#   #  route("/", root_page),
#   #  route("/other", other_page),
#   route("/Affordability", Affordability)
# )

# This generates menu in user interface with links.
menu <- (
  tags$ul(
    #    tags$li(a(class = "item", href = "/", "Page")),
    tags$li(a(class = "item", href = "/Affordability", "Affordability"))
  )
)


#' files for the development tab section
FGreqChoices <- c("macro nutrients", "minerals", "vitamins")
staplesReqChoices <- c("energy","macro nutrients", "minerals", "vitamins")
initialCountryName <- "India"
initialScenarioName <- "SSP2-NoCC"
initialCountryCode <- countryCodeLookup(initialCountryName, fileloc("mData"))
userCountryChoice <- initialCountryName # until user chooses something different in the first tab
userScenarioChoice <- initialScenarioName # until user chooses something different in the first tab
#' the rsconnect code below is only for use in setting up the website at the shinyapps.io website
# rsconnect::setAccountInfo(name = 'nutrientmodeling',
#                           token = '3E8A9773C46C19C6EF42EE20C8C65BF0',
#                           secret = 'hk4UOGDwRKr5Pkw2hKMzSxcRqL0GRsoU67shiwR/')
# rsconnect::deployApp(appDir = paste(getwd(),"nutrientModeling", sep = "/"))

#region groupings
WBcountries <- list()
region_name.WB <- c("Low income", "Lower middle income", "Upper middle income", "High income")
dt <- data.table::copy(dt.regions.all)
for (i in 1:length(region_name.WB)) {
  dt <- dt.regions.all[region_name.WB %in% region_name.WB[i]]
  temp <- sort(unique(dt$region_name.IMPACT159))
  temp <- gsub(" plus", "", temp)
  temp <- temp[temp %in% countryNames]
  WBcountries[[region_name.WB[i]]] <- temp
}

dataSetsToLoad <- c(
  "dt.foodAvail_foodGroup",
  "dt.nutrients_kcals",
  "food_agg_AMDR_hi",
  "reqRatio_sum_RDA_macro",
  "reqRatio_sum_RDA_vits",
  "reqRatio_sum_RDA_minrls",
  "dt.nutBalScore",
  "dt.MRVRatios",
  "dt.shannonDiversity",
  "dt.KcalShare_nonstaple",
  "dt.RAOqe"
)

# edited Aug 10, 2018 to deal with the .var element of file names
dataSetsToLoad <- paste0(dataSetsToLoad, ".var")
dataSetsToLoad <- c(dataSetsToLoad, "dt.budgetShare") # no var version of this.

# use for big data sets
dataSetsToLoad.supplemental <- c("dt.nutrients_sum_FG.var")

dataSetsToLoad.desc <- c("Food availability by food group", "Kilocalorie availability", "AMDR ratios", "Adequacy, macro",
                         "Adequacy, vitamins", "Adequacy, minerals", "Nutrient balance score", "Maximum recommended intake score", "Shannon diversity", "Share of kcals, nonstaples",
                         "Rao's QE", "Budget share")
dataSetsToLoad.desc.supplemental <- c("Nutrient availability")

datasetsToLoad.complete <- c(dataSetsToLoad, dataSetsToLoad.supplemental)
datasetsToLoad.desc.complete <- c(dataSetsToLoad.desc, dataSetsToLoad.desc.supplemental)

#' foodGroupNames and foodGroupNamesNoWrap must align
codeNames.foodGroups <- c("alcohol", "beverages", "cereals", "dairy", "eggs", "fish", "fruits", "meats", "nutsNseeds",
                          "oils", "pulses", "rootsNPlantain", "sweeteners", "vegetables")
foodGroupNamesNoWrap <- c("Beverages, alcoholic", "Beverages, nonalcoholic", "Cereals", "Dairy", "Eggs", "Fish", "Fruits", "Meats", "Nuts and seeds",
                          "Oils", "Pulses", "Roots and plantain", "Sweeteners", "Vegetables")
foodGroupNamesWrap <- c("Beverages,\nalcoholic", "Beverages,\nnonalcoholic", "Cereals", "Dairy", "Eggs", "Fish", "Fruits", "Meats", "Nuts and\nseeds",
                        "Oils", "Pulses", "Roots and\nplantain", "Sweeteners", "Vegetables")
codeNames.macro <- c("carbohydrate_g", "protein_g", "totalfiber_g")
nutNamesNoUnitsWrap.macro <- c("Carbohydrate", "Protein", "Total fiber")
codeNames.vits <- c("folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg", "vit_a_rae_µg", "vit_b12_µg", "vit_b6_mg", "vit_c_mg",
                    "vit_d_µg", "vit_e_mg", "vit_k_µg")
nutNamesNoUnitsWrap.vits <- c("Folate", "Niacin", "Riboflavin", "Thiamin", "Vitamin\nA RAE", "Vitamin\nB12", "Vitamin\nB6", "Vitamin\nC",
                              "Vitamin\nD", "Vitamin\nE", "Vitamin\nK")
codeNames.minrls <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
nutNamesNoUnitsWrap.minrls <- c("Calcium", "Iron", "Magnesium", "Phosphorus", "Potassium", "Zinc")

# Define UI -----
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  title = "Nutrient modeling",

  router_ui(), # needed for table of contents

  useShinyjs(debug = TRUE),
  div(
    id = "loading_page",
    includeHTML("www/introText.html")
  ),
  hidden( # used to make the tabs not appear until the data sets are finished loading
    div(id = "mainTabsetPanel",
        tabsetPanel(
          # Introduction tab panel -----
          tabPanel(title = "Introduction",
                   sidebarLayout(
                     sidebarPanel( #width = 2,
                       includeHTML("www/countryChoice.html"),
                       selectizeInput(inputId = "userCountryName", label = "Choose country", choices = countryNames, selected = NULL),
                       selectizeInput(inputId = "userScenarioName", label = "Choose scenario", choices = scenarioNames, selected = initialScenarioName)),
                     mainPanel(includeHTML("www/introText.html")))),
          # Affordability tab panel ------
          tabPanel(title = "Affordability",
                   sidebarLayout(
                     sidebarPanel(
                       selectizeInput(inputId = "affordabilityCountryName", label = "Choose country", choices = countryNames,
                                      options = list(placeholder = "Select country")),
                       downloadButton("downloadData.afford", "Download")),
                     mainPanel(titlePanel("Food expenditure, per capita income and affordability"),
                               includeHTML("www/affordabilityText.html"),
                               DT::dataTableOutput("affordabilityTable"),
                               includeHTML("www/tableNote_affordability.html")))),
          # availability tab panel ------
          tabPanel(title = "Food Availability",
                   sidebarLayout(
                     sidebarPanel(
                       selectizeInput(inputId = "availabilityCountryName", label = "Choose country",
                                      choices = countryNames,
                                      options = list(placeholder = "Select country")),

                       # selectizeInput(inputId = "availabilityScenarioName", label = "Choose a scenario (see definition in glossary)",
                       #                choices = scenarioNames),
                       downloadButton("downloadData.avail", "Download")),
                     mainPanel(titlePanel("Average daily food availability by food group"),
                               includeHTML("www/availabilityText.html"),
                               radioButtons("availabilityScenarioName", "Choose scenario (See glossary for details):",
                                            list("SSP2-NoCC", "SSP2-HGEM", "SSP1-NoCC", "SSP3-NoCC"), inline = TRUE),
                               ggiraphOutput("availabilitySpiderGraphP1", height = "400px"),
                               DT::dataTableOutput("availabilityTableP1")))),
          # Nut availability by food group tab panel ------
          tabPanel(title = "Nutrient availability",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                                  selectizeInput(inputId = "FGcountryName", label = "Choose country", choices = countryNames),
                                  selectizeInput(inputId = "nutrientGroup", label = "Choose a nutrient group", choices = c("vitamins", "minerals", "macronutrients")),
                                  downloadButton("downloadData.nutAvailFG", "Download")),
                     mainPanel(titlePanel("Nutrient availability"),
                               includeHTML("www/foodGroupSpiderGraphText.html"),
                               radioButtons("FGscenarioName", "Choose scenario (See glossary for details):",
                                            list("SSP2-NoCC", "SSP2-HGEM", "SSP1-NoCC", "SSP3-NoCC"), inline = TRUE),
                               uiOutput("plot.NutAvailFGbarGraphP1"),
                               DT::dataTableOutput("NutAvailFGTable"),
                               includeHTML("www/nutrientDescription.html")))),

          # Adequacy tab panel ------
          tabPanel(title = "Under- and over nutrition",
                   tabsetPanel(
                     # adequacy tab panel -----
                     tabPanel(title = "Adequacy ratios",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "adequacyCountryName", label = "Choose country", choices = countryNames),
                                             #                                            selectizeInput(inputId = "adequacyScenarioName", label = "Choose a scenario (see definition in glossary)", choices = scenarioNames),
                                             downloadButton("downloadData.adequacy.macro", "Macro data"),
                                             downloadButton("downloadData.adequacy.vits", "Vitamin data"),
                                             downloadButton("downloadData.adequacy.minrls", "Minerals data"),
                                             downloadButton("downloadData.energyRat", "Energy share data"),
                                             downloadButton("downloadData.energyQ", "Kcal data")
                                ),
                                mainPanel(titlePanel("Nutrient adequacy and kilocalorie availability"),
                                          includeHTML("www/adequacyText.html"),
                                          radioButtons("adequacyScenarioName", "Choose scenario (See glossary for details):",
                                                       list("SSP2-NoCC", "SSP2-HGEM", "SSP1-NoCC", "SSP3-NoCC"), inline = TRUE),
                                          fluidRow(
                                            #   column(width = 6, ggiraphOutput("adequacySpiderGraphP1", height = "400px")),
                                            #   column(width = 6, ggiraphOutput("adequacySpiderGraphP2", height = "400px"))),
                                            column(width = 6, ggiraphOutput("adequacySpiderGraphP1")),
                                            column(width = 6, ggiraphOutput("adequacySpiderGraphP2"))),
                                          fluidRow(
                                            # column(width = 6, ggiraphOutput("adequacySpiderGraphP3", height = "400px"))),
                                            column(width = 6, ggiraphOutput("adequacySpiderGraphP3"))),
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
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "AMDRCountryName", label = "Choose country", choices = countryNames),
                                             downloadButton("downloadData.AMDR", "Download")),
                                mainPanel(titlePanel("Acceptable Macronutrient Distribution Range (AMDR)"),
                                          # titlePanel("Acceptable Macronutrient Distribution Range"),
                                          includeHTML("www/AMDRText.html"),
                                          fluidRow(column(width = 12, ggiraphOutput("AMDRbarGraphP1"))),
                                          fluidRow(column(width = 12, DT::dataTableOutput("AMDRTableP1"))),
                                          includeHTML("www/tableNote_AMDR.html")))),
                     # nutrient balance tab panel -----
                     tabPanel(title = "Nutrient Balance Score",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "nutbalCountryName", label = "Choose country", choices = countryNames),
                                             downloadButton("downloadData.nutbal", "Download")),
                                mainPanel(
                                  # titlePanel("Nutrient balance score"),
                                  includeHTML("www/nutbalGraphText.html"),
                                  DT::dataTableOutput("nutbalTableP1")))),
                     # MRV tab panel -----
                     tabPanel(title = "Maximum Recommended Intake",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "MRVCountryName", label = "Choose country", choices = countryNames),
                                             downloadButton("downloadData.MRV", "Download")),
                                mainPanel(titlePanel("Maximum Recommended Intake (MRV) results"),
                                          includeHTML("www/MRVText.html"),
                                          DT::dataTableOutput("MRVTableP1")))))),
          # Diversity tab panel, with tabset ------
          tabPanel("Dietary diversity",
                   tabsetPanel(
                     # Shannon diversity tab panel ------
                     tabPanel(title = "Shannon diversity index",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "diversityCountryName", label = "Choose country", choices = countryNames),
                                             downloadButton("downloadData.ShannonDiversity", "Download")),
                                mainPanel(titlePanel("Shannon diversity index"),
                                          includeHTML("www/shannonDiversityText.html"),
                                          DT::dataTableOutput("diversityTable")))),

                     # nonstaple energy share tab panel ------
                     tabPanel(title = "Nonstaple share of dietary energy",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "nonstapleEnergyShareCountryName", label = "Choose country", choices = countryNames),
                                             downloadButton("downloadData.nonStapleShare", "Download")),
                                mainPanel(
                                  titlePanel("Nonstaple share of dietary energy"),
                                  includeHTML("www/nonStapleShareGraphText.html"),
                                  DT::dataTableOutput("nonStapleEnergyShareTable"),
                                  includeHTML("www/tableNote_nonstapleShare.html")))),
                     # Rao's QE tab panel ------
                     tabPanel(title = "Rao's quadratic entropy metric",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             selectizeInput(inputId = "RaosQECountryName", label = "Choose country", choices = countryNames),
                                             downloadButton("downloadData.RaoQE", "Download")),
                                mainPanel(
                                  titlePanel("Rao's quadratic entropy metric"),
                                  includeHTML("www/RaosQEGraphText.html"),
                                  DT::dataTableOutput("RaosQETable")))))),
          # glossary tab panel ------
          tabPanel(title = "Glossary",
                   mainPanel(
                     titlePanel("Glossary"),
                     includeHTML("www/glossaryText.html"))),
          # data review and download tab panel ------
          tabPanel(title = "Data",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                                  selectizeInput("dataset.full", "Choose a dataset:",
                                                 choices = datasetsToLoad.desc.complete),
                                  downloadButton('downloadData.full', 'Download'),
                                  includeHTML("www/downloadFullText.html")
                     ),
                     mainPanel(
                       titlePanel("Data download"),
                       DT::dataTableOutput('table')))),
          # data and developer information tabs with tabset -----
          tabPanel(
            "Developer Info",
            tabsetPanel(
              # Metadata tab panel -----
              tabPanel(
                title = "Files",
                mainPanel(
                  titlePanel("Information on file content for developers"),
                  fluidRow(column(width = 12, div(DT::dataTableOutput("metadataTable"), style = "font-size:80%", family = fontFamily))))),
              # IMPACT metadata tab panel -----
              tabPanel(title = "IMPACT metadata",
                       mainPanel(
                         titlePanel("IMPACT metadata"),
                         fluidRow(column(width = 12, div(DT::dataTableOutput("IMPACTmetadataTable"), style = "font-size:80%", family = fontFamily))))),
              # Foodgroup lookup panel -----
              tabPanel(title = "Food group lookup table",
                       mainPanel(
                         titlePanel("Food group lookup table"),
                         fluidRow(column(width = 12, div(DT::dataTableOutput("IMPACTfoodgroupTable"), style = "font-size:80%", family = fontFamily))))),
              # nutrient lookup -----
              tabPanel(title = "Nutrient lookup table",
                       mainPanel(
                         titlePanel("Nutrient lookup table"),
                         includeHTML("www/nutrientLookupText.html"),
                         downloadButton("downloadData.nutrients.adj", "Download"),
                         fluidRow(column(width = 12, div(DT::dataTableOutput("nutrientLookup"), style = "font-size:80%", family = fontFamily))))),
              # File documentation -----
              tabPanel(title = "File documentation",
                       mainPanel(
                         titlePanel("File documentation"),
                         fluidRow(column(width = 12, div(DT::dataTableOutput("fileDocumentation"), style = "font-size:80%", family = fontFamily))))))),
          # Acknowledgements -----
          tabPanel(title = "Acknowledgements",
                   mainPanel(
                     includeHTML("www/acknowledgementsText.html"))),
          tabPanel(title = "For further information",
                   mainPanel(
                     includeHTML("www/furtherInformation.html")))
        )
    )
  )
)

server <- function(input, output, session) {
  load_data(dataSetsToLoad) # load most of the data. Big files can be loaded elsewhere
  #  router(input, output) # for table of contents
  #not sure this is the best place for this. Need to get crud out of budget share. Just do once
  # update Jan 28, 2018. I think this this code was written, dt.budgetShare is now already in long form with name changes done
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
  dt.budgetShare.long <- unique(dt.budgetShare.long)
  formula.budgetShare <- paste("scenario + region_code.IMPACT159 + Variable  ~ year")
  dt.budgetShare.wide <- data.table::dcast(
    dt.budgetShare.long,
    formula = formula.budgetShare,
    value.var = "value")

  #' set up data download

  # foodAfford reactive -----
  data.afford <- reactive({
    countryName <- input$affordabilityCountryName
    dt <- data.table::copy(dt.budgetShare.wide)
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- dt[region_code.IMPACT159 %in% countryCode, ]
    setnames(dt, old = "region_code.IMPACT159", new = "Country code")
    dt[, scenario := factor(scenario, levels = scenarioNames)]
    #dt[, scenario := gsub("", "", scenario)]
    dt
  })

  # country name choice observer -----
  observe({
    countryName <- input$userCountryName
    # Can also set the label and select items
    updateSelectizeInput(session, "affordabilityCountryName", selected = countryName)
    updateSelectizeInput(session, "availabilityCountryName", selected = countryName)
    updateSelectizeInput(session, "adequacyCountryName", selected = countryName)
    updateSelectizeInput(session, "nutbalCountryName", selected = countryName)
    updateSelectizeInput(session, "AMDRCountryName", selected = countryName)
    updateSelectizeInput(session, "diversityCountryName", selected = countryName)
    updateSelectizeInput(session, "nonstapleEnergyShareCountryName", selected = countryName)
    updateSelectizeInput(session, "MRVCountryName", selected = countryName)
    updateSelectizeInput(session, "RaosQECountryName", selected = countryName)
    updateSelectizeInput(session, "FGcountryName", selected = countryName)

    # scenario choice observer -----
    scenarioName <- input$userScenarioName
    # Can also set the label and select items
    updateSelectizeInput(session, "availabilityScenarioName", selected = scenarioName)
    updateSelectizeInput(session, "adequacyScenarioName", selected = scenarioName)
    updateSelectizeInput(session, "FGscenarioName", selected = scenarioName)
  })

  # food availability reactive -----
  data.foodAvail <- reactive({
    countryName <- input$availabilityCountryName
    scenarioName <- input$availabilityScenarioName
    dt <- data.table::copy(dt.foodAvail_foodGroup.var)
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "food_group_code")
  })

  # food adequacy.macro reactive -----
  data.adequacy.macro <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- reqRatio_sum_RDA_macro.var
    dt <- data.table::copy(reqType)
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # food adequacy.vits reactive -----
  data.adequacy.vits <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- reqRatio_sum_RDA_vits.var
    dt <- data.table::copy(reqType)
    #  dt[, food_group_code := capwords(cleanupNutrientNames(nutrient))]
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # food adequacy.minrls reactive -----
  data.adequacy.minrls <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    reqType <- reqRatio_sum_RDA_minrls.var
    dt <- data.table::copy(reqType)
    spiderData <- spiderGraphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # energy.quantity reactive -----
  data.energy.quantity <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListRow <- c("kcalsPerDay_other", "kcalsPerDay_carbohydrate",
                     "kcalsPerDay_fat", "kcalsPerDay_protein")

    dt <- data.table::copy(dt.nutrients_kcals.var)
    dt <- dt[nutrient %in% keepListRow,]
    dt[, value := round(value, 1)][, nutrient := gsub("kcalsPerDay_", "", nutrient)]
    spiderData <- graphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # energy.share reactive -----
  data.energy.share <- reactive({
    countryName <- input$adequacyCountryName
    scenarioName <- input$adequacyScenarioName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    keepListRow <- c("kcalsPerDay_other_share", "kcalsPerDay_carbohydrate_share",
                     "kcalsPerDay_fat_share", "kcalsPerDay_protein_share")
    dt <- data.table::copy(dt.nutrients_kcals.var)
    dt <- dt[scenario %in% scenarioName & nutrient %in% keepListRow,]
    dt[, value := round(value, 1)][, nutrient := gsub("kcalsPerDay_", "", nutrient)][, nutrient := gsub("_share", "", nutrient)]
    spiderData <- graphData(countryName, scenarioName, dt, displayColumnName = "nutrient")
  })

  # nonStaple.share reactive -----
  data.nonStaple.share <- reactive({
    countryName <- input$nonstapleEnergyShareCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- dt.KcalShare_nonstaple.var[region_code.IMPACT159 == countryCode,]
    dt[, value := round(value, 2)]
    #dt[, scenario := gsub("", "", scenario)]
    dt[, year := gsub("X", "", year)]
    dt
  })

  # MRV reactive -----
  data.MRV <- reactive({
    countryName <- input$MRVCountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- data.table::copy(dt.MRVRatios.var)
    dt <- dt[region_code.IMPACT159 %in% countryCode ,]
#    setnames(dt, old = "region_code.IMPACT159", new = "Country code")
    dt[, value := round(value, 2)]
    dt[, year := gsub("X", "", year)]
    dt
  })

  # AMDR reactive -----
  data.AMDR <- reactive({
    countryName <- input$AMDRCountryName
    barData <- barGraphData(countryName, inData = food_agg_AMDR_hi.var)
    barData[, scenario := gsub("", "", scenario)][, year := gsub("X", "", year)]
  })

  # nutAvail reactive -----
  data.nutAvailFG <- reactive({
    countryName <- input$FGcountryName
    scenarioName <- input$FGscenarioName
    reqName <- input$nutrientGroup
    load_data_special("dt.nutrients_sum_FG.var") # checks to see if data already loaded. If not, load it
    if (reqName %in% "macronutrients") nutrientGroup <- c("carbohydrate_g", "protein_g",  "totalfiber_g", "fat_g")
    if (reqName %in% "minerals") nutrientGroup <- c("calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg")
    if (reqName %in% "vitamins") nutrientGroup <- c("folate_µg", "niacin_mg", "riboflavin_mg", "thiamin_mg", "vit_a_rae_µg", "vit_b6_mg",
                                                    "vit_b12_µg", "vit_c_mg", "vit_d_µg",  "vit_e_mg", "vit_k_µg")
    inData <- data.table::copy(dt.nutrients_sum_FG.var)
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
    dt <- data.table::copy(dt.nutBalScore.var)
    dt[, year := gsub("X", "", year)]
    dt <- dt[region_code.IMPACT159 == countryCode,]
    dt[, value := round(value, 2)]
    #dt[, scenario := gsub("", "", scenario)][, year := gsub("X", "", year)]
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
    dt <- data.table::copy(dt.shannonDiversity.var)
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
    #' reorder scenarios in temp to be the same order as scenarioNames
    dt.wide[, scenario := factor(scenario, levels = scenarioNames)]
    dt.wide[, scenario := gsub("", "", scenario)]
  })

  # RaosQE reactive -----
  data.RaosQE <- reactive({
    countryName <- input$RaosQECountryName
    countryCode <- countryCodeLookup(countryName, fileloc("mData"))
    dt <- data.table::copy(dt.RAOqe.var)
    dt <- dt[region_code.IMPACT159 == countryCode,]
    dt[, value := round(value, 2)]
    dt[, year := gsub("X", "", year)]
    formula.wide <- paste("scenario + region_code.IMPACT159 ~ year")
    spiderData <- data.table::dcast(
      data = dt,
      formula = formula.wide,
      value.var = "value")
  })

  # full data set download reactive -----
  datasetInput <- reactive({
    # look up number of location of input value in the datasets name list
    get(datasetsToLoad.complete[match(input$dataset.full, datasetsToLoad.desc.complete)])
  })

  # affordabilityTable -----
  output$affordabilityTable <- DT::renderDataTable({
    dt <- data.table::copy(data.afford())
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    #   dt[, region_code.IMPACT159 := NULL]
    #   data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    data.table::setnames(dt, old = codeNames.foodGroups, new = foodGroupNamesWrap)
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

  # availability table server side -----
  output$availabilityTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.foodAvail())
    dt <- pivotWideToWideYear(dt)
    nutrient <- "food group"
    data.table::setnames(dt, old = "nutrient_foodGroup", new = nutrient) # new depends on whether dt is for food groups or nutrients
    #dt[, scenario := gsub("", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    #dt[, scenario := gsub("", "", scenario)]
    #   data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    data.table::setnames(dt, old = codeNames.macro, new = nutNamesNoUnitsWrap.macro)
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

  output$adequacySpiderGraphP2 <- renderggiraph({
    dt <- data.table::copy(data.adequacy.vits())
    scenarioName <- unique(dt$scenario)
    #dt[, scenario := gsub("", "", scenario)]
    #   data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    data.table::setnames(dt, old = codeNames.vits, new = nutNamesNoUnitsWrap.vits)
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

  output$adequacySpiderGraphP3 <- renderggiraph({
    dt <- data.table::copy(data.adequacy.minrls())
    scenarioName <- unique(dt$scenario)
    #dt[, scenario := gsub("", "", scenario)]
    #   data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    data.table::setnames(dt, old = codeNames.minrls, new = nutNamesNoUnitsWrap.minrls)
    p <- spiderGraphOutput(dt, scenarioName)
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

  # adequacy tables server side -----
  output$adequacyTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.adequacy.macro())
    dt <- pivotWideToWideYear(dt)
    nutrient <- "nutrient"
    data.table::setnames(dt, old = "nutrient_foodGroup", new = nutrient) # new depends on whether dt is for food groups or nutrients
    #dt[, scenario := gsub("", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    #dt[, scenario := gsub("", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    #dt[, scenario := gsub("", "", scenario)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    countryName <- countryNameLookup(countryCode)
    #    countryName <- countryNameLookup((unique(dt$region_code.IMPACT159)))
    # colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    titleText <- paste("Share of total kilocalories by macronutrient\n", "Country: ", countryName, "Scenario: ", scenarioName)
    yLab <- "(percent)"
    p <- ggplot(dt, aes(x = year, y = value, tooltip = value, fill = nutrient, order = c("region_name") )) +
      geom_bar_interactive(stat = "identity") +
      theme(axis.title.y = element_text(family = fontFamily, face = "plain"))
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                             face = "plain")) + ggtitle(titleText)
    p <- p + theme(axis.text = element_text(size = 12, family = fontFamily, face = "plain"))
    p <- p + theme(legend.text = element_text(size = 12, family = fontFamily, face = "plain")) +
      labs(y = yLab, x = NULL)
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

  # energy quantity bar chart -----
  output$energyQuantityBarPlot <- renderggiraph({
    dt <- data.table::copy(data.energy.quantity())
    scenarioName <- unique(dt$scenario)
    countryCode <- unique(dt$region_code.IMPACT159)
    countryName <- countryNameLookup(countryCode)
    # colors_in <- c( "gray", "green", "blue", "red", "yellow" )
    titleText <- paste("Average daily availability of kilocalories\n", "Country:", countryName," Scenario:", scenarioName)
    yLab <- "(kcals)"
    p <- ggplot(dt, aes(x = year, y = value, tooltip = value, fill = nutrient, order = c("region_name") )) +
      geom_bar_interactive(stat = "identity") +
      theme(axis.title.y = element_text(family = fontFamily, face = "plain"))
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, family = fontFamily,
                                             face = "plain")) + ggtitle(titleText)
    p <- p + theme(axis.text = element_text(size = 12, family = fontFamily, face = "plain"))
    p <- p + theme(legend.text = element_text(size = 12, family = fontFamily, face = "plain")) +
      labs(y = yLab, x = NULL)
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

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
    #dt[, scenario := gsub("", "", scenario)]
    # formula.wide <- paste("scenario + region_code.IMPACT159 + ", nutrient, " ~ year")
    # dt <- data.table::dcast(data = dt, formula = formula.wide, value.var = "value")
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    #dt[, scenario := gsub("", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    #dt[, scenario := gsub("", "", scenario)]
    # formula.wide <- paste("scenario + region_code.IMPACT159 + ", nutrient, " ~ year")
    # dt <- data.table::dcast(data = dt, formula = formula.wide, value.var = "value")
    # colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    colsToRound <- names(dt)[!names(dt) %in% c("scenario", "region_code.IMPACT159", "year", nutrient)]
    dt[,(colsToRound) := round(.SD,1), .SDcols = colsToRound]
    data.table::setnames(dt, old = "region_code.IMPACT159", new = "country code")
    #dt[, scenario := gsub("", "", scenario)]
    data.table::setnames(dt, old = names(dt), new = capwords(names(dt)))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
    dt
  })

  output$downloadData.energyQ <- downloadHandler(
    filename = function() {paste("energyQ", input$adequacyCountryName, "_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.energy.quantity(), file)}
  )

  # adequacy AMDR graph server side ------
  output$AMDRbarGraphP1 <- renderggiraph({
    dt <- data.table::copy(data.AMDR())
    p <- plotByRegionBarAMDRinShiny(dt, yLab = "share of total dietary energy (percent)")
    ggiraph(code = print(p), zoom_max = 1, selection_type = "single")
  })

  # adequacy AMDR table server side ------
  output$AMDRTableP1 <- DT::renderDataTable({
    dt <- data.table::copy(data.AMDR())
    oldnames <- c("carbohydrate_g.kcalpercent", "fat_g.kcalpercent", "protein_g.kcalpercent")
    newnames <- c("carbohydrate", "protein", "fat")
    data.table::setnames(dt, old = oldnames, new = newnames)
    #    print(dt)
    dt <- pivotWideToWideYear(dt)
    colsToRound <- c("2010", "2030", "2050")
    dt[,(colsToRound) := round(.SD,2), .SDcols = colsToRound]

    oldnames <- c("region_code.IMPACT159", "nutrient_foodGroup")
    newnames <- c("Country Code", "Nutrient")
    data.table::setnames(dt, old = oldnames, new = newnames)
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
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
    #       print(head(dt))
    displayColumnName <- "food_group_code" # all these food groups are included in each bar chart
    facetColumnName <- "nutrient" # one spider graph per facetColumnName
    p <- facetGraphOutput(inData = dt, facetColumnName, displayColumnName, codeNames.foodGroups, foodGroupNamesNoWrap)
    p
  }, height = "auto")

  output$plot.NutAvailFGbarGraphP1 <- renderUI({
    if (input$nutrientGroup == "vitamins") plotHeight = "800px"
    if (input$nutrientGroup == "macronutrients") plotHeight = "500px"
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
    if (facetColumnName %in% "nutrient") dt[, nutrient := capwords(cleanupNutrientNames(nutrient))]
    data.table::setnames(dt, old = names(dt), new = capwords(names.new))
    # dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25, dom = 't', filter = list(position = "top")))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
    # dt
  })

  output$downloaddata.nutAvailFG <- downloadHandler(
    filename = function() {paste("nutDiv_", "_", input$FGcountryName, Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(data.nutAvailFG(), file)}
  )

  #view and download data ------
  output$table <- DT::renderDataTable({
    dt <- datasetInput()
    currentDT <- datasetsToLoad.complete[match(input$dataset.full, datasetsToLoad.desc.complete)]
    valueDTNames <- paste0(c( "dt.foodAvail_foodGroup", "dt.nutrients_kcals",
                              "reqRatio_sum_RDA_macro",  "reqRatio_sum_RDA_vits", "reqRatio_sum_RDA_minrls",
                              "dt.nutBalScore", "dt.MRVRatios", "dt.KcalShare_nonstaple", "dt.RAOqe"), ".var")
    setnames(dt, old = "region_code.IMPACT159", new = "Country code")
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
    dt <- getNewestVersion("dt.metadata", fileloc("mData"))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
  })

  # IMPACTmetadataTable ------
  output$IMPACTmetadataTable <- DT::renderDataTable({
    dt <- getNewestVersion("dt.IMPACTgdxParams", fileloc("mData"))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
  })

  # IMPACTfoodgroupLookupTable ------
  output$IMPACTfoodgroupTable <- DT::renderDataTable({
    data.table::setnames(dt.foodGroupsInfo, old = names(dt.foodGroupsInfo), new = gsub("_", " ", names(dt.foodGroupsInfo)))
    dt <- dt.foodGroupsInfo[,1:5]
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
    dt
  })

  # nutrient lookup -----
  output$nutrientLookup <- DT::renderDataTable({
    dt <- getNewestVersion("dt.nutrients.var", fileloc("mData"))
#    dt[, ft_acds_tot_trans_g := as.numeric(ft_acds_tot_trans_g)] #dt.nutrients.var comes in as chr because one entry is "ph (Aug 10, 2018); fixed Aug 12, 2018
    colsNotNumeric <- c("IMPACT_code",  "region_code.IMPACT159", "food_group_code", "staple_code")
    colsToRound3 <- names(dt)[!names(dt) %in% colsNotNumeric]
    # colsToRound3 <- c("phytate_mg", "protein_g", "fat_g", "carbohydrate_g", "totalfiber_g", "energy_kcal",
    #                  "calcium_mg", "iron_mg", "magnesium_mg", "phosphorus_mg", "potassium_g", "zinc_mg", "vit_c_mg", "thiamin_mg", "riboflavin_mg",
    #                  "niacin_mg", "vit_b6_mg", "folate_µg", "vit_b12_µg", "vit_a_rae_µg", "vit_e_mg", "vit_d_µg", "vit_k_µg", "sugar_g", "ft_acds_tot_sat_g",
    #                  "ft_acds_mono_unsat_g", "ft_acds_plyunst_g", "ft_acds_tot_trans_g", "ethanol_g", "caffeine_mg", "cholesterol_mg",
    #                  "kcals.fat_g", "kcals.protein_g", "kcals.carbohydrate_g", "kcals.sugar_g", "kcals.ft_acds_tot_sat_g", "kcals.ethanol_g")
    colsToRound0 <- c("magnesium_mg", "phosphorus_mg", "ethanol_g", "caffeine_mg", "cholesterol_mg",
                      "ethanol_g_cr", "calcium_mg_cr", "iron_mg_cr", "magnesium_mg_cr", "phosphorus_mg_cr",
                      "potassium_g_cr", "zinc_mg_cr", "vit_a_rae_µg_cr", "vit_c_mg_cr", "thiamin_mg_cr",
                      "riboflavin_mg_cr", "niacin_mg_cr", "vit_b6_mg_cr", "vit_b12_µg_cr", "folate_µg_cr",
                      "vit_e_mg_cr")
    dt[, (colsToRound3) := round(.SD, 3), .SDcols = colsToRound3]
    dt[, (colsToRound0) := round(.SD, 0), .SDcols = colsToRound0]
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
    dt
  })
  output$downloadData.nutrients.adj <- downloadHandler(
    filename = function() {paste("dt.nutrients.adj_", Sys.Date(), '.csv', sep = '') },
    content = function(file) {write.csv(dt.nutrients.adj, file)}
  )
  # file documentation -----
  output$fileDocumentation <- DT::renderDataTable({
    dt <- getNewestVersion("resultFileLookup", fileloc("mData"))
    dt <- DT::datatable(dt, rownames = FALSE, options = list(pageLength = 25))
    dt
  })
  session$onSessionEnded(stopApp)
}

# Run the application -----
shinyApp(ui = ui, server = server)


