#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny);
library(shinyjs);

options(stringsAsFactors = FALSE);
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv");
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");

councilNames <- (type.df$Council);
dataCats <- (defs.df$Activity);

shinyUI(
  fluidPage(
    tags$head(
      tags$title("My Life, My Council"),
      tags$link(rel="stylesheet", type="text/css", href="bootstrap.min.css"),
      tags$link(rel="stylesheet", type="text/css", href="styles.css"),
      tags$script(src="myScript.js")
    ),
    useShinyjs(),  ## load shiny js library
    ## Application title
    fluidRow(
    tags$div(id="mainApp", fluidRow(column(1,tags$img(id="logo", src="mylifemycouncil-logo.png")))),
             tabsetPanel(id="tabPanel",
                ### Select panel ###
                tabPanel(title="Select", value="select", class="tabInterface",
                         tags$img(src="mylifemycouncil-bannerimage.jpg", id="banner"),
                         tags$h3("How does my council spend my money?"),
                         tags$p("We know that how your money is spent is important to you."),
                         tags$p("At My Life My Council we have brought together information from councils across the country to show you where your money is going."),
                         tags$h3("Find my council"),
                         tags$p("Select from the options below to find out how your council spends money on
                                what is important to you."),
                         fluidRow(
                           column(2,"My council is:"),
                           column(6,selectInput("council", label=NULL,
                                                choices=councilNames,
                                                selected="Wellington City Council"))
                         ),
                         fluidRow(
                           column(2,"I'm interested in:"),
                           column(6,selectInput("cat", label=NULL, choices=dataCats))
                         ),
                         fluidRow(
                           column(2,"I want to:"),
                           column(9,radioButtons("dataType", label=NULL,
                                                 choices=c("compare with other councils for 2015" = "Last Year",
                                                           "see my council's spending since 2010" = "Over Time")))
                         ),
                         fluidRow(
                           column(2,NULL),
                           column(9,actionButton("viewButton", label="See your council"))
                         ),
                         tags$br(),
                         tags$br(),
                         fluidRow(
                           column(9,tags$p("Description: Information used in this website is from the Local Authority
                                          Financial Statistics (LAFS) 30 June 2015 data set provided by Statistics New Zealand,
                                          and information on Council by Type and Council Contact Details held by the Department of Internal Affairs.
                                          All information is licensed for re-use under the Creative Commons Attribution 4.0 International licence.", id="smallerfont"))),
                         tags$br(),
                         tags$br(),
                         tags$div(id="bottomBorder", style="background-color: #23723F") 
                         
                         ),
                ### View panel ###
                ## fixed position panel
                tabPanel(title="View", value="view", class="tabInterface",
                         tags$h2("My Council"),
                         uiOutput("viewTopBlurb",inline=TRUE),
                         tags$h3("About this data"),
                         uiOutput("viewDataDesc",inline=TRUE),
                         ## less about height, more about position
                         fluidRow(column(3,actionButton("backButton", label="Back")),
                                  column(3,actionButton("emailButton", label="Email Council")),
                                  column(3,actionButton("pdfButton", label="Make PDF"))),
                         tags$br(),
                         conditionalPanel(condition="input.dataType=='Last Year'",
                                          fluidRow(
                                            column(3,"Sort by:"),
                                            column(6,selectInput("sortBy", label=NULL, 
                                                                 choices=c("Lowest to highest"="ascending",
                                                                           "Highest to lowest"="descending",
                                                                           "Alphabetical"="alpha"))))
                         ),
                         tags$div(class="plotGraph",
                                  conditionalPanel(condition="input.dataType=='Last Year'",
                                                   tags$div(style="height: 400px; width: 95%; overflow: auto;",plotOutput("comparisonPlot", width="90%", height="1500px"))),
                                  conditionalPanel(condition="input.dataType=='Over Time'",
                                                   plotOutput("yearPlot", width="90%"))
                         ))
    ),
    tags$p(id="footer", "Powered by RStudio Shiny and Catalyst Cloud"))
  ));
  
