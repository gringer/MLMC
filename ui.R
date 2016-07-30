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
      tags$link(rel="stylesheet", type="text/css", href="bootstrap.min.css"),
      tags$link(rel="stylesheet", type="text/css", href="styles.css"),
      tags$script(src="myScript.js")
    ),
    useShinyjs(),  ## load shiny js library
    ## Application title
    fluidRow(
      column(1,tags$img(src="mylifemycouncil-logo.png"))
    ),
    tabsetPanel(id="tabPanel",
                ### Select panel ###
                tabPanel(title="Select", value="select",
                         tags$img(src="mylifemycouncil-bannerimage.png", id="banner"),
                         tags$p("We know that how your money is spent is very important to know but it
                                can be very difficult to find out if your council is spending your money
                                on your priorities."),
                         tags$p("At My Life My Council we have brought together data from councils across
                                the country to make it easy."),
                         tags$h3("Find my council"),
                         tags$p("Select from the options to find out how your council spends money on
                                what is important to you."),
                         fluidRow(
                           column(3,"My council is:"),
                           column(6,selectInput("council", label=NULL,
                                                choices=councilNames,
                                                selected="Wellington City Council"))
                         ),
                         fluidRow(
                           column(3,"I'm interested in:"),
                           column(6,selectInput("cat", label=NULL, choices=dataCats))
                         ),
                         fluidRow(
                           column(3,"I want to:"),
                           column(9,radioButtons("dataType", label=NULL,
                                                 choices=c("compare with other councils" = "Last Year",
                                                           "see my council's spending since 2010" = "Over Time")))
                         ),
                         fluidRow(
                           column(3,NULL),
                           column(9,actionButton("viewButton", label="See your council"))
                         )
                ),
                ### View panel ###
                ## fixed position panel
                tabPanel(title="View", value="view",
                         tags$div(
                           tags$h2("My Council"),
                           uiOutput("viewTopBlurb",inline=TRUE),
                           tags$h3("About this data"),
                           uiOutput("viewDataDesc",inline=TRUE),
                           fluidRow(
                             column(3,"Sort by:"),
                             column(6,selectInput("sortBy", label=NULL, choices=c("ascending","descending")))
                           )
                         ),
                         ## less about height, more about position
                         actionButton("backButton", label="Back"),
                         tags$div(class="plotGraph", height="400px",
                                  plotOutput("dataPlot", height="1500px"))
                         )
    )
  ));
