#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

options(stringsAsFactors = FALSE);
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv");
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");

councilNames <- (type.df$Council);
dataCats <- (defs.df$Activity);

shinyUI(fluidPage(
  
  # Application title
  titlePanel("My Life, My Council"),
    tabsetPanel(id="tabPanel",
                ### Select panel ###
                tabPanel(title="Select", value="select",
                         selectInput("council",label = "My Council",
                                     choices=councilNames, selected = "Wellington City Council"),
                         selectInput("cat",
                                     label = "I'm Interested in", dataCats),
                         radioButtons("dataType", label = "I want to see",
                                      choices = c("Over Time","Last Year")),
                         actionButton("viewButton", label="View")),
                ### View panel ###
                tabPanel(title="View", value="view",
                         plotOutput("dataPlot"),
                         actionButton("backButton", label="Back"))
    )
));
