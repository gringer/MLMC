#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

govdata.df <- read.csv("data/nalafs-jun2015-tables-csv.csv.gz", stringsAsFactors = FALSE);

excludeCouncilNames <- (c("Auckland Tourism, Events and Economic Development", "Museums", "Eliminations", "Total (excluding Museums)"));

govdata.df <- subset(govdata.df, !(Series_title_1 %in% excludeCouncilNames));

councilNames <- unique(govdata.df$Series_title_1);
dataCats <- unique(govdata.df$Series_title_2);

shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(href = "myScript.js")
  ),
  
  #includeCSS("styles.css"),
  #includeScript("myScript.js"),
                  
  #el <- div(HTML("I like <u>turtles</u>")),
  #cat(as.character(el)),
  
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
