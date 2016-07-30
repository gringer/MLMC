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
  
  # Application title
  titlePanel("My Life, My Council"),
    tabsetPanel(id="tabPanel",
                tabPanel(title="Select", value="select",
                  selectInput("council",label = "My Council",
                              choices=councilNames),
                  selectInput("cat",
                              label = "I'm Interested in", dataCats),
                  radioButtons("dataType", label = "I want to see",
                                     choices = c("Over Time","Last Year")),
                  actionButton("viewButton", label="View")
                ),
                tabPanel(title="View", value="view",
                         plotOutput("dataPlot"),
                         actionButton("backButton", label="Back")
                )
    )
));

library(shiny);

govdata.df <- read.csv("data/nalafs-jun2015-tables-csv.csv.gz", stringsAsFactors = FALSE);

excludeCouncilNames <- (c("Auckland Tourism, Events and Economic Development", "Museums", "Eliminations", "Total (excluding Museums)"));

govdata.df <- subset(govdata.df, !(Series_title_1 %in% excludeCouncilNames));

councilNames <- unique(govdata.df$Series_title_1);
dataCats <- unique(govdata.df$Series_title_2);

shinyServer(function(input, output, session) {
  
  output$dataPlot <- renderPlot({
    
    data.sub.df <- subset(govdata.df, (Series_title_1 == input$council) & (Series_title_2 == input$cat));
    data.units <- head(data.sub.df$UNITS,1);
    plot(data.sub.df$Period, data.sub.df$Data_value, horiz=TRUE, data=data.sub.df, ylab=data.units);

  });
  
  ## Observers to detect button changes and switch tabs
  observeEvent(input$viewButton,{
    cat("view\n");
    session$sendCustomMessage(type="setTab","view");
    });
  
  observeEvent(input$backButton,{
    cat("back\n");
    session$sendCustomMessage(type="setTab","select");
  });
  
  
  });


shinyApp(ui = ui, server = server);
