#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

govdata.df <- read.csv("/bioinf/govhack/data/govdata/nalafs-jun2015/nalafs-jun2015-tables-csv.csv.gz", stringsAsFactors = FALSE);
councilNames <- unique(govdata.df$Series_title_1);
dataCats <- unique(govdata.df$Series_title_2);

ui <- fluidPage(
  
  # Application title
  titlePanel("My Life, My Council"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("council",label = "My Council",
                         choices=councilNames),
      tag("h3","This is a test"),
      selectInput("cat",
                  label = "I'm Interested in", dataCats)
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("dataPlot")
    )
  )
);

server <- shinyServer(function(input, output) {
  
  output$dataPlot <- renderPlot({
    
    data.sub.df <- subset(govdata.df, (Series_title_1 == input$council) & (Series_title_2 == input$cat));
    data.units <- head(data.sub.df$UNITS,1);
    plot(Data_value ~ Period, data=data.sub.df, ylab=data.units);

  });
  
});


shinyApp(ui = ui, server = server);
