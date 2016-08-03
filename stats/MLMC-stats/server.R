## MLMC statistics
## - Backend server -

library(shiny);
library(tidyr);

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data.df <- read.csv("../../logs/accesslog.csv");
  data.df$time <- as.POSIXct(data.df$time);
  data.wide.df <- spread(data = data.df, key = "inputCategory", value="value");

  output$distPlot <- renderPlot({
    plot(tapply(data.wide.df$time, round(data.wide.df$time, "hours"),length));
  })
  
})
