## MLMC statistics
## - Backend server -

library(shiny);
library(tidyr);

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data.df <- read.csv("../../logs/accesslog.csv");
  data.df$time <- as.POSIXct(data.df$time);
  data.wide.df <- spread(data = data.df, key = "inputCategory", value="value");
  data.wide.df$hours <- round(data.wide.df$time, "hours");

  output$distPlot <- renderPlot({
    htable <- table(as.character(round(data.wide.df$hours,"hours")));
    plot(x=as.POSIXct(names(htable)), y=htable, ylab = "number of uses");
  })
  
})
