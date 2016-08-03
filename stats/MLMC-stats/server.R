## MLMC statistics
## - Backend server -

library(shiny);
library(tidyr);

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data.df <- read.csv("../../logs/accesslog.csv", stringsAsFactors=FALSE);
  data.df$time <- as.POSIXct(data.df$time);
  data.wide.df <- spread(data = data.df, key = "inputCategory", value="value");
  data.wide.df$hours <- round(data.wide.df$time, "hours");
  data.wide.df$viewButton <- as.numeric(data.wide.df$viewButton);

  output$timePlot <- renderPlot({
    htable <- table(as.character(round(data.wide.df$hours,"hours")));
    plot(x=as.POSIXct(names(htable)), y=htable, xlab="Time",
         ylab = "Number of uses", yaxt="n", type="b");
    axis(2);
  });

  output$catPlot <- renderPlot({
    par(mar=c(5,10,1,1));
    barplot(sort(table(data.wide.df$cat)), xlab="Graphs Viewed",
            cex.names=0.75, horiz = TRUE, las=1);
  });

  output$councilPlot <- renderPlot({
    par(mar=c(5,10,1,1));
    barplot(sort(table(data.wide.df$council)), xlab="Graphs Viewed",
            cex.names=0.75, horiz = TRUE, las=1);
  })
  
  output$dwellPlot <- renderPlot({
    dwellCount <- table(tapply(data.wide.df$viewButton, data.wide.df$requestID,
                         max));
    barplot(dwellCount, xlab = "Graphs viewed per visit",
            ylab = "Number of visits");
  })
  
})
