## MLMC statistics
## - Backend server -

library(shiny);
library(tidyr);
library(digest);

logOutput <- function(input, requestID){
  if(!file.exists("../../logs")){
    return();
  }
  ## Don't destroy the App just because logging fails
  tryCatch({
    timeStr <- as.character(Sys.time());
    if(!file.exists("../../logs/usageusagelog.csv")){
      ## add file header (append=TRUE for the rare case of race conditions)
      cat("requestID,time,inputCategory,value\n",
          file = "../../logs/usageusagelog.csv", append=TRUE);
    }
    # for(n in names(input)){
    #   if(is.character(input[[n]]) || (is.numeric(input[[n]]) && (length(input[[n]]) == 1))){
    #     cat(file = "../../logs/usageusagelog.csv",
    #         append=TRUE, sprintf("%s,%s,\"%s\",\"%s\"\n",
    #                              requestID, timeStr, n,
    #                              substring(paste(input[[n]], collapse=";"),1,100)));
    #   }
    # }
    cat(file = "../../logs/usageusagelog.csv",
        append=TRUE, sprintf("%s,%s,\"%s\",\"%s\"\n",
                             requestID, timeStr, "Access",1));
  }, error=function(cond){
    cat("Error:\n");
    #message(cond);
  }, warning=function(cond){
    cat("Warning:\n");
    #message(cond);
  });
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data.df <- read.csv("../../logs/accesslog.csv", stringsAsFactors=FALSE);
  data.df$time <- as.POSIXct(data.df$time);
  usage.df <- read.csv("../../logs/usageusagelog.csv", stringsAsFactors=FALSE);
  usage.df$time <- as.POSIXct(usage.df$time);
  data.wide.df <- spread(data = data.df, key = "inputCategory", value="value");
  data.wide.df$hours <- round(data.wide.df$time, "hours");
  data.wide.df$viewButton <- as.numeric(data.wide.df$viewButton);

  output$timePlot <- renderPlot({
    htable <- table(as.character(data.wide.df$hours));
    hutable <- table(as.character(round(usage.df$time,"hours")));
    plot(x=as.POSIXct(names(htable)), y=htable, xlab="Time",
         ylab = "Number of visits", yaxt="n", type="b", pch=16, col="blue");
    points(x=as.POSIXct(names(hutable)), y=hutable, type="b",
           pch=17, col="darkgreen");
    legend("topright",legend=c("MLMC App","Usage App"), pch=c(16,17), 
           col=c("blue", "darkgreen"), inset=0.05, bg="#FFFFFFC0");
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

  ## record the data request in a log file
  requestID <- substr(digest(Sys.time()),1,8);
  logOutput(input, requestID = requestID);
    
})
