library(shiny);
library(digest);

options(stringsAsFactors = FALSE);
core.df <- read.csv("data/Local_Authority_Financial_Statistics_Year_ended_June_2015_EXPENDITURE_TABLE.csv");
## rename last column to something a bit easier to write in code
colnames(core.df)[4] <- "opex.1000";
caveats.df <- read.csv("data/Local_Authority_Financial_Statistics_Caveats.csv");
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv", row.names=1);
core.df$Council.Type <- type.df$Council.Type[core.df$Council];
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");
rownames(defs.df) <- defs.df$Activity;

typeFull <- c("Last Year" = "compare with other councils.",
              "Over Time" = "see my council's spending since 2010.");

total.exp <- xtabs(opex.1000 ~ Council + Year, data=core.df);

lastYear <- max(core.df$Year);

councilNames <- type.df$Council;
dataCats <- defs.df$Activity;

logOutput <- function(input, requestID){
  timeStr <- as.character(Sys.time());
  if(!file.exists("logs/accesslog.csv")){
    ## add file header (append=TRUE for the rare case of race conditions)
    cat("requestID,time,inputCategory,value\n", file = "accesslog.csv", append=TRUE);
  }
  for(n in names(input)){
    if(is.character(input[[n]]) || (is.numeric(input[[n]]) && (length(input[[n]]) == 1))){
      cat(file = "logs/accesslog.csv",
          append=TRUE, sprintf("%s,%s,\"%s\",\"%s\"\n",
                               requestID, timeStr, n,
                               substring(paste(input[[n]], collapse=";"),1,100)));
    }
  }
}

shinyServer(function(input, output, session) {
  
  requestID <- substr(digest(Sys.time()),1,8);
  
  output$viewTopBlurb <- renderUI({
    print(input$dataType);
    list("My council is ",tags$b(input$council),
         ", I'm interested in ", tags$b(input$cat),
         "and I want to ", tags$b(typeFull[input$dataType]));
  });
  
  output$viewDataDesc <- renderUI({
    list(defs.df[input$cat,"Definitions"]);
  });
  
  output$dataPlot <- renderPlot({
    if(input$dataType == "Over Time"){
      data.sub.df <- subset(core.df, (Council == input$council) & (Activity == input$cat));
      #data.sub.df <- subset(core.df, (Council == input$council) & (Activity == input$cat) & 
      #                        (Council.Type != type.df[input$council,"Council.Type"]));
      data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
        total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
      plot(data.sub.df$Year, data.sub.df$pct.exp, ylab="Percent Expenditure",
           xlab="Year", type="b", lwd=2, col="darkgreen");
    }
    if(input$dataType == "Last Year"){
      data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat));
      #data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat) & 
      #                        Council.Type == type.df[input$council,"Council.Type"]);
      data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
        total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
      data.sub.df$order <- order(data.sub.df$pct.exp);
      data.sub.df <- data.sub.df[data.sub.df$order,];
      data.sub.df$col <- ifelse(data.sub.df$Council == input$council,"darkGreen","grey");
      par(mar=c(5,1,1,1));
      res <- barplot(data.sub.df$pct.exp, horiz = TRUE, las=2,
              col = data.sub.df$col,
              xlab="Percent Expenditure");
      text(x=0,y=res,pos=4,labels = data.sub.df$Council, cex=0.8);
    }
    if(input$tabPanel == "view"){
      ## record the data request in a log file
      ## [disabled for now until I can work out how to log as www-user]
      #logOutput(input, requestID = requestID);
    }
  });
  
  ## Observers to detect button changes and switch tabs
  observeEvent(input$viewButton,{
    updateTabsetPanel(session, "tabPanel", selected = "view");
    });
  
  observeEvent(input$backButton,{
    updateTabsetPanel(session, "tabPanel", selected = "select");
  });
  
  });
