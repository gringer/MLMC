library(shiny);

options(stringsAsFactors = FALSE);
core.df <- read.csv("data/Local_Authority_Financial_Statistics_Year_ended_June_2015_EXPENDITURE_TABLE.csv");
## rename last column to something a bit easier to write in code
colnames(core.df)[4] <- "opex.1000";
caveats.df <- read.csv("data/Local_Authority_Financial_Statistics_Caveats.csv");
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv");
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");

total.exp <- xtabs(opex.1000 ~ Council + Year, data=core.df);

lastYear <- 2015;

councilNames <- unique(govdata.df$Series_title_1);
dataCats <- unique(govdata.df$Series_title_2);

shinyServer(function(input, output, session) {
  
  output$dataPlot <- renderPlot({
    if(input$dataType == "Over Time"){
      data.sub.df <- subset(core.df, (Council == input$council) & (Activity == input$cat));
      data.sub.df$pct.exp <- data.sub.df$opex.1000 /
        total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))];
      plot(data.sub.df$Year, data.sub.df$pct.exp, ylab="Percent Expenditure",
           xlab="Year", type="l");
    }
    if(input$dataType == "Last Year"){
      data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat));
      data.sub.df$pct.exp <- data.sub.df$opex.1000 /
        total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))];
      data.sub.df$order <- order(data.sub.df$pct.exp);
      data.sub.df$col <- ifelse(data.sub.df$Council == input$council,"darkGreen","grey");
      barplot(data.sub.df$pct.exp[data.sub.df$order], horiz = TRUE, las=2,
              col = data.sub.df$col[data.sub.df$order],
              xlab="Percent Expenditure");
    }
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
