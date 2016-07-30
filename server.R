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
    plot(data.sub.df$Period, data.sub.df$Data_value, ylab=data.units);

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
