library(shiny);
library(digest);

options(stringsAsFactors = FALSE);
core.df <- read.csv("data/Local_Authority_Financial_Statistics_Year_ended_June_2015_EXPENDITURE_TABLE.csv");
## rename last column to something a bit easier to write in code
colnames(core.df)[4] <- "opex.1000";
caveats.df <- read.csv("data/Local_Authority_Financial_Statistics_Caveats.csv");
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv", row.names=1);
core.df$Council.Type <- type.df[core.df$Council,"Council.Type"];
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");
rownames(defs.df) <- defs.df$Activity;
contacts.df <- read.csv("data/NZ_Councils_Contact_List.csv");
rownames(contacts.df) <- contacts.df$Council;

values <- reactiveValues();
values$barPlotHeight <- 400;

typeFull <- c("Last Year" = "compare with other councils.",
              "Over Time" = "see my council's spending since 2010.");

total.exp <- xtabs(opex.1000 ~ Council + Year, data=core.df);

lastYear <- max(core.df$Year);

councilNames <- type.df$Council;
dataCats <- defs.df$Activity;

logOutput <- function(input, requestID){
  if(!file.exists("/home/govhack/ShinyLog")){
    return();
  }
  return(); ## still not working...
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
    list("My council is ",tags$b(input$council),
         ", I'm interested in ", tags$b(input$cat),
         "and I want to ", tags$b(typeFull[input$dataType]));
  });
  
  output$plotHeading <- renderUI({
    councilType <- type.df[input$council,"Council.Type"];
    fluidRow(
      column(3,NULL),
      column(9,tags$h3(sprintf("Percent Expenditure (vs other %s authorities)", 
                               tolower(councilType)), style="margin-bottom:-25px")));
  });

  output$contactDetails <- renderUI({
    fluidPage(
      fluidRow(column(2,tags$b("Authority:")), column(5,input$council)),
      fluidRow(column(2,tags$b("Phone:")), column(5,contacts.df[input$council,"Phone"])),
      fluidRow(column(2,tags$b("Address:")), column(5,contacts.df[input$council,"Post"])),
      fluidRow(column(2,tags$b("Email:")), 
               column(5,tags$a(href=sprintf("mailto:%s",contacts.df[input$council,"Email"]),
                               contacts.df[input$council,"Email"]))),
      fluidRow(column(2,tags$b("Website:")), 
               column(5,tags$a(href=sprintf("http://%s",contacts.df[input$council,"URL"]),
                               contacts.df[input$council,"URL"])))
    );
  });

  output$viewDataDesc <- renderUI({
    list(tags$p(input$council, " is a ", tags$b(type.df[input$council,"Council.Type"]),
         "Authority."),
         tags$p(tags$b(input$cat)," ",
         defs.df[input$cat,"Basic.definition."],
         tags$p("See ",tags$a(href="http://www.localcouncils.govt.nz/lgip.nsf/wpg_url/About-Local-Government-Index","Internal Affairs"),
                " and ",tags$a(href="http://datainfoplus.stats.govt.nz/Item/nz.govt.stats/319be03d-287d-4384-993a-d38ef9acb3e6#/nz.govt.stats/6b459c66-0e05-4aa7-997e-d4a5e40941dd","Statistics New Zealand"),
                " for more information.")));
  });
  
  output$yearPlot <- renderPlot({
    data.sub.df <- subset(core.df, (Council == input$council) & (Activity == input$cat));
    data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
                                   total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
    plot(data.sub.df$Year, data.sub.df$pct.exp,
         ylab=sprintf("Percent Expenditure"),
         xlab="Year", type="b", lwd=2, col="darkgreen");
    if(input$tabPanel == "view"){
      ## record the data request in a log file
      logOutput(input, requestID = requestID);
    }
  });
  
  output$plotScaleBar <- renderPlot({
    councilType <- type.df[input$council,"Council.Type"];
    data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat) & 
                            Council.Type == councilType);
    data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
                                   total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
    dataMax <- max(data.sub.df$pct.exp);
    par(mar=c(0,20,4,2));
    res <- barplot(NA, horiz=TRUE, las=2, xlim=c(0,dataMax*1.1), yaxs="i", xaxt="n");
    axis(3);
  });
  
  output$dynamicCompPlot <- 
    renderUI(tags$div(style=sprintf("height: %dpx; width: 100%%; overflow: auto;",
                                    min(values$barPlotHeight,250)),
                      plotOutput("comparisonPlot", width="100%", 
                                 height=sprintf("%dpx",values$barPlotHeight))));
  
  output$comparisonPlot <- renderPlot({
    councilType <- type.df[input$council,"Council.Type"];
    typeCount <- sum(type.df$Council.Type == councilType);
    data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat) & 
                            Council.Type == councilType);
    data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
                                   total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
    council.value <- subset(data.sub.df, Council == input$council)$pct.exp;
    data.sub.df$order <- 
      if(input$sortBy == "descending") {
        order(data.sub.df$pct.exp);
      } else if(input$sortBy == "ascending"){
        order(-data.sub.df$pct.exp);
      } else if(input$sortBy == "alpha"){
        order(-xtfrm(data.sub.df$Council));
      }
    data.sub.df <- data.sub.df[data.sub.df$order,];
    data.sub.df$col <- ifelse(c(data.sub.df$Council == input$council),"#23723F","#90DDAB");
    par(mar=c(0,20,0,2));
    dataMax <- max(data.sub.df$pct.exp);
    res <- barplot(c(data.sub.df$pct.exp,0,council.value),
                   names.arg=c(data.sub.df$Council,"",input$council), 
                   horiz=TRUE, las=2, yaxs="i",
                   col=c(data.sub.df$col,NA,"#23723F"),
                   border=NA, xaxt="n");
    abline(h=head(tail(res,2),1),lty="dotted", lwd=3);
    text(x=c(data.sub.df$pct.exp,NA,council.value), y=res, pos=4, 
         labels=c(data.sub.df$pct.exp,NA,council.value), cex=0.8,
         col=c(data.sub.df$col,NA,"#23723F"), xpd=TRUE);
    mtext(sprintf("Percent Expenditure (vs other %s authorities)", tolower(councilType)),
          3, line = 3, cex=2);
    if(input$tabPanel == "view"){
      ## record the data request in a log file
      logOutput(input, requestID = requestID);
    }
  });
  
  output$makePDF <- downloadHandler(
    filename = function(){
      gsub(" ","_",sprintf("%s_%s_%s_%s.pdf",input$council,input$cat,
              input$dataType,format(Sys.Date(),"%Y-%b-%d")));
    },
    content = function(con){
      pdf(con, paper="a4r", width=11, height=8);
      councilType <- type.df[input$council,"Council.Type"];
      typeCount <- sum(type.df$Council.Type == councilType);
      ## bar plot -- expenses compared to other councils
      data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat) & 
                              Council.Type == councilType);
      data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
                                     total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
      council.value <- subset(data.sub.df, Council == input$council)$pct.exp;
      data.sub.df$order <- order(data.sub.df$pct.exp);
      data.sub.df <- data.sub.df[data.sub.df$order,];
      data.sub.df$col <- ifelse(c(data.sub.df$Council == input$council),"#23723F","#90DDAB");
      par(mar=c(0,10,5,2));
      dataMax <- max(data.sub.df$pct.exp);
      res <- barplot(c(data.sub.df$pct.exp,0,council.value),
                     names.arg=c(data.sub.df$Council,"",input$council), 
                     horiz=TRUE, las=2, cex.names=0.5,
                     col=c(data.sub.df$col,NA,"#23723F"),
                     border=NA, xaxt="n", main=input$cat);
      text(x=c(data.sub.df$pct.exp,NA,council.value), y=res, pos=4, 
           labels=c(data.sub.df$pct.exp,NA,council.value), cex=0.5,
           col=c(data.sub.df$col,NA,"#23723F"), xpd=TRUE);
      mtext("Percent Expenditure",3,1);
      mtext(sprintf("(vs other %s authorities)", tolower(councilType)),3,0.5, cex=0.5);
      abline(h=head(tail(res,2),1),lty="dotted", lwd=3);
      ## line graph -- expenses over time
      data.sub.df <- subset(core.df, (Council == input$council) & (Activity == input$cat));
      data.sub.df$pct.exp <- round(data.sub.df$opex.1000 /
                                     total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,1);
      par(mar=c(5, 4, 4, 2) + 0.1); # reset mar to default
      plot(data.sub.df$Year, data.sub.df$pct.exp,
           ylab=sprintf("Percent Expenditure"), main=input$cat,
           xlab="Year", type="b", lwd=2, col="darkgreen");
      mtext(input$council, 3, 0.5, cex=0.75);
      dummy <- dev.off();
    },
    contentType = "text/pdf"
  );

  ## Observers to detect button changes and switch tabs
  ## view button
  observeEvent(input$viewButton,{
    updateTabsetPanel(session, "tabPanel", selected = "view");
    });
  
  ## select / back button and logo
  observeEvent(input$backButton+input$logoLink,{
    updateTabsetPanel(session, "tabPanel", selected = "select");
  });
  
  ## Change bar plot height on council type change
  observeEvent(input$council,{
    values$typeCount <- sum(type.df$Council.Type == type.df[input$council,"Council.Type"]);
    values$barPlotHeight <- values$typeCount * 25;
    });
  
  });
