library(shiny);
library(digest);
library(rmarkdown);
library(leaflet);

options(stringsAsFactors = FALSE);
core.df <- read.csv("data/Local_Authority_Financial_Statistics_Year_ended_June_2015_EXPENDITURE_TABLE.csv");
## rename last column to something a bit easier to write in code
colnames(core.df)[4] <- "opex.1000";
caveats.df <- read.csv("data/Local_Authority_Financial_Statistics_Caveats.csv");
rownames(caveats.df) <- caveats.df$Council;
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv");
rownames(type.df) <- type.df$Council;
core.df$Council.Type <- type.df[core.df$Council,"Council.Type"];
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");
rownames(defs.df) <- defs.df$Activity;
contacts.df <- read.csv("data/NZ_Councils_Contact_List.csv");
rownames(contacts.df) <- contacts.df$Council;

values <- reactiveValues();
values$barPlotHeight <- 400;

## Retrieve region IDs from Stats NZ WFS service
## [see https://wiki.state.ma.us/confluence/display/massgis/GeoServer+-+WFS+-+Examples]

authority.regions.df <-
  read.csv(paste(sep="&","https://datafinder.stats.govt.nz/services;key=0b76d98b83fa4e0dab5c295f760826b9/wfs?service=WFS",
             "request=getFeature",
             "outputFormat=csv",
             "typeName=layer-8409",
             "propertyname=TA2016_NAME,LAND_AREA_SQ_KM"), stringsAsFactors = FALSE);

typeFull <- c("Last Year" = "compare with other councils.",
              "Over Time" = "see my council's spending since 2010.");

total.exp <- xtabs(opex.1000 ~ Council + Year, data=core.df);

lastYear <- max(core.df$Year);

councilNames <- type.df$Council;
dataCats <- defs.df$Activity;

logOutput <- function(input, requestID){
  if(!file.exists("logs")){
    return();
  }
  ## Don't destroy the App just because logging fails
  tryCatch({
    timeStr <- as.character(Sys.time());
    if(!file.exists("logs/accesslog.csv")){
      ## add file header (append=TRUE for the rare case of race conditions)
      cat("requestID,time,inputCategory,value\n",
          file = "logs/accesslog.csv", append=TRUE);
    }
    for(n in names(input)){
      if(is.character(input[[n]]) || (is.numeric(input[[n]]) && (length(input[[n]]) == 1))){
        cat(file = "logs/accesslog.csv",
            append=TRUE, sprintf("%s,%s,\"%s\",\"%s\"\n",
                                 requestID, timeStr, n,
                                 substring(paste(input[[n]], collapse=";"),1,100)));
      }
    }
  }, error=function(cond){
    cat("Error:\n");
    message(cond);
  }, warning=function(cond){
    cat("Warning:\n");
    message(cond);
  });
}

basicPDF <- function(input, con){
  pdf(con, paper="a4r", width=11, height=8);
  councilType <- type.df[input$council,"Council.Type"];
  typeCount <- sum(type.df$Council.Type == councilType);
  ## bar plot -- expenses compared to other councils
  data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat) & 
                          Council.Type == councilType);
  data.sub.df$pct.exp <- signif(data.sub.df$opex.1000 /
                                 total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,2);
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
  data.sub.df$pct.exp <- signif(data.sub.df$opex.1000 /
                                 total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,2);
  par(mar=c(5, 4, 4, 2) + 0.1); # reset mar to default
  plot(data.sub.df$Year, data.sub.df$pct.exp,
       ylab=sprintf("Percent Expenditure"), main=input$cat,
       xlab="Year", type="b", lwd=2, col="darkgreen");
  mtext(input$council, 3, 0.5, cex=0.75);
  dummy <- dev.off();
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

  output$dataCaveats <- renderUI({
    if(input$council %in% rownames(caveats.df)){
      fluidRow(column(9,tags$b("Note:"), caveats.df[input$council,"Caveats"]));
    }
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
    data.sub.df$pct.exp <- signif(data.sub.df$opex.1000 /
                                   total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,2);
    dataMax <- max(data.sub.df$pct.exp);
    plot(data.sub.df$Year, data.sub.df$pct.exp, ylim=c(0,dataMax),
         ylab=sprintf("Percent Expenditure"),
         xlab="Year", type="b", lwd=2, col="darkgreen");
  });
  
  output$plotScaleBar <- renderPlot({
    councilType <- type.df[input$council,"Council.Type"];
    data.sub.df <- subset(core.df, (Year == lastYear) & (Activity == input$cat) & 
                            Council.Type == councilType);
    data.sub.df$pct.exp <- signif(data.sub.df$opex.1000 /
                                   total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,2);
    dataMax <- max(data.sub.df$pct.exp);
    if(dev.size("px")[1] > 600){
      par(mar = c(0,20,4,2));
    } else {
      par(mar = c(0,8,4,2));
    }
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
    data.sub.df$pct.exp <- signif(data.sub.df$opex.1000 /
                                   total.exp[cbind(data.sub.df$Council,as.character(data.sub.df$Year))] * 100,2);
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
    councilLabels <- c(data.sub.df$Council,"",input$council);
    if(dev.size("px")[1] > 600){
      par(mar = c(0,20,0,2));
    } else {
      par(mar = c(0,8,0,2), cex.axis=0.6);
      councilLabels <- sub(" District$","", sub(" Council$","",councilLabels));
    }
    dataMax <- max(data.sub.df$pct.exp);
    res <- barplot(c(data.sub.df$pct.exp,0,council.value),
                   names.arg=councilLabels,
                   horiz=TRUE, las=2, yaxs="i",
                   col=c(data.sub.df$col,NA,"#23723F"),
                   border=NA, xaxt="n");
    abline(h=head(tail(res,2),1),lty="dotted", lwd=3);
    text(x=c(data.sub.df$pct.exp,NA,council.value), y=res, pos=4, 
         labels=c(data.sub.df$pct.exp,NA,council.value), cex=0.8,
         col=c(data.sub.df$col,NA,"#23723F"), xpd=TRUE);
    mtext(sprintf("Percent Expenditure (vs other %s authorities)", tolower(councilType)),
          3, line = 3, cex=2);
  });
  
  output$makePDF <- downloadHandler(
    filename = function(){
      gsub(" ","_",sprintf("%s_%s_%s_%s.pdf",input$council,input$cat,
              input$dataType,format(Sys.Date(),"%Y-%b-%d")));
    },
    content = function(con){
      #basicPDF(input, con);
      cat("Making PDF\n");
      isolate({rmarkdown::render("MLMC.Rmd", "pdf_document", output_file = con,
                        intermediates_dir = "tmp")});
    },
    contentType = "text/pdf"
  );
  
  output$nzMap <- renderLeaflet({
    values$baseMap <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      fitBounds(lng1=165.973643765, lng2=175.37904683,
                lat1=-47.724045517, lat2=-33.9584981); # set to range of Territorial Authorities
  });
  
  ## Populate address finder
  

  ## Observers to detect button changes and switch tabs
  ## view button
  observeEvent(input$viewButton,{
    updateTabsetPanel(session, "tabPanel", selected = "view");
    ## record the data request in a log file
    logOutput(input, requestID = requestID);
  });
  
  ## select / back button and logo
  observeEvent(input$backButton+input$logoLink,{
    updateTabsetPanel(session, "tabPanel", selected = "select");
  });
  
  ## Change bar plot height on council type change
  ## Change displayed region on council type change
  observeEvent(input$council,{
    if(!(input$council %in% councilNames)){
      if(nchar(input$council) > 5){
        searchName <- isolate(input$council);
        cat(sprintf("Searching for '%s'...", searchName));
        ## find address latitude/longitude
        req <- paste(sep="&","http://data.linz.govt.nz/services;key=cbcae793850342e8be9908602a1e60a8/wfs?service=WFS",
                     "request=getFeature",
                     "outputFormat=csv",
                     "typeName=layer-3353",
                     "count=5",
                     URLencode(sprintf("CQL_FILTER=full_address ILIKE '%s%%'", searchName)),
                     "propertyname=full_address,gd2000_xcoord,gd2000_ycoord");
        addr.df <- read.csv(req, stringsAsFactors = FALSE);
        cat(" done\n");
        if(nrow(addr.df) == 1){
          leafletProxy("nzMap") %>% clearMarkers() %>%
            addMarkers(addr.df$gd2000_xcoord,addr.df$gd2000_ycoord,popup=addr.df$full_address) %>%
            setView(addr.df$gd2000_xcoord, addr.df$gd2000_ycoord, zoom=17);
          ## identify intersecting territorial authority
          req <- paste(sep="&","https://datafinder.stats.govt.nz/services;key=0b76d98b83fa4e0dab5c295f760826b9/wfs?service=WFS",
                       "request=getFeature",
                       "typeNames=layer-8409",
                       "outputFormat=csv",
                       "count=5",
                       URLencode(sprintf("CQL_FILTER=CONTAINS(GEOMETRY, POINT(%f %f))",addr.df$gd2000_ycoord, addr.df$gd2000_xcoord)),
                       "propertyname=TA2016_NAME");
          council.df <- read.csv(req, stringsAsFactors = FALSE);
          council.df$TAName <- paste0(council.df$TA2016_NAME," Council");
          if(council.df$TAName %in% rownames(type.df)){
            updateSelectizeInput(session, "council", choices = councilNames, selected=council.df$TAName, label=NULL);
          }
        }
      }
    } else {
      values$typeCount <- sum(type.df$Council.Type == type.df[input$council,"Council.Type"]);
      values$barPlotHeight <- values$typeCount * 25;
      region.id <- authority.regions.df$FID[match(sub(" Council$","",input$council),
                                                  authority.regions.df$TA2016_NAME)];
      values$councilJSON <-
        scan(paste(sep="&","https://datafinder.stats.govt.nz/services;key=0b76d98b83fa4e0dab5c295f760826b9/wfs?service=WFS",
                   "request=getFeature",
                   "typeNames=layer-8409",
                   "outputFormat=application/json",
                   paste0("featureId=",region.id),
                   "count=1"),
             sep="\n", what=character(), quiet = TRUE);
      leafletProxy("nzMap") %>% clearGeoJSON() %>%
        addGeoJSON(input$baseMap, geojson = values$councilJSON);
    }
  });
  
  });
