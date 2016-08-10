## MLMC - My Life, My Council
## - User interface -

library(shiny);
library(shinyjs, quietly = TRUE);
library(leaflet);

options(stringsAsFactors = FALSE);
type.df <- read.csv("data/Local_Authority_Financial_Statistics_Council_Type.csv");
defs.df <- read.csv("data/Local_Authority_Financial_Statistics_Activity_Definitions.csv");

councilNames <- (type.df$Council);
dataCats <- (defs.df$Activity);

shinyUI(
  fluidPage(
    tags$head(
      tags$title("My Life, My Council"),
      tags$link(rel="stylesheet", type="text/css", href="bootstrap.min.css"),
      tags$link(rel="stylesheet", type="text/css", href="styles.css"),
      tags$script(src="myScript.js")
    ),
    useShinyjs(),  ## load shiny js library
    ## Application title
    tags$div(id="mainApp", 
             fluidRow(column(6,tags$div(id="logoDiv",
                                        actionLink("logoLink",tags$img(id="logo", src="mylifemycouncil-logo.png")))),
                      column(6,tags$div(id="videoDiv", class="pull-right",
                                        tags$iframe(id="video", src="https://player.vimeo.com/video/176900559",
                                                    frameborder=0)))),
#<p><a href="https://vimeo.com/176900559">My Life My Council</a> from <a href="https://vimeo.com/user54945005">Lucy Morgan</a> on <a href="https://vimeo.com">Vimeo</a>.</p>")),
                      #column(6,NULL),
                      #column(5,tags$iframe(width=560, height=315, src="https://www.youtube.com/embed/yTgZVVHJMlw",
                      #                  frameborder=0))),
             tabsetPanel(id="tabPanel",
                ### Select panel ###
                tabPanel(title="Select", value="select", class="tabInterface",
                         tags$div(id="bannerDiv",tags$img(src="mylifemycouncil-bannerimage.jpg", id="banner")),
                         tags$h3("How does my council spend my money?"),
                         tags$p("We know that how your money is spent is important to you."),
                         tags$p("At My Life My Council we have brought together information from councils across the country to show you where your money is going."),
                         fluidRow(
                           column(8,
                                  tags$h3("Find my council"),
                                  tags$p("Select from the options below to find out how",
                                         "your council spends money on what is important",
                                         "to you."),
                                  fluidRow(
                                    column(4,"My council is:"),
                                    column(8,selectizeInput("council", label=NULL,
                                                         choices=c("Enter Address or Choose" = "",councilNames),
                                                         options=list(create=TRUE, addPrecedence=TRUE, render=I("{
                                                           option_create: function(data, escape) {
                                                             return '<div class=\"create\">Search for &quot;<strong>' + escape(data.input) + '</strong>&quot;&hellip;</div>';
                                                           }
                                                         }"))))
                                  ),
                                  fluidRow(
                                    column(4,"I'm interested in:"),
                                    column(8,selectInput("cat", label=NULL, choices=dataCats, selected=sample(dataCats,1)))
                                  ),
                                  fluidRow(
                                    column(4,"I want to:"),
                                    column(8,radioButtons("dataType", label=NULL,
                                                 choices=c("compare with other councils for 2015" = "Last Year",
                                                           "see my council's spending since 2010" = "Over Time")))
                                  ),
                                  fluidRow(
                                    column(4,NULL),
                                    column(8,actionButton("viewButton", label="See your council"))
                                  )
                           ), # closes council/category select column
                           column(4,leafletOutput("nzMap"))
                         ),# closes council information row
                         tags$br(),
                         tags$br(),
                         fluidRow(
                           column(9,tags$p("Description: Information used in this website is from the Local Authority",
                                            "Financial Statistics (LAFS) 30 June 2015 data set provided by ",
                                            tags$a(href="http://www.stats.govt.nz/tools_and_services/releases_csv_files/csv-files-for-infoshare.aspx",
                                                   "Statistics New Zealand,"),
                                            " and information on Council by Type and Council Contact Details held by the ",
                                           tags$a(href="http://www.localcouncils.govt.nz/","Department of Internal Affairs."),
                                           " Address lookup is based on data from",
                                           tags$a(href="https://data.linz.govt.nz/layer/3353-nz-street-address-beta/",
                                                   "LINZ"),"and",
                                           tags$a(href="https://datafinder.stats.govt.nz/layer/8409-territorial-authority-2016-generalised-version/",
                                                  "Statistics New Zealand."),
                                           "All information is licensed for re-use under the Creative Commons Attribution",
                                           " 4.0 International licence.", id="smallerfont"))
                         )
                ),
                ### View panel ###
                ## fixed position panel
                tabPanel(title="View", value="view", class="tabInterface",
                         tags$h3("My Council"),
                         uiOutput("viewTopBlurb",inline=TRUE),
                         tags$h3("About this data"),
                         uiOutput("viewDataDesc",inline=TRUE),
                         ## less about height, more about position
                         fluidRow(column(3,actionButton("backButton", label="Back")),
                                  column(6,NULL),
                                  column(3,downloadButton("makePDF", label="Make PDF"))),
                         tags$br(),
                         conditionalPanel(condition="input.dataType=='Last Year'",
                                          fluidRow(
                                            column(3,"Sort by:"),
                                            column(6,selectInput("sortBy", label=NULL, selected = "descending",
                                                                 choices=c("Lowest to highest"="ascending",
                                                                           "Highest to lowest"="descending",
                                                                           "Alphabetical"="alpha"))))
                         ),
                         tags$div(class="plotGraph", height="400px",
                                  conditionalPanel(condition="input.dataType=='Last Year'",
                                                   uiOutput("plotHeading"),
                                                   tags$div(id="scaleBarDiv", plotOutput("plotScaleBar", width="100%", height="75px")),
                                                   uiOutput("dynamicCompPlot"),
                                                   tags$br(),
                                                   tags$p(tags$b("Description:")," The graph shows how much each council is spending on an activity,
                                                          compared to other similar councils. It's measured as a percentage of the council's total
                                                          operating expenditure. Councils are grouped into Unitary, Regional, and Territorial authorities,
                                                          because each of these groups are responsible for different activities.")),
                                  conditionalPanel(condition="input.dataType=='Over Time'",
                                                   plotOutput("yearPlot", height="400px", width="80%"),
                                                   tags$br(),
                                                   tags$p(tags$b("Description:")," The graph shows how much your council is spending on an activity over time.
                                                          It's measured as a percentage of total operating expenditure."))
                         ),
                         uiOutput("dataCaveats"),
                         tags$h3("Contact Details"),
                         uiOutput("contactDetails",inline=TRUE),
                         tags$br(),
                         fluidRow(
                           column(9,tags$textarea(id="councilFeedback", class="form-control", 
                                                  width="100%", rows=4, label = NULL,
                                              placeholder = "Anything you would like to tell your council?")),
                           column(3,actionButton("feedbackButton",label = "Submit comment\nto my council"))),
                         conditionalPanel(condition="values.dialogMessage != ''",
                                          tags$br(),
                                          uiOutput("dialogMessage"))
                ) # closes "view" tabPanel
             ), # closes tabSetPanel
             tags$br(),
             tags$br(),
             tags$div(id="bottomBorder", style="background-color: #23723F"),
             tags$p(id="footer", "Powered by ",
                    tags$a(href="https://www.rstudio.com/products/shiny/",
                           "RStudio",tags$sup("®")," Shiny")," and ",
                    tags$a(href="http://www.catalyst.net.nz/catalyst-cloud",
                           "Catalyst Cloud."),
                    " See ",
                    tags$a(href="https://github.com/gringer/MLMC","here"),
                    "for the code used to make this website.")
    ) # closes mainApp div
  ) # closes fluidPage
);
