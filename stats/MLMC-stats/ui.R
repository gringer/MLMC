## MLMC Statistics
## - User interface -

library(shiny);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Usage Statistics for MLMC"),
  fluidRow(
    column(6,plotOutput("timePlot")),
    column(6,plotOutput("dwellPlot"))
  ),
  fluidRow(
    column(6,plotOutput("catPlot")),
    column(6,plotOutput("councilPlot"))
  )
));
