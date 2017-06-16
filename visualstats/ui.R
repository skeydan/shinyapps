library(shiny)
library(threejs)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visual stats"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_pkg"),
      br(),
      uiOutput("choose_data"),
      br(),
      uiOutput("choose_x"),
      br(),
      uiOutput("choose_y"),
      br(),
      uiOutput("choose_z"),
      br(),
      uiOutput("choose_color")
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("qqplot 1-dim", plotOutput("qq1")),
                  tabPanel("qqplot 2-dim", plotOutput("qq2")),
                  tabPanel("theor. qqplot", plotOutput("qqt")),
                  tabPanel("scatter plot", plotOutput("scatterplot")),
                  tabPanel("3-dim plot", scatterplotThreeOutput("three")),
                  tabPanel("time series plot", plotOutput("tsplot"))
      )
    )
  )
)
)