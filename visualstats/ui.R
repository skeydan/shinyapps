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
      uiOutput("choose_grid_col"),
      br(),
      uiOutput("choose_grid_row"),
      br(),
      uiOutput("choose_color"),
      br(),
      uiOutput("choose_size_or_shape")
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("qqplot 1d", plotOutput("qq1")),
                  tabPanel("qqplot 2d", plotOutput("qq2")),
                  tabPanel("theor. qqplot", plotOutput("qqt")),
                  tabPanel("scatterplot", plotOutput("scatterplot")),
                  tabPanel("3d plot", scatterplotThreeOutput("three")),
                  tabPanel("time series plot", plotOutput("tsplot"))
      )
    )
  )
)
)