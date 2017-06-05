library(shiny)

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
      uiOutput("choose_color")
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("scatter plot", plotOutput("scatterplot")),
                  tabPanel("time series plot", plotOutput("tsplot"))
      )
    )
  )
)
)