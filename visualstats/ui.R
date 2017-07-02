library(shiny)
library(threejs)

dists <- c("beta","cauchy","chi-squared","exponential","f","gamma","geometric","log-normal",
           "lognormal","logistic","negative binomial","normal","Poisson","t","weibull")

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
      plotOutput("get_points", click = "click"),
      verbatimTextOutput("info")
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("qqplot 1d",
                           plotOutput("qq1"),
                           checkboxInput("log_x1", "log data?", FALSE)),
                  tabPanel("qqplot 2d",
                           plotOutput("qq2"),
                           checkboxInput("log_x", "log x data?", FALSE),
                           checkboxInput("log_y", "log y data?", FALSE)),
                  tabPanel("prob plots", plotOutput("probplots"),
                           radioButtons("pp", NULL, 
                                        list("plot cumulative probabilities" = "ppPlot",
                                             "plot quantiles" = "qqPlot")),
                           br(),
                           selectInput("choose_dist", "Choose reference distribution:", 
                                        choices = dists, width = "20%"),
                           br(),
                           checkboxInput("log_xp", "log data?", FALSE)),
                  tabPanel("scatterplot", 
                           plotOutput("scatterplot"),
                           uiOutput("choose_grid_col"),
                           uiOutput("choose_grid_row"),
                           uiOutput("choose_color"),
                           uiOutput("choose_size_or_shape")),
                  tabPanel("3d plot",
                           scatterplotThreeOutput("three"),
                           uiOutput("choose_z")),
                  tabPanel("time series plot",
                           plotOutput("tsplot")),
                  tabPanel("adj. var plot",
                           plotOutput("adj_var_plot"))
      )
    )
  )
)
)