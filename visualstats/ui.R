library(shiny)
library(threejs)

dists <- c("beta","cauchy","chi-squared","exponential","f","gamma","geometric","log-normal",
           "lognormal","logistic","negative binomial","normal","Poisson","t","weibull")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visual stats"),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(HTML("By default, data from the chosen dataset is used.<br/>
               Use check box below to switch to drawing your own data (only usable with qqplots 1d or 2d, prob plots, and simple scatter plots.)"),
          style="font-weight: bold;"),
      br(),
      uiOutput("choose_pkg"),
      uiOutput("choose_data"),
      uiOutput("choose_x"),  
      uiOutput("choose_y"),
      br(),
      radioButtons("type", NULL, 
                   list("Use given dataset" = "use_dataset",
                        "Use self-drawn points" = "use_points")),
      br(),
      div("Draw your own points here:", style="font-weight: bold;"),
      plotOutput("get_points", click = "click")
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