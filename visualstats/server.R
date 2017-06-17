library(shiny)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(threejs)

pkgs <- c( "ggplot2", "datasets")

getasnumeric <- function(vec) {
  if (is.numeric(vec)) vec
  else if (is.factor(vec)) as.numeric(vec) 
  else if (is.character(vec)) as.numeric(as.factor(vec))
  else validate(need(TRUE, "Please choose only numeric or convetible to numeric variables for this plot type"))
}

shinyServer(function(input, output) {

  # choose package 
  output$choose_pkg <- renderUI({
    selectInput("choose_pkg", "Choose package", as.list(pkgs))
  })
  
  # choose data set
  output$choose_data <- renderUI({
    if(is.null(input$choose_pkg))
      return()
    pkg <- input$choose_pkg
    choices = unclass(data(package = pkg))$results[,3]
    selectInput("choose_data", "Choose data set", as.list(choices))
  })
 
  # get column names
  choose_column <- reactive({
    if(is.null(input$choose_pkg) || is.null(input$choose_data))
      return()
    pkg <- input$choose_pkg
    do.call(require, list(pkg))
    ds <- get(input$choose_data)
    vars <- names(ds)
    vars
    
  })
  
  # choose x column  
  output$choose_x <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_x", "Choose x variable:", choices = c("", vars))
  })

  # choose y column  
  output$choose_y <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_y", "Choose y variable:", choices = c("",vars))
  })
  
  # choose z column for 3-dim
  output$choose_z <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_z", "Choose z variable for 3d plot:", choices = c("",vars))
  })
  
  # choose column for color aesthetic
  output$choose_color <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_color", "Choose variable for color aesthetic (scatterplot):", choices = c("",vars))
  })
  
  # choose column for color aesthetic
  output$choose_size <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_size", "Choose variable for size aesthetic (scatterplot):", choices = c("",vars))
  })
  
  #
  output$qq1 <- renderPlot({
    if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x))
      return()
    validate(need(input$choose_x != "", "Please choose an x variable for the qqplot."))
    ds <- get(input$choose_data)
    x <- input$choose_x
    vals <- ds[[x]]
    vals <- getasnumeric(vals)
    sorted <- sort(vals)
    df <- data_frame(x = (1:(length(sorted)) - 0.5)/length(sorted), y = sorted)
    ggplot(df, aes(x=x, y=y)) + geom_point() + scale_x_continuous(c(0.0,1.0)) + theme(aspect.ratio = 1)
  },
  height = 700)
  
  # 
  output$scatterplot <- renderPlot({
    if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x) || 
      is.null(input$choose_y)) 
        return()
    validate(need(input$choose_x != "", "Please choose an x variable for the scatterplot."))
    validate(need(input$choose_y != "", "Please choose a y variable for the scatterplot."))
    ds <- get(input$choose_data)
    x <- input$choose_x
    y <- input$choose_y
    color <- input$choose_color
    print(color)
    ggplot(ds, aes_string(x=x, y=y, color=color)) + geom_point()
  })  
  
  # 
  output$three <- renderScatterplotThree({
    if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x) || 
       is.null(input$choose_y) || is.null(input$choose_z)) 
      return()
    ds <- get(input$choose_data)
    x <- getasnumeric(ds[[input$choose_x]])
    y <- getasnumeric(ds[[input$choose_y]])
    z <- getasnumeric(ds[[input$choose_z]]) 
    scatterplot3js(x,y,z, axisLabels = c(input$choose_x, input$choose_y, input$choose_z), color=rainbow(length(z)), bg = "whitesmoke")
  })  
  
  # time series
  output$tsplot <- renderPlot({
    if(is.null(input$choose_pkg) || is.null(input$choose_data)) return()
    ds <- get(input$choose_data)
    validate(need(is.ts(ds), "Time series plots will appear here, but for time series only."))
    if (!is.ts(ds)) return()
    autoplot(ds)
  }) 
  
})