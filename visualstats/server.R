library(shiny)
library(ggplot2)
library(ggfortify)

pkgs <- c("datasets", "ggplot2")

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
    selectInput("choose_x", "Choose x variable:", choices = vars)
  })

  # choose y column  
  output$choose_y <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_y", "Choose y variable:", choices = vars)
  })
  
  # choose column for color aesthetic
  output$choose_color <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_color", "Choose variable for color aesthetic:", choices = vars)
  })
  
  # 
  output$scatterplot <- renderPlot({
    if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x) || 
      is.null(input$choose_y)) 
        return()
    ds <- get(input$choose_data)
    x <- input$choose_x
    y <- input$choose_y
    ggplot(ds, aes_string(x=x, y=y)) + geom_point()
  })  
  
  # time series
  output$tsplot <- renderPlot({
    if(is.null(input$choose_pkg) || is.null(input$choose_data)) return()
    ds <- get(input$choose_data)
    if (!is.ts(ds)) return()
    autoplot(ds)
  }) 
  
})