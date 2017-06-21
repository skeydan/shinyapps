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
  else validate(need(TRUE, "Please choose only numeric or convertible to numeric variables for this plot type"))
}

shinyServer(function(input, output) {

  # choose package 
  output$choose_pkg <- renderUI({
    selectInput("choose_pkg", "Choose package", as.list(pkgs))
  })
  
  # choose data set
  output$choose_data <- renderUI({
    # if(is.null(input$choose_pkg))
    #   return()
    pkg <- input$choose_pkg
    choices = unclass(data(package = pkg))$results[,3]
    selectInput("choose_data", "Choose data set", as.list(choices))
  })
 
  # get column names
  choose_column <- reactive({
    # if(is.null(input$choose_pkg) || is.null(input$choose_data))
    #   return()
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
    selectInput("choose_x", "Choose x variable:", choices = c("NA", vars))
  })

  # choose y column  
  output$choose_y <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_y", "Choose y variable:", choices = c("NA",vars))
  })
  
  # choose z column for 3-dim
  output$choose_z <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_z", "3d plot: choose z variable:", choices = c("NA",vars))
  })
  
  # choose column for color aesthetic
  output$choose_color <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_color", "color:", choices = c("NA",vars))
  })
  
  # choose column for size resp. shape aesthetic
  output$choose_size_or_shape <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_size_or_shape", "size (continuous) / shape (discrete):", choices = c("NA",vars))
  })
  
  # choose column for color aesthetic
  output$choose_grid_col <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_grid_col", "facet_grid: column", choices = c("NA",vars))
  })
  
  # choose column for color aesthetic
  output$choose_grid_row <- renderUI({
    vars <- choose_column()
    if(is.null(vars)) return()
    selectInput("choose_grid_row", "facet_grid: row", choices = c("NA",vars))
  })
  
  #
  output$qq1 <- renderPlot({
    # if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x))
    #   return()
    validate(need(input$choose_x != "NA", "Please choose an x variable for the qqplot."))
    ds <- get(input$choose_data)
    x <- input$choose_x
    vals <- ds[[x]]
    vals <- getasnumeric(vals)
    sorted <- sort(vals)
    df <- data_frame(x = (1:(length(sorted)) - 0.5)/length(sorted), y = sorted)
    ggplot(df, aes(x=x, y=y)) + geom_point() + scale_x_continuous(c(0.0,1.0)) + theme(aspect.ratio = 1)
  },
  height = 600)
  
  # 
  output$qq2 <- renderPlot({
    # if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x) || 
    #    is.null(input$choose_y)) 
    #  return()
    validate(need(input$choose_x != "NA" & input$choose_y != "NA", "Please choose x and y variables for the qqplot."))
    ds <- get(input$choose_data)
    x <- ds[[input$choose_x]]
    y <- ds[[input$choose_y]]
    d <- as.data.frame(qqplot(x, y, plot.it=FALSE))
    g <- ggplot(d) + geom_point(aes(x=x, y=y)) + theme(aspect.ratio = 1)
    g
  },
  height = 600)
    
  # 
  output$scatterplot <- renderPlot({
    # if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x) || 
    #   is.null(input$choose_y)) 
    #     return()
    validate(need(input$choose_x != "NA" & input$choose_y != "NA", "Please choose x and y variables for the scatterplot."))
    ds <- get(input$choose_data)
    x <- input$choose_x
    y <- input$choose_y
    if (input$choose_color != "NA") {
      color <- input$choose_color
      color_col <- ds[[color]]
    }
    if (input$choose_size_or_shape != "NA") {
      var <- input$choose_size_or_shape
      var_col <- ds[[var]]
      if(is.numeric(var_col)) { 
        size <- var
        } else {shape <- var}
    }
    if (input$choose_grid_col != "NA") {
      grid_col <- input$choose_grid_col
    }
    if (input$choose_grid_row != "NA") {
      grid_row <- input$choose_grid_row
    }
    if(exists("grid_row") && exists("grid_col")) {
      grid_formula <- paste0(grid_row, " ~ ", grid_col)
    } else if (exists("grid_row")) {
      grid_formula <- paste0(grid_row, " ~ .")
    } else if (exists("grid_col")) {
      grid_formula <- paste0(". ~ ", grid_col)
    }
    g <- ggplot(ds, aes_string(x=x,
                               y=y,
                               color=if(exists("color")) color else NULL,
                               shape = if(exists("shape")) shape else NULL,
                               size = if (exists("size")) size else NULL)) + geom_point()
    if(exists("color") && is.numeric(color_col)) g <- g +  scale_color_continuous(low='cyan', high='orange')
    if(exists("grid_formula")) g <- g + facet_grid(grid_formula)
    g
  })  
  
  # 
  output$three <- renderScatterplotThree({
    # if(is.null(input$choose_pkg) || is.null(input$choose_data) || is.null(input$choose_x) || 
    #    is.null(input$choose_y) || is.null(input$choose_z)) 
    #   return()
    validate(need(input$choose_x != "NA" & input$choose_y != "NA" & input$choose_z != "NA",
                  "Please choose x, y, and z variables for the 3d plot."))
    ds <- get(input$choose_data)
    x <- getasnumeric(ds[[input$choose_x]])
    y <- getasnumeric(ds[[input$choose_y]])
    z <- getasnumeric(ds[[input$choose_z]]) 
    scatterplot3js(x,y,z, axisLabels = c(input$choose_x, input$choose_y, input$choose_z), color=rainbow(length(z)), bg = "whitesmoke")
  })  
  
  # time series
  output$tsplot <- renderPlot({
    # if(is.null(input$choose_pkg) || is.null(input$choose_data)) return()
    ds <- get(input$choose_data)
    validate(need(is.ts(ds), "Time series plots will appear here, but for time series only."))
    if (!is.ts(ds)) return()
    autoplot(ds)
  }) 
  
})