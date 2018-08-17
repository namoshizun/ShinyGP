#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(htmlwidgets)
library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)
library(reshape)
library(readr)
library(DT)

source('utils.R')
source('kernels.R')
source('gp.R')


add_dot_js <- readr::read_file('add_dot.js')
# add_dot_js <- gsub('[\n\r]', '', add_dot_js)


shinyServer(function(input, output, session) {
  # ---------------
  # Globals
  # ---------------
  XMAX <- 10
  XMIN <- -10
  DOMAIN <- matrix(seq(XMIN, XMAX, 0.2), ncol = 1)
  
  # Reactive Values
  # ---------------
  observationsRv <- reactiveVal(value = data.frame(x = c(), y = c()))
  ?reactiveVal
  
  # -----
  # Main
  # -----
  # Choosing a kernel function using its lower-case name
  chooseKernel <- function(kernName) {
    kern_fn <- list(
      Matern32 = matern32,
      Matern52 = matern52,
      RBF = rbf
    )[kernName][[1]]
    
    if (is.null(kern_fn)) {
      stop('Unknown Kernel!')
    }
    
    return(kern_fn)
  }
  
  # Build Gaussian Processes model using existing hyper-parameters
  buildGP <- function() {
    hy_params <- list(
      lenscale = input$lenScale,
      variance = input$variance
    )
    
    kern <- do.call(chooseKernel(input$kernel), hy_params)
    gp(kern)
  }
  
  # Data of functions sampled from the prior distribution over the domain.
  priorPlotData <- eventReactive(input$doSample, {
    gp_model <- buildGP()
    f_priors <- gp_model$draw_priors(X = DOMAIN, n_priors = as.integer(input$nFunc))
    
    plot_data <- f_priors %>%
      data.frame(.) %>% 
      cbind(DOMAIN, .) %>% 
      melt(., id = 'DOMAIN')
    
    return(plot_data)
    }, ignoreNULL = FALSE
  )
  
  
  # Data of functions sampled from the posterior distribution over the domain
  postPlotData <- function(observations) {
    X <- as.matrix(observations[, 'x'])
    Y <- as.matrix(observations[, 'y'])
    
    gp_model <- buildGP()
    mean_and_var <- gp_model$predict_f(X, Y, DOMAIN)
    
    f_mean <- mean_and_var$mean
    f_var <- mean_and_var$var
    f_lwr <- f_mean - diag(f_var)
    f_upr <- f_mean + diag(f_var)
    plot_data <- data.frame(cbind(X_test, f_mean, f_lwr, f_upr))
    colnames(plot_data) <- c('X_test', 'f_mean', 'lwr', 'upr')
    
    return(plot_data)
  }
  
  
  # ----------------
  # Output Producers
  # ----------------
  # Resetting model configuration inputs
  observeEvent(input$resetParam, {
    updateNumericInput(session, inputId = 'kernel', value = 'Matern32')
    updateNumericInput(session, inputId = 'nFunc', value = 5)
    updateNumericInput(session, inputId = 'lenScale', value = 1.)
    updateNumericInput(session, inputId = 'variance', value = 2.)
    updateNumericInput(session, inputId = 'period', value = 2)
  })
  
  
  # Resetting observations
  observeEvent(input$resetObs, {
    observationsRv(data.frame(x = c(), y = c()))
  })
  
  
  # Append [x, y] to the observation dataframe. This should trigger a refresh of posterior plot
  observeEvent(input$addObs, {
    observations <- observationsRv()
    observations <- rbind(observations, list(x = input$addObs$x, y = input$addObs$y))
    observationsRv(observations)
  })
  
  
  # Sample functions from the prior distribution
  output$priorPlot <- renderPlot({
    data <- priorPlotData()
    ggplot(data = data) + geom_line(aes(x = DOMAIN, y = value, colour = variable))
  })
  
  
  output$modelConfig <- renderUI({
    config <- list(
      Kernel = input$kernel,
      Lenscale = input$lenScale,
      Variance = input$variance,
      Period = input$period
    )
    
    ulist <- tags$ul(
      names(config) %>% 
        purrr::keep(~ !is.na(config[[.]])) %>%
        purrr::map(~ tags$li(paste0(., ': ', config[[.]])))
    )
    return(ulist)
  })
  
  
  output$postPlot <- renderPlotly({
    observations <- observationsRv()
    
    if (is.null(observations) || nrow(observations) == 0) {
      # Draw prior functions if no observation point is placed
      message('No observations... Plot priors')
      data <- priorPlotData()
      plot <- plot_ly() %>% 
        plotly::add_lines(data = data, x = ~DOMAIN, y = ~value, color = ~variable) %>% 
        plotly::layout(title = 'GP Posterior') %>% 
        htmlwidgets::onRender(add_dot_js)
      return(plot)
    }
    
    # Otherwise, draw posterior distributions
    data <- postPlotData(as.matrix(observations))
    plot <- plot_ly() %>% 
      plotly::add_lines(data = data, x = ~X_test, y = ~f_mean, name = 'GP Mean') %>% 
      plotly::add_ribbons(data = data, x = ~X_test, ymin = ~lwr, ymax = ~upr, name = 'GP Uncertainty') %>% 
      plotly::add_markers(data = observations, x = ~x, y = ~y, name = 'Observation Points') %>% 
      plotly::layout(legend = list(x = -0.05, y = 0.99))
    return(plot)
 
  })
})