library(shiny)
library(shinydashboard)
library(plotly)


?selectInput

# Main Body
priorVisTabItem <- tabItem(
  tabName = "priorVis",
  
  # SidebarPanel: Kernel and HyperParameter form
  sidebarPanel(
    h4('Kernel'),
    selectInput(
      inputId = 'kernel',
      label = NULL,
      choices = c('Matern32', 'Matern52', 'RBF', 'ExpSineSquared')
    ),
    numericInput(
      inputId = 'nFunc',
      value = 3,
      label = '#Functions'
    ),
    hr(),
    h4('Hyper Params'),
    conditionalPanel(
      condition = 'input.kernel == "RBF" || input.kernel == "Matern32" || input.kernel == "Matern52"',
      numericInput(inputId = 'lenScale', label = "Length Scale", value = 1.0, min = 0),
      numericInput(inputId = 'variance', label = "Variance", value = 2.0, min = 0)
    ),
    conditionalPanel(
      condition = 'input.kernel == "ExpSineSquared"',
      numericInput(inputId = 'lenScale', label = "Length Scale", value = 1.0, min = 0),
      numericInput(inputId = 'variance', label = "Variance", value = 2.0, min = 0),
      numericInput(inputId = 'period', label = "Period", value = NULL, min = 0)
    ),
    actionButton(inputId = 'resetParam', label = 'Reset', width = "100%"),
    actionButton(inputId = 'doSample', label = 'Sample Functions', width = "100%"),
    width = 4
  ),
  
  # MainPanel: Prior Functions
  mainPanel(
    plotOutput(outputId = 'priorPlot'),
    width = 8
  )
)


postVisTabItem <- tabItem(
  tabName = "postVis",
  fluidRow(
    box(h4('Model'),
        uiOutput(outputId = 'modelConfig')
    ),
    box(
      h4('Observation'),
      helpText('Click on the plot to place observation points.'),
      tableOutput(outputId = 'obsTable')
    )
  ),
  actionButton(inputId = 'resetObs', label = 'Reset', width = '100%'),
  hr(),
  fluidRow(mainPanel(
    plotlyOutput(outputId = 'postPlot'),
    width = 12
  ))
)


# Major components
dsHeader <- dashboardHeader(title = "Shiny GP :D", titleWidth = 230)
dsSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Vis Prior", tabName = "priorVis", icon = icon("eye")),
    menuItem("Vis Post", tabName = "postVis", icon = icon("hand-spock"))
  )
)
dsBody <- dashboardBody(
  tabItems(
    priorVisTabItem,
    postVisTabItem
  )
)




# Assemble!
dashboardPage(
  dsHeader,
  dsSidebar,
  dsBody
)


