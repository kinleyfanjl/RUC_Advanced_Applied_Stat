
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visulazition of datasets"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    selectInput('type', 'choose a type',
                choices = c('map', 'curve')),
    
    sliderInput("top",
                  label = "Choose the top cities in map:",
                  min = 1,
                  max = 30, step = 1, value = 15),
    
    numericInput('obs', "# of obs in View:", 10),
    
    actionButton("update", "Update View")
    ),
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Map-plot"),
      plotOutput("map"),
      # Output: Header + table of distribution ----
      h4("View"),
      tableOutput('view')
    )
    
    # Show a plot of the generated distribution
  )
))
