## ui.R
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visulazition of datasets"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    selectInput('type', 'choose a type',
                choices = c('map', 'curve', 'words')),
    
    sliderInput("top",
                  label = "Choose the obs in decreasing order:",
                  min = 1,
                  max = 71, step = 1, value = 2),
    
    numericInput('num', "Random # of obs in Datasets:", 10),
    helpText("Note: Because ggpolygen() in ggplot, which is used to generate 
             a beautiful map, needs long time for computation. So I didn't put it here, because 
             it will needs you to wait for a long time." ),
    actionButton("update", "Update View")
    ),
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Map-plot"),
      plotOutput("map"),
      # Output: Header + table of distribution ----
      h4("View"),
      tableOutput('Random samples')
    )
    
    # Show a plot of the generated distribution
  )
))
