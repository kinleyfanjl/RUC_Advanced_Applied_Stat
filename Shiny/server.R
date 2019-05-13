## server.R
load('final_data.Rdata')
load('map.Rdata')
load('d')
library(dplyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
library(ggmap)
library(shiny)
library(wordcloud)

shinyServer(function(input, output) {
    maptypeInput = eventReactive(input$update, {
      switch(input$type,
             'map' = 'map',
             'curve' = 'curve',
             'words' = 'words')
    })
   
  # Generate a summary of the dataset ----
    output$view <- renderTable({
      # if(maptypeInput() == 'map')
      #   {
      # final_data[sample(1:nrow(final_data), isolate(input$num)),]
      # }
      # if(maptypeInput() == 'cu')
      # {
      #   final_data[sample(1:nrow(final_data), isolate(input$num)),]
      # }
      if(maptypeInput() == 'words'){
        d[sample(1:nrow(d), isolate(input$num)),]
      }
      else{
        final_data[sample(1:nrow(final_data), isolate(input$num)),]
      }
      
    })
    
  
  #####plot about entropy and Monop.######

  output$map <- renderPlot({
    K = isolate(input$top)
    final_data_plt_cities = final_data %>% filter(rank(entropy) > length(entropy) - K) 
    if(maptypeInput() == 'map'){
      plot(map)
      points(final_data_plt_cities$lon, final_data_plt_cities$lat, pch = 20, col = 'blue')
    }
    else{ 
      if(maptypeInput() == 'curve'){
      p = ggplot(final_data,aes(x=total,y = entropy))
      p = p + geom_point(data = final_data_plt_cities, alpha = 0.7, aes(colour = No.lice)) + geom_smooth(lwd = 0.6,col = 'red')+
        labs(title = 'Total Taxis, Monopolization, # of admitted lincenses') +
        scale_color_gradient(low="blue", high="red")
      p
      }
      else{
        if(maptypeInput() == 'words'){
          wordcloud(d[1:K,1], d[1:K,2], scale = c(4,0.5),
                         family = 'STKaiti', colors=brewer.pal(8, "Dark2"))
          
        }
      }
    }
  })
  
})
