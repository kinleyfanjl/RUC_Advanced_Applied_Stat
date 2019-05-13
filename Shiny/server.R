## server.R
load('final_data.Rdata')
load('map.Rdata')
library(dplyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)

library(shiny)

shinyServer(function(input, output) {
    maptypeInput = eventReactive(input$update, {
      switch(input$type,
             'map' = 'map',
             'curve' = 'curve')
    })
  # Generate a summary of the dataset ----
    output$view <- renderTable({
      head(final_data, n = isolate(input$obs))
    })
    
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(final_data, n = isolate(input$obs))
  })
  
  #####plot about entropy and Monop.######
  output$map <- renderPlot({
    K = input$top
    final_data_plt_cities = final_data %>% filter(rank(entropy) > length(entropy) - K) 
    
      if(maptypeInput() == 'map'){
        mymap = ggplot(data = fortify(map)) +
          geom_polygon(aes(x = long, y = lat, group=group), colour = "gray",fill = "aliceblue") +
          labs(title="The number of admitted licenses for E-Taxi companies",x="",y="") +
          geom_point(data=final_data_plt_cities,aes(x=lon,y=lat,colour=entropy,size = No.lice, pch = fac_total ),alpha = 0.8)+ 
          scale_color_gradientn(colours = rainbow(5)) 
        mymap
      }
    if(maptypeInput() == 'curve'){
      p = ggplot(final_data,aes(x=total,y = entropy))
      p = p + geom_point(data = final_data, alpha = 0.7, aes(colour = No.lice)) + geom_smooth(lwd = 0.6,col = 'red')+
        labs(title = 'Total Taxis, Monopolization, # of admitted lincenses') +
        scale_color_gradient(low="blue", high="red")
      p
    }
    
  })
  
})
