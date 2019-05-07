
library(jiebaR)
library(readxl)
library('tm')
library('NLP')
library('cidian')
library('stingi')
library('pbapply')
library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)

#########After running all coding files, we fetch the final dataset#########

final_data1 = merge(cond_data1, final_data, by.x = 'cond_cities', by.y = 'cities')
final_data2 = merge(final_data1, news_plt_data, by.x = 'cond_cities', by.y = 'cities')
View(final_data2)
