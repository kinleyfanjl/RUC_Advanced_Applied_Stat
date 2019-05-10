######不同城市给不同网约车公司发送牌照#####
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
###Manuplating Funciton for in Dataset#####
combine_fn = function(x,y){
     ind1 = as.numeric(x != 'NA')
     ind1 = which(ind1 !=1)
     ind2 = as.numeric(y != 'NA')
     ind2 = which(ind2 !=1)
     ind = setdiff(ind1, ind2)
     
     y[ind] = paste0(y[ind], y[ind], sep='、') 
     return(y)
}
regl_fn = function(x){
  c = strsplit(x,split = ' ')[[1]]
  return(c[which(c != '')])
}
regl_fn2 = function(x){
  c = strsplit(x,split = '\'')[[1]]
  return(c[which(c != ' ')])
}


a = read_excel('paizhao.xlsx')
a = a[-1,]
names(a) = a[1,]
a = a[-1,]
names(a) = c('platform','num','city')
b = a %>% mutate(out = combine_fn(num, city))   %>% 
          filter(platform != 'NA')   %>%
          mutate(city = strsplit(out,'、'))  %>%
          select(city)
city_vec_cont = unlist(b)

city_vec= unlist(sapply(city_vec_cont, FUN = regl_fn))
city_ta = table(city_vec)

####此处可以考虑绘制 top10####的图
barplot(city_ta)
plot(density(city_ta))
###########

####可以画一个查处机车数量的中国地图 在不同城市#####
chachu = read_excel('chachu.xlsx')

entro = read.csv('entrpoy排名')
city_en = entro$cities
city_plt = intersect(names(city_ta), city_en)



map<-readShapePoly('chinamapdata/bou2_4p.shp')
plot(map)

data_la = read.csv('shop_area.csv')[,c(4,8,9)]
names(data_la) = c('city','lon','lat')
data_la = apply(data_la,2, as.character)

data_la[,1] = unlist(sapply(data_la[,1], regl_fn2))
data_la[,2] = as.numeric(unlist(sapply(data_la[,2], regl_fn2)))
data_la[,3] = as.numeric(unlist(sapply(data_la[,3], regl_fn2)))
city_all = data_la[,1]

city_long_lat = data_la[city_all %in% city_plt,]

data_city_plt = data.frame(city_long_lat)
No.lincense = unlist(sapply(city_long_lat[,1], 
                            function(x){city_ta[which(x == names(city_ta))]}))
names(No.lincense) = unlist(lapply(strsplit(names(No.lincense), split = '[.]'), 
                                   function(x){return(x[[1]][1])}))

data_city_plt = data_city_plt %>% 
            mutate(No.lice = No.lincense[which(city == names(No.lincense))])

data_city_plt = data_city_plt %>% 
      mutate(city = as.character(city)) %>%
      mutate(lon = as.numeric(as.character(lon))) %>%
      mutate(lat = as.numeric(as.character(lat))) %>%
      mutate(No.lice = as.numeric(as.character(No.lice))) %>% 
      filter(!duplicated(city))
      
#####Insert another data about monopoly power in one city#########
ent = read.csv('entrpoy排名')
ent = ent %>% mutate(cities = as.character(cities)) %>%
              select(entropy,count,cities,total)

final_data = merge(ent,data_city_plt, by.x = 'cities', by.y = 'city', y.all = L)
final_data$fac_total = as.factor(cut(final_data$total, c(0,1000,5000,10000,20000,30000),labels = 1:5))

######plot about all the variables####
final_data_plt_cities = final_data %>% filter(No.lice > 6 | entropy < 2.0) 

mymap = ggplot(data = fortify(map)) +
  geom_polygon(aes(x = long, y = lat, group=group), colour = "gray",fill = "aliceblue") +
  labs(title="The number of admitted licenses for E-Taxi companies",x="",y="") +
  geom_point(data=final_data,aes(x=lon,y=lat,colour=entropy,size = No.lice ),alpha = 0.8)+ 
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text(aes(x=lon+1,y=lat-0.3,label=cities), data=final_data_plt_cities,color="blue4",size=2.5,family = 'STKaiti') +
  theme(text=element_text(family="STKaiti",size=14))
mymap

mymap = ggplot(data = fortify(map)) +
  geom_polygon(aes(x = long, y = lat, group=group), colour = "gray",fill = "aliceblue") +
  labs(title="The number of admitted licenses for E-Taxi companies",x="",y="") +
  geom_point(data=final_data,aes(x=lon,y=lat,colour=entropy,size = No.lice, pch = fac_total ),alpha = 0.8)+ 
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text(aes(x=lon+1,y=lat-0.3,label=cities), data=final_data,color="blue4",size=2.5,family = 'STKaiti') +
  theme(text=element_text(family="STKaiti",size=14))
mymap
#####plot about entropy and Monop.######

p = ggplot(final_data,aes(x=total,y = entropy))
p+ geom_point(data = final_data, alpha = 0.7, aes(colour = No.lice)) + geom_smooth(lwd = 0.6,col = 'red')+
  theme(text=element_text(family="STKaiti",size=13))+
  labs(title = 'Total Taxis, Monopolization, # of admitted lincenses') +
  scale_color_gradient(low="blue", high="red")




