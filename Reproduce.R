
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
library(infotheo)
library('tm')
library('topicmodels')
library('NLP')
library('BBmisc')
library('cidian')
library('stringi')
library('pbapply')
library('dbscan') 
library(jiebaR)
library(readxl)
library('wordcloud2')
library(stringr)

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
##############
###Definition of functions###
getnum = function(x){
  k = 0
  for(i in 1:length(x)){
   if(is.na(x[i])){
     k = i-1;
     break;
   }
    else if(!is.na(x[length(x)])){
      k = length(x)
    }
    else k=0
  }
  return(k)
}

parasum = function(x){
  k = 0
  for(i in 1:length(x)){
    if(is.na(x[i])){
      k = i-1;
      break;
    }
    else {k = 0}
  }
  return(sum(x[1:k]))
}

getfinal = function(x,i){
  x = x[-which(is.na(x))]
  x =x[which(x-i>0)]
  return(c(entropy(x), length(x)))
}


####main procedure####
a=  read_excel('2018111802.xlsx',col_names = FALSE)
a = data.frame((a))
cities = unlist(strsplit(a[,1],'-'))
cities = cities[((1:length(cities))-2)%%4 == 0]

b = apply(a[,-1],2,as.numeric)

result = apply(b, 1, getnum)
newdata = NULL
for(i in 0:(nrow(b)/2-1)){
newdata1  = b[2*i+1,1:result[2*i+1]]
    if(result[2*i+2] == 0){
      newdata2 = NULL
    }
   else {newdata2 = b[2*i +2, 1:result[2*i+2]]}
    newda = c(newdata1,newdata2)
    newda = c(newda, rep(NA,200-length(newda)))
  
  newdata = rbind(newdata,newda)
}

total = apply(newdata,1,parasum)
cutpro = total*0.01

final = NULL
for(i in 1:nrow(newdata)){
  finalvec = getfinal(newdata[i,],cutpro[i]) 
  final = cbind(final,finalvec)
}

colnames(final) = cities
final = data.frame(t(final))
colnames(final) = c('entropy','count')
final = cbind(final,total , cities)
final = final[-which(final$count == 0),] ##delete cities with 0 companies
####Data visualization####
data1 = final[order(final$count,decreasing = T)[1:15],]
data1 = data1[order(data1$count,decreasing =T),]
p = ggplot(data1,aes(x = reorder(rownames(data1),count), y= count))
p + geom_bar(stat= 'identity',fill = 'orange',width = 0.3,alpha = 0.5)+
  theme(text=element_text(family="STKaiti",size=13))+coord_flip()

data2 = final[order(final$entropy,decreasing = T)[1:15],]
data2 = data2[order(data2$entropy,decreasing =T),]
p = ggplot(data2,aes(x = reorder(rownames(data2),entropy), y= exp(entropy)))
p + geom_bar(stat= 'identity',fill = 'red',width = 0.3,alpha = 0.5)+  coord_flip()

data3 = final[order(final$total,decreasing = T)[1:15],]
data3 = data3[order(data3$total,decreasing =T),]
p = ggplot(data3,aes(x = reorder(rownames(data3),total), y= total))
p + geom_bar(stat= 'identity',fill = 'orange',width = 0.3,alpha = 0.5)+coord_flip()


p = ggplot(final,aes(x=count,y = entropy))
p+ geom_point(alpha = 0.3) + geom_smooth(lwd = 0.6)+
   theme(text=element_text(family="STKaiti",size=13))

p = ggplot(final,aes(x=total,y = entropy))
p+ geom_point(alpha = 0.3) + geom_smooth(lwd = 0.6,col = 'red')



news = read_excel('news.xlsx')
news_text = news[,12]
pos = read.csv('pos.txt', stringsAsFactors = F)
neg = read.csv('neg.txt', stringsAsFactors = F)

pos = cbind(pos, rep(1, length(pos)))
neg = cbind(neg, rep(1, length(neg)))

load_user_dict('richang.txt')
load_user_dict('city.txt')
load_user_dict('chengyu.txt')
load_user_dict('pos.txt')
load_user_dict('neg.txt')

news_clean = function(news_text){
  news_text = gsub('[0-9a-zA-Z]', '', news_text)
  news_text = gsub('[[:punct:]]', '', news_text)
  news_text = gsub('[[:space:]]', '', news_text)
  return(news_text)
}

news_cleaned  = sapply(news_text, news_clean)
wk = worker(user = 'all.txt', stop = 'stop.txt')

fenci = function(x){
  wk <= x
}
#key_fenci = function(x){
#  keywk <= x
#}
seg_news = sapply(news_cleaned, fenci)
names(seg_news) = NULL
q_seg = seg_news
keywk = worker('keywords')
#ey_seg_news = sapply(news_cleaned, key_fenci)

cal_scoref = function(x, y, z){
  l1 = length(which(x %in% y))
  l2 = length(which(x %in% z))
  l = l1 - l2
  return(l)
}
score = lapply(seg_news, cal_scoref, y = pos[,1], z = neg[,1])

news = as.data.frame(news)
news[,13] = unlist(as.numeric(score))

news_city = news[,2] 


decode_scel(scel = 'city.scel' ,
                  output = 'city.txt', cpp = FALSE, progress = TRUE)
index = c()
naindex = function(x){
  for(i in 1:length(x)){
    if(x[i] == 'NA'){
      index = c(index, i)
    }
  }
  return(index)
}
naind = naindex(q_seg)
q_seg = q_seg[-naind]
news = news[-naind,]
###删除点赞关注 这种词
filter = c('点赞','点击','关注','原文','免费',' | ',
           '微','信号','阅读','来源','网友','蓝字','现场',
           '视频','精彩','留言','二维码','微信','朋友','爆料','年','月',
           '日', '上方', '下方', '提示')

fil_seg = filter_segment(q_seg, filter)
q_seg = fil_seg




doc_q = VCorpus(VectorSource(q_seg))
 doc_q = tm_map(doc_q, stripWhitespace)
control = list(removePunctuation = T,
               minDocFreq = 1,
               wordLengths = c(2, Inf),
               weighting = weightTfIdf)
doc_q = doc_q[-c(384, 480 ,569, 659 ,767, 939 ,1034 ,1099, 1164)]
q_doc.tdm = TermDocumentMatrix(doc_q, control)
library('wordcloud2')
m = as.matrix(q_doc.tdm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
wordcloud2(d[1:50,], 
           shape = 'circle',
           fontFamily = 'STKaiti')

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

num_words = c('一','二','三','四','五','六','七','八','九','十','百','千','万')
cancel_num = function(x){
  fun1 = function(y){
    if(sum(strsplit(y, split= '')[[1]] %in% num_words) > 0)
      return('啊')
    else
      return(y)
  } 
  sapply(x, fun1)
}
cancel_one = function(x){
  l = x
  l = l[-which(sapply(l, nchar) <= 1)]
  return(l)
}
#####删除单个字
q_seg = lapply(q_seg, cancel_num)
q_seg = lapply(q_seg, cancel_one)
term.table = table(unlist(q_seg))
term.table = sort(term.table, decreasing = TRUE)
term.table = term.table[term.table>1]
vocab = names(term.table)

documents <- lapply(q_seg, get.terms)

D <- length(documents) # number of documents (3740)
W <- length(vocab) # number of terms in the vocab (18,536)
doc.length <- sapply(documents,
                     function(x) sum(x[2, ]))
# number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length) # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)

# frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

K <- 5
G <- 5000
alpha <- 0.02
eta <- 0.02
# Fit the model:
library(lda)
set.seed(1234)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(
  documents = documents, K = K,
  vocab = vocab, num.iterations = G,
  alpha = alpha, eta = eta,
  initial = NULL, burnin = 1000,
  compute.log.likelihood = TRUE
)
#library(archivist)
#saveToRepo(fit,
#           repoDir = "../Museum")

theta <- t(apply(fit$document_sums + alpha,
                 2,
                 function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta,
               2,
               function(x) x/sum(x)))
library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(
  phi = phi,
  theta = theta,
  doc.length = doc.length,
  vocab = vocab,
  term.frequency = term.frequency
)
serVis(json, out.dir = 'vis',
       open.browser = TRUE)

assigned_topic = (lapply(fit[[1]], function(x){
  names(which.max(table(x)))}))
news_city = news$城市
news_city_list = sapply(news_city, function(x){strsplit(x,';')})

indicator = unlist(lapply(news_city_list, function(x){
  sum(x %in% city_plt) > 0
}))

null_ind = which(unlist(lapply(assigned_topic, is.null)))

news$indicator = as.numeric((indicator))
news = news[-null_ind,]
news$topic = as.numeric(unlist(assigned_topic))
city_mat = NULL
for(i in 1:length(news_city_list)){
  k =  as.numeric(city_plt %in% news_city_list[[i]])
  city_mat = rbind(city_mat, k)
}
city_mat = as.data.frame(city_mat)
colnames(city_mat) = city_plt
city_mat = city_mat[-null_ind,]

news1 = data.frame(news, city_mat)
news1 = news1 %>% filter(indicator > 0)
news1_group = news1 %>% group_by(topic)

news1_group %>% summarise(emo_score = mean(V13))
news1_group %>% summarise(emo_score = median(V13))
##第1, 4类是偏好的， 第2类中性， 第3，5类很差

news1$topic = factor(news1$topic)
topic_city_mat = NULL
for(i in 16:86){
  k = table(news1$topic[which(news1[,i] == 1)])
  topic_city_mat = rbind(topic_city_mat, k)
}

city_score = NULL
for(i in 16:86){
  k = sum(news1$V13[which(news1[,i] == 1)])
  city_score = rbind(city_score, k)
}

news_plt = cbind(city_score, topic_city_mat) 
news_plt_data = data.frame(news_plt)
rownames(news_plt) = city_plt
news_plt_data$cities =  city_plt

news_plt[order(apply(news_plt[,-1], 1, sum), decreasing = TRUE)[1:10], ] 
news_plt[order(news_plt[,1], decreasing = TRUE)[1:10], ] 
news_plt[order(news_plt[,1])[1:10], ] 

cond = read_excel('admited_condi.xlsx')
cond_cities = cond$城市
cond_years = cond$车龄
cond_dist = cond$行驶里程
cond_insur = cond$保险

cond_data = data.frame(cond_cities)

cond_dist = sapply(cond_dist, FUN = str_extract, 
                   pattern = "\\d+")
cond_data$dist = cond_dist
cond_data$dist[which(is.na(cond_data$dist))] = 0  
cond_data$insur = sapply(cond_insur, function(x){sum(
  strsplit(x, split = '')[[1]] == '险')})
cond_data$insur[which(is.na(cond_data$insur))] = 0 
cond_data$years = sapply(cond_years, FUN = str_extract, 
                   pattern = "\\d+")
cond_data$years[which(is.na(cond_data$years))] = 0  
cond_data$years[60] = 3
cond_data %>% filter(cond_cities %in% city_plt)
cond_data1 = cond_data %>% filter(cond_cities %in% city_plt)
cond_data1 = cond_data1[!duplicated(cond_data1$cond_cities),]



final_data1 = merge(cond_data1, final_data, by.x = 'cond_cities', by.y = 'cities')

final_data2 = merge(final_data1, news_plt_data, by.x = 'cond_cities', by.y = 'cities')
View(final_data2)
final_data2$total = (final_data2$total - 
                          mean(final_data2$total)) / sd(final_data2$total)
final_data2$V1 = (final_data2$V1 - 
                       mean(final_data2$V1)) / sd(final_data2$V1)
final_data2$V1 = (final_data2$V1 - 
                    mean(final_data2$V1)) / sd(final_data2$V1)
final_data2$years = as.numeric(final_data2$years)/8 * 5
final_data2$insur = (as.numeric(final_data2$insur) + 1)/4 * 5
final_data2$dist = (as.numeric(final_data2$dist) + 1)/60 * 5


final_data_mat = apply(as.matrix(final_data2), 2, as.numeric)

#final_data_mat = normalize(final_data_mat, margin = 2L)
#####MeanShift clustering
h = c(rep(1.5,14)); iter = 1000
result <- meanShift(
  final_data_mat,
  final_data_mat,
  bandwidth=h,
  alpha=0,
  iterations = iter
)
assignments = result$assignment
####DBSCAN
kNNdistplot(final_data_mat, k = 15)
abline(h = 8, col = "red", lty=2)
res = hdbscan(final_data_mat, minPts = 15)
res
####Kmeans
ss = NULL
for(i in 1 : 20){
  fit1 = kmeans(final_data_mat, l)
  ss1 = 1 - sum(fit1$withinss)/sum(fit1$betweenss)
  ss = c(ss, ss1)
}
plot(ss, type = 'l')
l = 7
fit1 = kmeans(final_data_mat, l)
clusters = fit1$cluster

final_data2$cluster = clusters
final_data3 = final_data2
final_data3$city = city_plt
tapply(final_data3$city, factor(final_data3$cluster), print)
table(clusters)





