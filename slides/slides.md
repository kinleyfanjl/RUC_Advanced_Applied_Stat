---
title: "ETaxi Market Analysi"
author: Jinlin Fan
date: April 8th, 2019
output: 
  ioslides_presentation:
    widescreen: true
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, fig.width=6, fig.height=3) 
options(digits = 3)
```
#Lets' start

## Background
E-taxies are more and more important in people's daily choices of transportations. Not only for the convenience but also for the cheaper price and much better customer experience. Recently, lots of events come in front of public attention. Some of them are good, while some of them are bad. And for the safety of passengers, government and relevant companies publish some items and lists to control the quality of the E-taxies and drivers also. We have collected some data about different cities from respective government and companies.
  
## Data Description
- The datasets are collected from investigation about taxi markets.
- The datasets collected many features in different aspects, and many different cities. But because of lack of data, I selcect about 80 cities which contain all the feature in seperate data files.
- The variables are listed on the next page

## Data Description
```{r, include=FALSE}
library(jiebaR)
library(readxl)
library('tm')
library('topicmodels')
library('NLP')
library('wordcloud2')
library('cidian')
library('stringr')
library('pbapply')
library(stringr)
news = read_excel('news.xlsx')
cond = read_excel('admited_condi.xlsx')
```
```{r echo=TRUE}
colnames(news)
colnames(cond)
```
## Data Description
- We notice that many data in the tables are in form of chinese characteristics. Thus necessary tools such as regular expression or segmentation workers to divide long texts into short words.
- Many features are not directly involved with what we want, how to convert into handleable is a small question for further analysis.
- Lots of NAs happened in the datasets, I delete some of them and fill some of them.
  
## Data Description
- Considering many categorical , it's more reasonable to measure or order them by some intuitive methods.
- I try to combine every feature together in avoid of losing any information useful for us.


## Goal of Our Analysis
There are mainly three goals as listed below.

- Find some factor proper enough to preserve the order or other information in the variable.
- Build a model to analyze articles published on wechat platform, and find out whether there is some main topic or pattern for the content.
- Build a model to cluster cities with similar feature and give the reasons why?

#Data preprocessiong
## Quantification
We convert distribution of amout of taxies into entropy which can stand for monopolization power in a specific market.

<img src="222.png" width=256 height=256 />
```{r eval=FALSE, warning=FALSE, include=FALSE}
library(readxl)
library('tm')
library('topicmodels')
library('NLP')
library('wordcloud2')
library('cidian')
library('pbapply')
library(dplyr)
library('ggplot2')
library(stringr)
library('BBmisc')
library('cidian')
library('stringr')
library('pbapply')
library('dbscan')
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

```


## WordCloud of the news articles.
- Firstly, we should find out what result our segmentation worker gives us.
- Display top key words in all the texts.
<img src="444.png" width=256 height=256 />
```{r eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
 
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

```
-We can find that, many key words are invoved with positive or negtive setiments, which lead us to following text mining and topic models.
## Exploratory Data Analysis
- Check the illustration of our features on chinese map.
<img src="111.png" width=256 height=256 />
```{r eval=FALSE, include=FALSE}
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

# mymap = ggplot(data = fortify(map)) +
#   geom_polygon(aes(x = long, y = lat, group=group), colour = "gray",fill = "aliceblue") +
#   labs(title="The number of admitted licenses for E-Taxi companies",x="",y="") +
#   geom_point(data=final_data,aes(x=lon,y=lat,colour=entropy,size = No.lice ),alpha = 0.8)+ 
#   scale_color_gradientn(colours = rainbow(5)) +
#   geom_text(aes(x=lon+1,y=lat-0.3,label=cities), data=final_data_plt_cities,color="blue4",size=2.5,family = 'STKaiti') +
#   theme(text=element_text(family="STKaiti",size=14))
# mymap
#####plot about entropy and Monop.######

p = ggplot(final_data,aes(x=total,y = entropy))
p+ geom_point(data = final_data, alpha = 0.7, aes(colour = No.lice)) + geom_smooth(lwd = 0.6,col = 'red')+
  theme(text=element_text(family="STKaiti",size=13))+
  labs(title = 'Total Taxis, Monopolization, # of admitted lincenses') +
  scale_color_gradient(low="blue", high="red")


cond = read_excel('admited_condi.xlsx')
```
Some interesting pattern in big cities and small cities are obvious. Big cities have smaller entropy values than middle or small cities.

## LDA and topic model
Latent Dirichlet Allocation is a useful model for topic modeling. Here because of the complexity in computation, details of this model are in the report. I just main result here.
<img src="666.png" width=256 height=256 />
```{r eval=FALSE, warning=FALSE, include=FALSE}
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

D <- length(documents) 
W <- length(vocab) 
doc.length <- sapply(documents,
                     function(x) sum(x[2, ]))
N <- sum(doc.length) # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)

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
```

# Summary
## Conclusions
- Is there any obvious topics in all the articles?
- Is there any typical structure in cities?
- What is the suggestion can we get for cities in different clusters?

The conclusions and further improvements are served in the final report.

#Thank You!
