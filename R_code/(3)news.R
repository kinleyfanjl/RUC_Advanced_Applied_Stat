library(jiebaR)
library(readxl)
library('tm')
library('topicmodels')
library('NLP')
library('wordcloud2')
library('cidian')
library('stingi')
library('pbapply')
library(stringr)
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
