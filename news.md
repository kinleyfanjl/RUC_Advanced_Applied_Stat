```news = read_excel('news.xlsx')
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

#######Filtering irrevelant words

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
           '日')

fil_seg = filter_segment(q_seg, filter)
q_seg = fil_seg

#######LDA and topics assignments
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
G <- 4000
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
  initial = NULL, burnin = 0,
  compute.log.likelihood = TRUE
)
wordcloud2(sort(fit$topics[1,])[1:50], color = 'white',
           backgroundColor = 'grey',
           shape = 'circle',
           fontFamily = 'STKaiti')

t2 <- Sys.time()
t2 - t1 # about 7 seconds on my laptop
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

assigned_topic = lapply(fit[[1]], function(x){names(which.max(table(x)))})
news_city = news$城市
news_city_list = sapply(news_city, function(x){strsplit(x,';')})

indicator = lapply(news_city_list, function(x){
  sum(x %in% city_plt) > 0
})

news$indicator = indicator
news$topic = assigned_topic
```