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





