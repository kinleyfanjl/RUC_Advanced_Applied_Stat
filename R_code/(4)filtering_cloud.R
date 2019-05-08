library('tm')
library('topicmodels')
library('NLP')
library('BBmisc')
library('cidian')
library('stingi')
library('pbapply')
library('dbscan') 
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
