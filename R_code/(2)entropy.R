library(readxl)
library(infotheo)
library(ggplot2)
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
