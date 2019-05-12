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
