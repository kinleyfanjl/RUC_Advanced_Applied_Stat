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


