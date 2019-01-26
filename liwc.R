#this script compute the liwc score fro low and high groups
setwd("~/phd_work/depression dessertation/data")

cesd<- read.csv('~/phd_work/depression dessertation/data/depression_fix_nohis.csv')
file<- read.csv('~/phd_work/depression dessertation/data/3year_liwc.csv')

#cesd sum
cesd$cesd_sum <- rowSums(cesd[,3:13])

#merge groups with text and assign group labels
all <- merge(file,cesd, by = 'userid')
summary(all$cesd_sum,na.rm=TRUE)

high <- all[all$cesd_sum > 22, ]
high$group <- 1

low <- all[all$cesd_sum <= 22, ]
low$group <- 0


high <- high[high$day <= 365, ]
low <- low[low$day <= 365, ]
length(unique(high$userid))
length(unique(low$userid))

summary(high$cesd_sum)
sd(high$cesd_sum,na.rm=TRUE)

summary(low$cesd_sum)
sd(low$cesd_sum,na.rm=TRUE)

#bind the two groups with group labels
twoG <- rbind(high, low)



#stats functions
stats <- function (var1){
  
  summary <- round(aggregate(var1, list(twoG$group), summary),digits = 1)
  sds <- round(aggregate(var1, list(twoG$group), sd),digits = 1)
  #ktest <- kruskal.test(var1 ~ cluster_text$group, data = cluster_text) 
  #cluster_text$group2<- factor(cluster_text$group)
  #  dtest<- dunnTest(var1 ~ cluster_text$group2, method = "bonferroni")
  return (list(summary, sds))
  
}


stats_result <- lapply(twoG[8:102], function(x) stats(x))
stats_result[10:50]

save(stats_result_one, file = "liwc_3y_22.rda")


#select recent 3 months
oneY <- twoG[twoG$day <= 1096 , ]

stats1 <- function (var1){
  
  summary <- round(aggregate(var1, list(oneY$group), summary),digits = 1)
  sds <- round(aggregate(var1, list(oneY$group), sd),digits = 1)
  ktest <- kruskal.test(var1 ~ oneY$group, data = oneY) 

  return (list(summary, sds, ktest$p.value))
  
}

stats_result_one <- lapply(oneY[8:102], function(x) stats1(x))
stats_result_one[10:50]
#here we save the results to liwcData foler
save(stats_result_one, file = "liwc_3y_22.rda")

#now you can load different results from the folder
load(file = "~/phd_work/depression dessertation/data/importantData/liwcData/liwc_1m_22.rda")  #this is the one year LIWC result
load(file = "~/phd_work/depression dessertation/data/importantData/liwcData/liwc_3y_22.rda")  #this is the 3 year LIWC result

##here we can replace the object name and get the results
#get all mean
summary_result <- lapply(stats_result_one, `[`, 1)
summary_result

#get all sd
sd_result <- lapply(stats_result_one, `[`, 2)
sd_result

#get all kruskal test p values
ktest <- lapply(stats_result_one, `[`, 3)
ktest[1:2]

##see which group is significant in the ktest
sig  <- function(x) {
  if (x < 0.05) {
    return (x)
  }
  
}

significant_group <- lapply(ktest, function(x) sig(x))
#the variables below shows significant difference across groups
significant_group


##let's see if the high group has changes in 1 month and 3 years

#select recent 3 months
oneMo <- twoG[twoG$day <= 30 , ]
threeY <- twoG[twoG$day <= 1095 , ]

oneMon_h <- oneMo[oneMo$cesd_sum >= 22 , ]
threeY <- threeY[threeY$cesd_sum >= 22 , ]

length(unique(oneMon_h$userid))
length(unique(threeY$userid))


#assign time labels
threeY$group <- 0
oneMon_h$group <- 1

#high group, 1 month and three years
oneY <- rbind(threeY,oneMon_h)


stats_result <- lapply(oneY[8:102], function(x) stats1(x))
stats_result[10:50]

##here we can replace the object name and get the results
#get all mean
summary_result <- lapply(stats_result, `[`, 1)
summary_result

#get all sd
sd_result <- lapply(stats_result, `[`, 2)
sd_result

#get all kruskal test p values
ktest <- lapply(stats_result, `[`, 3)
ktest[1:2]

significant_group <- lapply(ktest, function(x) sig(x))
#the variables below shows significant difference across groups
significant_group


##let's see if the low group has changes in 1 month and 3 years
#select recent 3 months
oneMo <- twoG[twoG$day <= 30 , ]
threeY <- twoG[twoG$day <= 1095 , ]

oneMon_l <- oneMo[oneMo$cesd_sum < 22 , ]
threeY <- threeY[threeY$cesd_sum < 22 , ]

length(unique(oneMon_h$userid))
length(unique(threeY$userid))


#assign time labels
threeY$group <- 0
oneMon_h$group <- 1

#high group, 1 month and three years
oneY <- rbind(threeY,oneMon_h)


stats_result <- lapply(oneY[8:102], function(x) stats1(x))

##here we can replace the object name and get the results
#get all mean
summary_result <- lapply(stats_result, `[`, 1)

#get all sd
sd_result <- lapply(stats_result, `[`, 2)


#get all kruskal test p values
ktest <- lapply(stats_result, `[`, 3)
ktest[1:2]

significant_group <- lapply(ktest, function(x) sig(x))
#the variables below shows significant difference across groups
significant_group

































































































































































































































































































































































































































































































































































































































































































































































































































































































































































