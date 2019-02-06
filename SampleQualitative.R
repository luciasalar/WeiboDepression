
#this script generates data for qualitative analysis

setwd("~/phd_work/depression dessertation/data")

cesd<- read.csv('~/phd_work/depression dessertation/data/depression_fix_nohis.csv')
event<- read.csv('~/phd_work/depression dessertation/data/all_users_history copy.csv')  ##70 users reported they have big change in life recent 
event <- event[,1:6] #discard the cesd score in this table
#file<- read.csv('~/phd_work/depression dessertation/data/3year_liwc.csv')
file <- read.csv('~/phd_work/depression dessertation/data/sub_360.csv')


##weibo and day 
senti <- file[c('userid','weibo','time','day')]
cesd <- cesd[!duplicated(cesd$userid),]
senti_d <- merge(senti,cesd, by ='userid')
senti_d$cesd_sum <- rowSums(senti_d[,6:16])

#recent 3 months
threeM <- senti_d[senti_d$day <= 90 , ]
threeM <- threeM[c('userid','weibo','time','day')]

#merge CESD with life events #2934 posts with CESD 
eventCESD <- merge(event,threeM, by.x = 'user_id', by.y = 'userid', all.x = T)
eventCESD <- eventCESD[!duplicated(eventCESD$user_id), ]
eventCESD$cesd_sum <- rowSums(eventCESD[,8:18])
sd(eventCESD$cesd_sum, na.rm=TRUE)

#life event types
change <- eventCESD[eventCESD$Big_change == 1, ]  #852 posts with big change
change_L <- change[change$change_L == 1, ]  # 33
change_R <- change[change$change_R == 1, ]  # 23
change_J <- change[change$change_J == 1, ] 


write.csv(change_L,'~/phd_work/depression dessertation/importantData/qualitative analysis/qualitative_change_L2.csv')
write.csv(change_R,'~/phd_work/depression dessertation/importantData/qualitative analysis/qualitative_change_R2.csv')
write.csv(change_J,'~/phd_work/depression dessertation/importantData/qualitative analysis/qualitative_change_J2.csv')



