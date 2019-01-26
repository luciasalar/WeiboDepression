require(reshape2)

#in this script, we will see if there's any characteristics in the low valence group
#87345 posts in 3 years

# The  valence statistics of the whole sample (n = 1629) in 3 years
# #Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# #-0.800000 -0.009214  0.113420  0.122521  0.255556  0.950000 
# 
# 285 answered the question if they have any life changes happened recently
# 279 answered the question and fill out CESD  
# 51 among 279  have a mean valence lower than the first quantile   0.18
# 57 among 279  have high CESD score ( >= 28 )    0.204
# 
# 77 among 279 reported negative life event happened recently 
# 34 among 77  have high CESD score     0.44
# 
# Among those with low valence in 3 years  (n = 51) , 13 of them have high CESD     0.26
# Among those with low valence in 3 years but no negative life events happen recently  (n = 174),  23 of them have high CESD     0.13
# Among those with low valence in 3 years and reported negative life events (n = 16),  9 of them have high CESD      0.56   
# 
# Among the 77 people reported negative life events happened recently:
# 33 reported break up with a partner  (median cesd: 29       median valence: 0.13   )
# 23 reported a family member was diagnosed with a severe illness (median cesd:  23      median valence: 0.13   )
# 12 reported unemployment (median cesd: 29       median valence:  0.014  )
# 31 reported other  (median cesd:  25      median valence: 0.15   )
# 17 reported more than two negative life events (median cesd: 26       median valence:  0.12 )


#conclusion:negative life changes and long time low valence are important factor to predict depression score, there's an accumulating 
#effect of these two factors. 

setwd("~/phd_work/depression dessertation/data")

cesd<- read.csv('~/phd_work/depression dessertation/data/depression_fix_nohis.csv')
event<- read.csv('~/phd_work/depression dessertation/data/all_users_history copy.csv')  ##70 users reported they have big change in life recent 
event <- event[,1:6] #discard the cesd score in this table
file<- read.csv('~/phd_work/depression dessertation/data/3year_liwc.csv')

#let's see the sentiment data
summary(file$sentiment)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.68750 -0.33750  0.16667  0.09758  0.60000  1.52000 

###sentiment for h and l group
senti <- file[c('userid','sentiment','day')]
cesd <- cesd[!duplicated(cesd$userid),]
senti_d <- merge(senti,cesd, by ='userid')
senti_d$cesd_sum <- rowSums(senti_d[,5:15])
c <- senti_d[senti_d$senti < 0, ]

senti_h <- senti_d[senti_d$cesd_sum > 22, ]
senti_l <- senti_d[senti_d$cesd_sum <= 22, ]
#select recent 3 months
senti_h <- senti_l
m <- senti_h[senti_h$day <= 30 , ]
summary(m$cesd_sum)
summary(m$senti)
#m = 0.175, [-0.33, 0.60] 1st/3rd
oneyear <- senti_h[senti_h$day <= 365 , ]
summary(oneyear$cesd_sum)
summary(oneyear$senti)
#m = 0.175, [-0.33, 0.59] 1st/3rd
three<- senti_h[senti_h$day <=  1095 , ]
summary(three$cesd_sum)
summary(three$senti)
#m = 0.16, [-0.34, 0.60] 1st/3rd


################################low valence group
userSenti <- aggregate(file$sentiment, list(file$userid), sum)
weiboCount <- aggregate(file$userid, list(file$userid), length)
weiboCount2 <- cbind(userSenti,weiboCount)
colnames(weiboCount2) <- c('userid','SentiSum','userid2','count')
weiboCount2$userid2 <- NULL

weiboCount2$Mvalence <- weiboCount2$SentiSum/weiboCount2$count

summary(weiboCount2$Mvalence)

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.800000 -0.009214  0.113420  0.122521  0.255556  0.950000 

#here we have users with low valence 
lowValence <- weiboCount2[weiboCount2$Mvalence < -0.009, ]

#among people who fill out the life change questionnaire how many of them have low valence 
#let's see if they have any life changes happen
lowValenceEvent <- merge(lowValence,event, by.x = 'userid', by.y = 'user_id')
##51 answer big changes in life questions have lower valence (1st Quantile)

#let's see their cesd score
lowValenceCESD <- merge(lowValence,cesd, by.x = 'userid', by.y = 'userid')
lowValenceCESD$cesd_sum <- rowSums(lowValenceCESD[,6:16])
summary(lowValenceCESD$cesd_sum)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.00   19.00   23.00   24.18   28.00   44.00 

dep <- lowValenceCESD[lowValenceCESD$cesd_sum > 22, ] 

#among those with life changes, do those with low valence tend to have high cesd
#see the cesd of those with life changes, they dont have particular high cesd
eventCESD <- merge(event,cesd, by.x = 'user_id', by.y = 'userid')
eventCESD <- eventCESD[!duplicated(eventCESD$user_id), ]
eventCESD$cesd_sum <- rowSums(eventCESD[,8:18])
sd(eventCESD$cesd_sum, na.rm=TRUE)


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 11.00   19.00   22.00   23.99   27.00   47.00       1 

#let's merge this with valence 
# 279 people fill out the event scale 
eventCesdValence <- merge(eventCESD,weiboCount2, by.x = 'user_id', by.y = 'userid')

#among the whole sample how many people have high depression score
dep <- eventCesdValence[eventCesdValence$cesd_sum > 22, ]  #n =57  / 137
57/251  # 0.204 of them have high depression score 
119/251  #0.474

#among those with big changes in life, how many of them have high depression score
change <- eventCesdValence[eventCesdValence$Big_change == 1, ]
dep2 <- change[change$cesd_sum > 22, ] #55
34/77 # 0.44 of them have high depression score 
53/77 #0.68.8


Nochange <- eventCesdValence[eventCesdValence$Big_change == 2, ]
d <- Nochange[Nochange$cesd_sum > 22, ] 
66/174

#among those with low valence all time, how many of them have high depression score 
lowValence2 <- eventCesdValence[eventCesdValence$Mvalence < -0.009, ] #n = 50 have lower than mean valence
dep3 <- lowValence2[lowValence2$cesd_sum >= 22, ] # n = 13 of them have depression #30
13/50  #0.26 of them have high depression score   
30/50 ##0.6


highValence2 <- eventCesdValence[eventCesdValence$Mvalence >= -0.009, ] #n = 50 have lower than mean valence
dep6 <- highValence2[highValence2$cesd_sum >= 22, ] 
108/201

changeH <- highValence2[highValence2$Big_change == 1, ]
dep7 <- changeH[changeH$cesd_sum > 22, ] 
42/61

#among those with low valence all time, how many of them have high depression score even when no changes happen in life?
Nochange <- eventCesdValence[eventCesdValence$Big_change == 2, ] #174
lowValenceNoChange <- Nochange[Nochange$Mvalence < -0.009, ] #34
depp <- lowValenceNoChange[lowValenceNoChange$cesd_sum >= 22, ] #23  #19
19/34# 0.67 of them have high depression score  #0.558

highValenceNoChange <- Nochange[Nochange$Mvalence >= -0.009, ] #140
de <- Nochange[Nochange$cesd_sum >= 22, ] #23  #82
23/174
82/140  #0.58

#among those with low valence all time and  big change in life, how many of them have high depression score after big changes happen?
lowValence3 <- change[change$Mvalence < -0.009, ] #n= 16
dep4 <- lowValence3[lowValence3$cesd_sum >= 22, ] #n = 11
11/16  #0.5625 of them have high depression score  #0.6875


#conclusion:negative life changes and long time low valence are important factor to predict depression score, there's an accumulating 
#effect of these two factors. 
#change_L:  break up with a partner
#change_R:  a family member was diagnosed with a severe illness
#change_R:  unemployment
#change_O: other 

#now let's see the 77 people reported life changes
change_L <- change[change$change_L == 1, ]  # 33
change_R <- change[change$change_R == 1, ]  # 23
change_J <- change[change$change_J == 1, ]  # 12
change_O <- change[change$change_Other == 1, ]  # 31
#people with more than two changes    # 17
change$changeT <- rowSums(change[,3:6])
Twochange <- change[change$changeT >= 2, ]

#cesd score
summary(change_L$cesd_sum)  # median 29
summary(change_R$cesd_sum)  #median 23
summary(change_J$cesd_sum)  #median  29
summary(change_O$cesd_sum)  #median 25
summary(Twochange$cesd_sum)  #median 26

#people with L and J are more likely to have high CESD 
#median valence in the sample is 0.11

summary(change_L$Mvalence)  # median 0.13
summary(change_R$Mvalence)  #median 0.13
summary(change_J$Mvalence)  #median  0.014  
summary(change_O$Mvalence)  #median 0.15
summary(Twochange$Mvalence)  #median 0.12

#people with with unemployemnt has the lowest valence all time and highest CESD
#these people are struggling with a predisposing factor # need to see the qualitative data


##get posts from low valence group who has big changes and high CES-D
file2<- read.csv('~/phd_work/depression dessertation/data/sub_3year.csv')
posts <- file2[c('userid','weibo','time','day','sentiment')] 
sample <- merge(posts, dep4, by.x ='userid', by.y='user_id') #(n = 205)
write.csv(sample,'qualitative.csv')

#get posts from recent 6 months
#among the four events, what event people tend to show more sign #7 ppl indicated, show signs of rumination 
sample2 <- merge(posts, change_L, by.x ='userid', by.y='user_id') #(n = 1194)
de <- change_L[change_L$cesd_sum > 27, ]  # 18 shows high cesd, 4 of them show signs of rumination 
sample2 <- sample2[sample2$day <= 180, ] 
write.csv(sample2,'qualitative_change_L.csv')
#social media data 


#unemployment #no one shows sign in weibo
sample2 <- merge(posts, change_J, by.x ='userid', by.y='user_id') #(n = 1194)
sample2 <- sample2[sample2$day <= 180, ] 
write.csv(sample2,'qualitative_change_J.csv')

#unemployment #no one shows sign in weibo  only 2 ppl indicate
sample2 <- merge(posts, change_R, by.x ='userid', by.y='user_id') #(n = 1194)
sample2 <- sample2[sample2$day <= 180, ] 
write.csv(sample2,'qualitative_change_R.csv')




