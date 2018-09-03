install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggpubr")
install.packages("car")
install.packages("moments")
install.packages("reshape2")
library(reshape2)
library(moments)
library(car)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggpubr)
##################################################################################################################################
setwd("C:/Users/Michelle Chen/Documents/Michelle School Work/Undergraduate School/Fourth Year!/First Semester/Marketing Research")
a<-read.csv("Survey Data Cleaned.csv",header=TRUE)

###########
#Functions#
###########


Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

#########################
#Descriptive statistics:#
#########################

#Dim
dim(a)
#173 x 43



###############
##Female/Male##
###############

#Male : Female ratio
length(which(a$Q16=="Male")) #53
length(which(a$Q16=="Female")) #119
length(which(a$Q16=="Wish not to disclose")) #1

53/173
119/173


##########################
#Breakdown of school year#
##########################

x<-factor(a$Q13)
print(levels(x))
x<-factor(x,levels(x)[c(1,3,4,2)])
print(levels(x))
summary(x)/173
barplot(summary(x))

# First year Second year  Third year Fourth year 
# 0.4624277   0.1156069   0.1849711   0.2369942 
#Majority are first years. 

80/(80+20+32+41) #46.243% are first year

x<-factor(b$Q13)
print(levels(x))
x<-factor(x,levels(x)[c(1,3,4,2)])
print(levels(x))

anova_year<- lm(a$Q4..SatisfiedCurrent~factor(a$Q13), data=a)
summary(anova_year)
anova(anova_year)
confint(anova_year)

#Satisfaction does not differ between school year

confint(mean(a$Q4..SatisfiedCurrent))
summary(anova_gender)



################################################
#Frequency of get dessert, and existing options#
################################################
#############################################
#Overall satisfaction with current offerings#
#############################################


####Histogram for frequency of visit
ggplot(data.frame(a), aes(x=a$Q3.Timespermonth)) +
  geom_bar(aes(fill=factor(a$Q3.Timespermonth))) + labs(x="Visits Per Month", y="Total Count", title="Visits Per Month to Dessert Places on the Corner")+theme(text = element_text(size=20))


skewness(a$Q4..SatisfiedCurrent) #<|2|
kurtosis(a$Q4..SatisfiedCurrent) #slightly greater than |2|, platykurtic, les than 3
freq.aov <-aov(a$Q4..SatisfiedCurrent~a$Q3.Timespermonth, data=a)
summary(freq.aov) #people do have different satifaction given their visits  

t.test(a$Q4..SatisfiedCurrent) #@95% confidence level
# t = 41.482, df = 172, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   2.917807 3.209360
# sample estimates:
#   mean of x 
# 3.063584 

table(a$Q3.Timespermonth,a$Q4..SatisfiedCurrent)
demand<-aov(a$Q4..SatisfiedCurrent~a$Q3.Timespermonth)
TukeyHSD(demand)

t.test(a$Q7_RotatingMenuSocialMedia,mu=5, alternative="greater")
# One Sample t-test
# 
# data:  a$Q7_RotatingMenuSocialMedia
# t = 3.5998, df = 172, p-value = 0.0002082
# alternative hypothesis: true mean is greater than 5
# 95 percent confidence interval:
#   5.190614      Inf
# sample estimates:
#   mean of x 
# 5.352601 

b<-a




###################
#Times per month###
###################
summary(b$Q3.Timespermonth)




####################
#Social Media Usage#
####################

d<-str_split(b$Q9,",")
d[[1]]

count_facebook <- NULL
count_insta <- NULL
count_snap <- NULL
count_twitter <- NULL
count_none <- NULL
count_other <- NULL

dnew<- NULL
for(i in 1:174){
   dnew[i]<-length(d[[i]])
   if("Facebook" %in% d[[i]])
   {
    count_facebook[i]<-1
   }
   if("Instagram" %in% d[[i]])
   {
     count_insta[i]<-1
   }
   if("Snapchat" %in% d[[i]]){
     count_snap[i]<-1
   }
   if("Twitter" %in% d[[i]]){
     count_twitter[i]<-1
   }
   if("None" %in% d[[i]]){
     count_none[i]<-1
   }
   if("Other" %in% d[[i]]){
     count_other[i]<-1
   }
  i<-i+1
}

length(which(count_facebook==1))
length(which(count_insta==1))
length(which(count_snap==1))
length(which(count_none==1))
length(which(count_twitter==1))
length(which(count_other==1))

length(count_facebook)

which(count_facebook!=1)
length(b$Q16)


b_sm<-b %>% mutate(count_socialmedia=dnew)
b_sm
summary(dnew)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       2       2       3       5 

f<-cbind(b,dnew)
f
b
f %>% group_by(Q13) %>% filter(Q16=="Female") %>%
  summarise(avg_socialmediausage=mean(dnew),
            buy_socialmedia= Mode(Q8.BuySocialMedia))


f %>% group_by(Q13) %>% filter(Q16=="Male") %>%
  summarise(avg_socialmediausage=mean(dnew),
            buy_socialmedia= Mode(Q8.BuySocialMedia)[1])

g<-f %>% group_by(Q13) %>% filter(Q16=="Male") %>%
  summarise(avg_socialmediausage=mean(dnew),
            buy_socialmedia= Mode(Q8.BuySocialMedia)[2])

colnames(g)<-c("Year", "# of Social Media", "Buy Social Media") 
g

female_sm <- b_sm$count_socialmedia[b$Q16=="Female"]
male_sm <-b_sm$count_socialmedia[b$Q16=="Male"]


## Conduct the hypothesis test
t.test(female_sm, male_sm, mu=0, alternative="two.sided")

female_sm <- b$Q8.BuySocialMedia[b$Q16=="Female"]
summary(female_sm)
male_sm <-b$Q8.BuySocialMedia[b$Q16=="Male"]
summary(male_sm)
tablea<-table(b$Q8.BuySocialMedia,b_sm$count_socialmedia)

length(b_sm$count_socialmedia)

## Conduct the hypothesis test
t.test(female_sm, male_sm, mu=0, alternative="two.sided")

chisq.test(b$Q16,b$Q8.BuySocialMedia)

fisher.test(b_sm$count_socialmedia,b$Q8.BuySocialMedia,simulate.p.value=TRUE,B=1e7)
#https://stackoverflow.com/questions/17052639/fisher-test-error-ldstp-is-too-small

# Fisher's Exact Test for Count Data with simulated p-value (based
# 	on 1e+07 replicates)
# 
# data:  b_sm$count_socialmedia and b$Q8.BuySocialMedia
# p-value = 0.002652
# alternative hypothesis: two.sided

ggplot(b,aes(x=b_sm$count_socialmedia))+geom_bar(aes(fill=b$Q8.BuySocialMedia)) + labs(x= "# Social Media Platforms Used", y="Frequency", title="Purchase Behavior on Number of SM Platforms Used")+
  theme(text = element_text(size=20))





#########
#Pricing#
#########

summary(b$Q10_1)

#Average dollar amount UVa student would spend is $5.822
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   4.000   5.000   5.822   7.000  16.000 

summary(b$Q11_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 3.000   7.000  10.000   9.543  11.000  20.000       1 
#Mean = 9.543, Median = 10

factor(b$Q13)
z<-b %>% group_by(factor(Q13))%>% filter(Q16=="Male") %>%
  summarise(avg_price = mean(Q10_1), 
            max_price = max(Q11_1), 
            total = n())
 z    
 
 z<-b %>% group_by(factor(Q13))%>% filter(Q16=="Female") %>%
   summarise(avg_price = mean(Q10_1), 
             max_price = max(Q11_1), 
             total = n())
 z    

 
##Two sample t test
range(b$Q10_1)
t.test(b$Q10_1,mu=6, alternative="greater")
female <- b$Q10_1[b$Q16=="Female"]
male <-b$Q10_1[b$Q16=="Male"]
 
 ## Conduct the hypothesis test
 t.test(female, male, mu=0, alternative="two.sided")
 
 #male and females willing to pay same avg amount
 
 female <- b$Q11_1[b$Q16=="Female"]
 male <-b$Q11_1[b$Q16=="Male"]

 ## Conduct the hypothesis test
 t.test(female, male, mu=0, alternative="two.sided")
 #at a 90% confidence, males are more likely to pay more. 

 mean(female)
 mean(male)
 
t.test(female)
t.test(male)
 
 #Female
#  `factor(Q13)` avg_price max_price total
#  <fctr>     <dbl>     <dbl> <int>
# 1    First year  5.672414        15    58
#  2   Fourth year  5.814815        15    27
#  3   Second year  5.928571        20    14
#  4    Third year  5.800000        16    20
#                       
 
 #Male
 #`factor(Q13)`   avg_price max_price total
 #        <fctr>     <dbl>     <dbl> <int>
 #1    First year  5.761905        20    21
 # 2   Fourth year  6.000000        NA    15
 # 3   Second year  7.666667        20     6
 # 4    Third year  5.583333        15    12
 #                      
   




###############################
#Bootstrapping - average price#
###############################
 
## Find the sample mean
mean(a$Q10_1) #9.5433


## Determine the number of bootstrap samples
B<-10000

## Draw the bootstrap samples
boot_samp <- replicate(B, sample(a$Q11_1, replace=T))
boot_samp[,1:5]

## Determine the sample mean from each bootstrap sample
boot_means <- apply(boot_samp,2,mean)

## Estimate the center of the sampling distribution
mean(boot_means) #9.54088

## Determine the 95% bootstrap confidence interval 
boot_means_sort <- sort(boot_means)
lbp <- B*0.025
ubp <- B*0.975
boot_ci <- boot_means_sort[c(lbp,ubp)]
boot_ci
#[1]  9.040462 10.052023


## Determine the median of the sampled data
median(a$Q10_1) #10

## Determine the sample median from each bootstrap sample
boot_meds <- apply(boot_samp,2,median)

mean(boot_meds)
#[1] 2.678907

## Determine the 95% bootstrap confidence interval
boot_meds_sort <- sort(boot_meds)
boot_med_ci <- boot_meds_sort[c(lbp,ubp)]
boot_med_ci





##########################
#Boostrapping - Max price#
##########################
 
 ## Find the sample mean
 mean(a$Q11_1) #9.5433
summary(a$Q11_1)
t.test(a$Q11_1)#Confidence interval

 ## Determine the number of bootstrap samples
 B<-10000
 
 ## Draw the bootstrap samples
 boot_samp <- replicate(B, sample(a$Q11_1, replace=T))
 boot_samp[,1:5]
 
 ## Determine the sample mean from each bootstrap sample
 boot_means <- apply(boot_samp,2,mean)
 
 ## Estimate the center of the sampling distribution
 mean(boot_means) #9.54088
 
 ## Determine the 95% bootstrap confidence interval 
 boot_means_sort <- sort(boot_means)
 lbp <- B*0.025
 ubp <- B*0.975
 boot_ci <- boot_means_sort[c(lbp,ubp)]
 boot_ci
 #[1]  9.040462 10.052023
 

 ## Determine the median of the sampled data
 median(a$Q11_1) #10
 
 ## Determine the sample median from each bootstrap sample
 boot_meds <- apply(boot_samp,2,median)
 
 mean(boot_meds)
 #[1] 2.678907
 
 ## Determine the 95% bootstrap confidence interval
 boot_meds_sort <- sort(boot_meds)
 boot_med_ci <- boot_meds_sort[c(lbp,ubp)]
 boot_med_ci
 
 #[1] 1.77 4.30
   
 
 
 
 
 
 
#############                      
#Product mix#
#############


wilcox.test(b$Q2_Appearance, b$Q2_Price, alternative="less", exact=FALSE)
 # Wilcoxon rank sum test with continuity correction
 # 
 # data:  b$Q2_Taste and b$Q2_Appearance
 # W = 27482, p-value < 2.2e-16
 # alternative hypothesis: true location shift is greater than 0
 #
 
 # Wilcoxon rank sum test with continuity correction
 # 
 # data:  b$Q2_Appearance and b$Q2_Price
 # W = 13434, p-value = 0.08067
 # alternative hypothesis: true location shift is not equal to 0
#--------------------------------

 summary(factor(a$Q6_IntDessert))
 summary(factor(a$Q6_Vegan))
 summary(factor(a$Q6_GlutenFree))
 summary(factor(a$Q6_Hangout))
 summary(factor(a$Q6_ConvenientLocation))
 summary(factor(a$Q6_LateOpenHours))
 summary(factor(a$Q6_Delivery))
 summary(factor(a$Q6_Alcohol))
 summary(a$Q6_LateOpenHours)
t.test(a$Q6_LateOpenHours)
 
 newmatrix<-cbind(a$Q6_IntDessert,a$Q6_Vegan,a$Q6_GlutenFree,
                 a$Q6_Hangout,a$Q6_ConvenientLocation,a$Q6_LateOpenHours,
                 a$Q6_Delivery,a$Q6_Alcohol) 
apply(newmatrix,2,median)
 col.names(newmatrix)<-c("Int Dessert", "Vegan", "Gluten Free", "Hangout Area", "Convenient Location", "Late Hours", "Delivery", "Serves Alcohol")
t.test(a$Q6_Hangout)
t.test(a$Q6_ConvenientLocation)


summary(factor(a$Q2_Appearance))
summary(factor(a$Q2_Taste))
summary(factor(a$Q2_Popularity))
summary(factor(a$Q2_Unique))
summary(factor(a$Q2_Price))
summary(factor(a$Q2_Brand))


 c<-matrix(c(4,18,69,63,19,0,0,1,4,155,59,58,46,8,1,56,59,44,11,3,1,15,60,72,25,36,39,61,20,7),nrow=5, byrow =FALSE)
 c
 colnames(c)<-c("Appearance","Taste","Popularity","Unique","Price","Brand")
 row.names(c)<-c("Rank 1", "Rank 2", "Rank 3", "Rank 4", "Rank 5")
 c
 apply(c,2,median)
 
 
 
 tmp<-melt(c)
 names(tmp) <- c("Ranking", "Attribute", "Frequency")
 
 
 
 ggplot(tmp, aes(x=Attribute, y=Frequency, fill=factor(Ranking))) +
   geom_bar(stat="identity", position="dodge", colour="black") +
   scale_fill_brewer(type="qual", palette=1)+ggtitle("Frequency of Student Rankings")+theme(text = element_text(size=20))
  
 #------------------------------------------------------------
summary(factor(a$Q1_Cake))
summary(factor(a$Q1_IceCream))
summary(factor(a$Q1_FrozenYogurt))
summary(factor(a$Q1_Cookies))
summary(factor(a$Q1_Brownies))
summary(factor(a$Q1_Cheesecake))
summary(factor(a$Q1_Other))
(summary(factor(a$Q1_7_TEXT)))

a$Q1_7_TEXT[a$Q1_7_TEXT==""] <- NA
g<-na.omit(a$Q1_7_TEXT)

head(g)

ggplot(a,aes(x=a$Q1_7_TEXT))+geom_bar(aes(fill=factor(a$Q1_7_TEXT)))

c<-matrix(c(18,15,28,66,36,25,13,16,24,18,46,40,27,31,35,24,19,16,6,9,3),nrow=3, byrow =TRUE)
colnames(c)<-c("Cake","Ice Cream","Frozen Yogurt","Cookies","Brownies","Cheesecake","Other")
row.names(c)<-c("Rank 1", "Rank 2", "Rank 3")
apply(c,2,median)

barplot(c,main="Breakdown of Rankings", ylab="Frequency", 
        xlab="Top 3 Ranking of Dessert", beside=TRUE)
legend("left", c("First","Second","Third"), cex=1.3, bty="n")


tmp<-melt(c)
names(tmp) <- c("Ranking", "Dessert", "Frequency")



ggplot(tmp, aes(x=Dessert, y=Frequency, fill=factor(Ranking))) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_fill_brewer(type="qual", palette=1) + ggtitle("Frequency of Dessert Rankings")+theme(text = element_text(size=20))
 


#https://stackoverflow.com/questions/14285171/create-a-barplot-using-ggplot
#-----------------------------------------
barplot(summary(factor(a$Q6_IntDessert)))

barplot(summary(factor(a$Q6_Alcohol)))
summary(factor(a$Q6_Alcohol))
summary(a$Q6_Alcohol)
summary(a$Q6_Alcohol[which(a$Q16=="Female")])
summary(a$Q6_Alcohol[which(a$Q16=="Male")])

barplot(summary(factor(a$Q7_RotatingMenuSocialMedia)))




 

##############
#Use of Space#
##############

d<-str_split(a$Q5,",")
d[[1]]

count_hangoutwithfriends_f <- NULL
count_hangoutwithfriends_m<-NULL

count_pickupdessert_f <- NULL
count_pickupdessert_m<-NULL

count_studying_f <- NULL
count_studying_m <- NULL
# count_date <- NULL
# count_rent <- NULL
# count_other <- NULL

dnew<- NULL
for(i in 1:173){
  dnew[i]<-length(d[[i]])
  if("Picking up a dessert and leaving" %in% d[[i]] && (a$Q16[i]=="Female"))
  {
    count_pickupdessert_f[i]<-1
  }
  if("Picking up a dessert and leaving" %in% d[[i]] && (a$Q16[i]=="Male"))
  {
    count_pickupdessert_m[i]<-1
  }
  if("Hanging out with friends" %in% d[[i]]&& (a$Q16[i]=="Female"))
  {
    count_hangoutwithfriends_f[i]<-1
  }
  if("Hanging out with friends" %in% d[[i]]&& (a$Q16[i]=="Male"))
  {
    count_hangoutwithfriends_m[i]<-1
  }
  if("Studying" %in% d[[i]]&& (a$Q16[i]=="Female"))
  {
    count_studying_f[i]<-1
  }
  if("Studying" %in% d[[i]]&& (a$Q16[i]=="Male"))
  {
    count_studying_m[i]<-1
  }
  
  i<-i+1
}

f<-length(which(count_pickupdessert_f==1)) / 119
m<-length(which(count_pickupdessert_m==1)) / 53

f<-length(which(count_hangoutwithfriends_f==1))/119
m<-length(which(count_hangoutwithfriends_m==1))/53

f<-length(which(count_studying_f==1))/119
m<-length(which(count_studying_m==1))/53
phat <- c(f, m)
n <- c(119, 53)
x <- phat*n

z2.prop <- prop.test(x, n, alternative="two.sided", correct=FALSE)
z2.prop

## Determine test statistic
comb.phat <- sum(x)/sum(n)
z <- diff(phat)/sqrt(comb.phat*(1-comb.phat)*(1/n[1]+1/n[2]))
z

## Determine p-value
p.value <- 2*(1-pnorm(z)) 
p.value

## Compare test statistic and p-value to output
sqrt(z2.prop$statistic)
z2.prop$p.value


#----------------------

d<-str_split(a$Q5,",")
d[[1]]

count_hangoutwithfriends <- NULL
count_pickupdessert <- NULL
count_studying <- NULL
count_date <- NULL
count_rent <- NULL
count_other <- NULL

dnew<- NULL
for(i in 1:173){
  dnew[i]<-length(d[[i]])
  if("Picking up a dessert and leaving" %in% d[[i]])
  {
    count_pickupdessert[i]<-1
  }
  if("Hanging out with friends" %in% d[[i]])
  {
    count_hangoutwithfriends[i]<-1
  }
  if("Studying" %in% d[[i]]){
    count_studying[i]<-1
  }
  if("Date night" %in% d[[i]]){
    count_date[i]<-1
  }
  if("Renting it out for events" %in% d[[i]]){
    count_rent[i]<-1
  }
  if("Other (please specify)" %in% d[[i]]){
    count_other[i]<-1
  }
  i<-i+1
}

length(which(count_hangoutwithfriends==1))
length(which(count_pickupdessert==1))
length(which(count_studying==1))
length(which(count_date==1))
length(which(count_rent==1))
length(which(count_other==1))

f<-c(141,96,65,79,24,8)
f<-sort(f)
colnames(f)<-c("Other","Rent out for events", "Studying","Date","Pickup Dessert", "Hang out with friends")

barplot(f)
names(f) <- c("Other","Rent out for events", "Studying","Date","Pickup Dessert", "Hang out with friends")
barplot(f,xlab="Activity", ylab="Frequency",main="Dessert Shop Space Use ",cex.names=1.5,cex.axis=1.5)



