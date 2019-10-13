install.packages("readr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
setwd('C:\\Users\\hp\\Desktop\\College\\Rstudio sundays\\eda yelp')
df_reviews <- read.csv("review.csv")
df_business<- read.csv("yelp_business.csv")
library("readr")
install.packages("dplyr")
View(df_business)
View(df_reviews)
nrow(df_reviews)
df_reviews
as.data.frame(names(df_business))
as.data.frame(names(df_reviews))
qplot(data=df_reviews, business_stars)
qplot(data=df_reviews, stars)

qplot(data = df_reviews, stars, useful)
nrow(df_business)
df_states<-df_reviews %>% group_by(business_city) %>% summarize(avg_stars=mean(stars), count=n())
df_states
View(df_states)
plot<-ggplot(data=df_states,aes(x=business_city, y=avg_stars))+geom_bar(stat="identity")
plot
df_states<-df_reviews %>% group_by(business_city) %>% summarize(avg_stars=mean(stars), count=n()) %>% arrange(desc(count)) %>% filter(count>2000)
df_states
df_states<-df_states %>% arrange(desc(avg_stars)) %>% mutate(business_city = factor(business_city, levels=rev(business_city)))
df_states

plot<-ggplot(data=df_states,aes(x=business_city, y=avg_stars))+geom_bar(stat="identity") + coord_flip() +geom_text(aes(label=round(avg_stars,2)),hjust=1, 
                                                                                                                   color="white")
plot
df_states<-df_reviews %>% group_by(business_city) %>% summarize(avg_stars=mean(stars), count=n(), se_mean=sd(stars)/sqrt(count)) %>% arrange(desc(count))%>%
 filter(count>2000)
df_states
plot<-ggplot(data=df_states,aes(x=business_city, y=avg_stars))+geom_bar(stat="identity") + coord_flip() +geom_text(aes(label=round(avg_stars,2)),hjust=1, 
                                                          color="white")+ geom_errorbar(aes(ymin=avg_stars - 1.96*se_mean, ymax=avg_stars + 1.96*se_mean))

library(Hmisc)
hist(df_business$stars)
qqnorm(rnorm(100,5,2),main="Normal")
qqline(rnorm(100,5,2))
qqnorm(runif(100),main="Non-normal")
qqline(runif(100))


qqnorm(df_business$stars)
qqline(df_business$stars)

x<-rnorm(50)
y<-runif(50)
ks.test(x,y)
#not normal so null hypothesis rejected
#data:  x and y
#D = 0.56, p-value = 0.0000001453
#alternative hypothesis: two-sided

shapiro.test(rexp(100))
#verified not normal