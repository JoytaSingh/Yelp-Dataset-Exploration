library(ggplot2)
library(dplyr)
dataGroupByStateStar <- df_business %>% 
  filter(state != '') %>% mutate(tsum = n()) %>% 
  group_by(state, stars) 
dataForTableByStateStar <- dataGroupByStateStar %>% group_by(state) %>%
  summarise(total_business = n(), total_reviews = sum(review_count), avg_rating = round(mean(stars), 2)) %>% arrange(desc(avg_rating))
install.packages("pander")
library(pander)

panderOptions("digits", 3)
pander(dataForTableByStateStar)

ratings_and_users <- df_reviews %>% group_by(stars) %>% count()
library(RColorBrewer)
ggplot(data=ratings_and_users, axes(x=cut(stars,c(0,0.9,1.9,2.9,3.9,5)), y=n,fill=cut(stars, c(0,0.9,1.9,2.9,3.9,5)))) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "YlOrRd") + labs(title = "Distribution of Ratings by Users", y = "Count of Users", 
                                                                           x = "Star Category", fill = "Star Category") + theme(legend.position="none") +
  scale_x_discrete(labels=c("1.5","2.5","3.5","4.5"))                                                                                                                        

install.packages("tm")
library("tm")

install.packages("wordcloud")
library("wordcloud")

library(stringr)        #This package is used for string manipulation functions
library(dplyr)          #This package is used for data manipulation tasks
install.packages("tidyr")
library(tidyr)          #This package is used for data manipulation tasks
library(data.table)     #This package is used to access the function fread which is a better/faster way to read large data
library(wordcloud)      #This package is used to generate word clouds
library(tm)             #This is a text mining package used in the process of generating word clouds
install.packages("RWeka")
library(RWeka)          # This package is used to generate Bigramws and Trigrams
library(ggplot2)        #This package is used for visualizations (chart/graph plotting functions)
install.packages("ggmap")
library(ggmap)          #This package is used for map functions 
library(maps)           #This package is used for map functions
install.packages("leaflet")
library(leaflet)        #This package is used for plotting maps
library(knitr)

wordcloud (df_reviews$text, scale=c(5,0.5), max.words=10000, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
