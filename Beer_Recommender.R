#Collaborative Recommender System in R

library(tidyverse)
library(recommenderlab)

#I- Load and Investigate the data:

beer <- read_csv("beer_data.csv")
View(beer)

nrow(beer) #total 475985 rows in the data
ncol(beer) #3 columns

str(beer)
summary(beer)

#Length of unique values in each column

beer %>% summarise_all(funs(n_distinct))
#There are 40308 distinct beer ids, 22498 unique profile names 

#Checking NA values

colSums(is.na(beer))

missing_data <- beer %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+coord_flip()+ theme_bw()

#We have some 100 profile names missing, we will get rid of them

beer <- beer[complete.cases(beer),]

#II- Exploratory Data Analysis

#What are the ranges of ratings

ggplot(beer, aes(factor(review_overall), fill = factor(review_overall)))+geom_bar()+theme_bw()
#The ranges of ratings vary from 0-5

#What are the mean and median ratings

ggplot(beer, aes(x ="" ,y= review_overall)) + geom_boxplot()
#From the box plot we can see that '4' is the median rating and any observation below rating 2 is treated as an outlier

#What are the percentages of observations under each rating

beer%>% group_by(review_overall) %>% summarise(total = length(review_overall)) %>% mutate(percent = (total/sum(total)*100))%>%
  ggplot(aes(factor(review_overall), percent, fill = factor(review_overall)))+geom_col()+ 
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = -0.5, size =3)+ xlab("Rating") + ylab("Percent")

#only 0.69% and 0.80% respondents have provided ratings of 1 and 1.5 respectively and around 36.73% of beers have 4 rating.

#How many reviews each beer has

beer %>% group_by(beer_beerid) %>% summarise(total = length(beer_beerid)) %>% #filter(total <= 10)%>%
  mutate(percent = (total/sum(total)*100))%>%
 ggplot(aes(total))+ geom_histogram(bins = 1000)

beer %>% group_by(beer_beerid) %>% summarise(total = length(beer_beerid)) %>% #filter(total <= 10)%>%
  mutate(average = mean(total))
 






