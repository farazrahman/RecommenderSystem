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

#Removing duplicated beer reviews

beer <- beer[!duplicated(beer[c(1,2)]),]
str(beer) #Now there are 474462 unique beer id and profile combinations

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

#How many reviews each beer has.

beer %>% group_by(beer_beerid) %>% summarise(total = length(beer_beerid)) %>% #filter(total <= 10)%>%
  mutate(percent = (total/sum(total)*100))%>%
 ggplot(aes(total))+ geom_histogram(bins = 30)

beer %>% group_by(beer_beerid) %>% summarise(total = length(beer_beerid)) %>% filter(total <= 10)%>%
  mutate(average = mean(total))%>%
  ggplot(aes(x = "", y = total)) + geom_boxplot()

#Let's take 12 as minimum number of reviews a beer category has.

beer <- beer %>% group_by(beer_beerid) %>%  filter(length(beer_beerid)>= 12)
str(beer)
#Converting to realRatingMatrix

beer_df <- as.data.frame(beer)
r <- as(beer_df, "realRatingMatrix")
str(r)

# get some informtaion
dimnames(r)
rowCounts(r)
colCounts(r)
rowMeans(r)


##Data Exploration

#1-How similar are the first ten users are with each other

similar_users <- similarity(r[1:10, ],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")


#2-How similar are the first 10 beers items are with each other

similar_items <- similarity(r[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")


#3- What are the unique values of ratings?

ggplot(beer, aes(factor(review_overall), fill = factor(review_overall)))+geom_bar()+theme_bw()

#4- Visualise the rating values and notice:

#The average beer ratings

qplot(getRatings(r), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(r)) # The average beer rating is 3.858

qplot(getRatings(normalize(r, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(r, method = "Z-score"))) 


qplot(rowCounts(r), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "# of users", 
      ylab = "# of movies rated")
#Most users rate less number of beers.
#Very few users have rated more beers

#The average user ratings

mean_userRating <- beer %>% group_by(review_profilename) %>% summarise(meanRating = mean(review_overall))
  ggplot(mean_userRating, aes(x = meanRating)) + geom_histogram()

summary(mean_userRating$meanRating) #mean user rating is 3.924

#The average number of ratings given to the beers

meanRating_beers <- beer %>% group_by(beer_beerid) %>% summarise(meanRating = length(beer_beerid))

summary(meanRating_beers$meanRating) #63 ratings

#The average number of ratings given by the users

meanRating_users <- beer %>% group_by(review_profilename) %>% summarise(meanRating = length(review_profilename))

summary(meanRating_users$meanRating) #18 ratings

#5. Recommendation Models

#Divide your data into training and testing datasets

#Experiment with 'split' and 'cross-validation' evaluation schemes

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models

#Divide data into test 
scheme <- evaluationScheme(r, method = "split", train = .9,
                           k = 1, given = 2, goodRating = 4)
scheme


