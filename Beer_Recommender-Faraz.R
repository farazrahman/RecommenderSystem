#Beer Recommendation System- Author-Faraz

#Collaborative Recommender System in R

library(tidyverse)
library(recommenderlab)

#I- Load and Investigate the data:

beer <- read.csv("beer_data.csv")
View(beer)

nrow(beer) #total 475984 rows in the data
ncol(beer) #3 columns


str(beer)
summary(beer)

#Length of unique values in each column

beer %>% summarise_all(funs(n_distinct))
#There are 40308 distinct beer ids, 22498 unique profile names and 9 unique reviews

#Checking NA values

colSums(is.na(beer))

beer <- beer[complete.cases(beer),]

#Removing duplicated beer reviews

beer <- beer[!duplicated(beer[c(1,2)]),]
str(beer) #Now there are 474560 unique beer id and profile combinations

#II- Exploratory Data Analysis

#What are the ranges of ratings

ggplot(beer, aes(factor(review_overall), fill = factor(review_overall)))+geom_bar()+theme_bw()
#The ranges of ratings vary from 0-5

#What are the mean and median ratings

ggplot(beer, aes(x ="" ,y= review_overall)) + geom_boxplot()
#From the box plot we can see that '4' is the median rating and any observation below rating 2 is treated as an outlier
#Mean rating is 3.815


#What are the percentages of observations under each rating

beer%>% group_by(review_overall) %>% summarise(total = length(review_overall)) %>% mutate(percent = (total/sum(total)*100))%>%
  ggplot(aes(factor(review_overall), percent, fill = factor(review_overall)))+geom_col()+ 
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = -0.5, size =3)+ xlab("Rating") + ylab("Percent")

#only 0.69% and 0.80% respondents have provided ratings of 1 and 1.5 respectively and around 36.72% of beers have 4 rating.

#How many reviews each beer has.

beer_rev <- beer %>% group_by(beer_beerid) %>% summarise(total = length(beer_beerid)) %>% 
  arrange(desc(total))

#beer_id 2093 has maximum number of reviews which is 977

summary(beer_rev$total) #Min review is 1, Median is 2, Mean is 11.77 ~ 12, Max is 977 reviews.

#What is the mean and maximum number of reviews given by each user

user_rev <- beer %>% group_by(review_profilename) %>% summarise(total_rev = length(review_profilename))

summary(user_rev$total_rev) #Mean user review is 21, Min is 1 and Max is 1842 reviews.

#In order to do our analysis, we will consider those beers that have some considerable amount of reviews
#We will not consider those reviews that have very less number of reviews.
#Let's take 100 as minimum number of reviews a beer category must have
#(which is around 10% of the maximum review for the highest number of beers),

beer <- beer %>% group_by(beer_beerid) %>%  filter(length(beer_beerid)>=100)
str(beer)
summary(beer)


#Also let's remove users who have provided less than 21 ratings which is the mean number of user ratings.

beer <- beer %>% group_by(review_profilename) %>% filter(length(review_profilename) > 21)
beer_df <- beer[,c("review_profilename", "beer_beerid", "review_overall")]
beer_df <- as.data.frame(beer_df, row.names = TRUE)
View(beer_df)

#Converting to realRatingMatrix

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

#User 2 & 9 have some similarity


#2-How similar are the first 10 beers items are with each other

similar_items <- similarity(r[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

#3- What are the unique values of ratings?

ggplot(beer_df, aes(factor(review_overall), fill = factor(review_overall)))+geom_bar()+theme_bw()
#There are total 9 ratings

#4- Visualise the rating values and notice:

#The average beer ratings

qplot(getRatings(r), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(r)) # The average beer rating is 3.90

qplot(getRatings(normalize(r, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(r, method = "Z-score"))) 


qplot(rowCounts(r), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "# of users", 
      ylab = "# of beers rated")
#Most users rate less number of beers.
#Very few users have rated more beers

#The average user ratings

mean_userRating <- beer %>% group_by(review_profilename) %>% summarise(avg = mean(review_overall))
  ggplot(mean_userRating, aes(x =avg)) + geom_histogram()

summary(mean_userRating$avg) #mean user rating is 3.91

#The average number of ratings given to the beers

meanRating_beers <- beer %>% group_by(beer_beerid) %>% summarise(total = length(beer_beerid))
ggplot(meanRating_beers, aes(x =total)) + geom_histogram()


summary(meanRating_beers$total) #157 ratings

#The average number of ratings given by the users

meanRating_users <- beer %>% group_by(review_profilename) %>% summarise(meanRating = length(review_profilename))

summary(meanRating_users$meanRating) #60 ratings on an average

#5. Recommendation Models

#Divide your data into training and testing datasets

#Experiment with 'split' and 'cross-validation' evaluation schemes


#A- Split Evaluation Scheme
schemeSplit <- evaluationScheme(r, method = "split", train = .9,
                           k = 1, given = 2, goodRating = 4)
schemeSplit



algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next n movies
resultsA <- evaluate(schemeSplit, algorithms, n=c(1, 3, 5, 10, 15, 20))


# Draw ROC curve
plot(resultsA, annotate = 1:4, legend="topleft")


#B- Cross Validation Evaluation Scheme
schemeCV <- evaluationScheme(r, method = "cross-validation", 
                                k = 5, given = -1, goodRating = 4)
schemeCV



algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next n movies
resultsB <- evaluate(schemeCV, algorithms, n=c(1, 3, 5, 10, 15, 20))


# Draw ROC curve
plot(resultsB, annotate = 1:4, legend="topleft")


#From both the ROC plots from schemeA which is Split Scheme and schemeB i.e. cross validation scheme, we found that UBCF 
#performs better than IBCF in both the plots. Hence, we will consider UBCF to recommend beers for our given users here.

#4- Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"


RS <- Recommender(r, method = "UBCF")


#Recommendation for user "cokes"


cokes <- predict(RS, r["cokes",], n=5)
as(cokes, "list")

#$cokes
#[1] "645"   "41815" "48434" "22227" "2671" 


#Recommendation for user "genog"


genog <- predict(RS, r["genog",], n=5)
as(genog, "list")

#$genog
#[1] "56973" "45897" "54904" "7971"  "2758" 


#Recommendation for user "giblet"


giblet <- predict(RS, r["giblet",], n=5)
as(giblet, "list")


#$giblet
#[1] "19960" "11757" "141"   "582"   "599" 


#####################END###############################