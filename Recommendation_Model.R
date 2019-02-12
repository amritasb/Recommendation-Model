####################################################################################################

###
### Beer Recommendation Model for customers
###
### Author          - Amrita Bhadrannavar
###
### ASSUMPTIONS 
### Data Set File   - "beer_data.csv" is available in the working directory
###

####################################################################################################

## SETUP ##

## Load required libraries
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(doParallel)


## Register for parallel computing
cl = makeCluster(detectCores())
registerDoParallel(cl)


## Load the dataset
BeerData <- read.csv("beer_data.csv", stringsAsFactors = FALSE, na.strings=c("NA", "N/A","", " "))


####################################################################################################

## UNDERSTANDING DATA SET ##

## Check the dimensions
dim(BeerData)           
# 475984 obs with 3 variables 


## View the dataset
View(BeerData)          
# Variables available are Beer ID, User and rating provided for the beer by the user


## Check the structure of dataset
str(BeerData)
# 'data.frame':	475984 obs. of  3 variables:
# $ beer_beerid       : int  48215 52159 52159 52159 52159 58046 58046 58046 58046 58046 ...
# $ review_profilename: chr  "stcules" "oline73" "alpinebryant" "rawthar" ...
# $ review_overall    : num  3 3 3 4 3.5 4.5 4 4.5 4.5 4 ...


## Check for first few rows of dataset
head(BeerData)


## Check for data distribution
summary(BeerData)
# beer_beerid    review_profilename review_overall 
# Min.   :    3   Length:475984      Min.   :0.000  
# 1st Qu.: 1716   Class :character   1st Qu.:3.500  
# Median :13892   Mode  :character   Median :4.000  
# Mean   :21661                      Mean   :3.815  
# 3rd Qu.:39397                      3rd Qu.:4.500  
# Max.   :77317                      Max.   :5.000  


## Check for duplicate data
nrow(distinct(BeerData))    
# Total no of distinct records = 475404
# Total no of records          = 475984
# No of duplicates             =    580

## Recheck duplicated data
sum(duplicated(BeerData))       # 580 records

## 580 duplicate records will have to be eliminated


## Check for NA values
sum(is.na(BeerData$beer_beerid))          # No NA values
sum(is.na(BeerData$review_profilename))   # 100 obs have NA values
sum(is.na(BeerData$review_overall))       # No NA values

## 100 NA values in review_profilename will be replaced with "Anonymous_Reviewer" value


####################################################################################################

## DATA CLEANING & MANIPULATION ##

## Remove duplicate data
BeerData  <- BeerData[!duplicated(BeerData),]
sum(duplicated(BeerData))     # 0 duplicated data 
dim(BeerData)                 # 475404 X 3


## Update NA values in review_profilename with "Anonymous_Reviewer"
## First check if there is a reviewer with value "Anonymous_Reviewer"
BeerData[(BeerData$review_profilename == "Anonymous_Reviewer"),2]    
## No obs with serach value. So we can replace NA with "Anonymous_Reviewer"
BeerData$review_profilename[is.na(BeerData$review_profilename)] <- "Anonymous_Reviewer"


####################################################################################################

## CASE STUDY SOLUTIONS ##

## [1] Data preparation

##    1.  Choose only those beers that have at least N number of reviews
##        - Figure out an appropriate value of N using EDA; this may not have one correct answer,
##          but you shouldn't choose beers having extremely low number of ratings


## To select beers, first analysis of avg ratings of each beer will be done
## An optimal cut off value for rating will be chosen and beers with lower ratings will be 
## eliminated.
## Next reviews analysis will be done and a cut off value N (review count) will be selected
## and only beers with min N no of reviews will be selected.


## Tabulate each beer with count of reviews received and avg rating
BeerTable <- BeerData %>% group_by(beer_beerid) %>% summarise(review_count=n(), 
                                                              avg_rating=mean(review_overall))

dim(BeerTable)    # 40308 X 3    
# Total no of distinct beers = 40308

summary(BeerTable)
# Review count ranges from 1 to 980
# avg rating ranges from 0 to 5 with mean being 3.67

BeerTable[BeerTable$review_count == 980,]
# Higest review count is 980 for beer with id 2093 and avg rating 4.15


## Avg Ratings Analysis
hist(BeerTable$avg_rating, main = "Histogram of average ratings", xlab = "Avg Rating") 
# Skewed to right

## Since low rating beers would not be ideal for recommendation, we can eliminate beers with
## avg rating < 3

nrow(subset(BeerTable, avg_rating < 3)) 
# 3956 obs have rating below 3

BeerTable <- subset(BeerTable, avg_rating >=3)
hist(BeerTable$avg_rating, main = "Histogram of average ratings", xlab = "Avg Rating")
summary(BeerTable)
## Distribution looks good and we can finalise with avg rating 3+ condition


## Review count analysis
hist(BeerTable$review_count, main = "Histogram of review count", xlab = "Review Count per beer") 
# A high no of beers have a single review and also < 100 review count

## Lets create a review frequency table to understand distribution better
ReviewFreq <- BeerTable %>% group_by(review_count) %>% summarise(review_freq=n())
ReviewFreq
View(ReviewFreq)
# Review count 1-5  - No of beers 1000+
# Review count 6-24 - No of beers 100-1000


## Based on review frequency and histogram plot, initially we can choose N=25 and eliminate 
## obs with N<25
nrow(subset(BeerTable, review_count <25))
# No of beers with review count<50 = 32892

## Lets remove beers with reviews<25
BeerTable <- subset(BeerTable, review_count >= 25)
nrow(BeerTable)   # 3460 obs

summary(BeerTable)
# Avg Rating ranges from 3-4.65 with mean of 3.829


## 3460 distinct beers is still a high number, lets analyse more.
hist(BeerTable$review_count, main = "Histogram of review count", xlab = "Review Count per beer") 
boxplot(BeerTable$review_count)
summary(BeerTable$review_count)
## From histogram and summary results, its clear that high of beers have review count < 100
## Lets set N=100 and tabulate BeerTable again.

nrow(subset(BeerTable, review_count < 100))
# No of beers with review count<100 = 2476

## Lets remove beers with reviews<100
BeerTable <- subset(BeerTable, review_count >= 100)
nrow(BeerTable)   # 984 obs

## With N=100, we are left with 984 distnct beers
summary(BeerTable)
# Avg Rating ranges from 3-4.65 with mean of 3.905
# No major changes in Avg Rating range with N=100


## We will proceed with following filtering
## Avg Rating >= 3 are selected
## Beers with reviews 100 and more are considered

## Filtering beer data set with above assumptions
Beer <- merge(BeerData, BeerTable, by.x="beer_beerid",by.y="beer_beerid")

nrow(Beer)    # 216895 obs ; ~45% of origial data set selected
View(Beer)
summary(Beer)
# beer_beerid    review_profilename review_overall   review_count     avg_rating   
# Min.   :    5   Length:216546      Min.   :1.000   Min.   :100.0   Min.   :3.041  
# 1st Qu.:  779   Class :character   1st Qu.:3.500   1st Qu.:159.0   1st Qu.:3.798  
# Median : 2512   Mode  :character   Median :4.000   Median :244.0   Median :3.992  
# Mean   :12853                      Mean   :3.952   Mean   :308.8   Mean   :3.952  
# 3rd Qu.:21690                      3rd Qu.:4.500   3rd Qu.:413.0   3rd Qu.:4.130  
# Max.   :74986                      Max.   :5.000   Max.   :980.0   Max.   :4.651  


#--------------------------------------------------------------------------------------------------#

##    2.  Convert this data frame to a "realratingMatrix" before you build your collaborative
##        filtering models

## Convert data frame to realratingMatrix format 
BeerMatrix <- as(Beer[,c(2,1,3)], "realRatingMatrix")
class(BeerMatrix)
# [1] "realRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"


## Check realRatingMatrix created
dimnames(BeerMatrix)  # Beers ID & Reviewer profilename
rowCounts(BeerMatrix) # Total reviews by each user
colCounts(BeerMatrix) # Total reviews to each beer
rowMeans(BeerMatrix)  # Average rating by each user

## Coerce the matrix to a dataframe
BeerFinal <- as(BeerMatrix, "data.frame")

str(BeerFinal)
# 'data.frame':	216012 obs. of  3 variables:
# $ user  : Factor w/ 17719 levels "0110x011","01Ryan10",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ item  : Factor w/ 984 levels "10","100","1002",..: 975 31 141 146 578 827 908 955 17 25 ...
# $ rating: num  4 4 4.5 4.5 4.5 4 5 5 4 4 ...

summary(BeerFinal)
# user             item            rating      
# BuckeyeNation :   271   2093   :   977   Min.   : 1.000  
# mikesgroove   :   271   412    :   967   1st Qu.: 3.500  
# BEERchitect   :   268   1904   :   903   Median : 4.000  
# WesWes        :   257   1093   :   840   Mean   : 3.962  
# Knapp85       :   256   92     :   812   3rd Qu.: 4.500  
# TheManiacalOne:   255   4083   :   798   Max.   :11.500  
# (Other)       :214434   (Other):210715                   


####################################################################################################

## [2] Data Exploration

##    1.  Determine how similar the first ten users are with each other and visualise it

SimilarUsers <- similarity(BeerMatrix[1:10,], method = "cosine", which = "users") 

## Similarity matrix
as.matrix(SimilarUsers)

## Visualise similarity matrix
image(as.matrix(SimilarUsers), main = "User similarity")


#--------------------------------------------------------------------------------------------------#

##    2.  Compute and visualise the similarity between the first 10 beers

SimilarBeers <- similarity(BeerMatrix[,1:10], method = "cosine", which = "items")

## Similarity matrix
as.matrix(SimilarBeers)

## Visualise similarity matrix
image(as.matrix(SimilarBeers), main = "Beer similarity")


#--------------------------------------------------------------------------------------------------#

##    3.  What are the unique values of ratings?

sort(unique(getRatings(BeerMatrix)))
# Unique values of ratings are :
# 1.0  1.5  2.0  2.5  3.0  3.5  4.0  4.5  5.0  5.5  
# 6.0  6.5  7.0  7.5  8.0  8.5  9.0  9.5 10.5 11.5

length(unique(getRatings(BeerMatrix)))
# 20 unique values of ratings ranging from 1 to 11.5


#--------------------------------------------------------------------------------------------------#

##    4.  Visualise the rating values and notice:

##        - The average beer ratings
qplot(getRatings(BeerMatrix), binwidth = 1, 
      main = "Average beer ratings", 
      xlab = "Rating")
# Skewed to the left. Plotting normalised graph
qplot(getRatings(normalize(BeerMatrix, method = "Z-score")), 
      main = "Normalized Beer Ratings", 
      xlab = "Rating")

summary(getRatings(BeerMatrix)) # Average beer ratings = 3.962
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.500   4.000   3.962   4.500  11.500 



##        - The average user ratings
qplot(rowMeans(BeerMatrix), binwidth = 1, 
      main = "Average User Rating", 
      xlab = "Average ratings")

summary(rowMeans(BeerMatrix)) # Average User Ratings = 4.018
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.786   4.000   4.018   4.375   6.750 



##        - The average number of ratings given to the beers
qplot(colCounts(BeerMatrix), binwidth = 10, 
      main = "Average number of ratings given", 
      xlab = "Beer", 
      ylab = "# of ratings")

summary(colCounts(BeerMatrix)) # Average number of ratings given to beer = 219.5
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 99.0   125.0   171.0   219.5   254.0   977.0 



##        - The average number of ratings given by the users
qplot(rowCounts(BeerMatrix), binwidth = 10, 
      main = "Average Number of Ratings Given by Users", 
      xlab = "User", 
      ylab = "# of beer rated")

summary(rowCounts(BeerMatrix)) # Average Number of Ratings Given by Users is 12.19
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    3.00   12.19   10.00  271.00 


####################################################################################################

## [3] Recommendation Models

##    1.  Divide your data into training and testing datasets
##        - Experiment with 'split' and 'cross-validation' evaluation schemes

## Split evaluation Scheme with train/test(90/10) using split without cross validation & 
## goodRating as 4  
SchemeSplit <- evaluationScheme(BeerMatrix, method = "split", train = .9, k = 1, given = -1, 
                                goodRating = 4)
SchemeSplit
# Evaluation scheme using all-but-1 items
# Method: 'split' with 1 run(s).
# Training set proportion: 0.900
# Good ratings: >=4.000000
# Data set: 17719 x 984 rating matrix of class 'realRatingMatrix' with 216012 ratings.


## Cross-validation evaluation schemes using 5 fold cross-validation & goodRating as 4 
SchemeCrossVal <- evaluationScheme(BeerMatrix, method = "cross-validation", k = 5, given = -1, 
                                   goodRating = 4)
SchemeCrossVal
# Evaluation scheme using all-but-1 items
# Method: 'cross-validation' with 5 run(s).
# Good ratings: >=4.000000
# Data set: 17719 x 984 rating matrix of class 'realRatingMatrix' with 216012 ratings.



#--------------------------------------------------------------------------------------------------#

##    2.  Build IBCF and UBCF models

algorithms <- list( "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                                   method="Cosine",
                                                                   nn=50)),
                    "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))


## Evaluating SchemeSplit algorithms & predicting next n beers
start_time <- Sys.time()
ResultsSplitScheme <- evaluate(SchemeSplit, algorithms, n=c(1, 3, 5, 10, 15, 20))
end_time <- Sys.time()
end_time - start_time

class(ResultsSplitScheme)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"


## Evaluating SchemeCrossVal algorithms & predicting next n beers
start_time <- Sys.time()
ResultsCrossValScheme <- evaluate(SchemeCrossVal, algorithms, n=c(1, 3, 5, 10, 15, 20))
end_time <- Sys.time()
end_time - start_time

class(ResultsCrossValScheme)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"


#--------------------------------------------------------------------------------------------------#
##    3.  Compare the performance of the two models and suggest the one that should be deployed
##        - Plot the ROC curves for UBCF and IBCF and compare them

## Drawing ROC curve for Split Scheme
plot(ResultsSplitScheme, annotate = 1:4, legend="topleft")

## Drawing ROC curve for Cross validation Scheme
plot(ResultsCrossValScheme, annotate = 1:4, legend="topleft")

## Based on the above plots, UBCF seems to get better then IBCF especially with higher values of n
## Hence UBCF based model will be considered


#--------------------------------------------------------------------------------------------------#

##    4.  Give the names of the top 5 beers that you would recommend to the users "cokes", "genog"
##        & "giblet"

## Creation of recommender model based on ubcf 
RecModelUBCF <- Recommender(BeerMatrix, method = "UBCF") 
RecModelUBCF
# Recommender of type 'UBCF' for 'realRatingMatrix' 
# learned using 17719 users.


## Recommendation for user "cokes"
UserCokesRecomm <- predict(RecModelUBCF, BeerMatrix['cokes'], n=5)
as(UserCokesRecomm, "list")
## IDs of recommended beers
# "1867"  "42533" "6108"  "1212"  "48434"


## Recommendation for user "genog"
UserGenogRecomm <- predict(RecModelUBCF, BeerMatrix['genog'], n=5)
as(UserGenogRecomm, "list")
## IDs of recommended beers
# "51480" "2751"  "645"   "131"   "13741"


## Recommendation for user "giblet"
UserGibletRecomm <- predict(RecModelUBCF, BeerMatrix['giblet'], n=5)
as(UserGibletRecomm, "list")
## IDs of recommended beers
# "731"  "226"  "4083" "7971" "459"


####################################################################################################

## CLEANUP BEFORE EXIT ##

## Close the cluster of parallel computing
stopCluster(cl)

####################################################################################################
