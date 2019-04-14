#Website: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
rm(list = ls())
rawdata <- read.csv("vid_games.csv",
                    header = TRUE, sep = ",")
names(rawdata) <- tolower(names(rawdata))
str(rawdata)
# NameName of the game
# PlatformConsole on which the game is running
# Year_of_ReleaseYear of the game released
# GenreGame's category
# PublisherPublisher
# NA_SalesGame sales in North America (in millions of units)
# EU_SalesGame sales in the European Union (in millions of units)
# JP_SalesGame sales in Japan (in millions of units)
# Other_SalesGame sales in the rest of the world, i.e. Africa, Asia excluding Japan, 
# Australia, Europe excluding the E.U. and South America (in millions of units)
# Global_SalesTotal sales in the world (in millions of units)
# Critic_ScoreAggregate score compiled by Metacritic staff
# Critic_CountThe number of critics used in coming up with the Critic_score
# User_ScoreScore by Metacritic's subscribers
# User_CountNumber of users who gave the user_score
# DeveloperParty responsible for creating the game
# RatingThe ESRB ratings (E.g. Everyone, Teen, Adults Only..etc)

#Best Model to predict global sales 

#==============Fixing variables==============================================
remove_consoles <- c("2600", "3DO", "NG", "PC", "PCFX", "SAT", "SCD", "TG16", "DC", "GEN", "GG")
library(dplyr)
library(forcats)

data <- rawdata %>%
  mutate(year = (as.numeric(year_of_release)+1979)) %>%
  dplyr::select(-c(year_of_release, name, publisher, developer)) %>%
  mutate(user_score = as.numeric(user_score))%>%
  filter(!(platform %in% remove_consoles))%>%
  droplevels()
data$platform_g3 <- fct_collapse(data$platform, 
                      nintendo = c("Wii", "WiiU", "GB", "GBA", "3DS", "DS", "N64", "GC",
                                   "NES", "SNES", "WS"),
                      playstation = c("PS", "PS2", "PS3", "PS4", "PSP", "PSV"),
                      xbox = c("XB", "X360", "XOne"))
data$genre_g8 <- fct_collapse(data$genre, 
                              Action_Adven = c("Action", "Adventure", "Shooter", "Fighting"),
                              Strategy = c("Puzzle","Strategy"))
data$rating_g5 <- fct_collapse(data$rating, 
                               E = c("E", "K-A"),
                               M = c("M", "AO"))
data$user_score_g10 <- cut(data$user_score, c(0,10,20,30,40,50,60,70,80,90,100), labels = c(1:10))
#user score 1 has a lot of missing data for some reason, going to remove them
data <- data %>%
  select(-c(genre, rating, platform, user_score))%>%
  filter(user_score_g10 !=1)%>%
  droplevels()
data$rating_g5 <- droplevels(data$rating_g5, exclude = "")
data$genre_g8 <- droplevels(data$genre_g8, exclude = "")
str(data)
#===============Split the data============================
library(caret)
set.seed(181818)
inTrain <- createDataPartition(y=data$global_sales, 
                               p=0.75, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
#==========Exploratory==============================
featurePlot(y=training$global_sales,
            x=training[,-5],
            plot = "pairs") #too much data 
#===========Checking missing data============================
sapply(training, function(x) sum(is.na(x)))
sapply(training, function(x) sum(is.na(x))/length(x))
sapply(testing, function(x) sum(is.na(x))/length(x))

#=============Imputing missing data======================
library(DMwR)
names(training)

#Training Data
cat_train <- training %>%
  select(platform_g3, genre_g8, rating_g5, user_score_g10)
num_train <- training %>%
  select(-c(platform_g3, genre_g8, rating_g5, user_score_g10))

cat_train <- knnImputation(cat_train, scale = T, meth = "median")
num_train <- knnImputation(num_train, scale = T, meth = "mean")

training <- data.frame(cat_train, num_train)

#Test Data
cat_test <- testing %>%
  select(platform_g3, genre_g8, rating_g5, user_score_g10)
num_test <- testing %>%
  select(-c(platform_g3, genre_g8, rating_g5, user_score_g10))

cat_test <- knnImputation(cat_test, scale = T, meth = "median", distData = cat_train)
num_test <- knnImputation(num_test, scale = T, meth = "mean", distData = num_train)
testing <- data.frame (cat_test, num_test)


#==========Checking for 0 variance=================
nsv <- nearZeroVar(training, saveMetrics = TRUE)
?nearZeroVar

#possibly jp_sales...

#==========Checking PCA before preprocessing===========================
names(training)
num_vars <- training %>%
  select(na_sales, eu_sales, jp_sales, other_sales, critic_count, critic_score)

my_pca <- prcomp(num_vars, center = T, scale = T)
summary(my_pca)
my_pca$sdev^2

screeplot(my_pca, main = "Scree Plot", xlab = "Components")
screeplot(my_pca, main="Scree Plot", type="line" )

par(mfrow=c(1,1))
biplot(my_pca, choices=c(1,3))
abline(h=0, v=0)

my_pca$rotation



biplot(my_pca, choices = c(3,4))
training[c("1", "18", "17"),]

table(training$user_score)
#Want to keep first 3 Principal Components

#PC2:  sales increase, critic score and critic count decrease 

#===========Using Preprocessing for PCA==================
grep("year", names(training))

preProc <- preProcess(training[,-c(9,13)], 
                      method = c("pca", "center", "scale"), pcaComp = 3)
trainPC <- predict(preProc, training)
testPC <- predict(preProc, testing)

#===========Creating Dummy Vars========================
trainPC <- dummy_cols(trainPC, remove_first_dummy = TRUE)
testPC <- dummy_cols(testPC, remove_first_dummy = TRUE)

trainPC <- trainPC %>%
  select(-c(genre_g8, rating_g5, platform_g3, user_score_g10))
testPC <- testPC %>%
  select(-c(genre_g8, rating_g5, platform_g3, user_score_g10))

#=======Model===================================
full_model <- train(log(global_sales+1) ~.,
                    method = "glm", family = gaussian(link = "identity"), data= trainPC )
full_model
fm_res <- full_model$finalModel
summary(fm_res)

table(trainPC$rating)

hist(log(trainPC$global_sales+1))

shapiro.test(log(trainPC$global_sales+1))



#===========BoxCox=====================================

library(MASS)
m1 <- lm(global_sales ~., data = trainPC)
summary(m1)
bc <- boxcox(m1)
plot(bc)

bc.max <- bc$x[which(bc$y == max(bc$y))]

