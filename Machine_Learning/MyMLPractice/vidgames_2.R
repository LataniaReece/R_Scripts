#Website: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
rawdata <- read.csv("vid_games.csv",
                    header = TRUE, sep = ",")
names(rawdata) <- tolower(names(rawdata))
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

rawdata <- rawdata %>%
  select(-c(name, publisher, developer)) %>%
  mutate(year = (as.numeric(year_of_release)+1979)) %>%
  mutate(user_score = as.numeric(user_score))%>%
  filter(!(platform %in% remove_consoles))%>%
  droplevels()
rawdata$platform_g3 <- fct_collapse(rawdata$platform, 
                                 nintendo = c("Wii", "WiiU", "GB", "GBA", "3DS", "DS", "N64", "GC",
                                              "NES", "SNES", "WS"),
                                 playstation = c("PS", "PS2", "PS3", "PS4", "PSP", "PSV"),
                                 xbox = c("XB", "X360", "XOne"))
rawdata$genre_g8 <- fct_collapse(rawdata$genre, 
                              Action_Adven = c("Action", "Adventure", "Shooter", "Fighting"),
                              Strategy = c("Puzzle","Strategy"))
rawdata$rating_g5 <- fct_collapse(rawdata$rating, 
                               E = c("E", "K-A"),
                               M = c("M", "AO"))
rawdata$user_score_g10 <- cut(rawdata$user_score, c(0,10,20,30,40,50,60,70,80,90,100), labels = c(1:10))
#user score 1 has a lot of missing data for some reason, going to remove them
str(rawdata)
rawdata <- rawdata %>%
  select(-c(year_of_release, genre, rating, user_score, platform)) #should only have 13 variables
#==================missing values===============================
complete_cases <- complete.cases(rawdata)
complete_data <- rawdata[complete_cases,]
missing_data <- rawdata[!complete_cases,]

rawdata$missing <- as.factor(ifelse(rownames(rawdata) %in% rownames(missing_data), "NAs", "No-NAs"))
#should now have 14 variables 

sapply(rawdata, function(x) sum(is.na(x))/length(x)) 
#going to remove critic_score, critic_count and user_count since they determined NA's 
set.seed(100)
missing_test_data <- rawdata %>%
  select(-c(critic_score, critic_count, user_count))%>%
  filter(year >=1999) %>%
  group_by(missing)%>%
  sample_n(400)

str(rawdata)
#checking to see which variables have a relationship with missing values 
set.seed(100)
logitMod <- glm(missing ~.,
                data = missing_test_data,
                family=binomial,
                maxit = 100)
summary(logitMod) #year has a relationship

boxplot(missing_test_data$year ~ missing_test_data$missing)
table(missing_test_data$missing, missing_test_data$platform_g3)

library(ggplot2)
ggplot(data = missing_test_data, aes(x = missing, fill = platform_g3))+
  geom_bar(stat = "count", position = "dodge")

missing_test_data %>%
  group_by(missing)%>%
  summarize(min = min(year), max = max(year))
View(missing_test_data)

#=================Models============================
#Going to just work with complete cases,I think in this case, expert opinion matters. 
#Seems that year and platform is influencing the missing values

data <- complete_data
str(data)

data$rating_g5 <- droplevels(data$rating_g5, exclude = "")

#Data Partition (60/20/20)
library(caret)
set.seed(100)
inTrain <- createDataPartition(data$global_sales,
                               p = 0.6,
                               list = FALSE)
training <- data[inTrain, ] #training set
not_train <- data[-inTrain,]
set.seed(100)
inValid <- createDataPartition(not_train$global_sales,
                               p = 0.5,
                               list = FALSE)
cv_data <- not_train[inValid,] #cross validation set
testing <- not_train[-inValid,] #test set 


#Quick and Dirty algorithm, pca regression
custom <- trainControl(method = "repeatedcv",
                       number = 10, 
                       repeats = 3)
linear <- train(global_sales~.,
                data = training,
                method = "lm",
                preProcess = c("scale", "center", "pca"),
                trControl = custom,
                na.action = na.exclude)
summary(linear)
lin_mod <- linear$finalModel
linear$preProcess$numComp
