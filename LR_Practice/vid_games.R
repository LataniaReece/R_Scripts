#Website: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
rm(list = ls())
rawdata <- read.csv("C:\\Users\\reece\\Desktop\\Video_Games_Sales_as_at_22_Dec_2016.csv",
                    header = TRUE, sep = ",")

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
remove_consoles <- c("2600", "3DO", "NG", "PC", "PCFX", "SAT", "SCD", "TG16")
nintendo <- c("Wii", "Wiiu", "GB", "GBA", "3DS", "DS", "N64", "GC", "NES", "SNES", "WS")
playstation <- c("PS", "PS2", "PS3", "PS4", "PSP", "PSV")
xbox <- c("XB", "X360", "XOne")
sega <- c("DC", "GEN", "GG")


View(table(rawdata$Developer))

library(dplyr)
data <- rawdata %>%
  select(-c(Name, NA_Sales, EU_Sales, JP_Sales, Other_Sales))%>%
  mutate(Year = (as.numeric(Year_of_Release)+1979)) %>%
  select(-Year_of_Release) %>%
  mutate(User_Score = as.numeric(User_Score))%>%
  filter(!(Platform %in% remove_consoles))%>%
  droplevels()%>%
  mutate(Platform_g4 = ifelse(Platform %in% nintendo, "nintendo", 
                              ifelse(Platform %in% playstation, "playstation", 
                                     ifelse(Platform %in% xbox, "xbox",
                                            ifelse(Platform %in% sega, "sega", NA)))))
reg_data <- data %>%
  select(-Platform)
names(reg_data) <- tolower(names(reg_data))


table(data$Publisher)

#==========missing values====================
percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(data, 1, percent_miss)
colmiss <- apply(data, 2, percent_miss)

crit_part_miss <- subset(data, rowmiss > 50)
crit_var_miss <- names(data[colmiss > 50])

missing_rows <- which(! complete.cases(reg_data))
missing <- reg_data[missing_rows,]
complete_data <- reg_data[-missing_rows,]
#===================SPLITTINGDATA=================

set.seed(1818)
n = nrow(complete_data)
trainIndex = sample(1:n, size = (n/2), replace=FALSE)
train = complete_data[trainIndex ,]
test = complete_data[-trainIndex ,]

#=====================REGRESSION============================


#Full Model: 
full_model <- lm(global_sales ~., data = train)
summary(full_model)
head(sort((summary(full_model)$coefficients[,4]), decreasing = TRUE), n = 10)


library(MASS)
bc <- boxcox(full_model)
bc.lam <- bc$x[which(bc$y==max(bc$y))]
detach("package:MASS", unload=TRUE)

#log(response)
m1 <- lm(log10(global_sales) ~., data = train)
summary(m1)
plot(m1, which = c(4)) #169 
plot(m1)

#removing outliers to see effect
train_noout <- train[!rownames(train) %in% c("169"), ]
m2 <- lm(log10(global_sales) ~., data = train_noout)
summary(m2)

plot(m2, which = c(4)) #148 152
plot(m2)

#removing more outliers to see effect
train_noout2 <- train[!rownames(train) %in% c("148", "169", "152"), ]
m3 <- lm(log10(global_sales) ~., data = train_noout2)
summary(m3)

plot(m3, which = c(4))
plot(m3)


#Fixing independent variables: 
m4 <- lm(log10(global_sales) ~ user_score + critic_count + log(user_count)+ critic_score
         + year+ rating + genre,
         data  = train_noout2)

summary(m4)
names(train_noout2)

hist(log10(train$user_count))

shapiro.test(log10(train$user_count))



#==================Predictions===================================

library(stats)
?predict

predict(final_model, test, se.fit = TRUE, level = 0.95, interval = "confidence")

#since RP is in test data but not in train data, I am going to remove that observation in the test data 
test <- test %>%
  filter(rating != "RP")

#predict again
predictions <- predict(final_model, test)
head(predictions)
#Correlation between predicted values and actual values: 

sales_pred_act <- data.frame(predicted = predictions, actual = test$global_sales)
head(sales_pred_act)

test2 <- test %>% 
  select(global_sales) %>%
  mutate(g_log = log10(global_sales)) %>%
  mutate(g_back = 10^g_log)%>%
  mutate(preds = predictions)

head(test2)
plot(test2$g_log, test2$preds)
library(GGally)
ggpairs(test2)


hist(log10(train_noout2$global_sales+1))



