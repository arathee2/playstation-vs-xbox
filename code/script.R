# LOAD DATA -------------------------------

# housekeeping
rm(list=ls())


# load libraries
library(tidyverse)
library(mice)
library(corrplot)
library(car)
library(gridExtra)
library(MASS)
options(scipen=10000)

# read data
sales <- read.csv("../data/video-game-sales.csv", na.strings = c("", "N/A", "tbd", NA),
                  colClasses = c("character", "factor", "integer", "factor", 
                                 "character", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "integer", "integer",
                                 "numeric", "integer", "character", "factor"))



# change variable names to lower case
names(sales) <- tolower(names(sales))

# data summary
head(sales)
str(sales)
summary(sales)

# EDA -----------------------------------

# discard all sales variables except global sales
sales$na_sales <- NULL
sales$eu_sales <- NULL
sales$jp_sales <- NULL
sales$other_sales <- NULL

# also discard publisher and developer - both of these have around 1000 categories
sales$developer <- NULL
sales$publisher <- NULL




# missing data analyses
md.pattern(sales, rotate.names = TRUE)

# percentage missing values in each column
sapply(sales, function(x) sum(is.na(x))*100/length(x))

# 60% rows have missing data, therefore, drop rows with missing value(s)
sales <- na.omit(sales)



# change scale of global_sales from number of units in million to number of units sold
sales$global_sales <- sales$global_sales * 1e6



# change buckets of rating
table(sales$rating)
sales <- sales[!sales$rating %in% c("AO", "K-A", "RP"), ]
sales$rating <- as.character(sales$rating)
sales$rating[sales$rating == "E"] <- "Everyone"
sales$rating[sales$rating == "E10+"] <- "10+"
sales$rating[sales$rating == "M"] <- "17+"
sales$rating[sales$rating == "T"] <- "13+"
sales$rating <- factor(sales$rating)

# make scale of user_score equal to critic_score which is 0-100
sales$user_score <- sales$user_score*10







# data summary
md.pattern(sales, rotate.names = TRUE)
head(sales)
str(sales)
summary(sales)

# correlation matrix
numeric <- sapply(sales, is.numeric)
cordata <- sales[ , numeric]
cor_matrix <- cor(cordata)
cor_matrix
corrplot(cor_matrix, method = "square", type = "lower") # to visualize correlation matrix






# global sales
quantile(sales$global_sales, probs = seq(0, 1, 0.01))

sales %>%
  ggplot(aes(x=global_sales/1e6)) +
  geom_histogram(fill="orange") +
  labs(title="Distribution of Global Sales", x="Global Sales (in million units)", y="Number of Video Games") +
  theme_minimal()

## global sales on log scale - looks better
sales %>%
  ggplot(aes(x=log(global_sales))) +
  geom_histogram(fill="orange") +
  labs(title="Distribution of Log of Global Sales")

## global sales on square root scale
sales %>%
  ggplot(aes(x=sqrt(global_sales))) +
  geom_histogram(fill="orange")

## global sales on cube root scale
sales %>%
  ggplot(aes(x=global_sales^(1/3))) +
  geom_histogram(fill="orange")

## global sales on tenth root scale
sales %>%
  ggplot(aes(x=global_sales^(1/10))) +
  geom_histogram(fill="orange")






# platform and sales -- too many platforms
sales %>%
  ggplot(aes(x=platform, y=log(global_sales))) +
  geom_boxplot() +
  labs(title = "Platforms and Sales", x="Platform", y="Log Global Sales")

table(sales$platform)

# bucket all playstations in one category, all xbox in one category and discard all others
consoles <- levels(sales$platform)[levels(sales$platform) %in% c("PS", "PS2", "PS3", 
                                                                 "PS4", "X360", "XB", "XOne")]
sales <- sales[sales$platform %in% consoles, ]
sales$platform <- factor(sales$platform)
sales$platform <- as.character(sales$platform)
sales$platform[sales$platform == "PS"] <- "Playstation"
sales$platform[sales$platform == "PS2"] <- "Playstation"
sales$platform[sales$platform == "PS3"] <- "Playstation"
sales$platform[sales$platform == "PS4"] <- "Playstation"
sales$platform[sales$platform == "PSP"] <- "Playstation"
sales$platform[sales$platform == "PSV"] <- "Playstation"
sales$platform[sales$platform == "X360"] <- "Xbox"
sales$platform[sales$platform == "XB"] <- "Xbox"
sales$platform[sales$platform == "XOne"] <- "Xbox"
sales$platform <- factor(sales$platform)

sales %>%
  ggplot(aes(x=platform, y=log(global_sales))) +
  geom_boxplot() +
  labs(title = "Platforms and Sales", x="Platform", y="Log Global Sales")

# consider games only after 2002 -- when both consoles were available in the market for the first time simultaneously
sales <- sales[sales$year_of_release >= 2002, ]






# genre and sales
sales %>%
  ggplot(aes(x=genre, y=log(global_sales))) +
  geom_boxplot()

# rating and sales
sales %>%
  ggplot(aes(x=rating, y=log(global_sales))) +
  geom_boxplot()





# year of release and global sales
sales %>%
  ggplot(aes(x=year_of_release, y=log(global_sales))) +
  geom_point()

# average sales per year
d1 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(mean_sales = mean(global_sales)))

d2 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales_min = min(global_sales)))

d3 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales10 = quantile(global_sales, probs = 0.1)))


d4 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales90 = quantile(global_sales, probs = 0.9)))

d5 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales_max = max(global_sales)))


time_data <- merge(d1, d2, by = c("year"))
time_data <- merge(time_data, d3, by = c("year"))
time_data <- merge(time_data, d4, by = c("year"))
time_data <- merge(time_data, d5, by = c("year"))

gather(time_data, quantile, sales, mean_sales:sales_max) %>%
  ggplot(aes(x=year+1994, y=sales, color=quantile)) +
  geom_line() +
  theme_minimal()


# age of the game and global sales
sales %>%
  ggplot(aes(x=2017-year_of_release, y=log(global_sales))) +
  geom_point()





# user count -- rating seems to have a interaction with user_count
sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales), color=platform)) +
  geom_point()

sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales), color=genre)) +
  geom_point() +
  labs(title = "User Counts and Sales By Video Game Genre", x="User Count", y="Log Sales")

sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales), color=rating)) +
  geom_point()


# user score -- no pattern in any categorical variable
sales %>%
  ggplot(aes(x=user_score, y=log(global_sales), color=platform)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=user_score, y=log(global_sales), color=genre)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=user_score, y=log(global_sales), color=rating)) +
  geom_jitter()


# critic count -- rating seems to have a interaction with critic_count
sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=platform)) +
  geom_point()

sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=genre)) +
  geom_point()

sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=rating)) +
  geom_point() +
  labs(title = "Sales, Critic Count and Rating", y="Log Global Sales", x="Critic Count")


# critic score -- no pattern in any categorical variable
sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales), color=platform)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales), color=genre)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales), color=rating)) +
  geom_jitter()




# check if platforms have different different slopes and intercepts
sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)

sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)


sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)


sales %>%
  ggplot(aes(x=user_score, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)

sales %>%
  ggplot(aes(x=user_score, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)


# MODELING --------------------------------


# simple linear regression on log(global_sales) -- problem with residuals
linear.model <- lm(log(global_sales) ~ log(critic_count) + critic_score + log(user_count) + user_score,
                   data = sales)

summary(linear.model)
plot(linear.model)








# simple linear regression on log(global_sales) -- problem with residuals
linear.model <- lm(log(global_sales) ~ platform + year_of_release + genre + 
                   critic_count + critic_score + user_count + user_score + rating,
                   data = sales)

summary(linear.model)
plot(linear.model)








# step model with all the important variables and interactions -- problem with residuals
linear.model <- lm(log(global_sales) ~ platform + year_of_release + genre + 
                   critic_count + critic_score + user_count + user_score + rating +
                   user_count:platform + user_count:genre + user_count:rating +
                   critic_count:platform + critic_count:genre + critic_count:rating,
                   data = sales)

step.model <- step(linear.model, direction = c("both"))
summary(step.model)
plot(step.model)








# simple linear regression on sqrt(global_sales) -- problem with residuals
linear.model <- lm(sqrt(global_sales) ~ platform + year_of_release + genre + 
                     critic_count + critic_score + user_count + user_score + rating,
                   data = sales)

summary(linear.model)
plot(linear.model)




# EDA ------------------------


# see how many games have been released for 
game.counts <- data.frame(name = unique(sales$name), count = 0)
game.counts$name <- as.character(game.counts$name)
str(game.counts)
head(game.counts)


# count how many times each game appears in the data
for (name in game.counts$name) {
  for (name2 in sales$name) {
    if(name == name2){
      game.counts$count[game.counts$name == name] <- game.counts$count[game.counts$name == name] + 1
    }
  }
}

game.counts[order(game.counts$count, decreasing = TRUE), ]

sales[sales$name == "Need for Speed: Most Wanted", ]
sales[sales$name == "Grand Theft Auto V", ]
sales[sales$name == "Sonic Heroes", ]
sales[sales$name == "NBA 2K14", ]

sales[order(sales$global_sales, decreasing = FALSE), ]




# NEW RESPONSE: mean sales per year
sales$game_age <- 2017 - sales$year_of_release
sales$sales <- sales$global_sales/sales$game_age


summary(sales)


# platform and sales
sales %>%
  ggplot(aes(x=platform, y=log(sales))) +
  geom_boxplot() +
  labs(title = "Consoles and Sales", x="Console", y="LOG(Sales)") +
  theme_minimal()


# sales -- log seems to be a good transformation
sales %>%
  ggplot(aes(x=sales/1e6)) +
  geom_histogram(fill="orange") +
  labs(title="Distribution of Mean Sales Per Year", x="Sales (in million units)", y="Number of Video Games") +
  theme_minimal()

sales %>%
  ggplot(aes(x=log(sales))) +
  geom_histogram(fill="orange") +
  labs(title="Log Transformation of Sales", x="LOG(Sales)", y="Number of Video Games") +
  theme_minimal()

sales %>%
  ggplot(aes(x=sales^(1/20))) +
  geom_histogram(fill="orange") +
  labs(title="20th Root of Sales", x="Sales^(1/20)", y="Number of Video Games") +
  theme_minimal()



# critic

## critic score and sales - perhaps an exponential (degree 2 or 3) transformation required for critic_score
sales %>%
  ggplot(aes(x=critic_score, y=(log(sales)))) +
  geom_jitter() +
  geom_smooth(method = "auto") + 
  labs(title = "Critic Score and Sales", x="Critic Score", y="LOG(Sales)") +
  theme_minimal()

sales %>%
  ggplot(aes(x=critic_score^3, y=(log(sales)))) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=critic_score^3, y=log(sales), color=platform)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=critic_score^3, y=log(sales), color=genre)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=critic_score^3, y=log(sales), color=rating)) +
  geom_jitter() +
  geom_smooth(method = "auto")


## critic count and sales - linear relation
sales %>%
  ggplot(aes(x=critic_count, y=(log(sales)))) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=critic_count, y=log(sales), color=platform)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=critic_count, y=log(sales), color=genre)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=critic_count, y=log(sales), color=rating)) +
  geom_jitter() +
  geom_smooth(method = "auto")





# user

## user score and sales - linear relation
sales %>%
  ggplot(aes(x=user_score, y=(log(sales)))) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=user_score, y=log(sales), color=platform)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=user_score, y=log(sales), color=genre)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=user_score, y=log(sales), color=rating)) +
  geom_jitter() +
  geom_smooth(method = "auto")


## user count and sales - use log user count
sales %>%
  ggplot(aes(x=user_count, y=(log(sales)))) +
  geom_point(color="red") +
  geom_smooth(method = "auto") +
  labs(x="User Count", y="LOG(Sales)") +
  theme_minimal()

sales %>%
  ggplot(aes(x=log(user_count), y=(log(sales)))) +
  geom_point(color="red") +
  geom_smooth(method = "auto") +
  labs(x="LOG(User Count)", y="LOG(Sales)") +
  theme_minimal()

sales %>%
  ggplot(aes(x=log(user_count), y=log(sales), color=platform)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=log(user_count), y=log(sales), color=genre)) +
  geom_jitter() +
  geom_smooth(method = "auto")

sales %>%
  ggplot(aes(x=log(user_count), y=log(sales), color=rating)) +
  geom_jitter(alpha=0.3) +
  geom_smooth(method = "auto") +
  theme_minimal() +
  labs(title = "User Count and Rating", x="LOG(User Count)", y="LOG(Sales)")

sales$log_user_count <- log(sales$user_count)


# user and critic score
sales %>%
  ggplot(aes(x=user_score, critic_score)) +
  geom_point() +
  geom_smooth(method = "auto")



# user and critic counts
sales %>%
  ggplot(aes(x=log(user_count), critic_count)) +
  geom_point() +
  geom_smooth(method = "auto")






# SALES MODELING

linear.model <- lm(log(sales) ~ critic_score, data = sales)
summary(linear.model)
plot(linear.model)

linear.model <- lm(log(sales) ~ poly(critic_score, 2), data = sales)
summary(linear.model)
plot(linear.model)

linear.model <- lm(log(sales) ~ poly(critic_score, 5), data = sales)
summary(linear.model)
plot(linear.model)


anova(lm(log(sales) ~ poly(critic_score, 2), data = sales), 
      lm(log(sales) ~ poly(critic_score, 3), data = sales))






linear.model <- lm(log(sales) ~ platform + genre + 
                     critic_count + poly(critic_score, 2) + log(user_count) + user_score + rating +
                     user_score:genre + 
                     log(user_count):rating +
                     critic_count:platform + 
                     log(user_count):user_score + critic_count:poly(critic_score, 2),
                   data = sales)

summary(linear.model)
plot(linear.model)

step.model <- step(linear.model, direction = c("both"))
summary(step.model)
plot(step.model)

qplot(y=residuals(step.model), x=critic_count,data=sales, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")







poisson.model <- glm(sales ~ platform + genre + poly(critic_score, 2) + 
                     log(user_count) + user_score + rating + log(user_count):rating,
                     family=poisson,
                     data = sales)

summary(poisson.model)
plot(poisson.model)

salesregresid1 <- resid(poisson.model, type = "pearson")
salesregpred1 <- predict(poisson.model, type= "response")
qplot(y=salesregresid1, x=salesregpred1,data=sales, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals") +
  coord_cartesian(xlim = c(0, 5e6), ylim = c(-2500, 4000))







poisson.model <- glm(sales ~ platform + genre + poly(critic_score, 2) + 
                       log(user_count) + user_score + rating + log(user_count):rating,
                     family=quasipoisson,
                     data = sales)

summary(poisson.model)
plot(poisson.model)

salesregresid1 <- resid(poisson.model, type = "pearson")
salesregpred1 <- predict(poisson.model, type= "response")
qplot(y=salesregresid1, x=salesregpred1,data=sales, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals") +
  coord_cartesian(xlim = c(0, 5e6), ylim = c(-2500, 4000))







nb.model <- glm.nb(sales ~ platform + genre + poly(critic_score, 2) + 
                       log(user_count) + user_score + rating + log(user_count):rating,
                     data = sales)

summary(nb.model)
plot(nb.model)









linear.model <- lm(log(sales) ~ platform + genre + 
                     critic_count + poly(critic_score, 2) + log(user_count) + user_score + rating +
                     user_score:genre +
                     log(user_count):rating +
                     critic_count:platform + 
                     log(user_count):user_score + critic_count:poly(critic_score, 2),
                   data = sales)

summary(linear.model)
plot(linear.model)
vif(linear.model)

step.model <- step(linear.model, direction = c("both"))
summary(step.model)
vif(step.model)

plot(step.model)

qplot(y=residuals(step.model), x=critic_count,data=sales, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")





# model with no interactions
linear.model <- lm(log(sales) ~ platform + genre + 
                     critic_count + critic_score + log(user_count) + user_score + rating,
                   data = sales)

summary(linear.model)
plot(linear.model)
vif(linear.model)

step.model <- step(linear.model, direction = c("both"))
summary(step.model)
vif(step.model)






# final model
linear.model <- lm(log(sales) ~ platform + genre + critic_score + log(user_count) + 
                     user_score + rating,
                   data = sales)

summary(linear.model)
plot(linear.model)
exp(coef(linear.model))
exp(confint(linear.model))
vif(linear.model)


## model assessment and validation
sales %>% 
  ggplot(aes(x=fitted(linear.model), y=residuals(linear.model))) + 
  geom_point(alpha=0.5) +
  labs(title="Residual Plot", x="Fitted Log(Sales)", y="Residuals") +
  theme_minimal() 


sales %>% 
  ggplot(aes(x=critic_score, y=residuals(linear.model))) + 
  geom_jitter(alpha=0.5) +
  labs(title="Residual Plot", x="Critic Score", y="Residuals") +
  theme_minimal() 


sales %>%
  ggplot(aes(sample = log(sales))) + stat_qq(alpha=0.5) + stat_qq_line() + theme_minimal()











# $$  log(sales) = 8.03 - 0.26 \times Xbox - 0.47 \times  genreAdventure + 0.09 \times  genreFighting + 0.41 \times  genreMisc \\ - 0.34 \times  genrePlatform - 0.85 \times genrePuzzle - 0.18 \times  genreRacing - 0.54 \times  genreRolePlaying \\ - 0.02 \times  genreShooter + 0.12 \times  genreSimulation + 0.1 \times  genreSports - 0.7 \times  genreStrategy \\ + 0.02 \times critic\_score + 0.73 \times log(user\_count) - 0.01 \times user\_score - 0.4 \times ratingTeen \\ - 0.63 \times ratingMature - 0.13 \times ratingEveryone$$
  
Variable                   |  Description
-------------------------- | -------------------------------------------------------------------------------
  Name                       |  Name of the video game.
Platform                   |  Xbox, Xbox One, Xbox 360, PC, Wii, Playstation, Playstation 2, etc.
Year of Release            |  Year when the game was released or is expected to release. Year ranges from 1985 - 2020.
Genre                      |  Action, racing, shooting, etc.
Global Sales               |  Millions of copies sold globally.
Critic Score               |  Weighted average score given by video game critics. Ranges from 0 to 100.
Critic Count               |  Number of critics who rated the game.
User Score                 |  Average score given by users. Goes from 0 to 10.
User Count                 |  Number of users who rated the game.
Rating                     |  Audience for which the game is appropriate for - teens, adults, kids, etc.




Variable                   |  Exponentiated Coefficient  |  p-value       | Significance at $\alpha$ = 0.05
-------------------------- | --------------------------- | --------------------- | --------
  (Intercept)                |    3065.1                   |  < 0.0000000000000002 |  Significant
platformXbox               |     0.77                    | < 0.0000000000000002  | Significant
genreAdventure             |    0.63                     | 0.0000001899190246    | Significant
genreFighting              |    1.09                     |  0.16061              | Insignificant
genreMisc                  |    1.52                     |  0.0000000080625175   | Significant
genrePlatform              |   0.71                      |   0.0000347591217814  | Significant
genrePuzzle                |  0.42                       |           0.00190     | Significant 
genreRacing                |  0.83                       |              0.00256  | Significant 
genreRole-Playing          |  0.58                       | < 0.0000000000000002  | Significant
genreShooter               |  0.98                       |             0.61018   | Insignificant  
genreSimulation            |  1.13                       |            0.17000    | Insignificant  
genreSports                |  0.11                       |           0.07340     | Insignificant  
genreStrategy              |  0.49                       |   0.0000000000092703  | Significant  
critic_score               |   1.02                      | < 0.0000000000000002  | Significant  
log(user_count)            |  2.09                       | < 0.0000000000000002  | Significant  
user_score                 |  0.99                       | < 0.0000000000000002  | Significant  
rating13+                  |  0.66                       |   0.0000000000000207  | Significant  
rating17+                  |  0.53                       | < 0.0000000000000002  | Significant  
ratingEveryone             |  0.88                       |              0.02453  | Significant  



















