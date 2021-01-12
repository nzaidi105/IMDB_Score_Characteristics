#Nasir Zaidi
#Relationship between a movie's IMDB score and characteristics of production
#importing movies dataset
movies <- read.csv(file.choose(), header = T)

#Subset only containing movies of these directors
moviessubA <- subset(movies,movies$country == 'USA' &
                    (movies$director_name == 'Quentin Tarantino' |
                       movies$director_name == 'Christopher Nolan' |
                       movies$director_name == 'Steven Spielberg' |
                       movies$director_name == 'James Cameron' |
                       movies$director_name == 'Martin Scorsese'))

#Dummy variables for genres, only selected the most popular 6 genres                  
moviessubA$genres_ad <- ifelse(grepl("Adventure", moviessubA$genres), 1, ifelse(is.na(moviessubA$genres), NA, 0))
table(moviessubA$genres_ad, grepl("Adventure", moviessubA$genres))

moviessubA$genres_action <- ifelse(grepl("Action", moviessubA$genres), 1, ifelse(is.na(moviessubA$genres), NA, 0))
table(moviessubA$genres_action, grepl("Action", moviessubA$genres))

moviessubA$genres_b <- ifelse(grepl("Biography", moviessubA$genres), 1, ifelse(is.na(moviessubA$genres), NA, 0))
table(moviessubA$genres_b, grepl("Biography", moviessubA$genres))

moviessubA$genres_c <- ifelse(grepl("Crime", moviessubA$genres), 1, ifelse(is.na(moviessubA$genres), NA, 0))
table(moviessubA$genres_c, grepl("Crime", moviessubA$genres))

moviessubA$genres_d <- ifelse(grepl("Drama", moviessubA$genres), 1, ifelse(is.na(moviessubA$genres), NA, 0))
table(moviessubA$genres_d, grepl("Drama", moviessubA$genres))

moviessubA$genres_t <- ifelse(grepl("Thriller", moviessubA$genres), 1, ifelse(is.na(moviessubA$genres), NA, 0))
table(moviessubA$genres_t, grepl("Thriller", moviessubA$genres))

#Dummy variables for content ratings
moviessubA$content2_R <- ifelse(moviessubA$content_rating == "R", 1, 0)
moviessubA$content2_PG13 <- ifelse(moviessubA$content_rating == "PG-13", 1, 0)
moviessubA$content2_PG <- ifelse(moviessubA$content_rating == "PG", 1, 0)


#Frequency of movies produced by each director
table(moviessubA$director_name == 'Quentin Tarantino')
table(moviessubA$director_name == 'Christopher Nolan')
table(moviessubA$director_name == 'Steven Spielberg')
table(moviessubA$director_name == 'James Cameron')
table(moviessubA$director_name == 'Martin Scorsese')

#Summary stats of budget and IMDB variables
summary(moviessubA$budget)
summary(moviessubA$imdb_score)

#Standard deviations of both, budget sub-varibale created without NA's
budget.noNa <- na.omit(moviessubA$budget)
sd(budget.noNa)
sd(moviessubA$imdb_score)


#Dividing budget var by 1 million to make values larger 
budget.m <- moviessubA$budget/10000000
#OLS Regression
reg_imdb <- lm(imdb_score ~  genres_ad + genres_action + genres_b + genres_c + genres_d + genres_t +
               content2_R + content2_PG13 + budget.m + director_name, data = moviessubA)
summary(reg_imdb)


#Diagnostic text table
library(stargazer)
stargazer(reg_imdb, type = "text")


#Diagnostic Tests
par(mfrow=c(2,2))
plot(lm(reg_imdb))
par(mfrow=c(1,1))

#Vif Scores
library(car)
round(vif(reg_imdb),3)




