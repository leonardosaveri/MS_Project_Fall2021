#Leonardo Saveri - 3139812
#MS-Project Fall 2021

#OPTIONS AND CHOOSING THE DATA
options(scipen=999)
data <- read.csv(file.choose(), sep=",", dec=".", header=TRUE)
#Clean Data
data <- na.omit(data)

#Select only the columns I need
data <- data[c('artist_name', 'track_name', 'popularity', 'acousticness',
               'danceability', 'key', 'mode', 'music_genre')]

View(data)
detach(data)
attach(data)
names(data)

#GRAPHS
#Scatterplot, Popularity-Danceability
plot(danceability, popularity, main="Relation Popularity-Danceability", 
     xlab="Danceability ", ylab="Popularity", pch=19, cex = 0.15)

#Correlation Popularity-Danceability
abline(lm(popularity ~ danceability), col = "red", lwd = 3)

#Scatterplot, Popularity-Acousticness
plot(acousticness, popularity, main="Relation Popularity-Acousticness", 
     xlab="Acousticness ", ylab="Popularity", pch=19, cex = 0.15)

#Correlation Popularity-Acousticness
abline(lm(popularity ~ acousticness), col = "red", lwd = 3)

#Scatterplot, Acousticness-Danceability
plot(acousticness, danceability, main="Relation Acousticness-Danceability", 
     xlab="Acousticness ", ylab="Danceability", pch=19, cex = 0.15)

#Correlation Acousticness-Danceability
abline(lm(danceability ~ acousticness), col = "red", lwd = 3)


#3d Scatterplot Showing the relation between Acousticness, Danceability, music_genre and Popularity
install.packages("plotly")
library(plotly)

fig <- plot_ly(data, x = ~acousticness, z = ~popularity, y = ~danceability, color = ~music_genre,
               size = 0.1, text = ~paste('</br> Artist: ', artist_name,
                             '</br> Song: ', track_name))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'acousticness'),
                                   yaxis = list(title = 'danceability'),
                                   zaxis = list(title = 'popularity')))

fig

#Two-sample t-test
#H0: mu_major_pop ≠ mu_minor_pop, H1: mu_major_pop = mu_minor_pop
alpha = .05
#since the variances are different
t.test(popularity ~ mode, data = data, var.equal = FALSE, conf.level = 1-alpha)
#we reject the null hypothesis because the p value < 0.05


#Multiple Linear Regression
lm1 <- lm(popularity~danceability+acousticness, data = data)
summary(lm1)

#Cross Validation
install.packages("caret")
library(caret)

#tanking only the columns I need
data <- data[c('popularity', 'acousticness', 'danceability', 'key', 'mode',
               'music_genre')]

#setting the seed
set.seed(123)

random_sample <- createDataPartition(data $ popularity, p = 0.8, list = FALSE)
training_dataset <- data[random_sample, ]
testing_dataset <- data[-random_sample, ]
model <- lm(popularity ~., data = training_dataset)
predictions <- predict(model, testing_dataset)

#understanding the data
data.frame( R2 = R2(predictions, testing_dataset $ popularity),
           RMSE = RMSE(predictions, testing_dataset $ popularity))
