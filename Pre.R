library(plyr)
library(ggplot2)
library(randomForest)

# Split the dataset into train,test,validation
   set.seed(7)
   df <- df[sample(nrow(df)), ]
   train <- df[1:2000,]
   validation <- df[2001:2801,]
   test <- df[2802:nrow(df), ]

# Remove unwanted variables from prediction
set.seed(5)
r <- randomForest(imdb_score~., data = train, mtry = 5)  #mtry is a parameter in randomForest and the default is the square root of the number of predictor variables.

#To find the Root Mean Square Error
p <- predict(r, validation)
sqrt((sum((validation$imdb_score - p)^2))/ nrow(validation))  # RMSE = 0.68

#To find the mean square error
  mean((p - validation$imdb_score)^2)   #MSE = 0.472

# Predict on test dataset
  ptest <- predict(r, test)

# To find the Root mean squared error 
sqrt ((sum((test$imdb_score - ptest)^2))/ nrow(test))  #RMSE = 0.744

# To find MSE
mean((ptest - test$imdb_score)^2)   #MSE = 0.553

# By exploratory analysis  we understood that facenumber_in_poster and aspect_ratio are the two variables which don't help much in the prediction process
set.seed(5)
rf <- randomForest(imdb_score~num_critic_for_reviews + duration + director_facebook_likes + actor_3_facebook_likes + actor_1_facebook_likes + gross + num_voted_users + cast_total_facebook_likes  + num_user_for_reviews + budget + title_year + actor_2_facebook_likes + movie_facebook_likes, data = train, mtry = 5)

#RMSE
pre <- predict(rf, validation)
sqrt((sum((validation$imdb_score - pre)^2))/ nrow(validation)) #RMSE = 0.684

#MSE
mean((pre - validation$imdb_score)^2)  #MSE = 0.468

   # MSE measures the average of the squares of the errors or deviations,i.e the difference between the estimator and what is estimated. 
   # RMSE is a frequently used measure of the differences between values (sample and population values) predicted by a model or an estimator and the values actually observed.

#predict on test set
ptest2 <- predict(rf, test)
#RMSE
sqrt((sum((test$imdb_score - ptest2)^2))/ nrow(test))   #RMSE = 0.738

#MSE
mean((ptest2 - test$imdb_score)^2)  #MSE = 0.54 

       # From above observation, we can infer that error has been pulled out. 

#From exploratory data analysis conducted we realised that imdb_scores and num_voted_users are positively correlated.
   
    ggplot(movie_data, aes(x = sqrt(num_voted_users), y = imdb_score)) + geom_point(color="black") + geom_smooth(color="smooth") #this gives the graph of imdb_scores vs num_voted users which shows a positive curvilinear relationship
  
  ggplot(movie_data, aes(x = sqrt(num_voted_users), y = imdb_score)) + geom_point() + geom_smooth() + geom_smooth(method= "lm", color = "red)
    # The x-axis from previous graph was transformed using the square root function to get a more linear relationship. The smoothing function shows a more linear function. The red line was added to get the linear of best fit.

#linear regression model
     lm(formula = imdb_score ~ sqrt(num_voted_users), data = movie_data) 
   # The residual,intercepts,co-efficients,multiple r-squared,adjusted r-squared were obtained

# The R-squared was found to be 0.258 ~ 0.26




