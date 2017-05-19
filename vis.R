
#VISUALISATION OF IMDB DATASET

#to read data
    data <- read.csv ("movies.csv")
    print(data)

#to find the missing values in the dataset
     sum(is.na(data))   #this gives the total number of missing values in each dataset. 
     colSums(is.na(data)) #this gives the number of missing values in each column.
     mean(is.na(data))    #this gives the proportion of missing values. The mean was found to be small
     data <- na.omit(data)  #discards all the missing values
	summary(data)  #this gives the mean,median,1st quartile,3rd quartile,min nd max values in all 28 variables.


#EXPLORATORY ANALYSIS AND VISUALISATION

  df <- dataset[, c(3, 4, 5, 6, 8, 9, 13, 14, 16, 19, 23, 24, 25, 26, 27, 28)] #creates a dataframe of required variables
 
 ggpairs(df, diag = list(continuous = "density", discrete = "bar"), axisLabels = "internal") # to create correlation of the variables

 # Find top 10 directors from the dataset with more number of movies
    library(ggplot2)
   directors <- as.data.frame(table(data$director_name)) #to reference column in a dataframe
   directors <- arrange(directors, desc(Freq))
	plot <- function(directorsdf)
{
  directors <- ggplot2(head(directors, 10), aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Directors", y = "Number of movies") + ggtitle("Top 10 directors with more number of  movies") + coord_flip() + theme_classic()  #this function gives the barplot
}
plot(directorsdf)


# Find actors with most number of movies 
   library(ggplot2)
   actors <- as.data.frame(table(data$actor_1_name)) #to reference column in a dataframe
   actors <- arrange(actors, desc(Freq))
   plot <- function(actorsdf)
   {
     ggplot(head(actors, 20), aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Actors", y = "Number of movies") + ggtitle("Top 20 actors with most number of movies movies") + coord_flip() + theme_dark() #this function gives the barplot
   }
   plot(actorsdf)

#Find number of movies produced in different countries
  library(ggplot2)
  country <- as.data.frame(table(data$country))  #to reference column in a dataframe
  plot <- function(countrydf)
  {
    ggplot(country, aes(x = reorder(factor(Var1), Freq), y = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Countries", y = "Number of movies") + ggtitle("Total number of movies of different countries") + coord_flip() + theme_get()  #this function gives the barplot
  }
  plot(countrydf)
 
 #Find production of movies every year
 library(ggplot2)
year <- as.data.frame(table(data$title_year)) #to reference column in a dataframe
year <- arrange(year, desc(Freq))
 plot <- function(yeardf)
{
	ggplot(year[1:30,], aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Years", y = "Number of movies") + ggtitle("Total number of movies every year") + coord_flip() + theme_dark()  #this function gives the barplot
}
plot(yeardf)

#To find the content ratings of movies
   library(ggplot2)
   rating <- as.data.frame(table(data$content_rating))  #to reference column in a dataframe
   rating <- arrange(rating, desc(Freq))
   plot <- function(ratingdf)
   {
	ggplot(rating, aes(x = reorder(factor(Var1), Freq), y = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Contents", y = "Number of movies") + ggtitle("Number of movies with different content ratings") + coord_flip() + theme_light()  #this fucntion gives the barplot
   }
 plot(ratingdf)

# To find the distribution of IMDB score
   library(ggplot2)
   plot <- function(disdf)
   {
   	ggplot(df, aes(x = imdb_score))+ geom_histogram(stat = "bin", fill = "blue") +ggtitle(paste("Distribution of", names(df)[14])) + theme_dark()
   }
    plot (disdf)

# To plot the correlation graph between  num_critic_for_reviews and num_voted_users
  ggplot(df, aes(x = df[, 7], y = df[, 1])) + geom_point(color = "blue") + labs(x = names(df)[7], y = names(df)[1]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.595)) + geom_smooth(color = "black")   #from the ggpairs graph correlation was found to be 0.595. To find a smoother interpolation of data we use geom_smooth

 #To plot the correlation graph between actor_1_facebook likes and cast_total_facebook_likes
   ggplot(df, aes(x = df[, 8], y = df[, 5])) + geom_point(color = "blue") + labs(x = names(df)[8], y = names(df)[5]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.945)) + geom_smooth(color = "black") #from the ggpairs graph the correlation was found to be 0.945. To find a smoother interpolation of data we use geom_smooth

 #To plot the correlation graph between movie_facebook_likes and num_critic_for_reviews
 ggplot(df, aes(x = df[, 16], y = df[, 1])) + geom_point(color = "blue") + labs(x = names(df)[16], y = names(df)[1]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.704)) + geom_smooth(color = "black") #from the ggpairs graph correlation was found to be 0.704. To find a smoother interpolation of data we use geom_smooth

 #To plot the correlation graph between num_voted_users and num_user_for_reviews
 ggplot(df, aes(x = df[, 10], y = df[, 7])) + geom_point(color = "blue") + labs(x = names(df)[10], y = names(df)[7]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.78)) + geom_smooth(color = "black") #from the ggpairs graph correlation was found to be 0.78. To find a smoother interpolation of data we use geom_smooth

 # To plot the correlation graph between num_voted_users and movie_facebook_likes
 ggplot(df, aes(x = df[, 16], y = df[, 7])) + geom_point(color = "blue") + labs(x = names(df)[16], y = names(df)[7]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.519)) + geom_smooth(color = "black") #from the ggpairs graph correlation was found to be 0.519. To find a smoother interpolation of data we use geom_smooth

 # To plot the correlation graph between num_voted users and imdb_scores
  ggplot(df, aes(x = df[, 14], y = df[, 7])) + geom_point(color = "blue") + labs(x = names(df)[14], y = names(df)[7]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.478)) + geom_smooth(color = "black") # from the ggpairs graph correlation was found to be 0.478. To find a smoother interpolation of data we use geom_smooth
 