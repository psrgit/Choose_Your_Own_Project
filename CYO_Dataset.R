---
title: "CYO_Dataset"
author: "Puja"
date: "1/17/2022"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#OVERVIEW/INTRODUCTION/EXECUTIVE SUMMARY

#This data analysis project marks the end of the 9th and final section (Capstone) of the HarvardX Data Science course; made possible by edx. The main objective of this section as a whole is to allow its students to apply all the R skills they've learned thus far in the course in order to see how far they've come.

#This project expects similar work from the students as the previous one; with an exception of giving its students the opportunity to choose a data set of their own liking for this one.
#They are expected to choose a data set, split the data set into a training data set and a testing data set and then familiarize themselves with the training data set by exploring it.

#They will ultimately be expected to utilize machine learning skills on the training data set in order to construct different prediction models. This will then be applied to the testing data set in order to see how accurate the different prediction models are.

#CHOSEN DATASET

#The data set which I decided to choose for my CYO Project is called **"The Chatterjee-Price Attitude Data"** data set. The main reason as to why I decided to choose this specific data set is because it's easily accessible and does not take time to load at all.

#The previous project's data set, MovieLens, took extremely long to load and it required you to be connected to the internet in order to access it. I found that to be extremely frustrating & time consuming; and for these reasons, I've decided to go with this data set.

#This data set consists of numbers given in percentage proportion taken by a large financial organization. The data was recorded by means of questionnaires of roughly 35 employees from 30 different departments.
#I will be using my training data set in order to try and predict the rating scores recorded from the questionnaires using my testing data set.

#R PACKAGES

#1. REQUIRED PACKAGES WHICH NEED TO BE INSTALLED IF THEY HAVE NOT BEEN ALREADY

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

#2. ACCESS TO NECESSARY R PACKAGES

library(tidyverse)
library(dslabs)
library(caret)
library(ggpubr)

#ACCESS TO MY CHOSEN DATASET

#The data set which I have chosen to work on for my CYO Project is a build-in data set in R called "attitude". This data set can be easily be accessed by all R users.

#OVERALL DATASET SUMMARY

#This step shall help broaden our understanding of the chosen dataset

head(attitude)
str(attitude)
glimpse(attitude)

#According to the information above; the data set comprises of 30 rows (observations) and 7 numeric columns (variables) - namely: rating, complaints, privileges, learning, raises, critical and advance.

#TRAIN AND TEST DATASET SPLIT

#It is of paramount importance that the data which we are busy working with is  partitioned into two sets. One will be a training set and the other shall be the testing sets.
#This is done in order to avoid overfitting and to improve accuracy.
#The training set and it's observations will first be used to help form the required models. Once the is specific models are formed using the training set, they will then be applied to our test set in order to help analyse the performance of the model.

set.seed(33)
cyo_dataset <- attitude
head(cyo_dataset)
dim(cyo_dataset)

#For my data split; I have decided to use a **70/30** split. I have chose this ration because I feel as if a 90/10 split would not provide enough observations for my testing set and a 50/50 split won't be able to provide a reading which is  accurate enough. Therefore I have decide to go in between with a 70/30 split.
#I truly believe that this ratio will yield the highest accuracy.
#I shall now split the data 'cyo_dataset' into two with the help of the 'sample' function.

split<- sample(c(rep(1, 0.7 * nrow(cyo_dataset)), rep(2, 0.3 * nrow(cyo_dataset))))
split
table(split)

#The data set has now been split 70/30. We have 21 observation in our training set (1) ad 9 observations in our test set (2).
#They shall now receive their specific names.

#TRAINING SET:
training_set <- cyo_dataset[split == 1, ]

#TESTING SET:
testing_set <- cyo_dataset[split== 2, ]

#DATA EXPLORATION

#Let us now get an overall summary in order to observe the newly formed training dataset

head(training_set)
summary(training_set)
dim(training_set)
glimpse(training_set)

#Distinct numbers belonging to each variable in the training dataset

Distinct_Numbers <- training_set %>% 
  summarize(D_rating = n_distinct(training_set$rating),
            D_complaints = n_distinct(training_set$complaints),
            D_privileges = n_distinct(training_set$privileges),
            D_learning = n_distinct(training_set$learning),
            D_raises = n_distinct(training_set$raises),
            D_critical = n_distinct(training_set$critical),
            D_advance = n_distinct(training_set$advance))
Distinct_Numbers

#Ratings used the most by employees

Popular_Ratings <- training_set %>%
  group_by(rating) %>%
  summarize(num_used = n()) %>%
  top_n(10) %>%
  arrange(desc(num_used))
Popular_Ratings

#Maximum Value Belonging To Each Variable

tab <- matrix(c(max(training_set$rating),
                max(training_set$complaints),
                max(training_set$privileges),
                max(training_set$learning),
                max(training_set$raises),
                max(training_set$critical),
                max(training_set$advance)), ncol=1, byrow = FALSE)
colnames(tab) <- c('Maximum Value in Each Column')
rownames(tab) <- c('Rating', 'Complaints', 'Privileges', 'Learning', 'Raises', 'Critical', 'Advance')
tab <- as.table(tab)
tab

#Minimum Value Belonging To Each Variable

tab <- matrix(c(min(training_set$rating),
                min(training_set$complaints),
                min(training_set$privileges),
                min(training_set$learning),
                min(training_set$raises),
                min(training_set$critical),
                min(training_set$advance)), ncol=1, byrow = FALSE)
colnames(tab) <- c('Minimum Value in Each Column')
rownames(tab) <- c('Rating', 'Complaints', 'Privileges', 'Learning', 'Raises', 'Critical', 'Advance')
tab <- as.table(tab)
tab

#DATA VISUALIZATION

#SCATTER PLOT: CORRELATION BETWEEN RATING AND OTHER VARIABLES

plot(training_set, pch = 19, col = c("orange", "orange1"), 
     main = "Correlation")

#INSIGHT:

#According to the scatter plot above, we see that the variables 'complaints', 'raises' and 'learning' have a strong correlation rate with 'rating' in that order.
#In contrast, the variables 'privileges', 'critical' and 'advance' have a poor correlation with 'rating'.

#BAR GRAPH: RANGE OF RATING

Range_of_Rating <- training_set %>% 
  ggplot(aes(rating)) +
  geom_bar(color = "chartreuse3",  fill = "chartreuse1") +
  scale_x_continuous(breaks = c(35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) +
  labs(x = "Ratings", y = "Range") +
  ggtitle("Range of Rating") +
  theme_linedraw()
Range_of_Rating

#INSIGHTS:

#I have chosen to print the ratings that fall in between the numbers 35-90 because according our our data evaluation above; all of the recorded ratings fall within this interval.
#According to the bar graph shown above, the range of ratings are between 0, 1 and 2 only. Most of the ratings fall between the range of 0 and 1, with the exception of five ratings, which have a range of 2.

#BOXPLOT: RANGE OF RATINGS FOR VARIABLES WITH GOOD CORRELATION

boxplot(training_set$rating~training_set$complaints,
        xlab="Complaints", ylab="Rating")

boxplot(training_set$rating~training_set$raises,
        xlab="Raises", ylab="Rating")

boxplot(training_set$rating~training_set$learning,
        xlab="Learning", ylab="Rating")

#INSIGHTS:

#BOXPLOT 1 - This boxplot seems to show a dip in the middle of the plot, and then a sharp increase towards the right.
#This shows us that an increase in resolved complaints leads to an increase in ratings.

#BOXPLOT 2 - This boxplot seems to be relatively even throughout. This shows us that departments which offer their employees raises receive higher ratings compared to departments which do not.

#BOXPLOT 3 - This boxplot shows us that there is a positive relationship between learning and ratings; however, most of the ratings fall towards the lower left corner of the plot.

#MODELING OF DATA

#After analyzing all of the data evaluation and data visualization results above, we can now say that we have a very clear idea of the data set with which we are working on.
#We shall now use the knowledge gained thus far in order to construct models using the training set and implement them onto the testing set to predict the accuracy of the models.

#We shall measure the accuracy of the models through the use of the Residual Mean Squared Error (RMSE) Formula.
#This formula assists in measuring the error between the observed and the predicted values.

#RMSE Formula
RMSE_Formula <- function(observed, predicted){
  sqrt(mean((observed - predicted)^2))
}

#I shall now represent "testing_set$rating" as "b" in order to save myself from typing it out whenever I have to calculate the RMSE.
b <- testing_set$rating

#1ST MODEL

#The first model which I will be implementing onto the training set, and eventually onto the testing set is the most simplest of them all.
#This shall be done by using the mean of the ratings only. 

#Calculate the mean/average of all recorded ratings.
mu_cap <- mean(training_set$rating)
#RMSE of Model 1
RMSE_MODEL_1 <- RMSE_Formula(b, mu_cap)
RMSE_MODEL_1

#INSIGHTS GAINED FROM MODEL 1:
#The resulting RMSE for Model 1 is extremely high. Much higher than I had expected it to be. I shall now try to reduce it in the next model.

#2ND MODEL

#I shall now attempt to reduce the previously shown RMSE result by upgrading my first model. I shall try doing this by including the variable 'complaints'.

#Measure the effect of all complaints to the data set.
#Compute penalty term c1; associated with complaints
complaints_rating <- training_set %>%
  group_by(complaints) %>%
  summarise(c1 = mean(rating - mu_cap))
complaints_rating_forecast <- mu_cap + testing_set %>%
  left_join(complaints_rating, by = 'complaints') %>%
  .$c1
#RMSE OF MODEL 2
RMSE_MODEL_2 <- RMSE_Formula(b, complaints_rating_forecast)
RMSE_MODEL_2

#INSIGHTS GAINED FROM MODEL 2:
#I am extremely disheartened to see that the RMSE for model 2 has a result of N/A. Unfortunately, I have no idea as to why this has happened. This means that I will have to find another way to reduce the RMSE value.

#3RD MODEL
#I shall now implement the **Regulization** method to the data set in the hopes of obtaining a lower RMSE reading than the first model. Unfortunately, the second model gave a result of N/A. Hopefully this method will give a more promising RMSE result.

#Regulization
Regulization_Lmd <- seq(0, 15, 0.5)

Regulization_RMSE <- sapply(Regulization_Lmd, function(lmd){
    Regulization_c1 <- training_set %>%
      group_by(complaints) %>%
      summarize(Regulization_c1 = sum(rating - mu_cap)/(n() + lmd))
    
Regulization_c2 <- training_set %>%
      left_join(Regulization_c1, by = "complaints") %>%
      group_by(raises) %>%
      summarize(Regulization_c2 = sum(rating - Regulization_c1 - mu_cap)/(n() + lmd))
    
Regulization_c1_c2_mu_Forecast <- testing_set %>%
      left_join(Regulization_c1, by = "complaints") %>%
      left_join(Regulization_c2, by = "raises") %>%
      mutate(forecast_reg = mu_cap + Regulization_c1 + Regulization_c2) %>%
      pull(forecast_reg)
    return(RMSE_Formula(b, Regulization_c1_c2_mu_Forecast))
  })
#RMSE OF MODEL 3
RMSE_MODEL_3 = min(Regulization_RMSE)
RMSE_MODEL_3

#INSIGHTS GAINED FROM MODEL 3:
#Sadly,  the RMSE reading has yet again come up as N/A after using regulization wih the variables 'complaints' and 'raises'. This means that I will have to try another method in order to get a lower RMSE than the first model.

#4TH MODEL

#I shall now try a different approach and use the standard deviation to try and get a lower RMSE reading compared to the first model, which made use of the mean.
sd_cap <- sd(training_set$rating)
#RMSE of Model 4
RMSE_MODEL_4 <- RMSE_Formula(b, sd_cap)
RMSE_MODEL_4

#INSIGHTS GAINED FROM MODEL 4:
#On the bright side, this model managed to produce a RMSE reading with digits and not 'N/A'. However, this RMSE reading is not lower than the first model. This means that another method will have to be used in order to reduce the RMSE value.

#5TH MODEL

#Seeing that our previously constructed models were regrettably unable to achieve a strong RMSE I have now decided to use the only other method which comes to mind: the **Matrix Factorization** method. I shall now implement this method in the hopes of obtaining a lower RMSE value compard to the first constructed model. 

#MATRIX FACTORIZATION
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)
set.seed(1)
matrifac_r <- Reco()
matrifactest <- with(testing_set, data_memory(rating = rating, user_index = complaints, item_index = raises))
matrifactrain <- with(training_set, data_memory(rating = rating, user_index = complaints, item_index = raises))

matrifac_p <- matrifac_r$tune(matrifactrain, opts = list(niter = 10, nthread = 1, dim = c(1, 20)))
matrifac_r$train(matrifactrain, opts = c(matrifac_p$min, niter = 10, nthread = 1))

matrifac_answer <- matrifac_r$predict(matrifactest, out_memory())
#RMSE OF MODEL 5
RMSE_MODEL_5 <- RMSE_Formula(b, matrifac_answer)
RMSE_MODEL_5

#INSIGHTS GAINED FROM MODEL 5:
#This is great! I have finally succeeded in obtaining an RMSE value which is lower than all previous results. I am extremely pleased with this result.

#RESULTS

#I shall now create a 'results' table in which I shall record all of the RMSE Model recordings.

tab <- matrix(c(RMSE_MODEL_1, RMSE_MODEL_2, RMSE_MODEL_3, RMSE_MODEL_4, RMSE_MODEL_5),
              ncol=1, byrow = FALSE)
colnames(tab) <- c('RMSE Results')
rownames(tab) <- c('RMSE MODEL 1', 'RMSE MODEL 2', 'RMSE MODEL 3', 'RMSE MODEL 4',
                   'RMSE MODEL 5')
tab <- as.table(tab)
tab

#I am extremely happy that I managed to achieve an RMSE result that is lower than the one previously obtained. Despite this triumph; I will admit that I was hoping for a much lower RMSE result, but I find peace in knowing that I tried my best in trying to do so.

#CONCLUSION

#The 'Choose Your Own' Project was a great way to end this marvelous program! If I could change anything regarding my project, I think that I'd most probably choose a different dataset than the one I chose. Nevertheless, I managed to obtain a decent RMSE result which I am happy with. I'm glad I was able to better my previous result.

#I am extremely grateful to edx and HarvardX for creating such an amazing data science program. After completing all 9 courses, I feel much more confident when it comes to coding in R and programming in general. I've managed to learn so much and it amazes me as to wide the scope of coding can go! It's extremely limitless! You can literally code anything! This was an extremely joyful and insightful experience. 