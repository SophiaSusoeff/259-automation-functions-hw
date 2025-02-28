#PSYC 259 Homework 4 - Writing functions
#Optional, for extra credit if all questions are answered

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

#writing function
limit_replace <- function(vector) {
  vector[vector < 2 | vector > 8] <- NA
  return(vector)
}

#running function on x & new vector
x_replace <- limit_replace(x)

#viewing in console
(x_replace)
  

### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower bounary and an upper boundary
  #so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

#new version
limit_replace <- function(vector, lower_bound, upper_bound) {
  vector[vector < lower_bound | vector > upper_bound] <- NA
  return(vector)
}

#running function on y & new vector
y_replace <- limit_replace(y, lower_bound = 4, upper_bound = 6)
(y_replace)


### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
  #and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

#writing function
plus_minus_SD <- function(vector, num_of_SDs = 1) {
  mean_q3 <- mean(vector, na.rm = TRUE)
  sd_q3 <- sd(vector, na.rm = TRUE)
  lower_bound <- mean_q3 - num_of_SDs * sd_q3
  upper_bound <- mean_q3 + num_of_SDs * sd_q3
  return(list(lower_bound = lower_bound, upper_bound = upper_bound))
}

#running function on x
plus_minus_SD(x,1)

#on y
plus_minus_SD(y,1)

#on z 
plus_minus_SD(z,1)


### Question 4 ----------

#Write another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

#another new function
limit_replace <- function(vector, lower_bound = NULL, upper_bound = NULL) {
  if(is.null(lower_bound) || is.null(upper_bound)) {
    boundaries <- plus_minus_SD(vector,1)
    lower_bound <- boundaries$lower_bound
    upper_bound <- boundaries$upper_bound
  }
  vector[vector < lower_bound | vector > upper_bound] <- NA
  return(vector)
}

#applying function to columns 
View(ds)

ds_replace <- ds %>% mutate(across(everything(), ~limit_replace(.)))

(ds_replace)


### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

#adding the command
limit_replace <- function(vector, lower_bound = NULL, upper_bound = NULL) {
  stopifnot(is.numeric(vector))
  if(is.null(lower_bound) || is.null(upper_bound)) {
    boundaries <- plus_minus_SD(vector,1)
    lower_bound <- boundaries$lower_bound
    upper_bound <- boundaries$upper_bound
  }
  vector[vector < lower_bound | vector > upper_bound] <- NA
  return(vector)
}

#checking on non-numeric input
limit_replace("id")

### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement

#Answer: We need numeric values in the lower_bound and upper_bound input to make the function work correctly. 
#We also need to make sure the vector is not empty. 

#adding another stopifnot to enforce non-empty vector
limit_replace <- function(vector, lower_bound = NULL, upper_bound = NULL) {
  stopifnot(is.numeric(vector))
  stopifnot(length(vector) > 0)
  if(is.null(lower_bound) || is.null(upper_bound)) {
    boundaries <- plus_minus_SD(vector,1)
    lower_bound <- boundaries$lower_bound
    upper_bound <- boundaries$upper_bound
  }
  vector[vector < lower_bound | vector > upper_bound] <- NA
  return(vector)
}


### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

#loading functions from external file
source("259_written_functions.R") #it worked, yay! :)

#running limit_replace on new data set
ds_trimmed <- ds_diamonds %>% 
  mutate(across(where(is.numeric), ~ limit_replace(.))) %>% 
  drop_na()

#viewing tibble
ds_trimmed


### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()


library(ggplot2)

#creating function
boxplot <- function(data, cut_label, trimmed = FALSE) {
  dataset_name <- ifelse(trimmed, "trimmed", "all")
  ggplot(data %>% filter(cut == cut_label), aes(x = clarity, y = price)) + 
    geom_boxplot() + 
    ggtitle(paste(cut_label, dataset_name)) + 
    theme_minimal()
}

#list of cuts
cuts <- c("Premium", "Ideal", "Good")

#loop for graphs
for (cut in cuts) {
  print(boxplot(ds_diamonds, cut, trimmed = FALSE))  # Untrimmed
  print(boxplot(ds_trimmed, cut, trimmed = TRUE))   # Trimmed
}


