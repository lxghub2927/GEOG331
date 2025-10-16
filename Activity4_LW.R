#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
#install.packages("tidyverse")
library(tidyverse)

#utilize dplyr::filter(...) or stats::filter(...) to switch between masked functions from conflicts

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

versicolor_only <- iris %>% filter(Species == "versicolor")
head(versicolor_only)

relationship_lists <- list(
  list(y = "Sepal.Length", x = "Sepal.Width"),
  list(y = "Petal.Length", x = "Petal.Width"),
  list(y = "Sepal.Length", x = "Petal.Length")
)

reg_results <- list()

for (i in seq_along(relationship_lists)) {
  y <- relationship_lists[[i]]$y
  x <- relationship_lists[[i]]$x
  formula <- as.formula(paste(y, "~", x))
  model <- lm(formula, data = versicolor_only)
  reg_results[[paste(y, "vs", x)]] <- summary(model)
}

reg_results[["Sepal.Length vs Sepal.Width"]]

library(broom)
tidied_results <- lapply(reg_results, tidy)
tidied_results
#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

head(iris)
new_df <- iris %>%
  left_join(height, by = "Species")
head(new_df)
#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)


#3a. now make the same plot in ggplot


#3b. make a scatter plot with ggplot and get rid of  busy grid lines


#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		