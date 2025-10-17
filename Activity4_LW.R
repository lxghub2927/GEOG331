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

#pipe data to only contain versicolor
versicolor_only <- iris %>% filter(Species == "versicolor")
head(versicolor_only)

#create lists with relationships to compare
relationship_lists <- list(
  list(y = "Sepal.Length", x = "Sepal.Width"),
  list(y = "Petal.Length", x = "Petal.Width"),
  list(y = "Sepal.Length", x = "Petal.Length")
)

reg_results <- list()

#for loop to run relationships and produce regression table
for (i in seq_along(relationship_lists)) {
  y <- relationship_lists[[i]]$y
  x <- relationship_lists[[i]]$x
  formula <- as.formula(paste(y, "~", x))
  model <- lm(formula, data = versicolor_only)
  reg_results[[paste(y, "vs", x)]] <- summary(model)
}
#show results of calculations
reg_results[["Sepal.Length vs Sepal.Width"]]
reg_results[["Sepal.Length vs Petal.Width"]]
reg_results[["Sepal.Length vs Petal.Length"]]

#attempt to tidy results using broom from tidyverse
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

#utilization of left_join to join data of maximum height to new iris dataframe
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
ggplot(data = iris, aes(x= Sepal.Length, y = Sepal.Width))+geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
#utilization of theme_classic to remove grid lines
ggplot(data = iris, aes(x= Sepal.Length, y = Sepal.Width))+geom_point() + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
# additional features added into aes. title added via theme(plot.title.position = "plot")
ggplot(data = iris, aes(x= Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Length))+geom_point() + 
  labs(title = "Sepal Dimensions in Iris Species",
  x = "Sepal Length (cm)",
  y = "Sepal Width (cm)",
  color = "Species",
  size = "Petal Length") +theme_classic() + ggtitle("Sepal Dimensions in Iris Species") + theme(plot.title.position = "plot")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

#in plot, you have to put everything in the plot function while in ggplot, you can add elements and other directions step by step each time you plot the graph, so you can add it on the go with ggplot.
#also in plot:
#you enter vectors and controls appearance of plot with separate arguments
#meanwhile in ggplot:
#you enter the dataframe you are working with and the parameters of the data frame you want to plot. Furthermore, you declare how variables map visual properties inside aes. 
#you add additional description of appearance separately using additional layers such as "geom_point."