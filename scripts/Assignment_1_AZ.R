#Asignment_1 Anastasija Zubova

project_setup <- function(){
  folders <- c('data', 'results', 'figures', 'scripts')
  mainDir <- getwd()
  sapply(folders, function(subDir){
    dir.create(file.path(mainDir, subDir))
  })
}

project_setup()

renv::init() #initializing renv


#Step 3. 
install.packages("tidyverse")
library(tidyverse)

#Step 4.
data(iris)

write.csv(iris, file = "data/iris_dataset.csv", row.names = FALSE) #puting the dataset into the data folder in csv format

head(iris)
summary(iris)
str(iris)

#Step 5.
install.packages("dplyr")
library(dplyr)

species_mean <- iris %>% group_by(Species) %>% summarize_all(mean) #computing the mean of each variable in the iris dataset for each species
write.csv(species_mean, "results/species_mean.csv", row.names = FALSE)


take <- sample(seq(nrow(iris)), size = 15) #random sample of 15 rows from iris dataset and storing them in the vector "take"
iris_sample <- iris[take, ] #printing the subset of the dataset from the random sample "take"
write.csv(iris_sample, "results/iris_sample.csv", row.names = FALSE)

#Step 6.
install.packages("ggplot2")
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)
renv::snapshot() #saving the library state

box_plot <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))+ #box plot for the mean of Sepal Lenght of each species in the dataset
  geom_boxplot(aes(fill=Species)) +
  ylab("Sepal Length") + #labeling y axis
  ggtitle("Iris Boxplot", ) + #title
  theme(plot.title = element_text(hjust = 0.5)) + #centering the title
  stat_summary(fun = mean, geom = "point", shape = 0, size = 4) + #adding mean point in the shape "0", size 4
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 3)) #using wesanderson palette "GrandBudapest2" to colour the bars

ggsave("figures/box_plot.png", plot = box_plot, width = 8, height = 6, dpi = 300)

facet_plot <- ggplot(data=iris, aes(Sepal.Length, y=Sepal.Width, color=Species)) + #facet plot separates panels for each species and plots sepal width vs. sepal lenght
  facet_grid(. ~ Species)+
  geom_point(aes(shape=Species), size=2) + #different shapes for each species
  xlab("Sepal Length") + #labeling x axis
  ylab("Sepal Width") + #labeling y axis
  theme_light() +
  ggtitle("Faceting") + #plot axis
  theme(plot.title = element_text(hjust = 0.5))+ #centering the title
  scale_color_manual(values = wes_palette("FantasticFox1", n = 3)) #adding wesanderson color panel

ggsave("figures/facet_plot.png", plot = facet_plot, width = 8, height = 6, dpi = 300)

