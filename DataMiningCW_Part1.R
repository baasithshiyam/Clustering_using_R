
library(tidyverse)

library(readxl)

library(NbClust)

library(knitr)

library(tidymodels)

library(flexclust)

library(funtimes)

theme_set(theme_light())

################part 1 ######### 
############# BAASITH ##############


setwd("E:/2nd Year/2nd sem/DataMining/cw")
getwd()

# Read in the original excel datafile

vechile_data <- read_excel("vehicles.xlsx") %>%
  
  janitor::clean_names() %>%
  
 
  
  mutate(class = as_factor(class))

# Get view dataset looks like

summary(vechile_data)



vechile_data %>%
  # pivot_longer() is an updated approach to gather() 
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  # fct_reorder works wonders when you want to have the order of factor levels
  mutate(class = fct_reorder(class,value,median)) %>%
  #aes => Aesthetic mappings describe how variables in the data are mapped
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Outlier Detection for class: 'van'")+
  
  coord_flip()



vechile_data %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Outlier Detection for class: 'bus'")+
  
  coord_flip()





vechile_data %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Outlier Detection for class: saab")+
  
  coord_flip()





vechile_data %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Outlier Detection for class: opel")+
  
  coord_flip()







vehicles_bus = vechile_data %>%
  
  filter(class == "bus") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.10, .85)))))



vehicles_van = vechile_data %>%
  
  filter(class == "van") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))



vehicles_opel = vechile_data %>%
  
  filter(class == "opel") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))


vehicles_saab = vechile_data %>%
  
  filter(class == "saab") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .90)))))


vechile_data_modified = bind_rows(list(vehicles_bus,vehicles_van,vehicles_opel,vehicles_saab)) %>%
  
  arrange(samples)



print(vechile_data_modified)



vechile_data_modified %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Transformed Outliers class: 'bus' 1")+
  
  coord_flip()



vechile_data_modified %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Transforme Detection for class: 'van' 1")+
  
  coord_flip()


vechile_data_modified %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Transformed Outliers for class: saab 1")+
  
  coord_flip()



vechile_data_modified %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.colour = "red") +
  
  labs(title = "Transformed Detection for class: 'opel' 1")+
  
  coord_flip()

  
  


is.null(vechile_data_modified)




# Remove the sample name and the class name. Both of these will be remove 

#numerical data is left for the algorithm.


vehicles_data_points = vechile_data_modified
vehicles_data_points$class <- NULL
vehicles_data_points$samples <- NULL



# Now that we have the "vehicles_data_points" dataset, scaling is performed

vehicles_scaled = vehicles_data_points %>%

 mutate(across(everything(), scale)) #scale all values


vehicles_scaled
summary(vehicles_scaled)


set.seed(123)
library(cluster)
kmeans <- kmeans(vehicles_scaled, centers = 2)
str(kmeans)
#install.packages("factoextra")
library("factoextra")
fviz_cluster(kmeans, data = vehicles_scaled)

column<-factor(vechile_data_modified$class)
as.numeric(column)
vehicles_numeric<-as.numeric(column)
library(caret)
conf_matrix<-confusionMatrix(data = as.factor(kmeans$cluster),reference = as.factor(vehicles_numeric))
print(conf_matrix)




##PCA
vehicles_data_points1 = vechile_data_modified
vehicles_data_points1$class <- NULL
vehicles_data_points1$samples <- NULL

vehicles_scaled1 = vehicles_data_points1 



library("FactoMineR")
library(psych)
pca_vehicles_scaled <- prcomp(vehicles_scaled1 , scale. = TRUE)

summary(pca_vehicles_scaled$x)
pca_vehicles_scaled
str(pca_vehicles_scaled)

library(factoextra)
fviz_pca_var(pca_vehicles_scaled,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
kc <- kmeans(pca_vehicles_scaled$x , 2)
plot(pca_vehicles_scaled,col=factor(kc$cluster))



library(caret)
conf_matrix1<-confusionMatrix(data = as.factor(kc$cluster),reference = as.factor(vehicles_numeric))
print(conf_matrix1)



#kmean attempt 2
set.seed(123)
library(cluster)
kmeans.1 <- kmeans(vehicles_scaled,center = 3,iter.max = 10)
str(kmeans.1)
#install.packages("factoextra")
library("factoextra")
fviz_cluster(kmeans.1, data = vehicles_scaled)


library(caret)
conf_matrix2<-confusionMatrix(data = as.factor(kmeans.1$cluster),reference = as.factor(vehicles_numeric))
print(conf_matrix2)



#kmean attempt 3
set.seed(123)
library(cluster)
kmeans.3 <- kmeans(vehicles_scaled, center = 4,iter.max = 5 )
str(kmeans.3)
#install.packages("factoextra")
library("factoextra")
fviz_cluster(kmeans.3, data = vehicles_scaled)


library(caret)
conf_matrix3<-confusionMatrix(data = as.factor(kmeans.3$cluster),reference = as.factor(vehicles_numeric))
print(conf_matrix3)







#automated  centers in cluster using kmeans 


set.seed(233)
library(cluster)

kmeans.2 <- eclust(vehicles_scaled, "kmeans", nstart = 25)
str(kmeans.2)
#install.packages("factoextra")
library("factoextra")
fviz_cluster(kmeans.2, data = vehicles_scaled)








set.seed(1235)
# Perform the kmeans using the NbClust function

# Use Euclidean for distance

cluster_euclidean = NbClust(vehicles_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")


# Use manhattan for distance

cluster_manhattan = NbClust(vehicles_scaled,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")




