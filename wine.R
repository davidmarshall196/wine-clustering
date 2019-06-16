
# Red Wine Quality
library(readr)
data <- read_delim("winequality-red.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

# Rename some columns
library(dplyr)
data <- data %>% rename(fixed_acidity = `fixed acidity`, vol_acidity = `volatile acidity`,
                        citric_acid = `citric acid`, res_sugar = `residual sugar`,
                        free_dioxide = `free sulfur dioxide`, 
                        tot_dioxide = `total sulfur dioxide`)

# Save the quality
quality <- data %>% select(quality)

# Remove from main data
data <- data %>% select(-quality)

# SUmmarise
library(skimr)
skim(data)

# Impute the missing
library(mice)
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500, verbose )
data <- complete(tempData,1)
skim(data)

# plot some of the data with simple scatters
library(ggplot2)
ggplot(data, aes(x = fixed_acidity, y = res_sugar)) +
    geom_point(colour = "blue") 
ggplot(data, aes(x = citric_acid, y = chlorides)) +
  geom_point(colour = "red") 
ggplot(data, aes(x = pH, y = density)) +
  geom_point(colour = "green") 

# scale the data
scaled <- scale(data)

# WSS
set.seed(2019)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(scaled, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
library(purrr)
wss_values <- map_dbl(k.values, wss)

wss <- plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
wss

# Other approach
library(factoextra)
fviz_nbclust(scaled, kmeans, method = "silhouette")
fviz_nbclust(scaled, kmeans, method = "wss")

# Gap stat
library(cluster)
gap_stat <- clusGap(scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)



# Cluster
cluster_3 <- kmeans(scaled, centers = 3, nstart = 50)
print(str(cluster_3))

# Bind back
data <- cbind(data, cluster_3$cluster)

# Rename
data <- data %>% rename(cluster = `cluster_3$cluster`)

# Cluster sizes
print(data %>% select(cluster) %>% group_by(cluster) %>% 
        summarise(count = n()))

# cluster means
print(data %>% group_by(cluster) %>% summarise_all(mean))


# plot pca
library(factoextra)
fviz_cluster(cluster_3, data)

# There is quite a lot of overlap
data %>%
  as_tibble() %>%
  ggplot(aes(pH, density, color = factor(cluster), label = cluster)) +
  geom_text()

# Can plot them all
cluster_4 <- kmeans(scaled, centers = 4, nstart = 25)
cluster_5 <- kmeans(scaled, centers = 5, nstart = 25)
cluster_6 <- kmeans(scaled, centers = 6, nstart = 25)

# plots to compare
p1 <- fviz_cluster(cluster_3, geom = "point", scaled) + ggtitle("k = 3")
p2 <- fviz_cluster(cluster_4, geom = "point",  scaled) + ggtitle("k = 4")
p3 <- fviz_cluster(cluster_5, geom = "point",  scaled) + ggtitle("k = 5")
p4 <- fviz_cluster(cluster_6, geom = "point",  scaled) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Now do some 3d plots
library(plotly)
p3d <- plot_ly(data, x = ~fixed_acidity, y = ~res_sugar, z = ~density, 
               color = ~factor(cluster), 
               colors = c('#BF382A', '#0C4B8E', "greenyellow")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'fixed acidity'),
                      yaxis = list(title = 'residual sugar'),
                      zaxis = list(title = 'density')))
p3d



print(data %>% group_by(cluster) %>% summarise_all(mean))

# Now do some 3d plots
p3d2 <- plot_ly(data, x = ~free_dioxide, y = ~citric_acid, z = ~vol_acidity, 
               color = ~cluster, 
               colors = c('#BF382A', '#0C4B8E', "greenyellow")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Free Sulphur Dioxide'),
                      yaxis = list(title = 'Citric acid concentration'),
                      zaxis = list(title = 'Volatile acidity')))
p3d2

# 3d plot 2
p3d3 <- plot_ly(data, x = ~chlorides, y = ~alcohol, z = ~pH, 
               color = ~cluster, 
               colors = c('#BF382A', '#0C4B8E', "greenyellow")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Chlorides'),
                      yaxis = list(title = 'Alcohol Level'),
                      zaxis = list(title = 'pH Level')))
p3d3

# Finally, compare with old clusters
quality <- quality %>% mutate_if(is.integer, as.factor)
library(Hmisc)
describe(quality)









