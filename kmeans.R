library(ggplot2)
library(fossil)
library(geosphere)
library(geodist)
library(raster)

##Import Data
data_eoc <- read.csv("data.csv", sep = ",")

##Create a Datafarme
df <- data.frame(long = data_eoc$Longitude, lat = data_eoc$Latitude, state = data_eoc$Province, result = data_eoc$AfterInvestigationTrueAFP)

##Distance Matrix
d <- geodist(df)

#hierarchiacal clustering
hclustfunc <- function(d, method = "complete", dmeth = "euclidean") {
  hclust(dist(d, method = dmeth), method = method)
}

fit <- hclustfunc(data_eoc)

##Dendogram
plot(fit)

df$clust <- cutree(fit, k=4)

##Import shapefile
map.NG <- shapefile("C:/Users/Joseph.Oladokun/Desktop/eoc_R/Machine_Learning_Spatial/NIR-level_1.shp")
map.state <- map.NG[map.NG$NAME=="Province"]
map.df <- fortify(map.state)
map.df

##Visualise the clusters
ggplot(map.df)
ggplot(map.df) +
  geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = df, aes(x = long, y = lat, color = factor(result)), size = 2) +
  scale_color_discrete("Cluster") +
  coord_fixed()

##Kmeans
km <- kmeans(d, centers = 2)
df$clust <- km$cluster
km
