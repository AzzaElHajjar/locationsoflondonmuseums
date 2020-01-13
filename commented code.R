#import libraries
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(rgdal)
#read the csv
museum<-read.csv("/Users/macbookpro/Desktop/museums data/Museums_and_public_galleries.csv")
#convert relevant info to a data frame
museum_df<-data.frame(museum$name,museum$longitude,museum$latitude)
#clean data by removing NA values
museum_df_cl<-na.omit(museum_df)
#convert datafram to a spatial point dataframe 2:3 are colums with x,y values in this case
museum_spdf<-SpatialPointsDataFrame(museum_df_cl[,2:3],museum_df_cl)
#remove duplicates
#note you may need to install them first...
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
##First, get the London Borough Boundaries
EW <- geojson_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_4.geojson", what = "sp")
BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]
#plot it using the base plot function
qtm(BoroughMap)
summary(BoroughMap)
#now set up an EPSG string to help set the projection 
BNG = "+init=epsg:27700"
WGS = "+init=epsg:4326"
BoroughMapBNG <- spTransform(BoroughMap,BNG)
#set map view to interactive viewing
tmap_mode("view")
tm_shape(BoroughMapBNG) + tm_polygons(col = NA, alpha = 0.5) + tm_shape(museum_spdf) + tm_dots(col = "red")

#create ppp object
museum.ppp <- ppp(x= museum_spdf@coords[,1], y=museum_spdf@coords[,2],c(-0.48,0.18), c(51.32,51.66))
##Warning message:158 points were rejected as lying outside the specified window

#plot the ppp object within the specified window
plot(museum.ppp,pch=16,cex=0.5, main="Museums in London")
##Warning message:In plot.ppp(museum_spdf.ppp, pch = 16, cex = 0.5, main = "Museums in London") : 158 illegal points also plotted

#pixellate the ppp object as a first assessment of density
pixellate.ppp(museum.ppp)

#produce Kernel Density Estimation (KDE) map from the ppp object for further study of density
plot(density(museum.ppp), sigma =500)

#Ripley’s K test on data with the spatstat package using the kest function
K <- Kest(museum.ppp, correction="border", rmax = 0.25)
plot(K)





