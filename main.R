###### WUR Geo-scripting Course
### Exercise 6
### January 31st 2017
### Paulo Bernardino

#Load libraries
library(sp)
library(rgdal)
library(rgeos)

#Create "data" directory
dir.create("data",showWarnings=FALSE)

#Download files
download.file("http://www.mapcruzin.com/download-shapefile/netherlands-railways-shape.zip",destfile = "netherlands-railways-shape.zip", method="auto")
download.file("http://www.mapcruzin.com/download-shapefile/netherlands-places-shape.zip",destfile = "netherlands-places-shape.zip", method="auto")

#Extract files
unzip("netherlands-places-shape.zip", exdir="data")
unzip("netherlands-railways-shape.zip", exdir="data")

#Read railways shapefile
dsn = file.path("data", "railways.shp")
rw<- readOGR(dsn,layer=ogrListLayers(dsn))
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
rwrd<- spTransform(rw, prj_string_RD)

#Visualize railways shapefile
class(rwrd)
plot(rwrd)
length(rwrd)

#Subseting only industrial railways
ind_rw <- rwrd[rwrd@data$type=="industrial",]
plot(ind_rw,add=TRUE,col="red")


#Buffer the industrial railways with a 1000m buffer
ind_buf<- gBuffer(ind_rw,width=1000,byid=TRUE)
plot(ind_buf)
plot(ind_rw,col="red",add=TRUE)

#Read places shapefile
dsn2 = file.path("data", "places.shp")
places<- readOGR(dsn2,layer=ogrListLayers(dsn2))
places_rw<- spTransform(places, prj_string_RD)

#Visualize places
class(places_rw)
plot(places_rw)

#Visualize the intersection
plot(ind_buf)
plot(ind_rw,col="red",add=TRUE)
plot(places_rw,col="dark green",add=TRUE)

#Select the place that intersects with the buffer
int_place <- gIntersection(places_rw, ind_buf, id=as.character(places_rw$osm_id), byid=T)
int_place <- places_rw[places_rw$osm_id == rownames(int_place@coords),]

class(int_place)
str(int_place)

#Extracting the name of the city
head(int_place)
city_name<- int_place$name[1]
city_name

#Plotting the buffer, the point and the city name
plot(ind_buf,main=city_name)
plot(int_place,col="red",add=T)

#Extracting city population
city_pop<- int_place$population[1]
city_pop

#Printing name and population of the city
paste("The population of",city_name,"is",city_pop)
