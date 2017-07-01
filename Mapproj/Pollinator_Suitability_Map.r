##Pollinator suitability by Max Pohlman

## This script creates the map located at .
## All the data is downloaded within this script.

#Required Packages
library(units)
library(geosphere)
library(PBSmapping)
library(spatstat)
library(sp)
library(rgeos)
library(grDevices)
library(shinythemes)
library(rLiDAR)
library(shiny)
library(sf)
library(dplyr)
library(maptools)
library(rgdal)
library(raster)
library(EBImage)

#Makes data folder
if(!dir.exists("azdata")){dir.create("azdata")}

##############################
# Gets boundary data         #
##############################

download.file(url = 'http://www.rigis.org/geodata/bnd/muni97d.zip',
              destfile = "azdata/a.zip")
unzip(zipfile = "azdata/a.zip",
      exdir = "azdata")
muni <- st_read("azdata/muni97d.shp")
muni <- subset(muni,NAME=='EAST GREENWICH')
muni <- st_transform(muni,4326)
##############################
# Gets elevation data        #
##############################

#Downloads the contour files
dls<-c('0320001800_cntr10ft.zip',
       '0310001900_cntr10ft.zip',
       '0320001900_cntr10ft.zip',
       '0310002000_cntr10ft.zip',
       '0320002000_cntr10ft.zip',
       '0330002000_cntr10ft.zip',
       '0310002100_cntr10ft.zip',
       '0320002100_cntr10ft.zip',
       '0330002100_cntr10ft.zip',
       '0340002000_cntr10ft.zip',
       '0340002100_cntr10ft.zip',
       '0330001800_cntr10ft.zip',
       '0340001800_cntr10ft.zip',
       '0330001900_cntr10ft.zip',
       '0340001900_cntr10ft.zip')
for (i in dls){
  download.file(url = paste("http://www.rigis.org/geodata/topo/2011/Contour10ft/",i, sep=""),
                destfile = "azdata/a.zip")
  unzip(zipfile = "azdata/a.zip",
        exdir = "azdata")
}


##Reads in the files
a<-st_read("azdata/0320001800_cntr10ft.shp")            
b<-st_read("azdata/0310001900_cntr10ft.shp")
c<-st_read("azdata/0320001900_cntr10ft.shp")
d<-st_read("azdata/0310002000_cntr10ft.shp")
e<-st_read("azdata/0320002000_cntr10ft.shp")
f<-st_read("azdata/0330002000_cntr10ft.shp")
g<-st_read("azdata/0310002100_cntr10ft.shp")
h<-st_read("azdata/0320002100_cntr10ft.shp")
i<-st_read("azdata/0330002100_cntr10ft.shp")
j<-st_read("azdata/0340002000_cntr10ft.shp")
k<-st_read("azdata/0340002100_cntr10ft.shp")
l<-st_read("azdata/0330001800_cntr10ft.shp")
m<-st_read("azdata/0340001800_cntr10ft.shp")
n<-st_read("azdata/0330001900_cntr10ft.shp")
o<-st_read("azdata/0340001900_cntr10ft.shp")

# Merges them all together
z<-rbind(a,b)
z<-rbind(z,c)
z<-rbind(z,d)
z<-rbind(z,e)
z<-rbind(z,f)
z<-rbind(z,g)
z<-rbind(z,h)
z<-rbind(z,i)
z<-rbind(z,j)
z<-rbind(z,k)
z<-rbind(z,l)
z<-rbind(z,m)
z<-rbind(z,n)
z<-rbind(z,o)

#converts into polygons. Assigns a binary variable if
# over 125 feet
elevation_data <- z %>% st_cast("POLYGON")
#Must have LWGEOM installed. Otherwise export to arc/qgis 
#and make valid there.
elevation_data<-st_make_valid(elevation_data)


#st_write(elevation_data, 'tofix.shp')
#elevation_data<-st_read('fixed_elevation.shp')


elevation_data$lvl[elevation_data$Contour>125]<-1
elevation_data <- st_transform(elevation_data,4326)
elevation_data <- elevation_data %>% st_intersection(muni)

##############################
# Gets soil data             #
##############################
download.file(url = 'http://www.rigis.org/geodata/soil/Soils16.zip',
              destfile = "azdata/a.zip")
unzip(zipfile = "azdata/a.zip",
      exdir = "azdata")
soil <- st_read("azdata/Soils16.shp")
soil <- st_transform(soil,4326)
soil <- soil %>% st_intersection(muni)

##############################
# Gets land use data         #
##############################
download.file(url = 'http://www.rigis.org/geodata/plan/rilc11d.zip',
              destfile = "azdata/a.zip")
unzip(zipfile = "azdata/a.zip",
      exdir = "azdata")
lulc <- st_read("azdata/rilc11d.shp")
lulc <- st_transform(lulc,4326)
lulc <- lulc %>% st_intersection(muni)

##############################
# Gets development data      #
##############################
download.file(url = 'http://www.rigis.org/geodata/plan/landuse2025.zip',
              destfile = "azdata/a.zip")
unzip(zipfile = "azdata/a.zip",
      exdir = "azdata")
dev <- st_read("azdata/landuse2025.shp")
dev <- st_transform(dev,4326)
dev <- dev %>% st_intersection(muni)


##############################
# Gets road data             #
#############################
download.file(url = 'http://www.rigis.org/geodata/trans/RIDOTrds16.zip',
              destfile = "azdata/a.zip")
unzip(zipfile = "azdata/a.zip",
      exdir = "azdata")
roads <- st_read("azdata/RIDOTrds16.shp")
roads <- st_transform(roads, 4326)
roads <- roads %>% st_intersection(muni)

## AUTHOR'S NOTE:
# This is a very roundabout way of calculating line density, since to my knowledge there is no function in the sf package
conv <- as(roads,'Spatial') # Converts to sp Spatial object
convv<- as.psp(conv) # Converts to psp object
px <- pixellate.psp(convv, eps=.0001) #Pixelates the object as an image, calculates line density
rLength <- raster(px) #converts image to raster
outpu<-rasterToPolygons(rLength,dissolve=TRUE) #converts raster to sp object
road_density<-st_as_sf(outpu) #converts sp object to sf object
st_crs(road_density) <- "+proj=longlat +datum=WGS84 +no_defs"
road_densityx<-road_density %>% st_intersection(muni)
##############################
# Gets leaking tank data     #
##############################
download.file(url = 'http://www.rigis.org/geodata/env/LUSTs12.zip',
              destfile = "azdata/a.zip")
unzip(zipfile = "azdata/a.zip",
      exdir = "azdata")
tank <- st_read("azdata/LUSTs12.shp")
tank <- st_transform(tank,4326)
tank <- tank %>% st_intersection(muni)


##############################
# Assign Scores
#############################
elevation_conv <- subset(elevation_data,lvl==1)
elevation_conv$score<-1
elevation_conv <- subset(elevation_conv, select= score)

soil_conv<-subset(soil,FARM_CLS == 'Prime farmland')
soil_conv$score<-3
soil_conv<-subset(soil_conv, select = score)

lulc$score[lulc$Descr_2011 == 'Wetland'] <- 2
lulc$score[(lulc$Descr_2011 == 'Brushland (shrub and brush areas, reforestation)')|(lulc$Descr_2011 == 'Mixed Forest') |
             (lulc$Descr_2011 == 'Softwood Forest (>80% softwood)') | (lulc$Descr_2011 == 'Deciduous Forest (>80% hardwood)')|
             (lulc$Descr_2011 == 'Cropland (tillable)') | (lulc$Descr_2011 == 'Orchards, Groves, Nurseries')] <- 2
lulc_conv<-subset(lulc, score>0, select = score)

undevs <- c('Conservation/Limited', ' Major Parks & Open Space', 'Non-urban Developed', 'Reserve', 'Prime Farmland')
dev$score[dev$Map_Legend %in% undevs] <- 1
dev_conv <-subset(dev, select=score)

road_densityx$score[road_density$layer<mean(road_density$layer)]<-1
roaddensity_conv<-subset(road_densityx, score>0, select = score)

devz<- subset(dev_conv,score==1)

#######################################
# Converts to sp data for rasterizing #
#######################################
muniq<-as(muni,'Spatial')
devzq<-as(devz,'Spatial')
eleq<-as(elevation_conv,'Spatial')
lulcq<-as(lulc_conv,'Spatial')
roadq<-as(roaddensity_conv,'Spatial')
soilq<-as(soil_conv,'Spatial')
tankq<-as(tank,'Spatial')

#######################################
# Rasterizes the data                 #
#######################################

r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(muniq)
devr <- rasterize(devzq, r, 'score')

r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(muniq)
eler <- rasterize(eleq, r, 'score')

r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(muniq)
lulcr <- rasterize(lulcq, r, 'score')

r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(muniq)
roadr <- rasterize(roadq, r, 'score')

r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(muniq)
soilr <- rasterize(soilq, r, 'score')

#######################################
# Downloads icons for map             #
#######################################

download.file('https://vignette3.wikia.nocookie.net/amazingrace/images/5/54/Icon-Hazard.png/revision/latest?cb=20110926234917&format=original', destfile = 'icon1.png', mode = 'wb')
icon4 <- readImage('icon1.png')
icon4<- resize(icon4, dim(icon4)[1]/16)

download.file('http://cliparts101.com/files/743/B6BD4A366B8212D46086FECD5BD40C3B/Compass_Arrow.png', destfile = 'comp.png', mode = 'wb')
comp<-readImage('comp.png')
comp<-rotate(comp,357,bg.col="white")

#######################################
# Compresses layers into one raster   #
#######################################
aa<-brick(devr,eler,lulcr,roadr,soilr)
bb<-sum(aa, na.rm=T)
tankw<-st_buffer(tank, 0.002745946, nQuadSegs = 500)
tankz<-tankw %>% st_intersection(muni)
bb <- focal(bb, w=matrix(1, 3, 3), mean)
#######################################
# Plots the map                       #
#######################################
bg <- mask(bb,muniq, inverse = T)
offset<-.0015
plot(bb, legend.args=list(text='Suitability Score', side = 4 ,cex = 1.3, line = 2))
plot(bg,add=T, col='white', legend = F)
plot(tankz, col = 'red', border = 'red',add=T)
for(i in 1:length(coordinates(tankq))/2){
  if(gContainsProperly(muniq,gBuffer(tankq[i,], width =.0015, quadsegs = 50 ))){
    rasterImage(icon4,coordinates(tankq)[i,1]-offset, coordinates(tankq)[i,2]-offset, coordinates(tankq)[i,1]+offset, coordinates(tankq)[i,2]+offset)
  }      
}
plot(muniq, add=T)
x<- -71.45
y<- 41.61
rasterImage(icon4,x-offset,y-offset,x+offset,y+offset )
text(x, y-.004, "Indicates a Leaking\nUnderground Storage Tank")

title(main = "Plant Here, Save the Bees\nEast Greenwhich, Rhode Island",
      sub = 'This map demonstrates areas in East Greenwhich, RI most suitable for planting pollinator friendly flora and fauna.')
coffset<-.008
cx<- -71.45
cy<- 41.625
rasterImage(comp,cx-coffset,cy-coffset,cx+coffset,cy+coffset)