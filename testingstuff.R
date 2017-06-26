install.packages("githubinstall") # if you have not installed "devtools" package
install.packages("viridis")
library(githubinstall)
library(sf)
library(ggplot2)
library(devtools)
library(dplyr)
test <- plot(muni$geometry[muni$NAME == 'BARRINGTON'])
b<-'BARRINGTON'
bound<-muni$geometry[muni$NAME == 'BARRINGTON']
plot(buff)
road<-
unique(lulc$lu[lulc])

outp<-lulc %>%
  st_intersection(bound) 
asdf <- st_intersection(lu,bont)

duff <- vector()
streem <- streams %>% st_intersection(bound)
luu <- lu %>% st_intersection(bound)
plot

test <- plot(muni$geometry[muni$NAME == 'BARRINGTON'])
test<- test + plot(outp, fill = 'lu', add=TRUE)

unique(lulc$Descr_2011)

col()
bont <-st_transform(bound, 4326)
ggplot() +
 geom_sf(data = subset(muni, NAME == 'BARRINGTON')) +
  geom_sf(data = streem, col = 'blue') +
   theme(legend.position="none")

ggplot(streams$geometry)

gh_install_packages("ggplot2", ref = "sf")


lu <- st_read("ridata/Land_Use_2025.shp")
road<- st_read('ridata/RIDOT_Roads_2016.shp')
unique(lu$Map_Legend)
plot(lu)



save(busroutes, census, growth_cent, lu, lulc, muni, pond, road, streams, file='data.RData')

saveRDS(busroutes, file = "busroutes.rds")
saveRDS(census, file = "census.rds")
saveRDS(growth_cent, file = "growth_cent")
saveRDS(lu, file = "lu.rds")
saveRDS(lulc, file = "lulc.rds")
saveRDS(muni, file = "muni.rds")
saveRDS(pond, file = "pond.rds")
saveRDS(road, file = "road.rds")
saveRDS(streams, file = "streams.rds")




growth_cent <- st_read("ridata/growth06.shp")
streams <- st_read("ridata/streams.shp")
muni <- st_read("ridata/muni97d.shp")
lulc <- st_read("ridata/rilc11d.shp")
lulc$lu<-as.character(lulc$Descr_2011)
growth_cent <- st_transform(growth_cent, 4326)
muni <- st_transform(muni, 4326)
streams <- st_transform(streams, 4326)
lulc <- st_transform(lulc,4326)
lu <- st_read("ridata/Land_Use_2025.shp")
road<- st_read('ridata/RIDOT_Roads_2016.shp')
busroutes <- st_read('ridata/RIPTA_Bus_Routes.shp')
pond <- st_read('ridata/Rhode_Island_Ponds_and_Lakes.shp')
census <- st_read('ridata/US_Census_2010_Summary_File_1_Indicators.shp')

bound<-tmuni$geometry[muni$NAME == 'PROVIDENCE']
streem <- tstreams %>% st_intersection(bound)
rroads <- road %>% st_intersection(bound)
luu <- lu %>% st_intersection(bound)
busrts<- busroutes %>% st_intersection(bound)
buslables <- data.frame(name = busrts$RT_NAME, x=NA, y=NA)
for(i in unique(busrts$RT_NAME)){
  
}
busz <- busrts %>%
  group_by(RT_NAME)
ggplot() +
  geom_sf(data = subset(tmuni, NAME == 'PROVIDENCE')) +
  geom_sf(data = luu, aes(fill = Map_Legend))+
  geom_sf(data = streem, col = 'blue') +
  #geom_sf(data = rroads, col = 'brown') +
  geom_sf(data = busrts, aes(color = RT_NAME, fill = RT_NAME)) +
  geom_text(data = busrts) +
  scale_fill_viridis("Map_Legend")
  theme(legend.position="none")

  setwd('/Mapproj/')