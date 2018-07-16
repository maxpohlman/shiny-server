# d<-filter(df, common_name == 'Mallard')
# 
# e<-d %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
# 
# f<-st_as_sf(e, coords = c('lng', 'lat'), crs = 4326)
# 
# d<-d %>%
#   mutate(b_icon = map(image_url, ~makeIcon(., iconWidth = 20, iconHeight = 20)))
# 
# 
# icons <- makeIcon('https://static.inaturalist.org/photos/4631438/medium.jpeg?1471807669', iconWidth = 20, iconHeight = 20)
# 
# leaflet() %>% addTiles() %>% addMarkers(data = f, popup = ~month)
# 
# 
# months <- df %>% `st_geometry<-`(NULL) %>% group_by(common_name) %>% summarize(v = length(unique(month)))
# 
#                                         
# 
#z<-lines[1:3,]
# 
# leaflet() %>% addTiles() %>% addPolylines(data = zz)

line_subsetter <- function(geom, origin, target, interval = 5){
  
  coords <- st_coordinates(geom)
  
  init_lng <- coords[[1]]
  init_lat <- coords[[3]]
  final_lng <-coords[[2]]
  final_lat <-coords[[4]]
  
  lng_per_step<- ((final_lng - init_lng) / interval)
  lat_per_step<-((final_lat - init_lat) / interval)
  
  ls_matrix <- matrix(, nrow = interval+1, ncol = 2)
  
  for (i in 1:(interval+1)){
    begin_lng <- init_lng + (i-1)*lng_per_step
    begin_lat <- init_lat + (i-1)*lat_per_step
    
    ls_matrix[i,1]<- begin_lng
    ls_matrix[i,2]<- begin_lat
  }
  
  lsdf<-tibble(test = runif(interval))
  
  lsdf<- lsdf %>%
    mutate(test = map(2:(interval+1), ~st_linestring(ls_matrix[(.-1):.,])))
  
  output <- st_sf(start = rep(origin, interval),  end = rep(target, interval), geometry = st_sfc(lsdf$test, crs = 4326), dim = 'XY', row.names = NULL)
  
  
  return(output)
}

to_lines <- function(df) {
  op<-tibble(origin = character(), target = character())
  for (i in 1:nrow(df)){
    j <- i+1
    if (i == 12){
      j <-1
    }
    org <- df$month[i]
    tar<- df$month[j]
    
    op<-add_row(op, origin = org, target = tar)
  }
  return(op)
}


beta <- function(bf, origin, target){
  first <- bf %>%
    filter(month == origin)
  
  second <- bf %>%
    filter(month == target)
  
  frame<-rbind(first, second)
  frame %>%
    st_coordinates() %>%
    st_linestring() %>%
    return()
}

make_middle_line <- function(){
  
  clineone<-tibble(lng = c(-98.583333,-98.583333),
                   lat = c(48.88, 26.50))
  
  clinetwo<-st_as_sf(clineone, coords = c('lng', 'lat'))
  
  clinethree<-clinetwo %>% st_coordinates() %>% st_linestring() %>% st_sfc(crs = 4326)
  df<-st_sf(id = 'middle_line', geometry = clinethree, dim = 'XY', row.names = NULL)
  
  return(df)
}

