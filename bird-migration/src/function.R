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
  
  output <- st_sf(start = rep(origin, interval),  end = rep(target, interval), geometry = st_as_sfc(lsdf$test, crs = 4326))
  
  
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
  #df<-st_sf(id = 'middle_line', geometry = clinethree, dim = 'XY', row.names = NULL)
  
  return(clinethree)
}

get_line_color <- function(n){
  between <- function(x, a, b) a < x && x <= b
  
  dplyr::case_when(
    between(n, 0, 5) ~'#0765ff',
    between(n, 5, 10)~'#003ea3',
    between(n, 10, 15) ~'#003ea3',
    between(n, 15, 20) ~'#00ff0c',
    between(n, 20, 25) ~'#00ff0c',
    between(n, 25, 30) ~'#03c413',
    between(n, 30, 35) ~'#03c413',
    between(n, 35, 40) ~'#008c06',
    between(n, 40, 45) ~'#008c06',
    between(n, 45, 50) ~'#ff8484',
    between(n, 50, 55) ~'#ff8484',
    between(n, 55, 60) ~'#ff0000',
    between(n, 60, 65) ~'#ff0000',
    between(n, 65, 70) ~'#af0000',
    between(n, 70, 75) ~'#af0000',
    between(n, 76, 80) ~'#ffcb00',
    between(n, 80, 85) ~'#ffcb00',
    between(n, 85, 90) ~'#a58b24',
    between(n, 90, 95) ~'#a58b24',
    between(n, 95, 100) ~'#595828',
    between(n, 100, 105) ~'#595828',
    between(n, 105, 110) ~'#72a7ff',
    between(n, 110, 115) ~'#72a7ff',
    between(n,115, 120) ~'#0765ff',
  )
}
