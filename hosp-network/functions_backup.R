network_link <- function(hospital_df, origin_name, current_count, distance_matrix, jumps, scans){
  
  bads <- c()
   bads<- append(bads, origin_name) 
close_checker <- function(distance_matrix, origin, count = 5){
  distance_matrix%>%
    
    #Selects only the origin hospital distance and the name columns
    select(origin, name) %>% 
    
    #Filters to the bottom 6 distances (includes distance between hospital and itself)
    top_n(-(count+1), !!as.name(origin)) %>% 
    
    #Drops the instance where the distance between a hospital and itself is 0
    filter(name != origin) %>% 
    
    #Orders the results
    arrange(!!as.name(origin)) %>% 
    
    #Adds a column with the origin hospital name (passed into the function)
    cbind(hospital = origin) %>%
    
    #Renames columns
    `colnames<-`(c('distance','target','origin')) %>%
    
    return()
  
}

linestring_func <- function(origin, target){
  hospital_df %>%
    
    # Filters to the two names we are connecting
    filter(names == origin | names == target) %>%
    
    # Extracts the raw coordinates of the two points
    st_coordinates() %>%
    
    # Creates a linestring
    st_linestring() %>% 
    return()
}


for (i in 1:jumps){
  if (i == 1){
  origin_point<-hospital_df %>%
    filter(names == origin_name) %>%
    mutate(map_icon = as.character(i))
  
  target_names <- close_checker(origin = origin_point$names, distance_matrix = distance_matrix, count = 8)
  
  target_points <- hospital_df %>%
    filter(names %in% target_names$target) %>%
    mutate(map_icon = as.character(i+1))
  
  pre_lines <- target_names %>% mutate(ls = map2(origin, target, linestring_func))
  lines<- st_sf(origin = pre_lines$origin,
                target=pre_lines$target,
                geometry = st_sfc(pre_lines$ls, crs = 4326),
                dim = 'XY',
                row.names = NULL)
  bads <- append(bads, c(origin_name, as.character(target_names$target)))
  }
  else{
    if (i == 2){
      origins <- target_names$target
      used_targets <- c()
    }    
    else{
      origins <- used_targets
      used_targets <- c()
    }
    
    for (o in unique(origins)){
      
      target_names <- close_checker(origin = o, distance_matrix = distance_matrix, count = scans)
      target_names <- target_names %>%
        filter(!as.character(target) %in% bads)
      target_points_temp <- hospital_df %>%
        filter(names %in% target_names$target) %>%
        mutate(map_icon = as.character(i+1))
      
      target_points <- rbind(target_points, target_points_temp)
      
      if (nrow(target_names) != 0){  
      pre_lines <- target_names %>% mutate(ls = map2(origin, target, linestring_func))
      lines_temp<- st_sf(origin = pre_lines$origin,
                    target=pre_lines$target,
                    geometry = st_sfc(pre_lines$ls, crs = 4326),
                    dim = 'XY',
                    row.names = NULL)
      
      lines <- rbind(lines, lines_temp)
      }
      used_targets <- append(used_targets, as.character(target_names$target))
      
    }
    bads<-append(bads, as.character(used_targets))
  }
}
  return(list(origin_point = origin_point, target_points = target_points, lines = lines))
    
}




num_splitter<- function(val, segs){
  op <- rmultinom(n = 1, size = val, prob = rep(1/segs, segs))
  return(op)
}


