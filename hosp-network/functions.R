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
  
  get_dist <- function(origin, target){
    
    return(filter(target_points, `Received From` == origin, names == target)[['Patients Received']])    
  }
  
  
  for (i in 1:jumps){
    if (i == 1){
      
      origin_point<-hospital_df %>%
        filter(names == origin_name) %>%
        mutate(map_icon = as.character(i))
      
      target_names <- close_checker(origin = origin_point$names, distance_matrix = distance_matrix, count = scans)
      
      attrition_vals <- num_splitter(current_count, length(target_names$target)+1)
      
      origin_point <- origin_point %>%
        mutate(stayed = attrition_vals[1])
      
      target_points <- hospital_df %>%
        filter(names %in% target_names$target) %>%
        mutate(map_icon = as.character(i+1)) %>% 
        cbind(attrition_vals[2:length(attrition_vals)]) %>%
        rename(`Patients Received` = attrition_vals.2.length.attrition_vals..) %>%
        mutate(`Patients Stayed` = 0, `Received From` = origin_name, `Total Patients Received` = `Patients Received`) 
      
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
        
        
        
        if (nrow(target_names) != 0){ 
          
          attrition_vals <- num_splitter(filter(target_points, names == o)[['Total Patients Received']], length(target_names$target)+1)
          target_points[target_points$names==o, 'Patients Stayed'] <- attrition_vals[1]
          
          target_points_temp <- target_points_temp %>%
            cbind(attrition_vals[2:length(attrition_vals)]) %>%
            rename(`Patients Received` = attrition_vals.2.length.attrition_vals..) %>%
            mutate(`Patients Stayed` = 0, `Received From` = o, `Total Patients Received` = 0)
          
          target_points <- rbind(target_points, target_points_temp)
          
          pre_lines <- target_names %>% mutate(ls = map2(origin, target, linestring_func))
          lines_temp<- st_sf(origin = pre_lines$origin,
                             target=pre_lines$target,
                             geometry = st_sfc(pre_lines$ls, crs = 4326),
                             dim = 'XY',
                             row.names = NULL)
          
          lines <- rbind(lines, lines_temp)
        }
        
        if (nrow(target_names) == 0){
          target_points[target_points$names==o, 'Patients Stayed'] <- filter(target_points, names == o)[['Patients Received']]
        }
        
        used_targets <- append(used_targets, as.character(target_names$target))
        
      }
      
      bads<-append(bads, as.character(used_targets))
      target_points <- target_points %>% group_by(names) %>% mutate(`Total Patients Received` = sum(`Patients Received`))
    }
  }
  lines<-lines %>% 
    mutate(`Patients Transferred` = map2_dbl(origin, target, get_dist))
  return(list(origin_point = origin_point, target_points = target_points, lines = lines))
  
}

num_splitter<- function(val, segs){
  op <- rmultinom(n = 1, size = val, prob = rep(1/segs, segs))
  return(op)
}

make_line_label <- function(index, df){
  f <- slice(df, index)
  
  label<- sprintf(
    "<strong>Origin:</strong> %s<br/> <strong>Destination:</strong> %s<br/> <strong>Patients Transferred:</strong> %g</sup>",
    f$origin, f$target, f[['Patients Transferred']]
  )%>% lapply(htmltools::HTML)
  
  return(label)
  
}

make_target_labels <- function(df){
  
  df<-df %>% mutate(outgoing_transfer = `Total Patients Received` - `Patients Stayed`)
  
  label<- sprintf(
    "<strong>Hospital Name:</strong> %s<br/> <strong>Patients Received:</strong> %s<br/> <strong>Patients Discharged to Home:</strong> %s<br/> <strong>Patients Transferred Further:</strong> %s</sup>",
    df$names, df[['Total Patients Received']], df[['Patients Stayed']], df$outgoing_transfer
  )%>% lapply(htmltools::HTML)
  
  return(label)
  
}
  