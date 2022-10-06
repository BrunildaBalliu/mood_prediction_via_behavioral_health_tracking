##########

clusterCheck <- function(trajectory_summary, cluster, prev_cluster, prev_stop, params){
  require("geosphere")
  if(is.null(prev_cluster) && is.null(prev_stop)){
    prev_cluster <- cluster
  }
  else if(is.null(prev_cluster)){
    time_interval = cluster$start_time - prev_stop$end_time
    dist_between = distHaversine(c(cluster$lng,cluster$lat),c(prev_stop$lng,prev_stop$lat))
    if((time_interval <= params$stop_time_threshold) && (dist_between<=params$stop_dist_threshold)){
      prev_stop <- mergeCluster(prev_stop, cluster)
    }
  }
  else{
    time_interval = cluster$start_time - prev_cluster$end_time
    dist_between = distHaversine(c(cluster$lng,cluster$lat),c(prev_cluster$lng,prev_cluster$lat))
    if((time_interval <= params$stop_time_threshold) && (dist_between<=params$stop_dist_threshold)){
      prev_cluster <- mergeCluster(prev_cluster, cluster)
      if(prev_cluster$duration >= params$stop_time_threshold){
        results <- addStopPoint(trajectory_summary, prev_cluster, prev_stop, params)
        trajectory_summary <- results[[1]]
        cluster <- results[[2]]
        prev_cluster <- results[[3]]
        prev_stop <- results[[4]]
      }
    }
  }
  cluster <- NULL
  return(list(trajectory_summary, cluster, prev_cluster, prev_stop))
}

addClusterPoint <- function(cluster, point) {
  cluster_cnt = length(cluster$points)
  cluster$points[cluster_cnt+1] = point$id
  cluster$end_time = point$time
  cluster$duration = cluster$end_time-cluster$start_time
  cluster$lng = (cluster_cnt * cluster$lng + point$lng)/(cluster_cnt+1)
  cluster$lat = (cluster_cnt * cluster$lat + point$lat)/(cluster_cnt+1)
  return(cluster)
}

addStopPoint<- function(trajectory_summary, cluster, prev_stop, params){
  require("geosphere")
  #print(paste("Adding stop point"))
  if(is.null(prev_stop)){
    prev_stop <- cluster
  }
  else{
    dist_between = distHaversine(c(cluster$lng,cluster$lat),c(prev_stop$lng,prev_stop$lat))
    if(dist_between <= params$stop_dist_threshold){
      prev_stop <- mergeCluster(prev_stop, cluster)
    }
    else {
      if(!is.null(prev_stop)) trajectory_summary <- logStop(trajectory_summary, prev_stop)
      prev_stop <- cluster
    }
  }
  prev_cluster <- NULL
  cluster <- NULL

  return(list(trajectory_summary, cluster, prev_cluster, prev_stop))
}

logStop <- function(trajectory_summary, cluster){
  if(nrow(trajectory_summary$overview) == 0) {
    cluster_id = 1
  } else {
    cluster_id = max(trajectory_summary$overview$period_id)+1
  }
  
  trajectory_summary$overview[nrow(trajectory_summary$overview)+1,]= list(cluster_id,
                                                                          "stop",
                                                                          cluster$start_time,
                                                                          cluster$end_time,
                                                                          cluster$end_time-cluster$start_time,
                                                                          cluster$lng,
                                                                          cluster$lat,
                                                                          NA, 
                                                                          NA,
                                                                          NA,
                                                                          NA,
                                                                          NA,
                                                                          NA,
                                                                          length(cluster$points))
  
  
  trajectory_summary$points_to_stops[unlist(cluster$points)] <- cluster_id
  return(trajectory_summary)
}

logGap <- function(trajectory_summary, start_time, end_time){
  if(nrow(trajectory_summary$overview) == 0) {
    cluster_id = 1
  } else {
    cluster_id = max(trajectory_summary$overview$period_id)+1
  }
  
  trajectory_summary$overview[nrow(trajectory_summary$overview)+1,]= list(cluster_id,
                                                                          "gap",
                                                                          start_time,
                                                                          end_time,
                                                                          end_time-start_time,
                                                                          NA,
                                                                          NA,
                                                                          NA, 
                                                                          NA,
                                                                          NA,
                                                                          NA,
                                                                          NA,
                                                                          NA,
                                                                          0)
  
  return(trajectory_summary)
}

mergeCluster <- function(prev_cluster, cluster){ 
  prev_cluster_cnt = length(prev_cluster$points)
  cluster_cnt = length(cluster$points)
  
  prev_cluster$points <- append(prev_cluster$points, cluster$points)
  prev_cluster$end_time <- cluster$end_time
  prev_cluster$duration <- prev_cluster$end_time - prev_cluster$start_time
  
  prev_cluster$lng = (cluster_cnt * cluster$lng + prev_cluster_cnt * prev_cluster$lng) / (cluster_cnt + prev_cluster_cnt)
  prev_cluster$lat = (cluster_cnt * cluster$lat + prev_cluster_cnt * prev_cluster$lat) / (cluster_cnt + prev_cluster_cnt)
  
  return(prev_cluster)
}

initializeCluster <- function(point){
  
  cluster <- list(lng = double(),
                  lat = double(),
                  start_time = double(),
                  end_time = double(),
                  duration = double(),
                  points = list())
  cluster$points[1] = point$id
  cluster$lng <- point$lng
  cluster$lat <- point$lat
  cluster$start_time <- point$time
  cluster$end_time <- point$time
  cluster$duration <- 0
  
  return(cluster)
}



initTrajectorySummary <- function(trajectory){
  overview <-data.frame(period_id = integer(),
                        type = character(),
                        start_time = double(),
                        end_time = double(),
                        duration = double(),
                        stop_lng = double(),
                        stop_lat = double(),
                        stop_max_dist = double(),
                        move_start_lng = double(),
                        move_start_lat = double(),
                        move_end_lng = double(),
                        move_end_lat = double(),
                        move_distance =double(),
                        num_points = integer(),
                        stringsAsFactors = FALSE)
  
  points_to_stops <- rep(-1,nrow(trajectory))
  
  trajectory_summary<-list(overview = overview , points_to_stops = points_to_stops)
  
  return(trajectory_summary)
}




summarize_trajectory <- function(trajectory, params){
  require("dplyr")
  require("geosphere")
  
  trajectory = data.frame(trajectory)
  
  trajectory_summary <- initTrajectorySummary(trajectory)
  
  movement <- list(start_lng = double(),
                   start_lat = double(),
                   end_lng = double(),
                   end_lat = double(),
                   start_time = double(),
                   end_time = double(),
                   points = list())
  
  gap <- list(start_time = double(),
              end_time = double(),
              points = list())
  
  point <- list(id = integer(),
                lng = double(),
                lat = double(),
                time = double())
  
  
  
  point$id = 1
  point$time = trajectory[1,1]
  point$lng = trajectory[1,2]
  point$lat = trajectory[1,3]
  num_points = nrow(trajectory)
  cluster <- initializeCluster(point)
  prev_cluster <- NULL
  prev_stop <- NULL
  prev_point <- NULL
  
  for(i in 2:num_points){
    point$id = i
    point$time = trajectory[i,1]
    point$lng = trajectory[i,2]
    point$lat = trajectory[i,3]
    
    dist_cluster = distHaversine(c(point$lng,point$lat),c(cluster$lng,cluster$lat))
    time_interval <- point$time - cluster$end_time
    ##### Gap handling
    if(time_interval > params$gap_interval_threshold){
      start_time <- cluster$end_time
      end_time <- point$time
      
      if(cluster$duration >= params$stop_time_threshold){
        # Point out of range after threshold
        results <- addStopPoint(trajectory_summary, cluster, prev_stop, params)
        trajectory_summary <- results[[1]]
        cluster <- results[[2]]
        prev_cluster <- results[[3]]
        prev_stop <- results[[4]]
      } else{
        # Point out of range before threshold
        results <- clusterCheck(trajectory_summary, cluster, prev_cluster, prev_stop, params)
        trajectory_summary <- results[[1]]
        cluster <- results[[2]]
        prev_cluster <- results[[3]]
        prev_stop <- results[[4]]
      }
      
      if(!is.null(prev_stop)){
        trajectory_summary <- logStop(trajectory_summary, prev_stop)
        prev_stop <- NULL
      }
      
      cluster <- initializeCluster(point)
      trajectory_summary <- logGap(trajectory_summary, start_time, end_time)
    } 
    ##### Non -gap handling
    else {
      if(dist_cluster <= params$stop_dist_threshold) {
        # Point within threshold
        cluster <- addClusterPoint(cluster, point)
      } else {
        if(cluster$duration >= params$stop_time_threshold){
          # Point out of range after threshold
          results <- addStopPoint(trajectory_summary, cluster, prev_stop, params)
          trajectory_summary <- results[[1]]
          cluster <- results[[2]]
          prev_cluster <- results[[3]]
          prev_stop <- results[[4]]
        } else{
          # Point out of range before threshold
          results <- clusterCheck(trajectory_summary, cluster, prev_cluster, prev_stop, params)
          trajectory_summary <- results[[1]]
          cluster <- results[[2]]
          prev_cluster <- results[[3]]
          prev_stop <- results[[4]]
        }
        cluster <- initializeCluster(point)  
      }
    }
  }
  # Finished loop
  if(cluster$duration >= params$stop_time_threshold){
    # Final cluster is a stop 
    results <- addStopPoint(trajectory_summary, cluster, prev_stop, params)
    trajectory_summary <- results[[1]]
    cluster <- results[[2]]
    prev_cluster <- results[[3]]
    prev_stop <- results[[4]]
  } else {
    # Final cluster below threshold 
    results <- clusterCheck(trajectory_summary, cluster, prev_cluster, prev_stop, params)
    trajectory_summary <- results[[1]]
    cluster <- results[[2]]
    prev_cluster <- results[[3]]
    prev_stop <- results[[4]]
  }
  if(!is.null(prev_stop)){
    trajectory_summary <- logStop(trajectory_summary, prev_stop)
  }  
  
  
  return(trajectory_summary)  
}

cluster_trajectory_stops <- function(trajectory_summary){
  require("dplyr")
  require("geosphere")
  
  stops_data <- subset(trajectory_summary$overview, type=="stop")
  
  if(dim(stops_data)[1]>1){
    stops_distm <- as.dist(distm(cbind(stops_data$stop_lng,stops_data$stop_lat),fun=distHaversine))

    stops_hclust <- hclust(stops_distm)
    
    stops_data$cluster_level100  <- cutree(stops_hclust, h=100)
    stops_data$cluster_level200  <- cutree(stops_hclust, h=200)
    stops_data$cluster_level400  <- cutree(stops_hclust, h=400)
    stops_data$cluster_level800  <- cutree(stops_hclust, h=800)
    stops_data$cluster_level1600 <- cutree(stops_hclust, h=1600)
  }
  else{
    stops_data$cluster_level100  <- 1
    stops_data$cluster_level200  <- 1
    stops_data$cluster_level400  <- 1
    stops_data$cluster_level800  <- 1
    stops_data$cluster_level1600 <- 1
  }
  trajectory_summary$overview <- merge(trajectory_summary$overview,
                                       stops_data[,c("period_id",
                                                     "cluster_level100",
                                                     "cluster_level200",
                                                     "cluster_level400",
                                                     "cluster_level800",
                                                     "cluster_level1600")],
                                       all.x = TRUE)
  
  tmp <- data.frame(trajectory_summary$points_to_stops)
  colnames(tmp) <- "period_id"
  tmp <- left_join(tmp,
                   stops_data[,c("period_id",
                                 "cluster_level100",
                                 "cluster_level200",
                                 "cluster_level400",
                                 "cluster_level800",
                                 "cluster_level1600")],
                   by = "period_id")
  trajectory_summary$cluster_levels <- tmp
  
  return(trajectory_summary)
}

named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " | ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

generate_points_convex_hull <- function(point_coords){
  require(dplyr)
  coords_only = point_coords %>% dplyr::select(lngs,lats)
  convex_hull_points = chull(coords_only)
  convex_hull_coords = coords_only[convex_hull_points,]
}


cluster_trajectory_stops2 <- function(lngs,
                                      lats,
                                      is_stop,
                                      stop_ids,
                                      method = "hierarchical",
                                      thresholds){
  require("dplyr")
  require("geosphere")
  
  points_df = as.data.frame(cbind(lngs, lats, is_stop, stop_ids))
  
  stops_locations <- points_df %>%
    dplyr::filter(is_stop == TRUE) %>%
    group_by(stop_ids) %>%
    summarize(n_points = n(),
              center_lng = median(lngs),
              center_lat = median(lats))
  
  stops_points <- points_df %>%
    dplyr::filter(is_stop == TRUE) %>%
    named_group_split(stop_ids) 
  
  stops_boundaries <- lapply(stops_points, generate_points_convex_hull)
  
  
  clusters = points_df %>% dplyr::select(stop_ids)


  if(method == "hierarchical"){
    
    stops_distm <- as.dist(distm(cbind(stops_locations$center_lng,stops_locations$center_lat),fun=distHaversine))
    stops_hclust <- hclust(stops_distm)
    
    for(threshold in thresholds){
      cluster_map <- stops_locations %>% dplyr::select(stop_ids)
      if(dim(stops_locations)[1]>1){
        cluster_map$clusters <- cutree(stops_hclust, h=threshold)
        points_to_clusters <- points_df %>%
          dplyr::select(stop_ids) %>%
          left_join(cluster_map, by=c("stop_ids")) %>%
          dplyr::select(clusters)
        col_name= paste("cluster", method, threshold, sep = ".")
        clusters[col_name] <- points_to_clusters
      }
      else {
        clusters[col_name] <- 1
      }
    }
    
  }
  # Generate cluster
  cluster_info = list()
  for(col in colnames(clusters)){
    tmp <- points_df %>% dplyr::select(is_stop, lngs, lats)
    
    col_id <- quo(col)
    cluster_col <- clusters %>% dplyr::select(!!col_id)
    tmp <- bind_cols(tmp, cluster_col)
    cluster_locations <- tmp %>%
      dplyr::filter(is_stop == TRUE) %>%
      group_by(.dots = col) %>%
      summarize(n_points = n(),
                center_lng = median(lngs),
                center_lat = median(lats))
    
    cluster_points <- tmp %>%
      dplyr::filter(is_stop == TRUE) %>%
      named_group_split(.dots = col) 
    
    cluster_boundaries <- lapply(cluster_points, generate_points_convex_hull)
    
    cur_cluster_info <- list(cluster_locations = cluster_locations,
                         cluster_boundaries = cluster_boundaries)
    
    cluster_info[[col]] <- cur_cluster_info
  }
  

  
  cluster_summary = list(stops_locations = stops_locations,
                         stops_boundaries = stops_boundaries,
                         clusters = clusters,
                         cluster_info = cluster_info)
  
  return(cluster_summary)
}
