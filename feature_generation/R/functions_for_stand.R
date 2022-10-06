#% Adapted from Liat and Ariel code in base STAND repo

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%% May 7th 2020
#%%%%%%%%%%%%%%% Functions used in the STAND project
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################# Function for re-leveling demographic data using a Redcap data dictionary 
create_data_dict <- function(filename){
  # Create data_dict structure that contains question info (type, text displayed, etc.) and a response map for each questions
  # translating between a numerical response id and a plaintext response answer chosen by the user
  
  # Using a filename of a Redcap data dictionary, creates a data_dict object with:
  #   - a map between question ids and question text
  #   - a map between response ids (numbers) and response text for question types with multiple possible responses, e.g. radio, dropdown, checkbox
  
  require(dplyr)
  require(tidyr)
  require(tibble)
  
  data_dict_raw = read.csv(file = filename, check.names = F, stringsAsFactors = F)
  
  question_map = data_dict_raw %>%
    select("Variable / Field Name",
           "Field Type",
           "Field Label") %>%
    column_to_rownames(., var = "Variable / Field Name")
  
  response_map = data_dict_raw %>%
    select("Variable / Field Name",
           "Field Type", 
           "Field Label",
           "Choices, Calculations, OR Slider Labels") %>%
    dplyr::filter(`Field Type` %in% c("checkbox", "dropdown", "radio")) %>%   #field types with numerical coding of responses
    separate_rows("Choices, Calculations, OR Slider Labels", sep = "\\|") %>%   #separate response options into separate rows, delimited by |
    separate("Choices, Calculations, OR Slider Labels", c("Response_ID","Response_text"), "\\,", extra="merge") %>%   #split numerical response id from matching response text
    mutate_if(is.character, trimws) %>% #trim whitespace
    group_by(`Variable / Field Name`) %>% #group by field/question
    group_split() #split into list of tables, one per question, which maps response_id to response_text cols
  
  # Name the list of response tables to allow indexing based on field name
  response_map.names <- NULL
  for (i in 1:length(response_map)){
    response_map.names[i]<-c(unique(response_map[[i]]$`Variable / Field Name`))
    names(response_map) <- response_map.names
  }
  
  data_dict = list(question_map = question_map, response_map = response_map)   #combine question and response map into data_dict object
  
  return(data_dict)
}

################# Function for replacing multiple number->text maps in a single data field, e.g. in demographic race, mapping e.g "1,2" to "White,Black/African American"
replace_tokens <- function(old_vector, old_values, new_values, old_sep = ",", new_sep = NULL){
  
  require(stringr)
  
  ####Function to map vector of IDs to their corresponding text, using the named vector level_key
  replace_tokens_single_vector <- function(vector, level_key){
    require(dplyr)
    return(recode(vector, !!!level_key))
  }
  
  if(is.null(new_sep)){  #Default to using old_sep in output if new_sep is not specified
    new_sep <- old_sep
  }
  
  level_key <- setNames(new_values, old_values)  # Create a named vector mapping IDs to corresponding text
  
  list_of_old_tokens = str_split(old_vector,old_sep)  # Split multi-id fields into separate ids based on a delimiter old_sep
  list_of_new_tokens = sapply(list_of_old_tokens, replace_tokens_single_vector, level_key=level_key) # Convert each ID to the corresponding text using the level_key map
  new_vector = unlist(lapply(list_of_new_tokens, FUN=paste, collapse=new_sep)) # Merge each individual translated text back into the original combined field 
  
  return(new_vector)
}


##### Function for location profile generation
earth.dist = function (long1, lat1, long2, lat2){
  rad = pi/180
  a1 = lat1 * rad
  a2 = long1 * rad
  b1 = lat2 * rad
  b2 = long2 * rad
  dlon = b2 - a2
  dlat = b1 - a1
  a = (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  R = 6378.145 #6.371 * 10^6
  d = R * c
  return(d)
}

find_cluster= function(stationary_loc){
  
  kmeans_distance_max = 1
  N=1 #initial number of clusters
  result_cl= NA
  while(kmeans_distance_max > 0.5){
    
    x = data.frame(stationary_loc$double_latitude, stationary_loc$double_longitude)
    # x = data.frame(location_ind_1$double_latitude, location_ind_1$double_longitude)
    x = na.omit(x)
    colnames(x) = c("x", "y")
    cl = kmeans(x, N)
    # View(x)
    
    # hist(location_ind_1$double_latitude, breaks = 100)
    # hist(location_ind_1$double_longitude, breaks = 100)
    
    #    plot(x, col = cl$cluster)
    #    points(cl$centers, col = c(1:N), pch = 8, cex = 2)
    
    
    cluster_data = data.frame(x, cl$cluster)
    
    max_distance = c()
    mean_distance=c()
    for(k in 1:N){
      
      #print(k)
      
      cluster_data_tmp = cluster_data[cluster_data$cl.cluster == k,]
      
      #Claculate the average distance between clusters
      
      mean_dist_centers = c()
      for(j in 1:dim(cl$centers)[1]){
        
        tmp = c()
        
        for(i in 1:dim(cl$centers)[1]){
          
          tmp[i] = earth.dist(long1 = cl$centers[j,1], lat1 = cl$centers[j,2],
                              long2 = cl$centers[i,1], lat2 = cl$centers[i,2])
        }
        
        mean_dist_centers[j] = mean(tmp)
        
      }
      
      
      mean_dist = c()
      for(j in 1:dim(cl$centers)[1]){
        
        tmp_dist = c()
        
        for(i in 1:dim(cluster_data_tmp)[1]){
          
          tmp_dist[i] = earth.dist(long1 = cl$centers[j,1], lat1 = cl$centers[j,2],
                                   long2 = cluster_data_tmp$x[i], lat2 = cluster_data_tmp$y[i])
        }
        
        mean_dist[j] = mean(tmp_dist)
        
      }
      
      l = which(mean_dist == min(mean_dist))
      
      
      
      distance = c()
      
      for(i in 1:(dim(cluster_data_tmp)[1])){
        
        min_ind = which(cluster_data_tmp$x == min(cluster_data_tmp$x))
        
        # print(cl$centers[k,])
        
        
        distance[i] = earth.dist(long1 = cl$centers[l,1], lat1 = cl$centers[l,2],
                                 long2 = cluster_data_tmp$x[i], lat2 = cluster_data_tmp$y[i])
        
        # print(distance[i])
      }
      
      max_distance[k] = max(na.omit(distance))
      mean_distance[k] = mean(na.omit(distance))
      
      
    }
    kmeans_distance_max = max(max_distance)
    
    #    print("max distance")
    #   print(kmeans_distance_max)
    result_cl=cl
    N=N+1
    if(N>10)
      break
    
  }
  return(result_cl)
  
}

abstract_feature = function(stationary_loc,top_clusters = NULL){
  
  cluster.sum = data.frame(stationary_loc %>% group_by(cluster) %>% tally()) %>% arrange(desc(n))
  cluster1=cluster.sum[1,"cluster"]
  cluster2=cluster.sum[2,"cluster"]
  cluster3=cluster.sum[3,"cluster"]
  cluster4 = cluster.sum[nrow(cluster.sum),"cluster"]
  
  location_profile = as.data.frame(matrix(1 , ncol = 4))
  names(location_profile)=c("top1","top3","last1","ent")
  top1_time = sum(stationary_loc$time_diff[which(stationary_loc$cluster==cluster1)],na.rm=TRUE)
  top2_time= sum(stationary_loc$time_diff[which(stationary_loc$cluster==cluster2)],na.rm=TRUE)
  top3_time=sum(stationary_loc$time_diff[which(stationary_loc$cluster==cluster3)],na.rm=TRUE)
  last1_time=sum(stationary_loc$time_diff[which(stationary_loc$cluster==cluster4)],na.rm=TRUE)
  profile=c()
  probability=c()
  
  for(i in 1:length(cluster.sum[,1])){
    probability[i] = cluster.sum[i,2] / length(stationary_loc[,1])
    profile[i]=probability[i]*log(probability[i])
  }
  
  entropy=-sum(profile)
  location_profile$top1 = top1_time
  location_profile$top3 = top1_time+top2_time+top3_time
  location_profile$last1 = last1_time
  location_profile$ent = entropy
  return(location_profile)
}

##### Function for screen profile generation
#screen status, one of the following: 0=off, 1=on, 2=locked, 3=unlocked

Sleep_recognition <- function(temp_data){
  
  time_diff_new = c()
  for(i in 1:(nrow(temp_data) - 1)){
    
    if(i == 1){
      time_diff_new[1] = temp_data$time_diff[1]
      time_diff_new[2] = temp_data$time_diff[2]
    } else {    
      if(temp_data$screen_status[i+1] != temp_data$screen_status[i]){
        time_diff_new[i+1] = as.numeric(difftime(as.POSIXct.default(temp_data$time_points[i+1]), 
                                                 as.POSIXct.default(temp_data$time_points[i]), 
                                                 units = c("mins")))
        
      }else{
        
        time_diff_new[i+1] = 0
      }
      
    }
    
    # if(i == nrow(temp_data))
    #   time_diff_new[i] = temp_data$time_diff[nrow(temp_data)] 
    
    
  }
  temp_data$time_diff_new = time_diff_new
  
  save_ind = c()
  
  TH1 = 2*60
  TH2 = 2*60
  k = 1
  for(i in 2:(nrow(temp_data) - 1)){
    
    if(temp_data$time_diff_new[i] < 1 & temp_data$time_diff_new[i+1] > TH1 & temp_data$time_diff_new[i-1] > TH2){
      
      save_ind[k] = i
      save_ind[k+1] = i-1
      k = k+1
    }
    
  }
  
  if(length(save_ind) > 1){
    temp_data = temp_data[-save_ind,]
  }
  
  
  
  
  time_diff_new_2 = c()
  for(i in 1:(nrow(temp_data) - 1)){
    
    if(i == 1){
      
      time_diff_new_2[1] = temp_data$time_diff_new[1]
      time_diff_new_2[2] = temp_data$time_diff_new[2]
    }
    
    
    if(i > 1 & i < nrow(temp_data)){    
      
      if(temp_data$screen_status[i+1] != temp_data$screen_status[i]){
        
        z <- difftime(as.POSIXct.default(temp_data$time_points[i+1]), as.POSIXct.default(temp_data$time_points[i]), 
                      units = c("auto", "secs", "mins", "hours",
                                "days", "weeks"))
        
        time_diff_new_2[i+1] = as.numeric(z, units = "mins")
        
      }else{
        
        time_diff_new_2[i+1] = 0
      }
      
    }
    
    # if(i == nrow(temp_data))
    #   time_diff_new[i] = temp_data$time_diff[nrow(temp_data)] 
    
    
  }
  temp_data$time_diff_new_2 = time_diff_new_2
  
  return(temp_data)
  
}

screen_social_pattern <- function(screens, outliers_flag = 0){
  
  # Compute time between screen status changes and only keep screen data for timpoints with screen status change
  screens %<>%
    mutate(time_points_iplus1 = c(time_points[-1],NA), 
           screen_status_iplus1 = c(screen_status[-1],NA)) %>% 
    mutate(time_diff = ifelse(test = screen_status ==screen_status_iplus1,yes = 0, 
                              no = as.numeric(difftime(time1 = as.POSIXct.default(time_points_iplus1), 
                                                       time2 = as.POSIXct.default(time_points), 
                                                       units = "mins")))) %>% 
    
    select(-screen_status_iplus1,-time_points_iplus1) %>% 
    dplyr::filter(time_diff > 0)
  
  screens$seq = 1:nrow(screens)
  screens$Date = as.Date(screens$time_points)
  
  
  # Explanation
  time_diff = c()
  curr_step = c()
  next_step = c()
  time_point = c()
  weekday_per_timepoint = c()
  
  
  Dates_uniq = na.omit(unique(as.Date(screens$time_points)))
  
  daily_counter = c()
  daily_screen_time = c()
  
  Test_list = list()
  Sleep_time = c()
  Wake_up_time = c()
  weekday_data = c()
  Date = c()
  
  for (j in 1:length(Dates_uniq)) {
    
    # print(j)
    
    #Record the time going to sleep
    #Record the time waking up
    
    ######### WARNING: THERE IS NO CHECK HERE THAT THE DAYS ARE CONSECUTIVE
    temp_data = screens[screens$Date %in% c(Dates_uniq[j], Dates_uniq[j+1]),]
    
    
    if(nrow(temp_data) > 2){
      
      #From locked to unlock
      Test = Sleep_recognition(temp_data = temp_data)
      Short_list = head(Test[order(-Test$time_diff_new_2),])
      time_tmp = strftime(Short_list$time_points, format="%H:%M:%S")
      # ind_sleep = which(Short_list$Date %in% Dates_uniq[j+1] & time_tmp < "14:00:00")
      ind_sleep = which(Short_list$Date %in% Dates_uniq[j+1])
      
      if(length(ind_sleep) > 0){
        
        
        ind_sleep_2 = which(Short_list$time_diff_new_2[ind_sleep] == max(Short_list$time_diff_new_2[ind_sleep]))
        
        if(length(ind_sleep_2) > 0){
          
          Test_list[[j]] = Short_list[ind_sleep[ind_sleep_2],]
          Sleep_time[j] = Test_list[[j]]$time_diff_new_2
          Wake_up_time[j] = strftime(Test_list[[j]]$time_points, format="%H:%M:%S")
          weekday_data[j] = Test_list[[j]]$weekday
          Date[j] = as.character(Test_list[[j]]$Date)
        }
        
        if(length(ind_sleep_2) == 0){
          
          Test_list[[j]] = NA
          Sleep_time[j] = NA
          Wake_up_time[j] = NA
          weekday_data[j] = NA
          Date[j] = NA
        }
        
      }
      
      if(length(ind_sleep) == 0){
        
        
        Test_list[[j]] = NA
        Sleep_time[j] = NA
        Wake_up_time[j] = NA
        weekday_data[j] = temp_data$weekday[nrow(temp_data)]
        Date[j] = as.character( Dates_uniq[j+1])
        
      }
      
      
      daily_counter_tmp = 1
      daily_screen_time_tmp = 0
      
      
      temp_vec_per_date = which(screens$Date %in% c(Dates_uniq[j]))
      temp_data_per_date = screens[temp_vec_per_date,]
      
      if(dim(temp_data_per_date)[1] > 2){
        
        for(i in 2:(dim(temp_data_per_date)[1]-1)){
          
          
          #unlocked to locked = screen time
          if(temp_data_per_date$screen_status[i] == 3 & temp_data_per_date$screen_status[i+1] == 2){
            
            daily_counter_tmp = daily_counter_tmp + 1
            daily_screen_time_tmp = daily_screen_time_tmp + temp_data_per_date$time_diff[i+1]
            
          } 
        }
        
        daily_counter[j] = daily_counter_tmp
        daily_screen_time[j] = daily_screen_time_tmp
      }
      
      
      if(dim(temp_data_per_date)[1] < 2){
        
        daily_counter[j] = NA
        daily_screen_time[j] = NA
        
      }
      
      
    }
    
    
  }
  
  #Remove outliers if outliers_flag = 1
  Sleep_time[is.na(Sleep_time)] = NA
  
  if(outliers_flag == 1){
    
    
    Sleep_time_results =  data.frame(Sleep_time[which(Sleep_time < 1000 & Sleep_time > 180)], 
                                     Wake_up_time[which(Sleep_time < 1000 & Sleep_time > 180)],
                                     weekday_data[which(Sleep_time < 1000 & Sleep_time > 180)],
                                     Date[which(Sleep_time < 1000 & Sleep_time > 180)])
    names(Sleep_time_results) = c("Sleep_time", "Wake_up_time", "weekday", "Date")
    
    ind_na = which(is.na(Sleep_time_results$Sleep_time))
    
    if(length(ind_na) > 0){
      
      return(Sleep_time_results[-ind_na,])
    }else{
      
      return(Sleep_time_results)
    }
    
  }
  
  if(outliers_flag == 0){
    
    
    Sleep_time_results =  data.frame(Sleep_time, 
                                     Wake_up_time,
                                     weekday_data,
                                     daily_counter,
                                     daily_screen_time,
                                     Date)
    names(Sleep_time_results) = c("Sleep_time", "Wake_up_time", "weekday", "daily_counter",
                                  "daily_screen_time", "Date")
    
    Sleep_time_results$Date = as.Date(Sleep_time_results$Date)
    remove_na_date <- which(is.na(Sleep_time_results$Date))
    
    if(length(remove_na_date) > 0)
      Sleep_time_results = Sleep_time_results[-remove_na_date,]
    
    
    
    return(Sleep_time_results)
    
  }
  
  
  
}
