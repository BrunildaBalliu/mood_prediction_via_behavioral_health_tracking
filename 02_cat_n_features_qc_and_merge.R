#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Predicting Subjective Measures Of Mood From Mobile Sensor Data
#%%%%%%%%%%%%%%% Scripts to perform quality control and merge CAT and AWARE features
#%%%%%%%%%%%%%%% March 10th 2021
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%% Libraries 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
args=commandArgs(TRUE)
project_dir=args[1]
freeze_date=args[2]
pred=args[3]
imputation_method = args[4]


data_dir=paste0(project_dir,"/data/")
source(paste0(project_dir,"/code/00_functions_for_stand.R"))

cat_di_breaks=c(0,35,65,75,100)
cat_anx_breaks=c(0,35,50,65,100)
cat_levels=c("normal", "mild", "moderate", "severe")

filter_n_interviews_individuals = T # Filter individuals based on number of CAT-MH interviews
days_with_cat_interviews = 5 # Minimum number of CAT-MH interviews required

filter_days_sensor_data = T # Filter individuals based on number of days with phone sensor data?
days_with_sensor_data = 30  # Minimum number of days with phone sensor data required

average_duplicated_entries= T  # merge duplicated entries using average

filter_nr_long_breaks = T  # Filter individuals with long breaks?
break_days=30              # Max number of days (break) allowed between interviews

make_cat_trajecoty_plots = F # Plot and save CAT trajectories of each individual? 
make_feature_cor_plots = F #Plot and save correlation between CAT and features of each individual? 

if(imputation_method == "no_imputation") imputed_features=NULL
if(imputation_method == "softimpute") imputed_features="_and_softimputed"
if(imputation_method == "spline")   imputed_features="_and_imputed"

save_file = TRUE # save the final qc-ed data file?

prc_train=.70

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Data processing 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%% read cat 
cat = read.csv(file = paste0(data_dir,'cat_complete_data_freeze_',freeze_date,'.csv'), stringsAsFactors = F) %>% 
  select(-cat_subject_id, -interview_id) %>% 
  filter(!study_name %in% c("is1","ss1","ss3")) %>% 
  droplevels() %>% 
  mutate(date=as.Date(x = as.character(cat_start_time)),
         cat_duration = cat_duration/60,
         treatment_wave = factor(x = study_name, 
                                 levels = c("is2", "ss2", "itn", "itnc"),
                                 labels = c("Wave 1 - Online support", 
                                            "Wave 2 - Online support", 
                                            "Wave2 - Clinical care", 
                                            "Wave2 - Clinical care"))) %>%
  select(-study_name) %>%
  arrange(subject_id,cat_start_time) %>% 
  mutate(wave = factor(x = ifelse(test = grepl(pattern = "Wave 1",x = treatment_wave),yes = "Wave 1", no = "Wave 2")),
         treatment_group = factor(x = case_when(
           treatment_wave=="Wave 1 - Online support"~"online support", 
           treatment_wave=="Wave 2 - Online support"~"online support", 
           treatment_wave=="Wave2 - Clinical care"~"clinical care",
           TRUE ~ NA_character_
         ), levels = c("online support","clinical care")))


sprintf(fmt = "finished reading cat_mh data")

n1=length(unique(cat$subject_id))


# Keep only individuals with at least X cat interviews
if(filter_n_interviews_individuals){
  nr_interviews=cat %>% group_by(subject_id) %>% summarise(n=n()) %>% filter(n>=days_with_cat_interviews)
  cat = cat %>% filter(subject_id %in% nr_interviews$subject_id) %>% droplevels()
}
n2=length(unique(cat$subject_id))


# Read phone sensor features 
features = readRDS(file = paste0(data_dir,'features_data_freeze_',freeze_date,'.rds')) %>% 
  mutate(subject_id = as.numeric(subject_id),
         is_weekend=as.numeric(is_weekend)) %>%
  arrange(subject_id,date)
sprintf(fmt = "finished reading feature data")

# Keep individuals with at least 30 days of phone data
if(filter_days_sensor_data){
  nr_days_sensor_data=features %>% group_by(subject_id) %>% summarise(n=n()) %>% filter(n>=days_with_sensor_data)
  features = features %>% filter(subject_id %in% nr_days_sensor_data$subject_id) %>% droplevels()
}

# Keep individuals with both cat and phone sensor features 
subject_id_complete=intersect(features$subject_id, cat$subject_id)
features %<>% filter(subject_id %in% subject_id_complete) %>% droplevels() %>% arrange(subject_id, date)
feature_dates = features %>% group_by(subject_id) %>% summarise(min_d=min(date), max_d=max(date))
cat %<>% filter(subject_id %in% subject_id_complete) %>% droplevels() %>% arrange(subject_id,date)
n3=length(unique(cat$subject_id))

# Merge duplicated entries using average
if(average_duplicated_entries){
  duplicated=cat %>% mutate(ind_date=paste(subject_id,date,sep = "_")) %>% group_by(ind_date) %>% summarise(n=n()) %>% filter(n>1)
  cat_duplicated=cat %>% mutate(ind_date=paste(subject_id,date,sep = "_")) %>% filter(ind_date %in% duplicated$ind_date) 
  
  cat_duplicated %<>% 
    group_by(subject_id, treatment_wave, study_subject_id,date ) %>% 
    summarise(cat_start_time=min(cat_start_time), 
              cat_duration = mean(cat_duration), 
              MDD.diagnosis = paste0(sort(unique(MDD.diagnosis)), collapse = "-"), 
              Suicide.diagnosis = paste0(sort(unique(Suicide.diagnosis)), collapse = "-"), 
              Anxiety.severity=mean(Anxiety.severity, na.rm=T), 
              Depression.severity=mean(Depression.severity, na.rm=T), 
              Mania.severity=mean(Mania.severity, na.rm=T), 
              Suicide.Scale.severity=mean(Suicide.Scale.severity, na.rm=T), 
              Anxiety.category = NA, 
              Depression.category= NA, 
              Mania.category = NA, 
              Suicide.Scale.category=NA)
  
  cat_no_duplicates=cat %<>% mutate(ind_date=paste(subject_id,date,sep = "_")) %>% 
    filter(!ind_date %in% duplicated$ind_date) %>% 
    select(-ind_date)
  
  cat = bind_rows(cat_no_duplicates[,colnames(cat_duplicated)], cat_duplicated) %>% arrange(subject_id,cat_start_time)
  
}


############################################################
# Interpolate cat for each subject and merge with features
############################################################
# This one takes a long time to run

cat_n_features = NULL
subjects=unique(cat$subject_id)
missing_all = NULL

for(iid in subjects){
  # iid = 9873
  print(iid)
  # Get CAT and feature data for individual iid
  cat_iid = cat %>% filter(subject_id==iid) %>% droplevels() %>% arrange(date)
  if(pred=="nextcat") cat_iid %<>% mutate(Depression.severity=c(Depression.severity[-1],NA)) 
  if(pred=="catdiff") cat_iid %<>% mutate(Depression.severity=c(diff(Depression.severity),NA)) 
  
  # Interpolate CAT data in between interviews
  cat_interpolated_iid = NULL
  for(i in 1:nrow(cat_iid)){
    if(i < nrow(cat_iid)) date_seq = seq(as.Date(cat_iid$date[i]), as.Date(cat_iid$date[i+1])-1, by = "day") else date_seq = cat_iid$date[i] 
    if(filter_nr_long_breaks & length(date_seq)>=break_days) date_seq = date_seq[1:break_days]
    real_cat = c(1, rep(0, (length(date_seq) -1) )) 
    
    cat_interpolated_iid_tmp = cat_iid[rep(i,length(date_seq)),] %>% mutate(date=date_seq, real_cat=real_cat)
    
    cat_interpolated_iid = rbind(cat_interpolated_iid, cat_interpolated_iid_tmp)
    rm(date_seq,real_cat,cat_interpolated_iid_tmp)
  }
  
  features_iid=features %>% filter(subject_id == iid) %>% droplevels() %>% arrange(date) %>% mutate(study_day=1:n()) 
  
  if(nrow(features_iid)==0) next;
  
  # Interpolate features in between interviews
  if(imputation_method != "no_imputation"){
    
    
    # Merge CAT-DI and features
    cat_n_features_iid = merge(x = cat_interpolated_iid, y = features_iid, by = c("subject_id", "date"), all.x = F, all.y = F)
    
    # Get training and test set
    N_obs=sum(cat_n_features_iid$real_cat)
    if(N_obs< 5) next 
    train_end = max(1, round(N_obs*prc_train))
    train_end_date=(cat_n_features_iid %>% filter(real_cat==1))[train_end+1,'date']-1
    train_end=sum(cat_n_features_iid$date<=train_end_date)
    
    features_iid=cat_n_features_iid %>% select(colnames(features_iid))
    
    missing_i=c(subject_id=features_iid$subject_id[1],(apply(is.na(features_iid),2,sum)/nrow(features_iid))*100)
    missing_all=rbind(missing_all,missing_i)
    
    if(imputation_method == "softimpute"){
      fits=softImpute(x = features_iid[,-c(1:4)] %>% mutate(n=1:n()) %>% filter(n %in% 1:train_end) ,
                      trace=TRUE, 
                      type="svd",
                      maxit = 200)
      
      features_iid_imputed=cbind(features_iid[,c(1:4)],
                                 rbind(softImpute::complete(x = features_iid[,-c(1:4)] %>% mutate(n=1:n()) %>% filter(n %in% 1:train_end), object = fits),
                                       softImpute::complete(x = features_iid[,-c(1:4)] %>% mutate(n=1:n()) %>% filter(!n %in% 1:train_end), object = fits))
      )
      
    }  
    
    if(imputation_method == "spline"){
      
      features_with_NAs=setdiff(names(which(apply(is.na(features_iid),2,sum)>=1)),c("subject_id", "os", "date", "weekday", "is_weekend"))
      
      predicted_features_iid=data.frame(date=features_iid$date,matrix(data = NA, nrow = nrow(features_iid), ncol = length(features_with_NAs))) 
      colnames(predicted_features_iid) = c("date",features_with_NAs)
      
      for(j in 1:length(features_with_NAs)){
        print(j)
        feature=features_with_NAs[j]
        tmp_features_iid=features_iid[,c("date",feature)] %>% na.omit()
        if(nrow(tmp_features_iid)<=20) { 
          predicted_features_iid[,feature] = NA
        } else {
          predicted_features_iid[,feature]=predict(object = smooth.spline(x = as.numeric(tmp_features_iid[,"date"]),
                                                                          y = as.numeric(tmp_features_iid[,feature]), all.knots=TRUE, cv= TRUE),
                                                   x = as.numeric(features_iid$date))$y
          
          # predicted_features_iid[,feature]=predict(object = smooth.spline(x = as.numeric(tmp_features_iid[,"date"]),
          #                                                                 y = as.numeric(tmp_features_iid[,feature]),
          #                                                                 all.knots=TRUE, cv= TRUE,
          #                                                                 control.spar = list(trace = TRUE)),
          #                                          x = as.numeric(features_iid$date))$y
          
        }      
        }
      
      features_iid_imputed=merge(x = features_iid %>% select(-all_of(features_with_NAs)) , y = predicted_features_iid)
      features_iid=features_iid_imputed
      
    }
    
    features_iid = features_iid_imputed %>% select(-n)
    
  }
  
  features_iid=features_iid %>% filter(date>=min(cat_iid$date) & date<=max(cat_iid$date)) %>% droplevels() %>% arrange(date)
  
  cat_n_features_iid = merge(x = cat_interpolated_iid, y = features_iid, by = c("subject_id", "date"), all.x = F, all.y = F)
  
  # Remove last observation if further than X days from previous one, e.g. subject_id = 971
  if(as.numeric(diff(tail(cat_n_features_iid[,"date"],n=2)))>60) cat_n_features_iid = cat_n_features_iid[-nrow(cat_n_features_iid),]

  cat_n_features = rbind(cat_n_features, cat_n_features_iid)
  
}

n4 = length(unique(cat_n_features$subject_id))

# Keep only study subjects with at least X cat interviews
if(filter_n_interviews_individuals){
  nr_interviews = cat_n_features %>% group_by(study_subject_id) %>% summarise(n=max(length(unique(Anxiety.severity)),length(unique(Depression.severity)),sum(real_cat))) %>% filter(n>=2)
  cat_n_features = cat_n_features %>% filter(study_subject_id %in% nr_interviews$study_subject_id) %>% droplevels()

  nr_interviews = cat_n_features %>% group_by(subject_id) %>% summarise(n=max(length(unique(Anxiety.severity)),length(unique(Depression.severity)),sum(real_cat))) %>% filter(n>=days_with_cat_interviews)
  cat_n_features = cat_n_features %>% filter(subject_id %in% nr_interviews$subject_id) %>% droplevels()
  missing_all = missing_all[missing_all[,"subject_id"] %in% nr_interviews$subject_id,]
  rownames(missing_all)=missing_all[,1]
  missing_all=missing_all[,-1]
}
n5 = length(unique(cat_n_features$subject_id))
sprintf(fmt = "finished merging cat and feature data")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Save the data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(save_file) write.csv(x = missing_all, file = paste0(project_dir,'/results/missing_data_percentage',imputed_features,'.csv'), quote = F, row.names = T)

if(save_file) write.csv(x = cat_n_features, file = paste0(data_dir,'cat_n_features_qced',imputed_features,'_data_freeze_',freeze_date,"_",pred,'.csv'), quote = F, row.names = F)

sprintf(fmt = "finished saving data")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Additional QC
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Fix obs_cat to be one for the first non-missing interpolated cat-mh 
# Remove weird subjects with daily CAT fixed at the same level

if(make_cat_trajecoty_plots){
  individuals_multi_studies=cat_n_features %>% group_by(subject_id) %>% summarise(nr_studies=length(unique(treatment_wave))) %>% filter(nr_studies==1)
  
  subject_id_complete = unique(cat_n_features$subject_id)
  for(iid in subject_id_complete){
    
    p = cat_n_features %>%  
      filter(subject_id==iid) %>% 
      mutate(real_cat = factor(x = real_cat, levels = c(0,1), labels = c("interpolated","real"))) %>%  
      ggplot(mapping = aes(x=date, y = Depression.severity, col=treatment_wave, size=real_cat)) + 
      geom_point() + 
      coord_cartesian(ylim = c(0,100)) + 
      ylab(iid) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            legend.position = c(.1,.2)) +
      scale_size_manual(values = c(1,2)) +
      guides(size=guide_legend(title = "CAT-MH"),col=guide_legend(title = "Study"))
    
    
    ggsave(filename = paste0(project_dir,'results/trajectory_plots/',iid,".png"), plot = p, width = 10, height = 5)
  }
}

if(make_feature_cor_plots){
  subject_id_complete = unique(cat_n_features$study_subject_id) 
  for(iid in subject_id_complete){
    tmp_data = cat_n_features %>%  filter(study_subject_id==iid) 
    
    tmp_data1 = tmp_data[,c("Depression.severity", "Anxiety.severity")]
    tmp_data1=tmp_data1[,!apply(is.na(tmp_data1), 2,all)]
    tmp_data1 = tmp_data1[,apply(tmp_data1, 2,var, na.rm=T)!=0]
    
    tmp_data2 = tmp_data[,c(21:23,26:53,56:100)]
    tmp_data2=tmp_data2[,!apply(is.na(tmp_data2), 2,all)]
    tmp_data2 = tmp_data2[,apply(tmp_data2, 2,var, na.rm=T)!=0]
    
    all_corrs=cor(cbind(tmp_data1,tmp_data2), tmp_data1, use = "pairwise.complete.obs")  #ncol(cat_n_features) 
    p=ggcorrplot(corr = all_corrs, show.legend = F) + theme(axis.text.x = element_text(angle=90)) 
    ggsave(filename = paste0(project_dir,'results/cat_features_correlations/',iid,".png"), plot = p, width = 15, height = 4)
  }
}


