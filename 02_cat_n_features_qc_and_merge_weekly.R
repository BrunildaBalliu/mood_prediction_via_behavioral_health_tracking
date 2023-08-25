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
# args=commandArgs(TRUE)
# project_dir=args[1]
# freeze_date=args[2]
# pred=args[3]
# source(file = paste0(project_dir,"/scripts/00_functions_for_stand.R"))

project_dir=getwd()
freeze_date = "20210511"
pred= "present" # # "catdiff" #"nextcat"
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

impute_features = TRUE
Autocomplete = FALSE

if(impute_features) {
  if(Autocomplete) {
    imputed_features="_and_Autocomplete"
    } else {
      imputed_features="_and_imputed"
      }} else {imputed_features=NULL}

save_file = TRUE # save the final qc-ed data file?


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% CAT-MH Data processing 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# read CAT-MH data
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

n1=length(unique(cat$subject_id))

# Keep only individuals with at least X cat interviews
if(filter_n_interviews_individuals){
  nr_interviews=cat %>% group_by(subject_id) %>% summarise(n=n()) %>% filter(n>=days_with_cat_interviews)
  cat = cat %>% filter(subject_id %in% nr_interviews$subject_id) %>% droplevels()
}
n2=length(unique(cat$subject_id))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Sensor data processing 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Read phone sensor features 
features = readRDS(file = paste0(data_dir,'features_data_freeze_',freeze_date,'.rds')) %>% 
  mutate(subject_id = as.numeric(subject_id),
         is_weekend=as.numeric(is_weekend)) %>%
  arrange(subject_id,date)
sprintf(fmt = "finished reading feature data")

# Keep individuals with at least 30 days of phone data
if(filter_days_sensor_data){
  nr_days_sensor_data=features %>% group_by(subject_id) %>% summarise(n=sum(!is.na(n_locations_obs))) %>% filter(n>=days_with_sensor_data)
  features = features %>% filter(subject_id %in% nr_days_sensor_data$subject_id) %>% droplevels()
}

# Keep individuals with 70% of days observed (median across all features) 
pass_features = melt(data = data.table(id=features$subject_id, date = features$date, !is.na(features[,c(6:1328)])) %>% arrange(id,date),
                    id.vars = c("id", "date")) %>%  
  group_by(id, variable) %>% summarise(pobs = sum(value)/n()) %>% 
  group_by(id) %>% summarise(pobs = median(pobs)) %>% filter(pobs > .70) %>% 
  pull(id)

features = features %>% filter(subject_id %in% pass_features) %>% droplevels()

n3=length(unique(features$subject_id))

# Filter days for which most features are missing
tmp=apply(!is.na(features[,c(6:60)]),1,sum)/ncol(features[,c(6:60)])
features = features [tmp >= .5,]
n3.1=length(unique(features$subject_id))

############################################################
# Merge cat with features
############################################################

# Keep individuals with both cat and phone sensor features 
subject_id_complete=intersect(features$subject_id, cat$subject_id)
features %<>% filter(subject_id %in% subject_id_complete) %>% droplevels() %>% arrange(subject_id, date)
feature_dates = features %>% group_by(subject_id) %>% summarise(min_d=min(date), max_d=max(date))
cat %<>% filter(subject_id %in% subject_id_complete) %>% droplevels() %>% arrange(subject_id,date)

n4=length(unique(cat$subject_id))

cat_n_features = merge(x = cat %>% select(subject_id, treatment_wave, study_subject_id, date, MDD.diagnosis, Anxiety.severity, Depression.severity, Anxiety.category, Depression.category), 
                       y = features, by = c("subject_id", "date"), all.x = F, all.y = F) %>%
  arrange(treatment_wave, subject_id, date)

n5 = length(unique(cat_n_features$subject_id))

# Keep only study subjects with at least X cat interviews
if(filter_n_interviews_individuals){
  nr_interviews = cat_n_features %>% group_by(subject_id) %>% summarise(n=n()) %>% filter(n>=days_with_cat_interviews)
  cat_n_features = cat_n_features %>% filter(subject_id %in% nr_interviews$subject_id) %>% droplevels()
}
n6 = length(unique(cat_n_features$subject_id))

sprintf(fmt = "finished merging cat and feature data")


missing = data.table(id=cat_n_features$subject_id, date = cat_n_features$date, !is.na(cat_n_features[,c(13:1335)])) %>% 
  melt(id.vars = c("id", "date")) %>% 
  mutate(id_date = paste0(id,date)) %>%
  arrange(id,date) %>% filter(grepl(pattern = ".rmean7|.var7|.rmean14|.var14", x = variable))

ggsave(filename = "~/Desktop/tmp.pdf", width = 30, height = 30, 
       plot = ggplot(data = missing %>% 
         rename(measured = value) %>% 
           filter(grepl(pattern = ".rmean7|.var7|.rmean14|.var14", x = variable)), #  filter(id=="869") %>% head(n=500000)
       mapping = aes(y =  id_date, x = variable, fill=measured)) + 
  geom_tile() + 
  theme(#axis.text.y = element_blank(), 
    axis.text = element_blank(), #element_text(angle = 90,hjust = 1)
    strip.text = element_text(angle = 90)) + facet_wrap(~id, strip.position = "left", scales = "free_y",ncol = 1))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Save the data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(save_file) write.csv(x = missing_all, file = paste0(project_dir,'/results/missing_data_percentage.csv'), quote = F, row.names = T)

if(pred=="catdiff") cat_n_features$Depression.category = "Not applicable" else cat_n_features$Depression.category = cut(x = cat_n_features$Depression.severity, breaks = cat_di_breaks, labels = cat_levels, include.lowest = T)

if(save_file) write.csv(x = cat_n_features, file = paste0(data_dir,'cat_n_features_weekly_qced',imputed_features,'_data_freeze_',freeze_date,"_",pred,'.csv'), quote = F, row.names = F)

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


