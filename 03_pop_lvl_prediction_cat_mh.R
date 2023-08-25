#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Predicting Subjective Measures Of Mood From Mobile Sensor Datavb  velwfjowi;fjOEW;JFO;IWEJFOIJWEFOIJWEFOIJWWEK
#%%%%%%%%%%%%%%% code to predict CAT-DI and CAT-ANX from smartphone-inferred behavioral features
#%%%%%%%%%%%%%%% Population level prediction
#%%%%%%%%%%%%%%% July 14th 2021
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
args=commandArgs(TRUE)
project_dir=args[1]
freeze_date=args[2]
pred=args[3]
impute_features = args[4]
family = args[5]

# project_dir=getwd() # "/u/project/bballiu/bballiu/STAND"
# freeze_date = "20210511"
# pred="present" #  "nextcat" # "catdiff"
# impute_features = "AutoComplete" # "no_imputation" # "spline" # "softimpute" #
# family='binomial'

data_dir=paste0(project_dir,"/data/")
source(file = paste0(project_dir,"/code/00_functions_for_stand.R"))

cat_di_breaks=c(0,35,65,75,100)
cat_anx_breaks=c(0,35,50,65,100)
cat_suicide_breaks=c(0,35,70,100)

cat_levels=c('normal', 'mild', 'moderate', 'severe')
cat_levels_suicide=c('low risk', 'intermediate risk', 'high risk')

prc_train=.70
split_by_interview_date = TRUE # better to keep this TRUE otherwise some LOCF-interpolated points end up in the test set
smooth_cat = TRUE # better to keep this TRUE
set_bad_predictions_na = FALSE # better to do this after i get all prediction results

if(impute_features =="no_imputation") imputation_msg=imputed_pred=NULL
if(impute_features == "spline") {imputation_msg='_and_imputed' ; imputed_pred='_imputed_features' }
if(impute_features =="softimpute" ) {imputation_msg='_and_softimputed' ; imputed_pred='_softimputed_features' }
if(impute_features =="AutoComplete") {imputation_msg='_and_AutoComplete' ; imputed_pred='_AutoComplete_features' }

if(family=='binomial') fam_msg='Binom' else fam_msg = NULL


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Load and process data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##### Individual-level elastic net model with features
predictions=read.csv(file = paste0(project_dir,'/results/revision_predictions/offline_pred_', pred, imputed_pred, '_enet', fam_msg ,'_', freeze_date,'.csv'), header = T) 
predictions_wide <- pivot_wider(data = predictions %>% select(-predicted),  id_cols = c(id,date,real_cat,set),  names_from = scale, values_from = observed) %>% arrange(id,date) 
id_dates_2_keep = predictions_wide %>% mutate(id_date=paste(id,date, sep = "_")) %>% pull(id_date)

#####  CAT & features
cat_n_features = data.frame(fread(file = paste0(data_dir,'cat_n_features_qced',imputation_msg,'_data_freeze_',freeze_date,"_",pred,'.csv'), header = T, stringsAsFactors = F), stringsAsFactors = F, check.names = F)
if(any(colnames(cat_n_features) == "split")) cat_n_features %<>% select(-split)
if(any(colnames(cat_n_features) == "study_name")) cat_n_features %<>% select(-study_name)
if(any(colnames(cat_n_features) == "treatment_wave")) cat_n_features %<>% select(-treatment_wave)

cat_n_features %<>% 
  mutate(date=as.Date(x = as.character(date)),
         Depression.category=factor(x = cut(x = Depression.severity, breaks = cat_di_breaks, labels = cat_levels, include.lowest = T), levels = cat_levels),
         Anxiety.category=factor(x = cut(x = Anxiety.severity, breaks = cat_anx_breaks, labels = cat_levels, include.lowest = T), levels = cat_levels),
         Suicide.Scale.category=factor(x = cut(x = Suicide.Scale.severity, breaks = cat_suicide_breaks, labels = cat_levels_suicide, include.lowest = T), levels = cat_levels_suicide)) %>% 
  mutate(id_date=paste(subject_id,date, sep = "_")) %>% 
  filter(id_date %in% id_dates_2_keep) %>% 
  droplevels() %>% 
  arrange(subject_id,date) 

##### Annotated (train/test) CAT and features
print("Annotating training and test set")
cat_n_features = merge(x = cat_n_features, 
                       y = predictions_wide %>% 
                         select(id,date,set), 
                       by.x = c("subject_id","date"), 
                       by.y = c("id","date"), all = F)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Prediction
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
offline_pred_all_enet=NULL

daily_data = cat_n_features %>% 
  select(-study_subject_id, -cat_duration, -MDD.diagnosis, -Suicide.diagnosis, -Anxiety.severity, -Mania.severity, -Suicide.Scale.severity, -Anxiety.category, -Mania.category, -Suicide.Scale.category, -weekday,  -cat_start_time, -os ) %>%
  rename(Date=date, severity = Depression.severity, category=Depression.category) 

# Remove columns with many missing values and rows with any missing values
daily_data=daily_data[,apply(is.na(daily_data),2,sum)/nrow(daily_data)<=.5] # remove features missing in more than 50% of days
daily_data = na.omit(daily_data)

# Keep only individuals with non-missing data in training and test set 
# Only relevant when we features without imputation are used
ids2keep=daily_data %>% group_by(subject_id,set) %>% filter(!duplicated(subject_id)) %>% 
  group_by(subject_id) %>% summarise(n=n()) %>% ungroup() %>% filter(n==2) %>% 
  pull(subject_id)
daily_data %<>% filter(subject_id %in% ids2keep)

# Separate X and Y
daily_data_y = daily_data %>% select(subject_id, Date, severity, category, real_cat, set) 
daily_data_x = daily_data %>% select(-subject_id, -Date, -severity, -category, -real_cat, -is_weekend, -id_date, -set) 

# Remove features without variation 
daily_data_x = daily_data_x[,apply(daily_data_x,2,var, na.rm=T)!=0] 
daily_data_x %<>% mutate(subject_id = daily_data_y$subject_id, set=daily_data_y$set)

# Add golden truth spline phenotypes 
daily_data_y = merge(x = daily_data_y, 
                     y = predictions_wide %>% select(-real_cat,-set, -LOCF), 
                     by.x = c("subject_id", "Date"), 
                     by.y = c("id", "date")) %>% 
  rename(severity_cs_4k=`CS(4df)`,
         severity_cs_uk=`CS(udf)`,
         severity_cs_best = `CS(best)`) 


# Separate training and test data
train_data_y_all=daily_data_y %>% filter(set=="Train") %>% select("subject_id","Date","real_cat","severity","category") 
test_data_y_all=daily_data_y %>% filter(set=="Test") %>% select("subject_id","Date","real_cat","severity","category")

# Add training set spline phenotypes 
indeces = sort(unique(daily_data$subject_id))

if(smooth_cat){
  
  train_data_y_all=train_data_y_all %>% mutate(severity_cs_4k=NA, severity_cs_uk=NA, severity_cs_best=NA)
  
  for (u in indeces) {
  # u=869
    train_data_y = train_data_y_all %>% filter(subject_id==u)
    
    if(sum(train_data_y$real_cat,na.rm = T)>=4){
      
      unique_cat_train=max(2,length(unique(train_data_y %>% pull(category))))
      train_data_y_all[train_data_y_all$subject_id==u,'severity_cs_4k'] = smooth.spline2(daily_data = train_data_y, imputation_type = "fixed_knots", n_knots = 4)  
      train_data_y_all[train_data_y_all$subject_id==u,'severity_cs_uk'] = smooth.spline2(daily_data = train_data_y, imputation_type = "fixed_knots", n_knots = unique_cat_train)    
      train_data_y_all[train_data_y_all$subject_id==u,'severity_cs_best'] = smooth.spline2(daily_data = train_data_y, imputation_type = "best")  
      
    }
    }
}

# Keep only individuals for which spline does not give negative values or values above 100
ids2keep=train_data_y_all %>% group_by(subject_id) %>% 
  summarise(missing=any(c(
    is.na(severity_cs_4k), 
    is.na(severity_cs_uk),
    is.na(severity_cs_best),
    severity_cs_4k<0, 
    severity_cs_uk<0,
    severity_cs_best<0,
    severity_cs_4k>100, 
    severity_cs_uk>100,
    severity_cs_best>100
  )))  %>% 
  filter(missing==FALSE) %>% 
  pull(subject_id) 

train_data_y_all %<>% filter(subject_id %in% ids2keep)
test_data_y_all %<>% filter(subject_id %in% ids2keep)


train_data_x_all = daily_data_x %>% filter(set=="Train" & subject_id %in% ids2keep) %>% select(-subject_id, -set) %>% as.matrix()
test_data_x_all=daily_data_x %>% filter(set=="Test" & subject_id %in% ids2keep) %>% select(-subject_id, -set) %>% as.matrix()


# Scale X data 
center_x_train = apply(train_data_x_all,2,mean,na.rm=T);
scale_x_train = apply(train_data_x_all,2,sd,na.rm=T);
train_data_x_all_scaled = scale(train_data_x_all, center = center_x_train, scale = scale_x_train);
test_data_x_all_scaled = scale(test_data_x_all,  center = center_x_train, scale = scale_x_train);

########### Prediction Models
design_matrix_train=model.matrix(~as.factor(subject_id), data = train_data_y_all)[,-1]
colnames(design_matrix_train) = gsub(pattern = 'as.factor\\(subject_id\\)',replacement = "",x = colnames(design_matrix_train))

design_matrix_test=model.matrix(~as.factor(subject_id), data = test_data_y_all)[,-1]
colnames(design_matrix_test) = gsub(pattern = 'as.factor\\(subject_id\\)',replacement = "",x = colnames(design_matrix_test))

if(1){
  
  # Predict severity score 
  if(family=='binomial'){
    
    # Prediction using linear regression with population intercept only
    pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity, 100-train_data_y_all$severity), test_x=test_data_x_all_scaled, family = 'binomial')
    print("Finished prediction using linear regression with population intercept only")
    
    # Prediction using elastic net with individual intercepts only
    pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                         train_y=cbind(train_data_y_all$severity, 100-train_data_y_all$severity), 
                                                         test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]), 
                                                         family = 'binomial')
    print("Finished prediction using elastic net with individual intercepts only")
    
    
    
    # Predict severity score using elastic net with features only
    pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity, 100-train_data_y_all$severity),  test_x=test_data_x_all_scaled, family = 'binomial')
    print("Finished prediction using elastic net with features only")
    
    # Predict severity score using elastic net with individual intercepts and features
    pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=cbind(train_data_y_all$severity, 100-train_data_y_all$severity), test_x=cbind(design_matrix_test, test_data_x_all_scaled), family = 'binomial')
    print("Finished prediction using elastic net with individual intercepts and features")
    
  } else{
    
    # Prediction using linear regression with population intercept only
    pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity, test_x=test_data_x_all_scaled)
    print("Finished prediction using linear regression with population intercept only")
    
    # Prediction using elastic net with individual intercepts only
    pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                         train_y=train_data_y_all$severity, 
                                                         test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]))
    print("Finished prediction using elastic net with individual intercepts only")
    
    # Predict severity score using elastic net with features only
    pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity,  test_x=test_data_x_all_scaled)
    print("Finished prediction using elastic net with features only")
    
    # Predict severity score using elastic net with individual intercepts and features
    pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=train_data_y_all$severity, test_x=cbind(design_matrix_test, test_data_x_all_scaled))
    print("Finished prediction using elastic net with individual intercepts and features")
    
  }
  
    predictions = 
    data.frame(id=c(train_data_y_all$subject_id,test_data_y_all$subject_id), 
               date = c(train_data_y_all$Date,test_data_y_all$Date), 
               real_cat = c(train_data_y_all$real_cat,test_data_y_all$real_cat), 
               set = c(rep("Train",nrow(train_data_y_all)), rep("Test", nrow(test_data_y_all))), 
               scale="LOCF", 
               observed = c(train_data_y_all$severity,test_data_y_all$severity), 
               predicted_intercept = pred_severity_intercept,
               predicted_indiv_intercept = pred_severity_indiv_intercept$predictions,
               predicted_features=pred_severity_features$predictions,
               predicted_indiv_intercept_features=pred_severity_indiv_intercept_features$predictions)
  
  betas = rbind(data.frame(pred_severity_indiv_intercept$betas, features="indiv_intercept", scale="LOCF"),
                data.frame(pred_severity_features$betas, features="features",scale="LOCF"),
                data.frame(pred_severity_indiv_intercept_features$betas, features="indiv_intercept_features",scale="LOCF"))
  
  # Smooth and impute CAT-MH severity score 
  if(smooth_cat){
    
    print("Predicting CS(4df)")
    if(1){

      # Predict severity score 
      if(family=='binomial'){
        
        # Prediction using linear regression with population intercept only
        pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity_cs_4k, 100-train_data_y_all$severity_cs_4k), test_x=test_data_x_all_scaled, family = 'binomial')
        print("Finished prediction using linear regression with population intercept only")
        
        # Prediction using elastic net with individual intercepts only
        pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                             train_y=cbind(train_data_y_all$severity_cs_4k, 100-train_data_y_all$severity_cs_4k), 
                                                             test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]), 
                                                             family = 'binomial')
        print("Finished prediction using elastic net with individual intercepts only")
        
        # Predict severity score using elastic net with features only
        pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity_cs_4k, 100-train_data_y_all$severity_cs_4k),  test_x=test_data_x_all_scaled, family = 'binomial')
        print("Finished prediction using elastic net with features only")
        
        # Predict severity score using elastic net with individual intercepts and features
        pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=cbind(train_data_y_all$severity_cs_4k, 100-train_data_y_all$severity_cs_4k), test_x=cbind(design_matrix_test, test_data_x_all_scaled), family = 'binomial')
        print("Finished prediction using elastic net with individual intercepts and features")
        
      } else{
        # Prediction using linear regression with population intercept only
        pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity_cs_4k, test_x=test_data_x_all_scaled)
        print("Finished prediction using linear regression with population intercept only")
        
        # Prediction using elastic net with individual intercepts only
        pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                             train_y=train_data_y_all$severity_cs_4k, 
                                                             test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]))
        print("Finished prediction using elastic net with individual intercepts only")
        
        # Predict severity score using elastic net with features only
        pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity_cs_4k,  test_x=test_data_x_all_scaled)
        print("Finished prediction using elastic net with features only")
        
        # Predict severity score using elastic net with individual intercepts and features
        pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=train_data_y_all$severity_cs_4k, test_x=cbind(design_matrix_test, test_data_x_all_scaled))
        print("Finished prediction using elastic net with individual intercepts and features")
        
      }
      
      
      predictions_cs_4k = 
        data.frame(id=c(train_data_y_all$subject_id,test_data_y_all$subject_id), 
                   date = c(train_data_y_all$Date,test_data_y_all$Date), 
                   real_cat = c(train_data_y_all$real_cat,test_data_y_all$real_cat), 
                   set = c(rep("Train",nrow(train_data_y_all)), rep("Test", nrow(test_data_y_all))), 
                   scale="CS(4df)", 
                   predicted_intercept = pred_severity_intercept,
                   predicted_indiv_intercept = pred_severity_indiv_intercept$predictions,
                   predicted_features=pred_severity_features$predictions,
                   predicted_indiv_intercept_features=pred_severity_indiv_intercept_features$predictions)
      
      # Get observed CAT-DI
      predictions_cs_4k = merge(x = predictions_cs_4k %>% 
                                  mutate(sid_date=paste(id,date)),
                                y= daily_data_y %>% 
                                  mutate(sid_date=paste(subject_id,Date )) %>% 
                                  select(sid_date,severity_cs_4k) %>% 
                                  rename(observed=severity_cs_4k)) %>% 
        select(id,date,real_cat,set,scale,observed,predicted_intercept,predicted_indiv_intercept,predicted_features,predicted_indiv_intercept_features)
      
      betas_cs_4k = rbind(data.frame(pred_severity_indiv_intercept$betas, features="indiv_intercept", scale="CS(4df)"),
                          data.frame(pred_severity_features$betas, features="features",scale="CS(4df)"),
                          data.frame(pred_severity_indiv_intercept_features$betas, features="indiv_intercept_features",scale="CS(4df)"))
      
      
    }
    
    print("Predicting CS(udf)")
    if(1){
      
      # Predict severity score 
      if(family=='binomial'){
        
        # Prediction using linear regression with population intercept only
        pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity_cs_uk, 100-train_data_y_all$severity_cs_uk), test_x=test_data_x_all_scaled, family = 'binomial')
        print("Finished prediction using linear regression with population intercept only")
        
        # Prediction using elastic net with individual intercepts only
        pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                             train_y=cbind(train_data_y_all$severity_cs_uk, 100-train_data_y_all$severity_cs_uk), 
                                                             test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]), 
                                                             family = 'binomial')
        print("Finished prediction using elastic net with individual intercepts only")
        
        # Predict severity score using elastic net with features only
        pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity_cs_uk, 100-train_data_y_all$severity_cs_uk),  test_x=test_data_x_all_scaled, family = 'binomial')
        print("Finished prediction using elastic net with features only")
        
        # Predict severity score using elastic net with individual intercepts and features
        pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=cbind(train_data_y_all$severity_cs_uk, 100-train_data_y_all$severity_cs_uk), test_x=cbind(design_matrix_test, test_data_x_all_scaled), family = 'binomial')
        print("Finished prediction using elastic net with individual intercepts and features")
        
      } else{
        # Prediction using linear regression with population intercept only
        pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity_cs_uk, test_x=test_data_x_all_scaled)
        print("Finished prediction using linear regression with population intercept only")
        
        # Prediction using elastic net with individual intercepts only
        pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                             train_y=train_data_y_all$severity_cs_uk, 
                                                             test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]))
        print("Finished prediction using elastic net with individual intercepts only")
        
        # Predict severity score using elastic net with features only
        pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity_cs_uk,  test_x=test_data_x_all_scaled)
        print("Finished prediction using elastic net with features only")
        
        # Predict severity score using elastic net with individual intercepts and features
        pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=train_data_y_all$severity_cs_uk, test_x=cbind(design_matrix_test, test_data_x_all_scaled))
        print("Finished prediction using elastic net with individual intercepts and features")
        
      }
      
      
      predictions_cs_uk = 
        data.frame(id=c(train_data_y_all$subject_id,test_data_y_all$subject_id), 
                   date = c(train_data_y_all$Date,test_data_y_all$Date), 
                   real_cat = c(train_data_y_all$real_cat,test_data_y_all$real_cat), 
                   set = c(rep("Train",nrow(train_data_y_all)), rep("Test", nrow(test_data_y_all))), 
                   scale="CS(udf)", 
                   predicted_intercept = pred_severity_intercept,
                   predicted_indiv_intercept = pred_severity_indiv_intercept$predictions,
                   predicted_features=pred_severity_features$predictions,
                   predicted_indiv_intercept_features=pred_severity_indiv_intercept_features$predictions)
      
      # Get observed CAT-DI
      predictions_cs_uk = merge(x = predictions_cs_uk %>% 
                                  mutate(sid_date=paste(id,date)),
                                y= daily_data_y %>% 
                                  mutate(sid_date=paste(subject_id,Date )) %>% 
                                  select(sid_date,severity_cs_uk) %>% 
                                  rename(observed=severity_cs_uk)) %>% 
        select(id,date,real_cat,set,scale,observed,predicted_intercept,predicted_indiv_intercept,predicted_features,predicted_indiv_intercept_features)
      
      betas_cs_uk = rbind(data.frame(pred_severity_indiv_intercept$betas, features="indiv_intercept", scale="CS(4df)"),
                          data.frame(pred_severity_features$betas, features="features",scale="CS(4df)"),
                          data.frame(pred_severity_indiv_intercept_features$betas, features="indiv_intercept_features",scale="CS(4df)"))
      
      
    }
    
    print("Predicting CS(best)")
    if(1){
      
      # Predict severity score 
      if(family=='binomial'){
        
        # Prediction using linear regression with population intercept only
        pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity_cs_best, 100-train_data_y_all$severity_cs_best), test_x=test_data_x_all_scaled, family = 'binomial')
        print("Finished prediction using linear regression with population intercept only")
        
        # Prediction using elastic net with individual intercepts only
        pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                             train_y=cbind(train_data_y_all$severity_cs_best, 100-train_data_y_all$severity_cs_best), 
                                                             test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]), 
                                                             family = 'binomial')
        print("Finished prediction using elastic net with individual intercepts only")
        
        # Predict severity score using elastic net with features only
        pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=cbind(train_data_y_all$severity_cs_best, 100-train_data_y_all$severity_cs_best),  test_x=test_data_x_all_scaled, family = 'binomial')
        print("Finished prediction using elastic net with features only")
        
        # Predict severity score using elastic net with individual intercepts and features
        pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=cbind(train_data_y_all$severity_cs_best, 100-train_data_y_all$severity_cs_best), test_x=cbind(design_matrix_test, test_data_x_all_scaled), family = 'binomial')
        print("Finished prediction using elastic net with individual intercepts and features")
        
      } else{
        # Prediction using linear regression with population intercept only
        pred_severity_intercept = pred_intercept(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity_cs_best, test_x=test_data_x_all_scaled)
        print("Finished prediction using linear regression with population intercept only")
        
        # Prediction using elastic net with individual intercepts only
        pred_severity_indiv_intercept = pred_glmnet_popscale(train_x=cbind(design_matrix_train, study_day = train_data_x_all_scaled[,"study_day"]), 
                                                             train_y=train_data_y_all$severity_cs_best, 
                                                             test_x=cbind(design_matrix_test, study_day = test_data_x_all_scaled[,"study_day"]))
        print("Finished prediction using elastic net with individual intercepts only")
        
        # Predict severity score using elastic net with features only
        pred_severity_features = pred_glmnet_popscale(train_x=train_data_x_all_scaled, train_y=train_data_y_all$severity_cs_best,  test_x=test_data_x_all_scaled)
        print("Finished prediction using elastic net with features only")
        
        # Predict severity score using elastic net with individual intercepts and features
        pred_severity_indiv_intercept_features = pred_glmnet_popscale(train_x=cbind(design_matrix_train, train_data_x_all_scaled),  train_y=train_data_y_all$severity_cs_best, test_x=cbind(design_matrix_test, test_data_x_all_scaled))
        print("Finished prediction using elastic net with individual intercepts and features")
        
      }
      
      
      predictions_cs_best = 
        data.frame(id=c(train_data_y_all$subject_id,test_data_y_all$subject_id), 
                   date = c(train_data_y_all$Date,test_data_y_all$Date), 
                   real_cat = c(train_data_y_all$real_cat,test_data_y_all$real_cat), 
                   set = c(rep("Train",nrow(train_data_y_all)), rep("Test", nrow(test_data_y_all))), 
                   scale="CS(best)", 
                   predicted_intercept = pred_severity_intercept,
                   predicted_indiv_intercept = pred_severity_indiv_intercept$predictions,
                   predicted_features=pred_severity_features$predictions,
                   predicted_indiv_intercept_features=pred_severity_indiv_intercept_features$predictions)
      
      # Get observed CAT-DI
      predictions_cs_best = merge(x = predictions_cs_best %>% 
                                  mutate(sid_date=paste(id,date)),
                                y= daily_data_y %>% 
                                  mutate(sid_date=paste(subject_id,Date )) %>% 
                                  select(sid_date,severity_cs_best) %>% 
                                  rename(observed=severity_cs_best)) %>% 
        select(id,date,real_cat,set,scale,observed,predicted_intercept,predicted_indiv_intercept,predicted_features,predicted_indiv_intercept_features)
      
      betas_cs_best = rbind(data.frame(pred_severity_indiv_intercept$betas, features="indiv_intercept", scale="CS(4df)"),
                          data.frame(pred_severity_features$betas, features="features",scale="CS(4df)"),
                          data.frame(pred_severity_indiv_intercept_features$betas, features="indiv_intercept_features",scale="CS(4df)"))
      
      
    }
    
    predictions = rbind(predictions,predictions_cs_4k, predictions_cs_uk, predictions_cs_best)
    
    betas = rbind(betas,betas_cs_4k,betas_cs_uk,betas_cs_best)
    
  }
  
}

write.csv(x = predictions, file = paste0(project_dir,'/results/revision_predictions/offline_pred_', pred, imputed_pred, '_enet', fam_msg ,'_pop_lvl_', freeze_date,'.csv'),row.names = F)
write.csv(x = betas, file = paste0(project_dir,'/results/revision_feature_coefs/coef_',pred, imputed_pred, '_enet', fam_msg ,'_pop_lvl_', freeze_date,'.csv'),row.names = F)




