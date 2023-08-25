#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Predicting Subjective Measures Of Mood From Mobile Sensor Data
#%%%%%%%%%%%%%%% Code to predict CAT-DI and CAT-ANX from smartphone-inferred behavioral features
#%%%%%%%%%%%%%%% March 10th 2021
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
args=commandArgs(TRUE)
project_dir=args[1]
freeze_date=args[2]
pred=args[3]
batch=as.numeric(args[4])
impute_features = args[5]
family = args[6]

# project_dir=getwd() #"/Users/bballiu/Library/CloudStorage/GoogleDrive-bballiu@g.ucla.edu/My Drive/Work/Projects/STAND" #"/u/project/bballiu/bballiu/STAND" #'/u/home/b/bballiu/project-pajukant/STAND'
# freeze_date = '20210511'
# pred='present' # 'nextcat' #  'catdiff'
# batch=0
# impute_features = "AutoComplete" # "softimpute" #"no_imputation" # "spline" #
# family = 'normal'

if(family=='binomial') fam_msg='Binom' else fam_msg = NULL

pheno='depression' # 'suicide' 
data_dir=paste0(project_dir,'/data/')
source(file = paste0(project_dir,'/code/00_functions_for_stand.R'))

cat_di_breaks=c(0,35,65,75,100)
cat_anx_breaks=c(0,35,50,65,100)
cat_suicide_breaks=c(0,35,70,100)

cat_levels=c('normal', 'mild', 'moderate', 'severe')
cat_levels_suicide=c('low risk', 'intermediate risk', 'high risk')

offline = TRUE
prc_train=.70
split_by_interview_date = TRUE # better to keep this TRUE otherwise some LOCF-interpolated points end up in the test set
smooth_cat = TRUE # better to keep this TRUE


if(impute_features =="no_imputation") imputation_msg=imputed_pred=NULL
if(impute_features == "spline") {imputation_msg='_and_imputed' ; imputed_pred='_imputed_features' }
if(impute_features =="softimpute" ) {imputation_msg='_and_softimputed' ; imputed_pred='_softimputed_features' }
if(impute_features =="AutoComplete") {imputation_msg='_and_AutoComplete' ; imputed_pred='_AutoComplete_features' }

if(pheno=='depression') pheno_file=NULL else if(pheno=='suicide')   pheno_file='suicide_'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Load and process data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cat_n_features = data.frame(fread(file = paste0(data_dir,'cat_n_features_qced',imputation_msg,'_data_freeze_',freeze_date,'_',pred,'.csv'), header = T, stringsAsFactors = F), stringsAsFactors = F, check.names = F)

if(any(colnames(cat_n_features) == "split")) cat_n_features %<>% select(-split)
if(any(colnames(cat_n_features) == "study_name")) cat_n_features %<>% select(-study_name)
if(any(colnames(cat_n_features) == "treatment_wave")) cat_n_features %<>% select(-treatment_wave)

cat_n_features %<>% 
  filter(!subject_id %in% c("7745", "8631", "8690")) %>% # these are individuals from an entry level study
  mutate(date=as.Date(x = as.character(date)),
         Depression.category=factor(x = cut(x = Depression.severity, breaks = cat_di_breaks, labels = cat_levels, include.lowest = T), levels = cat_levels),
         Anxiety.category=factor(x = cut(x = Anxiety.severity, breaks = cat_anx_breaks, labels = cat_levels, include.lowest = T), levels = cat_levels),
         Suicide.Scale.category=factor(x = cut(x = Suicide.Scale.severity, breaks = cat_suicide_breaks, labels = cat_levels_suicide, include.lowest = T), levels = cat_levels_suicide)) %>% 
  droplevels() %>% 
  arrange(subject_id,date) 


indeces = sort(unique(cat_n_features$subject_id))
if(batch==0) index=indeces else index = matrix(indeces, ncol = 3)[batch,]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Idiographic prediction of CAT-DI severity scores
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

offline_pred_all_enet = betas_all_enet = offline_pred_all_enet_smooth_pred = offline_pred_all_enet_intercept = NULL

for(u in index){
  # u=9873
  print(u)
  
  # Daily data for individual u
  daily_data = cat_n_features %>% 
    filter(subject_id==u) %>% 
    select(-subject_id, -study_subject_id, -cat_duration, -MDD.diagnosis, -Suicide.diagnosis, -Anxiety.severity, -Mania.severity, -Suicide.Scale.severity, -Anxiety.category, -Mania.category, -Suicide.Scale.category, -weekday,  -cat_start_time, -os ) %>%
    rename(Date=date, severity = Depression.severity, category=Depression.category) 
  
  daily_data = daily_data[,-grep(pattern = 'dummy',colnames(daily_data))]
  
  # Remove columns with many missing values and rows with any missing values
  daily_data=daily_data[,!apply(is.na(daily_data),2,all)] # remove features that are completely missing
  daily_data=daily_data[apply(is.na(daily_data[,1:min(ncol(daily_data),50)]),1,sum)/min(ncol(daily_data),50)<.9,] #remove days where most features are missing
  daily_data=daily_data[,apply(is.na(daily_data),2,sum) < min(31,nrow(daily_data))] # remove features with more than 31 days missing
  daily_data = na.omit(daily_data)
  
  # Start with first measured (not inferred) CAT score and end with last measured CAT score.
  if(0){
    first_real_cat=head(x=which(daily_data$real_cat==1),n=1)
    last_real_cat=tail(x=which(daily_data$real_cat==1),n=1)
    daily_data = daily_data[first_real_cat:last_real_cat,]
  }
  N = nrow(daily_data)
  if(N<30) print('This individual only has 30 days worth of data. Skipping to next one.')
  if(N<30) next()
  
  # Separate X and Y
  daily_data_y = daily_data %>% select(Date,severity,category,real_cat) 
  daily_data_x = daily_data %>% select(-Date,-severity,-category,-real_cat) 
  daily_data_x=daily_data_x[,apply(daily_data_x,2,var)!=0] #remove columns without variation 
  
  if(sum(daily_data_y$real_cat)<5) { print('This individual has less than 5 observed CAT-DI with non missing feature data. Skipping to next one.') ; next()}
  
  
  # Scale X data 
  daily_data_x_scaled=data.frame(sapply(1:ncol(daily_data_x), function(var) scale(as.numeric(daily_data_x[,var]))))
  colnames(daily_data_x_scaled)=colnames(daily_data_x)
  
  # Split data into training and test set
  if(split_by_interview_date){
    N_obs=sum(daily_data$real_cat)
    train_end = max(1, round(N_obs*prc_train))
    train_end_date=(daily_data %>% filter(real_cat==1))[train_end+1,'Date']-1
    train_end=sum(daily_data$Date<=train_end_date)
  } else {
    train_end = max(1, round(N*prc_train))  
  }
  
  train_data_x=as.matrix(daily_data_x_scaled[1:train_end,])
  test_data_x=as.matrix(daily_data_x_scaled[(train_end+1):N,])
  train_data_y=daily_data_y[1:train_end,c('Date','real_cat','severity')]
  
  # Stop if there is no variability in train_data_y
  if(var(train_data_y$severity)==0) next; 
  
  # Predict severity score 
  if(family=='binomial'){
    pred_severity = pred_glmnet_popscale(train_x=train_data_x, train_y=cbind(train_data_y$severity, 100-train_data_y$severity), test_x=test_data_x, family = 'binomial') 
    pred_severity_intercept = pred_intercept(train_x=train_data_x, train_y=cbind(train_data_y$severity, 100-train_data_y$severity), test_x=test_data_x, family = 'binomial')
    
  } else{
    pred_severity = pred_glmnet_popscale(train_x=train_data_x, train_y=train_data_y$severity, test_x=test_data_x) 
    pred_severity_intercept = pred_intercept(train_x=train_data_x, train_y=train_data_y$severity, test_x=test_data_x)
  }
  
  predictions_enet_offline =  data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)),  scale='LOCF', observed = daily_data$severity, predicted=pred_severity$predictions)
  predictions_enet_offline_intercept =  data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='LOCF', observed = daily_data$severity, predicted=pred_severity_intercept)
  betas_enet_offline =  data.frame(id=u, scale='LOCF', feature = pred_severity$betas$variable, betas = pred_severity$betas$coef)
  
  # Smooth and impute CAT-MH severity score 
  if(smooth_cat & sum(train_data_y$real_cat)>=4){
    
    unique_cat=max(2,length(unique(daily_data_y$category)))
    unique_cat_train=max(2,length(unique(train_data_y$category)))
    
    # golden truth = imputation across all time points
    severity_smooth_4_knots = smooth.spline2(daily_data = daily_data_y, imputation_type = 'fixed_knots', n_knots = 4)  
    severity_smooth_u_knots = smooth.spline2(daily_data = daily_data_y, imputation_type = 'fixed_knots', n_knots = unique_cat)   
    severity_smooth_best = smooth.spline2(daily_data = daily_data_y, imputation_type = 'best')  
    
    # local truth = imputation only on training set
    train_data_y$severity_smooth_4_knots = smooth.spline2(daily_data = train_data_y, imputation_type = 'fixed_knots', n_knots = 4)  
    train_data_y$severity_smooth_u_knots = smooth.spline2(daily_data = train_data_y, imputation_type = 'fixed_knots', n_knots = unique_cat_train)   
    train_data_y$severity_smooth_best = smooth.spline2(daily_data = train_data_y, imputation_type = 'best')  
  
    # Predict CAT-MH severity score 
    if(family=='binomial'){
      
      if(
        any(severity_smooth_4_knots<0|severity_smooth_4_knots>100)|
        any(severity_smooth_u_knots<0|severity_smooth_u_knots>100)|
        any(severity_smooth_best<0|severity_smooth_best>100)|
        any(train_data_y$severity_smooth_4_knots<0|train_data_y$severity_smooth_4_knots>100)|
         any(train_data_y$severity_smooth_u_knots<0|train_data_y$severity_smooth_u_knots>100)|
         any(train_data_y$severity_smooth_best<0|train_data_y$severity_smooth_best>100)) { 
        print('Spline for this individual returns negative values. Skipping to next one.') ; 
        next()
        }
      
      # using model with features
      pred_severity_smooth_4_knots = pred_glmnet_popscale(train_x=train_data_x, train_y=cbind(train_data_y$severity_smooth_4_knots, 100-train_data_y$severity_smooth_4_knots), test_x=test_data_x, family = 'binomial') 
      pred_severity_smooth_u_knots = pred_glmnet_popscale(train_x=train_data_x, train_y=cbind(train_data_y$severity_smooth_u_knots, 100-train_data_y$severity_smooth_u_knots), test_x=test_data_x, family = 'binomial') 
      pred_severity_smooth_best = pred_glmnet_popscale(train_x=train_data_x, train_y=cbind(train_data_y$severity_smooth_best, 100-train_data_y$severity_smooth_best), test_x=test_data_x, family = 'binomial') 
      
      # using intercept only model
      pred_severity_smooth_4_knots_intercept = pred_intercept(train_x=train_data_x, train_y=cbind(train_data_y$severity_smooth_4_knots, 100-train_data_y$severity_smooth_4_knots), test_x=test_data_x, family = 'binomial')
      pred_severity_smooth_u_knots_intercept = pred_intercept(train_x=train_data_x, train_y=cbind(train_data_y$severity_smooth_u_knots, 100-train_data_y$severity_smooth_u_knots), test_x=test_data_x, family = 'binomial')
      pred_severity_smooth_best_intercept = pred_intercept(train_x=train_data_x, train_y=cbind(train_data_y$severity_smooth_best, 100-train_data_y$severity_smooth_best), test_x=test_data_x, family = 'binomial')
      
    } else {
      
      # using model with features
      pred_severity_smooth_4_knots = pred_glmnet_popscale(train_x=train_data_x, train_y=train_data_y$severity_smooth_4_knots, test_x=test_data_x)
      pred_severity_smooth_u_knots = pred_glmnet_popscale(train_x=train_data_x, train_y=train_data_y$severity_smooth_u_knots, test_x=test_data_x)
      pred_severity_smooth_best = pred_glmnet_popscale(train_x=train_data_x, train_y=train_data_y$severity_smooth_best, test_x=test_data_x)
      
      # using intercept only model
      pred_severity_smooth_4_knots_intercept = pred_intercept(train_x=train_data_x, train_y=train_data_y$severity_smooth_4_knots, test_x=test_data_x)
      pred_severity_smooth_u_knots_intercept = pred_intercept(train_x=train_data_x, train_y=train_data_y$severity_smooth_u_knots, test_x=test_data_x)
      pred_severity_smooth_best_intercept = pred_intercept(train_x=train_data_x, train_y=train_data_y$severity_smooth_best, test_x=test_data_x)
    }
    
    predictions_enet_offline_smooth_4_knots = data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='CS(4df)',  observed = severity_smooth_4_knots, predicted=pred_severity_smooth_4_knots$predictions)
    betas_enet_offline_smooth_4_knots =  data.frame(id=u, scale='CS(4df)', feature = pred_severity_smooth_4_knots$betas$variable, betas = pred_severity_smooth_4_knots$betas$coef)
    
    predictions_enet_offline_smooth_u_knots = data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='CS(udf)', observed = severity_smooth_u_knots, predicted=pred_severity_smooth_u_knots$predictions)
    betas_enet_offline_smooth_u_knots =  data.frame(id=u, scale='CS(udf)', feature = pred_severity_smooth_u_knots$betas$variable, betas = pred_severity_smooth_u_knots$betas$coef)
    
    predictions_enet_offline_smooth_best = data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='CS(best)', observed = severity_smooth_best, predicted=pred_severity_smooth_best$predictions)
    betas_enet_offline_smooth_best =  data.frame(id=u, scale='CS(best)', feature = pred_severity_smooth_best$betas$variable, betas = pred_severity_smooth_best$betas$coef)
    
    predictions_enet_offline_smooth_4_knots_intercept = data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='CS(4df)', observed = severity_smooth_4_knots, predicted=pred_severity_smooth_4_knots_intercept)
    
    predictions_enet_offline_smooth_u_knots_intercept = data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='CS(udf)', observed = severity_smooth_u_knots, predicted=pred_severity_smooth_u_knots_intercept)
    
    predictions_enet_offline_smooth_best_intercept = data.frame(id=u, date = daily_data$Date, real_cat = daily_data$real_cat, set = c(rep('Train',train_end), rep('Test', N-train_end)), scale='CS(best)', observed = severity_smooth_best, predicted=pred_severity_smooth_best_intercept)
    
    predictions_enet_offline = rbind(predictions_enet_offline, 
                                     predictions_enet_offline_smooth_4_knots, 
                                     predictions_enet_offline_smooth_u_knots, 
                                     predictions_enet_offline_smooth_best)
    
    betas_enet_offline = rbind(betas_enet_offline, 
                               betas_enet_offline_smooth_4_knots, 
                               betas_enet_offline_smooth_u_knots, 
                               betas_enet_offline_smooth_best)
    
    
    predictions_enet_offline_intercept = rbind(predictions_enet_offline_intercept, 
                                               predictions_enet_offline_smooth_4_knots_intercept,
                                               predictions_enet_offline_smooth_u_knots_intercept,
                                               predictions_enet_offline_smooth_best_intercept)
    
    
    # Smooth predictions - Not using this anymore 
    predictions_enet_offline_smooth_pred = 
      rbind(predictions_enet_offline, 
            
            predictions_enet_offline_smooth_4_knots %>% 
              mutate(predicted2=ifelse(set=='Train', yes = observed, no = predicted),
                     predicted=smooth.spline(x = as.numeric(date),  y = predicted2, df=4)$y) %>% 
              select(-predicted2),
            
            predictions_enet_offline_smooth_u_knots %>% 
              mutate(predicted2=ifelse(set=='Train', yes = observed, no = predicted),
                     predicted=smooth.spline(x = as.numeric(date),  y = predicted2, df=unique_cat_train)$y) %>% 
              select(-predicted2),
            
            predictions_enet_offline_smooth_best%>% 
              mutate(predicted2=ifelse(set=='Train', yes = observed, no = predicted),
                     predicted=smooth.spline(x = as.numeric(date),  y = predicted2)$y) %>% 
              select(-predicted2)) 
    
    
    
  }
  
  offline_pred_all_enet=rbind(offline_pred_all_enet, predictions_enet_offline)
  offline_pred_all_enet_intercept=rbind(offline_pred_all_enet_intercept, predictions_enet_offline_intercept)
  betas_all_enet=rbind(betas_all_enet, betas_enet_offline)
  offline_pred_all_enet_smooth_pred=rbind(offline_pred_all_enet_smooth_pred, predictions_enet_offline_smooth_pred)
  
}

if(batch == 0) {
  write.csv(x = offline_pred_all_enet, file = paste0(project_dir,'/results/revision_predictions/offline_pred_', pred, imputed_pred, '_enet', fam_msg,'_', freeze_date, '.csv'),row.names = F)
  write.csv(x = betas_all_enet, file = paste0(project_dir,'/results/revision_feature_coefs/coef_',pred, imputed_pred, '_enet', fam_msg,'_', freeze_date,'.csv'),row.names = F)
  write.csv(x = offline_pred_all_enet_intercept, file = paste0(project_dir,'/results/revision_predictions/offline_pred_', pred, imputed_pred, '_intercept', fam_msg,'_', freeze_date, '.csv'), row.names = F)
  
} else {
  write.csv(x = offline_pred_all_enet, file = paste0(project_dir,'/results/revision_predictions/offline_pred_', pred, imputed_pred, '_enet', fam_msg,'_batch', batch, freeze_date, '.csv'),row.names = F)
  write.csv(x = betas_all_enet, file = paste0(project_dir,'/results/revision_feature_coefs/coef_',pred, imputed_pred, '_enet', fam_msg,'_batch', batch, freeze_date,'.csv'),row.names = F)
  write.csv(x = offline_pred_all_enet_intercept, file = paste0(project_dir,'/results/revision_predictions/offline_pred_', pred, imputed_pred, '_intercept', fam_msg,'_batch', batch, freeze_date, '.csv'), row.names = F)
  }






