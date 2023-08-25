#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%% June 23rd 2021
#%%%%%%%%%%%%%%% Functions to generate manuscript figures 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())
source(file = 'code/00_functions_for_stand.R')

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  cat_di_breaks=c(0,35,65,75,100)
  cat_anx_breaks=c(0,35,50,65,100)
  cat_levels=c("normal", "mild", "moderate", "severe")
  freeze_date = "20210511"
  pred='present' # 'nextcat' #  'catdiff'
  filter_n_interviews_individuals = FALSE
  N_threshold=5
  
  # Prediction parameters
  scale_levels = c("LOCF", "CS(best)", "CS(4df)", "CS(udf)")
  scale_labels = c("LOCF", "CS(cv)", "CS(4df)", "CS(2-4df)")
  scale_colors=c("#450D54", "#30688E", "#35B779", "#FDE725")

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Read and process predictions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
process_predictions = function(timeframe = 'daily',
                               family = 'binomial', 
                               set_bad_predictions_na = FALSE, 
                               model="Idiographic",
                               features = 'Feature-based',
                               impute_features = 'AutoComplete', 
                               cat_levels= c("normal", "mild", "moderate", "severe"), 
                               cat_breaks = c(0,35,65,75,100),
                               pred = 'present', 
                               freeze_date = "20210511",
                               scale_levels = c("LOCF", "CS(best)", "CS(4df)", "CS(udf)"),
                               scale_labels = c("LOCF", "CS(cv)", "CS(4df)", "CS(2-4df)")
){
  
  ######## Set parameter values
  if(family=='binomial') fam_msg='Binom' else fam_msg = NULL
  
  if(model=="Idiographic") model_msg='_' else model_msg='_pop_lvl_'
  
  features_msg='_enet' 
  if(model=="Idiographic") {if(features == 'Feature-based') features_msg='_enet' else features_msg = '_intercept'}
  
  if(impute_features == 'cubicSpline') {imputation_msg='_and_imputed' ; imputed_pred='_imputed_features' }
  if(impute_features =='softImpute') {imputation_msg='_and_softimputed' ; imputed_pred='_softimputed_features' }
  if(impute_features =='AutoComplete') {imputation_msg='_and_AutoComplete' ; imputed_pred='_AutoComplete_features' }
  if(impute_features =='noImputation') imputation_msg=imputed_pred=NULL
  
  if(timeframe == 'weekly') {timeframe_msg = 'weekly_' ; features_msg='_enet'} else timeframe_msg=NULL
  
  filename = paste0('results/revision_predictions/',timeframe_msg,'offline_pred_', pred, imputed_pred, features_msg, fam_msg, model_msg, freeze_date,'.csv')
  
  ######## Read and process predictions
  predictions=read.csv(file = filename, header = T) 
  
  if(timeframe == 'weekly') { 
    
    if(model=="Idiographic") predictions %<>% filter(model == "Idiographic") 
    if(model=="Nomothetic") predictions %<>% filter(model == "Nomothetic") %>% select(-model)
    if(model=="Nomothetic*") predictions %<>% filter(model == "Nomothetic*") %>% select(-model)
    
    if(features == 'Feature-based') predictions %<>% filter(scale=="Features") %>% select(-scale)
    if(features == 'Intercept-only') predictions %<>% filter(scale=="Intercept") %>% select(-scale)
  }

  if(timeframe == 'daily'){
    if(model=="Nomothetic" & features == 'Feature-based') predictions %<>% select(-predicted_intercept, -predicted_indiv_intercept, -predicted_indiv_intercept_features) %>% rename(predicted=predicted_features)
    if(model=="Nomothetic" & features == 'Intercept-only') predictions %<>% select(-predicted_features, -predicted_indiv_intercept, -predicted_indiv_intercept_features) %>% rename(predicted=predicted_intercept)
    if(model=="Nomothetic*" & features == 'Feature-based') predictions %<>% select(-predicted_intercept, -predicted_indiv_intercept, -predicted_features) %>% rename(predicted=predicted_indiv_intercept_features)
    if(model=="Nomothetic*" & features == 'Intercept-only') predictions %<>% select(-predicted_features, -predicted_intercept, -predicted_indiv_intercept_features) %>% rename(predicted=predicted_indiv_intercept)
  }
  
  predictions %<>%
    mutate(timeframe=timeframe, family = family, featue_imputation = impute_features, set_bad_predictions_na=set_bad_predictions_na, model = model, features = features) %>%
    # convert data to R date format
    mutate(date = as.Date(date)) %>%
    # convert predicted proportions back to CAT-DI scale
    mutate(predicted=ifelse(test = family=='binomial', yes = 100*(1-predicted), no = predicted)) %>% 
    
    # set predictions outside the [0,100] range to NA
    mutate(observed = case_when((observed < 0 & set_bad_predictions_na) ~ NA_real_ , 
                                (observed > 100 & set_bad_predictions_na) ~ NA_real_, 
                                TRUE ~ observed),
           predicted = case_when((predicted < 0 & set_bad_predictions_na) ~ NA_real_ , 
                                 (predicted > 100 & set_bad_predictions_na) ~ NA_real_, 
                                 TRUE ~ predicted)) %>% 
    # convert continuous CAT-DI scores into categories
    mutate(true_category=factor(x = cut(x = observed, breaks = cat_breaks, labels = cat_levels, include.lowest = T), levels = cat_levels), 
           predicted_category=factor(cut(x = predicted, breaks = cat_breaks,  labels = cat_levels, include.lowest = T), levels = cat_levels)) %>%
    
    
    # re-level scale variable and variable that indicates if CAT-DI value is observed or imputed
    mutate(real_cat = ifelse(test = timeframe == 'daily', yes = real_cat, no = 1),
           scale = ifelse(test = timeframe == 'daily', yes = scale, no = "LOCF")) %>% 
    mutate(real_cat = factor(x = real_cat, levels = c(1,0), labels = c("observed", "imputed")),
           scale=factor(x = scale, levels = scale_levels, labels = scale_labels)) %>% 
    # order prediction by individual ID and CAT-DI assessment date
    arrange(id, date) %>% 
    # reorder columns 
    select(timeframe, family, featue_imputation, set_bad_predictions_na, model, features, id, date, real_cat, set,scale, observed, predicted, true_category, predicted_category)
  
  ######## Return final output

  return(predictions)
}

idio_predictions_features_binom_AutoComp =     process_predictions (timeframe = 'daily', model="Idiographic", features = 'Feature-based', family = 'binomial',   impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
idio_predictions_baseline_binom_AutoComp =     process_predictions (timeframe = 'daily', model="Idiographic", features = 'Intercept-only', family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nom_predictions_features_binom_AutoComp =      process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Feature-based',   family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nom_predictions_baseline_binom_AutoComp =      process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Intercept-only',  family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nommod_predictions_features_binom_AutoComp =   process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Feature-based',  family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nommod_predictions_baseline_binom_AutoComp =   process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Intercept-only', family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)

idio_predictions_features_binom_softImpute =   process_predictions (timeframe = 'daily', model="Idiographic", features = 'Feature-based', family = 'binomial',   impute_features = 'softImpute', set_bad_predictions_na = FALSE)
idio_predictions_baseline_binom_softImpute =   process_predictions (timeframe = 'daily', model="Idiographic", features = 'Intercept-only', family = 'binomial',  impute_features = 'softImpute', set_bad_predictions_na = FALSE)
nom_predictions_features_binom_softImpute =    process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Feature-based',   family = 'binomial',  impute_features = 'softImpute', set_bad_predictions_na = FALSE)
nom_predictions_baseline_binom_softImpute =    process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Intercept-only',  family = 'binomial',  impute_features = 'softImpute', set_bad_predictions_na = FALSE)
nommod_predictions_features_binom_softImpute = process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Feature-based',  family = 'binomial',  impute_features = 'softImpute', set_bad_predictions_na = FALSE)
nommod_predictions_baseline_binom_softImpute = process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Intercept-only', family = 'binomial',  impute_features = 'softImpute', set_bad_predictions_na = FALSE)


idio_predictions_features_norm_AutoComp =      process_predictions (timeframe = 'daily', model="Idiographic", features = 'Feature-based', family = 'normal',     impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
idio_predictions_baseline_norm_AutoComp =      process_predictions (timeframe = 'daily', model="Idiographic", features = 'Intercept-only', family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
# check
nom_predictions_features_norm_AutoComp =       process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Feature-based',   family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nom_predictions_baseline_norm_AutoComp =       process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Intercept-only',  family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nommod_predictions_features_norm_AutoComp =    process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Feature-based',  family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nommod_predictions_baseline_norm_AutoComp =    process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Intercept-only', family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)


idio_predictions_features_norm_softImpute =    process_predictions (timeframe = 'daily', model="Idiographic", features = 'Feature-based', family = 'normal',     impute_features = 'softImpute',   set_bad_predictions_na = FALSE)
idio_predictions_baseline_norm_softImpute =    process_predictions (timeframe = 'daily', model="Idiographic", features = 'Intercept-only', family = 'normal',    impute_features = 'softImpute',   set_bad_predictions_na = FALSE)
# check
nom_predictions_features_norm_softImpute =     process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Feature-based',   family = 'normal',    impute_features = 'softImpute',   set_bad_predictions_na = FALSE)
nom_predictions_baseline_norm_softImpute =     process_predictions (timeframe = 'daily', model="Nomothetic", features = 'Intercept-only',  family = 'normal',    impute_features = 'softImpute',   set_bad_predictions_na = FALSE)
nommod_predictions_features_norm_softImpute =  process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Feature-based',  family = 'normal',    impute_features = 'softImpute',   set_bad_predictions_na = FALSE)
nommod_predictions_baseline_norm_softImpute =  process_predictions (timeframe = 'daily', model="Nomothetic*", features = 'Intercept-only', family = 'normal',    impute_features = 'softImpute',   set_bad_predictions_na = FALSE)


idio_weeklypredictions_features_binom_AutoComp =     process_predictions (timeframe = 'weekly', model="Idiographic", features = 'Feature-based', family = 'binomial',   impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
idio_weeklypredictions_baseline_binom_AutoComp =     process_predictions (timeframe = 'weekly', model="Idiographic", features = 'Intercept-only', family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nom_weeklypredictions_features_binom_AutoComp =      process_predictions (timeframe = 'weekly', model="Nomothetic", features = 'Feature-based',   family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nom_weeklypredictions_baseline_binom_AutoComp =      process_predictions (timeframe = 'weekly', model="Nomothetic", features = 'Intercept-only',  family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nommod_weeklypredictions_features_binom_AutoComp =   process_predictions (timeframe = 'weekly', model="Nomothetic*", features = 'Feature-based',  family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)
nommod_weeklypredictions_baseline_binom_AutoComp =   process_predictions (timeframe = 'weekly', model="Nomothetic*", features = 'Intercept-only', family = 'binomial',  impute_features = 'AutoComplete', set_bad_predictions_na = FALSE)


idio_weeklypredictions_features_norm_AutoComp =      process_predictions (timeframe = 'weekly', model="Idiographic", features = 'Feature-based', family = 'normal',     impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
idio_weeklypredictions_baseline_norm_AutoComp =      process_predictions (timeframe = 'weekly', model="Idiographic", features = 'Intercept-only', family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nom_weeklypredictions_features_norm_AutoComp =       process_predictions (timeframe = 'weekly', model="Nomothetic", features = 'Feature-based',   family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nom_weeklypredictions_baseline_norm_AutoComp =       process_predictions (timeframe = 'weekly', model="Nomothetic", features = 'Intercept-only',  family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nommod_weeklypredictions_features_norm_AutoComp =    process_predictions (timeframe = 'weekly', model="Nomothetic*", features = 'Feature-based',  family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)
nommod_weeklypredictions_baseline_norm_AutoComp =    process_predictions (timeframe = 'weekly', model="Nomothetic*", features = 'Intercept-only', family = 'normal',    impute_features = 'AutoComplete',   set_bad_predictions_na = FALSE)

# Combine predictions
all_predictions_binom_AutoComp = rbind(
  idio_predictions_features_binom_AutoComp,
  idio_predictions_baseline_binom_AutoComp,
  nom_predictions_features_binom_AutoComp ,
  nom_predictions_baseline_binom_AutoComp ,
  nommod_predictions_features_binom_AutoComp,
  nommod_predictions_baseline_binom_AutoComp) %>% 
  mutate(model = factor(x = model,levels = c("Nomothetic", "Nomothetic*","Idiographic")),
         features = factor(x = features,levels = c("Intercept-only", "Feature-based")))


# Compute nr of interviews in training and test set.
nr_interviews = idio_predictions_features_binom_AutoComp %>% filter(scale=="LOCF") %>% group_by(id) %>% 
  summarise(n=n(),
            n_train=sum(set=="Train"),
            n_test=sum(set=="Test"),
            total=sum(real_cat=="observed"),
            train=sum(set=="Train" & real_cat=="observed"),
            test=sum(set=="Test" & real_cat=="observed"))  %>% 
  arrange(desc(total))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%% Feature importance
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
feature_importance=read.csv(file = paste0('results/revision_feature_coefs/coef_',pred, '_AutoComplete_features_enetBinom_', freeze_date,'.csv'),header = T) %>%
  mutate(scale=factor(x = scale, levels = scale_levels, labels = scale_labels)) %>% 
  filter(id %in% unique(idio_predictions_features_binom_AutoComp$id)) %>%
  droplevels()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Read and process data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%% Raw CAT-MH and Features
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  cat_n_features_raw = fread(file = paste0('data/cat_n_features_qced_data_freeze_',freeze_date,'_',pred,'.csv'), stringsAsFactors = F) %>% 
    
    rename(depression_severity=Depression.severity,
           anxiety_severity=Anxiety.severity) %>%
    mutate(interview_date=as.Date(x = as.character(cat_start_time)),
           depression_category=factor(x = cut(x = depression_severity, breaks = cat_di_breaks, labels = cat_levels,include.lowest = T), levels = cat_levels),
           anxiety_category=factor(x = cut(x = anxiety_severity, breaks = cat_anx_breaks, labels = cat_levels,include.lowest = T), levels = cat_levels)
    )  %>%
    arrange(subject_id,study_subject_id, date) 
  
  cat_n_features_raw$study_name = sapply(X = 1:nrow(cat_n_features_raw), FUN = function(i) strsplit(cat_n_features_raw$study_subject_id[i],split = "_")[[1]][1])  
  
  cat_n_features_raw %<>% filter(!study_name %in% c("is1","ss1","ss3")) %>% # remove entry studies and controls / normal 
    droplevels() %>% 
    mutate(treatment_wave = factor(x = study_name, 
                                   levels = c("is2", "ss2", "itn", "itnc"),
                                   labels = c("Wave 1 - Online support", 
                                              "Wave 2 - Online support", 
                                              "Wave2 - Clinical care", 
                                              "Wave2 - Clinical care"))) %>%
    select(-study_name) %>%
    mutate(wave = factor(x = ifelse(test = grepl(pattern = "Wave 1",x = treatment_wave),yes = "Wave 1", no = "Wave 2")),
           treatment_group = factor(x = case_when(
             treatment_wave=="Wave 1 - Online support"~"online support", 
             treatment_wave=="Wave 2 - Online support"~"online support", 
             treatment_wave=="Wave2 - Clinical care"~"clinical care",
             TRUE ~ NA_character_
           ), levels = c("online support","clinical care"))) 
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%% Imputed CAT-MH and Features
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  cat_n_features = fread(file =paste0('data/cat_n_features_qced','_and_AutoComplete','_data_freeze_',freeze_date,'_',pred,'.csv'), stringsAsFactors = F) %>% 
    rename(depression_severity=Depression.severity,
           anxiety_severity=Anxiety.severity) %>%
    mutate(interview_date=as.Date(x = as.character(cat_start_time)),
           depression_category=factor(x = cut(x = depression_severity, breaks = cat_di_breaks, labels = cat_levels,include.lowest = T), levels = cat_levels),
           anxiety_category=factor(x = cut(x = anxiety_severity, breaks = cat_anx_breaks, labels = cat_levels,include.lowest = T), levels = cat_levels)
    )  %>%
    arrange(subject_id,study_subject_id, date) 
  
  cat_n_features$study_name = sapply(X = 1:nrow(cat_n_features), FUN = function(i) strsplit(cat_n_features$study_subject_id[i],split = "_")[[1]][1])  
  
  cat_n_features %<>% 
    filter(!study_name %in% c("is1","ss1","ss3")) %>% # remove entry studies and controls / normal 
    droplevels() %>% 
    mutate(treatment_wave = factor(x = study_name, 
                                   levels = c("is2", "ss2", "itn", "itnc"),
                                   labels = c("Wave 1 - Online support", 
                                              "Wave 2 - Online support", 
                                              "Wave2 - Clinical care", 
                                              "Wave2 - Clinical care"))) %>%
    select(-study_name) %>%
    mutate(wave = factor(x = ifelse(test = grepl(pattern = "Wave 1",x = treatment_wave),yes = "Wave 1", no = "Wave 2")),
           treatment_group = factor(x = case_when(
             treatment_wave=="Wave 1 - Online support"~"online support", 
             treatment_wave=="Wave 2 - Online support"~"online support", 
             treatment_wave=="Wave2 - Clinical care"~"clinical care",
             TRUE ~ NA_character_
           ), levels = c("online support","clinical care")))
  
  # Keep data only for individuals with prediction
  ind_date_2keep= idio_predictions_features_binom_AutoComp %>% 
    filter(scale=="LOCF") %>% 
    mutate(ind_date=paste(id,date,sep = "-")) %>% 
    pull(ind_date)
  
  cat_n_features %<>% 
    mutate(ind_date=paste(subject_id,date,sep = "-")) %>%  
    filter(ind_date %in% ind_date_2keep) %>% 
    droplevels()
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%% Observed CAT-MH 
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  obs_data=cat_n_features %>% 
    select(subject_id, study_subject_id, wave, treatment_group, treatment_wave, interview_date, depression_severity, depression_category, anxiety_severity, anxiety_category, real_cat, os) %>% 
    mutate(real_cat_inst=paste0(study_subject_id,interview_date)) %>% 
    filter(!duplicated(real_cat_inst)) %>% 
    select(-real_cat_inst)
  
  # Order subject ID by first interview start date
  id_order=obs_data %>% 
    group_by(subject_id) %>% 
    summarise(start=min(interview_date)) %>% 
    arrange(start) %>% 
    pull(subject_id)
  
  obs_data %<>% mutate(subject_id=factor(x = subject_id, levels = id_order))
  
  # Rename study participants
  obs_data %<>% 
    mutate(protocol_days= ifelse(test = wave =="Wave 1",yes = 20*7,no = 40*7),
           frequency_days=ifelse(test = treatment_group =="clinical care",yes = 7,no = 14))
  
  # Create year, month, day, and covid variables
  obs_data %<>% arrange(subject_id, study_subject_id, interview_date)
  
  obs_data %<>% 
    group_by(subject_id) %>% 
    mutate(entry_date=as.Date(min(interview_date,na.rm = T))) %>% 
    mutate(days=as.numeric(as.Date(interview_date)-entry_date)) %>% 
    mutate(year=year(interview_date), 
           month=month(interview_date), 
           season=case_when(month %in% c(12,1,2)~"winter",
                            month %in% 3:5~"spring",
                            month %in% 6:8~"summer",
                            month %in% 9:11~"autumn"), 
           covid=interview_date >=as.Date("2020-03-01")) 
  
  # Keep data only for individuals with prediction
  obs_data %<>% filter(subject_id %in% unique(idio_predictions_features_binom_AutoComp$id)) %>% droplevels()
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%% 
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Demographics
  demographics = read.csv(file = paste0('data/demographics_data_freeze_',freeze_date,'.csv')) 
  
  # CAT-DI administration schedule
  study_design=readxl::read_xlsx(path = 'data/cat_mh_assesment_schedule.xlsx')
  study_design_long=reshape2::melt(study_design, id.vars="Event") %>% 
    filter(!is.na(value)) %>% 
    mutate(value = factor(x = value, levels = c("Baseline", paste("Week", 1:40))),
           variable= factor(x = str_replace(string = variable,pattern = " - ",
                                            replacement = "\n"),
                            levels = c("Wave 1\nOnline support",
                                       "Wave 2\nOnline support",
                                       "Wave 2\nClinical care" ))) 
  
  # CAT-DI for all individuals in the study (before filtering by prediction results)
  cat_mh = read.csv(file = 'data/cat_clean_data_freeze_20210511.csv', stringsAsFactors = T)  %>% 
    mutate(treatment_group = factor(x = treatment_group, levels = c("online support","clinical care"))) %>% 
    arrange(subject_id, study_subject_id, cat_start_time) %>% 
    mutate(cat_start_time=as.Date(x = as.character(cat_start_time))) %>% 
    group_by(study_subject_id) %>% 
    mutate(entry_date=as.Date(min(cat_start_time,na.rm = T))) %>% 
    mutate(days=1+as.numeric(as.Date(cat_start_time)-entry_date)) %>% 
    ungroup() 
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%% Feature description and missingness
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  feature_description=read.csv(file =  paste0('data/features_data_freeze_',freeze_date,'_column_descriptions.csv')) %>% 
    filter(type %in% c("mobility","device usage", "sleep","social interaction", "other", "qc")) %>% 
    mutate(timeframe = factor(x = timeframe, levels = c("present", "past"), labels = c("present-only", "present & past"))) %>% 
    droplevels() %>% 
    arrange(type)
  
  missing_info=merge(x = read.csv(file = 'results/missing_data_percentage.csv', header = T)[,-c(2:6)] %>% 
                       reshape2::melt(id.vars="X") %>% 
                       rename(iid=X), 
                     y = feature_description, by.x = "variable", by.y = "feature") %>% 
    # Keep data only for individuals with prediction
    filter(iid %in% unique(idio_predictions_features_binom_AutoComp$id))
    
  


}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Figure 1: Overview of the study
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){

  #%%%%%%%
  # Follow-up and adherence 
  #%%%%%%%
  adherence=obs_data %>%
    group_by(study_subject_id) %>% 
    summarise(study_name=unique(treatment_wave)[1],
              protocol_days=max(as.numeric(protocol_days)),
              frequency_days=max(as.numeric(frequency_days)),
              nr_interviews=n(),
              follow_up_length_days=as.numeric(max(interview_date)-min(interview_date)),
              med_time_bw_interviews=as.numeric(median(diff(as.Date(interview_date))))) %>% 
    mutate(expected_nr_interviews=as.numeric(protocol_days)/as.numeric(frequency_days)) %>% 
    ungroup() %>% 
    filter(follow_up_length_days<=280)
  levels(adherence$study_name) = gsub(pattern = "-", replacement = "\n", levels(adherence$study_name))
  
  
  p1 = ggplot(data = adherence, 
                 mapping = aes(x = factor(x = study_name, 
                                          levels = levels(study_name), 
                                          labels =  paste(levels(study_name), c("(N=13)", "(N=21)", "(N=44)"),sep = "\n")),
                               y = nr_interviews)) + 
    geom_boxplot() +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(colour = 'black'),
          strip.text = element_text(size = 12)) + 
    # facet_wrap(~"# of CAT-DI Assessments") +
    ylab("# of Assessments") 
  
  p2=ggplot(data = adherence, 
               mapping = aes(x = factor(x = study_name, 
                                        levels = levels(study_name), 
                                        labels =  paste(levels(study_name), c("(N=140)", "(N=280)", "(N=280)"),sep = "\n")), 
                             y = follow_up_length_days)) + 
    geom_boxplot() +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(colour = 'black'),
          strip.text = element_text(size = 12)) + 
    # facet_wrap(~"Follow-up (in days)") +
    ylab("Follow-up days") 
  
  
  p3=ggplot(data = adherence, 
               mapping = aes(x = factor(x = study_name, 
                                        levels = levels(study_name), 
                                        labels =  paste(levels(study_name), c("(N=7-14)", "(N=14)", "(N=7)"),sep = "\n")),
                             y = med_time_bw_interviews)) + 
    geom_boxplot() +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(colour = 'black'),
          strip.text = element_text(size = 12)) + 
    # facet_wrap(~"Days b/w Assessments") +
    ylab("Assessment Freq")   
  
  
  #%%%%%%%
  # Proportion of variance explained
  #%%%%%%%
  load("results/variance_explained.RData")
  
  p4=ggplot(data = data.table(VE_CAT_DI_prc,keep.rownames = T) %>% 
                 mutate(rn = ifelse(test = rn=="Subject", yes = "Individual", no = rn),
                        rn=factor(x = rn, levels = unique(rn))),
               aes(x=rn, y = VarExp)) + 
    geom_point(position=position_dodge(.1)) + 
    geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12)) +
    ylab("CI-DI Severity Var Exp (in %)")
  
  
  Fig1=ggdraw() +
    draw_plot(plot = p1, x = 0.01, y = .66,  width = .38, height = .34) +
    draw_plot(plot = p3, x = 0.01, y = .33,  width = .38, height = .33) +
    draw_plot(plot = p2, x = 0.01, y = .0,   width = .38, height = .33) +
    draw_plot(plot = p4, x = .4, y = .00, width = .6, height = 1) +
    draw_plot_label(label = c('A','B','C','D'),  
                    x = c(0, 0, 0, .4),  
                    y = c(1, .67, .34, 1), size = 20)
  
  ggsave(filename = 'manuscript/figures/fig01_cat_overview.png', plot = Fig1, width = 10, height = 7, dpi = 300, units = "in")  
  
  rm(p1,p2,p3,p4)
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Figure 2: Overview of digital behavioral phenotypes (features) derived from smartphone sensors
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  ids=nr_interviews %>% filter(total>=10) %>% pull(id)
  feature_ids=c(21:23,26:53,56:74)+1
  feature_ids=c(21:23,26:39, 42:53,56:74)+1
  
  # Individual-level correlation and p-values
  if(1){
  all_cor_mats = all_cor_mats_pval= data.frame(matrix(data = NA,
                                                      nrow = ncol(cat_n_features[,feature_ids, with=FALSE]),
                                                      ncol = length(ids)))
  colnames(all_cor_mats) = colnames(all_cor_mats_pval) = ids
  rownames(all_cor_mats) =  rownames(all_cor_mats_pval) = colnames(cat_n_features[,feature_ids, with=FALSE])
  
  for (id in ids){
    tmp_data = cat_n_features %>%  filter(subject_id==id) 
    tmp_data1= tmp_data$depression_severity
    
    tmp_data2 = tmp_data[,feature_ids, with=FALSE] 
    if(all(is.na(tmp_data2))) next
    tmp_data2 = tmp_data2 %>% select(names(which(!apply(is.na(tmp_data2), 2,all))))
    tmp_data2 = tmp_data2 %>% select(names(which(apply(tmp_data2, 2,var, na.rm=T)!=0)))
    
    all_corrs=cor(tmp_data2, tmp_data1, use = "pairwise.complete.obs")[,1]
    all_corrs_pvals=cor_pmat(cbind(tmp_data2,tmp_data[,"depression_severity"]))
    
    all_cor_mats[names(all_corrs), as.character(id)] = all_corrs
    all_cor_mats_pval[names(all_corrs), as.character(id)] = all_corrs_pvals[names(all_corrs),"depression_severity"]
  }
  
  all_cor_mats = all_cor_mats[,which(!apply(is.na(all_cor_mats), 2, all))]
  all_cor_mats_pval = all_cor_mats_pval[,which(!apply(is.na(all_cor_mats_pval), 2, all))]
  
  all_cor_mats_pval=apply(all_cor_mats_pval,2,p.adjust, method="BH")
  
  
  # For one individual, the number of unique locations visited during the day shows a strong negative correlation with their depression severity scores during the study
  all_cor_mats['n_locations_obs',"8779"]
  all_cor_mats_pval['n_locations_obs',"8779"]
  
  # We observed a lot of heterogeneity in the strength and direction of the correlation of these features with depression severity across individuals. 
  # For example, features related to location entropy are positively correlated with depression severity for some individuals 
  # but negatively or not correlated for others
  sort(all_cor_mats['location_entropy.total.norm',c("9437", "9455")])
  all_cor_mats_pval['location_entropy.total.norm',c("9437", "9455")]
  
  all_cor_mats_pval[all_cor_mats_pval<=.05] = ""
  all_cor_mats_pval[all_cor_mats_pval!="" | is.na(all_cor_mats_pval)] = "x"
  
  row_annot=data.frame(feature_description[feature_description$feature %in% rownames(all_cor_mats),c("feature","type")], 
                       row.names = "feature")[rownames(all_cor_mats),,drop=F]
  col_anot = cat_mh %>% 
    filter(!duplicated(study_subject_id)) %>% 
    select(subject_id, wave, treatment_group) %>% 
    group_by(subject_id) %>% 
    summarise(wave = paste(unique(wave[!is.na(wave)])[1], collapse = ", "), 
              treatment_group = paste(unique(treatment_group[!is.na(treatment_group)])[1], collapse = ", ")) %>% 
    filter(subject_id %in% colnames(all_cor_mats)) %>% 
    data.frame(row.names = "subject_id") 
  }
  
  # Population-level correlation and p-values
  if(1){
  global_corr=cor(cat_n_features[,feature_ids, with=FALSE], cat_n_features$depression_severity, use = "pairwise.complete.obs")[,1]
  
  global_corr_pvals=sapply(1:length(feature_ids), function(i) {
    formula_i=paste("depression_severity", paste("(1|subject_id)",colnames(cat_n_features[,feature_ids, with=FALSE])[i],sep = "+"),sep = "~")
    mylm=lmer(formula = formula_i,
              data = cat_n_features)
    coef(summary(mylm))[2,"Pr(>|t|)"]
  })
  names(global_corr_pvals) = colnames(cat_n_features[,feature_ids, with=FALSE])
  
  # the strongest correlation was observed for the wake up time
  global_corr[which.max(abs(global_corr))]
  format(p.adjust(p = global_corr_pvals,method = "BH")[names(global_corr[which.max(abs(global_corr))])],scientific=T)
  
  global_corr_pvals[p.adjust(p = global_corr_pvals,method = "BH")<=.05] = ""
  global_corr_pvals[global_corr_pvals!="" | is.na(global_corr_pvals)] = "x"
  }
  
  my_heatmap=pheatmap(mat = all_cor_mats, 
                      cluster_rows = T, 
                      cluster_cols = T, 
                      scale = "none", 
                      fontsize_row = 5, 
                      fontsize_col = 7, 
                      show_rownames = T,
                      show_colnames = F,
                      annotation_row = row_annot,
                      annotation_col = col_anot,
                      display_numbers = all_cor_mats_pval, 
                      number_color = "lightgrey") 
  dev.off()
  
  idn_order=c("all",colnames(all_cor_mats)[my_heatmap$tree_col$order])
  
  
  heatmap_result <- pheatmap(mat = cbind(all=global_corr[rownames(all_cor_mats)], all_cor_mats)[,idn_order], 
                             cluster_rows = T, 
                             cluster_cols = F, 
                             scale = "none", 
                             fontsize_row = 10, 
                             fontsize_col = 5, 
                             show_rownames = T,
                             show_colnames = F,
                             annotation_row = row_annot,
                             annotation_col = rbind(matrix(data = c("All", "All"),nrow = 1,dimnames = list("All", c("wave", "treatment_group"))), col_anot)[idn_order,],
                             display_numbers = cbind(all=global_corr_pvals[rownames(all_cor_mats_pval)], all_cor_mats_pval)[,idn_order],
                             number_color = "lightgrey",
                             legend = T,
                             breaks = seq(min(all_cor_mats,na.rm = T)-.1, max(all_cor_mats,na.rm = T)+.1, length.out = 21),  # Set the breaks from -1 to 1 with 11 intervals
                             color = colorRampPalette(c("blue", "white", "red"))(21),  # Custom color scale from blue to red with 21 colors
                             legend_breaks = round(seq(-max(abs(all_cor_mats),na.rm = T), max(abs(all_cor_mats),na.rm = T), length.out = 7),2),
                             legend_labels = as.character(round(seq(-max(abs(all_cor_mats),na.rm = T), max(abs(all_cor_mats),na.rm = T), length.out = 7),1))
                             
  ) 
  
  dev.off()
  
  
  #%%%%%%%
  # Identification of sleep disruption with sensor data
  #%%%%%%%
  axis_title_size=15
  
  example_ind="8252"
  y=cat_n_features_raw %>% filter(subject_id==example_ind & phone_off_duration_12_8.h>0) %>% select(phone_off_duration_12_8.h, date) %>% na.omit()
  cp=processStream(y$phone_off_duration_12_8.h, cpmType = "Mann-Whitney")
  
  ggplot_result <- cat_n_features_raw %>% 
    filter(subject_id==example_ind & phone_off_duration_12_8.h>0) %>% 
    select(date, real_cat, phone_off_duration_12_8.h, depression_severity) %>% 
    reshape2::melt(id.vars=c("date","real_cat")) %>% 
    filter(!(variable=="depression_severity" & real_cat==0)) %>%
    mutate(variable = factor(x = variable, 
                             levels = c("phone_off_duration_12_8.h","depression_severity"),
                             labels = c("Estimated hours of sleep","CAT-DI Severity"))) %>%
    ggplot(mapping = aes(x = as.Date(date), y = value)) + 
    facet_wrap(~variable, scales = "free_y", ncol = 1) + 
    geom_point() + 
    geom_vline(xintercept = as.Date(y$date[cp$changePoint]), linetype="dashed") + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=axis_title_size),
          axis.text.x = element_text(size=axis_title_size, color = "black"),
          strip.text = element_text(size=axis_title_size)) 
  
  png(file = 'manuscript/figures/fig02_features_overview.png', width = 13, height = 12,units = "in", res = 900)
  setHook("grid.newpage", function() pushViewport(viewport(x=1,y=0.9, width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")
  grid.draw(arrangeGrob(heatmap_result[[4]], ggplotGrob(ggplot_result), ncol = 1, heights = unit(c(2, 1.5), c("null", "null"))))
  setHook("grid.newpage", NULL, "replace")
  grid.text("Individuals", y=.45, x=0.4, gp=gpar(fontsize=16))
  grid.text("A", y=.99, x=0.01, gp=gpar(fontsize=20))
  grid.text("B", y=.4, x=0.01, gp=gpar(fontsize=20))
  grid.text(expression("Pearson's"~rho), y=.95, x=0.84, rot=90, gp=gpar(fontsize=12))
  dev.off()
  
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Figure 3: Illustration of latent trait inference and prediction 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  ## Figure A: Illustration of interpolation methods
  u=10442
  
  dates=idio_predictions_features_binom_AutoComp %>% filter(id==u & real_cat == "observed" & scale == "LOCF") %>% pull(date)
  
  p1 = ggplot() + 
    geom_point(data = idio_predictions_features_binom_AutoComp %>% filter(id==u & real_cat == "observed" & scale == "LOCF"),
               mapping = aes(x = date, y = observed, color=scale, size=factor(real_cat))) +
    geom_path(data = idio_predictions_features_binom_AutoComp %>% filter(id==u),
              mapping = aes(x = date, y = observed, color=scale, size=factor(real_cat), group=scale), size=.5) +
    scale_size_manual(values = c(3,1)) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(.2,.3), #c(.9,.72)
          legend.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(size=12),
          legend.text = element_text(size=12)) + 
    guides(col=guide_legend(title = NULL), #"Latent trait"
           size=guide_legend(title = NULL)) +
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    ylab("CAT-DI severity")  + 
    geom_hline(yintercept = cat_di_breaks, linetype="dotted") + #cat_di_breaks[3:4]
    ylim(c(0,100)) + 
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=90, label="severe", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  +
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=73, label="moderate", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  +
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=45, label="mild", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  +
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=25, label="normal", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  
  
  p1
  
  # Figure B
  # u=10690
  predictions_i=idio_predictions_features_binom_AutoComp %>% filter(id==u & scale %in% c("LOCF", "CS(2-4df)")) 
  
  train_set=predictions_i %>% filter(!scale %in% c("LOCF") & set=="Train" & real_cat=="observed") 
  unique_cat_train=max(2,length(unique(train_set$true_category)))
  train_set$severity_smooth = smooth.spline2(daily_data = train_set %>% 
                                               mutate(Date=date, severity=observed, real_cat = real_cat=="observed"), 
                                             imputation_type = "fixed_knots", 
                                             n_knots = unique_cat_train)  
  
  data_i=rbind(predictions_i %>% 
                 select(date, scale, real_cat, observed, set, predicted) %>%
                 mutate(trajectory="Full trajectory"),
               train_set %>% 
                 select(date, scale, real_cat, severity_smooth, set, predicted) %>%
                 rename(observed=severity_smooth) %>%
                 mutate(trajectory="Training set trajectory")) %>% 
    droplevels()
  
  
  p2=ggplot() +    
    # geom_point(data = data_i %>% 
    #              filter(scale=="LOCF" & trajectory=="Full trajectory" & real_cat=="observed"), 
    #            mapping = aes(x = date, y = observed, color=scale), 
    #            size=3, 
    #            alpha=1) + 
    geom_point(data = data_i %>% 
                 filter(scale != "LOCF" & trajectory=="Full trajectory" & real_cat=="observed") %>% 
                 select(date,scale,set,observed,predicted) %>% 
                 melt(id.vars=c("date","scale","set")) %>% 
                 filter(!(set=="Train")), #& variable == "predicted"
               mapping = aes(x = date, y = value, color=scale, shape=variable), 
               size=3, 
               alpha=1) + 
    geom_path(data = data_i %>%
                filter(!scale %in% c("LOCF")), 
              mapping = aes(x = date, y = observed, linetype=trajectory, color=scale), size=1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(.3,.26), 
          legend.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(size=12),
          legend.title = element_text(size = 15),
          legend.text = element_text(size=12)) +
    guides(color=guide_legend(title = NULL), #"Latent trait"
           linetype=guide_legend(title = NULL),
           shape= guide_legend(title = NULL)) +
    ylab(paste0("CAT-DI severity")) + 
    xlab(paste0("Date")) + 
    geom_vline(xintercept = predictions_i$date[max(which(predictions_i$set=="Train"))]) + 
    annotate(geom="text", x=as.Date("2020-06-05"), y=95,label="Training set",fontface="bold", size=5) +
    annotate(geom="text", x=as.Date("2020-10-14"), y=95,label="Test set",fontface="bold", size=5)  +
    scale_color_manual(values = scale_colors[c(1,4)], drop=F) + 
    geom_hline(yintercept = cat_di_breaks, linetype="dotted") +
    ylim(c(0,100)) + 
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=90, label="severe", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  +
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=73, label="moderate", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  +
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=45, label="mild", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  +
    geom_text(data=data.frame(date=as.Date("2020-10-05"), observed=25, label="normal", real_cat="observed"), mapping = aes(x = date, y = observed, label=label), hjust = 0)  
  
  Fig3=ggdraw() +
    draw_plot(plot = p1, x = .00, y = .00,  width = .5, height = 1) +
    draw_plot(plot = p2, x = .50, y = .00,  width = .5, height = 1) +
    draw_plot_label(label = c('A','B'),  x = c(0,0.5), y = c(1,1), size = 20)
  
  ggsave(filename = 'manuscript/figures/fig03_cat_di_imputation.png', 
         plot = Fig3, width = 10, height = 6, dpi = 300, units = "in")  
  
  
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Figure 4: Prediction performance for CAT-DI severity score across all individuals  
#%%%%%%%%%%%%%%%%  Binomial regression - Features imputed using Autocomplete
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){

  pop_lvl_metrics_binom_AutoComp = all_predictions_binom_AutoComp %>% 
    filter(set=="Test" & real_cat == "observed" & !is.na(observed) & !is.na(predicted)) %>% 
    group_by(scale, model, features) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted,use = "complete.obs"),
              LCI=cor.test(x = observed, y = predicted)$"conf.int"[1],
              UCI=cor.test(x = observed, y = predicted)$"conf.int"[2],
              rho_sq=(cor(observed, predicted,use = "complete.obs")^2),
              LCI_sq=(cor.test(x = observed, y = predicted)$"conf.int"[1]^2),
              UCI_sq=(cor.test(x = observed, y = predicted)$"conf.int"[2]^2),
              mse=mse(observed,predicted),
              mae=mae(observed,predicted),
              mape=smape(observed,predicted)) 
  
  
  d=.9
  axis.text.size=12
  strip.text_size=12
  
  p1 = ggplot(data = pop_lvl_metrics_binom_AutoComp %>% filter(features ==  "Feature-based"), 
              mapping = aes(x = scale, y=rho_sq, col=model)) + 
    geom_point(position=position_dodge(d), size=2) + 
    geom_errorbar(aes(ymin=LCI_sq, ymax=UCI_sq), position=position_dodge(d)) +
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    ylim(c(0,1)) + 
    geom_hline(yintercept = .70, linetype="dotted") + 
    ylab(expression(Prediction~Accuracy~"("~R^2~")")) 
  
  p1_rel = ggplot(data = pop_lvl_metrics_binom_AutoComp %>% 
                    group_by(scale, model) %>% 
                    summarise(features = features, 
                              rho_rel = ((rho_sq-rho_sq[features=="Intercept-only"])/rho_sq[features=="Intercept-only"]),
                              rho_rel_log2 = log2(rho_sq/rho_sq[features=="Intercept-only"])) %>% 
                    filter(features!="Intercept-only" & model!="Nomothetic") , 
                  mapping = aes(x = scale, y=rho_rel_log2, fill=model)) + #
    geom_col(position=position_dodge(d), size=2) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    ylim(c(-.23,.15)) +
    geom_hline(yintercept = 0) + 
    geom_hline(yintercept = c(0.0994,-.194), linetype="dotted") + 
    geom_text(mapping = aes(x = 2.5,y = .13, label="Feature-based model\nperforms better")) +
    geom_text(mapping = aes(x = 2.5,y = -.22, label="Feature-based model\nperforms worse")) +
    ylab(expression(atop(log[2]~Fold~Change~"in"~R^2, Over~Baseline~Model)))
  
  
  p2=ggplot(data = pop_lvl_metrics_binom_AutoComp %>% 
              filter(features ==  "Feature-based"), 
            mapping = aes(x = scale, y=mape, fill=model)) + 
    geom_col(position=position_dodge(d)) + 
    geom_text(mapping = aes(label = round(mape,2), group=model), 
              angle = 90, hjust=1, vjust=.5, color = "white", fontface = "bold",position = position_dodge(d)) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    ylim(c(0,.3)) +
    ylab("Mean abs perc error (MAPE)") 
  
  p2_rel=ggplot(data = pop_lvl_metrics_binom_AutoComp %>% 
                  group_by(scale, model) %>% summarise(features = features, 
                                                       MAPE_rel_log2 = log2(mape/mape[features=="Intercept-only"]),
                                                       MAPE_diff = ((mape-mape[features=="Intercept-only"])),
                                                       MAPE_rel = ((mape-mape[features=="Intercept-only"])/mape[features=="Intercept-only"])) %>% 
                  filter(features!="Intercept-only") %>% 
                  droplevels(), 
                mapping = aes(x = scale, y=MAPE_rel_log2, fill=model)) + #
    geom_col(position=position_dodge(d), size=2) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors ) +
    scale_fill_manual(values = scale_colors) +
    ylim(-1.7,0.4) +
    geom_hline(yintercept = 0) + 
    geom_hline(yintercept = c(.141, -1.54), linetype="dotted") + 
    ylab(expression(atop(log[2]~Fold~Change~"in"~"MAPE", Over~Baseline~Model))) + 
    geom_text(mapping = aes(x = 2.5,y = .3, label="Feature-based model\nperforms worse")) +
    geom_text(mapping = aes(x = 2.5,y = -1.65, label="Feature-based model\nperforms better")) 

  # Accuracy by day
  accuracy_by_day=merge(all_predictions_binom_AutoComp %>% filter(features == "Feature-based" & real_cat=="observed" & set == "Test"  ), #& scale == "CS(2-4df)"
                        all_predictions_binom_AutoComp %>% filter(features == "Feature-based" & real_cat=="observed" & model=="Idiographic" & set == "Train"  & scale=="CS(2-4df)" ) %>% 
                          group_by(id) %>% 
                          filter(date==max(date)) %>% select(id,date) %>% rename(last_date=date)) %>% 
    mutate(days=as.numeric(as.Date(date)-last_date),
           cat_days=cut(x = days, breaks = c(seq(0,28,7),150),include.lowest = T, labels = c(paste("week",1:4),"> week 4"))) %>% 
    group_by(cat_days,scale,model) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted, use = "complete.obs"),
              rho_sq=cor(observed, predicted, use = "complete.obs")^2,
              LCI=cor.test(x = observed, y = predicted)$"conf.int"[1]^2,
              UCI=cor.test(x = observed, y = predicted)$"conf.int"[2]^2,
              mse=mse(observed,predicted),
              mae=mae(observed,predicted)) %>% 
    ungroup()
  
  d=0.9
  p3=accuracy_by_day %>% 
    filter(model=="Idiographic") %>% 
    ggplot(mapping = aes(x = cat_days, y = rho_sq, color=model, shape=scale)) + 
    geom_point(position=position_dodge(d), size=2) + #aes(size=n),
    geom_errorbar(aes(ymin = LCI, ymax = UCI),position=position_dodge(d)) + 
    theme_bw() + 
    ylab(expression(Prediction~Accuracy~"("~R^2~")")) + 
    xlab("Weeks in test set") + 
    theme(legend.position = c(.9,.9), 
          legend.text = element_text(size=12,color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size,color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_size_continuous(breaks = seq(from=0,to=250,by=50), range = c(2,5)) +
    labs(shape = "", color = "", size = "# CAT-DI assessments") + 
    guides(color = "none")  +
    ylim(c(0,1)) + 
    geom_hline(yintercept = .70, linetype="dotted") + 
    scale_color_manual(values = scale_colors, drop=F) 
  
  # extract a legend that is laid out horizontally
  legend_b <- get_legend( p3 +  guides(color = guide_legend(nrow = 1,override.aes = list(size = 1))) +
                            theme(legend.position = "top", legend.box = "horizontal"))
  
  Fig4=ggdraw() +
    
    draw_plot(plot = p2, x = .00, y = .50,  width = .35, height = .5) +
    draw_plot(plot = p1, x = .00, y = .00,  width = .35, height = .5) +
    
    draw_plot(plot = p3 + theme(legend.position = "none"), x = .35, y = .00,  width = .3, height = 1) +
    
    draw_plot(plot = p2_rel, x = .65, y = .50,  width = .35, height = .5) +
    draw_plot(plot = p1_rel, x = .65, y = .00,  width = .35, height = .5) +
    
    
    draw_plot_label(label = c('A','B','C', 'D', 'E'),  
                    x = c(0, 0, 0.35, 0.65, 0.65), 
                    y = c(1,.5,1,1,0.5), size = 20)
  
  
  # add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
  Fig4_withleg=plot_grid(legend_b, Fig4, ncol = 1, rel_heights = c(.03, 1))
  ggsave(plot = Fig4_withleg, filename = paste0('manuscript/figures/fig04_group_level_pred_performance.png'), width = 10, height = 10)
  
  rm(p1,p1_rel,p2,p2_rel,p3)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Figure 5: Prediction performance for CAT-DI severity score within each individual
#%%%%%%%%%%%%%%%%  Binomial regression - Features imputed using Autocomplete
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  # Individuals with at least five assessments (observed or imputed) in test set
  ids_n_geq_5=all_predictions_binom_AutoComp %>% 
    group_by(id, set, scale, model, features) %>% 
    summarise(n=sum(!is.na(predicted))) %>% 
    ungroup() %>% 
    group_by(id) %>% 
    summarise(min_n=min(n)) %>% 
    filter(min_n>=5) %>% 
    pull(id) 
  
  # Individuals with no variation in their assessments (observed or imputed) in test set
  id_with_novar_in_test=all_predictions_binom_AutoComp %>%
    filter(id %in% ids_n_geq_5 & set=="Test") %>% 
    group_by(scale, model, features, id) %>%
    summarise(var_obs=var(observed)) %>%
    filter(var_obs==0)%>%
    pull(id) %>%
    unique()  
  
  # Individual-level metric of prediction accuracy 
  tmp_x = all_predictions_binom_AutoComp %>% 
    filter(id %in% ids_n_geq_5 & set=="Test") %>% 
    filter(!id %in% id_with_novar_in_test) %>%
    group_by(scale, model, features, id) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted, use = "complete.obs"),
              P_rho=cor.test(x = observed, y = predicted, alternative = "greater")$"p.value",
              LCI=cor.test(x = observed, y = predicted)$"conf.int"[1],
              UCI=cor.test(x = observed, y = predicted)$"conf.int"[2],
              rho_sq=cor(observed, predicted,use = "complete.obs")^2,
              LCI_sq=cor.test(x = observed, y = predicted)$"conf.int"[1]^2,
              UCI_sq=cor.test(x = observed, y = predicted)$"conf.int"[2]^2,
              mse=mse(observed,predicted),
              mae=mae(observed,predicted),
              mape=smape(observed,predicted)) %>% 
    ungroup() %>% 
    mutate(LCL=CI.Rsq(rsq = rho_sq, n = n, k = 1, level = 0.95)$"LCL",
           UCL=CI.Rsq(rsq = rho_sq, n = n, k = 1, level = 0.95)$"UCL")  %>% 
    group_by(features, scale, model) %>% #
    mutate(BHP=p.adjust(p = P_rho, method = "BH")) %>% 
    ungroup() 
  
  fig4reviewer = ggplot(data = pivot_wider(tmp_x %>% 
                              select(id, scale, model, features, mae) , id_cols = c(id, scale, model), names_from = features, values_from = mae)  %>% 
           filter(model=="Idiographic"),
         mapping = aes(x = `Feature-based`, y = `Intercept-only`)) + 
    facet_grid(~scale) + 
    geom_point() + 
    geom_abline() + 
    theme_bw()  +
    ylim(c(0,80)) + xlim(c(0,80)) + 
    xlab("MAE of feature-based model") + 
    ylab("MAE of baseline model")
  
  ggsave(filename = 'manuscript/figures/fig4reviewer.png',plot = fig4reviewer, width = 10, height = 2.5)
  
  # Individual-level metric of prediction accuracy 
  tmp_y = all_predictions_binom_AutoComp %>% 
    select(id, scale, observed, set) %>% 
    group_by(id,scale) %>% 
    summarize(abs_med_diff=abs(median(observed[set=="Train"],na.rm = T)-median(observed[set=="Test"],na.rm = T)))
  length(unique(tmp_y$id))
  
  ind_lvl_metrics_binom_AutoComp = merge(x = merge(x = tmp_x, y = demographics %>% select(subject_id, treatment_wave), by.x = "id", by.y =  "subject_id", all.x = T,all.y = F), 
                                         y = tmp_y, by.x = c("id","scale"), by.y = c("id","scale"), all.x = T,all.y = F) %>%
    arrange(id, scale,model, features)
  
  rm(tmp_x,tmp_y)
  
  d=.9
  axis.text.size=12
  strip.text_size = 12 
  
  median_mape = ind_lvl_metrics_binom_AutoComp %>% 
    filter(features ==  "Feature-based" & scale == "CS(2-4df)" & model == "Idiographic") %>% 
    summarise(med=median(mape)) %>% pull(med)
  
  p1=ggplot(data = ind_lvl_metrics_binom_AutoComp %>% 
              filter(features ==  "Feature-based"), 
            mapping = aes(x = scale, y=mape, fill=model)) + 
    geom_boxplot(position=position_dodge(d)) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    scale_y_continuous(limits = c(0, 1.3), breaks = seq(0,1.3,.2)) +
    ylab("Mean abs perc error (MAPE)") +
    geom_hline(yintercept = median_mape, linetype="dashed") 
  p1
  
  p1_rel=ggplot(data = ind_lvl_metrics_binom_AutoComp %>% 
                  group_by(id, scale, model) %>% 
                  summarise(features = features, 
                            abs_med_diff = abs_med_diff, 
                            MAPE_diff = (mape - mape[features=="Intercept-only"]),
                            MAPE_log2FC = log2(mape/mape[features=="Intercept-only"]),
                            MAPE_rel = (mape - mape[features=="Intercept-only"])/mape[features=="Intercept-only"]) %>% 
                  filter(features!="Intercept-only") , 
                mapping = aes(x = scale, y=MAPE_log2FC, fill=model)) + #abs_med_diff > 10
    geom_boxplot(position=position_dodge(d)) + 
    theme_bw() + 
    theme( legend.position = "none",
           legend.title = element_blank(),
           legend.text = element_text(size=axis.text.size, color="black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.title = element_text(size=15),
           axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
           axis.text = element_text(size=axis.text.size, color="black"),
           axis.title.x = element_blank(),
           strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors ) +
    scale_fill_manual(values = scale_colors) +
    ylab(expression(atop(log[2]~~Fold~Change~"in"~"MAPE", Over~Baseline~Model))) +
    xlab(expression(abs(Median~CAT-DI[Training]-Median~CAT-DI[Test])>10)) +
    # scale_y_continuous(limits = c(-6.5,6.5)) + #, breaks = seq(0,1,.1)
    geom_hline(yintercept = 0)  + 
    geom_hline(yintercept = -1.5, linetype='dotted')  
  p1_rel
  
  p2= ind_lvl_metrics_binom_AutoComp  %>% 
    filter(features == "Feature-based") %>% 
    group_by(scale, model) %>% 
    summarise(n=sum(BHP<=.05), N=n()) %>% 
    mutate(pi = n/N) %>% 
    ggplot(mapping = aes(x = scale, y = pi, fill=model)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right", # c(.3,.85),
          legend.title = element_blank(),
          axis.title.y = element_text(size=14),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=12,color="black"),
          axis.text.x = element_text(size=12,color="black", angle = 90, vjust = .5),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) + 
    geom_col(position = position_dodge(.9)) + 
    geom_text(mapping = aes(label = n, group=model), angle = 90, hjust=1, vjust=.5, color = "white", fontface = "bold",position = position_dodge(.9)) + 
    ylab("% Sign. Predicted Individuals") 
  p2

  
  d = 0
  p3 = ind_lvl_metrics_binom_AutoComp %>% 
    mutate(model_id_scale=paste(model,id,scale,sep = "-")) %>% 
    filter(BHP <=.05 & features == "Feature-based") %>%  
    ggplot(mapping = aes(x = as.factor(id), col=model)) + 
    facet_grid(scale~.) +
    geom_point(aes(y = rho_sq),position=position_dodge(d)) +
    geom_errorbar(aes(ymin=LCL, ymax=UCL),position=position_dodge(d)) +
    theme_bw() + 
    theme(#panel.grid.major = element_blank(),
      legend.position = "none",
      legend.title = element_blank(),
      axis.title.y = element_text(size=15),
      axis.title.x = element_text(size=15),
      axis.text.y = element_text(size=12,color="black"),
      axis.text.x = element_blank(), #element_text(size=12,color="black", angle = 90, vjust = .5),
      strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) + 
    ylab(expression(Prediction~Accuracy~"("~R^2~")"))  +
    geom_hline(yintercept = .70, linetype = 'dashed') + 
    xlab("Individuals")
  p3
  
  # extract a legend that is laid out horizontally
  legend_b <- get_legend( p2 +  guides(color = guide_legend(nrow = 1,override.aes = list(size = 5))) +
                            theme(legend.position = "top", legend.box = "horizontal"))
  
  Fig5=ggdraw() +
    draw_plot(plot = p1,     x = .00, y = .65, width = .33, height = .35) +
    draw_plot(plot = p2 + theme(legend.position = "none"),     x = .33, y = .65, width = .33, height = .35) +
    draw_plot(plot = p1_rel, x = .66, y = .65, width = .34, height = .35) +
    draw_plot(plot = p3,     x = .00, y = .00, width = 1,   height = .65) +
    draw_plot_label(label = c('A','B','D', 'C'),  
                    x = c(0, .33, .66, 0), 
                    y = c(1,1, 1, .65), size = 20)
  
  Fig5_withleg=plot_grid(legend_b, Fig5, ncol = 1, rel_heights = c(.03, 1))
  
  
  ggsave(plot = Fig5_withleg, filename = paste0('manuscript/figures/fig05_ind_level_pred_performance.png'), width = 12, height = 12)
  
  rm(p1,p2, p1_rel,p3)
  
  
  # The number of significantly predicted individuals across all latent traits for each model was:
  ind_lvl_metrics_binom_AutoComp %>% 
    filter(BHP<=.05 & features == "Feature-based") %>% 
    group_by(model) %>% 
    filter(!duplicated(id)) %>% 
    summarize(n=n()/143)
  
  # The median R2 value across significantly predicted individuals and all latent traits for each models was:
  ind_lvl_metrics_binom_AutoComp %>% 
    filter(BHP<=.05 & features == "Feature-based") %>% 
    group_by(model) %>% 
    filter(!duplicated(id)) %>% 
    summarize(med=median(rho_sq)) 
  
  # The number of significantly predicted individuals across all latent traits for each models was:
  ind_lvl_metrics_binom_AutoComp %>% 
    filter(BHP<=.05 & rho_sq>=.7 & features == "Feature-based") %>% 
    group_by(model) %>% 
    filter(!duplicated(id)) %>% 
    summarize(n=n()) #/143
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Figure 6: Feature importance from idiographic models
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  # Keep data only for significantly predicted individuals 
  # Keep scale with largest Rsq (this might be driven by study day)
  uid_sign = all_predictions_binom_AutoComp %>% 
    filter(model == "Idiographic" & features == "Feature-based" & set=="Test" & id %in% ids_n_geq_5) %>% 
    group_by(id,scale) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted, use = "complete.obs"),
              P_rho=cor.test(x = observed, y = predicted, alternative = "greater")$"p.value"
    ) %>% 
    ungroup() %>% 
    group_by(scale) %>% 
    mutate(BHP=p.adjust(p = P_rho, method = "BH")) %>% 
    filter(BHP<=.05)  %>%
    ungroup() %>% 
    group_by(id) %>% 
    summarise(scale = scale[which(rho==max(rho))]) %>%
    mutate(id_scale=paste(id,scale,sep = "-")) %>% 
    pull(id_scale) 
  
  feature_importance_sign_ind = feature_importance %>%
    mutate(uid = paste(id,scale,sep = "-")) %>% 
    filter(uid %in% uid_sign) %>% 
    filter(feature!="(Intercept)") %>% 
    droplevels() %>% 
    arrange(desc(abs(betas)))
  
  # Keep only features significant in more than two individuals - exclude study day 
  sig_features = feature_importance_sign_ind %>% 
    filter(abs(betas)>=0.05) %>% 
    select(id, feature,betas) %>%  
    group_by(feature) %>%
    summarise(n=n()) %>% 
    arrange(desc(n)) %>% 
    filter(n>=1)  %>% 
    droplevels() %>%
    pull(feature) 
  
  feature_importance_sign_ind %<>% filter(feature %in% sig_features & feature!="study_day") 
  
  #  Keep only individuals with at least one feature with OR > exp(abs(0.03)) 
  tmp_ids2keep=feature_importance_sign_ind %>% group_by(id) %>% summarise(max_beta=max(abs(betas))) %>% filter(max_beta>=0.03) %>% pull(id)
  feature_importance_sign_ind %<>% filter(id %in% tmp_ids2keep) %>% mutate(betas=exp(-betas))
  rm(tmp_ids2keep)
  
  # Add feature type information
  feature_importance_sign_ind$type=
    data.frame(feature_description,row.names = 1)[feature_importance_sign_ind$feature,"type"] 
  feature_importance_sign_ind %<>% arrange(type)
  
  #  Convert feature coefficient matrix from long to wide
  feature_importance_sign_ind_wide=as.matrix(data.frame(reshape(feature_importance_sign_ind %>% select(id, feature,betas), idvar = "feature", timevar = "id", direction = "wide"),row.names = 1))
  feature_importance_sign_ind_wide[is.na(feature_importance_sign_ind_wide)]=1
  colnames(feature_importance_sign_ind_wide) = gsub(pattern = "betas.",replacement = "",x = colnames(feature_importance_sign_ind_wide))
  
  # Order by type
  VarOrder = feature_description[feature_description$feature %in% rownames(feature_importance_sign_ind_wide),"feature"]
  feature_importance_sign_ind_wide=feature_importance_sign_ind_wide[VarOrder,]
  
  breaks = seq(from=0.81,to=1.20, by=.01) 
  
  png(file = 'manuscript/figures/fig06_feature_importance.png', width = 10, height = 12,units = "in", res = 900)
  
  setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1, width=1, height=1, name="vp", just=c("right","top"))), action="prepend")
  
  pheatmap(mat = feature_importance_sign_ind_wide, 
           cluster_rows = T, cluster_cols = T, 
           scale = "none", 
           breaks=breaks, 
           legend_breaks = seq(from=0.8,to=1.2,by=.05),
           legend_labels = as.character(seq(from=0.8,to=1.2,by=.05)),
           color=colorRampPalette(c("navy", "white", "red"))(length(breaks)),
           fontsize_row = 8, fontsize_col = 5, show_colnames = F, 
           annotation_row = data.frame(feature_description[feature_description$feature %in% rownames(feature_importance_sign_ind_wide),c("feature","type")], row.names = "feature"),
           annotation_col = cat_mh %>% filter(!duplicated(study_subject_id)) %>% select(subject_id, wave, treatment_group) %>% 
             group_by(subject_id) %>% 
             summarise(wave = paste(unique(wave[!is.na(wave)])[1], collapse = ", "),
                       treatment_group = paste(unique(treatment_group[!is.na(treatment_group)])[1], collapse = ", ")) %>% 
             filter(subject_id %in% colnames(feature_importance_sign_ind_wide)) %>%
             data.frame(row.names = "subject_id") 
           
  ) 
  
  setHook("grid.newpage", NULL, "replace")
  grid.text("Individuals", x=0.4, y=0.03, gp=gpar(fontsize=16)) 
  grid.text("Odds ratio", y=.83, x=0.79, rot=90, gp=gpar(fontsize=12,fontface = "bold"))
  dev.off()
  
  data.frame(sort(apply(abs(feature_importance_sign_ind_wide),1,max),decreasing = T)[1:20])
  data.frame(sort(apply(abs(feature_importance_sign_ind_wide),1,min),decreasing = F)[1:10])
  
  contact=feature_importance_sign_ind_wide[grep(pattern = 'contact_attempts_out.pct.var30', x = rownames(feature_importance_sign_ind_wide)),]
  contact[contact>=1.05|contact<=.95]

  wakeup=feature_importance_sign_ind_wide[grep(pattern = 'time_first_interaction.hrsam.rmean30', x = rownames(feature_importance_sign_ind_wide)),]
  wakeup[wakeup>=1.05|wakeup<=.95]

  bedtime=feature_importance_sign_ind_wide[grep(pattern = 'last_observation.hrsam.rmean30', x = rownames(feature_importance_sign_ind_wide)),]
  bedtime[bedtime>=1.05|bedtime<=.95]

  timehome=feature_importance_sign_ind_wide[grep(pattern = 'time_spent_at_home.total.pct.var30', x = rownames(feature_importance_sign_ind_wide)),]
  timehome[timehome>=1.05|timehome<=.95]
  
  min(contact[contact>=1.05|contact<=.95], wakeup[wakeup>=1.05|wakeup<=.95], bedtime[bedtime>=1.05|bedtime<=.95], timehome[timehome>=1.05|timehome<=.95])
  max(contact[contact>=1.05|contact<=.95], wakeup[wakeup>=1.05|wakeup<=.95], bedtime[bedtime>=1.05|bedtime<=.95], timehome[timehome>=1.05|timehome<=.95])
  
  data.table(t(apply(feature_importance_sign_ind_wide,1,range)), keep.rownames = T) %>% mutate(diff = V2-V1) %>% arrange(desc(abs(diff))) %>% head(n=10)
  location_entropy=feature_importance_sign_ind_wide[grep(pattern = 'location_entropy.evening.norm.var30', x = rownames(feature_importance_sign_ind_wide)),]
  min(location_entropy)
  max(location_entropy)
  
  phone_off=feature_importance_sign_ind_wide[grep(pattern = 'longest_phone_off_duration.h.var30', x = rownames(feature_importance_sign_ind_wide)),]
  min(phone_off)
  max(phone_off)
  
  
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Figure 7: Factors associated with prediction performance 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  metrics_binom_AutoComp=all_predictions_binom_AutoComp %>% 
    filter((real_cat=="observed" & model == "Idiographic" &  # 
              id %in% (nr_interviews %>% filter(test>=5) %>% pull(id))))  %>% 
    group_by(id,features,model,scale) %>% 
    summarise(
      n_train=sum(set!="Test"),
      n_test=sum(set=="Test"),
      abs_med_diff=abs(median(observed[set=="Train"])-median(observed[set=="Test"])),
      entropy=entropy::entropy(y = observed),
      entropy_test=entropy::entropy(y = observed[set=="Test"]),
      entropy_train=entropy::entropy(y = observed[set=="Train"]),
      entropy_diff=abs(entropy_train-entropy_test),
      n_states=length(unique(true_category)),
      n_states_train=length(unique(true_category[set=="Train"])),
      n_states_test=length(unique(true_category[set=="Test"])),
      rho=cor(observed, predicted, use = "complete.obs"),
      rho_sq=cor(observed, predicted, use = "complete.obs")^2,
      mse=mse(observed,predicted),
      mae=mae(observed,predicted),
      mape=ifelse(test = is.infinite(100*mape(observed,predicted)),yes = NA, no = 100*mape(observed,predicted))) %>% 
    filter(n_states!=5)  %>% 
    droplevels() %>% 
    arrange(id, scale,features, model) 
  
  tmp_y = metrics_binom_AutoComp %>% 
    group_by(id, model, scale) %>% 
    summarize(features=features,
              mae_diff = mae - mae[features=='Intercept-only'],
              mape_diff = mape - mape[features=='Intercept-only']) 
  
  metrics_binom_AutoComp=merge(x = metrics_binom_AutoComp %>% filter(features == "Feature-based"), y = tmp_y %>% filter(features == "Feature-based")) %>% select(-features)
  
  metrics_binom_AutoComp=merge(x = metrics_binom_AutoComp, y = demographics[,c("subject_id","age","sex","wave", "treatment_group")] %>% rename(id=subject_id), by = "id",all.x = T)
  metrics_binom_AutoComp$sex = as.numeric(metrics_binom_AutoComp$sex=="Female") 
  metrics_binom_AutoComp$wave = as.numeric(metrics_binom_AutoComp$wave=="Wave 1") 
  metrics_binom_AutoComp$treatment_group = as.numeric(metrics_binom_AutoComp$treatment_group=="online support") 
  
  cor_mat_LOCF=cor(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="LOCF",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")], use = "pairwise.complete.obs", method = "spearman")[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  p_mat_LOCF=cor_pmat(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="LOCF",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")])[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  
  cor_mat_cv=cor(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="CS(cv)",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")], use = "pairwise.complete.obs", method = "spearman")[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  p_mat_cv=cor_pmat(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="CS(cv)",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")])[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  
  cor_mat_cs4=cor(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="CS(4df)",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")], use = "pairwise.complete.obs", method = "spearman")[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  p_mat_cs4=cor_pmat(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="CS(4df)",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")])[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  
  cor_mat_cs24=cor(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="CS(2-4df)",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")], use = "pairwise.complete.obs", method = "spearman")[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  p_mat_cs24=cor_pmat(metrics_binom_AutoComp[metrics_binom_AutoComp$scale=="CS(2-4df)",c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group","rho_sq","mape", "mape_diff")])[c("rho_sq","mape", "mape_diff"),c("abs_med_diff","n_train","n_test", "n_states","n_states_train","n_states_test","age","sex","wave", "treatment_group")]
  
  cor_mat = rbind(
    melt(cor_mat_LOCF) %>% mutate(scale="LOCF"),
    melt(cor_mat_cv) %>% mutate(scale="CS(cv)"),
    melt(cor_mat_cs4) %>% mutate(scale="CS(4df)"),
    melt(cor_mat_cs24) %>% mutate(scale="CS(2-4df)")) %>% 
    mutate(scale = factor(x = scale,levels = scale_labels))

  p_mat = rbind(
    melt(p_mat_LOCF) %>% mutate(scale="LOCF"),
    melt(p_mat_cv) %>% mutate(scale="CS(cv)"),
    melt(p_mat_cs4) %>% mutate(scale="CS(4df)"),
    melt(p_mat_cs24) %>% mutate(scale="CS(2-4df)")) %>% 
    mutate(scale = factor(x = scale,levels = scale_labels)) %>% 
    mutate(label=ifelse(test = value<=.05,yes = " ",no = " X "))
  
  p1 = ggplot(data = cor_mat, mapping = aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = 'grey') +
    geom_text(mapping = aes(label = round(value,2)), size = 3) + 
    geom_text(data = p_mat, mapping = aes(label = label), size = 5) + 
    facet_grid(~factor(scale)) + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Corr") + 
    scale_y_discrete(labels=c(expression(abs(median(CAT-DI[training])-median(CAT-DI[test]))),
                              "# CAT-DI assessment in train set","# CAT-DI assessment in test set",
                              "# unique CAT-DI categories total", "# unique CAT-DI categories in training set",
                              "# unique CAT-DI categories in test set",
                              "Age","Sex","Wave","Treatment group")) +
    scale_x_discrete(labels=c(expression(R[features]^2),expression(MAPE[features]),expression(MAPE[features]-MAPE[baseline]))) +
    theme_bw() + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle=45, vjust=1, hjust=1),
          axis.text = element_text(color = "black", size=12),
          strip.text = element_text(size = 20)) + 
    coord_fixed() 
  
  
  ggsave(filename = 'manuscript/figures/fig07_factors_affecting_prediction.png', plot = p1, width = 10, height = 7, dpi = 300, units = "in")  
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Sup Figure 1: Demographics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  p0 = 
    ggplot(data = demographics, mapping = aes(fill=treatment_wave)) + 
    theme_bw() + 
    theme(
      legend.position = "none",
      axis.title = element_text(size=15),
      axis.text.x = element_text(size=12, color = "black"))
  
  
  
  p1 = p0 + geom_histogram(mapping = aes(x = age),bins = 30) +  xlab("Age") 
  
  p2 = p0 + geom_histogram(aes(x = bmi), bins = 30) + xlab("BMI")  
  
  p3 = p0 + geom_bar(aes(x = sex), position = "dodge") +  xlab("Sex") + theme(axis.text.x = element_text(angle=30, hjust = 1))
  
  p4 = p0 + geom_bar(aes(x = factor(gender, levels = c("Female", "Male", "Transgender", "Other"))), position = "dodge") + xlab("Gender")  + 
    theme(axis.text.x = element_text(angle=30,hjust = 1))
  
  p5 =  p0 + geom_bar(aes(x = factor(race, levels = c("AI or AN alone",
                                                      "Asian alone", 
                                                      "Black/AA alone",  
                                                      "White alone", 
                                                      "Some other race",  
                                                      "Two or more races", 
                                                      "Prefer not to answer", 
                                                      "Don't know", 
                                                      "Did not answer"
  ))), position = "dodge") + 
    xlab("Race") + 
    theme(axis.text.x = element_text(angle=40, hjust = 1))
  
  p6 = p0 + geom_bar(aes(x = factor(hispanic, levels = c("Yes", "No", "Prefer not to answer", "Don't know"))), position = "dodge") + xlab("Hispanic?") + 
    theme(axis.text.x = element_text(angle=30, hjust = 1))
  
  FigS1 = grid.arrange(p1,p3,p4,p2,p5,p6, nrow=2)
  
  ggsave(filename = 'manuscript/figures/figs01_demographic_info.png', plot = FigS1, width = 15, height = 10, dpi = 300, units = "in")  
  
  dev.off()
  rm(p0, p1,p2,p3,p4, p5, p6)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Sup Figure 2: CAT-DI administration schedule and compliance 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  p1=ggplot(data = study_design_long %>% filter(value!="Baseline"),
            mapping = aes(x=value, y = variable)) + 
    geom_tile(colour = "black",fill="grey") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, colour = "black", vjust = .5, size = 12),
          axis.text.y = element_text(colour = "black", size = 12),
          axis.title = element_blank(),
          panel.grid.major = element_blank()) + 
    geom_text(data = study_design_long %>% filter(grepl(pattern = "IP", x = Event)),
              mapping = aes(x = value,label = "*"),
              size=10)
  
  # Compliance with CAT-DI assessment schedule   
  study_days = c(seq(1,40*7,7))
  study_weeks = 1:40
  
  start_end_all_participants=cat_mh %>% 
    group_by(study_subject_id, wave, treatment_group, treatment_wave) %>% 
    summarise(subject_id=unique(subject_id),
              min=min(days), 
              max=max(days)) %>%
    arrange(subject_id)
  
  still_in_study_all_participants=matrix(data = 0, nrow = nrow(start_end_all_participants), ncol = length(study_days), dimnames = list(NULL,study_days))
  for(i in 1:nrow(still_in_study_all_participants)){
    still_in_study_all_participants[i,] = as.numeric(colnames(still_in_study_all_participants)) <= start_end_all_participants[i,]$max
  }
  
  start_end_all_participants = data.frame(start_end_all_participants,still_in_study_all_participants, check.names = F) %>% 
    melt(id.vars=c("subject_id", "study_subject_id", "wave", "treatment_group", "treatment_wave", 'min', "max")) %>%
    rename(study_day = variable, in_study= value)
  
  attrition_all_participants = merge(x = start_end_all_participants %>% 
                                       group_by(wave, treatment_group, study_day) %>% 
                                       summarise(nr_in_study=sum(in_study)), 
                                     y=start_end_all_participants %>% 
                                       filter(study_day==1) %>%
                                       group_by(wave, treatment_group) %>% 
                                       summarise(n=n())) %>% 
    mutate(pct_in_study = nr_in_study/n,
           study_day = factor(x = study_day, levels = study_days, labels = study_weeks)) %>% 
    filter((as.numeric(study_day)<=20 & wave=="Wave 1")|(as.numeric(study_day)<=40 & wave=="Wave 2"))
  
  
  p2 = ggplot(data = attrition_all_participants,
              mapping = aes(x = as.numeric(study_day), y = pct_in_study)) + 
    geom_point() + 
    geom_line() +
    facet_wrap(~wave + treatment_group, scales = "free_x", nrow = 1) + 
    theme_bw() +
    theme(legend.position = "top",
          legend.text = element_text(size=15),
          strip.text = element_text(size=15),
          axis.title = element_text(size=15)) + 
    xlab("Study period [in weeks]") + 
    ylab("% individuals remaining in the study") + 
    guides(color = guide_legend(title = NULL)) + 
    geom_smooth(method = 'lm')  + 
    coord_cartesian(ylim = c(0,1))
  
  
  FigS2=ggdraw() +
    draw_plot(plot = p1,    x = .0, y = .6,  width = 1, height = .4) +
    draw_plot(plot = p2, x = .0, y = 0,  width = 1, height = .6)  + 
    draw_plot_label(label = c('A','B'), x = c(0, 0), y = c(1,.6), size = 20)
  
  
  ggsave(filename = 'manuscript/figures/figs02_cat_mh_administration_schedule_and_attrition.png', plot = FigS2, width = 8, height = 7, dpi = 300, units = "in")  
  
  # Does participant adherence to CAT-DI assessments vary by wave, treatment, sex and age
  if(1){
    prc_completion_with_demo=merge(x = start_end_all_participants %>% 
                                     filter(study_day == 1) %>% 
                                     select(subject_id, wave,treatment_group,max) %>%
                                     rename(the_max = max) %>%
                                     mutate(
                                       prc = ifelse(test = wave == "Wave 1",yes = the_max/134,no = the_max/274) 
                                     ) %>%
                                     mutate(prc = ifelse(test = prc>1,yes = 1, no = prc)), 
                                   y = demographics[,c("subject_id","age","sex")],by = "subject_id",all.x = T)
    
    
    # Does participant adherence to CAT-DI assessments vary by 
    # wave
    anova(glm(formula = cbind(prc*100, 100-prc*100) ~ wave, family = 'binomial',data = prc_completion_with_demo),test = "LRT")
    # treatment group in wave 2
    anova(glm(formula = cbind(prc*100, 100-prc*100) ~ treatment_group, family = 'binomial',data = prc_completion_with_demo %>% filter(wave=="Wave 2")),test = "LRT")
    # sex overall 
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(!is.na(sex)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + sex, family = 'binomial', data = prc_completion_with_demo)
    summary(myglm1)
    exp(myglm1$coef)
    # sex in wave 1
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 1" & !is.na(sex)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + sex , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 1"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # sex in wave 2
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & !is.na(sex)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + sex , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # sex in wave 2 - online support
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group != "clinical care"& !is.na(sex)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + sex , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group != "clinical care"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # sex in wave 2 - clinical care
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group == "clinical care"& !is.na(sex)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + sex , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group == "clinical care"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    
    
    # age overall 
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(!is.na(age)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + age, family = 'binomial', data = prc_completion_with_demo)
    summary(myglm1)
    exp(myglm1$coef)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # age in wave 1
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 1" & !is.na(age)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + age , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 1"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # age in wave 2
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & !is.na(age)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + age , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # age in wave 2 - online support
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group != "clinical care"& !is.na(age)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + age , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group != "clinical care"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    # age in wave 2 - clinical care
    myglm0=glm(formula = cbind(prc*100, 100-prc*100) ~ 1, family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group == "clinical care"& !is.na(age)))
    myglm1 = glm(formula = cbind(prc*100, 100-prc*100) ~ 1 + age , family = 'binomial', data = prc_completion_with_demo %>% filter(wave=="Wave 2" & treatment_group == "clinical care"))
    exp(myglm1$coef)
    summary(myglm1)
    format(anova(myglm0, myglm1, test = "LRT")[2,5], digits=2, scientific=T)
    
    
  }
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Sup Figure 3: Treatment effect
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  FigS3=ggplot(data = obs_data %>% filter((days<=280 & wave=="Wave 2") | (days<=140 & wave=="Wave 1")), 
               mapping = aes(x = days, y = depression_severity)) + 
    facet_wrap(~treatment_wave, scales = "free_x") + 
    geom_point(alpha=.2, color="lightgrey") + 
    geom_smooth(method = 'gam') + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.background = element_blank(),
          legend.title = element_text(size=15),
          legend.text = element_text(size=15),
          axis.title = element_text(size=15),
          axis.text  = element_text(size=12, colour = "black"),
          strip.text = element_text(size=15)) +  
    scale_color_brewer() + 
    ylab("CAT-DI score") + xlab("Study day") + 
    guides(col=guide_legend(title = "Treatment group", override.aes = list(size=3))) + 
    geom_hline(yintercept = cat_di_breaks) + 
    geom_text(data=data.frame(days=0, depression_severity=90, label="severe", study_name="Tier 1"), mapping = aes(x = days, y = depression_severity, label=label), hjust = 0)  +
    geom_text(data=data.frame(days=0, depression_severity=70, label="moderate", study_name="Tier 1"), mapping = aes(x = days, y = depression_severity, label=label), hjust = 0) +  
    geom_text(data=data.frame(days=0, depression_severity=55, label="mild", study_name="Tier 1"), mapping = aes(x = days, y = depression_severity, label=label), hjust = 0)  + 
    geom_text(data=data.frame(days=0, depression_severity=25, label="normal", study_name="Tier 1"), mapping = aes(x = days, y = depression_severity, label=label), hjust = 0)  
  
  
  ggsave(filename = 'manuscript/figures/figs03_treatment_effect.png', 
         plot = FigS3, width = 10, height = 5, dpi = 300, units = "in")  
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Sup Figure 4: Missing feature data summary
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  FigS4=missing_info %>% 
    filter(timeframe=="present-only")  %>% 
    ggplot(aes(x = as.character(iid),y = variable, fill=value/100)) + 
    geom_tile() + 
    # facet_grid(type~., scales="free_y", space='free') + 
    ggforce::facet_col(vars(type), scales = 'free', space = 'free') +
    theme(axis.text = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size=15),
          strip.text = element_text(size=15)) + 
    scale_fill_gradient(low = "white", high = "red",limits=c(0,1)) +
    ylab("Features") + xlab("Individuals") + 
    guides(fill=guide_legend(title = "Missing data (%)"))
  
  
  ggsave(filename = 'manuscript/figures/figs04_missing_data.png',plot = FigS4, width = 8.5, height = 9, dpi = 300, units = "in")
  
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Sup Figure 6: Prediction performance for CAT-DI severity score across all individuals  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  all_predictions = rbind(
    idio_predictions_features_binom_AutoComp,
    idio_predictions_baseline_binom_AutoComp,
    nom_predictions_features_binom_AutoComp ,
    nom_predictions_baseline_binom_AutoComp ,
    nommod_predictions_features_binom_AutoComp,
    nommod_predictions_baseline_binom_AutoComp,
    
    idio_predictions_features_binom_softImpute,
    idio_predictions_baseline_binom_softImpute,
    nom_predictions_features_binom_softImpute,
    nom_predictions_baseline_binom_softImpute,
    nommod_predictions_features_binom_softImpute,
    nommod_predictions_baseline_binom_softImpute,
  
    idio_predictions_features_norm_AutoComp,
    idio_predictions_baseline_norm_AutoComp,
    nom_predictions_features_norm_AutoComp,
    nom_predictions_baseline_norm_AutoComp,
    nommod_predictions_features_norm_AutoComp,
    nommod_predictions_baseline_norm_AutoComp,
  
    idio_predictions_features_norm_softImpute,
    idio_predictions_baseline_norm_softImpute,
    nom_predictions_features_norm_softImpute,
    nom_predictions_baseline_norm_softImpute,
    nommod_predictions_features_norm_softImpute,
    nommod_predictions_baseline_norm_softImpute) %>% 
    
    mutate(model = factor(x = model,levels = c("Nomothetic", "Nomothetic*","Idiographic")),
           features = factor(x = features,levels = c("Intercept-only", "Feature-based")))
  
  # IDs for individuals with results from all models, families, etc
  ids_all_models = all_predictions %>% 
    group_by(id, family, featue_imputation, model, features, scale) %>%  
    filter(!duplicated(id)) %>% 
    group_by(id) %>%  
    summarise(n=n()) %>% 
    filter(n==96)  %>% 
    pull(id)
  
  
  pop_lvl_metrics = all_predictions %>% 
    filter(id %in% ids_all_models) %>% 
    filter(set=="Test" & real_cat == "observed" & !is.na(observed) & !is.na(predicted)) %>% 
    group_by(family, featue_imputation, scale, model, features) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted,use = "complete.obs"),
              LCI=cor.test(x = observed, y = predicted)$"conf.int"[1],
              UCI=cor.test(x = observed, y = predicted)$"conf.int"[2],
              rho_sq=(cor(observed, predicted,use = "complete.obs")^2),
              LCI_sq=(cor.test(x = observed, y = predicted)$"conf.int"[1]^2),
              UCI_sq=(cor.test(x = observed, y = predicted)$"conf.int"[2]^2),
              mse=mse(observed,predicted),
              mae=mae(observed,predicted),
              mape=smape(observed,predicted)) 
  
  
  d=.9
  axis.text.size=12
  strip.text_size=9
  
  p1 = ggplot(data = pop_lvl_metrics %>% filter(features ==  "Feature-based"), 
              mapping = aes(x = family, y=rho_sq, col=model, alpha = featue_imputation)) +
    facet_grid(~scale) + 
    geom_point(position=position_dodge(d), size=2) + 
    geom_errorbar(aes(ymin=LCI_sq, ymax=UCI_sq), position=position_dodge(d)) +
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_alpha_manual(values = c(1,.5)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    ylim(c(0,1)) + 
    geom_hline(yintercept = .70, linetype="dotted") + 
    ylab(expression(Prediction~Accuracy~"("~R^2~")")) 
  p1
  
  p2=ggplot(data = pop_lvl_metrics %>% 
              filter(features ==  "Feature-based"), 
            mapping = aes(x = family, y=mape, fill=model, alpha=featue_imputation)) + 
    facet_grid(~scale) + 
    geom_col(position=position_dodge(d)) + 
    # geom_text(mapping = aes(label = round(mape,2), group=model), 
    # angle = 90, hjust=1, vjust=.5, color = "white", fontface = "bold",position = position_dodge(d)) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    scale_alpha_manual(values = c(1,.5)) + 
    # ylim(c(0,.3)) +
    ylab("Mean abs perc error (MAPE)") 
  
  p1_rel = ggplot(data = pop_lvl_metrics %>%
                    group_by(scale, model, family, featue_imputation) %>% 
                    summarise(features = features, 
                              rho_rel = ((rho_sq-rho_sq[features=="Intercept-only"])/rho_sq[features=="Intercept-only"]),
                              rho_rel_log2 = log2(rho_sq/rho_sq[features=="Intercept-only"])) %>% 
                    filter(features!="Intercept-only" & model!="Nomothetic") , 
                  mapping = aes(x = family, y=rho_rel_log2, fill=model, alpha = featue_imputation)) + 
    facet_grid(~scale) + 
    geom_col(position=position_dodge(d), size=2) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    scale_alpha_manual(values = c(1,.5)) + 
    geom_hline(yintercept = 0) + 
    ylab(expression(atop(log[2]~Fold~Change~"in"~R^2, Over~Baseline~Model)))
  # ylim(c(-.23,.15)) +
  # geom_hline(yintercept = c(0.0994,-.194), linetype="dotted") + 
  # geom_text(mapping = aes(x = 2.5,y = .13, label="Feature-based model\nperforms better")) +
  # geom_text(mapping = aes(x = 2.5,y = -.22, label="Feature-based model\nperforms worse")) +
  
  
  p2_rel=ggplot(data = pop_lvl_metrics %>% 
                  group_by(scale, model, family, featue_imputation) %>% 
                  summarise(features = features, 
                            MAPE_rel_log2 = log2(mape/mape[features=="Intercept-only"]),
                            MAPE_diff = ((mape-mape[features=="Intercept-only"])),
                            MAPE_rel = ((mape-mape[features=="Intercept-only"])/mape[features=="Intercept-only"])) %>% 
                  filter(features!="Intercept-only") %>% 
                  droplevels(), 
                mapping = aes(x = family, y=MAPE_rel_log2, fill=model, alpha = featue_imputation)) + #
    facet_grid(~scale ) + 
    geom_col(position=position_dodge(d), size=2) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors ) +
    scale_fill_manual(values = scale_colors) +
    scale_alpha_manual(values = c(1,.5)) + 
    ylab(expression(atop(log[2]~Fold~Change~"in"~"MAPE", Over~Baseline~Model))) + 
    geom_hline(yintercept = 0) 
  # ylim(-1.7,0.4) +
  # geom_hline(yintercept = c(.141, -1.54), linetype="dotted") + 
  # geom_text(mapping = aes(x = 2.5,y = .3, label="Feature-based model\nperforms worse")) +
  # geom_text(mapping = aes(x = 2.5,y = -1.65, label="Feature-based model\nperforms better")) 
  
  # Accuracy by day
  accuracy_by_day=merge(x = all_predictions %>% filter(features == "Feature-based" & real_cat=="observed" & set == "Test"  ), 
                        y = all_predictions %>% filter(features == "Feature-based" & real_cat=="observed" & model=="Idiographic" & set == "Train"  & scale=="CS(2-4df)") %>% 
                          group_by(id) %>% filter(date==max(date)) %>% select(id,date) %>% rename(last_date=date)) %>% 
    mutate(days=as.numeric(as.Date(date)-last_date),
           cat_days=cut(x = days, breaks = c(seq(0,28,7),150),include.lowest = T, labels = c(paste("week",1:4),"> week 4"))) %>% 
    group_by(cat_days,scale,model, family, featue_imputation) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted, use = "complete.obs"),
              rho_sq=cor(observed, predicted, use = "complete.obs")^2,
              LCI=cor.test(x = observed, y = predicted)$"conf.int"[1]^2,
              UCI=cor.test(x = observed, y = predicted)$"conf.int"[2]^2,
              mse=mse(observed,predicted),
              mae=mae(observed,predicted)) %>% 
    ungroup()
  
  d=0.9
  p3=accuracy_by_day %>% 
    filter(model=="Idiographic") %>% 
    ggplot(mapping = aes(x = cat_days, y = rho_sq, color=model, shape=scale)) + 
    facet_grid(~family + featue_imputation) + 
    geom_point(position=position_dodge(d), size=2) + #aes(size=n),
    geom_errorbar(aes(ymin = LCI, ymax = UCI),position=position_dodge(d)) + 
    theme_bw() + 
    ylab(expression(Prediction~Accuracy~"("~R^2~")")) + 
    xlab("Weeks in test set") + 
    theme(legend.position = c(.9,.9), 
          legend.text = element_text(size=12,color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size,color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_size_continuous(breaks = seq(from=0,to=250,by=50), range = c(2,5)) +
    labs(shape = "", color = "", size = "# CAT-DI assessments") + 
    guides(color = "none")  +
    ylim(c(0,1)) + 
    geom_hline(yintercept = .70, linetype="dotted") + 
    scale_color_manual(values = scale_colors, drop=F) 
  p3
  
  # extract a legend that is laid out horizontally
  legend_b <- get_legend( p2_rel +  guides(color = guide_legend(nrow = 1,override.aes = list(size = 1))) +
                            theme(legend.position = "top", legend.box = "horizontal"))
  
  FigS6=ggdraw() +
    draw_plot(plot = p2, x = .00, y = .50,  width = .5, height = .5) +
    draw_plot(plot = p2_rel + theme(legend.position = "none"), x = .00, y = .00,  width = .5, height = .5) +
    draw_plot(plot = p1, x = .5, y = .50,  width = .5, height = .5) +
    draw_plot(plot = p1_rel, x = .5, y = .00,  width = .5, height = .5) +
    
    
    draw_plot_label(label = c('A','C','B', 'D'),  
                    x = c(0, 0, 0.5, 0.5), 
                    y = c(1,.5,1,0.5), size = 20)
  
  
  # add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
  FigS6_withleg=plot_grid(legend_b, FigS6, ncol = 1, rel_heights = c(.03, 1))
  ggsave(plot = FigS6_withleg, filename = paste0('manuscript/figures/figs06_group_level_pred_performance.png'), width = 12, height = 10)
  
  rm(p1,p1_rel,p2,p2_rel,p3)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Sup Figure 7: Prediction performance for CAT-DI severity score within each individual
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  ids_n_geq_5=all_predictions %>% 
    group_by(family, featue_imputation, id, set, scale, model, features) %>% 
    summarise(n=sum(!is.na(predicted))) %>% 
    ungroup() %>% 
    group_by(id) %>% 
    summarise(min_n=min(n)) %>% 
    filter(min_n>=5) %>% 
    pull(id) 
  
  
  id_with_novar_in_test=all_predictions %>%
    filter(id %in% ids_n_geq_5 & set=="Test") %>% 
    group_by(family, featue_imputation, scale, model, features, id) %>%
    summarise(var_obs=var(observed)) %>%
    filter(var_obs==0)%>%
    pull(id) %>%
    unique()  
  

  tmp_x = all_predictions %>% 
    filter(id %in% ids_n_geq_5 & set=="Test") %>% 
    filter(!id %in% id_with_novar_in_test) %>%
    filter(id %in% ids_all_models) %>% 
    group_by(family, featue_imputation, scale, model, features, id) %>% 
    summarise(n=n(),
              rho=cor(observed, predicted, use = "complete.obs"),
              P_rho=cor.test(x = observed, y = predicted, alternative = "greater")$"p.value",
              LCI=cor.test(x = observed, y = predicted)$"conf.int"[1],
              UCI=cor.test(x = observed, y = predicted)$"conf.int"[2],
              rho_sq=cor(observed, predicted,use = "complete.obs")^2,
              LCI_sq=cor.test(x = observed, y = predicted)$"conf.int"[1]^2,
              UCI_sq=cor.test(x = observed, y = predicted)$"conf.int"[2]^2,
              mse=mse(observed,predicted),
              mae=mae(observed,predicted),
              mape=smape(observed,predicted)) %>% 
    ungroup() %>% 
    mutate(LCL=CI.Rsq(rsq = rho_sq, n = n, k = 1, level = 0.95)$"LCL",
           UCL=CI.Rsq(rsq = rho_sq, n = n, k = 1, level = 0.95)$"UCL")  %>% 
    group_by(family, featue_imputation, features, scale, model) %>% #
    mutate(BHP=p.adjust(p = P_rho, method = "BH")) %>% 
    ungroup() 
  
  tmp_y = all_predictions %>% 
    select(family, featue_imputation, id,scale,observed, set, model, features) %>% 
    group_by(family, featue_imputation, scale, model, features, id) %>% 
    summarize(abs_med_diff=abs(median(observed[set=="Train"],na.rm = T)-median(observed[set=="Test"],na.rm = T)))
  
  ind_lvl_metrics = merge(x = merge(x = tmp_x,  y = tmp_y,all.x = T), y = demographics %>% select(subject_id, treatment_wave),  by.x = "id", by.y =  "subject_id") %>%
    arrange(id, scale,model, features)
  
  rm(tmp_x,tmp_y)
  
  d=.9
  axis.text.size=12
  
  median_mape = ind_lvl_metrics %>% 
    group_by(family, featue_imputation, scale, model, features) %>% 
    filter(features ==  "Feature-based") %>%
    summarise(med=median(mape)) 
  
  p1=ggplot(data = ind_lvl_metrics %>% 
              filter(features ==  "Feature-based"), 
            mapping = aes(x = family, y=mape, fill=model, alpha = featue_imputation)) + 
    facet_grid(~scale) + 
    geom_boxplot(position=position_dodge(d)) + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size=axis.text.size, color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          axis.text = element_text(size=axis.text.size, color="black"),
          strip.text = element_text(size=strip.text_size)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) +
    scale_alpha_manual(values = c(1,.5)) + 
    # scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,.2)) + 
    ylab("Mean abs perc error (MAPE)") +
    geom_hline(yintercept = median_mape[which.min(median_mape$med),] %>% pull(med), linetype="dashed")
  
  
  p1_rel=ggplot(data = ind_lvl_metrics %>% 
                  group_by(family, featue_imputation, id, scale, model) %>% 
                  summarise(features = features, 
                            abs_med_diff = abs_med_diff, 
                            MAPE_diff = (mape - mape[features=="Intercept-only"]),
                            MAPE_log2FC = log2(mape/mape[features=="Intercept-only"]),
                            MAPE_rel = (mape - mape[features=="Intercept-only"])/mape[features=="Intercept-only"]) %>% 
                  filter(features!="Intercept-only") , 
                mapping = aes(x = family, y=MAPE_log2FC, fill=model, alpha = featue_imputation)) + #abs_med_diff > 10
    facet_grid(~ scale ) + 
    geom_boxplot(position=position_dodge(d)) + 
    theme_bw() + 
    theme( legend.position = "none",
           legend.title = element_blank(),
           legend.text = element_text(size=axis.text.size, color="black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.title = element_text(size=15),
           axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
           axis.text = element_text(size=axis.text.size, color="black"),
           axis.title.x = element_blank(),
           strip.text = element_text(size=strip.text_size)) + 
    scale_alpha_manual(values = c(1,.5)) + 
    scale_color_manual(values = scale_colors ) +
    scale_fill_manual(values = scale_colors) +
    ylab(expression(atop(log[2]~~Fold~Change~"in"~"MAPE", Over~Baseline~Model))) +
    geom_hline(yintercept = 0)  + 
    scale_y_continuous(limits = c(-8,8), breaks = seq(-8,8,2)) + 
    geom_hline(yintercept = 1, linetype='dotted')  + 
    geom_hline(yintercept = -1, linetype='dotted')  
  
  p2 = ind_lvl_metrics  %>% 
    filter(features != "Intercept-only") %>% 
    group_by(family, featue_imputation, scale, model) %>% 
    summarise(n=sum(BHP<=.05), N=n()) %>% 
    mutate(pi = n/N) %>% 
    ggplot(mapping = aes(x = family , y = pi, fill=model, alpha = featue_imputation)) + 
    facet_grid(~scale) + 
    geom_col(position = position_dodge(.9)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right", # c(.3,.85),
          legend.title = element_blank(),
          axis.title.y = element_text(size=14),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=12,color="black"),
          axis.text.x = element_text(size=12,color="black", angle = 90, vjust = .5),
          strip.text = element_text(size=strip.text_size)) + 
    scale_alpha_manual(values = c(1,.5)) + 
    scale_color_manual(values = scale_colors, drop=F) +
    scale_fill_manual(values = scale_colors, drop=F) + 
    geom_text(mapping = aes(label = n, group=model), angle = 90, hjust=1, vjust=.5, color = "white", fontface = "bold",position = position_dodge(.9)) + 
    ylab("% Sign. Predicted Individuals") 
  p2
  
  # extract a legend that is laid out horizontally
  legend_b <- get_legend( p1 +  guides(color = guide_legend(nrow = 1,override.aes = list(size = 5))) +
                            theme(legend.position = "top", legend.box = "horizontal"))
  
  FigS7=ggdraw() +
    draw_plot(plot = p1 + theme(legend.position = 'none'),     x = .00, y = .5, height = .5, width = 1) +
    draw_plot(plot = p1_rel, x = .00, y = .0, height = .5, width = 1) +
    
    draw_plot_label(label = c('A','B'),  
                    x = c(0,0), 
                    y = c(1,.5), size = 20)
  
  FigS7_withleg=plot_grid(legend_b, FigS7, ncol = 1, rel_heights = c(.03, 1))
  
  ggsave(plot = FigS7_withleg, filename = paste0('manuscript/figures/figS07_ind_level_pred_performance.png'), width = 10, height = 10)
  
  rm(p1,p2, p1_rel,p3)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%  Sup Figure 8: Weekly prediction performance across and within individuals
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(1){
  
  all_predictions_weekly = rbind(
    idio_weeklypredictions_features_binom_AutoComp,
    idio_weeklypredictions_baseline_binom_AutoComp,
    nom_weeklypredictions_features_binom_AutoComp,
    nom_weeklypredictions_baseline_binom_AutoComp,
    nommod_weeklypredictions_features_binom_AutoComp,
    nommod_weeklypredictions_baseline_binom_AutoComp,
    
    idio_weeklypredictions_features_norm_AutoComp,
    idio_weeklypredictions_baseline_norm_AutoComp,
    nom_weeklypredictions_features_norm_AutoComp,
    nom_weeklypredictions_baseline_norm_AutoComp,
    nommod_weeklypredictions_features_norm_AutoComp,
    nommod_weeklypredictions_baseline_norm_AutoComp) %>%    
    mutate(model = factor(x = model,levels = c("Nomothetic", "Nomothetic*","Idiographic")),
           features = factor(x = features,levels = c("Intercept-only", "Feature-based")))
  
  
  # IDs for individuals with results from all models, families, etc
  ids_all_models = all_predictions_weekly %>% 
    group_by(id, family, featue_imputation, model, features, scale) %>%  
    filter(!duplicated(id)) %>% 
    group_by(id) %>%  
    summarise(n=n()) %>% 
    filter(n==12)  %>% 
    pull(id)
  
  # Population level metrics
  if(1){
    pop_lvl_metrics_weekly = all_predictions_weekly %>% 
      filter(id %in% ids_all_models) %>% 
      filter(set=="Test" & real_cat == "observed" & !is.na(observed) & !is.na(predicted)) %>% 
      group_by(family, featue_imputation, scale, model, features) %>% 
      summarise(n=n(),
                rho=cor(observed, predicted,use = "complete.obs"),
                LCI=cor.test(x = observed, y = predicted)$"conf.int"[1],
                UCI=cor.test(x = observed, y = predicted)$"conf.int"[2],
                rho_sq=(cor(observed, predicted,use = "complete.obs")^2),
                LCI_sq=(cor.test(x = observed, y = predicted)$"conf.int"[1]^2),
                UCI_sq=(cor.test(x = observed, y = predicted)$"conf.int"[2]^2),
                mse=mse(observed,predicted),
                mae=mae(observed,predicted),
                mape=smape(observed,predicted)) 
    
    
    d=.9
    axis.text.size=12
    strip.text_size=9
    
    p1 = ggplot(data = pop_lvl_metrics_weekly %>% filter(features ==  "Feature-based"), 
                mapping = aes(x = family, y=rho_sq, col=model)) + 
      geom_point(position=position_dodge(d), size=2) + 
      geom_errorbar(aes(ymin=LCI_sq, ymax=UCI_sq), position=position_dodge(d)) +
      theme_bw() + 
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size=axis.text.size, color="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
            axis.text = element_text(size=axis.text.size, color="black"),
            strip.text = element_text(size=strip.text_size)) + 
      scale_color_manual(values = scale_colors, drop=F) +
      scale_fill_manual(values = scale_colors, drop=F) +
      ylim(c(0,1)) + 
      geom_hline(yintercept = .70, linetype="dotted") + 
      ylab(expression(Prediction~Accuracy~"("~R^2~")")) 
    p1  
    
    p2=ggplot(data = pop_lvl_metrics_weekly %>% 
                filter(features ==  "Feature-based"), 
              mapping = aes(x = family, y=mape, fill=model)) + 
      geom_col(position=position_dodge(d)) + 
      geom_text(mapping = aes(label = round(mape,2), group=model),
                angle = 90, hjust=1, vjust=.5, color = "white", fontface = "bold",position = position_dodge(d)) +
      theme_bw() + 
      theme(axis.title.x = element_blank(),
            legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size=axis.text.size, color="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
            axis.text = element_text(size=axis.text.size, color="black"),
            strip.text = element_text(size=strip.text_size)) + 
      scale_color_manual(values = scale_colors, drop=F) +
      scale_fill_manual(values = scale_colors, drop=F) +
      # ylim(c(0,.3)) +
      ylab("Mean abs perc error (MAPE)") 
    p2
    
    p1_rel = ggplot(data = pop_lvl_metrics_weekly %>%
                      group_by(scale, model, family, featue_imputation) %>% 
                      summarise(features = features, 
                                rho_rel = ((rho_sq-rho_sq[features=="Intercept-only"])/rho_sq[features=="Intercept-only"]),
                                rho_rel_log2 = log2(rho_sq/rho_sq[features=="Intercept-only"])) %>% 
                      filter(features!="Intercept-only" & model!="Nomothetic") , 
                    mapping = aes(x = family, y=rho_rel_log2, fill=model)) + 
      geom_col(position=position_dodge(d), size=2) + 
      theme_bw() + 
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size=axis.text.size, color="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
            axis.text = element_text(size=axis.text.size, color="black"),
            strip.text = element_text(size=strip.text_size)) + 
      scale_color_manual(values = scale_colors, drop=F) +
      scale_fill_manual(values = scale_colors, drop=F) +
      geom_hline(yintercept = 0) + 
      ylab(expression(atop(log[2]~Fold~Change~"in"~R^2, Over~Baseline~Model)))
    # ylim(c(-.23,.15)) +
    # geom_hline(yintercept = c(0.0994,-.194), linetype="dotted") + 
    # geom_text(mapping = aes(x = 2.5,y = .13, label="Feature-based model\nperforms better")) +
    # geom_text(mapping = aes(x = 2.5,y = -.22, label="Feature-based model\nperforms worse")) +
    p1_rel
    
    p2_rel=ggplot(data = pop_lvl_metrics_weekly %>% 
                    group_by(scale, model, family, featue_imputation) %>% 
                    summarise(features = features, 
                              MAPE_rel_log2 = log2(mape/mape[features=="Intercept-only"]),
                              MAPE_diff = ((mape-mape[features=="Intercept-only"])),
                              MAPE_rel = ((mape-mape[features=="Intercept-only"])/mape[features=="Intercept-only"])) %>% 
                    filter(features!="Intercept-only") %>% 
                    droplevels(), 
                  mapping = aes(x = family, y=MAPE_rel_log2, fill=model)) + #
      geom_col(position=position_dodge(d), size=2) + 
      theme_bw() + 
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size=axis.text.size, color="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
            axis.text = element_text(size=axis.text.size, color="black"),
            strip.text = element_text(size=strip.text_size)) + 
      scale_color_manual(values = scale_colors ) +
      scale_fill_manual(values = scale_colors) +
      ylab(expression(atop(log[2]~Fold~Change~"in"~"MAPE", Over~Baseline~Model))) + 
      geom_hline(yintercept = 0) 
    # ylim(-1.7,0.4) +
    # geom_hline(yintercept = c(.141, -1.54), linetype="dotted") + 
    # geom_text(mapping = aes(x = 2.5,y = .3, label="Feature-based model\nperforms worse")) +
    # geom_text(mapping = aes(x = 2.5,y = -1.65, label="Feature-based model\nperforms better")) 
    p2_rel
    
    # extract a legend that is laid out horizontally
    legend_b <- get_legend( p2 +  guides(color = guide_legend(nrow = 1,override.aes = list(size = 1))) +
                              theme(legend.position = "top", legend.box = "horizontal"))
  }
  
  # Individual-level metrics
  if(1){
    
    ids_n_geq_5=all_predictions_weekly %>% 
      group_by(family, featue_imputation, id, set, scale, model, features) %>% 
      summarise(n=sum(!is.na(predicted))) %>% 
      ungroup() %>% 
      group_by(id) %>% 
      summarise(min_n=min(n)) %>% 
      filter(min_n>=5) %>% 
      pull(id) 
    
    
    id_with_novar_in_test=all_predictions_weekly %>%
      filter(id %in% ids_n_geq_5 & set=="Test") %>% 
      group_by(family, featue_imputation, scale, model, features, id) %>%
      summarise(var_obs=var(observed)) %>%
      filter(var_obs==0)%>%
      pull(id) %>%
      unique()  
    
    
    tmp_x = all_predictions_weekly %>% 
      filter(id %in% ids_n_geq_5 & set=="Test") %>% 
      filter(!id %in% id_with_novar_in_test) %>%
      filter(id %in% ids_all_models) %>% 
      group_by(family, featue_imputation, scale, model, features, id) %>% 
      summarise(n=n(),
                rho=cor(observed, predicted, use = "complete.obs"),
                P_rho=cor.test(x = observed, y = predicted, alternative = "greater")$"p.value",
                LCI=cor.test(x = observed, y = predicted)$"conf.int"[1],
                UCI=cor.test(x = observed, y = predicted)$"conf.int"[2],
                rho_sq=cor(observed, predicted,use = "complete.obs")^2,
                LCI_sq=cor.test(x = observed, y = predicted)$"conf.int"[1]^2,
                UCI_sq=cor.test(x = observed, y = predicted)$"conf.int"[2]^2,
                mse=mse(observed,predicted),
                mae=mae(observed,predicted),
                mape=smape(observed,predicted)) %>% 
      ungroup() %>% 
      mutate(LCL=CI.Rsq(rsq = rho_sq, n = n, k = 1, level = 0.95)$"LCL",
             UCL=CI.Rsq(rsq = rho_sq, n = n, k = 1, level = 0.95)$"UCL")  %>% 
      group_by(family, featue_imputation, features, scale, model) %>% #
      mutate(BHP=p.adjust(p = P_rho, method = "BH")) %>% 
      ungroup() 
    
    tmp_y = all_predictions_weekly %>% 
      select(family, featue_imputation, id,scale,observed, set, model, features) %>% 
      group_by(family, featue_imputation, scale, model, features, id) %>% 
      summarize(abs_med_diff=abs(median(observed[set=="Train"],na.rm = T)-median(observed[set=="Test"],na.rm = T)))
    
    ind_lvl_metrics_weekly = merge(x = merge(x = tmp_x,  y = tmp_y,all.x = T), y = demographics %>% select(subject_id, treatment_wave),  by.x = "id", by.y =  "subject_id") %>%
      arrange(id, scale,model, features)
    
    rm(tmp_x,tmp_y)
    
    d=.9
    axis.text.size=12
    
    median_mape = ind_lvl_metrics_weekly %>% 
      group_by(family, featue_imputation, scale, model, features) %>% 
      filter(features ==  "Feature-based") %>%
      summarise(med=median(mape)) 
    
    p3=ggplot(data = ind_lvl_metrics_weekly %>% 
                filter(features ==  "Feature-based"), 
              mapping = aes(x = family, y=mape, fill=model)) + 
      geom_boxplot(position=position_dodge(d)) + 
      theme_bw() + 
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size=axis.text.size, color="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size=14),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
            axis.text = element_text(size=axis.text.size, color="black"),
            strip.text = element_text(size=strip.text_size)) + 
      scale_color_manual(values = scale_colors, drop=F) +
      scale_fill_manual(values = scale_colors, drop=F) +
      ylab("Mean abs perc error (MAPE)") +
      geom_hline(yintercept = median_mape[which.min(median_mape$med),] %>% pull(med), linetype="dashed")
    p3
    
    p3_rel=ggplot(data = ind_lvl_metrics_weekly %>% 
                    group_by(family, featue_imputation, id, scale, model) %>% 
                    summarise(features = features, 
                              abs_med_diff = abs_med_diff, 
                              MAPE_diff = (mape - mape[features=="Intercept-only"]),
                              MAPE_log2FC = log2(mape/mape[features=="Intercept-only"]),
                              MAPE_rel = (mape - mape[features=="Intercept-only"])/mape[features=="Intercept-only"]) %>% 
                    filter(features!="Intercept-only") , 
                  mapping = aes(x = family, y=MAPE_log2FC, fill=model)) + #abs_med_diff > 10
      geom_boxplot(position=position_dodge(d)) + 
      theme_bw() + 
      theme( legend.position = "none",
             legend.title = element_blank(),
             legend.text = element_text(size=axis.text.size, color="black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.title = element_text(size=15),
             axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
             axis.text = element_text(size=axis.text.size, color="black"),
             axis.title.x = element_blank(),
             strip.text = element_text(size=strip.text_size)) + 
      scale_color_manual(values = scale_colors ) +
      scale_fill_manual(values = scale_colors) +
      ylab(expression(atop(log[2]~~Fold~Change~"in"~"MAPE", Over~Baseline~Model))) +
      geom_hline(yintercept = 0)  + 
      geom_hline(yintercept = 1, linetype='dotted')  + 
      geom_hline(yintercept = -1, linetype='dotted')  
    p3_rel
  }
  
  FigS8=ggdraw() +
    
    draw_plot(plot = p2 + theme(legend.position = "none"), x = .00, y = .66, width = .5, height = 0.33) +
    draw_plot(plot = p1,                                   x = .50, y = .66, width = .5, height = 0.33) +
    
    draw_plot(plot = p2_rel,                               x = .00, y = .33,  width = .5, height = 0.33) +
    draw_plot(plot = p1_rel,                               x = .50, y = .33,  width = .5, height = 0.33) +
    
    draw_plot(plot = p3,                                   x = .00, y = .00,  width = .5, height = 0.33) +
    draw_plot(plot = p3_rel,                               x = .50, y = .00,  width = .5, height = 0.33) +
    
    
    draw_plot_label(label = c('A','B','C', 'D','E', 'F'),  
                    x = c(0, 0.5, 0, 0.5, 0, 0.5), 
                    y = c(.99,.99,0.66,0.66,.33,.33), size = 20)
  
  
  # add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
  FigS8_withleg=plot_grid(legend_b, FigS8, ncol = 1, rel_heights = c(.03, 1))
  ggsave(plot = FigS8_withleg, filename = paste0('manuscript/figures/figs08_weekly_pred_performance.png'), width = 12, height = 15)
  
  rm(p1,p1_rel,p2,p2_rel,p3, p3_rel)
  
}

