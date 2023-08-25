#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Predicting Subjective Measures Of Mood From Mobile Sensor Data
#%%%%%%%%%%%%%%% Scripts to perform quality control of CAT-MH data
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
project_dir=getwd()
freeze_date = "20210511"
data_dir=paste0(project_dir,"/data/")
source(paste0(project_dir,"/code/00_functions_for_stand.R"))

cat_di_breaks=c(0,35,65,75,100)
cat_anx_breaks=c(0,35,50,65,100)
cat_levels=c("normal", "mild", "moderate", "severe")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Data processing 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%% read cat-mh data 
cat_mh = read.csv(file = paste0(data_dir,'cat_complete_data_freeze_',freeze_date,'.csv'), stringsAsFactors = F) %>% 
  filter(!study_name %in% c("is1","ss1","ss3")) %>% # remove entry studies and controls / normal
  droplevels() %>% 
  mutate(cat_start_time=as.Date(x = as.character(cat_start_time)),
         cat_duration_min = cat_duration/60,
         treatment_wave = factor(x = study_name, 
                                  levels = c("is2", "ss2", "itn", "itnc"),
                                  labels = c("Wave 1 - Online support", 
                                             "Wave 2 - Online support", 
                                             "Wave2 - Clinical care", 
                                             "Wave2 - Clinical care"))) %>%
  select(-study_name, -cat_duration) %>%
  arrange(subject_id, cat_start_time) %>% 
  mutate(wave = factor(x = ifelse(test = grepl(pattern = "Wave 1",x = treatment_wave),yes = "Wave 1", no = "Wave 2")),
         treatment_group = factor(x = case_when(
           treatment_wave=="Wave 1 - Online support"~"online support", 
           treatment_wave=="Wave 2 - Online support"~"online support", 
           treatment_wave=="Wave2 - Clinical care"~"clinical care",
           TRUE ~ NA_character_
         ), levels = c("online support","clinical care"))) %>% 
  rename(depression_severity=Depression.severity, anxiety_severity=Anxiety.severity) %>% 
  mutate(depression_category=factor(x = cut(x = depression_severity, breaks = cat_di_breaks, 
                                            labels = cat_levels,include.lowest = T), levels = cat_levels),
         anxiety_category=factor(x = cut(x = anxiety_severity, breaks = cat_anx_breaks, 
                                         labels = cat_levels,include.lowest = T), levels = cat_levels)) %>%
  select(-Depression.category, -Anxiety.category)


sprintf(fmt = "finished reading cat_mh data")

write.csv(x = cat_mh, file = paste0(data_dir,'cat_clean_data_freeze_',freeze_date,'.csv'))

n1=length(unique(cat_mh$subject_id))
n2=length(unique(cat_mh$study_subject_id))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% Manuscript claims about sample size 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Wave 1 included 182 individuals while Wave 2 included 266 individuals. 
cat_mh %>% 
  filter(!duplicated(study_subject_id)) %>%
  group_by(wave) %>%
  summarise(n=n())


# Wave 2 included individuals with mild to moderate (N=142) and severe (N=124) symptoms 
cat_mh %>% 
  select(subject_id, study_subject_id, treatment_wave, wave, treatment_group) %>% 
  filter(wave == "Wave 2" & !duplicated(study_subject_id) ) %>% 
  group_by(treatment_group) %>% 
  summarise(n=n())


# Eleven individuals participated in both Waves.
cat_mh %>% 
  filter(!duplicated(study_subject_id)) %>%
  group_by(subject_id) %>%
  summarise(n=n())%>%
  arrange(desc(n)) %>%
  group_by(n)%>%
  summarise(n=n())

# 10 individuals moved from the digital treatment arm to clinical care 
cat_mh %>% 
  select(subject_id, study_subject_id, treatment_wave, wave, treatment_group) %>% 
  filter(!duplicated(paste0(subject_id,treatment_group)) ) %>% 
  group_by(subject_id) %>% 
  summarise(n=n())%>%
  arrange(desc(n)) %>% 
  group_by(n)%>%
  summarise(n=n())

# Wave 1 participants can have a maximum of 13 CAT-DI assessments 
cat_mh %>% 
  filter(wave == "Wave 1") %>% 
  group_by(subject_id) %>%
  summarise(n=n())%>%
  arrange(desc(n))

# Wave 2 participants can have a maximum of 21 or 44 assessments, depending on severity and excluding assessments at baseline. 
cat_mh %>% 
  filter(wave == "Wave 2") %>% 
  group_by(subject_id,treatment_group) %>%
  summarise(n=n())%>%
  group_by(treatment_group) %>% 
  summarise(max_n=max(n))
  
# In total, participants provided a total of 4,507 CAT-DI assessments (out of 11,218 expected by the study protocols). 
# observed
nrow(cat_mh)
# expected
cat_mh %>% 
  select(subject_id, study_subject_id, treatment_wave, wave, treatment_group) %>% 
  filter(!duplicated(study_subject_id) ) %>% 
  group_by(wave,treatment_group) %>% 
  summarise(n=n())
182*13 + (124)*21 + 142*44


#%%%%%%%
# Proportion of variance explained
#%%%%%%%
demographics = read.csv(file = 'data/demographics_data_freeze_20210511.csv', header = T,as.is = T)
                        
# Filter individuals with at least two interviews in training set and two in test set.
ind_pass = cat_mh %>% group_by(subject_id) %>% summarise(n=n())  %>% filter(n>=5) %>% pull(subject_id)

cat_mh %<>% filter(subject_id %in% ind_pass)

cat_mh_demo=merge(x = cat_mh %>% mutate(subject_id=as.character(subject_id)), 
                  y = demographics[,c("subject_id", "age", "sex")],by = "subject_id")


cat_mh_demo %<>% group_by(subject_id) %>% 
  mutate(entry_date=as.Date(min(cat_start_time,na.rm = T))) %>% 
  mutate(days=as.numeric(as.Date(cat_start_time)-entry_date)) %>% 
  mutate(year=year(cat_start_time), 
         month=month(cat_start_time), 
         season=case_when(month %in% c(12,1,2)~"winter",
                          month %in% 3:5~"spring",
                          month %in% 6:8~"summer",
                          month %in% 9:11~"autumn"), 
         covid=cat_start_time >=as.Date("2020-03-01")) 


pheno=t(cat_mh_demo[,c("depression_severity","anxiety_severity")])

# Using multiple linear mixed models, without covariates with many NAs 
vars2plot=c("subject_id", "treatment_group", "days", "treatment_group.days", "season","year","covid", "age", "sex") 
labsvars2plot=c("Individual", "Treatment group", "Study week", "Treatment group x Study week", "Season", "Year", "COVID", "Age", "Sex","Residuals") 
form=as.formula("~(1|subject_id) + (1|season) + treatment_group + days + covid + age + sex + year + treatment_group:days")  
varPartRes=data.frame(fitExtractVarPartModel(formula = form,  exprObj = pheno, data = cat_mh_demo %>% 
                                                   mutate(treatment_group=treatment_group == "clinical care",
                                                          sex = sex == "Female", 
                                                          year=as.numeric(year)))) 
colnames(varPartRes) = gsub(pattern = "TRUE", replacement = "", x = colnames(varPartRes))
varPartRes = varPartRes[,c(vars2plot,"Residuals")] 
colnames(varPartRes) = labsvars2plot

varPartResCI=varPartConfInf(formula = form,  exprObj = pheno, data = cat_mh_demo %>% 
                              mutate(treatment_group=treatment_group == "clinical care",
                                     sex = sex == "Female", 
                                     year=as.numeric(year)), nsim=100) 
colnames(varPartResCI[[1]]) = colnames(varPartResCI[[2]])= gsub(pattern = "TRUE", replacement = "", x = colnames(varPartResCI[[1]]))
vars2plot=c("subject_id", "treatment_group", "days", "treatment_group:days", "season","year","covid", "age", "sex") 
varPartResCI_DI=varPartResCI[[1]][,c(vars2plot,"Residuals")] 
varPartResCI_ANX=varPartResCI[[2]][,c(vars2plot,"Residuals")] 
colnames(varPartResCI_DI) = colnames(varPartResCI_ANX)= labsvars2plot

VE_CAT_DI_prc=round(100*data.frame(t(varPartRes[1,]),t(varPartResCI_DI),check.names = F),2)
VE_CAT_ANX_prc=round(100*data.frame(t(varPartRes[2,]),t(varPartResCI_ANX),check.names = F),2)
colnames(VE_CAT_DI_prc)[1]=colnames(VE_CAT_ANX_prc)[1]="VarExp"

save(VE_CAT_DI_prc, VE_CAT_ANX_prc, file = "results/variance_explained.RData")




