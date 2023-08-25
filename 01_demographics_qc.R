#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Predicting Subjective Measures Of Mood From Mobile Sensor Data
#%%%%%%%%%%%%%%% Scripts to perform quality control and merge CAT and AWARE features
#%%%%%%%%%%%%%%% March 10th 2021
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%% Parameters and functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
project_dir=getwd()
freeze_date = "20210511"
source(file = "code/00_functions_for_stand.R")
library(dplyr)
library(readr)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%% Load and preprocess data 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

demographics=readRDS(file = paste0('data/subject_demographics_20210808.rds'))

"Load demographic supplement csv exports collected from Box folder"
supplement_csv_files <- list.files("data/demographic_supplements", full.names = TRUE, pattern = "csv$")

list_of_frames <- lapply(supplement_csv_files, function(file) {
  df <- read.csv(file)
  df <- df[, grepl("^(screen_id|record_id|ss_record_id|ss_quest_age|demog_age|ss_cont_race|demog_race)$", colnames(df))]
  colnames(df)[grepl("id", colnames(df))] <- "id"
  colnames(df)[grepl("age", colnames(df))] <- "age"
  colnames(df)[grepl("race", colnames(df))] <- "race"
  df
})

list_of_frames <- lapply(seq_along(list_of_frames), function(i) {
  data.frame(FileName = basename(supplement_csv_files[i]), list_of_frames[[i]])
})

demographic_supplement <- do.call(rbind, list_of_frames)
demographics_original <- demographics

# Assuming the "study_subject_id" / "id" column is unique in both data frames
# Fill in demographic data from supplemental files where missing in original demographics export
demographics$demog_race[demographics$demog_race == ""] <- with(demographic_supplement, race[match(demographics$study_subject_id[demographics$demog_race == ""], id)])

# Try to convert height and weight inputs to numerical values, removing extraneous text like "in", "inches", "pounds", "lbs"
calculate_bmi <- function(height, weight) {
  
  tryCatch(
    {
      height = parse_number(height, trim_ws = TRUE)
      weight = parse_number(weight, trim_ws = TRUE)
    },
    error=function(e){
      return(NA)
    },
    warning=function(w) {
      return(NA)
    }
  )
  if(is.numeric(height) && is.numeric(weight)) {
    bmi <- (weight / (height * height)) * 703
    return(bmi)
  }
  else {
    return(NA)
  }
}

calculate_bmis <- function(heights, weights) {
  mapply(calculate_bmi, heights, weights)
}

# Calculate BMI where missing in the original demographics data (primarily "is2" / "Wave 1 online" subjects)
demographics <- demographics %>%
  mutate(demog_bmi = ifelse(is.na(demog_bmi), calculate_bmis(demog_height_inches, demog_weight_lbs), demog_bmi))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


CleanData = demographics %>% 
  filter(treatment_group %in% c("iCBT", "ITN_Tier3", "ITN_Tier12")) %>% 
  rename(treatment_wave = treatment_group) %>% 
  mutate(treatment_wave = factor(x = treatment_wave, 
                                  levels = c("iCBT", "ITN_Tier12", "ITN_Tier3"),
                                  labels = c("Wave 1 - Online support", "Wave 2 - Online support", 
                                             "Wave2 - Clinical care"))) %>% 
  mutate(wave = factor(x = ifelse(test = grepl(pattern = "Wave 1",x = treatment_wave),yes = "Wave 1", no = "Wave 2")),
         treatment_group = case_when(
           treatment_wave=="Wave 1 - Online support"~"online support", 
           treatment_wave=="Wave 2 - Online support"~"online support", 
           treatment_wave=="Wave2 - Clinical care"~"clinical care",
           TRUE ~ NA_character_
       ))

data_dict = create_data_dict(paste0('data/DataDictionary_2020-03-28.csv'))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%% Re-level demographic data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Loop through all columns, and for each column that has a type requiring numerical coding of responses (e.g. radio, dropdown),
# re-level the data in that column using the corresponding answer text from the data_dict object
for (col in colnames(CleanData)) {
  if(!col %in% names(data_dict$response_map)) next
  cur_question_type = data_dict$question_map[col,"Field Type"]
  
  if(cur_question_type %in% c("radio","dropdown")){  #Single-response fields (e.g. income), i.e. only one numerical response allowed in form "0", "1", etc.
    cur_question_response_map = data_dict$response_map[[col]]
    CleanData %<>%
      mutate(!!as.name(col) := factor(CleanData[[col]],
                                      levels = cur_question_response_map$Response_ID,
                                      labels = cur_question_response_map$Response_text))
  } else if(cur_question_type %in% c("checkbox")){    #Multi-response fields (e.g. race), i.e. multiple numerical responses allowed in form "0", "0,1", "1" etc.
    cur_question_response_map = data_dict$response_map[[col]]
    CleanData %<>%
      mutate(!!as.name(col) := replace_tokens(CleanData[[col]],
                                              cur_question_response_map$Response_ID,
                                              cur_question_response_map$Response_text,
                                              old_sep = ",",
                                              new_sep = ","))
  } else if(cur_question_type %in% c("yesno")){    #Yes/no questions where 1=Yes and 0=No
    cur_question_response_map = data_dict$response_map[[col]]
    CleanData %<>%
      mutate(!!as.name(col) := factor(CleanData[[col]],
                                      levels = c(0,1),
                                      labels = c("No","Yes")))
  }                                    
}

colnames(CleanData) = gsub(pattern = "demog_",replacement = "",x = colnames(CleanData))
CleanData %<>% rename(sex=gender,gender=gender_curr)

CleanData = CleanData %>% 
  mutate(race = factor(x = race, 
  levels = c("White",         "Asian Indian", "Don't know", "Some other race",  "White,Chinese",     "Chinese",     "Prefer not to answer", "White,Vietnamese",   "White,Native Hawaiian",  "Korean",     "Filipino",    "Japanese",     "Vietnamese",  "Black/African American",  "Other Asian", "Chinese,Other Asian", "Native American,Some other race",  "Black/African American,Some other race", "White,Native American",  "White,Some other race", "Chinese,Japanese", "Chinese,Vietnamese",  "White,Filipino",     "White,Asian Indian", "White,Black/African American,Native American,Japanese",  "Chinese,Filipino", "White,Japanese",     "",                "NA","White,Other Asian",  "White,Black/African American", "Don't know,Prefer not to answer",  "White,Chinese,Other Asian", "White,Black/African American,Some other race",  "Vietnamese,Other Asian", "Native American", "Other Asian,Some other race",  "Japanese,Other Asian", "Filipino,Japanese", "White,Chinese,Filipino", "Filipino,Don't know",  "White,Korean"),
  labels = c(c("White alone", "Asian alone",  "Don't know", "Some other race",  "Two or more races", "Asian alone", "Prefer not to answer", "Two or more races",  "Two or more races",     "Asian alone", "Asian alone", "Asian alone",  "Asian alone", "Black/AA alone",          "Asian alone", "Asian alone",         "Two or more races",                "Two or more races",                      "Two or more races",      "Two or more races",     "Asian alone",      "Asian alone",         "Two or more races",  "Two or more races",  "Two or more races",                                      "Asian alone",      "Two or more races",  "Did not answer",  "Did not answer", "Two or more races", "Two or more races", "Prefer not to answer",  "Two or more races", "Two or more races", "Asian alone", "AI or AN alone",  "Two or more races", "Asian alone", "Asian alone", "Two or more races", "Asian alone",  "Two or more races")))
) %>%
  mutate(race = factor(x = race, 
                       levels = c("White alone", "Asian alone", "Black/AA alone", 
                                  "AI or AN alone", "Some other race",  
                                  "Two or more races", "Don't know", 
                                  "Prefer not to answer", "Did not answer"))) %>% 
  mutate(bmi = ifelse(test = bmi < 100,yes = bmi, no = NA)) %>% 
  mutate(gender = factor(x = gender, 
                         levels =  c("Male", "Female", "Transgender",  "Do not identify as male, female or transgender"),
                         labels = c("Male", "Female", "Transgender",  "Other")
                         ))
         
# Fix zero value BMI 
CleanData[CleanData$bmi == 0 & (!is.na(CleanData$bmi)),"bmi"] = NA
write.csv(x = CleanData, file = paste0('data/demographics_data_freeze_',freeze_date,'.csv'),row.names = F)



