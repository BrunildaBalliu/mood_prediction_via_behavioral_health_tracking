#!/bin/bash

# qrsh -l h_data=36G,h_rt=24:00:00

project_dir="/u/project/bballiu/bballiu/STAND"

JobType=1  # 1: Data QC and merge
           # 2: individual-level prediction of CAT-MH
           # 3: population-level prediction of CAT-MH
freeze_date="20210511"


###### 1: Generate data freeze 
if ([ $JobType -eq 1 ]); then
  for pred in "present"; do 
    for imputation_method in "no_imputation" "spline" "softimpute"; do 
      qsub -N $pred_$imputation_method.data -o ${project_dir}/logfiles/data_freeze_$pred_$imputation_method.o -e ${project_dir}/logfiles/data_freeze_$pred_$imputation_method.e -l h_data=32G,h_rt=12:00:00,highp $project_dir/code/submit_job.sh $JobType $project_dir $freeze_date $pred $imputation_method
    done
  done
fi

###### 2: Individual-level predictions
if ([ $JobType -eq 2 ]); then
pred="present"
for family in "gaussian" "binomial"; do
    for impute_features in "no_imputation" "spline" "softimpute" "AutoComplete"; do 
        batch=0
        qsub -N $family.$pred.$impute_features.$batch -o ${project_dir}/logfiles/$family.$pred.$impute_features.$batch.o -e ${project_dir}/logfiles/$family.$pred.$impute_features.$batch.e -l h_data=32G,h_rt=12:00:00,highp $project_dir/code/submit_job.sh $JobType $project_dir $freeze_date $pred $batch $impute_features $family
    done
  done
fi

###### 3: Population-level predictions
if ([ $JobType -eq 3 ]); then
pred="present"
for family in "binomial" "gaussian"; do # 
    for impute_features in "no_imputation" "spline" "softimpute" "AutoComplete"; do #
        qsub -N $family.$pred.$impute_features -o ${project_dir}/logfiles/$family.$pred.$impute_features.o -e ${project_dir}/logfiles/$family.$pred.$impute_features.e -l h_data=32G,h_rt=12:00:00,highp $project_dir/code/submit_job.sh $JobType $project_dir $freeze_date $pred $impute_features $family
    done
  done
fi
