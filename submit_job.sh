#!/bin/bash

echo "Loading dependencies"
. /u/local/Modules/default/init/modules.sh
# module load R/3.6.1
module load R/4.1.0-BIO

JobType=$1
project_dir=$2
freeze_date=$3
pred=$4


# 1. Create data freeze 
if ([ $JobType -eq 1 ]); then
  imputation_method=$5
  my_script=${project_dir}/code/02_cat_n_features_qc_and_merge.R
  R --vanilla --slave -f $my_script --args $project_dir $freeze_date $pred $imputation_method
fi

# 2. Individual-level predictions
if ([ $JobType -eq 2 ]); then
  echo Running individual-level predictions
  batch=$5
  impute_features=$6
  family=$7
  my_script=${project_dir}/code/03_ind_lvl_prediction_cat_mh.R
  R --vanilla --slave -f $my_script --args $project_dir $freeze_date $pred $batch $impute_features $family
  
fi

# 3. Population-level predictions
if ([ $JobType -eq 3 ]); then
  echo Running population-level predictions
  impute_features=$5
  family=$6
  echo $project_dir 
  echo $freeze_date 
  echo $pred 
  echo $impute_features
  echo $family
  
  my_script=${project_dir}/code/03_pop_lvl_prediction_cat_mh.R
  R --vanilla --slave -f $my_script --args $project_dir $freeze_date $pred $impute_features $family
fi





