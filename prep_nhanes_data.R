# Author: Nikki DeCleene and Aisha Twalibu
# Date: 12/4/23
# Purpose: Download data of interest from NHANES & format variables

#####################################################################################
# Set-up
#####################################################################################

rm(list=ls())

# Load packages
library(utils)
library(data.table)
library(foreign)

# Save file names and variables of interest
files_to_download <- c('P_DEMO', 'P_BPXO', 
                       'P_ALQ', 'P_BPQ', 'P_DIQ', 'P_FSQ', 'P_HIQ', 
                       'P_HUQ', 'P_INQ', 'P_PAQ',  'P_SMQ', 'P_WHQ')

raw_vars <- c('RIAGENDR', 'RIDAGEYR', 'RIDRETH3', 'DMDEDUC2', 'DMDMARTZ', 'DMDBORN4', 'DMDYRUSZ', 'WTINTPRP', 'WTMECPRP',
              'BPXOSY1', 'BPXODI1', 'BPXOSY2', 'BPXODI2', 'BPXOSY3', 'BPXODI3',
              'ALQ111', 'ALQ121', 'ALQ130', 'BPQ020', 'BPQ050A', 'BPQ080', 'DIQ010', 'FSDHH', 'HIQ011', 
              'HUQ051', 'INDFMMPI', 'PAD680', 'SMQ020', 'SMQ040', 'WHD010', 'WHD020')

formatted_vars <- c('sex', 'age', 'race_ethnicity', 'education', 'marital_status',
                    'time_in_us', 'interview_survey_wt', 'exam_survey_wt',
                    'systolic_bp', 'diastolic_bp',
                    'avg_drinks_per_day', 'bp_hist', 'diagnosed_high_chol',
                    'diagnosed_diabetes', 'food_security', 'insurance', 'healthcare_util_year',
                    'poverty_index', 'min_sedentary_act', 'smoking_status', 'reported_bmi')

#####################################################################################
# Read in data
#####################################################################################

# Download each data file
for(file_name in files_to_download){
  print(file_name)
  download.file(paste0("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/", file_name, ".XPT"), temp_file <- tempfile(), mode="wb")
  assign(file_name, read.xport(temp_file))
}

# Merge data files together
list_df <- lapply(files_to_download, get)
data <- Reduce(function(x, y) merge(x, y, all=TRUE, by='SEQN'), list_df)
data <- setDT(data)
data <- data[, raw_vars, with=F]

#####################################################################################
# Data processing: Demographics
#####################################################################################

# Label sex codes
data[, sex := ifelse(RIAGENDR==1, 'Male', ifelse(RIAGENDR==2, 'Female', NA))]

# Label race/ethnicity codes
data[, race_ethnicity := ifelse(RIDRETH3==1, 'Mexican American',
                                ifelse(RIDRETH3==2, 'Other Hispanic',
                                       ifelse(RIDRETH3==3, 'Non-Hispanic White',
                                              ifelse(RIDRETH3==4, 'Non-Hispanic Black',
                                                     ifelse(RIDRETH3==6, 'Non-Hispanic Asian',
                                                            ifelse(RIDRETH3==7, 'Other or Multi-Racial', NA))))))]

# Set education as factor to order levels and label
data[, education := factor(DMDEDUC2, levels = c(1, 2, 3, 4, 5),
                           labels = c('Less than 9th grade', '9-11th grade', 'High school graduate',
                                      'Some college', 'College graduate'))]

# Label marital status codes
data[, marital_status := ifelse(DMDMARTZ==1, 'Married/Living with Partner',
                                ifelse(DMDMARTZ==2, 'Widowed/Divorced/Separated',
                                       ifelse(DMDMARTZ==3, 'Never married', NA)))]

# Set time in US as as factor to order levels and label
data[DMDBORN4==1, DMDYRUSZ := 0]
data[, time_in_us := factor(DMDYRUSZ, levels = c(0:4),
                            labels = c('Birth', '< 5 years', '5 years to < 15 years',
                                       '15 years to < 30 years', '>= 30 years'))]
# TODO: change to proportion of life in U.S. (as a backup, switch to born in US: yes or no)

# Rename age and survey weight variables
setnames(data, c('RIDAGEYR', 'WTINTPRP', 'WTMECPRP'), 
         c('age', 'interview_survey_wt', 'exam_survey_wt'))

# Subset data to just those age 20 or older
data <- data[age >= 20]

#####################################################################################
# Data processing: Examination
#####################################################################################

# create function for averaging BP readings following NHANES protocol from 1999-2002
avgbp <- function(x, diastolic) { 
  # initialize argument of whether to drop first reading
  drop_first <- T
  # remove missing measurements
  x <- na.omit(x) 
  # for diastolic readings, drop 0s when there is at least one non-zero reading
  if(diastolic & (0 %in% x)){
    if(length(x[x!=0]) > 0){
      if(x[1] == 0){
        # if first reading was a 0, this will be removed here, so first reading does not have to be dropped later
        drop_first <- F
      }
      x <- x[x!=0]
    }
  }
  if(length(x) == 0){
    # return NA if there are no measurements
    return(as.double(NA)) 
  } else if (length(x) == 1){
    # if there is only one measurement, return that
    return(x) 
  } else {
    if(drop_first){
      # if there are multiple measurements, drop the first reading 
      # (as long as first reading was not a 0, in which case it would have been removed already)
      x <- x[2:length(x)]
    }
    # return mean of readings
    return(mean(x))
  }
}

# Apply function to generate average SBP and DBP readings
data[, systolic_bp := avgbp(c(BPXOSY1, BPXOSY2, BPXOSY3), F), by=1:nrow(data)]
data[, diastolic_bp := avgbp(c(BPXODI1, BPXODI2, BPXODI3), T), by=1:nrow(data)]
# TODO: clean & improve code of this function

#####################################################################################
# Data processing: Questionnaire
#####################################################################################

# Create categories for alcohol consumption
setnames(data, 'ALQ130', 'avg_drinks_per_day')
data[avg_drinks_per_day %in% c(777, 999), avg_drinks_per_day := NA]
data[ALQ111 == 2 | ALQ121 == 0, avg_drinks_per_day := 0]
# TODO: Change into 3 categories to account for frequency (0 = Never, 1-2 = frequent, else = occasional)

# Create categories for high BP history
data[, bp_hist := ifelse(BPQ020==2, 'Not diagnosed', 
                         ifelse(BPQ020==1, 
                                ifelse(BPQ050A==2, 'Diagnosed, not on meds', 
                                       ifelse(BPQ050A==1, 'Diagnosed, on meds', NA)), NA))]

# Label high cholesterol diagnosis codes
data[, diagnosed_high_chol := ifelse(BPQ080==1, 'Yes', 
                                     ifelse(BPQ080==2, 'No', NA))]

# Label diabetes diagnosis codes
data[, diagnosed_diabetes := ifelse(DIQ010==1, 'Yes',
                                    ifelse(DIQ010 %in% c(2,3), 'No', NA))]

# Set food security categories as as factor to order levels and label
data[, food_security := factor(FSDHH, levels = c(1:4),
                               labels = c('Full', 'Marginal', 'Low', 'Very low'))]

# Label insurance status codes
data[, insurance := ifelse(HIQ011==1, 'Yes', 
                           ifelse(HIQ011==2, 'No', NA))]

# Set healthcare utilization categories as as factor to order levels and label
data[, healthcare_util_year := factor(HUQ051, 
                                      levels = c(0:8),
                                      labels = c('None', '1', '2-3',
                                                 '4-5', '6-7', '8-9',
                                                 '10-12', '13-15', '16+'))]
# TODO: Bin codes 4-8 into one category

# Set missing codes for minutes of sedentary activity to NA
data[PAD680 %in% c(7777, 9999), PAD680 := NA]

# Create variable for 'smoking status'
data[, smoking_status := ifelse(SMQ020==2, 'Never',
                                ifelse(SMQ020==1, 
                                       ifelse(SMQ040 %in% c(1, 2), 'Current', 
                                              ifelse(SMQ040==3, 'Former', NA)), NA))]

# Calculate self-reported BMI
data[WHD010 %in% c(7777, 9999), WHD010 := NA]
data[WHD020 %in% c(7777, 9999), WHD020 := NA]
data[, reported_bmi := 703 * (WHD020/(WHD010^2))]

# Rename poverty index and sedentary activity variables
setnames(data, c('INDFMMPI', 'PAD680'), c('poverty_index', 'min_sedentary_act'))

#####################################################################################
# Save data
#####################################################################################

# Drop unnecessary variables
data <- data[, formatted_vars, with=F]

# Check for missing data and variable type
for(col in names(data)){
  print(col)
  print(paste0('Percent missing: ', round(100*nrow(data[is.na(get(col))])/nrow(data), 2), '%'))
  print(paste0('Type: ', class(data[,get(col)])))
  print('----------------------------------------------')
}

# Save data
save(data, file = "prepped_nhanes_data.RData")
