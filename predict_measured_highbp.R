# Author: Nikki DeCleene and Aisha Twalibu
# Date: 12/8/23
# Purpose: Generate model to predict measured high blood pressure & assess performance

#####################################################################################
# Set-up
#####################################################################################

rm(list=ls())

# load packages
library(data.table)
library(stats)
library(gridExtra)

#####################################################################################
# Load data
#####################################################################################

# load data
load("prepped_nhanes_data.RData")

# create outcome variable
data[, high_measured_bp := ifelse(systolic_bp >= 140 | diastolic_bp >= 90, 1,
                                  ifelse(systolic_bp < 140 | diastolic_bp < 90, 0, NA))]

#####################################################################################
# Fit model
#####################################################################################

mod <- glm(high_measured_bp ~ sex + age + race_ethnicity + education + marital_status + time_in_us + 
             avg_drinks_per_day + bp_hist + diagnosed_high_chol + diagnosed_diabetes + food_security + insurance + 
             healthcare_util_year + poverty_index + min_sedentary_act + smoking_status + reported_bmi, 
           family = binomial(link='logit'), data = data)
summary(mod)

# TODO: training/test split to assess
# TODO: use survey weights in the model?
