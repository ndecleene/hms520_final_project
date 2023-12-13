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
library(boot)
library(ggplot2)

# set seed
set.seed(4321)

# set proportion of data to be used for testing set
test_prop <- 0.1

#####################################################################################
# Load data
#####################################################################################

# load data
load("prepped_nhanes_data.RData")

# create outcome variable
data[, high_measured_bp := ifelse(systolic_bp >= 140 | diastolic_bp >= 90, 1,
                                  ifelse(systolic_bp < 140 | diastolic_bp < 90, 0, NA))]

# drop data without outcome variable
data <- data[!is.na(high_measured_bp)]

#####################################################################################
# Fit model
#####################################################################################

# split data into training and test
sample_size <- nrow(data)
i_test <- sample.int(sample_size, size = as.integer(test_prop * sample_size))
data[, test := 0]
data[i_test, test := 1]

# fit the model on the training data
model <- glm(high_measured_bp ~ sex + age + race_ethnicity + education + marital_status + prop_life_in_us + 
               num_drinks_per_yr + bp_hist + diagnosed_high_chol + diagnosed_diabetes + food_security + insurance + 
             healthcare_util_year + poverty_index + min_sedentary_act + smoking_status + reported_bmi, 
           family = binomial(link='logit'), data = data[test == 0])
summary(model)

# apply model to the data
data[, pred := predict(model, data, 'response')]

# draw random values between 0 and 1
data[, draws := runif(nrow(data))]

# code as predicted high BP if the threshold probability is greater than the draw value
data[, high_pred_bp := as.numeric(pred >= draws)]

#####################################################################################
# Assess model performance
#####################################################################################

# create function for calculating prevalence
get_rmse <- function(y, y_fit, na_rm = F) {
  sqrt(mean((y - y_fit)^2, na.rm = na_rm))
}

# calculate in and out of sample RMSE
insample_rmse <- with(data[test == 0,], 
                      get_rmse(y = high_measured_bp, y_fit = high_pred_bp, na_rm = T))
outsample_rmse <- with(data[test == 1,], 
                       get_rmse(y = high_measured_bp, y_fit = high_pred_bp, na_rm = T))

print(paste("Insample RMSE:", round(insample_rmse, 4)))
print(paste("Outsample RMSE:", round(outsample_rmse, 4)))

#####################################################################################
# Calculate prevalence
#####################################################################################

# create function for calculating prevalence
get_prev <- function(variable) {
  counts <- nrow(data[get(variable) == 1])
  ss <- nrow(data[get(variable) %in% c(0, 1)])
  100 * (counts/ss)
}

# view counts of measured high BP
table(data$high_measured_bp, useNA = 'ifany')

# view counts of predicted high BP
table(data$high_pred_bp, useNA = 'ifany')

# calculate prevalence of measured high BP
print(paste0('Measured prevalence of high blood pressure: ', 
             round(get_prev(variable = 'high_measured_bp'), 2), '%'))

# calculate prevalence of measured high BP
print(paste0('Predicted prevalence of high blood pressure: ', 
             round(get_prev(variable = 'high_pred_bp'), 2), '%'))

# create age bins
data[, age_group := floor(age/5) * 5]
data[, age_group := ifelse(age_group == 80, '80+', paste0(age_group, '-', age_group + 4))]

# calculate prevalence by age and sex
prevalence <- data[, list(measured_prev = weighted.mean(high_measured_bp, interview_survey_wt, na.rm = T),
                          predicted_prev = weighted.mean(high_pred_bp, interview_survey_wt, na.rm = T)), 
                   by = c('sex', 'age_group')]

# create plots
prevalence <- melt(prevalence, id.vars = c('sex', 'age_group'), 
                   value.name = 'proportion', variable.name = 'type')
prevalence[type == 'measured_prev', type := 'Measured']
prevalence[type == 'predicted_prev', type := 'Predicted']

p <- ggplot(prevalence, aes(x = age_group, y = proportion * 100, color = type, group = type)) + 
  facet_wrap(~sex) + theme_bw() + geom_line() + labs(title = 'Prevalence of High Blood Pressure',
                                                     x = 'Age', y = 'Percent (%)', color = 'Data Type')

print(p)
ggsave("model_results.png", plot = p, width = 13, height = 10)
