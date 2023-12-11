# Author: Nikki DeCleene and Aisha Twalibu
# Date: 12/7/23
# Purpose: Create plots exploring relationship between BP and predictors

#####################################################################################
# Set-up
#####################################################################################

rm(list=ls())

# load packages
library(data.table)
library(ggplot2)
library(gridExtra)

#####################################################################################
# Load data
#####################################################################################

load("prepped_nhanes_data.RData")

#####################################################################################
# Exploratory plots: SBP
#####################################################################################

# create plots for each predictor
plot_list <- sapply(setdiff(names(data), c('systolic_bp', 'diastolic_bp', 'interview_survey_wt', 'exam_survey_wt')),
                    function(var) {
                      if(class(data[, get(var)]) == 'numeric'){
                        ggplot(data[!is.na(get(var))], aes(x=get(var), y=systolic_bp)) + geom_point() + theme_bw() + labs(x=var)
                      } else {
                        ggplot(data[!is.na(get(var))], aes(x=as.factor(get(var)), y=systolic_bp)) + geom_boxplot() + theme_bw() + labs(x=var) + 
                          theme(axis.text.x = element_text(angle = 15))
                      }
                    }, simplify = F)

# combine plots
sbp_fig <- grid.arrange(grobs = plot_list)

# save figure
ggsave("sbp_predictors.png", plot = sbp_fig, width = 13, height = 10)

#####################################################################################
# Exploratory plots: DBP
#####################################################################################

# create plots for each predictor
plot_list <- sapply(setdiff(names(data), c('systolic_bp', 'diastolic_bp', 'interview_survey_wt', 'exam_survey_wt')),
                    function(var) {
                      if(class(data[, get(var)]) == 'numeric'){
                        ggplot(data[!is.na(get(var))], aes(x=get(var), y=diastolic_bp)) + geom_point() + theme_bw() + labs(x=var)
                      } else {
                        ggplot(data[!is.na(get(var))], aes(x=as.factor(get(var)), y=diastolic_bp)) + geom_boxplot() + theme_bw() + labs(x=var) + 
                          theme(axis.text.x = element_text(angle = 15))
                      }
                    }, simplify = F)

# combine plots
dbp_fig <- grid.arrange(grobs = plot_list)

# save figure
ggsave("dbp_predictors.png", plot = dbp_fig, width = 13, height = 10)
