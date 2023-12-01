# HMS 520 Autumn 2023 Final Project

## Team members:
- Nikki DeCleene
- Aisha Twalibu

## Project format:
(3) Analyzing a dataset that uses data wrangling and modeling tools in R

## Overall goal:
To use data from the National Health and Nutrition Examination Survey (NHANES) to explore trends and predictors of measured high systolic blood pressure among the US population

## Detailed plans and timeline:

### 12/4/23-12/7/23:

- Download data from NHANES and merge data files of interest (demographics, examination, questionnaire) together by participant ID
- Clean and format the data
  - Bin the ages of participants into age groups
  - Format other variables of interest
  - Classify participants as having high systolic blood pressure if it is >= 140 mmHg
- Create visualizations to explore the data such as:
  - Box plots of the levels of systolic blood pressure measurements by age group and sex
  - Stacked bar charts showing the proportion of individuals with high systolic blood pressure that have been previously diagnosed with hypertension or are on medication for hypertension 

### 12/8/23-12/10/23:

- Fit a model on the data to predict whether a respondent has measured high systolic blood pressure
  - Split the data into training and testing data
  - Select predictors to use from the NHANES data (such as diagnosed hypertension, hypertension medication usage, age, sex, race/ethnicity, educational level, marital status, BMI, diagnosed diabetes, diagnosed hypercholesterolemia, diagnosed cardiovascular diseases, insurance status, smoking status, alcohol use, physical activity, and income)
  - Fit a logistic regression model on the training data
  
### 12/11/23-12/14/23
- Assess model performance by calculating the in- and out-of-sample RMSE
- Visualize results
- Record a 15 - 20 minute video to present our project including the code, the results, and the implications of this project
- Add all materials to the repository
