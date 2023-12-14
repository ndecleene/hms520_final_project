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
  - Rename variables
  - Label numeric codes
  - Create composite predictors from reported data
  - Average 3 systolic and diastolic blood pressure measurements using function incorporating NHANES protocol
  - Change missing codes to NAs
- Create visualizations to explore the data such as:
  - Box plots of the levels of systolic and diastolic blood pressure measurements by groups for categorical predictors
  - Scatter plots of the levels of systolic and diastolic blood pressure measurements vs continuous predictors 

### 12/8/23-12/10/23:

- Classify participants as having high blood pressure if the systolic blood pressure reading is >= 140 mmHg or the diastolic blood pressure reading is >= 90 mmHg
- Split the data into training and testing data using 90% and 10% of the data, respectively
- Fit a logistic regression model on the training data to predict whether a respondent has measured high systolic blood pressure using the following predictors:
  - Sex
  - Age
  - Race/ethnicity
  - Education
  - Marital status
  - Proportion of life lived within the US
  - Number of drinks consumed in a year
  - Blood pressure history
  - Diagnosis status of high cholesterol
  - Diagnosis status of diabetes
  - Food security
  - Insurance status
  - Healthcare utilization
  - Poverty index
  - Minutes of sedentary activity
  - Smoking status
  - Self-reported BMI
  
### 12/11/23-12/14/23

- Assess model performance by calculating the in- and out-of-sample RMSE
- Calculate the prevalence of measured and predicted high blood pressure in the sample
- Visualize results
- Record a 15 - 20 minute video to present our project including the code, the results, and the implications of this project
- Add all materials to the repository
