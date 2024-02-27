# Introduction

In this coding sample, I use a subset of the National Health and Nutrition Examination Survey (NHANES) 2011-2012 data to perform missing value imputation. This dataset contains survey data collected from respondents, alongside data obtained from wrist-worn accelerometers used to objectively measure physical activity(PA). Among 5,319 surveyed respondents, 1,065 individuals (20%) do not have sensory data due to refusal to consent. If individuals consenting to wear an accelerometer differ systematically from those who decline, the resulting sample may not accurately represent the target population (Catellier et al., 2005). This emphasizes the need to assess and address missing data arising from non-consent. 

The objective is to fill in the missing sensor-measured variables using multiple imputations. I experimented with three distinct methods available in the "mice" package: a parametric approach involving specifying variable distributions, predictive mean matching (semi-parametric), and random forest (nonparametric). I additionally evaluate the imputed results by comparing them with estimates derived from typical inverse consent probability-adjusted weights. I also show the process of fitting logistic models on multiply imputed data and combining estimates through pooling.

# Variables derived from the sensory data (variables of interest)
* mvpa_accelerometer: minutes spent in moderate-to-vigorous physical activity (MVPA)in a week: mvpa_accelerometer.
* FPC1: the score of the top principal component obtained from the extracted activity patterns through functional principal component analysis.
  
For those interested in the processing and feature extraction from accelerometer data, the corresponding codes are available in my GitHub repository: https://github.com/dsuolang/processing_accelerometer_nhanes.

# Variables from the survey data
* mvpa_selfreport: minutes spent in moderate-to-vigorous physical activity (MVPA)in a week reported in the survey.
* Demographic: age, gender, race, education, marital status, paid work, poverty/income ratio, self-reported health status, BMI
* Health outcome: hypertension, diabetes

They may also contain missing data.
