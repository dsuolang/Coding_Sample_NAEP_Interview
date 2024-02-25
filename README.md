# Coding_Sample_NAEP_Interview

In this coding sample, I use a subset of the National Health and Nutrition Examination Survey (NHANES) 2011-2012 data to perform missing value imputation. The data has already undergone preprocessing and cleaning, and these steps are not detailed here. This dataset contains survey data collected from respondents, alongside data obtained from wrist-worn accelerometers used to objectively measure physical activity(PA). 

Among 5,319 surveyed respondents, 1,065 individuals (20%) do not have sensor data due to refusal to consent. If individuals consenting to wear sensor devices differ systematically from those who decline, the resulting sample may not accurately represent the target population (Catellier et al., 2005). This emphasizes the need to assess and address missing data arising from non-consent. The objective is to fill in the missing sensor-measured variables using multiple imputation. I also examine the impact of the missing data on the estimation of physical activity.\

Variables derived from accelerometer data (variables of interest)\
* Minutes spent in moderate-to-vigorous physical activity (MVPA)in a week: mvpa_accelerometer\
* Extracted activity patterns using functional principal component analysis: FPC1 (the score of the top principal component)\
For those interested in the processing and feature extraction from accelerometer data, the corresponding codes are available in my GitHub repository: https://github.com/dsuolang/processing_accelerometer_nhanes.

Variables from survey data, they may also contain missing data.\
* Self-reported MVPA duration in a week: mvpa_selfreport\
* Demographic: age, gender, race, education, marital status, paid work, poverty/income ratio, insurance status, overall health status, BMI, alcohol, smoking\
* Health outcome: hypertension, diabetes, heartdisease\
