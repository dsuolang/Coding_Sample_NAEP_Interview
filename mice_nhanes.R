###-------Multiple Imputation for the Missing Data in NHANES------###
### Author: Deji Suolang

#----------------------required packages----------------------------#
library(mice)
library(dplyr)
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(foreign)
library(devtools)
library(lavaan)
library(survey)
library(party)
library(naniar)
library(finalfit)
library(knitr)

set.seed(2024)



#------------------------prepare data------------------------------#

df <- read_dta("test_data.dta")

#self-reported PA
df <- df %>%
  mutate(across(c(vigPA_status, modPA_status), ~ ifelse(.x %in% c(1, 2), .x, NA))) %>%
  mutate(across(c(vigPA_status, modPA_status), ~ ifelse(.x %in% c(2), 0, .x)))
df <- df %>%
  mutate(across(c(vigPA_days, modPA_days), ~ ifelse(.x %in% c(77, 99), NA, .x))) %>%
  mutate(across(c(vigPA_days, modPA_days), ~ ifelse(.x %in% c(2), 0, .x)))
df <- df %>%
  mutate(across(c(vigPA_durations, modPA_durations), ~ ifelse(.x %in% c(7777, 9999), NA, .x))) %>%
  mutate(across(c(vigPA_days, modPA_days), ~ ifelse(.x %in% c(2), 0, .x)))
df$vigPA_days[df$vigPA_status == 0] <- 0
df$vigPA_durations[df$vigPA_status == 0] <- 0
df$modPA_days[df$modPA_status == 0] <- 0
df$modPA_durations[df$modPA_status == 0] <- 0
df$mvpa_status <-
  as.factor(ifelse(df$modPA_status == 0 & df$vigPA_status == 0, 0, 1))
df$mvpa_selfreport <-
  df$modPA_days * df$modPA_durations + df$vigPA_days * df$vigPA_durations

#demographics
df$gender <- as.factor(ifelse(df$gender == 2, "Female", "Male"))
df$race <-
  as.factor(with(df, ifelse(
    race == 1 | race == 2,
    "Hispanic",
    ifelse(race == 3, "NHwhite",
           ifelse(
             race == 4, "NHblack",
             ifelse(race == 5, "Other", NA)
           ))
  )))
df$edu <-
  as.factor(with(df, ifelse(
    edu == 1 | edu == 2,
    "less than highschool",
    ifelse(
      edu == 3,
      "highschool/ged",
      ifelse(edu == 4, "some college",
             ifelse(edu == 5, "college and above", NA))
    )
  )))
df$marital <- as.factor(with(df, ifelse(
  marital == 1,
  "married",
  ifelse(
    marital == 6,
    "living with partner",
    ifelse(
      marital == 5,
      "never married",
      ifelse(
        marital == 2 |
          marital == 3 | marital == 4,
        "separated/divorced/widowed",
        NA
      )
    )
  )
)))
df$poverty <- ifelse(df$poverty == '', NA, df$poverty)
df$poverty <- as.factor(ifelse(
  df$poverty < 1,
  "less than 1",
  ifelse(
    df$poverty >= 1 & df$poverty < 2,
    "1 to 2",
    ifelse(
      df$poverty >= 2 & df$poverty < 4,
      "2 to 4",
      ifelse(df$poverty > 4, "4 or higher", NA)
    )
  )
))

#health conditions
df$bmi <-
  round((as.numeric(df$weight) / as.numeric(df$height ^ 2)) * 703, 1)
df$self_reported_health <-
  ifelse(df$self_reported_health > 5, NA, df$self_reported_health)
df$hypertension <-
  as.factor(ifelse(df$BPQ020 == 1, 1, ifelse(df$BPQ020 == 2, 0, NA)))
df$diabetes <-
  as.factor(ifelse(df$DIQ010 %in% c(1, 3), 1, ifelse(df$DIQ010 == 2, 0, NA)))
df <- df[,-c(5:12, 23:24)]

options(warn = -1)



#----------explore missing patterns and mechanisms-----------------#

colSums(is.na(df))
vis_miss(df) # visualize missing rates for all variables

pattern_summary <- md.pattern(df, plot = F, rotate.names = FALSE)
pattern_summary
print(paste("There are", nrow(pattern_summary), "unique patterns")) # Number of unique missing data patterns

# Evaluate whether the mechanisms are MCAR or MAR
explanatory <- c("gender", "race", "edu", "poverty", "bmi")
dependent <-
  c("mvpa_accelerometer")  # FPC1 shares same missing pattern
for (var in explanatory) {
  plot <- ggplot(df, aes_string(x = var, y = "mvpa_accelerometer")) +
    geom_miss_point()
  print(plot)
}
df %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r"))



#---------------------data transformation-------------------------#

plot_specific_histograms <- function(df, start_col, end_col) {
  selected_cols <-
    names(df)[start_col:end_col]  # Select specified columns
  par(mfrow = c(ceiling(sqrt(
    length(selected_cols)
  )), ceiling(sqrt(
    length(selected_cols)
  ))))
  for (col in selected_cols) {
    hist(df[[col]], main = paste("Histogram of", col), xlab = col)
  }
}
plot_specific_histograms(df, 2, 4) # histogram shows these variables are skewed

df_normalized <- df %>%
  mutate_at(vars(mvpa_accelerometer, mvpa_selfreport), sqrt)



#--------------multiple imputation (method 1: parametric)---------#

allVars <- names(df_normalized)
missVars <- names(df_normalized)[colSums(is.na(df_normalized)) > 0]
predictorMatrix <-
  matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- colnames(predictorMatrix) <- allVars

exclude_vars <-
  c("seqn", "sdmvstra", "sdmvpsu", "wtint2yr", "consent_status")
imputerVars <- setdiff(allVars, exclude_vars)
imputerVars
predictorMatrix[, imputerVars] <- 1
diag(predictorMatrix) <- 0

imp_mi1 <- mice(
  data = df_normalized,
  method =
    c(
      "",
      "norm",
      "",
      "norm",
      "norm",
      "polyreg",
      "polyreg",
      "polyreg",
      "logreg",
      "polyreg",
      "norm",
      "",
      "",
      "",
      "logreg",
      "norm",
      "norm",
      "logreg",
      "logreg"
    ),
  predictorMatrix = predictorMatrix,
  m = 5,
  maxit = 10, # limiting iteration to 10 for faster run time in this coding sample demonstration
  print = FALSE
)



#---------multiple imputation (method 2: semi-parametric: pmm)-----#

imp_mi2 <- mice(
  data = df,
  method =  "pmm", # predictive mean matching
  predictorMatrix = predictorMatrix,
  m = 5,
  maxit = 10, # limiting iteration to 10 for faster run time in this coding sample demonstration
  print = FALSE
)



#---------multiple imputation (method 3: nonparametric: rf)---------#

imp_mi3 <- mice(
  data = df,
  method =  "rf", #random forest
  predictorMatrix = predictorMatrix,
  m = 5,
  maxit = 10, # limiting iteration to 10 for faster run time in this coding sample demonstration
  print = FALSE
)



#--------------------imputation diagnostics------------------------#

imp_list <- list(imp_mi1, imp_mi2, imp_mi3)

for (i in seq_along(imp_list)) { # Diagnostic plots
  print(bwplot(
    imp_list[[i]],
    pch = c(21, 20),
    cex = c(1, 1.5),
    main = paste("method", i)
  ))
  print(densityplot(
    imp_list[[i]],
    layout = c(2, 2),
    main = paste("method", i)
  ))
  
  fit <- with(imp_list[[i]], # under MAR the conditional distributions should be similar (an example is created)
              glm(as.formula(paste(
                "ici(imp_list[[i]]) ~", paste(imputerVars, collapse = " + ")
              )),
              family = binomial)) # model the propensities of each record being incomplete
  ps <-
    rep(rowMeans(sapply(fit$analyses, fitted.values)), # average propensities over the imputed datasets to obtain stability
        imp_list[[i]]$m + 1)
  print(
    xyplot(
      imp_list[[i]],
      mvpa_accelerometer ~ ps | as.factor(.imp),
      xlab = "Propensity that data is missing ",
      ylab = "MVPA_acclerometer",
      pch = c(1, 19),
      col = mdc(1:2),
      main = paste("method", i)
    )
  )
  
  print(
    xyplot(
      imp_list[[i]],
      FPC1 ~ ps | as.factor(.imp),
      xlab = "Propensity that data is missing ",
      ylab = "FPC1",
      pch = c(1, 19),
      col = mdc(1:2),
      main = paste("method", i)
    )
  )
}



#----------pool estimates based on Rubin's combing rule-----------#
combined_mvpa_accelerometer_mean <- c()
combined_mvpa_accelerometer_se <- c()
combined_FPC1_mean <- c()
combined_FPC1_se <- c()

for (i in 1:3) { # loop through each imputation
  combined_data <-
    complete(get(paste0("imp_mi", i)), "long")   # Combine the imputed data
  
  if (i == 1) {
    combined_data$mvpa_accelerometer <-
      combined_data$mvpa_accelerometer ^ 2 # transform it back to original scale
  }
  
  survey_design <- svydesign(
    id = ~ sdmvpsu,
    strata = ~ sdmvstra,
    weights = ~ wtint2yr,
    nest = TRUE,
    data = combined_data
  )
  
  mvpa_accelerometer_imp <- svyby(~ mvpa_accelerometer,
                                  ~ .imp,
                                  survey_design,
                                  FUN = svymean)
  combined_mvpa_accelerometer_mean[i] <-
    mean(mvpa_accelerometer_imp$mvpa_accelerometer)
  combined_mvpa_accelerometer_se[i] <-
    mean(mvpa_accelerometer_imp$se)
  
  FPC1_imp <- svyby(~ FPC1,
                    ~ .imp,
                    survey_design,
                    FUN = svymean)
  combined_FPC1_mean[i] <- mean(FPC1_imp$FPC1)
  combined_FPC1_se[i] <- mean(FPC1_imp$se)
}



#-----------consent propensity weighting (for comparison)----------#
consent_model <-
  ctree(
    consent_status ~ age + race + edu + marital + gender + poverty + self_reported_health + mvpa_status + mvpa_selfreport + bmi + hypertension + diabetes,
    df
  )
df$prop_scores <- predict(consent_model, type = "response")
df$prop_rank <- ntile(df$prop_scores , 10)
df <- df %>%
  group_by(prop_rank) %>%
  summarize(adj_wtint2yr_decile = median(1 / prop_scores)) %>% # median of 10th decile groups
  inner_join(df, by = 'prop_rank')
df <- df %>%
  mutate(adj_wtint2yr_decile = wtint2yr * adj_wtint2yr_decile)# adjust original survey weight 

survey_designs <-
  svydesign(
    id = ~ sdmvpsu,
    strata = ~ sdmvstra,
    weights = ~ adj_wtint2yr_decile,
    nest = TRUE,
    data = df
  )

mvpa_stats <-
  as.data.frame(svymean( ~ mvpa_accelerometer, survey_design, na.rm = TRUE)) # append the estimate from weighting
combined_mvpa_accelerometer_mean <-
  c(combined_mvpa_accelerometer_mean, mvpa_stats$mean)
combined_mvpa_accelerometer_se <-
  c(combined_mvpa_accelerometer_se,
    mvpa_stats$mvpa_accelerometer)
fpc1_stats <-
  as.data.frame(svymean( ~ FPC1, survey_design, na.rm = TRUE))
combined_FPC1_mean <- c(combined_FPC1_mean, fpc1_stats$mean)
combined_FPC1_se <- c(combined_FPC1_se, fpc1_stats$FPC1)



#-----estimate across different method, mean and standard error----#
table_data <-
  matrix(NA,
         nrow = 2,
         ncol = 4,
         dimnames = list(
           c("mvpa_accelerometer", "FPC1"),
           c("imp1", "imp2", "imp3", "weighting")
         ))
table_data[1,] <-
  paste(
    format(round(combined_mvpa_accelerometer_mean, 1), nsmall = 1),
    " (",
    format(combined_mvpa_accelerometer_se, digits = 2),
    ")",
    sep = ""
  )
table_data[2,] <-
  paste(
    format(combined_FPC1_mean, digits = 2),
    " (",
    format(combined_FPC1_se, digits = 2),
    ")",
    sep = ""
  )

kable(as.data.frame(table_data))



#------------logreg models using the completed dataset------------#

hyplogreg_m5 <- with(
  data = imp_mi3, # proceed with complete data imputed with method 3, as an example
  glm( 
    hypertension ~  #predict hypertension from all m=5 data sets, 
      age + gender + edu + poverty + mvpa_accelerometer + FPC1,
    family = binomial
  )
)
hyplogreg_m5 = pool(hyplogreg_m5) 
summary(hyplogreg_m5)
