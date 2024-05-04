#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #setting working directory for project
library(tidyverse) #using tidyverse library to have access to variety of tidyverse packages
library(haven) #using haven library for read_spss to be used in data import
library(caret) #using to run machine learning models later
library(rsconnect) #using to make shiny web app after completing this R file

#Data Import and Cleaning
gss_2004_import_tbl <- read_spss("../data/GSS2004.sav") %>% #using read_spss from haven as per example from class demonstrations
  select(-where(~mean(is.na(.))>.75)) %>% #retain only variables with less than 75% missingness 
  mutate(across(everything(), as.numeric)) %>% #mutate all variables as numeric for easier analysis and visualization later
  mutate(sex = factor(SEX,
                      levels=c("1","2"),
                      labels=c("Male","Female")), #mutating sex to create a factor with 2 levels and labels for easier analysis and visualization later
         race = factor(RACE,
                       levels = c("1","2","3"),
                       labels = c("White", "Black", "Other")), #mutating across race using the options provided by the gss codebook to create a factor with 2 levels and labels for easier use later
         avg_empathy = rowMeans(across(c(EMPATHY1, EMPATHY2,EMPATHY3, EMPATHY4,
                                         EMPATHY5, EMPATHY6, EMPATHY7))), #mutate across rowMeans to get average empathy scores that state that greater scores is more empathy towards others
         avg_probehav = rowMeans(across(c(GIVBLOOD, GIVHMLSS, RETCHNGE, CUTAHEAD, VOLCHRTY, GIVCHRTY, GIVSEAT,
                                          HELPAWAY, CARRIED, DIRECTNS, LOANITEM))), #mutate across rowMeans to get average prosocial behavior scores in which lower scores is more enacted pro social behavior
         partyid = factor(PARTYID,
                          levels = c("0", "3","6"),
                          labels = c("Strong Democrat","Independent","Strong Republican")), #mutate as factor to create the 3 most important party ids we want to focus on for the project
        rel_strength = factor(RELSPRT1,
                              levels = c("1", "2","3","4","5","6"),
                              labels = c("Many times a day", "Every day", "Most days", "Some days", "Once in a while","Never/Almost Never")), #mutate as factor to create labels of finding strength in one's religion/spirituality, lower scores indicate greater tendency to find strength in religion/spirituality
        age = AGE) %>% #changing age from uppercase to lowercase to keep consistent among the analysis
  select(sex, race, age, partyid, rel_strength, avg_empathy, avg_probehav) #select the 7 variables that i want to focus on for the project and not having extra irrelevant information

gss_2004_tbl <- gss_2004_import_tbl[complete.cases(gss_2004_import_tbl$avg_empathy, gss_2004_import_tbl$avg_probehav, gss_2004_import_tbl$partyid, 
                                                   gss_2004_import_tbl$age, gss_2004_import_tbl$rel_strength, gss_2004_import_tbl$race, gss_2004_import_tbl$sex), ] #creating a new tbl that only contains cases that have responses to every variable; total is 561 observations

#write_csv(gss_2004_tbl, "../data/gss_clean.csv") #creating a cleaned gss_clean file for future use if those who reproduce this project want to skip the intial data cleaning
#using write_csv not write.csv because the consistency with other tidyverse functions and its improved performance.

#Visualization

(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
  ggplot(aes(x=avg_empathy, y=avg_probehav)) + #exploring avg_empathy and avg_probehav for the x and y axis
  geom_jitter() + #using geom_jitter because it spreads the points out horizontally and/or vertically, which makes it easier to see the distribution 
  labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Average Empathy and Prosocial Behavior")) %>% #providing title for the scatterplot to make it easily interpretable
  ggsave("../figs/fig1.png", ., width=1920, height=1080, units="px") #saving as a publishable figure in order to add to write up

(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
       ggplot(aes(x=avg_empathy)) + #exploring the distribution of avgerage empathy toward other scores among participants
  geom_histogram() + #using geom_histogram to create histogram because it allows for easier visualization of the distribution compared other graphs
  labs(x = "Empathy Scores", y = "Frequency", title = "Histogram of Empathy Scores")) %>% #providing labels for easier interpretation
  ggsave("../figs/fig2.png", ., width=1920, height=1080, units="px")#saving as a publishable figure in order to add to write up
  
(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
ggplot(aes(x = avg_probehav)) + #exploring the distribution of average prosocial behavior scores among participants
  geom_histogram() + #using geom_histogram to create histogram because it allows for easier visualization of the distribution compared other graphs
    labs(x= "Prosocial Behavior Scores", y = "Frequency", title = "Histogram of Prosocial Behavior Scores")) %>%  #providing labels for easier interpretation
  ggsave("../figs/fig3.png", ., width=1920, height=1080, units="px")#saving as a publishable figure in order to add to write up

(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = race)) + #exploring the variation of distribution among the different race groups
    geom_boxplot() + #using geom_boxplot to visualize distribution among the racial groups because it allows for easier visualization of the distribution compared other graphs
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Race on Prosocial Behavior")) %>% #providing labels for easier interpretation
    ggsave("../figs/fig4.png", ., width=1920, height=1080, units="px")#saving as a publishable figure in order to add to write up
  
(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = sex)) + #exploring the variation of distribution among the different sex groups
    geom_boxplot() +  #using geom_boxplot to visualize distribution among the sex groups because it allows for easier visualization of the distribution compared other graphs
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Sex on Prosocial Behavior")) %>% #providing labels for easier interpretation
  ggsave("../figs/fig5.png", ., width=1920, height=1080, units="px")#saving as a publishable figure in order to add to write up


(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = partyid)) + #exploring the variation of distribution among the different political party id groups
    geom_boxplot()+  #using geom_boxplot to visualize distribution among the political party id groups because it allows for easier visualization of the distribution compared other graphs
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Party ID on Prosocial Behavior")) %>% #providing labels for easier interpretation
  ggsave("../figs/fig6.png", ., width=1920, height=1080, units="px") #saving as a publishable figure in order to add to write up


(gss_2004_tbl %>% #piping from gss_2004_tbl to know where to pull the data from
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = rel_strength)) + #exploring the variation of distribution among the different religion/spirtiuality strength groups
    geom_boxplot() + #using geom_boxplot to visualize distribution among the groups because it allows for easier visualization of the distribution compared other graphs
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Religious Strength on Prosocial Behavior", fill = "Finding Strength in Religion")) %>% #providing labels for easier interpretation
  ggsave("../figs/fig7.png", ., width=1920, height=1080, units="px") #saving as a publishable figure in order to add to write up

#Analysis

##Descriptive Stats
#using dplyr to create the descrptive stats because it is more clean than base R
partyid <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(partyid) %>% #grouping by political party id
  summarise(count = n(), #providing the number of participants in each group by n(), not nrow() because its provides cleaner output and is used in dplyr and summarize to be consistent in using tidyverse code
            avg_e = mean(avg_empathy), #providing the average empathy scores among the groups
            sd_empathy = sd(avg_empathy),#providing the sd of empathy scores among the groups
            avg_pb = mean(avg_probehav), #providing the average prosocial behavior scores among the groups
            sd_pb = sd(avg_probehav)) #providing the sd of prosocial behavior among the groups

sex <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(sex) %>% #grouping by sex
  summarise(count = n(), #providing the number of participants in each group by n(), not nrow() because its provides cleaner output and is used in dplyr and summarize to be consistent in using tidyverse code
            avg_e = mean(avg_empathy), #providing the average empathy scores among the groups
            sd_empathy = sd(avg_empathy),#providing the sd of empathy scores among the groups
            avg_pb = mean(avg_probehav),#providing the average prosocial behavior scores among the groups
            sd_pb = sd(avg_probehav)) #providing the sd of prosocial behavior among the groups
  

race <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(race) %>% #grouping by race 
  summarise(count = n(), #providing the number of participants in each group by n(), not nrow() because its provides cleaner output and is used in dplyr and summarize to be consistent in using tidyverse code
            avg_e = mean(avg_empathy),#providing the average empathy scores among the groups
            sd_empathy = sd(avg_empathy), #providing the sd of empathy scores among the groups
            avg_pb = mean(avg_probehav), #providing the average prosocial behavior scores among the groups
            sd_pb = sd(avg_probehav)) #providing the sd of prosocial behavior among the groups

age <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(age) %>% #grouping by age
  summarise(count = n(), #providing the number of participants in each group by n(), not nrow() because its provides cleaner output and is used in dplyr and summarize to be consistent in using tidyverse code
            avg_e = mean(avg_empathy), #providing the average empathy scores among the groups
            sd_empathy = sd(avg_empathy), #providing the sd of empathy scores among the groups
            avg_pb = mean(avg_probehav), #providing the average prosocial behavior scores among the groups
            sd_pb = sd(avg_probehav)) #providing the sd of prosocial behavior among the groups

rel_strength <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(rel_strength) %>% #grouping by finding strength in religion or spirituality
  summarise(count = n(), #providing the number of participants in each group by n(), not nrow() because its provides cleaner output and is used in dplyr and summarize to be consistent in using tidyverse code
            avg_e = mean(avg_empathy), #providing the average empathy scores among the groups
            sd_empathy = sd(avg_empathy), #providing the sd of empathy scores among the groups
            avg_pb = mean(avg_probehav), #providing the average prosocial behavior scores among the groups
            sd_pb = sd(avg_probehav)) #providing the sd of prosocial behavior among the groups

##csv files in order to display the descriptive stats of the different groups we are focusing on and using write_csv not write.csv because the consistency with other tidyverse functions and its improved performance.
##write_csv(partyid,"../figs/party_id.csv")
##write_csv(age, "../figs/age.csv")
##write_csv(rel_strength, "../figs/rel_strength.csv")
##write_csv(race, "../figs/race.csv")
##write_csv(sex, "../figs/sex.csv")

##correlation between attitudes and behaviors 
cor.test(gss_2004_tbl$avg_empathy, gss_2004_tbl$avg_probehav) #using cor.test for correlation between the two variables and using cor.test because it is a valuable tool for testing the strength of the relationship between these two numeric variables

##liner regression model for sex
lm1 <- lm(avg_probehav ~ sex * avg_empathy, data = gss_2004_tbl) #running linear regression models to test the models for the purpose of this project
summary(lm1) #summarize the model to view the output

##linear regression model for race
lm2 <- lm(avg_probehav ~ race * avg_empathy, data = gss_2004_tbl) #running linear regression models to test the models for the purpose of this project
summary(lm2) #summarize the model to view the output

##linear regression model for age
lm3 <- lm(avg_probehav ~ age * avg_empathy, data = gss_2004_tbl) #running linear regression models to test the models for the purpose of this project
summary(lm3) #summarize the model to view the output

##linear regression model for party id
lm4 <- lm(avg_probehav ~ partyid * avg_empathy, data = gss_2004_tbl) #running linear regression models to test the models for the purpose of this project
summary(lm4) #summarize the model to view the output

##linear regression model for strength in religion
lm5 <- lm(avg_probehav ~ rel_strength * avg_empathy, data = gss_2004_tbl) #running linear regression models to test the models for the purpose of this project
summary(lm5) #summarize the model to view the output

##linear regression model for all potential predictors
lm6 <- lm(avg_probehav ~ . , data = gss_2004_tbl) #running linear regression models to test the models for the purpose of this project
summary(lm6) #summarize the model to view the output

##machine learning models
holdout_indices <- createDataPartition(gss_2004_tbl$avg_probehav, #using data partition to split data in half to be used to create training and test datasets for machine learning
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_2004_tbl[holdout_indices,]  #creating the test data by selecting the holdout indices from the gss_2004_tbl
training_tbl <- gss_2004_tbl[-holdout_indices,] #creating the training data by deselecting the holdout indices from the gss_2004_tbl

training_folds <- createFolds(training_tbl$avg_probehav) #create the index folds for the machine learning

model1 <- train(
  avg_probehav ~ ., #because the data is cleaned to only use the variables that we want for the project, we are predicting prosocial behaviors with all other variables as predictors 
  training_tbl, #using the training data per the previous class demonstrations and we want to use the training_tbl for shuffled data based on the split
  method="lm", #using lm because it is the OLS model
  na.action = na.pass, #used so the model will run
  preProcess = c("center","scale","zv","nzv"), #used methods created in class but not medianImpute because data is complete
  trControl = trainControl(method="cv", #method cv to create cross-validation
                           number=10, #choosing ten folds
                           verboseIter=T, #to monitor the progress of the optimization algorithm 
                           indexOut = training_folds) #using the folds created earlier
)
model1
cv_m1 <- model1$results$Rsquared #identifying the Rsquared 
holdout_m1 <- cor(
  predict(model1, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2 #we use this test to calculate the r-squared values for each model and #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model

model2 <- train(
  avg_probehav ~ ., #because the data is cleaned to only use the variables that we want for the project, we are predicting prosocial behaviors with all other variables as predictors 
  training_tbl, #using the training data per the previous class demonstrations and we want to use the training_tbl for shuffled data based on the split
  method="glmnet",
  na.action = na.pass, #used so the model will run
  preProcess = c("center","scale","zv","nzv"), #used methods created in class but not medianImpute because data is complete
  trControl = trainControl(method="cv", #method cv to create cross-validation
                           number=10, #choosing ten folds
                           verboseIter=T, #to monitor the progress of the optimization algorithm 
                           indexOut = training_folds) #using the folds created earlier
)
model2
cv_m2 <- max(model2$results$Rsquared) #identifying the Rsquared 
holdout_m2 <- cor(
  predict(model2, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2 #we use this test to calculate the r-squared values for each model and #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model

model3 <- train(
  avg_probehav ~ ., #because the data is cleaned to only use the variables that we want for the project, we are predicting prosocial behaviors with all other variables as predictors 
  training_tbl, #using the training data per the previous class demonstrations and we want to use the training_tbl for shuffled data based on the split
  method="ranger",
  na.action = na.pass, #used so the model will run
  preProcess = c("center","scale","zv","nzv"), #used methods created in class but not medianImpute because data is complete
  trControl = trainControl(method="cv", #method cv to create cross-validation
                           number=10,  #choosing ten folds
                           verboseIter=T, #to monitor the progress of the optimization algorithm 
                           indexOut = training_folds) #using the folds created earlier
)
model3
cv_m3 <- max(model3$results$Rsquared) #identifying the Rsquared
holdout_m3 <- cor(
  predict(model3, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2 #we use this test to calculate the r-squared values for each model and #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model

model4 <- train(
  avg_probehav ~ ., #because the data is cleaned to only use the variables that we want for the project, we are predicting prosocial behaviors with all other variables as predictors 
  training_tbl, #using the training data per the previous class demonstrations and we want to use the training_tbl for shuffled data based on the split
  method="xgbLinear",
  na.action = na.pass, #used so the model will run
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv"), #used methods created in class but not medianImpute because data is complete
  trControl = trainControl(method="cv", #method cv to create cross-validation
                           number=10, #choosing ten folds
                           verboseIter=T, #to monitor the progress of the optimization algorithm 
                           indexOut = training_folds) #using the folds created earlier
)
model4
cv_m4 <- max(model4$results$Rsquared) #identifying the Rsquared 
holdout_m4 <- cor(
  predict(model4, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2 #we use this test to calculate the r-squared values for each model and #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared") #summarising the rsquared for all models
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared") #viewing dotplot of the rsquared 

# Publication
make_it_pretty <- function (formatme) { #using the function to make it easier to format the table
  formatme <- formatC(formatme, format="f", digits=2) #only provide two functions
  formatme <- str_remove(formatme, "^0") #remove leading zero
  return(formatme)
}

table1_tbl <- tibble(
  algo = c("regression", "elastic net","random forests","xgboost"),
  cv_rqs = c(
    make_it_pretty(cv_m1), #using the make_it_pretty function on the Rsquared 
    make_it_pretty(cv_m2), #using the make_it_pretty function on the Rsquared 
    make_it_pretty(cv_m3), #using the make_it_pretty function on the Rsquared 
    make_it_pretty(cv_m4) #using the make_it_pretty function on the Rsquared 
  ),
  ho_rqs = c(
    make_it_pretty(holdout_m1), #using the make_it_pretty function on the holdout Rsquared 
    make_it_pretty(holdout_m2), #using the make_it_pretty function on the holdout Rsquared 
    make_it_pretty(holdout_m3), #using the make_it_pretty function on the holdout Rsquared 
    make_it_pretty(holdout_m4) #using the make_it_pretty function on the holdout Rsquared 
  )
)

print(partyid) #viewing descriptive stats of party identification

#View(age) #a lot of variables so view is better

print(sex) #viewing descriptive stats of sex

print(race) #viewing descriptive stats of race

print(rel_strength) #viewing descriptive stats of finding strength in religion/spirituality 

print(table1_tbl) #viewing the machine learning tibble

##clean data for shiny app
gss_2004_tbl %>% #piping from this data so it knows where to pull the data from
  select(sex, race, age, partyid, rel_strength, avg_empathy, avg_probehav) %>% #selecting the variables present to be included in the shiny app
  saveRDS("../shiny/gss_shiny/import.RDS") #creating the shiny rds file to be imported to create the shiny web page