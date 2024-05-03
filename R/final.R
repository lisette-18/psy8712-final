#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(8712)

#Data Import and Cleaning
gss_2004_import_tbl <- read_sav("../data/GSS2004.sav") %>%
  select(-where(~mean(is.na(.))>.75)) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(SEX = factor(SEX,
                      levels=c("1","2"),
                      labels=c("Male","Female")), 
         exclude = NA,
         RACE = factor(RACE,
                       levels = c("1","2","3"),
                       labels = c("White", "Black", "Other")),
         avg_att = rowMeans(across(c(EMPATHY1, EMPATHY2,EMPATHY3, EMPATHY4,EMPATHY5, EMPATHY6, EMPATHY7))), #lower scores is more accepting attitudes towards others
         avg_probehav = rowMeans(across(c(GIVBLOOD, GIVHMLSS, RETCHNGE, CUTAHEAD, VOLCHRTY, GIVCHRTY, GIVSEAT, HELPAWAY, CARRIED, DIRECTNS, LOANITEM)))) %>%
  #lower scores is more enacted pro social behavior
  select(SEX, RACE, avg_att, avg_probehav)

gss_2004_tbl <- gss_2004_import_tbl[complete.cases(gss_2004_import_tbl$avg_att, gss_2004_import_tbl$avg_probehav), ]

#write_csv(gss_2004_tbl, "../data/gss_clean.csv")

#clean data for shiny app
gss_2004_shiny <- gss_2004_tbl %>%
  select(SEX, RACE, avg_att, avg_probehav) %>%
  saveRDS("../shiny/gss_shiny/import.RDS")

#Visualization

gss_2004_tbl %>%
  ggplot(aes(x=avg_att, y=avg_probehav)) +
  geom_point() +
  geom_smooth(method="lm", color="purple") +
  labs(x = "Attitudes Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Average Attitudes and Prosocial Behavior")

ggplot(gss_2004_tbl,
       aes(x=avg_att)) +
  geom_histogram()

ggplot(gss_2004_tbl,
       aes(x = avg_probehav)) +
  geom_histogram()

gss_2004_tbl %>%
  ggplot(aes(x=avg_att, y=avg_probehav, color = SEX)) +
  geom_point() +
  geom_smooth(method="lm", color="purple") +
  labs(x = "Attitudes Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Average Attitudes and Prosocial Behavior")

gss_2004_tbl %>%
  ggplot(aes(x=avg_att, y=avg_probehav, color = RACE)) +
  geom_point() +
  geom_smooth(method="lm", color="purple") +
  labs(x = "Attitudes Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Average Attitudes and Prosocial Behavior")

gss_2004_tbl %>% 
  ggplot(aes(x= avg_att, y= avg_probehav, fill= RACE)) + 
  geom_boxplot() +
  labs(title= "Prosocial Behavior Scores by Attitudes and Race", xlab= "Attitudes Toward Others", ylab= "Prosocial Behavior")

gss_2004_tbl %>% 
  ggplot(aes(x= avg_att, y= avg_probehav, fill= SEX)) + 
  geom_boxplot() +
  labs(title= "Prosocial Behavior Scores by Attitudes and Sex", xlab= "Attitudes Toward Others", ylab= "Prosocial Behavior")
      
#Analysis

##correlation between attitudes and behaviors for Research Q1
cor.test(gss_2004_tbl$avg_att, gss_2004_tbl$avg_probehav)

##liner regression model for Research Q2
lm1 <- lm(avg_probehav ~ SEX * avg_att, data = gss_2004_tbl)
summary(lm1)

##linear regression model for Research Q3
lm2 <- lm(avg_probehav ~ RACE * avg_att, data = gss_2004_tbl)
summary(lm2)

##machine learning models
holdout_indices <- createDataPartition(gss_2004_tbl$avg_probehav,
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_2004_tbl[holdout_indices,]
training_tbl <- gss_2004_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$avg_probehav)

model1 <- train(
  avg_probehav ~ .,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model1
cv_m1 <- model1$results$Rsquared
holdout_m1 <- cor(
  predict(model1, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2

model2 <- train(
  avg_probehav ~ .,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model2
cv_m2 <- max(model2$results$Rsquared)
holdout_m2 <- cor(
  predict(model2, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2

model3 <- train(
  avg_probehav ~ .,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model3
cv_m3 <- max(model3$results$Rsquared)
holdout_m3 <- cor(
  predict(model3, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2

model4 <- train(
  avg_probehav ~ .,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model4
cv_m4 <- max(model4$results$Rsquared)
holdout_m4 <- cor(
  predict(model4, test_tbl, na.action = na.pass),
  test_tbl$avg_probehav
)^2

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared")
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared")

# Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

table1_tbl <- tibble(
  algo = c("regression", "elastic net","random forests","xgboost"),
  cv_rqs = c(
    make_it_pretty(cv_m1),
    make_it_pretty(cv_m2),
    make_it_pretty(cv_m3),
    make_it_pretty(cv_m4)
  ),
  ho_rqs = c(
    make_it_pretty(holdout_m1),
    make_it_pretty(holdout_m2),
    make_it_pretty(holdout_m3),
    make_it_pretty(holdout_m4)
  )
)
