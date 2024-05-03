#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(rsconnect)

#Data Import and Cleaning
gss_2004_import_tbl <- read_sav("../data/GSS2004.sav") %>%
  select(-where(~mean(is.na(.))>.75)) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(sex = factor(SEX,
                      levels=c("1","2"),
                      labels=c("Male","Female")), 
         exclude = NA,
         race = factor(RACE,
                       levels = c("1","2","3"),
                       labels = c("White", "Black", "Other")),
         avg_empathy = rowMeans(across(c(EMPATHY1, EMPATHY2,EMPATHY3, EMPATHY4,
                                         EMPATHY5, EMPATHY6, EMPATHY7))), #greater scores is more empathy towards others
         avg_probehav = rowMeans(across(c(GIVBLOOD, GIVHMLSS, RETCHNGE, CUTAHEAD, VOLCHRTY, GIVCHRTY, GIVSEAT,
                                          HELPAWAY, CARRIED, DIRECTNS, LOANITEM))), #lower scores is more enacted pro social behavior
         partyid = factor(PARTYID,
                          levels = c("0", "3","6"),
                          labels = c("Strong Democrat","Independent","Strong Republican")),
        rel_strength = factor(RELSPRT1,
                              levels = c("1", "2","3","4","5","6"),
                              labels = c("Many times a day", "Every day", "Most days", "Some days", "Once in a while","Never/Almost Never")),
  age = AGE) %>%
  select(sex, race, age, partyid, rel_strength, avg_empathy, avg_probehav)

gss_2004_tbl <- gss_2004_import_tbl[complete.cases(gss_2004_import_tbl$avg_empathy, gss_2004_import_tbl$avg_probehav, gss_2004_import_tbl$partyid, 
                                                   gss_2004_import_tbl$age, gss_2004_import_tbl$rel_strength), ]

#write_csv(gss_2004_tbl, "../data/gss_clean.csv")


#Visualization

(gss_2004_tbl %>%
  ggplot(aes(x=avg_empathy, y=avg_probehav)) +
  geom_jitter() +
  labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Average Empathy and Prosocial Behavior")) %>%
  ggsave("../figs/fig1.png", ., width=1920, height=1080, units="px")

(gss_2004_tbl %>%
       ggplot(aes(x=avg_empathy)) +
  geom_histogram() +
  labs(x = "Empathy Scores", y = "Frequency", title = "Histogram of Empathy Scores")) %>%
  ggsave("../figs/fig2.png", ., width=1920, height=1080, units="px")
  
(gss_2004_tbl %>%
ggplot(aes(x = avg_probehav)) +
  geom_histogram() +
    labs(x= "Prosocial Behavior Scores", y = "Frequency", title = "Histogram of Prosocial Behavior Scores")) %>%
  ggsave("../figs/fig3.png", ., width=1920, height=1080, units="px")

(gss_2004_tbl %>%
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = race)) +
    geom_boxplot() +
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Race on Prosocial Behavior")) %>%
    ggsave("../figs/fig4.png", ., width=1920, height=1080, units="px")
  
(gss_2004_tbl %>%
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = sex)) +
    geom_boxplot()+
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Sex on Prosocial Behavior")) %>%
  ggsave("../figs/fig5.png", ., width=1920, height=1080, units="px")


(gss_2004_tbl %>%
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = partyid)) +
    geom_boxplot()+
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Party ID on Prosocial Behavior")) %>%
  ggsave("../figs/fig6.png", ., width=1920, height=1080, units="px")


(gss_2004_tbl %>%
    ggplot(aes(x = avg_empathy, y = avg_probehav, fill = rel_strength)) +
    geom_boxplot()+
    labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Boxplot of Empathy and Religious Strength on Prosocial Behavior", fill = "Finding Strength in Religion")) %>%
  ggsave("../figs/fig7.png", ., width=1920, height=1080, units="px")

#Analysis

##Descriptive Stats
partyid <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(partyid) %>% 
  summarise(count = n(),
            avg_e = mean(avg_empathy),
            sd_empathy = sd(avg_empathy),
            avg_pb = mean(avg_probehav),
            sd_pb = sd(avg_probehav))

sex <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(sex) %>% 
  summarise(count = n(),
            avg_e = mean(avg_empathy),
            sd_empathy = sd(avg_empathy),
            avg_pb = mean(avg_probehav),
            sd_pb = sd(avg_probehav))

race <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(race) %>% 
  summarise(count = n(),
            avg_e = mean(avg_empathy),
            sd_empathy = sd(avg_empathy),
            avg_pb = mean(avg_probehav),
            sd_pb = sd(avg_probehav))

age <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(age) %>% 
  summarise(count = n(),
            avg_e = mean(avg_empathy),
            sd_empathy = sd(avg_empathy),
            avg_pb = mean(avg_probehav),
            sd_pb = sd(avg_probehav))

rel_strength <- gss_2004_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(rel_strength) %>% 
  summarise(count = n(),
            avg_e = mean(avg_empathy),
            sd_empathy = sd(avg_empathy),
            avg_pb = mean(avg_probehav),
            sd_pb = sd(avg_probehav))
#csv files
##write_csv(partyid,"../figs/party_id.csv")
##write_csv(age, "../figs/age.csv")
##write_csv(rel_strength, "../figs/rel_strength.csv")
##write_csv(race, "../figs/race.csv")
##write(sex, "../figs/sex.csv")

##correlation between attitudes and behaviors 
cor.test(gss_2004_tbl$avg_empathy, gss_2004_tbl$avg_probehav)

##liner regression model for sex
lm1 <- lm(avg_probehav ~ sex * avg_empathy, data = gss_2004_tbl)
summary(lm1)

##linear regression model for race
lm2 <- lm(avg_probehav ~ race * avg_empathy, data = gss_2004_tbl)
summary(lm2)

##linear regression model for age
lm3 <- lm(avg_probehav ~ age * avg_empathy, data = gss_2004_tbl)
summary(lm3)

##linear regression model for party id
lm4 <- lm(avg_probehav ~ partyid * avg_empathy, data = gss_2004_tbl)
summary(lm4)

##linear regression model for strength in religion
lm5 <- lm(avg_probehav ~ rel_strength * avg_empathy, data = gss_2004_tbl)
summary(lm5)

##linear regression model for all potential moderators
lm6 <- lm(avg_probehav ~ . , data = gss_2004_tbl)
summary(lm6)

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

print(partyid)

#View(age) a lot of variables so view is better

print(sex)

print(race)

print(rel_strength)

print(table1_tbl)

##clean data for shiny app
gss_shiny <- gss_2004_tbl %>%
  select(sex, race, age, partyid, rel_strength, avg_empathy, avg_probehav) %>%
  saveRDS("../shiny/gss_shiny/import.RDS")
