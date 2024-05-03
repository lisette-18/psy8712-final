#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

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


#Analysis

##correlation between attitudes and behaviors
cor.test(gss_2004_tbl$avg_att, gss_2004_tbl$avg_probehav)

