

library(shiny)
library(bslib)
library(MASS)
suppressPackageStartupMessages(library(tidyverse))
library(ggfortify)
library(gridExtra)
library(readxl)
library(broom)


setwd("G:/My Drive/03_research_and_development/01_research_topics/paper_0_CRD_main/lesson_plans/crd_paper_lessons/regression_hdi")
hdi_df = read_xlsx('hdi_2015.xlsx') %>% 
  select(-contains('HDI'),-Country)

explanatory_variable <- 'Trade (% GDP)'
response_variable <- 'Fertility Rate'

f <- as.formula(
  paste(response_variable, 
        paste(explanatory_variable, collapse = " + "), 
        sep = " ~ "))



tidy(lm(y~x,data = hdi_df))



