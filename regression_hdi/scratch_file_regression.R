

library(shiny)
library(bslib)
library(MASS)
suppressPackageStartupMessages(library(tidyverse))
library(ggfortify)
library(gridExtra)
library(readxl)
library(broom)


setwd("G:/My Drive/03_research_and_development/01_research_topics/paper_0_CRD_main/lesson_plans/crd_paper_lessons/regression_hdi")
## Bring in data
hdi_df = read_xlsx('hdi_2015.xlsx', sheet = 'raw_data') 
cn_df = read_xlsx('hdi_2015.xlsx', sheet = 'variables')

## Change column names
colnames(hdi_df) = cn_df$abbreviations
## Clean up data
hdi_df = hdi_df %>% 
  select(-contains('HDI'),-Country)


explanatory_variable <- 'Trade_Percent_GDP'
response_variable <- 'Fertility_Rate'

f <- as.formula(
  paste(response_variable, 
        paste(explanatory_variable, collapse = " + "), 
        sep = " ~ "))



tidy(lm(f,data = hdi_df))



