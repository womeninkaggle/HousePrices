
# SETUP ============================================================
# Code to do a regression of Sales Value by Week No for each Household
# the slope of the line will be positive for spending more over time 
# and negative for spending less. You can double check by pulling out
# data for a few households and doing a scatter plot in excel and 
# adding a regression line. The numbers are the same. You could
# further refine this into spending a little more/a lot more etc
# depending on the gradient.

rm(list=ls())
WorkDir <- "C:/Users/suzan/Documents/R/dunhumby/"

# LIBRARIES ========================================================
library(dplyr)
library(ggplot2)
library(caret)
library(purrr)
library(reshape2)

source("C:/Users/suzan/Documents/R/R_CROSSTAB_XY.R")

# DATA =============================================================
data.transaction.data <- read.csv(paste0(WorkDir,"transaction_data.csv"))

# CUSTOMER SPEND OVER TIME : MORE OR LESS ==========================
# get household, week_no, total spend
data.spend <- data.transaction.data %>%
  group_by(household_key, WEEK_NO) %>%
  summarise(Spend=sum(SALES_VALUE)) %>%
  arrange(household_key, WEEK_NO)

# plot linear regression of Spend vs WEEK_NO for each household
model.spend <- data.spend %>%
  group_by(household_key) %>%
  do(model = lm(Spend ~ WEEK_NO, data = .))

# extract the slope from each list element using purrr
model.slope <-
  map(.x = model.spend$model,
      .f = ~.[[1]][2])

# change from a list to a data.frame
model.slope <- data.frame(Slope=unlist(model.slope))

# +ve gradient is spending more
# -ve gradient is spending less
data.mol <- bind_cols(model.spend, model.slope) %>%
  mutate(MOL=ifelse(Slope>=0, "More", "Less")) %>%
  select(household_key, Slope, MOL)

write.csv(data.mol, paste0(WorkDir,"Spend_More_Less.csv"), row.names = FALSE)


