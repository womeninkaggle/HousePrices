
# clear any old stuff from the environment
rm(list=ls())
# clear any test variables
#rm(list=ls(pattern='^test'))

# ===================================================================
# kaggle - House Price regression 
# Setup - personalise to your environment
# ===================================================================

# cope with different paths on different machines
if (Sys.info()["nodename"]=="P37") {
  WorkDir <- "D:/kaggle/HousePrices/"
  FuncDir <- WorkDir
} else {
  WorkDir <- "C:/Users/suzan/Documents/Kaggle/"
}

library(dplyr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(caret)

File.train <- "train.csv"
data.train.orig <- fread(paste0(WorkDir, File.train))
source(paste0(FuncDir,"R_FUNCTIONS.R"))

# ===================================================================
# EDA 
# understand the basic shape of the data
# ===================================================================

data.train <- data.train.orig

# R doesn't like variable names that start with a number
data.train <- rename(data.train, X1stFlrSF = `1stFlrSF`)
data.train <- rename(data.train, X2ndFlrSF = `2ndFlrSF`)

# ===================================================
# First look at overall level of missing and unique
# ===================================================

# get a quick idea of the shape of the data
data.shape <- myShape(data.train, FALSE)

# set the factors so ggplot2 doesn't order alphabetically
data.shape$COLUMN_NAME <- factor(data.shape$COLUMN_NAME, 
                                 levels=unique(data.shape$COLUMN_NAME))

# visualise the missing and unique levels
ggplot(data.shape, aes(COLUMN_NAME, MISSING_PCENT)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Missing Value percent")

ggplot(data.shape, aes(COLUMN_NAME, UNIQUE_PCENT)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Unique Value percent")

# ==================================================
# Second, take a close look at the missing values
# ==================================================

# look at missing values
data.missing <- data.shape %>%
  dplyr::filter(MISSING_COUNT > 0) %>%
  dplyr::arrange(DATA_TYPE, desc(MISSING_PCENT))
#View(data.missing)

# drop some variables based on the result
dropvars <- c("PoolQC","MiscFeature","Alley","Fence",
              "FireplaceQu","LotFrontage","GarageYrBlt")
data.train <- data.train %>%
  select(-one_of(dropvars))

# have another look at the shape of the new data
data.shape <- myShape(data.train)

# ===================================================
# Third, take a close look at the near zero variance
# ===================================================

# look at near zero variance
data.zerovar <- data.shape %>%
  filter(CARET_NRZEROVAR > 0) 

# drop near zero variance data
dropvars <- data.zerovar$COLUMN_NAME
data.train <- data.train %>%
  select(-one_of(dropvars))

# have another look at the shape of the new data
data.shape <- myShape(data.train)

# tidy up my environment
rm(data.missing, data.zerovar)

# ===================================================
# Fourth, deal with remaining missing values
# ===================================================

# fill remaining NA values with central statistics
data.train = DMwR::centralImputation(data.train)

# save this version of the data to use in the markdown 
# document
write.csv(data.train, paste0(WorkDir,"train_eda.csv"))

# ===================================================
# Fifth, do some visualisation on character variables
# Or stop here and use the markdown document to write
# a report in word
# ===================================================

data.shape <- myShape(data.train)
data.shape$COLUMN_NAME <- factor(data.shape$COLUMN_NAME, 
                                 levels=unique(data.shape$COLUMN_NAME))

# get just the character variables 
variables.char <- as.vector(data.shape$COLUMN_NAME[data.shape$DATA_TYPE=="character"])

for (i in seq_along(variables.char)) {
  x <- c(variables.char[i], "SalePrice")
  
  print(paste(i, "plotting", x[1]))
  
  xplot <- data.train %>%
    select(one_of(x[1]), SalePrice) %>%
    ggplot(aes_string(x[1], "SalePrice")) +
    geom_boxplot() +
    geom_jitter(alpha = 0.1, colour="coral") +
    theme(axis.text.x = element_text(angle = 90, hjust =1)) +
    xlab(x[1])
  
  plot(xplot)
}

# ===================================================
# Sixth, scatter charts for numerics
# ===================================================

# get just the numeric variables 
variables.nums <- as.vector(data.shape$COLUMN_NAME[data.shape$DATA_TYPE=="integer"])
# remove the id
variables.nums <- setdiff(variables.nums,c("Id"))

for (i in seq_along(variables.nums)) {
  x <- c(variables.nums[i], "SalePrice")
  
  print(paste(i, "plotting", x[1]))
  
  xplot <- data.train %>%
    select(one_of(x[1]), SalePrice) %>%
    ggplot(aes_string(x[1], "SalePrice")) +
    geom_point(shape=1) +
    geom_smooth() +
    ggtitle(x[1])
  
  plot(xplot)
}
