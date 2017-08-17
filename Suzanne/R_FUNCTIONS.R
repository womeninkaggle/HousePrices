
# ===============================================================
# FUNCTION TO SUMMARISE AN INPUT DATAFRAME, RETURNING A DATAFRAME 
# WITH SUMMARY DETAILS ABOUT THE SHAPE OF THE DATA
# AUTHOR : SUZANNE FOX : JULY 2017
# WORK IN PROGRESS, NEEDS ERROR CHECKING ADDED AND A LOAD OF 
# OTHER THINGS
# ===============================================================
myShape <- function(data.input, diagnostics=FALSE) {
  library(tibble)
  library(dplyr)
  library(caret)
  
  if (diagnostics==TRUE) print("Step 1")
  # STEP 1. get column names and make the basic dataframe
  x <- names(data.input)
  x <- data.frame(matrix(unlist(x), nrow=length(x), byrow=T))
  names(x)[1] <- "COLUMN_NAME"
  data.shape <- x
  
  if (diagnostics==TRUE) print("Step 2")
  # STEP 2. get the variable type and add to data.shape
  x <- lapply(data.input, class)
  x <- data.frame(matrix(unlist(x), nrow=length(x), byrow=T))
  names(x)[1] <- "DATA_TYPE"
  data.shape <- bind_cols(data.shape, x)
  
  if (diagnostics==TRUE) print("Step 3")
  # STEP 3. find the level of missing values
  x <- data.frame(MISSING_COUNT = sapply(data.input, function(x) sum(is.na(x))))
  data.shape <- bind_cols(data.shape, x)
  
  if (diagnostics==TRUE) print("Step 4")
  # STEP 4. get the level of unique values for each column
  x <- data.frame(UNIQUE_COUNT = sapply(data.input, function(x) length(unique(x))))
  data.shape <- bind_cols(data.shape, x)
  
  if (diagnostics==TRUE) print("Step 5")
  # STEP 5. work out as percents
  data.shape <- mutate(data.shape, MISSING_PCENT = MISSING_COUNT / nrow(data.input) * 100)
  data.shape <- mutate(data.shape, UNIQUE_PCENT = UNIQUE_COUNT / nrow(data.input) * 100)

  if (diagnostics==TRUE) print("Step 6")
  # STEP 6. caret estimation of near zero variance
  nrZeroVar <- nearZeroVar(data.input)
  names <- names(data.input)
  nrZeroNames <- names[nrZeroVar]
  data.shape <- mutate(data.shape, 
                       CARET_NRZEROVAR = ifelse(COLUMN_NAME %in% nrZeroNames,1,0))
  
  # STEP 6a. caret estimation of high correlation
  #data.corr <- cor(data.input)
  #highCorr <- findCorrelation(data.corr, 0.90)
  
  if (diagnostics==TRUE) print("Step 7")
  # STEP 7. mins/max/medians
  
  # min/mean etc fail if you try to use them on factor variables, but are OK
  # with characters. So we need to convert any factor variables to character
  # use mutate_if to do this. BUT if the data has been read using fread then
  # for some reason mutate_if converts every column to character and you lose
  # the integers to character types and then max, min look for the string
  # values, so a range of ints 1-1500 should have max 1500, but if it's a 
  # character type the max value is 999
  
  classes <- sapply(data.input, class)
  factor.count <- length(grep("factor", classes))
  if (factor.count > 0) {
    data.input.nofac <- mutate_if(data.input, is.factor, as.character)
  } else {
    data.input.nofac <- data.input
  }
  
  options(warn=-1)
  imin <- data.input.nofac %>% summarise_each(funs(min(., na.rm = TRUE)))
  imax <- data.input.nofac %>% summarise_each(funs(max(., na.rm = TRUE)))
  if (diagnostics==TRUE) print("Step 7 - med etc")
  imed <- data.input.nofac %>% summarise_each(funs(median(., na.rm = TRUE)))
  if (diagnostics==TRUE) print("Step 7 - meam etc")
  imean  <- data.input.nofac %>% summarise_each(funs(mean(., na.rm = TRUE)))
  options(warn=0)
  if (diagnostics==TRUE) print("Step 7 - finished")
  
  data.x <- bind_rows(imin, imax, imed, imean)
  data.x <- data.frame(t(data.x))
  names(data.x) <- c("MIN","MAX","MEDIAN","MEAN")
  data.shape <- bind_cols(data.shape, data.x)
  
  if (diagnostics==TRUE) print("Step 8")
  # STEP 8. remove factors
  data.shape <- mutate_if(data.shape, is.factor, as.character)
  
  if (diagnostics==TRUE) print("Step 9")
  # STEP 9. return the result
  return(data.shape)
}
