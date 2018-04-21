##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

rm(list = ls())

##=======================================
# needed packages
##=======================================

# data.frames
library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(gsubfn)

# Set Option : Date in English
Sys.setlocale("LC_TIME", "English_United States")

completePath <- function(file, ...){
    path_proj <- "D:/Documents/PROJETS/Kaggle/ouessant/ouessant_copy"
    complete_path <- sprintf("%s/%s", path_proj, file)
    return(complete_path)
}

##=======================================
# needed packages
##=======================================

load(completePath("data/cleaned_data.RData"))
source(completePath("code/utils.R"))

# parameters to debug
train <- meteo_train
test <- meteo_prev
replace_na <- TRUE
center <- TRUE
scale <- TRUE

numerics <- c(
    "temp", "pression", "hr",  "p_ros", "visi",
    "vt_moy", "vt_raf", "vt_dir", "rr_3h", "neige", "nebul"
)
factors <- c("Weekday", "Month")

by <-  c('Month', 'Hour')
variables <- c(numerics, factors)


## Check what happens without factors
list[train1, test1] <- deal_train_test_numerics(
    meteo_train,
    meteo_prev,
    numerics = numerics
)
sapply(train1[numerics], mean)
sapply(train1[numerics], sd)

list[train2, test2] <- deal_train_test_numerics(
    meteo_train,
    meteo_prev,
    numerics = numerics,
    by = by
)
tapply(train2$temp, train2[factors], mean)
tapply(train2$temp, train2[factors], sd)


list[train3, test3] <- deal_train_test_factors(
    train2, test2, factors = c("Weekday", "Month")
)

list[train4, test4, cols] <- deal_train_test(
    train, test,
    numerics = numerics,
    by = c('Month', 'Hour'),
    factors = c("Weekday", "Month")
)
    
