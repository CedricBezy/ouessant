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
library(xgboost)
library(gsubfn)

# Set Option : Date in English
Sys.setlocale("LC_TIME", "English_United States")

completePath <- function(path, ...){
    path_proj <- "D:/Documents/PROJETS/Kaggle/ouessant/ouessant_copy"
    complete_path <- sprintf(path, path_proj)
    return(complete_path)
}


##==================================================
# make Train and Test Df
##==================================================
load(completePath('data/train_prev.RData'))
source(completePath('%s/code/utils.R'))
source(completePath('%s/code/scenario_1.R'))
source(completePath('%s/code/makeTrainTest_variable.R'))

list[trainDf, prevDf, x_vars] <- make_train_test_data(data_train, data_prev, x_vars)

##==================================================
# Xscale
##==================================================

Ytrain <- trainDf[c('P0', 'DP1', 'DP2')]

Xtrain <- trainDf[c('Hour', x_vars)]
Xprev <- prevDf[c('Hour', x_vars)]

params <- list(
    booster = "gbtree",
    nrounds = 400,
    eta = 0.1,
    max_depth = 15,
    min_child_weight = 0
)

bst0 <- xgboost(
    data = as.matrix(Xtrain),
    label = Ytrain$P0,
    max_depth = params$max_depth,
    eta = params$eta,
    min_child_weight = params$min_child_weight,
    nrounds = params$nrounds,
    params = list(booster = params$booster, normalize_type = 'forest'),
    objective = "reg:linear"
)

bst1 <- xgboost(
    data = as.matrix(Xtrain),
    label = Ytrain$DP1,
    max_depth = params$max_depth,
    eta = params$eta,
    min_child_weight = params$min_child_weight,
    nrounds = params$nrounds,
    params = list(booster = params$booster, normalize_type= 'forest'),
    objective = "reg:linear"
)

bst2 <- xgboost(
    data = as.matrix(Xtrain),
    label = Ytrain$DP2,
    max_depth = params$max_depth,
    eta = params$eta,
    min_child_weight = params$min_child_weight,
    nrounds = params$nrounds,
    params = list(booster = params$booster, normalize_type = 'forest'),
    objective = "reg:linear"
)


puiss_prev <- data.frame(
    dt_posix = prevDf$dt_posix,
    P0 = predict(bst0, as.matrix(Xprev)),
    DP1 = predict(bst1, as.matrix(Xprev)),
    DP2 = predict(bst2, as.matrix(Xprev))
)

submitDf <- puiss_prev %>%
    dplyr::mutate(
        P1 = P0 + DP1,
        P2 = P0 + DP2,
        DP1 = NULL,
        DP2 = NULL
    ) %>%
    tidyr::gather(variable, puissance, c(P0, P1, P2)) %>%
    dplyr::mutate(
        variable = factor(
            variable,
            levels = paste0("P", 0:2),
            labels = 0:2
        ),
        Hour = as.numeric(as.character(variable)),
        dt_posix = dt_posix + 3600 * Hour
    ) %>%
    dplyr::select(dt_posix, puissance) %>%
    dplyr::arrange(dt_posix)

##----------------------------
# evaluation
##----------------------------

reality <- read.csv('data/consovect.csv', header = FALSE)

rmse <- RMSE(submitDf$puissance, reality$V1)
mape <- MAPE(submitDf$puissance, reality$V1) * 100



