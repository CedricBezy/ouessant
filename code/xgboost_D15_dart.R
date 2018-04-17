
rm(list = ls())
library(xgboost)
library(dplyr)
library(lubridate)
library(magrittr)
library(FactoMineR)


Sys.setlocale("LC_TIME", "English_United States")

##==================================================
# import data
##==================================================
# setwd('C:/Users/rmouster/Documents/perso/sfds')

source('ouessant_copy/code/functions.R')

load('ouessant_copy/data/train_prev_BY_d15.RData')

Ytrain <- trainDf[c('P0', 'DP1', 'DP2')]

Xtrain <- trainDf[c('Hour', x_vars)]
Xtrain <- cbind(
    Xtrain,
    tab.disjonctif(trainDf$Month),
    tab.disjonctif(trainDf$D15)
)

Xprev <- prevDf[c('Hour', x_vars)]
Xprev <- cbind(
    Xprev,
    tab.disjonctif(prevDf$Month),
    tab.disjonctif(prevDf$D15)
)

bst0 <- xgboost(
    data = as.matrix(Xtrain),
    label = Ytrain$P0,
    max_depth = 4,
    eta = 1,
    nrounds = 500,
    params = list(booster = "dart"),
    objective = "reg:linear"
)

bst1 <- xgboost(
    data = as.matrix(Xtrain),
    label = Ytrain$DP1,
    max_depth = 4,
    eta = 1,
    nrounds = 500,
    params = list(booster = "dart"),
    objective = "reg:linear"
)

bst2 <- xgboost(
    data = as.matrix(Xtrain),
    label = Ytrain$DP2,
    max_depth = 4,
    eta = 1,
    nrounds = 500,
    params = list(booster = "dart"),
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

lines(submitDf)

RMSE <- sqrt(mean((submitDf$puissance - sample_solution$V1) ^ 2))


