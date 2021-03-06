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
load(completePath('data/cleaned_data.RData'))
load(completePath('data/train_prev.RData'))
source(completePath('%s/code/utils.R'))
source(completePath('%s/code/scenarios.R'))

trainDf <- data_train
prevDf <- data_prev
scenario <- scenarios$`1`

list[trainDf, prevDf, varnums] <- deal_train_test_numerics(
    trainDf,
    prevDf,
    variables = scenario$numerics,
    na_replace = TRUE,
    center = TRUE,
    scale = TRUE
)

list[trainDf, prevDf, varfacts] <- deal_train_test_factors(
    train = trainDf,
    test = prevDf,
    variables = scenario$factors
)

columns <- c(varnums, varfacts, scenario$others)


##==================================================
# Xscale
##==================================================

reality <- read.csv('data/consovect.csv', header = FALSE)

Ytrain <- trainDf[c('P0', 'DP1', 'DP2')]

Xtrain <- trainDf[columns]
Xprev <- prevDf[columns]

puiss_prev <- xgboost_predict(
    Ytrain, Xtrain, Xprev,
    nrounds = 1000,
    objective = "reg:linear",
    eta = 0.01,
    max_depth = 15,
    min_child_weight = 1,
    booster = "gbtree",
    normalize_type = 'forest'
)

puiss_prev$dt_posix <- prevDf$dt_posix

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
    dplyr::arrange(dt_posix) %>%
    tibble::add_column(
        reality = reality$V1,
        .after = "puissance"
    )

## Evaluation
rmse <- RMSE(submitDf$puissance, submitDf$reality)
mape <- MAPE(submitDf$puissance, submitDf$reality) * 100

print(mape)



