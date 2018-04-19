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
library(randomForest)

# Set Option : Date in English
Sys.setlocale("LC_TIME", "English_United States")

completePath <- function(path, ...){
    path_proj <- "D:/Documents/PROJETS/Kaggle/ouessant/ouessant_copy"
    complete_path <- sprintf(path, path_proj)
    return(complete_path)
}

source(completePath('%s/code/utils.R'))
source(completePath('%s/code/train_test_month_2.R'))
load(completePath('data/cleaned_data.RData'))

##========================================================
# creation des tables train and prev
##========================================================
##----------------------------------
# table Train : merger conso_train et conso_prev
##----------------------------------
# consoDf : contains consommation :
#   - puissance P0 for each 3 hours.
#   - puissance variation for each 3 hours + 1 (DP1 = P1 - P0)
#   - puissance variation for each 3 hours + 2 (DP1 = P1 - P0)

consoDf <- conso_train %>%
    dplyr::select(dt_posix, puissance) %>%
    dplyr::mutate(
        Hour = hour(dt_posix),
        H3 = 3 * (Hour %/% 3),
        R3 = Hour %% 3,
        vpuiss = factor(paste0("P", R3), levels = paste0("P", 0:2)),
        dt_posix = dt_posix - 3600 * R3
    ) %>%
    dplyr::select(-Hour, -H3, -R3) %>%
    tidyr::spread(vpuiss, puissance) %>%
    dplyr::mutate(
        DP1 = P1 - P0,
        DP2 = P2 - P0
    ) %>%
    dplyr::select(-P1, -P2)


# meteoDf : meteo without useless data
data_train <- consoDf %>%
    dplyr::right_join(meteo_train, by = c("dt_posix")) %>%
    dplyr::filter(!is.na(P0), !is.na(DP1), !is.na(DP2)) %>%
    dplyr::mutate(
        Weekday = wday(dt_posix, label = TRUE, abbr = FALSE),
        Month = factor(
            month(dt_posix, label = TRUE, abbr = TRUE),
            levels = month.abb
        ),
        Hour = hour(dt_posix)
    ) %>%
    dplyr::arrange(dt_posix)


data_prev <- meteo_prev %>%
    dplyr::filter(dt_posix <= as.POSIXct("2016-09-20 23:00:00")) %>%
    dplyr::mutate(
        Weekday = wday(dt_posix, label = TRUE, abbr = FALSE),
        Hour = hour(dt_posix),
        Month = factor(
            month(dt_posix, label = TRUE, abbr = TRUE),
            levels = month.abb
        )
    ) %>%
    dplyr::arrange(dt_posix)

##==================================================
# make Train and Test Df
##==================================================

x_vars <- c(
    "Month",
    "Weekday",
    "temp",
    "pression",
    "hr",
    "p_ros",
    "visi",
    "vt_moy",
    "vt_raf",
    "vt_dir",
    "rr_3h",
    "nebul"
)

list[trainDf, prevDf, x_vars] <- make_train_test_data(
    data_train,
    data_prev,
    x_vars,
    disj = FALSE
)

rf_vars <- c(x_vars, "Hour")

train0 <- trainDf[c('P0', rf_vars)]
train1 <- trainDf[c('DP1', rf_vars)]
train2 <- trainDf[c('DP2', rf_vars)]

form0 <- as.formula(paste("P0 ~", paste(rf_vars, collapse = " + ")))
form1 <- as.formula(paste("DP1 ~", paste(rf_vars, collapse = " + ")))
form2 <- as.formula(paste("DP2 ~", paste(rf_vars, collapse = " + ")))

rf0 <- randomForest(form0, data = train0)
p0_pred <- predict(rf0, newdata = prevDf)

rf1 <- randomForest(form1, data = train1)
dp1_pred <- predict(rf1, newdata = prevDf)
p1_pred <- p0_pred + dp1_pred

rf2 <- randomForest(form2, data = train2)
dp2_pred <- predict(rf2, newdata = prevDf)
p2_pred <- p0_pred + dp2_pred


submitDf <- data.frame(
    dt_posix = prevDf$dt_posix,
    P0 = p0_pred,
    P1 = p1_pred,
    P2 = p2_pred
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
mape <- MAPE(submitDf$puissance, reality$V1)
