##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(gsubfn)
library(sqldf)

Sys.setlocale("LC_TIME", "English_United States")

##==================================================
# import data
##==================================================

source('ouessant_copy/code/functions.R')

load('ouessant_copy/data/cleaned_data.RData')


##==================================================
# make Train Df
##==================================================

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

meteoDf <- meteo_train %>%
    dplyr::select(-date_time_utc, -neige)

# meteoDf : meteo without useless data
trainDf_withna <- consoDf %>%
    dplyr::right_join(meteoDf, by = c("dt_posix")) %>%
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

prevDf_withna <- meteo_prev  %>%
    dplyr::select(-date_time_utc, -neige) %>%
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
# make Test Df
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

list[trainDf, prevDf, x_vars] <- make_train_test_data(trainDf_withna, prevDf_withna, x_vars)



##==================================================
# Center and Scale + replace NA
##==================================================

save(
    trainDf, prevDf, x_vars, sample_solution,
    file = "ouessant_copy/data/train_prev_BY_month.RData"
)

