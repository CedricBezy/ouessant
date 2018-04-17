##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)

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
        Month = factor(
            month(dt_posix, label = TRUE, abbr = TRUE),
            levels = month.abb
        ),
        Hour = hour(dt_posix)
    ) %>%
    dplyr::arrange(dt_posix)

##==================================================
# make Test Df
##==================================================

prevDf_withna <- meteo_prev  %>%
    dplyr::select(-date_time_utc, -neige) %>%
    dplyr::filter(dt_posix <= as.POSIXct("2016-09-20 23:00:00")) %>%
    dplyr::mutate(
        Hour = hour(dt_posix),
        Month = factor(
            month(dt_posix, label = TRUE, abbr = TRUE),
            levels = month.abb
        )
    ) %>%
    dplyr::arrange(dt_posix)

##==================================================
# Center and Scale + replace NA
##==================================================

x_vars <- c(
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

## les valeurs des variables x sont reparties dans une seule colonne "value"
train_gth <- trainDf_withna %>%
    tidyr::gather_("variable", "value", x_vars)

## Maintenant, pour chaque couple ([temps], variable, value), on peut obtenir la moy et le std
## ici, temps correspond à chaque heure de chaque 15zaine du mois. 
train_summa <- train_gth %>%
    dplyr::group_by(variable, Month, Hour) %>%
    dplyr::summarise(
        nb_na = count_na(value),
        Mean = mean(value, na.rm = TRUE),
        Std = sd(value, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
        Std = replace(Std, is.nan(Std), 1),
        Std = replace(Std, Std == 0, 1)
    )

## Maintenant, on peut centrer, reduire
## Center and Scale X
trainDf <- dplyr::left_join(
    train_gth,
    train_summa,
    by = c("variable", "Month", "Hour")
) %>%
    dplyr::mutate(
        variable = factor(variable, unique(train_gth$variable)),
        value = ifelse(
            ## On remplace les valeurs manquantes par 0, et on centre et réduit les autres
            !is.na(value),
            ifelse(!is.na(Mean), (value - Mean) / Std, value),
            0
        ),
        nb_na = NULL,
        Mean = NULL,
        Std = NULL
    ) %>%
    dplyr::arrange(variable, dt_posix) %>%
    tidyr::spread(variable, value)



prevDf <- prevDf_withna %>%
    tidyr::gather_("variable", "value", x_vars) %>%
    dplyr::left_join(
        train_summa,
        by = c("variable", "Month", "Hour")
    ) %>%
    dplyr::mutate(
        variable = factor(variable, unique(train_gth$variable)),
        value = ifelse(
            !is.na(value),
            ifelse(!is.na(Mean), (value - Mean) / Std, value),
            0
        ),
        nb_na = NULL,
        Mean = NULL,
        Std = NULL
    ) %>%
    dplyr::arrange(variable, dt_posix) %>%
    tidyr::spread(variable, value)

## Verif
x_verif <- trainDf %>%
    tidyr::gather_("variable", "value", x_vars) %>%
    dplyr::group_by(Month, Hour) %>%
    dplyr::summarise(
        Mean = mean(value),
        Std = sd(value)
    )

## Verif
y_verif <- prevDf %>%
    tidyr::gather_("variable", "value", x_vars) %>%
    dplyr::group_by(Month, Hour) %>%
    dplyr::summarise(
        Mean = mean(value),
        Std = sd(value)
    )

##==================================================
# Center and Scale + replace NA
##==================================================

save(
    trainDf, prevDf, x_vars, sample_solution,
    file = "ouessant_copy/data/train_prev_BY_month.RData"
)

