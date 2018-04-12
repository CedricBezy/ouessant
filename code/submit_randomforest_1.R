##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(randomForest)

Sys.setlocale("LC_TIME", "English_United States")

##==================================================
# import data
##==================================================

source('ouessant_copy/code/functions.R')

load('ouessant_copy/data/cleaned_data.RData')


##==================================================
# make Train Df
##==================================================

consoDf <- conso_train %>%
    dplyr::select(dt_posix, puissance) %>%
    dplyr::mutate(
        Hour = hour(dt_posix),
        H3 = 3 * (Hour %/% 3),
        R3 = Hour %% 3,
        vpuiss = factor(paste0("P", R3), levels = paste0("P", 0:2))
    )
consoDf$dt_posix <- consoDf$dt_posix - 3600 * consoDf$R3

consoGth <- consoDf %>%
    dplyr::select(-Hour, -H3, -R3) %>%
    tidyr::spread(vpuiss, puissance) %>%
    dplyr::mutate(
        DP1 = P1 - P0,
        DP2 = P2 - P0
    ) %>%
    dplyr::select(-P1, -P2)


meteoDf <- meteo_train %>%
    dplyr::select(-date_time_utc)

trainDf_withna <- dplyr::right_join(consoGth, meteoDf, by = c("dt_posix")) %>%
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

testDf_withna <- meteo_prev  %>%
    dplyr::filter(dt_posix <= as.POSIXct("2016-09-20 23:00:00")) %>%
    dplyr::mutate(
        date_time_utc = NULL,
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

train_gth <- trainDf_withna %>%
    dplyr::mutate(
        D15 = factor(
            day(dt_posix) >= 15,
            levels = c(FALSE, TRUE),
            labels = c("Qz1", "Qz2")
        )
    ) %>%
    tidyr::gather_("variable", "value", x_vars)


test_gth <-  testDf_withna %>%
    dplyr::mutate(
        D15 = factor(
            day(dt_posix) >= 15,
            levels = c(FALSE, TRUE),
            labels = c("Qz1", "Qz2")
        )
    ) %>%
    tidyr::gather_("variable", "value", x_vars)



train_summa <- train_gth %>%
    dplyr::group_by(variable, Month, D15, Hour) %>%
    dplyr::summarise(
        nb_na = count_na(value),
        Mean = mean(value, na.rm = TRUE),
        Std = sd(value, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
        Std = replace(Std, is.nan(Std), 1),
        Std = replace(Std, Std == 0, 1)
    )

sum(is.na(train_summa$Mean))



## Center and Scale X

trainDf <- dplyr::left_join(
    train_gth,
    train_summa,
    by = c("variable", "Month", "D15", "Hour")
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



testDf <- dplyr::left_join(
    test_gth,
    train_summa,
    by = c("variable", "Month", "D15", "Hour")
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
    dplyr::group_by(Month, D15, Hour) %>%
    dplyr::summarise(
        Mean = mean(value),
        Std = sd(value)
    )

## Verif
y_verif <- testDf %>%
    tidyr::gather_("variable", "value", x_vars) %>%
    dplyr::group_by(Month, D15, Hour) %>%
    dplyr::summarise(
        Mean = mean(value),
        Std = sd(value)
    )

rf_vars <- c(x_vars, "Hour", "Month")
form0 <- as.formula(paste("P0 ~", paste(rf_vars, collapse = " + ")))
form1 <- as.formula(paste("DP1 ~", paste(rf_vars, collapse = " + ")))
form2 <- as.formula(paste("DP2 ~", paste(rf_vars, collapse = " + ")))

rf0 <- randomForest(form0, data = trainDf)
p0_pred <- predict(rf0, newdata = testDf)

rf1 <- randomForest(form1, data = trainDf)
dp1_pred <- predict(rf1, newdata = testDf)
p1_pred <- p0_pred + dp1_pred

rf2 <- randomForest(form2, data = trainDf)
dp2_pred <- predict(rf2, newdata = testDf)
p2_pred <- p0_pred + dp2_pred


submitDf <- data.frame(
    dt_posix = testDf$dt_posix,
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

RMSE <- sqrt(mean((submitDf$puissance - sample_solution$V1) ^ 2))
