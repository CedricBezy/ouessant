
rm(list = ls())
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(randomForest)
library(FactoMineR)


Sys.setlocale("LC_TIME", "English_United States")

##==================================================
# import data
##==================================================
setwd('C:/Users/rmouster/Documents/perso/sfds')

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

trainP0<-trainDf$P0
trainDf$P0<-NULL
trainDf$DP1<-NULL
trainDf$DP2<-NULL
trainDf$dt_posix<-NULL
testDf$dt_posix<-NULL
trainDf<-cbind(trainDf,tab.disjonctif(trainDf$Month))
testDf<-cbind(testDf,tab.disjonctif(testDf$Month))
testDf$Month<-NULL
trainDf$Month<-NULL
trainDf<-cbind(trainDf,tab.disjonctif(trainDf$D15))
testDf<-cbind(testDf,tab.disjonctif(testDf$D15))
testDf$D15<-NULL
trainDf$D15<-NULL
testDf$neige<-NULL
trainDf$neige<-NULL


bst <- xgboost(data = as.matrix(trainDf), label = trainP0, max_depth = 2, eta = 1,
               nrounds = 3000, objective = "reg:linear")
xgb.save(bst, 'model.save')
bst = xgb.load('model.save')
pred <- predict(bst, as.matrix(testDf))
