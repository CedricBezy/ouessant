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

# Set Option : Date in English
Sys.setlocale("LC_TIME", "English_United States")

completePath <- function(path, ...){
    path_proj <- "D:/Documents/PROJETS/Kaggle/ouessant/ouessant_copy"
    complete_path <- sprintf(path, path_proj)
    return(complete_path)
}

##=====================================================
# Importation et Nettoyage des Donnees
##=====================================================
##--------------------------------
# CONSO TRAIN
##--------------------------------
conso_train <- read.csv2(
    file = completePath("%s/data/conso_train.csv"),
    dec = ".",
    stringsAsFactors = FALSE
)

colnames(conso_train) <- c('date_time', 'puissance')


## delete doublons
conso_train <- conso_train[-which(duplicated(conso_train)),]

# dates
conso_train$date_time[1]
# [1] "2015-09-13T00:59:59+02:00"

date_conso <- strsplit(conso_train$date_time, split = "\\+")
dt <- strptime(
    sapply(date_conso, function(x){x[1]}),
    format = "%Y-%m-%dT%H:%M:%S"
)
conso_train <- conso_train %>%
    tibble::add_column(
        fuseau_hor = paste0("+", sapply(date_conso, function(x){x[2]})),
        dt_posix = as.POSIXct(dt),
        .after = "date_time"
    )

## round posix
conso_train$dt_posix <- round_date(conso_train$dt_posix, "hour")

##--------------------------------
# meteo train
##--------------------------------
meteo_train <- read.csv2(
    file = completePath("%s/data/meteo_train.csv"),
    dec = "."
)

colnames(meteo_train)
# [1] "date_utc"             "temp"                 "P..hPa."             
# [4] "HR...."               "P.rosâ.šÂ.e..Â.â.žC." "Visi..km."           
# [7] "Vt..moy...km.h."      "Vt..raf...km.h."      "Vt..dir..Â.â.ž."     
# [10] "RR.3h..mm."           "Neige..cm."           "Nebul...octats."

## rename meteo_train
newnames <- c(
    "ï..Date.UTC" = "date_time_utc",
    "TÂ.â.ž..C." =  "temp",
    "P..hPa." = "pression",
    "HR...." = "hr",
    "P.rosâ.šÂ.e..Â.â.žC." = "p_ros",
    "Visi..km." = "visi",
    "Vt..moy...km.h." = "vt_moy",
    "Vt..raf...km.h." = "vt_raf",
    "Vt..dir..Â.â.ž." = "vt_dir",
    "RR.3h..mm." = "rr_3h",
    "Neige..cm." = "neige",
    "Nebul...octats."  = "nebul"
)
colnames(meteo_train) <- newnames

## delete doublons
meteo_train <- meteo_train[-which(duplicated(meteo_train)),]

dt_train <- strptime(
    meteo_train$date_time_utc,
    format = "%d/%m/%y %Hh%M"
)
meteo_train <- meteo_train %>%
    tibble::add_column(
        dt_posix = as.POSIXct(dt_train),
        .after = "date_time_utc"
    ) %>%
    dplyr::select(-date_time_utc)

##--------------------------------
# meteo prev
##--------------------------------
meteo_prev <- read.csv2(
    file = completePath("%s/data/meteo_prev.csv"),
    dec = "."
)
colnames(meteo_prev) <- newnames

dt_prev <- strptime(
    meteo_prev$date_time_utc,
    format = "%d/%m/%y %Hh%M"
)
meteo_prev <- meteo_prev %>%
    tibble::add_column(
        dt_posix = as.POSIXct(dt_prev),
        .after = "date_time_utc"
    ) %>%
    dplyr::select(-date_time_utc)

##--------------------------------
# save
##--------------------------------
save(
    conso_train, meteo_train, meteo_prev,
    file = completePath("%s/data/cleaned_data.RData")
)
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

##--------------------------------
# save
##--------------------------------
save(
    data_train, data_prev,
    file = completePath("%s/data/train_prev.RData")
)

##=======================================
# write csv
##=======================================

if(FALSE){
    write.csv2(
        data_train,
        file = completePath("%s/data/data_train.csv"),
        row.names = FALSE
    )
    write.csv2(
        data_prev,
        file = completePath("%s/data/data_prev.csv"),
        row.names = FALSE
    )
    write.csv2(
        conso_train,
        file = completePath("%s/data/cleaned_conso_train.csv"),
        row.names = FALSE
    )
    write.csv2(
        meteo_train,
        file = sprintf("%s/data/cleaned_meteo_train.csv", path_files),
        row.names = FALSE
    )
    write.csv2(
        meteo_prev,
        file = sprintf("%s/data/cleaned_meteo_prev.csv", path_files),
        row.names = FALSE
    )
}



