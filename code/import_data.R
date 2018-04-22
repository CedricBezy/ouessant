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
library(circular)

# Set Option : Date in English
Sys.setlocale("LC_TIME", "English_United States")

completePath <- function(path, ...){
    path_proj <- "D:/Documents/PROJETS/Kaggle/ouessant/ouessant_copy"
    complete_path <- sprintf(path, path_proj, ...)
    return(complete_path)
}

##=====================================================
# Importation et Nettoyage des Donnees
##=====================================================


path_files <- completePath("data")

# Importation de conso_train
conso_train <- read.csv2(
    file = sprintf("%s/conso_train.csv", path_files),
    dec = ".",
    stringsAsFactors = FALSE
)

## Importation de meteo_train
meteo_train <- read.csv2(
    file = sprintf("%s/meteo_train.csv", path_files),
    dec = ".",
    stringsAsFactors = FALSE
)

## Importation de meteo_prev
meteo_prev <- read.csv2(
    file = sprintf("%s/meteo_prev.csv", path_files),
    dec = ".",
    stringsAsFactors = FALSE
)

##=====================================================
# Nettoyage des Donnees
##=====================================================
##--------------------------------
# noms de colonnes
##--------------------------------

colnames(conso_train) <- c('datetime', 'puissance')

colnames(meteo_train)
# [1] "date_utc"             "temp"                 "P..hPa."             
# [4] "HR...."               "P.rosâ.šÂ.e..Â.â.žC." "Visi..km."           
# [7] "Vt..moy...km.h."      "Vt..raf...km.h."      "Vt..dir..Â.â.ž."     
# [10] "RR.3h..mm."           "Neige..cm."           "Nebul...octats."

## rename meteo_train
newnames <- c(
    "datetime", "temp", "pression", "hr", "p_rosee",
    "visi", "vt_moy", "vt_raf", "vt_dir", "rr_3h",
    "neige", "nebul"
)
colnames(meteo_train) <- newnames
colnames(meteo_prev) <- newnames

##--------------------------------
# supprimer les doublons
##--------------------------------

## supprimer les doublons dans conso_train
dup_conso <- which(duplicated(conso_train))
conso_train <- conso_train[-dup_conso,]
print(sprintf("conso_train : %i lignes supprimées !", length(dup_conso)))

## supprimer les doublons dans meteo_train
dup_meteo <- which(duplicated(meteo_train))
meteo_train <- meteo_train[-dup_meteo,]
print(sprintf("meteo_train : %i lignes supprimées !", length(dup_meteo)))

##--------------------------------
# format des variables (date et vt_dir)
##--------------------------------
# dates
conso_train$datetime[1]
# [1] "2015-09-13T00:59:59+02:00"

conso_dt <- strsplit(conso_train$datetime, split = "\\+")
## Extraire la date
conso_dt_format <- as.POSIXct(strptime(
    sapply(conso_dt, function(x){x[1]}),
    format = "%Y-%m-%dT%H:%M:%S"
))
## Enfin, on arrondit la date
conso_train$datetime <- round_date(conso_dt_format, "hour")
## fuseau horaire
conso_train$fuseau <- paste0("+", sapply(conso_dt, function(x){x[2]}))

## dans meteo train
dt_meteotrain <- as.POSIXct(strptime(
    meteo_train$datetime,
    format = "%d/%m/%y %Hh%M"
))
meteo_train <- meteo_train %>%
    dplyr::mutate(
        datetime = dt_meteotrain,
        vt_dir = circular(vt_dir, type = "direction", units = "degrees", zero = 0)
    )

## dans meteo pred
dt_meteoprev <- as.POSIXct(strptime(
    meteo_prev$datetime,
    format = "%d/%m/%y %Hh%M"
))
meteo_prev <- meteo_prev  %>%
    dplyr::mutate(
        datetime = dt_meteoprev,
        vt_dir = circular(vt_dir, type = "direction", units = "degrees", zero = 0)
    )

##--------------------------------
# ajout de variables
##--------------------------------

meteo_train <- meteo_train %>%
    dplyr::mutate(
        Weekday = wday(datetime, label = TRUE, abbr = FALSE),
        Day = day(datetime),
        Month = month(datetime, label = TRUE, abbr = FALSE),
        Year = year(datetime),
        Hour = hour(datetime)
    )

meteo_prev <- meteo_prev %>%
    dplyr::mutate(
        Weekday = wday(datetime, label = TRUE, abbr = FALSE),
        Day = day(datetime),
        Month = month(datetime, label = TRUE, abbr = FALSE),
        Year = year(datetime),
        Hour = hour(datetime)
    )


##=================================================
# ajout de variables
##=================================================

jours_feries <- c("01/01", "01/05", "08/05", "14/07", "15/08", "01/11", "25/12")

meteo_train <- meteo_train %>%
    dplyr::mutate(
        rose_vt = cut(
            replace(vt_dir, vt_dir >= 315, 0),
            breaks = c(0, seq(45, 315, by = 90)),
            labels = c("North", "East", "South", "West"),
            right = FALSE
        ),
        f_weekend = Weekday %in% c("Saturday", "Sunday"),
        f_ferie = as.numeric(mapply(
            all,
            format(datetime, "%d/%m") %in% jours_feries,
            f_weekend
        )),
        f_season = factor(
            ifelse(month(datetime) %in% c(4:10), "summer", "winter"),
            levels = c("summer", "winter")
        ),
        f_evening = as.numeric(
            ifelse(month(datetime) %in% c(4:10), Hour >= 21, Hour >= 18)
        ),
        f_midnight = as.numeric(Hour <= 2),
        f_hour = factor(Hour, seq(0, 21, 3), paste0("H", seq(0, 21, 3)))
    )

summary(meteo_train)

meteo_prev <- meteo_prev %>%
    dplyr::mutate(
        rose_vt = cut(
            replace(vt_dir, vt_dir >= 315, 0),
            breaks = c(0, seq(45, 315, by = 90)),
            labels = c("North", "East", "South", "West"),
            right = FALSE
        ),
        f_weekend = Weekday %in% c("Saturday", "Sunday"),
        f_ferie = as.numeric(mapply(
            all,
            format(datetime, "%d/%m") %in% jours_feries,
            f_weekend
        )),
        f_season = factor(
            ifelse(month(datetime) %in% c(4:10), "summer", "winter"),
            levels = c("summer", "winter")
        ),
        f_evening = as.numeric(
            ifelse(month(datetime) %in% c(4:10), Hour >= 21, Hour >= 18)
        ),
        f_midnight = as.numeric(Hour <= 2),
        f_hour = factor(Hour, seq(0, 21, 3), paste0("H", seq(0, 21, 3)))
    )


##========================================================
# save
##========================================================

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
    dplyr::select(datetime, puissance) %>%
    dplyr::mutate(
        Hour = hour(datetime),
        H3 = 3 * (Hour %/% 3),
        R3 = Hour %% 3,
        vpuiss = factor(paste0("P", R3), levels = paste0("P", 0:2)),
        datetime = datetime - 3600 * R3
    ) %>%
    dplyr::select(-Hour, -H3, -R3) %>%
    tidyr::spread(vpuiss, puissance) %>%
    dplyr::mutate(
        DP1 = P1 - P0,
        DP2 = P2 - P0,
        DP12 = P2 - P1
    ) %>%
    dplyr::select(-P1, -P2)


# meteoDf : meteo without useless data
data_train <- consoDf %>%
    dplyr::right_join(meteo_train, by = c("datetime")) %>%
    dplyr::arrange(datetime) %>%
    dplyr::filter(!is.na(P0), !is.na(DP1), !is.na(DP2), !is.na(DP12))

data_prev <- meteo_prev %>%
    dplyr::filter(datetime <= as.POSIXct("2016-09-20 23:00:00")) %>%
    dplyr::arrange(datetime)

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



