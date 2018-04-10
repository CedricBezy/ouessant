##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(magrittr)

rm(list = ls())

ok_save <- TRUE
Sys.setlocale("LC_TIME", "English_United States")

path_files <- "ouessant_copy/data"

##=======================================
# conso train
##=======================================
conso_train <- read.csv2(
    file = sprintf("%s/conso_train.csv", path_files),
    dec = ".",
    stringsAsFactors = FALSE
)


colnames(conso_train) <- c('date_time', 'puissance')

conso_train$date_time[1]
# [1] "2015-09-13T00:59:59+02:00"

date_conso <- strsplit(conso_train$date_time, split = "\\+")
dt <- strptime(
    sapply(date_conso, function(x){x[1]}),
    format = "%Y-%m-%dT%H:%M:%S"
)
conso_train <- conso_train %>%
    tibble::add_column(
        dt_posix = as.POSIXct(dt),
        fuseau_hor = sapply(date_conso, function(x){x[2]}),
        .after = "date_time"
    )


##=======================================
# meteo train
##=======================================

meteo_train <- read.csv2(
    file = sprintf("%s/meteo_train.csv", path_files),
    dec = "."
)

colnames(meteo_train)
# [1] "date_utc"             "temp"                 "P..hPa."             
# [4] "HR...."               "P.rosâ.šÂ.e..Â.â.žC." "Visi..km."           
# [7] "Vt..moy...km.h."      "Vt..raf...km.h."      "Vt..dir..Â.â.ž."     
# [10] "RR.3h..mm."           "Neige..cm."           "Nebul...octats."

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
    
# meteo_train <- meteo_train %>%
#     dplyr::rename(
#         'date_utc' = ï..Date.UTC,
#         'temp' = TÂ.â.ž..C.,
#         'pression' = P..hPa.,
#         'hr' = HR....,                 
#         'p_ros' = P.rosâ.šÂ.e..Â.â.žC.,
#         'visi' = Visi..km.,            
#         'vt_moy' = Vt..moy...km.h.,
#         'vt_raf' = Vt..raf...km.h.,    
#         'vt_dir' = Vt..dir..Â.â.ž.,
#         'rr_3h' = RR.3h..mm.,          
#         'neige' = Neige..cm.,
#         'nebul' = Nebul...octats.   
#     )


dt_train <- strptime(
    meteo_train$date_time_utc,
    format = "%d/%m/%y %Hh%M"
)
meteo_train <- meteo_train %>%
    tibble::add_column(
        dt_posix_utc = as.POSIXct(dt_train),
        .after = "date_time_utc"
    )


##=======================================
# meteo_prev
##=======================================
meteo_prev <- read.csv2(
    file = sprintf("%s/meteo_prev.csv", path_files),
    dec = "."
)
colnames(meteo_prev) <- newnames

dt_prev <- strptime(
    meteo_prev$date_time_utc,
    format = "%d/%m/%y %Hh%M"
)
meteo_prev <- meteo_prev %>%
    tibble::add_column(
        dt_posix_utc = as.POSIXct(dt_prev),
        .after = "date_time_utc"
    )
##=======================================
# sample solution
##=======================================
sample_solution <- read.csv2(
    file = sprintf("%s/sample_solution.csv", path_files),
    dec = ".",
    header = FALSE
)

##=======================================
# save
##=======================================

save(
    conso_train, meteo_train, meteo_prev, sample_solution,
    file = sprintf("%s/cleaned_data.RData", path_files)
)

##=======================================
# write csv
##=======================================

if(ok_save){
    write.csv2(
        conso_train,
        file = sprintf("%s/cleaned_conso_train.csv", path_files),
        row.names = FALSE
    )
    write.csv2(
        meteo_train,
        file = sprintf("%s/cleaned_meteo_train.csv", path_files),
        row.names = FALSE
    )
    write.csv2(
        meteo_prev,
        file = sprintf("%s/cleaned_meteo_prev.csv", path_files),
        row.names = FALSE
    )
}



