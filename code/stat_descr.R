##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================


library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)

rm(list = ls())
load('ouessant_copy/data/cleaned_data.RData')
source('ouessant_copy/code/functions.R')

x <- conso_train$dt_posix
y <- conso_train$puissance
plot(x, y)

plot_na(meteo_train)
plot_na(meteo_prev)




dfconso <- conso_train %>%
    dplyr::mutate(
        year = format(dt_posix, "%Y")
    )

ggplot(data = dfconso,
       mapping = aes(
           x = dt_posix,
           y = puissance
       )) +
    facet_wrap(~year, scales = "free_x") +
    geom_point() +
    geom_line() +
    guides(col = FALSE)


dfconso_summa <- dfconso %>%
    dplyr::mutate(
        time = as.numeric(
            format(round(dt_posix, units = "hours"), "%H")
        ),
        month = factor(
            format(dt_posix, "%B"),
            levels = month.name
        )
    ) %>%
    dplyr::group_by(month, time) %>%
    dplyr::summarise(
        n = n(),
        moy_puiss = mean(puissance),
        std_puiss = sd(puissance)
    )
dfconso_summa <- bind_rows(
    dfconso_summa, 
    dfconso_summa %>%
        dplyr::filter(time == 0) %>%
        dplyr::mutate(time = 24)
)




ggplot(data = dfconso_summa,
       mapping = aes(
           x = as.numeric(time),
           y = moy_puiss,
           col = month
       )) +
    geom_point() +
    geom_line() +
    scale_x_continuous(
        name = "Hour",
        breaks = seq(0, 24, 2)
    )


