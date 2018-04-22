##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================


library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(magrittr)

rm(list = ls())
load('data/cleaned_data.RData')
load('data/train_prev.RData')
source('code/utils.R')

x <- conso_train$datetime
y <- conso_train$puissance
plot(x, y)

plot_na(meteo_train, subtitle = "Meteo Train")
plot_na(meteo_prev, subtitle = "Meteo Prev")




dfconso <- conso_train %>%
    dplyr::mutate(
        Weekday = wday(datetime, label = TRUE, abbr = FALSE),
        Day = day(datetime),
        Month = month(datetime, label = TRUE, abbr = FALSE),
        Year = year(datetime),
        Hour = hour(datetime)
    )

ggplot(data = dfconso) +
    geom_line(
        mapping = aes(
            x = Day,
            y = puissance,
            col= as.factor(Hour)
        )
    ) +
    facet_wrap(~Year+Month, nrow = 3) +
    scale_color_hue(name = 'Hour', h.start = 100, direction = -1) +
    guides(col = FALSE) +
    ggtitle("Consommation d'electricite par mois et par heure")


dfconso_summa <- dfconso %>%
    dplyr::group_by(Month, Hour) %>%
    dplyr::summarise(
        n = n(),
        moy_puiss = mean(puissance),
        std_puiss = sd(puissance)
    )

ggplot(data = dfconso_summa,
       mapping = aes(
           x = Hour,
           y = moy_puiss,
           col = Month
       )) +
    geom_point() +
    geom_line() +
    scale_x_continuous(
        name = "Hour",
        breaks = seq(0, 23, 2)
    ) +
    ggtitle("Consommation d'electricite moyenne par mois et par heure")

build_plot_train <- function(ivar){
    ggplot(data = meteo_train) +
        facet_wrap(~Year+Month, scales = "free_x", nrow = 3) +
        geom_line(
            mapping = aes_string('Day', ivar, col = 'as.factor(Hour)'),
            na.rm = TRUE
        ) +
        scale_color_hue(name = 'Hour', h.start = 100, direction = -1) +
        ggtitle(sprintf("%s selon mois et heure (train)", ivar))
}
build_plot_test <- function(ivar){
    ggplot(data = meteo_prev) +
        geom_line(
            mapping = aes_string('Hour', ivar, col = 'as.factor(Day)'),
            na.rm = TRUE
        ) +
        scale_color_hue(name = 'Day', h.start = 100, direction = -1) +
        ggtitle(sprintf("%s selon mois et heure (test)", ivar))
}

x_vars <- c(
    "temp", "pression", "hr",  "p_ros", "visi",
    "vt_moy", "vt_raf", "vt_dir", "rr_3h", "nebul"
)

plot_list_train <- lapply(
    x_vars,
    build_plot_train
)
names(plot_list_train) <- x_vars
plot_list_train

plot_list_test <- lapply(
    x_vars,
    build_plot_test
)
names(plot_list_test) <- x_vars
plot_list_test




