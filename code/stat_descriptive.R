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
source('code/utils.R')

x <- conso_train$dt_posix
y <- conso_train$puissance
plot(x, y)

plot_na(meteo_train, subtitle = "Meteo Train")
plot_na(meteo_prev, subtitle = "Meteo Prev")




dfconso <- conso_train %>%
    dplyr::mutate(
        Weekday = wday(dt_posix, label = TRUE, abbr = FALSE),
        Day = day(dt_posix),
        Month = month(dt_posix, label = TRUE, abbr = FALSE),
        Year = year(dt_posix),
        Hour = hour(dt_posix)
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

build_plot_var <- function(ivar){
    ggplot(data = meteo_train) +
        facet_wrap(~Year+Month, scales = "free_x", nrow = 3) +
        geom_line(
            mapping = aes_string('Day', ivar, col = 'as.factor(Hour)'),
            na.rm = TRUE
        ) +
        scale_color_hue(name = 'Hour', h.start = 100, direction = -1) +
        ggtitle(sprintf("%s selon mois et heure", ivar))
}


x_vars <- c("temp", "pression")

# x_vars <- c(
#     "temp", "pression", "hr",  "p_ros", "visi",
#     "vt_moy", "vt_raf", "vt_dir", "rr_3h", "neige", "nebul"
# )

build_plot_var(
    "temp",
    h.start = 100,
    direction = -1
)

plot_list <- lapply(
    x_vars,
    build_plot_var,
    h.start = 200,
    direction = -1
)
names(plot_list) <- x_vars
plot_list


