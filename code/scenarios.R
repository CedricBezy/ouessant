
scenarios <- list(
    `1` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h",
            "nebul"
        ),
        others = c('Hour')
    ),
    ## Par rapport au scenario 1 :
    ## suppression de la variable "nebul"
    `2` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour')
    ),
    ## Par rapport au scenario 2 :
    ## Suppression de la variable Weekday
    `3` = list(
        factors = c(
            "Month"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour')
    ),
    ## Par rapport aux scenarios 2 / 3 :
    ## - retablissement de la variable "weekday"
    ## - ajout du facteur week-end
    `4` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h",
            "nebul"
        ),
        others = c('Hour', 'f_weekend')
    ),
    ## Par rapport aux scenarios 4 :
    ## - ajout du facteur evening
    `5` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h",
            "nebul"
        ),
        others = c('Hour', 'f_weekend', 'f_evening')
    ),
    ## Par rapport au scenario 6 :
    ## - remplacement de la direction du vent par un facteur
    `6` = list(
        factors = c(
            "Month",
            "Weekday",
            "rose_vt"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_moy",
            "vt_raf",
            "rr_3h",
            "nebul"
        ),
        others = c('Hour', "f_weekend", "f_evening")
    ),
    ## Par rapport au scenario 6 :
    ## - enlever le jour de la semaine
    `7` = list(
        factors = c(
            "Month",
            "rose_vt",
            "Weekday",
            "f_season"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_raf",
            "rr_3h",
            "nebul"
        ),
        others = c('Hour', "f_weekend")
    ),
    `8` = list(
        factors = c(
            "Month",
            "rose_vt",
            "f_weekend",
            'Hour'
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_rosee",
            "visi",
            "vt_raf",
            "rr_3h",
            "nebul"
        )
    )
)
