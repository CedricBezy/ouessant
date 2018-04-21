
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
            "p_ros",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h",
            "nebul"
        ),
        others = c('Hour')
    ),
    `2` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_ros",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour')
    ),
    `3` = list(
        factors = c(
            "Month"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_ros",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour')
    ),
    `4` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_ros",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour', 'f_weekend')
    ),
    `5` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_ros",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour', 'f_weekend', 'f_evening')
    ),
    `6` = list(
        factors = c(
            "Month"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "p_ros",
            "visi",
            "vt_moy",
            "vt_raf",
            "vt_dir",
            "rr_3h"
        ),
        others = c('Hour', 'f_weekend')
    ),
    `7` = list(
        factors = c(
            "Month",
            "rose_vt"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "visi",
            "p_ros",
            "vt_moy",
            "vt_raf",
            "rr_3h"
        ),
        others = c('Hour', 'f_weekend')
    ),
    `7` = list(
        factors = c(
            "Month",
            "Weekday"
        ),
        numerics = c(
            "temp",
            "pression",
            "hr",
            "visi",
            "p_ros",
            "vt_moy",
            "vt_raf",
            "rr_3h"
        ),
        others = c('Hour', "vt_north", "vt_east", "vt_south", "vt_west")
    )
)
