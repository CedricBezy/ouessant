
require(dplyr)
require(ggplot2)



# TODAY
today <- function(format = "%d %B %Y"){
    format <- as.character(format)
    f <- switch(
        EXPR = format,
        "1" = "%d %B %Y",
        "2" = "%Y%m%d",
        "3" = "%Y-%m-%d",
        "4" = "%Y.%m.%d",
        "5" = "%Y/%m/%d",
        format
    )
    res <- format(Sys.time(), format = f)
    return(res)
}


count_na <- function(x){
    sum(is.na(x))
}
prop_na <- function(x){
    mean(is.na(x))
}


plot_na <- function(df, main_title = "Missing Values", sub_title = NULL){
    df_plot <- data.frame(
        variable = colnames(df),
        nb_na = sapply(df, count_na),
        p_na = sapply(df, prop_na)
    ) %>%
        dplyr::filter(nb_na >= 1)
    
    resbarplot <- ggplot(data = df_plot) +
        geom_bar(
            mapping = aes(variable, p_na, fill = p_na),
            stat = "identity",
            col = "black"
        ) +
        guides(fill = FALSE) +
        scale_x_discrete(
            name = NULL
        ) +
        scale_y_continuous(
            name = "Prop NA",
            breaks = seq(0, 1, 0.1),
            limits = c(0, 1)
        ) +
        ggtitle(main_title, sub_title) +
        theme_bw()
    return(resbarplot)
}



