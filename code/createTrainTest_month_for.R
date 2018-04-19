##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(gsubfn)
library(sqldf)

Sys.setlocale("LC_TIME", "English_United States")

##==================================================
# import data
##==================================================

source('ouessant_copy/code/functions.R')

load('ouessant_copy/data/cleaned_data.RData')


##==================================================
# make Train Df
##==================================================

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

meteoDf <- meteo_train %>%
    dplyr::select(-date_time_utc, -neige)

# meteoDf : meteo without useless data
trainDf_withna <- consoDf %>%
    dplyr::right_join(meteoDf, by = c("dt_posix")) %>%
    dplyr::filter(!is.na(P0), !is.na(DP1), !is.na(DP2)) %>%
    dplyr::mutate(
        Month = factor(
            month(dt_posix, label = TRUE, abbr = TRUE),
            levels = month.abb
        ),
        Hour = hour(dt_posix)
    ) %>%
    dplyr::arrange(dt_posix)

prevDf_withna <- meteo_prev  %>%
    dplyr::select(-date_time_utc, -neige) %>%
    dplyr::filter(dt_posix <= as.POSIXct("2016-09-20 23:00:00")) %>%
    dplyr::mutate(
        Hour = hour(dt_posix),
        Month = factor(
            month(dt_posix, label = TRUE, abbr = TRUE),
            levels = month.abb
        )
    ) %>%
    dplyr::arrange(dt_posix)


##==================================================
# make Test Df
##==================================================

x_vars <- c(
    "Month",
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



trainDf <- trainDf_withna
testDf <- prevDf_withna
sqldf('select distinct(nebul) from trainDf where Month = "Aug" and Hour = 3')
center <- TRUE
scale <- TRUE

make_train_test_data <- function(
    trainDf,
    testDf,
    x_vars,
    center = TRUE,
    scale = TRUE, 
    disj = TRUE
){
    ##-------------------------------.
    # cette fonction met en place les tables train et test :
    # pour les variables numeriques referencees dans x_vars:
    #   - on calcule la moyenne et l'ecart-type par mois et par semaine
    #   - les valeurs manquantes sont remplacees par la moyenne
    #   - les variables sont centrees reduites
    # pour les variables qualitatives :
    #   - on conservera les variables indicatrives
    # note : cette fonction a ete creee de maniere a ne pas utiliser de boucle "for"
    # peu optimisees sous R.
    ##-------------------------------.
    
    # Etape 1 : separer variables numeriques et facteurs
    ok_num <- sapply(trainDf[x_vars], is.numeric)
    x_vars_num <- setdiff(names(which(ok_num)), 'Hour')
    x_vars_fact <- names(which(!ok_num))
    save_vars <- x_vars
    
    # Etape 2 :
    # les valeurs des variables references dans x_vars
    # sont reparties dans deux colonnes (variable, value)
    # variable est le nom de la variable, value le nom de la value
    # les autres informations sont conservees pour chaque ligne
    train_values <- trainDf %>%
        tidyr::gather_("variable", "value", x_vars_num)
    
    # Etape 3 : calcul des moyennes et variances par variable, par mois et par heure
    train_summa <- train_values %>%
        dplyr::group_by_('variable', 'Month', 'Hour') %>%
        dplyr::summarise(
            Mean = mean(value, na.rm = TRUE),
            Std = sd(value, na.rm = TRUE)
        )
    
    # Etape 4 : gestion de la table resultat "train"
    # On ajoute les moyennes et ecart_type par jointure 
    # On remplace les valeurs manquantes par la moyenne
    # On centre et reduit si les parametres sont en accord
    resTrain <- dplyr::left_join(
        train_values,
        train_summa,
        by = c("variable", "Month", "Hour")
    ) %>%
        dplyr::mutate(
            ## factorisation de la colonne "variable"
            variable = factor(variable, x_vars_num),
            ## remplacer les valeurs manquantes
            value = ifelse(!is.na(value), value, Mean)
        )
    if(center){
        resTrain <- resTrain %>% dplyr::mutate(value = value - Mean)
    }
    if(scale){
        resTrain <- resTrain %>%
            dplyr::mutate(
                value = ifelse(!is.na(Std)&Std!=0, value / Std, value)
            )
    }
    ## On peut alors redistribuer les variables
    resTrain <- resTrain %>%
        dplyr::select(-Mean, -Std) %>%
        dplyr::arrange(variable, dt_posix) %>%
        tidyr::spread(variable, value)
    
    # Etape 5 : gestion de la table resultat "test"
    # On fait la meme chose que pour train !
    resTest <- testDf %>%
        tidyr::gather_("variable", "value", x_vars_num) %>%
        dplyr::left_join(train_summa, by = c("variable", "Month", "Hour")) %>%
        dplyr::mutate(
            ## factorisation de la colonne "variable"
            variable = factor(variable, x_vars_num),
            ## remplacer les valeurs manquantes
            value = ifelse(!is.na(value), value, Mean)
        )
    if(center){
        resTest <- resTest %>% dplyr::mutate(value = value - Mean)
    }
    if(scale){
        resTest <- resTest %>%
            dplyr::mutate(
                value = ifelse(
                    !is.na(Std) & Std!=0,
                    value / Std,
                    value
                )
            )
    }
    resTest <- resTest %>%
        dplyr::select(-Mean, -Std) %>%
        dplyr::arrange(variable, dt_posix) %>%
        tidyr::spread(variable, value)
    
    ## Etape 6 :
    ## Les facteurs deviennent disjonctifs (1 / 0)
    if(disj){
        for(ivar in x_vars_fact){
            tab <- tab.disjonctif(resTrain[[ivar]])
            resTrain <- cbind(resTrain, tab)
            resTrain[[ivar]] <- NULL
            save_vars <- c(setdiff(save_vars, ivar), colnames(tab))
            resTest <- cbind(
                resTest,
                tab.disjonctif(resTest[[ivar]])
            )
            resTest[[ivar]] <- NULL
        }
    }
    ## Resultats
    reslist <- list(train = resTrain, test = resTest, vars = save_vars)
    return(reslist)
}

list[trainDf, prevDf, x_vars] <- make_train_test_data(trainDf_withna, prevDf_withna, x_vars)



##==================================================
# Center and Scale + replace NA
##==================================================

save(
    trainDf, prevDf, x_vars, sample_solution,
    file = "ouessant_copy/data/train_prev_BY_month.RData"
)

