##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(gsubfn)
library(sqldf)

Sys.setlocale("LC_TIME", "English_United States")

##==================================================
# function to make train and test data
##==================================================

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
