##==================================================
# sid_RC
# Cedric Bezy, Riwan Mouster
##==================================================

require(dplyr)
require(tidyr)
require(FactoMineR)

##==================================================
# function to make train and test data
##==================================================

# replace_na <- function(train, test, factors = c())
# {
#     mat_mean <- tapply(train[[x]], train[factors], mean, na.rm = TRUE)
#     wna_train <- which(is.na(train[[x]]))
#     wna_test <- which(is.na(test[[x]]))
#     i <- wna_train[1]
#     for(i in wna_train){
#         irow <- train[i, factors]
#         for(n in length(factors)){
#             
#         }
#     }
#     if(length(factors)==0){
#         res <- replace(x, is.na(x), mean(x, na.rm = TRUE))
#     }
# }

make_train_test_data <- function(
    train,
    test,
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
    ok_num <- sapply(train[x_vars], is.numeric)
    x_vars_num <- names(which(ok_num))
    x_vars_fact <- names(which(!ok_num))
    save_vars <- x_vars
    
    # Etape 2 :
    # les valeurs des variables references dans x_vars
    # sont reparties dans deux colonnes (variable, value)
    # variable est le nom de la variable, value le nom de la value
    # les autres informations sont conservees pour chaque ligne
    train_values <- train %>%
        tidyr::gather_("variable", "value", x_vars_num)
    
    # Etape 3 : calcul des moyennes et variances par variable, par mois et par heure
    train_summa <- train_values %>%
        dplyr::group_by_('variable') %>%
        dplyr::summarise(Mean = mean(value, na.rm = TRUE))
    
    # Etape 4 : gestion de la table resultat "train"
    # On ajoute les moyennes et ecart_type par jointure 
    # On remplace les valeurs manquantes par la moyenne
    resTrain <- dplyr::left_join(
        train_values,
        train_summa,
        by = c("variable")
    ) %>%
        dplyr::mutate(
            ## factorisation de la colonne "variable"
            variable = factor(variable, x_vars_num),
            ## remplacer les valeurs manquantes
            value = ifelse(!is.na(value), value, Mean),
            Mean = NULL
        ) %>%
        ## On peut alors redistribuer les variables
        dplyr::arrange(variable, dt_posix) %>%
        tidyr::spread(variable, value)
    
    # Etape 5 : gestion de la table resultat "test"
    # On fait la meme chose que pour train !
    resTest <- test %>%
        tidyr::gather_("variable", "value", x_vars_num) %>%
        dplyr::left_join(train_summa, by = c("variable")) %>%
        dplyr::mutate(
            ## factorisation de la colonne "variable"
            variable = factor(variable, x_vars_num),
            ## remplacer les valeurs manquantes
            value = ifelse(!is.na(value), value, Mean),
            Mean = NULL
        ) %>%
        dplyr::arrange(variable, dt_posix) %>%
        tidyr::spread(variable, value)
    
    ## Etape 6 : centrer et reduire les variables numeriques
    if(center | scale){
        for(i in x_vars_num){
            itrain <- resTrain[[i]]
            itest <- resTest[[i]]
            if(center){
                imoy <- mean(itrain)
                itrain <- itrain - imoy
                itest <- itest - imoy
            }
            if(scale){
                istd <- sd(itrain)
                itrain <- itrain / istd
                itest <- itest / istd
            }
            resTrain[[i]] <- itrain
            resTest[[i]] <- itest
        }
    }
    
    ## Etape 7 :
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





