
require(dplyr)
require(ggplot2)
require(gsubfn)
require(xgboost)

#==============================================
# measure prediction
#==============================================

RMSE <- function(x, y){
    rmse <- sqrt(mean((x - y) ^ 2))
    return(rmse)
}

MAPE <- function(y_pred, y_true){
    mape <- mean(abs((y_true - y_pred)/y_true))
    return(mape)
}

MAE <- function (y_pred, y_true) {
    mae <- mean(abs(y_true - y_pred))
    return(mae)
}

#==============================================
# valeurs manquantes
#==============================================
#------------------------
# coutn and proportion of missing values
#------------------------
count_na <- function(x){
    sum(is.na(x))
}
prop_na <- function(x){
    mean(is.na(x))
}

#------------------------
# plotting missing values
#------------------------
# plot_na renvoie le diagramme en barre donnant la proportion de valeurs
#   manquantes (NA) dans chaque colonne d'une data.frame.
# En entrée:
#   "df": data.frame à évaluer
#   pareto: logical
#       si TRUE, les barres sont ordonnées par ordre décroissant
#       si FALSE, alors les barres sont rangées par ordre alphabetique
#   simplify: logical
#       si TRUE, les colonnes sans valeurs manquantes ne sont pas affichés,
#           seules restent les variables avec valeurs manquantes
#       si FALSE, toutes les variables sont affichées
#   legend:  logical ; = TRUE si la legende doit etre affichee, =FALSE sinon
#   maintitle : titre du graphique (voir "label" de l fonction ggtitle)
#   subtitle : soustitre du graphique (voir la fonction ggtitle)
# En sortie :
#   un graphique de type ggplot

plot_na <- function(
    df,
    pareto = TRUE,
    simplify = TRUE,
    legend = FALSE,
    maintitle = "Missing Values",
    subtitle = NULL
){
    ## data : proportion of na
    dfna <- data.frame(
        variable = colnames(df),
        count_na = sapply(df, count_na),
        prop_na = sapply(df, prop_na)
    )
    if(simplify){
        dfna <- dfna %>% dplyr::filter(nb_na >= 1)
    }
    if(pareto){
        dfna <- dfna %>% dplyr::arrange(desc(p_na), variable)
        dfna$variable <- factor(dfna$variable, unique(dfna$variable))
    }
    
    ## Plot Layers
    resbarplot <- ggplot(data = dfna) +
        geom_bar(
            mapping = aes(variable, prop_na, fill = prop_na),
            stat = "identity",
            col = "black"
        )
    resbarplot <- resbarplot +
        ## modify x axis
        scale_x_discrete(
            name = NULL
        ) +
        # modify y axis
        scale_y_continuous(
            name = "Prop NA",
            breaks = seq(0, 1, 0.1),
            limits = c(0, 1)
        ) +
        ## Modify colors
        scale_fill_continuous(
            name = "Prop NA",
            high = "#333333",
            low = "#CCCCCC"
        )
    ## set title abnd theme
    resbarplot <- resbarplot +
        ggtitle(maintitle, subtitle) +
        theme_bw()
    
    ## delete legend
    if(!legend){
        resbarplot <- resbarplot + guides(fill = FALSE)
    }
    ## results
    return(resbarplot)
}


#==============================================
# manage train and test data
#==============================================

train_test_split <- function(data, ptrain = 0.75)
{
    N <- nrow(data)
    n <- round(ptrain * N)
    s <- sample.int(N, n, replace = FALSE)
    reslist <- list(
        train = data[s,],
        test = data[-s,]
    )
    return(reslist)
}


#-----------------------------
# na_replace, center, scale
#-----------------------------
## function to replace na, and center and scale if needed
replace_na_center_scale <- function(x,
                                 y = NULL,
                                 na_replace = TRUE,
                                 center = TRUE,
                                 scale = TRUE)
{
    ## this function : replace na by mean into x, center and scale x
    ## then, it applies on y with x values
    ## useful for train/test as an example
    ## warning : that is important to replace na before calcul std
    ## (because "n-1" woulnt be the same it there are some "na")
    moy <- mean(x, na.rm = TRUE)
    if(na_replace){x <- replace(x, is.na(x), moy)}
    std <- sd(x, na.rm = TRUE)
    cent <- ifelse(center & !is.na(moy), moy, FALSE)
    scal <- ifelse(scale & !is.na(std) & std != 0, std, FALSE)
    x_scaled <- scale(x, cent, scal)[,1]
    ## Do the same on y, with x values
    if(length(y > 0)){
        if(na_replace){y <- replace(y, is.na(y), moy)}
        y_scaled <- scale(y, cent, scal)[,1]
        result <- list(x = x_scaled, y = y_scaled)
    }else{
        result <- x_scaled
    }
    return(result)
}

#-----------------------------
# apply center and scale on data
#-----------------------------
# /!\ train and test must have the same structure /!\
deal_train_test_numerics <- function(
    train,
    test,
    variables,
    by = NULL,
    na_replace = TRUE,
    center = TRUE,
    scale = TRUE
){
    numerics <- names(which(sapply(train[variables], is.numeric)))
    if(length(numerics) >= 1){
        ## keep only wished values to have the same structure
        train$DATA_ <- "train"
        train$ORDER_ <- 1:nrow(train)
        test$DATA_ <- "test"
        test$ORDER_ <- 1:nrow(test)
        all_variables <- c(numerics, by, "DATA_", "ORDER_")
        subTrain <- train[all_variables]
        subTest <- test[all_variables]
        ## Then, subTrain and subTest have the same structure
        # bind train and test data
        alldata <- rbind(subTrain, subTest)
        alldata$ORDER_ <- 1:nrow(alldata)
        
        # make a list of subdata
        if(length(by) >= 1){
            alldata_ls <- split(alldata, alldata[by])
        }else{
            alldata_ls <- list(alldata)
        }
        ## function to apply on each data.frame (see "replace_na_center_scale")
        ## return a new data frame with arrangeddata
        .repl_scale_vars <- function(df){
            ok <- (df$DATA_ == 'train')
            scaled <- mapply(
                replace_na_center_scale,
                x = df[which(ok), numerics],
                y = df[which(!ok), numerics],
                na_replace = na_replace,
                center = center,
                scale = scale
            )
            if(any(!ok)){
                scaled <- bind_rows(apply(scaled, 1, bind_cols))
            }
            others <- setdiff(colnames(df), numerics)
            resDf <- cbind(df[others], scaled)
            return(resDf)
        }
        # applying .repl_scale_numerics on each subdata
        arrangedDf <- bind_rows(lapply(alldata_ls, .repl_scale_vars))
        
        # then, we can rebuild train and test
        arrangedTrain <- arrangedDf %>%
            dplyr::filter(DATA_ == "train") %>%
            dplyr::arrange(ORDER_)
        train[numerics] <- arrangedTrain[numerics]
        train$DATA_ <- NULL
        train$ORDER_ <- NULL
        
        arrangedTest <- arrangedDf %>%
            dplyr::filter(DATA_ == "test") %>%
            dplyr::arrange(ORDER_) %>%
            dplyr::mutate(DATA_ = NULL, ORDER_ = NULL)
        test[numerics] <- arrangedTest[numerics]
        test$DATA_ <- NULL
        test$ORDER_ <- NULL
        
        ## result list : train / test
        resLs <- list(train = train, test = test, columns = numerics)
    }else{
        resLs <- list(train = train, test = test, columns = NULL)
    }
    return(resLs)
}

#-----------------------------
# table disjonctive
#-----------------------------
## tab.disjonctif
## from package FactoMineR
## see "help(tab.disjonctif)" for details

tab.disjonctif <- function(tab){
    tab <- as.data.frame(tab)
    .modalite_disjonctif <- function(i) {
        moda <- as.factor(tab[, i])
        n <- length(moda)
        x <- matrix(0L, n, nlevels(moda))
        x[(1:n) + n * (unclass(moda) - 1L)] <- 1L
        return(x)
    }
    if(ncol(tab) == 1){
        res <- .modalite_disjonctif(1)
        dimnames(res) <- list(attributes(tab)$row.names, levels(tab[,1]))
    }else{
        variable <- rep(attributes(tab)$names, lapply(tab, nlevels))
        listModa <- unlist(lapply(tab, levels))
        wlistModa <- which((listModa) %in% c("y", "n", "Y", "N"))
        if(!is.null(wlistModa)){
            listModa[wlistModa] <- paste(variable[wlistModa], 
                                         listModa[wlistModa], sep = ".")
        }
        numlistModa <- which(unlist(lapply(listModa, is.numeric)))
        if(!is.null(numlistModa)){
            listModa[numlistModa] <- paste(
                variable[numlistModa], 
                listModa[numlistModa],
                sep = "."
            )
        } 
        res <- lapply(1:ncol(tab), .modalite_disjonctif)
        res <- as.matrix(data.frame(res, check.names = FALSE))
        dimnames(res) <- list(attributes(tab)$row.names, listModa)
    }
    return(res)
}

#-----------------------------
# table disjonctive on train test
#-----------------------------

deal_train_test_factors <- function(train, test, variables, remove = TRUE, ...)
{
    if(length(variables) >= 1){
        tabTrain <- tab.disjonctif(train[variables])
        tabTest <- tab.disjonctif(test[variables])
        columns <- unname(colnames(tabTrain))
        if(remove){
            train <- train[setdiff(colnames(train), variables)]
            test <- test[setdiff(colnames(test), variables)]
        }
        train <- cbind(train, tabTrain)
        test <- cbind(test, tabTest)
    }else{
        columns <- NULL
    }
    reslist <- list(train = train, test = test, columns = columns)
}

#==============================================
# regroup numeric and factor treatments
#==============================================

# deal_train_test <- function(
#     train,
#     test,
#     numerics = NULL,
#     factors = NULL,
#     by = NULL,
#     variables = c(numerics, factors),
#     others = NULL,
#     ...
# ){
#     ## split numerics and factors
#     ok_num <- sapply(train[variables], is.numeric)
#     numerics <- variables[which(ok_num)]
#     factors <- variables[which(!ok_num)]
#     ## numerics
#     list[restrain, restest] <- deal_train_test_numerics(
#         train, test,
#         numerics = numerics,
#         by = by,
#         ...
#     )
#     ## factors
#     list[restrain, restest] <- deal_train_test_factors(
#         restrain, restest, factors, remove = FALSE
#     )
#     ## final variables
#     columns <- c(numerics, setdiff(colnames(restrain), colnames(train)), others)
#     ## results
#     reslist <- list(
#         train = restrain,
#         test = restest,
#         columns = columns
#     )
#     return(reslist)
# }

#==============================================
# Xgboost
#==============================================


xgboost_predict <- function(Ytrain, Xtrain, Xtest, ...)
{
    ## prediction sur une colonne, par xgboost
    ## ... = arguments of Xgboost
    .subpredict <- function(y){
        bst <- xgboost(
            data = as.matrix(Xtrain),
            label = y,
            ...
        )
        pred <- predict(bst, as.matrix(Xtest))
        return(pred)
    }
    res <- as.data.frame(lapply(Ytrain, .subpredict))
    return(res)
}

#==============================================
# plot result
#==============================================

plot_submits <- function(submitDf, numsc, mape){
    plots <- ggplot(
        data = submitDf,
        mapping = aes(x = hour(datetime))
    ) +
        ## reality
        geom_line(mapping = aes(y = reality),
                  col = "red",
                  lwd = 1) +
        geom_point(mapping = aes(y = reality),
                   col = "red") +
        ## prediction
        geom_line(mapping = aes(y = puissance),
                  col = "blue",
                  lwd = 1) +
        geom_point(mapping = aes(y = puissance),
                   col = "blue") +
        ## other parameters
        scale_x_continuous(
            name = "Hour",
            breaks = seq(0, 23, 3),
            minor_breaks = seq(0, 23, 1),
            limits = c(0, 23)
        ) +
        facet_wrap(
            ~format(submitDf$datetime, "%b-%d-(%a)"),
            scales = "free_x"
        ) +
        theme_bw() +
        theme(
            panel.grid.major = element_line(color = "grey40"),
            panel.grid.minor = element_line(color = "grey70",
                                            linetype = "dashed",
                                            size = 0.2)
        ) +
        ggtitle(
            label = sprintf(
                "Graphique Prediction, scenario %i, (mape = %.2f)",
                numsc, mape
            ),
            subtitle = "blue : prediction ; red : reality"
        )
    return(plots)
}
