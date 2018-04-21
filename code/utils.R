
require(dplyr)
require(ggplot2)
require(gsubfn)
require(xgboost)

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
plot_na <- function(
    df,
    pareto = TRUE,
    simplify = TRUE,
    guide = FALSE,
    maintitle = "Missing Values",
    subtitle = NULL
){
    ## data : proportion of na
    dfna <- data.frame(
        variable = colnames(df),
        nb_na = sapply(df, count_na),
        p_na = sapply(df, prop_na)
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
            mapping = aes(variable, p_na, fill = p_na),
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
    if(!guide){
        resbarplot <- resbarplot + guides(fill = FALSE)
    }
    ## results
    return(resbarplot)
}

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

#==============================================
# manage train and test data
#==============================================
#-----------------------------
# na_replace, center, scale
#-----------------------------
## function to replace na, and center and scale if needed
replace_center_scale <- function(x,
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
        ## function to apply on each data.frame (see "replace_center_scale")
        ## return a new data frame with arrangeddata
        .repl_scale_vars <- function(df){
            ok <- (df$DATA_ == 'train')
            scaled <- mapply(
                replace_center_scale,
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
    factors <- names(which(!sapply(train[variables], is.numeric)))
    if(length(factors) >= 1){
        tabTrain <- tab.disjonctif(train[factors])
        tabTest <- tab.disjonctif(test[factors])
        columns <- unname(colnames(tabTrain))
        if(remove){
            train <- train[setdiff(colnames(train), factors)]
            test <- test[setdiff(colnames(test), factors)]
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
        pred <- predict(bst, as.matrix(Xprev))
        return(pred)
    }
    res <- as.data.frame(lapply(Ytrain, .subpredict))
    return(res)
}
