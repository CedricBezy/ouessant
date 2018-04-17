
rm(list = ls())

library(dplyr)
library(randomForest)

load('ouessant_copy/data/train_prev_BY_d15.RData')

rf_vars <- c(x_vars, "Month", "D15", "Hour")

train0 <- trainDf[c('P0', rf_vars)]
train1 <- trainDf[c('DP1', rf_vars)]
train2 <- trainDf[c('DP2', rf_vars)]

form0 <- as.formula(paste("P0 ~", paste(rf_vars, collapse = " + ")))
form1 <- as.formula(paste("DP1 ~", paste(rf_vars, collapse = " + ")))
form2 <- as.formula(paste("DP2 ~", paste(rf_vars, collapse = " + ")))

rf0 <- randomForest(form0, data = train0)
p0_pred <- predict(rf0, newdata = prevDf)

rf1 <- randomForest(form1, data = train1)
dp1_pred <- predict(rf1, newdata = prevDf)
p1_pred <- p0_pred + dp1_pred

rf2 <- randomForest(form2, data = train2)
dp2_pred <- predict(rf2, newdata = prevDf)
p2_pred <- p0_pred + dp2_pred


submitDf <- data.frame(
    dt_posix = prevDf$dt_posix,
    P0 = p0_pred,
    P1 = p1_pred,
    P2 = p2_pred
) %>%
    tidyr::gather(variable, puissance, c(P0, P1, P2)) %>%
    dplyr::mutate(
        variable = factor(
            variable,
            levels = paste0("P", 0:2),
            labels = 0:2
        ),
        Hour = as.numeric(as.character(variable)),
        dt_posix = dt_posix + 3600 * Hour
    ) %>%
    dplyr::select(dt_posix, puissance) %>%
    dplyr::arrange(dt_posix)

RMSE <- sqrt(mean((submitDf$puissance - sample_solution$V1) ^ 2))
