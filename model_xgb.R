#### Santander Product Recommendation on Kaggle
#### Team 'SRRRK' (Sudalai Raj Kumar and Rohan Rao)
#### 11th Rank

## setting file paths and seed (edit the paths before running)
path_train <- "train.csv"
path_test <- "test.csv"
path_preds <- "preds.csv"

## put your favourite number as seed
seed <- 123
set.seed(seed)

## loading libraries
library(data.table)
library(xgboost)

## loading raw data
train <- fread(path_train, showProgress = T)
test <- fread(path_test, showProgress = T)

## removing five products
train[, ind_ahor_fin_ult1 := NULL]
train[, ind_aval_fin_ult1 := NULL]
train[, ind_deco_fin_ult1 := NULL]
train[, ind_deme_fin_ult1 := NULL]
train[, ind_viv_fin_ult1 := NULL]

## extracting train data of each product and rbinding them together with single multiclass label
i <- 0
target_cols <- names(train)[which(regexpr("ult1", names(train)) > 0)]

for (target_col in target_cols)
{
    i <- i + 1
    
    S <- paste0("train", i, " <- train[", target_col, " > 0]")
    eval(parse(text = S))
}

rm(train)
gc()

for (i in 1:19)
{
    S1 <- paste0("train", i, " <- train", i, "[, !target_cols, with = F]")
    eval(parse(text = S1))
    
    S2 <- paste0("train", i, "[, target := ", i-1, "]")
    eval(parse(text = S2))
}

X_train <- rbind(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10,
                 train11, train12, train13, train14, train15, train16, train17, train18, train19)

rm(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10,
   train11, train12, train13, train14, train15, train16, train17, train18, train19)
gc()

## rbinding train and test data
X_panel <- rbind(X_train, test, use.names = T, fill = T)

## adding corresponding numeric months (1-18) to fecha_dato
X_panel[, month := as.numeric(as.factor(fecha_dato))]

## creating user-product matrix
X_user_target <- dcast(X_panel[!is.na(target)], ncodpers + month ~ target, length, value.var = "target", fill = 0)

## creating product lag-variables of order-12 and merging with data
X_user_target[, month := month + 1]

setnames(X_user_target,
         c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
           "16", "17", "18"),
         c("prev_0", "prev_1", "prev_2", "prev_3", "prev_4", "prev_5", "prev_6", "prev_7",
           "prev_8", "prev_9", "prev_10", "prev_11", "prev_12", "prev_13", "prev_14", "prev_15",
           "prev_16", "prev_17", "prev_18"))

X_panel <- merge(X_panel, X_user_target, all.x = T, by = c("ncodpers", "month"))

for (i in 2:12)
{
    X_user_target[, month := month + 1]
    
    lag_cols <- names(X_user_target)[which(regexpr("prev", names(X_user_target)) > 0)]
    setnames(X_user_target, lag_cols, paste0("prev_", lag_cols))
    
    X_panel <- merge(X_panel, X_user_target, all.x = T, by = c("ncodpers", "month"))
}

X_panel[is.na(X_panel)] <- 0

## calculating historic average of product lag-variables with 1, 2, 3, 4, 5, 6, 9, 12 months
for (i in 0:18)
{
    S1 <- paste0("X_panel[, prev2_", i, " := (prev_", i, " + prev_prev_", i, ") / 2]")
    eval(parse(text = S1))
    
    S2 <- paste0("X_panel[, prev3_", i, " := (prev_", i, " + prev_prev_", i, " + prev_prev_prev_", i, ") / 3]")
    eval(parse(text = S2))
    
    S3 <- paste0("X_panel[, prev4_", i, " := (prev_", i, " + prev_prev_", i, " + prev_prev_prev_", i, " + prev_prev_prev_prev_", i, ") / 4]")
    eval(parse(text = S3))
    
    S4 <- paste0("X_panel[, prev5_", i, " := (prev_", i, " + prev_prev_", i, " + prev_prev_prev_", i, " + prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_", i, ") / 5]")
    eval(parse(text = S4))
    
    S5 <- paste0("X_panel[, prev6_", i, " := (prev_", i, " + prev_prev_", i, " + prev_prev_prev_", i, " + prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_", i, ") / 6]")
    eval(parse(text = S5))
    
    S6 <- paste0("X_panel[, prev9_", i, " := (prev_", i, " + prev_prev_", i, " + prev_prev_prev_", i, " + prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_", i, "
                 + prev_prev_prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, ") / 9]")
    eval(parse(text = S6))
    
    S6 <- paste0("X_panel[, prev12_", i, " := (prev_", i, " + prev_prev_", i, " + prev_prev_prev_", i, " + prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_", i, "
                 + prev_prev_prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, "
                 + prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, " + prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, ") / 12]")
    eval(parse(text = S6))
    
    S7 <- paste0("X_panel[, ':='(prev_prev_", i, " = NULL, prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_prev_prev_", i, " = NULL,
                 prev_prev_prev_prev_prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_prev_prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, " = NULL,
                 prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, " = NULL, prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_prev_", i, " = NULL)]")
    eval(parse(text = S7))
}

## cleaning raw features
X_panel[, ":="(ind_empleado = as.numeric(as.factor(ind_empleado)),
               pais_residencia = as.numeric(as.factor(pais_residencia)),
               sexo = as.numeric(as.factor(sexo)),
               year_joining = year(as.Date(fecha_alta)),
               month_joining = month(as.Date(fecha_alta)),
               fecha_alta = as.numeric(as.Date(fecha_alta) - as.Date("2016-05-31")),
               ult_fec_cli_1t = ifelse(ult_fec_cli_1t == "", 0, 1),
               indrel_1mes = as.numeric(as.factor(indrel_1mes)),
               tiprel_1mes = as.numeric(as.factor(tiprel_1mes)),
               indresi = as.numeric(as.factor(indresi)),
               indext = as.numeric(as.factor(indext)),
               conyuemp = as.numeric(as.factor(conyuemp)),
               canal_entrada = as.numeric(as.factor(canal_entrada)),
               indfall = as.numeric(as.factor(indfall)),
               tipodom = NULL,
               cod_prov = as.numeric(as.factor(cod_prov)),
               nomprov = NULL,
               segmento = as.numeric(as.factor(segmento)))]

## calculating product count of previous month
X_panel[, count_prev1_products := (prev_0 + prev_1 + prev_2 + prev_3 + prev_4 + prev_5 + prev_6
                                   + prev_7 + prev_8 + prev_9 + prev_10 + prev_11 + prev_12
                                   + prev_13 + prev_14 + prev_15 + prev_16 + prev_17 + prev_18)]

## label encoding binary string of previous month's products (in order of popularity)
X_panel[, prev_products := as.numeric(as.factor(paste0(prev_0, prev_18, prev_5, prev_2, prev_8,
                                                       prev_17, prev_16, prev_13, prev_14,
                                                       prev_15, prev_6, prev_7, prev_9, prev_4,
                                                       prev_3, prev_11, prev_10, prev_12, prev_1)))]

## creating train and test data for June-15 (seasonality) and May-16 (trend) models
X_train_1 <- X_panel[fecha_dato %in% c("2015-06-28")]
X_train_2 <- X_panel[fecha_dato %in% c("2016-05-28")]

X_test_1 <- X_panel[fecha_dato %in% c("2016-06-28")]
X_test_2 <- X_panel[fecha_dato %in% c("2016-06-28")]

X_test_order <- X_test_1$ncodpers

## creating binary flag for new products, test data will always have 1 since we need to predict new products
X_train_1$flag_new <- 0
X_train_2$flag_new <- 0

X_test_1$flag_new <- 1
X_test_2$flag_new <- 1

for (i in 0:18)
{
    S1 <- paste0("X_train_1$flag_new[X_train_1$prev_", i, " == 0 & X_train_1$target == ", i, "] <- 1")
    eval(parse(text = S1))
    
    S2 <- paste0("X_train_2$flag_new[X_train_2$prev_", i, " == 0 & X_train_2$target == ", i, "] <- 1")
    eval(parse(text = S2))
}

## removing lag6, lag9, lag12 variables from seasonality model
for (col in names(X_train_1)[which(regexpr("prev6", names(X_train_1)) > 0 | regexpr("prev9", names(X_train_1)) > 0 | regexpr("prev12", names(X_train_1)) > 0)])
{
    S1 <- paste0("X_train_1[, ", col, " := NULL]")
    eval(parse(text = S1))
    
    S2 <- paste0("X_test_1[, ", col, " := NULL]")
    eval(parse(text = S2))
}

## extracting labels
X_target_1 <- X_train_1$target
X_target_2 <- X_train_2$target

## removing redundant columns
X_train_1[, ":="(fecha_dato = NULL, ncodpers = NULL, month = NULL, target = NULL)]
X_train_2[, ":="(fecha_dato = NULL, ncodpers = NULL, month = NULL, target = NULL)]

X_test_1[, ":="(fecha_dato = NULL, ncodpers = NULL, month = NULL, target = NULL)]
X_test_2[, ":="(fecha_dato = NULL, ncodpers = NULL, month = NULL, target = NULL)]

## creating xgb.DMatrix
xgtrain1 <- xgb.DMatrix(as.matrix(X_train_1), label = X_target_1, missing = NA)
xgtrain2 <- xgb.DMatrix(as.matrix(X_train_2), label = X_target_2, missing = NA)

xgtest1 <- xgb.DMatrix(as.matrix(X_test_1), missing = NA)
xgtest2 <- xgb.DMatrix(as.matrix(X_test_2), missing = NA)

## xgboost parameters
params <- list()
params$objective <- "multi:softprob"
params$num_class <- 19
params$eta <- 0.1
params$max_depth <- 5
params$subsample <- 0.8
params$colsample_bytree <- 0.8
params$min_child_weight <- 3
params$eval_metric <- "mlogloss"

## xgboost training
model_xgb_1 <- xgb.train(params = params, xgtrain1, nrounds = 140, nthread = -1)
model_xgb_2 <- xgb.train(params = params, xgtrain2, nrounds = 140, nthread = -1)

## xgboost predictions
pred_1 <- predict(model_xgb_1, xgtest1)
pred_2 <- predict(model_xgb_2, xgtest2)

## converting predictions into 19 product columns
pred_matrix_1 <- data.table(matrix(pred_1, ncol = 19, byrow = T))
pred_matrix_2 <- data.table(matrix(pred_2, ncol = 19, byrow = T))

## cco weights
pred_matrix_1[,1] <- 0.9 * pred_matrix_1[,1]
pred_matrix_2[,1] <- 0.1 * pred_matrix_2[,1]

## cder weights
pred_matrix_1[,2] <- 0.5 * pred_matrix_1[,2]
pred_matrix_2[,2] <- 0.5 * pred_matrix_2[,2]

## cno weights
pred_matrix_1[,3] <- 0.3 * pred_matrix_1[,3]
pred_matrix_2[,3] <- 0.7 * pred_matrix_2[,3]

## ctju weights
pred_matrix_1[,4] <- 0.4 * pred_matrix_1[,4]
pred_matrix_2[,4] <- 0.6 * pred_matrix_2[,4]

## ctma weights
pred_matrix_1[,5] <- 0.3 * pred_matrix_1[,5]
pred_matrix_2[,5] <- 0.7 * pred_matrix_2[,5]

## ctop weights
pred_matrix_1[,6] <- 0.5 * pred_matrix_1[,6]
pred_matrix_2[,6] <- 0.5 * pred_matrix_2[,6]

## ctpp weights
pred_matrix_1[,7] <- 0.4 * pred_matrix_1[,7]
pred_matrix_2[,7] <- 0.6 * pred_matrix_2[,7]

## dela weights
pred_matrix_1[,8] <- 0.1 * pred_matrix_1[,8]
pred_matrix_2[,8] <- 0.9 * pred_matrix_2[,8]

## ecue weights
pred_matrix_1[,9] <- 0.3 * pred_matrix_1[,9]
pred_matrix_2[,9] <- 0.7 * pred_matrix_2[,9]

## fond weights
pred_matrix_1[,10] <- 0.1 * pred_matrix_1[,10]
pred_matrix_2[,10] <- 0.9 * pred_matrix_2[,10]

## hip weights
pred_matrix_1[,11] <- 0.5 * pred_matrix_1[,11]
pred_matrix_2[,11] <- 0.5 * pred_matrix_2[,11]

## plan weights
pred_matrix_1[,12] <- 0.5 * pred_matrix_1[,12]
pred_matrix_2[,12] <- 0.5 * pred_matrix_2[,12]

## pres weights
pred_matrix_1[,13] <- 0.5 * pred_matrix_1[,13]
pred_matrix_2[,13] <- 0.5 * pred_matrix_2[,13]

## reca weights
pred_matrix_1[,14] <- 0.9 * pred_matrix_1[,14]
pred_matrix_2[,14] <- 0.1 * pred_matrix_2[,14]

## tjcr weights
pred_matrix_1[,15] <- 0.5 * pred_matrix_1[,15]
pred_matrix_2[,15] <- 0.5 * pred_matrix_2[,15]

## valo weights
pred_matrix_1[,16] <- 0.6 * pred_matrix_1[,16]
pred_matrix_2[,16] <- 0.4 * pred_matrix_2[,16]

## nomina weights
pred_matrix_1[,17] <- 0.8 * pred_matrix_1[,17]
pred_matrix_2[,17] <- 0.2 * pred_matrix_2[,17]

## nom_pens weights
pred_matrix_1[,18] <- 0.8 * pred_matrix_1[,18]
pred_matrix_2[,18] <- 0.2 * pred_matrix_2[,18]

## recibo weights
pred_matrix_1[,19] <- 0.4 * pred_matrix_1[,19]
pred_matrix_2[,19] <- 0.6 * pred_matrix_2[,19]

## saving preds to csv
pred_matrix <- pred_matrix_1 + pred_matrix_2
pred_matrix[, ncodpers := X_test_order]

fwrite(pred_matrix, path_preds)
