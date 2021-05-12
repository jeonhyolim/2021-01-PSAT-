library(ROCR)
library(tidyverse)
library(plyr)
library(magrittr)
library(data.table)
library(caret)
library(randomForest)
library(xgboost)
library(ggplot2)
library(fastDummies)
setwd("C:/Users/hjh05/Downloads/ugi")
#########################
#### 멍멍이 결과해석 ####
#########################

# 모델링 성능 상위3개 Roc 곡선 그리기==================
# https://m.blog.naver.com/audgnsdl115/221512806105
## (1st) 랜덤포레스트-----------------------------------------
result <- fread("rf_dog_cbs.csv",header = TRUE,data.table = FALSE)
result = arrange(result,desc(f1score))

train_dog <- fread("cbs/dog_train.csv",header = TRUE,data.table = FALSE)
test_dog <- fread("cbs/dog_test.csv",header = TRUE,data.table = FALSE)
train_dog$adoptionYN = as.factor(train_dog$adoptionYN)
test_dog$adoptionYN = as.factor(test_dog$adoptionYN)

set.seed(613)
rf_mod_1 = randomForest(adoptionYN~., train_dog, mtry = 3, ntree = 300)
rf_pred_1 = predict(rf_mod_1, newdata = select(test_dog, -adoptionYN))

rf_pred_2 = predict(rf_mod_1, newdata = select(test_dog, -adoptionYN), type="prob")[,2]

rf_pred_3 = prediction(rf_pred_2,test_dog$adoptionYN)
perf_rf = performance(rf_pred_3, measure = "tpr", x.measure="fpr")
performance(rf_pred_3, "auc")@y.values[[1]] #0.7780608


## (2nd) xgboost-----------------------------------------
#파일 불러오기
result <- fread("xgb_dog_cbsR_aft.csv",header = TRUE,data.table = FALSE)
result = arrange(result,desc(f1score))

train_dog <- fread("cbs/dog_train.csv",header = TRUE,data.table = FALSE)
test_dog <- fread("cbs/dog_test.csv",header = TRUE,data.table = FALSE)

dtrain <- xgb.DMatrix(data = as.matrix(train_dog[,-1]), label=train_dog$adoptionYN)

set.seed(613)
ml_xgb <- xgb.train(data=dtrain, booster = "gblinear",  eval.metric = "logloss", objective = "binary:logistic", 
                    eta =result$eta[1] , 
                    max_depth=result$max_depth[1],
                    min_child_weight = result$min_child_weight[1],
                    subsample=result$subsample[1],
                    colsample_bytree=result$colsample_bytree[1],
                    early_stoppind_rounds = 100,
                    watchlist = list(train=dtrain),
                    nrounds = 1500, verdose=0)

xgb.pred = predict(ml_xgb, as.matrix(test_dog[,-1]))

xgb_pred_2 = predict(ml_xgb, as.matrix(test_dog[,-1]), type="response")
xgb_pred_3 = prediction(xgb_pred_2,test_dog$adoptionYN)
perf_xgb = performance(xgb_pred_3, measure = "tpr", x.measure="fpr")
plot(perf_xgb,col="blue")
performance(xgb_pred_3, "auc")@y.values[[1]] #0.680891


## (3rd) 로지스틱--------------------------------------------
train_dog <- fread("data/final_train_dog.csv",header = TRUE,data.table = FALSE)
test_dog <- fread("data/final_test_dog.csv",header = TRUE,data.table = FALSE)

train_dog$adoptionYN = as.factor(train_dog$adoptionYN)
train_dog$neuterYN = as.factor(train_dog$neuterYN)
train_dog$sex = as.factor(train_dog$sex)
train_dog$group_akc = as.factor(train_dog$group_akc)
train_dog$color = as.factor(train_dog$color)

test_dog$adoptionYN = as.factor(test_dog$adoptionYN)
test_dog$neuterYN = as.factor(test_dog$neuterYN)
test_dog$sex = as.factor(test_dog$sex)
test_dog$group_akc = as.factor(test_dog$group_akc)
test_dog$color = as.factor(test_dog$color)

set.seed(613)
ml_logistic <- glm(adoptionYN~., family=binomial, data= train_dog)
pro_logistic <- predict(ml_logistic, newdata=test_dog[,-1] , type="response")
lgi_pred_3 = prediction(pro_logistic,test_dog$adoptionYN)
perf_lgi = performance(lgi_pred_3, measure = "tpr", x.measure="fpr")
plot(perf_lgi,col="blue")
performance(lgi_pred_3, "auc")@y.values[[1]] #0.6868881


## 모아서 그리기
plot(perf_rf,col="#EA2C62", main="ROC",lwd="3")
abline(0,1)
plot(perf_xgb,col="#B3E2CD",add=T,lwd="3")
plot(perf_lgi,col="#E3D955",add=T,lwd="2")
legend('bottomright',
        legend=c("RandomForest","XGBoost","Logistic"),
        col=c("#EA2C62","#B3E2CD","#E3D955"),inset=.01, lty=1,lwd=5)

## 모아서 그리기
plot(perf_rf,col="#EA2C62", main="Dog :: ROC",lwd="3")
abline(0,1)
plot(perf_xgb,col="#B3E2CD",add=T,lwd="3")
plot(perf_lgi,col="#E3D955",add=T,lwd="2")

#########################
#### 야옹이 결과해석 ####
#########################

# 모델링 성능 상위3개 Roc 곡선 그리기==================
# https://m.blog.naver.com/audgnsdl115/221512806105
## (1st) 랜덤포레스트-----------------------------------------
result <- fread("rf_dog_dum.csv",header = TRUE,data.table = FALSE)
result = arrange(result,desc(f1score))

train_dog <- fread("data/final_train_dog.csv",header = TRUE,data.table = FALSE)
test_dog <- fread("data/final_test_dog.csv",header = TRUE,data.table = FALSE)

#전처리
train_dog$adoptionYN = as.factor(train_dog$adoptionYN)
train_dog$neuterYN = as.factor(train_dog$neuterYN)
train_dog$sex = as.factor(train_dog$sex)
train_dog$group_akc = as.factor(train_dog$group_akc)
train_dog$color = as.factor(train_dog$color)

test_dog$adoptionYN = as.factor(test_dog$adoptionYN)
test_dog$neuterYN = as.factor(test_dog$neuterYN)
test_dog$sex = as.factor(test_dog$sex)
test_dog$group_akc = as.factor(test_dog$group_akc)
test_dog$color = as.factor(test_dog$color)

train_dog2 <- dummy_cols(train_dog, 
                         select_columns = c('neuterYN', 'sex', 'group_akc', 'color'),
                         remove_selected_columns = TRUE)

test_dog2 <- dummy_cols(test_dog, 
                        select_columns = c('neuterYN', 'sex', 'group_akc', 'color'),
                        remove_selected_columns = TRUE)

#scaling
train_dog2[,c(2, 5, 6, 8, 9, 10)] <- scale(train_dog2[,c(2, 5, 6, 8, 9, 10)])
test_dog2[,c(2, 5, 6, 8, 9, 10)] <- scale(test_dog2[,c(2, 5, 6, 8, 9, 10)])

set.seed(613)
rf_mod_1 = randomForest(adoptionYN~., train_dog, mtry = result[1,'mtry'], ntree = result[1, 'ntree'])
rf_pred_1 = predict(rf_mod_1, newdata = select(test_dog, -adoptionYN))

rf_pred_2 = predict(rf_mod_1, newdata = select(test_cat, -adoptionYN), type="prob")[,2]

rf_pred_3 = prediction(rf_pred_2,test_cat$adoptionYN)
perf_rf = performance(rf_pred_3, measure = "tpr", x.measure="fpr")
performance(rf_pred_3, "auc")@y.values[[1]] #0.7780608


## (2nd) xgboost-----------------------------------------
#파일 불러오기
result <- fread("xgb_dog_bothR_aft.csv",header = TRUE,data.table = FALSE)
result = arrange(result,desc(f1score))

train_dog <- fread("both/dog_train.csv",header = TRUE,data.table = FALSE)
test_dog <- fread("both/dog_test.csv",header = TRUE,data.table = FALSE)

dtrain <- xgb.DMatrix(data = as.matrix(train_dog[,-1]), label=train_dog$adoptionYN)
set.seed(613)
ml_xgb <- xgb.train(data=dtrain, booster = "gblinear",  eval.metric = "logloss", objective = "binary:logistic", 
                    eta =result$eta[1] , 
                    max_depth=result$max_depth[1],
                    min_child_weight = result$min_child_weight[1],
                    subsample=result$subsample[1],
                    colsample_bytree=result$colsample_bytree[1],
                    early_stoppind_rounds = 100,
                    watchlist = list(train=dtrain),
                    nrounds = 1500, verdose=0)

xgb.pred = predict(ml_xgb, as.matrix(test_cat[,-1]))

xgb_pred_2 = predict(ml_xgb, as.matrix(test_cat[,-1]), type="response")
xgb_pred_3 = prediction(xgb_pred_2,test_cat$adoptionYN)
perf_xgb = performance(xgb_pred_3, measure = "tpr", x.measure="fpr")
plot(perf_xgb,col="blue")
performance(xgb_pred_3, "auc")@y.values[[1]] #0.6808929


## (3rd) 로지스틱--------------------------------------------
cat = fread('data/final_train_cat.csv', data.table = FALSE)
cat$adoptionYN = cat$adoptionYN %>% as.factor()
cat$season = cat$season %>% as.factor()
cat$neuterYN = cat$neuterYN %>% as.factor()
cat$sex = cat$sex %>% as.factor()
cat$hair = cat$hair %>% as.factor()
cat$color = cat$color %>% as.factor()
cat_cate = c('season', 'neuterYN', 'sex', 'hair', 'color')
cat_nume = c('age', 'disease_cnt', 'positives', 'negatives', 'grdp', 'economy', 'hospital_num')

cat_trainx = select(cat, -'adoptionYN')
cat_trainy = cat['adoptionYN']

cat_trainx_dum = dummy_cols(cat_trainx, select_columns = cat_cate, remove_first_dummy = TRUE)
cat_trainx_dum = select(cat_trainx_dum, -cat_cate)
cat_trainx_dum[cat_nume] = scale(cat_trainx_dum[cat_nume], center = TRUE, scale = TRUE)
cat_data = cbind(cat_trainy, cat_trainx_dum)

set.seed(613)
ml_logistic = glm(adoptionYN ~ ., cat_data, family = binomial)
pro_logistic <- predict(ml_logistic, newdata=test_cat[,-1] , type="response")
lgi_pred_3 = prediction(pro_logistic,test_cat$adoptionYN)
perf_lgi = performance(lgi_pred_3, measure = "tpr", x.measure="fpr")
plot(perf_lgi,col="blue")
performance(lgi_pred_3, "auc")@y.values[[1]] #0.6868881


## 모아서 그리기
plot(perf_rf,col="#EA2C62", main="ROC :: CAT",lwd="3")
plot(perf_xgb,col="#B3E2CD",add=T,lwd="3")
plot(perf_lgi,col="#E3D955",add=T,lwd="2")
abline(0,1)
legend('bottomright',
       legend=c("RandomForest","XGBoost","Logistic"),
       col=c("#EA2C62","#B3E2CD","#E3D955"),inset=.01, lty=1,lwd=5)

## 모아서 그리기
plot(perf_rf,col="#EA2C62", main="ROC :: CAT",lwd="3")
plot(perf_xgb,col="#B3E2CD",add=T,lwd="3")
plot(perf_lgi,col="#E3D955",add=T,lwd="2")
abline(0,1)
