
##Regression


######### Randomforest Model ##########

randfor_model_regress <- randomForest(fake_cubelos ~  ., data = train_2)
varImpPlot(randfor_model_regress,  main = "Feature Importance ( Random Forest :: Regression )")
importance(randfor_model_regress)



### variable selection ###

mae_train <- vector()
rmse_train <- vector()
mae_test <- vector()
rmse_test <- vector()


for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_2 <- analysis2[sampleNum,]
  test_2 <- analysis2[-sampleNum,]
  
  #model
  randfor_model_regress <- randomForest(fake_cubelos ~  ., data = train_2)
  
  ##train mae rmse
  train_pred <- predict(randfor_model_regress, newdata = train_2)
  mae_train[i] <- MAE(train_2$fake_cubelos, train_pred)
  rmse_train[i] <- RMSE(train_2$fake_cubelos, train_pred)
  
  ##test mae rmse
  test_pred <- predict(randfor_model_regress, newdata = test_2)
  mae_test[i] <- MAE(test_2$fake_cubelos, test_pred)
  rmse_test[i] <- RMSE(test_2$fake_cubelos, test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)
## Train  MAE:0.1885831  RMSE:0.3063916
## Test   MAE:0.3864168  RMSE:0.6256671

g <- ggplot(test_2, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( Random Forest )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/1_pred_randfor.png")




for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_2 <- analysis2[sampleNum,]
  test_2 <- analysis2[-sampleNum,]
  
  #model
  randfor_model_regress <- randomForest(fake_cubelos ~ fake_ipdc + fake_spec
                                        + fake_normage + fake_wait + fake_dgroup
                                        + fake_year# + fake_logcis #+ fake_month
                                        #+ fake_marital 
                                        #+ fake_admission2
                                        #+ fake_sigfac
                                        , data = train_2)
  
  ##train mae rmse
  train_pred <- predict(randfor_model_regress, newdata = train_2)
  mae_train[i] <- MAE(train_2$fake_cubelos, train_pred)
  rmse_train[i] <- RMSE(train_2$fake_cubelos, train_pred)
  
  ##test mae rmse
  test_pred <- predict(randfor_model_regress, newdata = test_2)
  mae_test[i] <- MAE(test_2$fake_cubelos, test_pred)
  rmse_test[i] <- RMSE(test_2$fake_cubelos, test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)

##1.Without fake_sex           2.Without fake_sigfac        3.Without fake_marital
##Train:: MAE 0.1988346        Train:: MAE 0.197879         Train:: MAE  0.1990543 
##       RMSE 0.3207215               RMSE 0.3214114                RMSE 0.324886
##Test:: MAE 0.4045049         Test:: MAE  0.3997307        Test::  MAE  0.398263
##       RMSE 0.652487                RMSE  0.6461782               RMS  0.6330645

##4.Without fake_admission2    5.Without fake_month         6.Without fake_logcis
##Train:: MAE 0.2247052        Train:: MAE 0.2361814        Train:: MAE 0.2536661  
##       RMSE 0.3613557               RMSE 0.3825668                RMSE 0.4123664
##Test:: MAE 0.3858308         Test:: MAE 0.3955023         Test::  MAE 0.3970795  
##       RMSE 0.6189437               RMSE 0.6325718                RMSE 0.6398887

##7.Without fake_dgroup        
##Train:: MAE 0.3680128         
##       RMSE 0.5859398         
##Test:: MAE  0.402982          
##       RMSE 0.6345724     


g <- ggplot(test_2, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( Random Forest )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/1_pred_randfor_varselect.png")








######### SVM Model ##########

tuned_svm <- train(fake_cubelos ~ .,
                   data = train_2,
                   method = "svmRadial", 
                   tuneGrid = expand.grid(C = c(1:10), sigma = seq(0.1, 1, 0.1)))
#sigma 0.1, C 1 

svm_model_regress <- ksvm(fake_cubelos ~ ., 
                          data = train_2, kernel = "rbfdot",
                          kpar = list(sigma=0.1), C = 1)


svm_model <- fit(fake_cubelos ~., data = train_2, model="svm", kpar=list(sigma=0.1), C=1)
svmimp <- Importance(svm_model, data=train_2, method = "sensv")
L=list(runs=1,sen=t(svmimp$imp),sresponses=svmimp$sresponses)
mgraph(L,graph="IMP",leg=names(train_2),col="gray",Grid=10)
mgraph(L,graph="VEC",xval=1,Grid=10)





### variable selection ###

for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_2 <- analysis2[sampleNum,]
  test_2 <- analysis2[-sampleNum,]
  
  #model
  svm_model_regress <- ksvm(fake_cubelos ~ ., 
                            data = train_2, kernel = "rbfdot",
                            kpar = list(sigma=0.1), C = 1)
  
  ##train mae rmse
  train_pred <- predict(svm_model_regress, newdata = train_2)
  mae_train[i] <- MAE(train_2$fake_cubelos, train_pred)
  rmse_train[i] <- RMSE(train_2$fake_cubelos, train_pred)
  
  ##test mae rmse
  test_pred <- predict(svm_model_regress, newdata = test_2)
  mae_test[i] <- MAE(test_2$fake_cubelos, test_pred)
  rmse_test[i] <- RMSE(test_2$fake_cubelos, test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)

## Train  MAE:0.3228376  RMSE:0.5500205
## Test   MAE:0.4081027  RMSE:0.6405417


g <- ggplot(test_2, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( SVM )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/2_pred_svm.png")




for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_2 <- analysis2[sampleNum,]
  test_2 <- analysis2[-sampleNum,]
  
  #model
  svm_model_regress <- ksvm(fake_cubelos ~ fake_ipdc + fake_normage 
                            + fake_dgroup + fake_marital + fake_wait
                            + fake_sigfac 
                            + fake_spec #+ fake_month
                            #+ fake_logcis
                            + fake_year # + fake_sex
                            , data = train_2, kernel = "rbfdot",
                            kpar = list(sigma=0.1), C = 1)
  
  ##train mae rmse
  train_pred <- predict(svm_model_regress, newdata = train_2)
  mae_train[i] <- MAE(train_2$fake_cubelos, train_pred)
  rmse_train[i] <- RMSE(train_2$fake_cubelos, train_pred)
  
  ##test mae rmse
  test_pred <- predict(svm_model_regress, newdata = test_2)
  mae_test[i] <- MAE(test_2$fake_cubelos, test_pred)
  rmse_test[i] <- RMSE(test_2$fake_cubelos, test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)

##1.Without fake_admission2     2.Without fake_sex         3.Without fake_logcis
##Train:: MAE 0.3307096         Train:: MAE 0.3316201        Train::  MAE 0.3402649  
##       RMSE 0.5621262                 RMSE 0.5631769                RMSE 0.573435
##Test:: MAE 0.3917445          Test:: MAE 0.4052201         Test::  MAE  0.3977156 
##       RMSE 0.6232769                RMSE 0.6420189                RMS  0.6367758

##4.Without fake_month         5.Without fake_sigfac       6.Without fake_year
##Train:: MAE 0.3483351       Train:: MAE 0.3536008          Train::  MAE 0.3598997   
##        RMSE 0.5842315              RMSE 0.5897627                  RMSE  0.5972279
##Test:: MAE 0.3886623         Test:: MAE 0.3842083          Test:: MAE 0.3903091  
##       RMSE 0.6268074               RMSE 0.6231599                RMS 0.6284713

##7.Without fake_spec         8.Without fake_wait           9.Without fake_normage
##Train:: MAE 0.3714914        Train:: MAE 0.3745171          Train::  MAE 0.3925115   
##       RMSE 0.610711                 RMSE 0.6147273                  RMSE 0.6368902 
##Test:: MAE 0.3891091         Test:: MAE 0.3896279           Test::  MAE 0.4017174  
##       RMSE 0.6389514               RMSE 0.6384434                  RMS 0.6457788

##10.Without fake_marital         8.Without fake_dgroup           
##Train:: MAE 0.3958701        Train:: MAE  0.4229682          
##       RMSE 0.6418558                RMSE 0.659562      
##Test:: MAE 0.4062344         Test:: MAE  0.4101999          
##       RMSE 0.6556514              RMSE 0.6372409                 


g <- ggplot(test_2, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( SVM )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/2_pred_svm_varselect.png")







######### Neural Network Model ##########


for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_2 <- analysis2[sampleNum,]
  test_2 <- analysis2[-sampleNum,]
  
  #model
  nn_model <- nnet(fake_cubelos~., data=train_2, size=3, linout=TRUE, act.fct = "relu")
  
  ##train mae rmse
  train_pred <- predict(nn_model, newdata = train_2)
  mae_train[i] <- MAE(train_2$fake_cubelos, train_pred)
  rmse_train[i] <- RMSE(train_2$fake_cubelos, train_pred)
  
  ##test mae rmse
  test_pred <- predict(nn_model, newdata = test_2)
  mae_test[i] <- MAE(test_2$fake_cubelos, test_pred)
  rmse_test[i] <- RMSE(test_2$fake_cubelos, test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)

## Train  MAE:0.3556123  RMSE:0.5664145
## Test   MAE:0.4315452  RMSE:0.6872336


g <- ggplot(test_2, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( Neural Network )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/3_pred_nnet.png")




for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_2 <- analysis2[sampleNum,]
  test_2 <- analysis2[-sampleNum,]
  
  #model
  nn_model <- nnet(fake_cubelos~ fake_ipdc + fake_normage 
                   + fake_dgroup 
                   #+ fake_marital
                   + fake_wait
                   + fake_sigfac + fake_spec #+ fake_month
                   + fake_logcis# + fake_year
                   + fake_admission2 #+ fake_sex
                   , data=train_2, size=3, linout=TRUE, act.fct = "relu")
  
  ##train mae rmse
  train_pred <- predict(nn_model, newdata = train_2)
  mae_train[i] <- MAE(train_2$fake_cubelos, train_pred)
  rmse_train[i] <- RMSE(train_2$fake_cubelos, train_pred)
  
  ##test mae rmse
  test_pred <- predict(nn_model, newdata = test_2)
  mae_test[i] <- MAE(test_2$fake_cubelos, test_pred)
  rmse_test[i] <- RMSE(test_2$fake_cubelos, test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)




##1.Without fake_year     1.Without fake_month         2.Without fake_sex
##Train:: MAE 0.3615235         Train:: MAE 0.360552     Train::  MAE  0.3574222
##       RMSE 0.5762679                 RMSE 0.5788852                RMSE 0.5690467
##Test:: MAE 0.5787875          Test:: MAE 0.4207695         Test::  MAE 0.4321756
##       RMSE 2.890397                RMSE 0.6871949              RMS 0.7145232

##4.Without fake_marital         5.Without fake_    6.Without fake_
##Train:: MAE 0.3547512       Train:: MAE          Train::  MAE   
##       RMSE 0.5696704               RMSE                  RMSE 
##Test:: MAE 0.4694326         Test:: MAE          Test::   MAE   
##       RMSE 1.034685                RMSE                  RMS






g <- ggplot(test_2, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( Neural Network )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/3_pred_nnet_varselect.png")













