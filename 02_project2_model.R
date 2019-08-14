
##Classification


######### NaiveBayes Model ##########

bayes_model <- NaiveBayes(fake_admission2~., data=train_1)
plot(bayes_model)

cal_train <- vector()
cal_test <- vector()


for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_1 <- analysis1[sampleNum,]
  test_1 <- analysis1[-sampleNum,]
  
  #model
  bayes_model <- NaiveBayes(fake_admission2~., data=train_1)
  
  #train accuracy
  model_accuracy <- predict(bayes_model, newdata = train_1)
  acc <- table(Actual = train_1$fake_admission2, Predicted = model_accuracy$class)
  cal_train[i] <- sum(diag(acc)/sum(acc))
  
  #test accuracy
  bayes.pred <- predict(bayes_model, newdata=test_1)
  tab <- table(Actual = test_1$fake_admission2, Predicted = bayes.pred$class)
  cal_test[i] <- sum(diag(tab)/sum(tab))
  
}
mean(cal_train); mean(cal_test)
##train::  0.8847342 
##test::   0.8876259




for (i in 1:10){
  #data 
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_1 <- analysis1[sampleNum,]
  test_1 <- analysis1[-sampleNum,]
  
  #model
  bayes_model <- NaiveBayes(fake_admission2 ~ fake_wait #+ fake_month #+ fake_logcis
                            #+ fake_sex
                            #+ fake_marital #+ fake_spec
                            #+ fake_year
                            #+ fake_dgroup
                            #+ fake_sigfac
                            , data=train_1)
  
  #train accuracy
  model_accuracy <- predict(bayes_model, newdata = train_1)
  acc <- table(Actual = train_1$fake_admission2, Predicted = model_accuracy$class)
  cal_train[i] <- sum(diag(acc)/sum(acc))
  
  #test accuracy
  bayes.pred <- predict(bayes_model, newdata=test_1)
  tab <- table(Actual = test_1$fake_admission2, Predicted = bayes.pred$class)
  cal_test[i] <- sum(diag(tab)/sum(tab))
}
mean(cal_train); mean(cal_test)

##1.Without fake_normage     2.Without fake_sigfac    3.Without fake_year
##Train:: 0.8873563            Train:: 0.8891882         Train:: 0.8860273 
##Test:: 0.8853237             Test:: 0.8798561          Test:: 0.8884892

##4.Without fake_sex         5.Without fake_dgroup    6.Without fake_spec
##Train::  0.890158           Train:: 0.9001796         Train:: 0.9012572
##Test:: 0.8853237            Test:: 0.9004317          Test:: 0.8976978

##7.Without fake_logcis     8.Without fake_month      9.Without fake_marital
##Train::  0.9001437          Train:: 0.9007184         Train:: 0.9015805
##Test:: 0.9021583            Test:: 0.8998561          Test:: 0.8964029





######### Randomforest Model ##########

randfor_model_classify <- randomForest(fake_admission2 ~  ., data = train_1)
varImpPlot(randfor_model_classify, main = "Feature Importance ( Random Forest :: Classification )")
importance(randfor_model_classify)




### variable selection ###

for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_1 <- analysis1[sampleNum,]
  test_1 <- analysis1[-sampleNum,]
  
  #model
  randfor_model_classify = randomForest(fake_admission2 ~  ., data = train_1)
  
  #train accuracy
  model_accuracy <- predict(randfor_model_classify, newdata = train_1)
  acc <- table(Actual = train_1$fake_admission2, Predicted = model_accuracy)
  cal_train[i] <-  sum(diag(acc)/sum(acc))
  
  #test accuracy
  randfor.pred_classify <- predict(randfor_model_classify, newdata = test_1)
  tab <- table(Actual = test_1$fake_admission2, Predicted = randfor.pred_classify)
  cal_test[i] <- sum(diag(tab)/sum(tab))
  
}
mean(cal_train); mean(cal_test)
##Train:: 0.9997486
##Test::  0.9041727



for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_1 <- analysis1[sampleNum,]
  test_1 <- analysis1[-sampleNum,]
  
  #model
  randfor_model_classify = randomForest(fake_admission2 ~ fake_wait + fake_dgroup
                                        + fake_spec #+ fake_normage
                                        #+ fake_sigfac
                                        #+ fake_year #+ fake_month
                                        #+ fake_logcis #+ fake_marital
                                        , data = train_1)
  
  #train accuracy
  model_accuracy <- predict(randfor_model_classify, newdata = train_1)
  acc <- table(Actual = train_1$fake_admission2, Predicted = model_accuracy)
  cal_train[i] <-  sum(diag(acc)/sum(acc))
  
  #test accuracy
  randfor.pred_classify <- predict(randfor_model_classify, newdata = test_1)
  tab <- table(Actual = test_1$fake_admission2, Predicted = randfor.pred_classify)
  cal_test[i] <- sum(diag(tab)/sum(tab))
  
}
mean(cal_train); mean(cal_test)
##1.Without fake_sex     2.Without fake_marital    3.Without fake_logcis
##Train:: 0.9997845        Train:: 0.995977          Train:: 0.9922414 
##Test:: 0.9067626         Test:: 0.9056115          Test:: 0.903741

##4.Without fake_month   5.Without fake_sigfac    6.Without fake_year
##Train:: 0.9751078        Train:: 0.9857759        Train:: 0.9607759
##Test:: 0.9011511        Test:: 0.8844604          Test:: 0.8823022

##4.Without fake_normage 
##Train:: 0.8996767    
##Test::  0.9041727      




######### SVM Model ##########

tuned_svm <- train(fake_admission2 ~ .,
                   data = train_1,
                   method = "svmRadial", 
                   tuneGrid = expand.grid(C = c(1:10), sigma = seq(0.1, 1, 0.1)))
#sigma 0.1, C 1 

svm_model <- fit(fake_admission2~., 
                 data = train_1, model="svm", kpar=list(sigma=0.1), C=1)
svmimp <- Importance(svm_model, data=train_1, method = "sensv")
L=list(runs=1,sen=t(svmimp$imp),sresponses=svmimp$sresponses)
mgraph(L,graph="IMP",leg=names(train_1),col="gray",Grid=10)
mgraph(L,graph="VEC",xval=1,Grid=10)





### variable selection ###

for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_1 <- analysis1[sampleNum,]
  test_1 <- analysis1[-sampleNum,]
  
  #model
  svm_model_classify <- ksvm(fake_admission2 ~ ., data = train_1,
                             kernel = "rbfdot", kpar = list(sigma=0.1),
                             C = 1, prob.model=TRUE)
  
  #train accuracy
  model_accuracy <- predict(svm_model_classify , newdata = train_1)
  acc <- table(Actual = train_1$fake_admission2, Predicted = model_accuracy)
  cal_train[i] <-  sum(diag(acc)/sum(acc))
  
  #test accuracy
  svm.pred_classify <- predict(svm_model_classify, newdata=test_1)
  tab <- table(Actual = test_1$fake_admission2, Predicted = svm.pred_classify)
  cal_test[i] <- sum(diag(tab)/sum(tab))
  
}
mean(cal_train); mean(cal_test)
##Train:: 0.9085848
##Test:: 0.9066187



for (i in 1:10){
  #data
  sampleNum <- sample(nrow(dat), n1+1, replace=FALSE ) 
  train_1 <- analysis1[sampleNum,]
  test_1 <- analysis1[-sampleNum,]
  
  #model
  svm_model_classify <- ksvm(fake_admission2 ~  fake_wait# + fake_sigfac
                               #+ fake_logcis #+ fake_dgroup
                               #+ fake_month #+ fake_year
                               #+ fake_sex
                               #+ fake_spec
                               #+ fake_normage
                              , data = train_1,
                             kernel = "rbfdot", kpar = list(sigma=0.1),
                             C = 1, prob.model=TRUE)
  
  #train accuracy
  model_accuracy <- predict(svm_model_classify , newdata = train_1)
  acc <- table(Actual = train_1$fake_admission2, Predicted = model_accuracy)
  cal_train[i] <-  sum(diag(acc)/sum(acc))
  
  #test accuracy
  svm.pred_classify <- predict(svm_model_classify, newdata=test_1)
  tab <- table(Actual = test_1$fake_admission2, Predicted = svm.pred_classify)
  cal_test[i] <- sum(diag(tab)/sum(tab))
  
}
mean(cal_train); mean(cal_test)
##1.Without fake_marital       2.without fake_spec      3.without fake_normage
##Train:: 0.9072198             Train:: 0.9075431        Train:: 0.9063937
##Test:: 0.9070504              Test::  0.9054676        Test::  0.9084892

##4.without fake_sex           5. without fake_year     6.without fake_dgroup        
##Train::  0.9079382              Train:: 0.9072917       Train:: 0.9073635
##Test::  0.9023022               Test::  0.9046043       Test::  0.9038849

##7.without fake_month         8.without fake_logcis    9.without fake_sigfac
##Train:: 0.9073276              Train:: 0.9095905        Train:: 0.9015805
##Test:: 0.9063309               Test:: 0.9043165         Test:: 0.8964029


