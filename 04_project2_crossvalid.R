

####### cross validation ########

###classify

folds <- createFolds(analysis1$fake_admission2, k = 5)

accuracy <- vector()
cv_acc <- vector()




## Naive Bayes

for (i in 1:5){
  train_cv <- analysis1[-folds[[i]],]
  test_cv <- analysis1[folds[[i]],]
  bayes_model <- NaiveBayes(fake_admission2 ~ fake_wait, data=train_cv)
  #train
  train_acc <- predict(bayes_model, newdata=train_cv)
  tab_acc <- table(Actual = train_cv$fake_admission2, Predicted = train_acc$class)
  accuracy[i] <- sum(diag(tab_acc)/sum(tab_acc))
  #test
  pred_cv <- predict(bayes_model, newdata=test_cv)
  tab_cv <- table(Actual = test_cv$fake_admission2, Predicted = pred_cv$class)
  cv_acc[i] <- sum(diag(tab_cv)/sum(tab_cv))
}
mean(accuracy)
mean(cv_acc)
##Train 0.900546
##Test  0.9005436


x <- data.frame(
  Number  = 1:length(cv_acc),
  Accuracy_rate = cv_acc
)

g <- ggplot(x, aes(x=Number, y=Accuracy_rate))
g <- g + geom_line() + geom_point()
g <- g + ggtitle("Accuracy of Naive Bayes") + labs(y="Accuracy")
plot(g)



## Random Forest

for (i in 1:5){
  train_cv <- analysis1[-folds[[i]],]
  test_cv <- analysis1[folds[[i]],]
  randfor_model_classify = randomForest(fake_admission2 ~ fake_wait + fake_dgroup
                                        + fake_spec + fake_normage, data = train_cv)
  #train
  train_acc <- predict(randfor_model_classify, newdata=train_cv)
  tab_acc <- table(Actual = train_cv$fake_admission2, Predicted = train_acc)
  accuracy[i] <- sum(diag(tab_acc)/sum(tab_acc))
  #test
  pred_cv <- predict(randfor_model_classify, newdata=test_cv)
  tab_cv <- table(Actual = test_cv$fake_admission2, Predicted = pred_cv)
  cv_acc[i] <- sum(diag(tab_cv)/sum(tab_cv))
}
mean(accuracy)
mean(cv_acc)
##Train 0.9583216
##Test 0.8881848


x <- data.frame(
  Number  = 1:length(cv_acc),
  Accuracy_rate = cv_acc
)

g <- ggplot(x, aes(x=Number, y=Accuracy_rate))
g <- g + geom_line() + geom_point()
g <- g + ggtitle("Accuracy of Random Forest") + labs(y="Accuracy")
plot(g)




## Support Vector Machine

for (i in 1:5){
  train_cv <- analysis1[-folds[[i]],]
  test_cv <- analysis1[folds[[i]],]
  svm_model_classify <- ksvm(fake_admission2 ~  fake_wait + fake_sigfac, data = train_cv,
                             kernel = "rbfdot", kpar = list(sigma=0.1),
                             C = 1, prob.model=TRUE)
  #train
  train_acc <- predict(svm_model_classify, newdata=train_cv)
  tab_acc <- table(Actual = train_cv$fake_admission2, Predicted = train_acc)
  accuracy[i] <- sum(diag(tab_acc)/sum(tab_acc))
  #test
  pred_cv <- predict(svm_model_classify, newdata=test_cv)
  tab_cv <- table(Actual = test_cv$fake_admission2, Predicted = pred_cv)
  cv_acc[i] <- sum(diag(tab_cv)/sum(tab_cv))
}
mean(accuracy)
mean(cv_acc)
#0.9085945
#0.9060059


x <- data.frame(
  Number  = 1:length(cv_acc),
  Accuracy_rate = cv_acc
)

g <- ggplot(x, aes(x=Number, y=Accuracy_rate))
g <- g + geom_line() + geom_point()
g <- g + ggtitle("Accuracy of SVM") + labs(y="Accuracy")
plot(g)



cols <- brewer.pal(5, "Set1")


auc_bayes <- vector()

for (i in 1:5){
  #data
  train_cv <- analysis1[-folds[[i]],]
  test_cv <- analysis1[folds[[i]],]
  #model
  bayes_model <- NaiveBayes(fake_admission2 ~ fake_wait, data=train_cv)
  bayes.pred <- predict(bayes_model, newdata=test_cv)
  #AUC
  roc.curve1 <- roc(response = test_cv$fake_admission2, predictor=bayes.pred$posterior[,"Urgent & Emergency Admission"], 
                  levels = c("Routine Admission","Urgent & Emergency Admission"))
  auc_bayes[i] <- auc(roc.curve1)
  
  par(new=T)
  plot(roc.curve1, col=cols[1], lwd=0.1)
}

mean(auc_bayes)
#0.912


auc_randfor <- vector()

for (i in 1:5){
  #data
  train_cv <- analysis1[-folds[[i]],]
  test_cv <- analysis1[folds[[i]],]
  #model
  randfor_model_classify = randomForest(fake_admission2 ~ fake_wait + fake_dgroup
                                        + fake_spec + fake_normage, data = train_cv)
  randfor.pred <- predict(randfor_model_classify, newdata = test_cv, type="prob")
  #AUC
  roc.curve2 <- roc(response = test_cv$fake_admission2, predictor=randfor.pred[,"Urgent & Emergency Admission"], 
                    levels = c("Routine Admission","Urgent & Emergency Admission"))
  auc_randfor[i] <- auc(roc.curve2)
  
  plot(roc.curve2, col=cols[2], lwd=0.1, add = TRUE)
}
mean(auc_randfor)
#0.940919



auc_svm <- vector()

for (i in 1:5){
  #data
  train_cv <- analysis1[-folds[[i]],]
  test_cv <- analysis1[folds[[i]],]
  #model
  svm_model_classify <- ksvm(fake_admission2 ~  fake_wait + fake_sigfac, data = train_cv,
                             kernel = "rbfdot", kpar = list(sigma=0.1),
                             C = 1, prob.model=TRUE)
  svm.pred <- predict(svm_model_classify, newdata=test_cv, type="prob")
  #AUC
  roc.curve3 <- roc(response = test_cv$fake_admission2, predictor=svm.pred[,"Urgent & Emergency Admission"], 
                    levels = c("Routine Admission","Urgent & Emergency Admission"))
  auc_svm[i] <- auc(roc.curve3)
  
  plot(roc.curve3, col=cols[3], lwd=0.1, add = TRUE)
}
mean(auc_svm)
# 0.9253718


legend(x = 0.7, y = 0.3, bty = "n", col = c(cols[1],cols[2],cols[3]), lwd = 2,
       legend = c("Naive Bayes","Random Forest","SVM"))









###regression

mae_train <- vector()
rmse_train <- vector()
mae_test <- vector()
rmse_test <- vector()


### Random Forest

for (i in 1:5){
  train_cv <- analysis2[-folds[[i]],]
  test_cv <- analysis2[folds[[i]],]
  randfor_model_regress <- randomForest(fake_cubelos ~  ., data = train_cv)
  #train
  train_pred <- predict(randfor_model_regress, newdata=train_cv)
  mae_train[i] <- MAE(train_cv$fake_cubelos,train_pred)
  rmse_train[i] <- RMSE(train_cv$fake_cubelos,train_pred)
  #test
  test_pred <- predict(randfor_model_regress, newdata=test_cv)
  mae_test[i] <- MAE(test_cv$fake_cubelos,test_pred)
  rmse_test[i] <- RMSE(test_cv$fake_cubelos,test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)
#0.1881151 0.3063898 0.392701 0.6325929


g <- ggplot(test_cv, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5, alpha=0.8) + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( Random Forest )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/pred1.png")



###Support Vector Machine

for (i in 1:5){
  train_cv <- analysis2[-folds[[i]],]
  test_cv <- analysis2[folds[[i]],]
  svm_model_regress <- ksvm(fake_cubelos ~ fake_ipdc + fake_normage 
                            + fake_dgroup + fake_marital + fake_wait
                            + fake_spec + fake_year
                            , data = train_cv, kernel = "rbfdot",
                            kpar = list(sigma=0.1), C = 1)
  #train
  train_pred <- predict(svm_model_regress, newdata=train_cv)
  mae_train[i] <- MAE(train_cv$fake_cubelos,train_pred)
  rmse_train[i] <- RMSE(train_cv$fake_cubelos,train_pred)
  #test
  test_pred <- predict(svm_model_regress, newdata=test_cv)
  mae_test[i] <- MAE(test_cv$fake_cubelos,test_pred)
  rmse_test[i] <- RMSE(test_cv$fake_cubelos,test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)
#0.352448 0.5886895 0.3888046 0.627809

g <- ggplot(test_cv, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5, alpha=0.8)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( SVM )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/pred2.png")




###Neural Network

for (i in 1:5){
  train_cv <- analysis2[-folds[[i]],]
  test_cv <- analysis2[folds[[i]],]
  nn_model <- nnet(fake_cubelos~., data=train_cv, size=3, linout=TRUE, act.fct = "relu")
  #train
  train_pred <- predict(nn_model, newdata=train_cv)
  mae_train[i] <- MAE(train_cv$fake_cubelos,train_pred)
  rmse_train[i] <- RMSE(train_cv$fake_cubelos,train_pred)
  #test
  test_pred <- predict(nn_model, newdata=test_cv)
  mae_test[i] <- MAE(test_cv$fake_cubelos,test_pred)
  rmse_test[i] <- RMSE(test_cv$fake_cubelos,test_pred)
}
mean(mae_train); mean(rmse_train); mean(mae_test); mean(rmse_test)
#0.3616798 0.5712396 0.4286382 0.6759062

g <- ggplot(test_cv, aes(x =  test_pred , y = fake_cubelos))
g <- g + geom_point( col = c("#685CE6"), size = 1.5, alpha=0.8)  + geom_abline(intercept = 0)
g <- g + ggtitle("Test data vs Prediction ( Neural Network )") + xlab("Prediction") + ylab("Test Data")
ggsave("../../plot/report/pred3.png")

















