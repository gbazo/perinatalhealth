library("e1071")

set.seed(123)

folds <- split(data, cut(sample(1:nrow(data)),5))

errs.nb <- rep(NA, length(folds))

wts <- 50 / table(data$premacat)

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- naiveBayes(premacat ~
  partoantpt+ampt+qtcpn3t+qtcpn2t+localnp+pn+malcoolg2t+causahosp+recanemia+doencagest
  +hipertgest+nfilhos+idademae+habitofumo+paroutrab+fumograv+mes1cpn+nuspn,train, weights = wts)
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$premacat, tmp.predict)
  errs.nb[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error: %.3f percent", 100*mean(errs.nb)))

confusionMatrix(conf.mat, positive = '1')
