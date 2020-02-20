library(randomForest)
library(plyr)

set.seed(123)

folds <- split(training, cut(sample(1:nrow(training)),5))

errs.rf <- rep(NA, length(folds))

wts <- 50 / table(training$premacat)

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- randomForest(premacat ~
  partoantpt+ampt+qtcpn3t+qtcpn2t+localnp+pn+malcoolg2t+hosp+recanemia+outrad+hipertgest
  +nfilhos+idademae+habitofumo+ativrem+fumograv+mes1cpn+nuspn, train, ntree=500, classwt = wts)
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$premacat, tmp.predict)
  errs.rf[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error: %.3f percent", 100*mean(errs.rf)))

confusionMatrix(conf.mat, positive = '1')

plot(tmp.model)

