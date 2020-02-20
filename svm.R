library(tidyverse)
library(caret)
library("e1071")

set.seed(123)

folds <- split(data, cut(sample(1:nrow(data)),5))

errs.svm <- rep(NA, length(folds))

wts <- 50 / table(train$premacat)

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- svm(premacat ~
  partoantpt+ampt+qtcpn3t+qtcpn2t+localnp+pn+malcoolg2t+causahosp+recanemia+doencagest
  +hipertgest+nfilhos+idademae+habitofumo+paroutrab+fumograv+mes1cpn+nuspn, train,
  probability = TRUE, class.weights = wts, kernel = "radial", epsilon = 0.1, cost = 4)
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$premacat, tmp.predict)
  errs.svm[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

tmodel <- tune(svm, premacat ~ partoantpt+ampt+qtcpn3t+fetos+localnp+recanemia+hipertgest+mothosp,
data = train[], class.weights = wts, ranges = list(epsolon = seq(0,1,0.1), cost = 2^(1:6)))

plot(tmodel)

summary(tmodel)

model <- tmodel$best.model

tmp.predict <- predict(model, newdata=test)

conf.mat <- table(test$premacat, tmp.predict)

print(sprintf("average error: %.3f percent", 100*mean(errs.svm)))

confusionMatrix(conf.mat, positive = '1')

