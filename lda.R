library(MASS)
library(plyr)

set.seed(123)

folds <- split(data, cut(sample(1:nrow(data)),5))

errs.lda <- rep(NA, length(folds))

wts <- 50 / table(data_na$premacat)

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- lda(premacat ~
  partoantpt+ampt+qtcpn3t+qtcpn2t+localnp+pn+malcoolg2t+causahosp+recanemia+doencagest
  +hipertgest+nfilhos+idademae+habitofumo+paroutrab+fumograv+mes1cpn+nuspn,data, method = "mle")
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$premacat, tmp.predict$class)
  errs.lda[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error: %.3f percent", 100*mean(errs.lda)))

confusionMatrix(conf.mat, positive = '1')

