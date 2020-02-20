library(partykit)
library(plyr)
library(caret)

form <- "premacat ~ partoantpt+ampt+qtcpn3t+qtcpn2t+localnp+pn+malcoolg2t+causahosp+recanemia+doencagest
+hipertgest+nfilhos+idademae+habitofumo+paroutrab+fumograv+mes1cpn+nuspn"

set.seed(123)

folds <- split(data_na, cut(sample(1:nrow(data_na)),5))

errs.ctree <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- ctree(as.formula(form), train, weights = ifelse(train$premacat=='0', 0.3, 1),
  control = ctree_control(maxdepth = 4, multiway = TRUE))
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$premacat, tmp.predict)
  errs.ctree[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error: %.3f percent", 100*mean(errs.ctree)))

plot(tmp.model)

confusionMatrix(conf.mat, positive = '1')

