library(caret)

set.seed(123)

indxTrain <- createDataPartition(y = data$premacat, p = 0.8, list = FALSE)

training <- data[indxTrain,]

testing <- data[-indxTrain,]

prop.table(table(training$premacat)) * 100

prop.table(table(testing$premacat)) * 100

trainX <- training[,names(training) != "premacat"]

preProcValues <- preProcess(x = trainX,method = c("center", "scale"))

attach(training)

train = data.frame(partoantpt,ampt,qtcpn3t,qtcpn2t,localnp,pn,malcoolg2t,causahosp,recanemia,doenc
agest,hipertgest,nfilhos,idademae,habitofumo,paroutrab,fumograv,mes1cpn,nuspn)

training = data.frame(scale(train))

model <- train(premacat ~ ., train, method = "knn", trControl = trainControl(method = "cv",
number = 5, verboseIter = TRUE))

knnPredict <- predict(model, newdata = testing)

confusionMatrix(knnPredict, testing$premacat, positive = '1')
