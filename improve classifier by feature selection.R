library(RWeka)
library(foreign)
library(MASS)
setwd("C:/Users/Kevin/Desktop/機器學習/hw3/wave_2_classes_with_irrelevant_attributes")
mydata <- read.arff(file("wave_2_classes_with_irrelevant_attributes.arff","r"))
tr_mydata <- read.arff(file("wave_2_classes_with_irrelevant_attributes.train.arff","r"))
te_mydata <- read.arff(file("wave_2_classes_with_irrelevant_attributes.test.arff","r"))
str(mydata)
str(mydata[,c("classe","sample")])
summary(mydata[,c("classe","sample")])
head(mydata)
nrow(tr_mydata)
nrow(te_mydata)
model <- glm(classe~.,data=tr_mydata,family=binomial)
summary(model)
proba <- predict(model,newdata = te_mydata,type="response")
pred <- ifelse(proba < 0.5,1,2)
pred <- factor(pred)
cm <- table(te_mydata$classe,pred,dnn = c("實際","預測"))
cm
accuracy <- (sum(diag(cm)))/(sum(cm))
accuracy
errorrate <- 1-accuracy
errorrate



model.initial <- glm(classe~1,data=tr_mydata,family = binomial)
model.forward <- stepAIC(model.initial,scope=list(lower="classe~1",
  upper=as.formula(model)),trace=TRUE,direction = "forward",k=log(nrow(tr_mydata)))
finalmodel <- glm(classe ~ v10 + v17 + v11 + v9 + v16 + v12 + v18 + v15 + v5 + 
    v8 + v4 + v19 + v13 + v20 + v14,data=tr_mydata,family = binomial)
summary(finalmodel)
fproba <- predict(finalmodel,newdata = te_mydata,type="response")
fpred <- ifelse(fproba < 0.5,1,2)
fpred <- factor(fpred)
fcm <- table(te_mydata$classe,fpred,dnn = c("實際","預測"))
fcm
faccuracy <- (sum(diag(fcm)))/(sum(fcm))
faccuracy
ferrorrate <- 1-faccuracy
ferrorrate

