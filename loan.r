train <- read.csv("C:\\Users\\431539\\Desktop\\Loan\\train.csv",na.strings = "") #614

test<- read.csv("C:\\Users\\431539\\Desktop\\Loan\\test.csv",na.strings = "") #367

str(train)
summary(train)

train= train[,-1]        

id=test[,1]
test=test[,-1]

test$Loan_Status = NA

loan = rbind(train,test)
str(loan)

table(loan$Gender)
# 
# loan$Gender[which(loan$Gender=="")] <- NA
# loan$Gender <- as.character(loan$Gender)
# loan$Gender[is.na(loan$Gender)]= 'Male'
# loan$Gender <- as.factor(loan$Gender)
# 
# table(loan$Married)
# loan$Married[which(loan$Married=="")] <- NA
# loan$Married <- as.character(loan$Married)
# loan$Married[is.na(loan$Married)]='Yes'
# loan$Married <- as.factor(loan$Married)
# 
# table(loan$Dependents)
# loan$Dependents =as.character(loan$Dependents)
# loan$Dependents[which(loan$Dependents=="")]<- 0
# loan$Dependents[which(loan$Dependents=="3+")]<- 3
# loan$Dependents =as.factor(loan$Dependents)
# 
# table(loan$Self_Employed)
# loan$Self_Employed[which(loan$Self_Employed=="")] <- NA
# loan$Self_Employed =as.character(loan$Self_Employed)
# loan$Self_Employed[which(loan$Self_Employed=="NO")]<- 'No'
# loan$Self_Employed =as.factor(loan$Self_Employed)

table(loan$Loan_Amount_Term)
#sum(is.na(loan$Loan_Status))
#sum(is.na(loan$ApplicantIncome))
#sum(is.na(loan$CoapplicantIncome))
sum(is.na(loan$LoanAmount))
sum(is.na(loan$Loan_Amount_Term))
sum(is.na(loan$Credit_History))
#sum(is.na(loan$Property_Area))

loan$Loan_Amount_Term <- as.integer(loan$Loan_Amount_Term)

loan$Loan_Amount_Term <- as.numeric(loan$Loan_Amount_Term)

loan$Loan_Amount_Term[is.na(loan$Loan_Amount_Term)]=360

loan$LoanAmount[is.na(loan$LoanAmount)]=0

ltrain <- loan[1:614,]
ltest <- loan[615:981,]
ltest <- ltest[,-12]

#-----Dummy ---#

dummyloan = loan

dummyloan = cbind (dummyloan, dummy(dummyloan$Gender))
dummyloan = cbind (dummyloan, dummy(dummyloan$Married))
dummyloan = cbind (dummyloan, dummy(dummyloan$Dependents))
dummyloan = cbind (dummyloan, dummy(dummyloan$Education))
dummyloan = cbind (dummyloan, dummy(dummyloan$Self_Employed))
dummyloan = cbind (dummyloan, dummy(dummyloan$Property_Area))

dummyloan$LoanAmount = (dummyloan$LoanAmount-min(dummyloan$LoanAmount))/(max(dummyloan$LoanAmount)-min(dummyloan$LoanAmount))
dummyloan$Loan_Amount_Term = (dummyloan$Loan_Amount_Term-min(dummyloan$Loan_Amount_Term))/(max(dummyloan$Loan_Amount_Term)-min(dummyloan$Loan_Amount_Term))
dummyloan$ApplicantIncome = (dummyloan$ApplicantIncome-min(dummyloan$ApplicantIncome))/(max(dummyloan$ApplicantIncome)-min(dummyloan$ApplicantIncome))
dummyloan$CoapplicantIncome = (dummyloan$CoapplicantIncome-min(dummyloan$CoapplicantIncome))/(max(dummyloan$CoapplicantIncome)-min(dummyloan$CoapplicantIncome))

dummyloan$LoanAmount=log(dummyloan$LoanAmount)
dummyloan$Loan_Amount_Term =log(dummyloan$Loan_Amount_Term)
dummyloan$ApplicantIncome = log(dummyloan$ApplicantIncome)
dummyloan$CoapplicantIncome=log(dummyloan$CoapplicantIncome)


names(dummyloan)[17]='dummyloanNotGraduate'

dummyloan = dummyloan[,-6]
dummyloan = dummyloan[,-c(1:5)]

ltrain <- dummyloan[1:614,]
ltest <- dummyloan[615:981,]
ltest <- ltest[,-6]

#----Random forest--------------#

set.seed(415)

tune.rf <- tuneRF(ltrain[,-12],ltrain[,12], stepFactor=0.5)

fit <- randomForest(as.factor(Loan_Status) ~.,data=ltrain,mtry=3, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, ltest)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:\\Users\\431539\\Desktop\\Titanic\\firstforest.csv", row.names = FALSE)

cbind(id,Prediction[,2])

#------------------------==Logistic-------------------=#
Logis <- glm(Loan_Status ~ . ,data=ltrain, family=binomial(link="logit"))

p=predict(Logis,newdata = ltest,type = "response")

##-----------------XGBoost------------------------##

library(xgboost)

target = ltrain[,6]
target=ifelse(target=='N',0,1)
target=as.integer(target)

ltrain=ltrain[,-6]

#convert dataset into numeric Matrix format

trainMatrix <- data.matrix(ltrain)
testMatrix <- data.matrix(ltest)

#cross-validation to choose the parameters

numberOfClasses <- 2

param <- list("objective" = "multi:softprob","eval_metric" = "mlogloss","num_class" = numberOfClasses)

cv.nround <- 500
cv.nfold <- 5
bst.cv = xgb.cv(param=param, data = trainMatrix, label = target,nfold = cv.nfold, nrounds = cv.nround)

plot(bst.cv$test.mlogloss.mean,lty =1)
nround <- which(bst.cv$test.mlogloss.mean==min(bst.cv$test.mlogloss.mean))

#train the model

bst = xgboost(data = trainMatrix, label = target, param=param, nrounds = nround)

#predict the model

ypred = predict(bst, testMatrix)

ypred=ifelse(ypred>0.47,1,0)
ypred=round(ypred)

#prepare for output

predMatrix <- data.frame(matrix(ypred, ncol=2, byrow=TRUE))
#colnames(predMatrix) = classnames
res<-data.frame(id, round(predMatrix[2]))
colnames(res)=c('Loan_ID','Loan_Status')
res$Loan_Status <- ifelse(res$Loan_Status==0,'N','Y')

write.csv(res,"C:\\Users\\431539\\Desktop\\Loan\\submission.csv",quote=F,row.names = F)

#-------------Predict Missing values-----------------------------------------------#

ploan = rbind(train,test)

#--Gender---#
ploan$Gender[which(ploan$Gender=="")] <- NA
ploan$Gender =as.character(ploan$Gender)
ploan$Gender =as.factor(ploan$Gender)

genderfit <- rpart(Gender ~.,data=ploan[!is.na(ploan$Gender),],method='anova')

a <- predict(genderfit, ploan[is.na(ploan$Gender),])
a=round(a)
a=ifelse(a==2,'Female','Male')
ploan$Gender[is.na(ploan$Gender)] <- a
ploan$Gender =as.character(ploan$Gender)
ploan$Gender =as.factor(ploan$Gender)

#---Married-----#
ploan$Married[which(ploan$Married=="")] <- NA
ploan$Married =as.character(ploan$Married)
ploan$Married =as.factor(ploan$Married)

Marriedfit <- rpart(Married ~.,data=ploan[!is.na(ploan$Married),],method='anova')

a <- predict(Marriedfit, ploan[is.na(ploan$Married),])
a=round(a)
a=ifelse(a==1,'No','Yes')
ploan$Married[is.na(ploan$Married)] <- a
ploan$Married =as.character(ploan$Married)
ploan$Married =as.factor(ploan$Married)

#-------Dependents----------#

ploan$Dependents[which(ploan$Dependents=="3+")]<- 3
ploan$Dependents[which(ploan$Dependents=="")] <- NA
ploan$Dependents =as.character(ploan$Dependents)
ploan$Dependents =as.factor(ploan$Dependents)

Dependentsfit <- rpart(Dependents ~.,data=ploan[!is.na(ploan$Dependents),],method='anova')

a <- predict(Dependentsfit, ploan[is.na(ploan$Dependents),])
a=round(a)
a=ifelse(a==1,'No','Yes')
ploan$Dependents[is.na(ploan$Dependents)] <- a
ploan$Dependents =as.character(ploan$Dependents)
ploan$Dependents =as.factor(ploan$Dependents)

#-----Self_Employed----#

ploan$Self_Employed[which(ploan$Self_Employed=="")] <- NA
ploan$Self_Employed =as.character(ploan$Self_Employed)
ploan$Self_Employed =as.factor(ploan$Self_Employed)

Self_Employedfit <- rpart(Self_Employed ~.,data=ploan[!is.na(ploan$Self_Employed),],method='anova')

a <- predict(Self_Employedfit, ploan[is.na(ploan$Self_Employed),])
a=round(a)
a=ifelse(a==1,'No','Yes')
ploan$Self_Employed[is.na(ploan$Self_Employed)] <- a
ploan$Self_Employed =as.character(ploan$Self_Employed)
ploan$Self_Employed =as.factor(ploan$Self_Employed)

##----------LoanAmount-------------##

ploan$LoanAmount[which(ploan$LoanAmount=="")] <- NA
ploan$LoanAmount =as.numeric(ploan$LoanAmount)

LoanAmountfit <- rpart(LoanAmount ~.,data=ploan[!is.na(ploan$LoanAmount),],method='anova')

a <- predict(LoanAmountfit, ploan[is.na(ploan$LoanAmount),])
a=round(a)

ploan$LoanAmount[is.na(ploan$LoanAmount)] <- a

##----Loan_Amount_Term-----------##

ploan$Loan_Amount_Term[which(ploan$Loan_Amount_Term=="")] <- NA
ploan$Loan_Amount_Term =as.numeric(ploan$Loan_Amount_Term)

Loan_Amount_Termfit <- rpart(Loan_Amount_Term ~.,data=ploan[!is.na(ploan$Loan_Amount_Term),],method='anova')

a <- predict(Loan_Amount_Termfit, ploan[is.na(ploan$Loan_Amount_Term),])
a=round(a)

ploan$Loan_Amount_Term[is.na(ploan$Loan_Amount_Term)] <- a

##-------Credit_History---------##

ploan$Credit_History[which(ploan$Credit_History=="")] <- NA
ploan$Credit_History =as.character(ploan$Credit_History)
ploan$Credit_History =as.factor(ploan$Credit_History)

Credit_Historyfit <- rpart(Credit_History ~.,data=ploan[!is.na(ploan$Credit_History),],method='anova')

a <- predict(Credit_Historyfit, ploan[is.na(ploan$Credit_History),])
a=round(a)
a=ifelse(a==1,0,1)
ploan$Credit_History[is.na(ploan$Credit_History)] <- a
ploan$Credit_History =as.character(ploan$Credit_History)
ploan$Credit_History =as.factor(ploan$Credit_History)

##-------------------------------------##
#--------------Logistic-----------------#

pltrain = loan[1:614,]
pltest =loan[615:981,]
pltest <- pltest[,-12]

Logis <- glm(Loan_Status ~ . ,data=pltrain, family=binomial(link="logit"))

Logis <- multinom(Loan_Status ~ . ,data=pltrain)

p=predict(Logis,newdata = pltest)

p=round(p)

p=ifelse(p>0.6,'Y','N')
p=ifelse(p==0,'N','Y')

id=as.character(id)
p=as.character(p)
op=cbind(id,p)

colnames(op)=c('Loan_ID','Loan_Status')

write.csv(op,"C:\\Users\\431539\\Desktop\\Loan\\submission.csv",quote=F,row.names = F)

#------------Random Forest-----------------------#

set.seed(415)

tune.rf <- tuneRF(ltrain[,-12],ltrain[,12], stepFactor=0.5)

fit <- randomForest(as.factor(Loan_Status) ~.,data=ltrain,mtry=3, importance=TRUE, ntree=2000)

# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, ltest)

Prediction=as.character(Prediction)
a=cbind(id,Prediction)

colnames(a)=c('Loan_ID','Loan_Status')

write.csv(a, file = "C:\\Users\\431539\\Desktop\\Loan\\RF_Pred_Mis.csv", row.names = FALSE)

 ploan$Credit_History=as.factor(ploan$Credit_History)
 ploan$Dependents=as.factor(ploan$Dependents)

ploan$LoanAmount=ploan$LoanAmount*1000
ploan$Loan_Amount_Term=ploan$Loan_Amount_Term/12
ploan$Loan_Amount_Term=round(ploan$Loan_Amount_Term)


pltrain <- read.csv("C:\\Users\\431539\\Documents\\pltrain.csv")
pltest <- read.csv("C:\\Users\\431539\\Documents\\pltest.csv")
pltest$Loan_Status = NA
ploan =rbind(pltrain,pltest)
