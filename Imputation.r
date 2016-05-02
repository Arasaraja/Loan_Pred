train <- read.csv("C:\\Users\\431539\\Desktop\\Loan\\train.csv",na.strings = "")

test<- read.csv("C:\\Users\\431539\\Desktop\\Loan\\test.csv",na.strings = "")

str(train)
summary(train)

train= train[,-1]        

id=test[,1]
test=test[,-1]

test$Loan_Status = NA

loan = rbind(train,test)
str(loan)

table(loan$Gender)

# loan$Gender[which(loan$Gender=="")] <- NA
# loan$Gender <- as.character(loan$Gender)
# #loan$Gender[is.na(loan$Gender)]= 'Male'
# loan$Gender <- as.factor(loan$Gender)
# 
# table(loan$Married)
# loan$Married[which(loan$Married=="")] <- NA
# loan$Married <- as.character(loan$Married)
# #loan$Married[is.na(loan$Married)]='Yes'
# loan$Married <- as.factor(loan$Married)
# 
# table(loan$Dependents)
# loan$Dependents =as.character(loan$Dependents)
# loan$Dependents[which(loan$Dependents=="")]<- NA
# loan$Dependents[which(loan$Dependents=="3+")]<- 3
# loan$Dependents =as.factor(loan$Dependents)
# 
# table(loan$Self_Employed)
# loan$Self_Employed[which(loan$Self_Employed=="")] <- NA
# loan$Self_Employed =as.character(loan$Self_Employed)
# #loan$Self_Employed[which(loan$Self_Employed=="NO")]<- 'No'
# loan$Self_Employed =as.factor(loan$Self_Employed)

table(loan$Loan_Amount_Term)
#sum(is.na(loan$Loan_Status))
#sum(is.na(loan$ApplicantIncome))
#sum(is.na(loan$CoapplicantIncome))
sum(is.na(loan$LoanAmount))

table(loan$LoanAmount)
loan$LoanAmount[is.na(loan$LoanAmount)] <- 126  #median
loan$LoanAmount[which(loan$LoanAmount=="")] <- 126
loan$LoanAmount =as.integer(loan$LoanAmount)


sum(is.na(loan$Loan_Amount_Term))
loan$Loan_Amount_Term[is.na(loan$Loan_Amount_Term)] <- 360  #median
loan$Loan_Amount_Term[which(loan$Loan_Amount_Term=="")] <- NA
loan$Loan_Amount_Term =as.integer(loan$Loan_Amount_Term)


sum(is.na(loan$Credit_History))
loan$Credit_History[which(loan$Credit_History=="")] <- NA
loan$Credit_History =as.integer(loan$Credit_History)
loan$Credit_History =as.factor(loan$Credit_History)

loan$LoanAmount=log(loan$LoanAmount)
loan$Loan_Amount_Term =log(loan$Loan_Amount_Term)
loan$ApplicantIncome = log(loan$ApplicantIncome)
loan$CoapplicantIncome=log(loan$CoapplicantIncome)



#sum(is.na(loan$Property_Area))

#------------------------==Logistic-------------------=#
LogisGender <- glm(Gender ~ . ,data=loan[!is.na(loan$Gender),], family=binomial(link="logit"),na.action=na.pass)

p=predict(LogisGender,newdata = loan[is.na(loan$Gender),-12],type = "response")



#---Multinom---#
# data2=read.csv(("C:\\Users\\431539\\Documents\\data.csv"))
# datat=read.csv(("C:\\Users\\431539\\Documents\\newdata.csv"))
# 
# LogisMarried <- multinom(Married ~ . ,data=data2,trace=FALSE)
# 
# p=predict(LogisMarried,newdata = datat)

LogisGender <- multinom(Gender ~ .,data=loan[!is.na(loan$Gender),-c(3,10,12)],trace=FALSE)

p=predict(LogisGender,newdata = loan[is.na(loan$Gender),-c(3,10,12)])

loan$Gender[is.na(loan$Gender)] <- p
sum(is.na(loan$Gender))

LogisMarried <- multinom(Married ~ . ,data=loan[!is.na(loan$Married),-c(3,12,8)],trace=FALSE)

p=predict(LogisMarried,newdata = loan[is.na(loan$Married),-c(3,12,8)])

loan$Married[is.na(loan$Married)] <- p
sum(is.na(loan$Married))

LogisDependents <- multinom(Dependents ~ . ,data=loan[!is.na(loan$Dependents),-c(10,12)],trace=FALSE)

p=predict(LogisDependents,newdata = loan[is.na(loan$Dependents),-c(10,12)])

loan$Dependents[is.na(loan$Dependents)] <- p
sum(is.na(loan$Dependents))

LogisSelf_Employed <- multinom(Self_Employed ~ . ,data=loan[!is.na(loan$Self_Employed),-c(10,12)],trace=FALSE)

p=predict(LogisSelf_Employed,newdata = loan[is.na(loan$Self_Employed),-c(10,12)])

loan$Self_Employed[is.na(loan$Self_Employed)] <- p
sum(is.na(loan$Self_Employed))

LogisCredit_History <- multinom(Credit_History ~ . ,data=loan[!is.na(loan$Credit_History),-12],trace=FALSE)

p=predict(LogisCredit_History,newdata = loan[is.na(loan$Credit_History),-12])

loan$Credit_History=as.factor(loan$Credit_History)
loan$Credit_History[is.na(loan$Credit_History)] <- p
sum(is.na(loan$Credit_History))


LoanAmount

linLoanAmount <- lm(LoanAmount~.,data=loan[!is.na(loan$LoanAmount),-c(3,5,10,12)])
p=predict(linLoanAmount,newdata=loan[is.na(loan$LoanAmount),-c(3,5,10,12)])
p=round(p)
loan$LoanAmount[is.na(loan$LoanAmount)] <- p
sum(is.na(loan$LoanAmount))

Loan_Amount_Term

linLoan_Amount_Term <- lm(Loan_Amount_Term~.,data=loan[!is.na(loan$Loan_Amount_Term),-c(3,5,10,12)])
p=predict(linLoan_Amount_Term,newdata=loan[is.na(loan$Loan_Amount_Term),-c(3,5,10,12)])
p=round(p)
loan$Loan_Amount_Term[is.na(loan$Loan_Amount_Term)] <- p
sum(is.na(loan$Loan_Amount_Term))

##--------------------------------------------------------------------------------------------##
Dependentsfit <- rpart(Dependents ~.,data=ploan[!is.na(ploan$Dependents),],method='anova')

a <- predict(Dependentsfit, ploan[is.na(ploan$Dependents),])
a=round(a)
a=ifelse(a==1,'No','Yes')

