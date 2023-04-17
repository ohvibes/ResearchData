install.packages("Matrix")
install.packages("glmnet")
library(Matrix)
library(glmnet)

install.packages("ordinalNet")
install.packages("AER")
library(ordinalNet)
library(AER)

install.packages("caTools")    # For Linear regression 
install.packages('car')        # To check multicollinearity 
install.packages("quantmod")
install.packages("MASS")
library(caTools)
library(car)
library(quantmod)
library(MASS)

#FOR BOYS

#Select the explanatory variables for Elastic Net
xMale <- dataUsedMale[, c(63:130)]
#set name for dependent variable BMI to y for simplified coding
colnames(dataUsedMale)[1] <- c("y")
#set X data and y ordinal response vector
yMale <- dataUsedMale$y
xMale <- data.matrix(xMale)

#loop 100 times cross validation for alpha
u = 1

while (u <= 100) {
  #alpha takes values between 0 and 1 in steps of 0.1
alphalist <- seq(0,1,by=0.1)
#apply cross validation for the range of alpha values
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(xMale, as.factor(yMale), alpha=a, family="multinomial", lambda.min.ratio=.001)
})
#print the lowest cross validation error per alpha, these are printed from values of alpha from 0 to 1 in steps of 0.1
for (i in 1:11) {print(min(elasticnet[[i]]$cvm))}
print(u)
u <- u + 1
}

#run ordinalNet with X as data matrix and y as factor of ordinal responses
#optimal alpha for boys is set to 0.611, family is cumulative and we use parallel model form
fitMale <- ordinalNet(xMale, as.factor(yMale), alpha = 0.611, family = "cumulative", link = "logit", parallelTerms = TRUE, nonparallelTerms = FALSE)

#give overview of the 20 lambda values and corresponding model information
summary(fitMale)
#print coefficients of the model with the lowest BIC, in our case this was the 18th model
print(coef(fitMale, matrix = TRUE, whichLambda = 18))

#remove Sex, White, other race and Oregon from control variables, remove all variables set to zero by elastic net
dataUsedFinalMale <- dataUsedMale[, -c(3, 5, 11, 49, 67, 69, 73, 75, 76, 79, 80, 83, 84, 84, 85, 86, 89, 94, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 108, 109, 111, 112, 115, 116, 120, 121, 122, 123, 125, 126, 127, 128, 129)]
#order the data based on the dependent variable BMI Class which we called y
dataUsedFinalMale <- dataUsedFinalMale[order(dataUsedFinalMale$y),]
#estimate the cumulative logit model with y as dependent variable factor regressed on all remaining variables
modelMale <- polr(as.factor(y) ~ ., dataUsedFinalMale, method = c("logistic"), Hess = TRUE)

#give output of the model test significance
summary(modelMale)


#variance inflation factor of all included variables
vif(modelMale)

#iteratively drop variable with highest VIF above threshold 5, till all are below 5
dataUsedFinalMaleAltered <- dataUsedFinalMale[, -c(87, 88)]
NewModelMale <- polr(as.factor(y) ~ ., dataUsedFinalMaleAltered, method = c("logistic"), Hess = TRUE)
vif(NewModelMale)
summary(NewModelMale)

#test significance
coefMale <- coeftest(NewModelMale)

#make table of coeficient test results
stars <- symnum(coefMale[, ncol(coefMale)], corr = FALSE, na = FALSE, 
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                symbols = c("***", "**", "*", ".", " "))
outputMale <- cbind(as.data.frame.matrix(coefMale), Stars = format(stars))

#FOR GIRLS

#select explanatory variables to include in elastic net
xFemale <- dataUsedFemale[, c(63:130)]
#set name of dependent variable BMI Class to y for easier programming
colnames(dataUsedFemale)[1] <- c("y")
#set X data and y as ordinal response vector
yFemale <- dataUsedFemale$y
xFemale <- data.matrix(xFemale)

#loop for 100 times cross validation to find optimal alpha
k = 1
while (k <= 100) {
  #set list of alpha values from 0 to 1 by steps of 0.1
alphalist <- seq(0,1,by=0.1)
#apply cross validation over range of alpha
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(xFemale, as.factor(yFemale), alpha=a, family="multinomial", lambda.min.ratio=.001)
})
#print the lowest cross validation error per alpha, these are printed from values of alpha from 0 to 1 in steps of 0.1
for (i in 1:21) {print(min(elasticnet[[i]]$cvm))}
print(k)
k <- k + 1
}

#run ordinalNet with X as data matrix and y as factor of ordinal responses
#optimal alpha for girls is set to 0.565, family is cumulative and we use parallel model form
fitFemale <- ordinalNet(xFemale, as.factor(yFemale), alpha = 0.582, family = "cumulative", link = "logit", parallelTerms = TRUE, nonparallelTerms = FALSE)

#give overview of the 20 lambda values and corresponding model information
summary(fitFemale) 

#print coefficients of the model with the lowest BIC, in our case this was the 19th model
print(coef(fitFemale, matrix = TRUE, whichLambda = 17))

#remove Sex, White, other race and Oregon from control variables, remove all variables set to zero by elastic net
dataUsedFinalFemale <- dataUsedFemale[, -c(3, 5, 11, 49, 64, 65, 69, 70, 71, 72, 73, 75, 76, 80, 81, 93, 97, 98, 100, 101, 102, 103, 104, 105, 106, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 125, 126, 128, 129)]

#order the data based on the dependent variable BMI Class which we called y
dataUsedFinalFemale <- dataUsedFinalFemale[order(dataUsedFinalFemale$y),]

#estimate the cumulative logit model with y as dependent variable factor regressed on all remaining variables
modelFemale <- polr(as.factor(y) ~ ., dataUsedFinalFemale, method = c("logistic"), Hess = TRUE)

#give output of the model
summary(modelFemale)

#check variance inflation factor
vif(modelFemale)

#iteratively drop the variable with the highest VIF value if they are above threshold 5, combined we drop these here
dataUsedFinalFemaleAltered <- dataUsedFinalFemale[, -c(83, 85)]

#Estimate final model and check if all VIF lower than 5
NewModelFemale <- polr(as.factor(y) ~ ., dataUsedFinalFemaleAltered, method = c("logistic"), Hess = TRUE)
vif(NewModelFemale)


coefFemale <- coeftest(NewModelFemale)

#make table of coeficient test results
stars <- symnum(coefFemale[, ncol(coefFemale)], corr = FALSE, na = FALSE, 
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                symbols = c("***", "**", "*", ".", " "))
outputFemale <- cbind(as.data.frame.matrix(coefFemale), Stars = format(stars))


