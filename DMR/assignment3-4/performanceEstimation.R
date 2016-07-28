library(e1071)
library(randomForest)
library(DMwR)
library(performanceEstimation)



# Load data
data <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data')

# Check any missing data
nrow(data[!complete.cases(data),])

# Assign column names
names(data) = c("AccountState", "DurationM", "CreditHistory", "Purpose", "CreditAmount", "SavingsAccount", "EmploymentSince",
                "InstallmentRate", "PersonalStatus", "OtherDebtors", "ResidenceSince", "Property", "Age", "InstallmentPlans",
                "Housing", "ExistingCredits", "Job", "NumPeoLiable", "Telephone", "ForeignWorker", "CreditAssessment")

# Convert target variable to factor
data$CreditAssessment = factor(data$CreditAssessment, levels = c("1","2"))
str(data)

# Random sampling
set.seed(1234)
trPerc <- 0.7
sp <- sample(1:nrow(data), as.integer(nrow(data)*trPerc))
tr <- data[sp,]
ts <- data[-sp,]

# Build SVM model
s <- svm(CreditAssessment ~ .,tr)
pSvm <- predict(s,ts)
mcSvm = table(ts$CreditAssessment,pSvm)
print(mcSvm)
errorSvm <- 100*(1-sum(diag(mcSvm))/sum(mcSvm))
print(errorSvm)

# Build random forest model
m <- randomForest(CreditAssessment ~ ., tr, ntree=3000, importance = TRUE)
pRforest <- predict(m,ts)
mcRforest = table(ts$CreditAssessment,pRforest)
print(mcRforest)
errorRforest <- 100*(1-sum(diag(mcRforest))/sum(mcRforest))
print(errorRforest)

importance(m)

# Evaluation

# It is worse to class a customer as good when they are bad (5), than it is to class a customer as bad when they are good (1).
# Concerning function classificationMetrics it also assumes the rows are the predicted values and columns the true values. This will be changed in the next version I'll upload to CRAN.
costMtrx = matrix(c(0,-1,-5,0), nrow = 2, ncol = 2)
colnames(costMtrx) = c("good", "bad")
rownames(costMtrx) = c("goodPred", "badPred")


# Evaluate models with F measure, lift and cost benefit matrix. See classificationMetrics
res <- performanceEstimation(
  PredTask(CreditAssessment ~ ., ts, "Credit assessment test data set"),
  workflowVariants("standardWF",learner=c("svm", "randomForest"), learner.pars=list(ntree=3000)),
  EstimationTask(metrics=c("F", "lift", "totU"), evaluator.pars =list(benMtrx = costMtrx), method=CV(nReps=2,nFolds=10)))

rankWorkflows(res)

plot(res)
