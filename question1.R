set.seed(2023) # For reproducibility

# Number of observations
n <- 1000

# Simulated data
data <- data.frame(
  Support = factor(rbinom(n, 1, 0.5), labels = c("No", "Yes")),
  AgeGroup = factor(sample(c('18-24', '25-34', '35-44', '45-54', '55-64'), n, replace = TRUE)),
  Gender = factor(sample(c('Female', 'Male'), n, replace = TRUE)),
  IncomeGroup = factor(sample(c('Low', 'Medium', 'High'), n, replace = TRUE)),
  HighestEducation = factor(sample(c('High School', "Bachelor's", "Master's", "Doctorate"), n, replace = TRUE))
)



#1.
model <- glm(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, data = data, family = binomial)
summary(model)


#2.
chisq.test(table(data$Gender, data$Support))

#3.
model1 <- glm(Support ~ Gender + IncomeGroup + HighestEducation, data = data, family = binomial)
anova(model1, model, test = "Chisq")


#4
stratified_models <- lapply(split(data, data$HighestEducation), function(subdata) glm(Support ~ AgeGroup + Gender + IncomeGroup, data = subdata, family = binomial))



#5.
library(multcomp)
glht_model <- glht(model, linfct = mcp(AgeGroup = "Tukey"))
summary(glht_model)



#6.
library(lmtest)
gof <- lrtest(model)
print(gof)


#7.
exp(coef(model))

#8.
interaction_model <- glm(Support ~ AgeGroup * Gender + IncomeGroup + HighestEducation, data = data, family = binomial)
summary(interaction_model)


#9.
library(pROC)
roc_curve <- roc(data$Support, fitted(model))
auc(roc_curve)


#10
library(MatchIt)
matchit_model <- matchit(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, data = data, method = "nearest")
summary(matchit_model)
