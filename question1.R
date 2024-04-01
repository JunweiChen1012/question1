
library(ggplot2)
library(rstanarm)

# Sample data creation, including an 'IncomeGroup' column this time
df <- data.frame(
  Support = sample(c("Yes", "No"), 10, replace = TRUE),
  AgeGroup = rep(c('18-24', '25-34', '35-44', '45-54', '55-64'), times = 2),
  Gender = rep(c('Female', 'Male'), each = 5),
  IncomeGroup = sample(c("Low", "Medium", "High"), 10, replace = TRUE),
  HighestEducation = sample(c("High School", "Bachelor's", "Master's", "Doctorate"), 10, replace = TRUE)
)

ggplot(df, aes(x = AgeGroup, y = Support, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Support for Political Party by Age Group and Gender",
       x = "Age Group", y = "Percentage Supporting") +
  scale_fill_brewer(palette = "Pastel1") # To distinguish genders


categories_to_convert <- c("AgeGroup", "Gender", "IncomeGroup", "HighestEducation")

for (category in categories_to_convert) {
  if (category %in% names(df)) {
    df[[category]] <- as.numeric(factor(df[[category]]))
  } else {
    warning(paste("Column", category, "does not exist in dataframe."))
  }
}

df$Support <- ifelse(df$Support == "Yes", 1, 0)

# Assuming 'Support' is binary (0/1) and already in your dataframe
stan_model <- stan_glm(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation,
                       data = df, family = binomial(link = "logit"))

print(summary(stan_model))


