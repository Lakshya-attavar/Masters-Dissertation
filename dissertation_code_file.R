#install packages
install.packages('psych')
install.packages("ltm")
install.packages("carData")
install.packages("lavaan")
install.packages("semTools")
install.packages("lmtest")
install.packages("car")
install.packages("moments")

# Load libraries
library(dplyr)
library(psych)
library(corrgram)
library(ggplot2)
library(ltm)
library(carData)
library(car)
library(lavaan)
library(semTools)
library(polycor)
library(lmtest)

data <- read.csv('Numeric_survey_dataset2.csv')
colSums(is.na(data))
head(data)
summary(data)

overcon <- as.factor(data$overcon)
gender <- as.factor(data$gender)
occupation <- as.factor(data$occupation)

#sample distributions
table(data$Cat1_Cosmetics_fragrances)
prop.table(table(data$Cat1_Cosmetics_fragrances)) * 100

table(data$Cat2_Watches_Jewellery)
prop.table(table(data$Cat2_Watches_Jewellery)) * 100

table(data$Cat3_Fashion)
prop.table(table(data$Cat3_Fashion)) * 100

table(data$Cat4_Leather.Goods)
prop.table(table(data$Cat4_Leather.Goods)) * 100

table(data$Luxury_Consumer)
prop.table(table(data$Luxury_Consumer)) * 100

#gender distribution
table(data$gender)
prop.table(table(data$gender)) * 100 

#nationality distribution
table(data$nationality)
prop.table(table(data$nationality)) * 100

#employment status distribution
table(data$occupation)
prop.table(table(data$occupation)) * 100

#age distribution
data$AgeGroup <- cut(data$age, 
                     breaks = c(0, 18, 27, 43, 59, Inf),
                     labels = c("Under 18", "18-27", "28-43", "44-59", "60+"),
                     right = FALSE)
age_freq <- table(data$AgeGroup)
print(age_freq)

# age percentage
age_percent <- prop.table(age_freq) * 100
print(age_percent)

#construct items
financial_val <- c("FIN_1", "FIN_2", "FIN_3")
functional_val <- c("FUN_1", "FUN_2", "FUN_3", "FUN_4")
Numeracy <- c("NUMcorrect")
social_val <- c("SOC_1", "SOC_2", "SOC_3", "SOC_4")
self_identity_val <- c("SFI_1", "SFI_2", "SFI_3")
materialism_val <- c("MAT_1", "MAT_2", "MAT_3", "MAT_4")
hedonism_val <- c("HED_1", "HED_2", "HED_3", "HED_4", "HED_5")
present <- c("Present")
overconf <- c("overcon")
lvperception <- c("PER_1", "PER_2", "PER_3", "PER_4")
purchase_int <- c("PIT_1")
willingness <- c("WPP_1", "WPP_2")

# Descriptive statistics
constructs <- list(financial_val, functional_val, Numeracy, social_val, self_identity_val, materialism_val, hedonism_val, present, overconf, lvperception, purchase_int, willingness)
financial_item_means<- data %>%dplyr::select(all_of(financial_val)) %>% describe()
functional_item_means<- data %>%dplyr::select(all_of(functional_val)) %>% describe()  
social_item_means<-data %>%dplyr::select(all_of(social_val)) %>% describe() 
selfidentity_item_means <- data %>%dplyr::select(all_of(self_identity_val)) %>% describe() 
materialism_item_means <- data %>%dplyr::select(all_of(materialism_val)) %>% describe() 
hedonism_item_means <- data %>%dplyr::select(all_of(hedonism_val)) %>% describe() 
present_bias_item_means <- data %>%dplyr::select(all_of(present)) %>% describe() 
overconfidence_item_means <- data %>%dplyr::select(all_of(overconf)) %>% describe() 
perception_item_means<-data %>%dplyr::select(all_of(lvperception)) %>% describe() 
purchase_item_means<- data %>%dplyr::select(all_of(purchase_int)) %>% describe() 
willingness_item_means <-data %>%dplyr::select(all_of(willingness)) %>% describe()
numeracy_item_means  <-data %>%dplyr::select(all_of(Numeracy)) %>% describe()
# construct item statistics
financial_item_means
functional_item_means
social_item_means
selfidentity_item_means
materialism_item_means
hedonism_item_means
present_bias_item_means
overconfidence_item_means
perception_item_means
purchase_item_means
willingness_item_means
numeracy_item_means

# construct row mean
data <- data %>%
  mutate(financial_rmean = rowMeans(dplyr::select(., all_of(financial_val)), na.rm = TRUE),
         functional_rmean = rowMeans(dplyr::select(., all_of(functional_val)), na.rm = TRUE),
         social_rmean = rowMeans(dplyr::select(., all_of(social_val)), na.rm = TRUE),
         self_identity_rmean = rowMeans(dplyr::select(., all_of(self_identity_val)), na.rm = TRUE),
         materialism_rmean = rowMeans(dplyr::select(., all_of(materialism_val)), na.rm = TRUE),
         hedonism_rmean = rowMeans(dplyr::select(., all_of(hedonism_val)), na.rm = TRUE),
         present_bias_rmean = rowMeans(dplyr::select(., all_of(present)), na.rm = TRUE),
         overconfidence_rmean = rowMeans(dplyr::select(., all_of(overconf)), na.rm = TRUE),
         perception_rmean = rowMeans(dplyr::select(., all_of(lvperception)), na.rm = TRUE),
         purchase_rmean = rowMeans(dplyr::select(., all_of(purchase_int)), na.rm = TRUE),
         willingness_rmean = rowMeans(dplyr::select(., all_of(willingness)), na.rm = TRUE),
         numeracy_rmean = rowMeans(dplyr::select(., all_of(Numeracy)), na.rm = TRUE),
  )

construct_mean <- data[, c("financial_rmean", "functional_rmean", "social_rmean",
                            "self_identity_rmean", "materialism_rmean", "hedonism_rmean",
                            "present_bias_rmean", "overconfidence_rmean", "perception_rmean", 
                            "purchase_rmean", "willingness_rmean", "numeracy_rmean")]
head(construct_mean)
colnames(data)
# Descriptive statistics for construct
data %>%
  summarise(financial_mean = mean(financial_rmean, na.rm = TRUE),
            financial_sd = sd(financial_rmean, na.rm = TRUE),
            functional_mean = mean(functional_rmean, na.rm = TRUE),
            functional_sd = sd(functional_rmean, na.rm = TRUE),
            social_mean = mean(social_rmean, na.rm = TRUE),
            social_sd = sd(social_rmean, na.rm = TRUE),
            self_identity_mean = mean(self_identity_rmean, na.rm = TRUE),
            self_identity_sd = sd(self_identity_rmean, na.rm = TRUE),
            materialism_mean = mean(materialism_rmean, na.rm = TRUE),
            materialism_sd = sd(materialism_rmean, na.rm = TRUE),
            hedonsim_mean = mean(hedonism_rmean, na.rm = TRUE),
            hedonism_sd = sd(hedonism_rmean, na.rm = TRUE),
            present_bias_mean = mean(present_bias_rmean, na.rm = TRUE),
            present_bias_sd = sd(present_bias_rmean, na.rm = TRUE),
            overconfidence_mean = mean(overconfidence_rmean, na.rm = TRUE),
            overconfidence_sd = sd(overconfidence_rmean, na.rm = TRUE),
            perception_mean = mean(perception_rmean, na.rm = TRUE),
            perception_sd = sd(perception_rmean, na.rm = TRUE),
            purchase_mean = mean(purchase_rmean, na.rm = TRUE),
            purchase_sd = sd(purchase_rmean, na.rm = TRUE),
            willingness_mean = mean(willingness_rmean, na.rm = TRUE),
            willingness_sd = sd(willingness_rmean, na.rm = TRUE),
            numeracy_mean = mean(numeracy_rmean, na.rm = TRUE),
            numeracy_sd = sd(numeracy_rmean, na.rm = TRUE))

#Factor Loadings
#CFA
model <- '
  # Latent variables
  fin_val =~ FIN_1 + FIN_2 + FIN_3
  func_val =~ FUN_1 + FUN_2 + FUN_3 + FUN_4
  soc_val =~ SOC_1 + SOC_2 + SOC_3 + SOC_4
  self_id_val =~ SFI_1 + SFI_2 + SFI_3
  mat_val =~ MAT_1 + MAT_2 + MAT_3 + MAT_4
  hed_val =~ HED_1 + HED_2 + HED_3 + HED_4 + HED_5
  num_val =~ NUMcorrect
  present_bias_b =~ Present
  overconfidence_b =~ overcon
  perception_val =~ PER_1 + PER_2 + PER_3 + PER_4
  pur_int =~ PIT_1
  Willing_int =~ WPP_1 + WPP_2
'

fit <- cfa(model, data = data, ordered = c("overcon"))
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Cronbach's alpha
alpha_financial <- cronbach.alpha(data[, c(financial_val)])
print(alpha_financial)

alpha_functional <- cronbach.alpha(data[, c(functional_val)])
print(alpha_functional)

alpha_social<- cronbach.alpha(data[, c(social_val)])
print(alpha_social)

alpha_self_identity<- cronbach.alpha(data[, c(self_identity_val)])
print(alpha_self_identity)

alpha_materialism<- cronbach.alpha(data[, c(materialism_val)])
print(alpha_materialism)

alpha_hedonism<- cronbach.alpha(data[, c(hedonism_val)])
print(alpha_hedonism)

alpha_perception<- cronbach.alpha(data[, c(lvperception)])
print(alpha_perception)

#alpha_purchase<- cronbach.alpha(data[, purchase, drop = FALSE])
#print(alpha_purchase)

alpha_willingness<- cronbach.alpha(data[, c(willingness)])
print(alpha_willingness)

#Composite reliability and McDonald's Omega
cr <- reliability(fit)
print(cr)

#inter-item correlation
inter_item_cor <- cor(data[, willingness], use = "pairwise.complete.obs")
upper_tri <- inter_item_cor[upper.tri(inter_item_cor)]
average_inter_item_correlation <- mean(upper_tri)
print(average_inter_item_correlation)
#financial_val = 0.5297316
#functional_val = 0.2475771
#social_val = 0.5989609
#self_identity_val = 0.5154249
#materialism_val = 0.4282419
#hedonism_val = 0.4255901
#lvperception = 0.6810744
#willingness = 0.7627375



#correlation and corrgram
continuous_constructs <- construct_mean[, c("financial_rmean", "functional_rmean", "social_rmean", "self_identity_rmean", "materialism_rmean", "hedonism_rmean",
                                             "present_bias_rmean", "numeracy_rmean", "perception_rmean", "purchase_rmean", "willingness_rmean")]
binary_construct <- construct_mean$overconfidence_rmean
#Pearson correlation matrix-continuous variables
pearson_matrix <- cor(continuous_constructs)

#point-biserial correlation between binary categorical variable and each of the continuous variables
point_biserial <- sapply(continuous_constructs, function(x) cor(x, binary_construct, method = "pearson"))

# complete matrix
cor_matrix <- matrix(NA, nrow = 12, ncol = 12)
rownames(cor_matrix) <- colnames(cor_matrix) <- c("financial", "functional", "social", "self_identity", "materialism", "hedonism","present_bias", "numeracy", "perception", "purchase", "willingness", "overconfidence")

cor_matrix[1:11, 1:11] <- pearson_matrix

for (i in 1:11) {
  cor_matrix[i, 12] <- point_biserial[i]
  cor_matrix[12, i] <- point_biserial[i]
}

diag(cor_matrix) <- 1

#correlation matrix
print(cor_matrix)
corrgram(cor_matrix)

#testing assumptions
#normality - skewness
library(moments)
ab<- c(data$financial_rmean+ data$functional_rmean+ data$social_rmean+ data$self_identity_rmean
       + data$materialism_rmean + data$hedonism_rmean + data$present_bias_rmean 
       + data$numeracy_rmean + data$overconfidence_rmean + data$perception_rmean
       + data$purchase_rmean + data$willingness_rmean)
shapiro.test(ab)
skewness_value <- skewness(data$financial_rmean+ data$functional_rmean+ data$social_rmean+ data$self_identity_rmean
                           + data$materialism_rmean + data$hedonism_rmean + data$present_bias_rmean 
                           + data$numeracy_rmean + data$overconfidence_rmean + data$perception_rmean
                           + data$purchase_rmean + data$willingness_rmean)
print(skewness_value)


#regression
# H1
model1 <- lm(perception_rmean ~ financial_rmean + functional_rmean + social_rmean + self_identity_rmean + materialism_rmean + hedonism_rmean, data = construct_mean)
summary(model1)
par(mfrow = c(3, 3))
# linearity assumption: residuals vs. fitted plot
plot(model1$fitted.values, model1$residuals,
     main = "Residuals vs Fitted: Model 1",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")  
#DW autocorrelation
dw_test_result1 <- dwtest(model1)
print(dw_test_result1)

#VIF multicollinearity
vif_values1 <- vif(model1)
print(vif_values1)

#H2a
model2a <- lm(purchase_rmean ~ perception_rmean, data = construct_mean)
summary(model2a)

dw_test_result2a <- dwtest(model2a)
print(dw_test_result2a)

plot(model2a$fitted.values, model2a$residuals,
     main = "Residuals vs Fitted : Model 2a",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

#H2b
model2b <- lm(willingness_rmean ~ perception_rmean, data = construct_mean)
summary(model2b)

dw_test_result2b <- dwtest(model2b)
print(dw_test_result2b)
plot(model2b$fitted.values, model2b$residuals,
     main = "Residuals vs Fitted: Model 2b",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")  

#H3a
model3a <- lm(purchase_rmean ~ present_bias_rmean, data = construct_mean)
summary(model3a)
dw_test_result3a <- dwtest(model3a)
print(dw_test_result3a)
plot(model3a$fitted.values, model3a$residuals,
     main = "Residuals vs Fitted: Model 3a",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

#H3b
model3b <- lm(willingness_rmean ~ present_bias_rmean, data = construct_mean)
summary(model3b)
dw_test_result3b <- dwtest(model3b)
print(dw_test_result3b)
plot(model3b$fitted.values, model3b$residuals,
     main = "Residuals vs Fitted: Model 3b",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

#H4a logistic regression
model4a <- glm(overconfidence_rmean ~ numeracy_rmean, data = construct_mean, family = binomial)
summary(model4a)
dw_test_result4a <- dwtest(model4a)
print(dw_test_result4a)
plot(model4a$fitted.values, model4a$residuals,
     main = "Residuals vs Fitted: Model 4a",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
construct_mean$overconfidence_rmean = as.factor(construct_mean$overconfidence_rmean)

#H4b simple linear regression
model4b <- lm(purchase_rmean ~ overconfidence_rmean, data = construct_mean)
summary(model4b)
dw_test_result4b <- dwtest(model4b)
print(dw_test_result4b)

plot(model4b$fitted.values, model4b$residuals,
     main = "Residuals vs Fitted: Model 4b",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

#H4c simple linear regression
model4c <- lm(willingness_rmean ~ overconfidence_rmean, data = construct_mean)
summary(model4c)
dw_test_result4c <- dwtest(model4c)
print(dw_test_result4c)
plot(model4c$fitted.values, model4c$residuals,
     main = "Residuals vs Fitted: Model 4c",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0


# T-test for difference in purchase intention of luxury vs. non-luxury consumers
t_test_result1 <- t.test(data$PIT_1 ~ data$Luxury_Consumer, data = data)

# Display the results
print(t_test_result1)

# Splitting data sets
data_split <- split(data, data$Luxury_Consumer)
str(data_split)
construct_means_luxury <-data_split[[1]][, c("financial_rmean", "functional_rmean", "social_rmean",
                                             "self_identity_rmean", "materialism_rmean", "hedonism_rmean",
                                             "present_bias_rmean", "overconfidence_rmean", "perception_rmean", 
                                             "purchase_rmean", "willingness_rmean", "numeracy_rmean")]


construct_means_nonluxury <-data_split[[2]][, c("financial_rmean", "functional_rmean", "social_rmean",
                                                "self_identity_rmean", "materialism_rmean", "hedonism_rmean",
                                                "present_bias_rmean", "overconfidence_rmean", "perception_rmean", 
                                                "purchase_rmean", "willingness_rmean", "numeracy_rmean")]


#regression for luxury vs. non-luxury
model_lux <- lm(perception_rmean ~ financial_rmean + functional_rmean + social_rmean + self_identity_rmean + materialism_rmean + hedonism_rmean, data = construct_means_luxury)
summary(model_lux)

model_nonlux <- lm(perception_rmean ~ financial_rmean + functional_rmean + social_rmean + self_identity_rmean + materialism_rmean + hedonism_rmean, data = construct_means_nonluxury)
summary(model_nonlux)

