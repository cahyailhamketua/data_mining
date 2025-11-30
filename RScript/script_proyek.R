library(MASS)
library(dplyr)
library(caret)
library(ggplot2)

data("Boston")
df <- Boston
str(df)
head(df)

set.seed(123)
train_index <- createDataPartition(df$medv, p = 0.8, list = FALSE)
train_data <- df %>% slice(train_index)
test_data <- df %>% slice(-train_index)

# Dengan Normalisasi
preproc <- preProcess(train_data, method = c("center", "scale"))
train_scaled <- predict(preproc, train_data)
test_scaled  <- predict(preproc, test_data)

model <- lm(medv ~ ., data = train_scaled)
pred <- predict(model, newdata = test_scaled)

MAE  <- mean(abs(pred - test_scaled$medv))
MSE  <- mean((pred - test_scaled$medv)^2)
RMSE <- sqrt(MSE)
R2   <- 1 - sum((pred - test_scaled$medv)^2) / sum((test_scaled$medv - mean(test_scaled$medv))^2)

coef_df <- data.frame(
  feature = names(coef(model)),
  coef = round(coef(model), 3),
  abs_coef = round(abs(coef(model)), 3)
)

coef_df <- coef_df[order(coef_df$abs_coef, decreasing = TRUE), ]
coef_df 

ggplot() +
  geom_point(aes(x = test_scaled$medv, y = pred)) +
  labs(x = "Harga Aktual", y = "Harga Prediksi",
       title = "Perbandingan Harga Aktual vs Prediksi") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal()

round(c(MAE, MSE, RMSE, R2), 3)

