### Read training data
#! Perhaps you need to set the working directory!?
#setwd("/home/pbac/g/course02417/2025/assignment1")
D <- read.csv("DST_BIL54.csv")
str(D)

# See the help
?strftime
D$time <- as.POSIXct(paste0(D$time,"-01"), "%Y-%m-%d", tz="UTC")


## Year to month for each of them
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
D$total <- as.numeric(D$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < teststart, ]
Dtest <- D[D$time >= teststart, ]



head(Dtrain)

library(ggplot2)

ggplot(data = Dtrain, aes(x = year, y = total)) +
  geom_point() +
  # 1. Increase the number of axis breaks (more grid lines)
  scale_x_continuous(breaks = seq(min(D$year), max(D$year), by = 1)) + 
  scale_y_continuous(n.breaks = 10) + 
  
  # 2. Set the 16/9 aspect ratio and increase base font size
  theme_minimal(base_size = 16) + 
  theme(
    aspect.ratio = 9/16,
    panel.grid.major = element_line(color = "gray90", size = 1),
    panel.grid.minor = element_blank()
  ) + labs(
    x = "Year", 
    y = "Cars Registration (millions)",
    title = "Vehicle Registration Trend Over Time"
  )

# Save as a large, high-res 16x9 image
ggsave("plot.png", width = 16, height = 9, dpi = 300)

library(dplyr)
library(ggplot2)

# Calculate the difference between years
D_diff <- Dtrain %>%
  arrange(year) %>% # Ensure years are in order
  mutate(diff_total = total - lag(total))

ggplot(data = subset(D_diff, !is.na(diff_total)), aes(x = year, y = diff_total)) +
  geom_col(fill = "steelblue") + 
  # Set the 16/9 aspect ratio
  theme_minimal(base_size = 16) +
  theme(
    aspect.ratio = 9/16,
    panel.grid.major = element_line(color = "gray90", size = 1),
    panel.grid.minor = element_blank()
  ) +
  # Labeling
  labs(y = "Change from Previous Year", x = "Year") +
  scale_x_continuous(breaks = seq(min(D$year), max(D$year), by = 1))

ggplot(data = Dtrain, aes(x = year, y = total)) +
  geom_point() +
  # 1. Increase the number of axis breaks (more grid lines)
  scale_x_continuous(breaks = seq(min(D$year), max(D$year), by = 1)) + 
  scale_y_continuous(n.breaks = 10) + 
  
  # 2. Set the 16/9 aspect ratio and increase base font size
  theme_minimal(base_size = 16) + 
  theme(
    aspect.ratio = 9/16,
    panel.grid.major = element_line(color = "gray90", size = 1),
    panel.grid.minor = element_blank()
  ) + labs(
    x = "Year", 
    y = "Cars Registration (millions)",
    title = "Vehicle Registration Trend Over Time"
  ) + geom_smooth(method = "lm", color = "red", se = TRUE)

model = lm(total~year, data = Dtrain)
summary(model)

# Extract coefficients
theta_estimates <- coef(model)

# Rename for clarity
theta_0 <- theta_estimates[1] # Intercept
theta_1 <- theta_estimates[2] # Slope (Effect of Year)

print(theta_0)
print(theta_1)

model_summary = summary(model)

se_values <- model_summary$coefficients[, "Std. Error"]
se_theta0 <- se_values[1]
se_theta1 <- se_values[2]
print(se_theta0)
print(se_theta1)


predicted_data = predict(model,newdata = data.frame(year = Dtest$year),interval = "prediction", 
                 level = 0.95)

predicted_dt_ols <- data.frame(
  year = Dtest$year,
  fit  = predicted_data[, "fit"],
  lwr  = predicted_data[, "lwr"],
  upr  = predicted_data[, "upr"]
)

View(predicted_dt_ols)

ggplot() +
  # 1. The Prediction Interval
  geom_ribbon(data = predicted_dt_ols, 
              aes(x = year, ymin = lwr, ymax = upr, fill = "Prediction Interval"), 
              alpha = 0.1) +
  
  # 2. The Training Data (Map 'color' to a string label)
  geom_point(data = Dtrain, aes(x = year, y = total, color = "Training Data")) +
  
  # 3. The Regression Line
  geom_smooth(method = "lm", data = Dtrain, aes(x = year, y = total, color = "Regression Line"), 
              linewidth = 1, se = FALSE) +

  # 4. The Predicted Points (Map 'shape' or 'color' to a string label)
  geom_point(data = predicted_dt_ols, aes(x = year, y = fit, color = "Predicted Points"),
             size = 3, shape = 18) +
  
  geom_point(data = Dtest, aes(x = year, y = total, color = "Test Data")) +
  # 5. Define the colors manually
  scale_color_manual(name = "Legend", values = c(
    "Training Data" = "black",
    "Regression Line" = "red",
    "Predicted Points" = "red"
  )) +
  scale_fill_manual(name = "Interval", values = c("Prediction Interval" = "red")) +
  
  scale_x_continuous(breaks = seq(min(D$year), max(D$year), by = 1)) +
  theme(legend.position = "bottom")

library(ggfortify)

autoplot(model)


Dtrain$resids <- residuals(model)



ggplot(Dtrain, aes(x = resids)) +
  geom_histogram(aes(y = ..density..), bins = 35, fill = "steelblue", color = "white") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram of Model Residuals",
       x = "Residuals (Error)",
       y = "Density") +
  theme_minimal()

# 1. Define your parameters
lambda <- 0.9         # Example decay rate
n <- nrow(Dtrain)         # The size of your input data

# 2. Create the vector of exponents (from n-1 down to 0)
# We use n-1 because lambda^0 = 1 (your final value)
exponents <- (n-1):0

# 3. Calculate the weights
weights <- lambda^exponents

# Preview the first few
head(weights)
sum(weights)

weighted_model <- lm(total ~ year, data = Dtrain, weights = weights)

summary(weighted_model)

ggplot() +
geom_point(data = Dtrain, aes(x = year, y = weights)) +
theme_minimal()

autoplot(weighted_model)

Dtrain$resids <- residuals(weighted_model)

ggplot(Dtrain, aes(x = resids)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", color = "white") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram of Model Residuals",
       x = "Residuals (Error)",
       y = "Density") +
  theme_minimal()

predicted_data = predict(weighted_model,newdata = data.frame(year = Dtest$year),interval = "prediction", 
                 level = 0.95)

predicted_dt <- data.frame(
  year = Dtest$year,
  fit  = predicted_data[, "fit"],
  lwr  = predicted_data[, "lwr"],
  upr  = predicted_data[, "upr"]
)

View(predicted_dt)
Dtrain$wls_fit <- predict(weighted_model)

summary(weighted_model)

ggplot() +
  # 1. The WLS Prediction Interval (Future)
  geom_ribbon(data = predicted_dt, 
              aes(x = year, ymin = lwr, ymax = upr, fill = "Prediction Interval"), 
              alpha = 0.1) +
  
  # 2. The Training Data
  # Tip: Map 'size' to weights to visually show the WLS influence!
  geom_point(data = Dtrain, aes(x = year, y = total, color = "Training Data")) +
  
  # 3. The WLS Regression Line (Actual model fit)
  geom_line(data = Dtrain, aes(x = year, y = wls_fit, color = "WLS Regression Line"), 
            linewidth = 1) +

  # 4. The Predicted Points (Future)
  geom_point(data = predicted_dt, aes(x = year, y = fit, color = "WLS Predicted Points"),
             size = 3, shape = 18) +
  
  # 5. Manual Scales
  scale_color_manual(name = "Legend", values = c(
    "Training Data" = "black",
    "WLS Regression Line" = "red",
    "WLS Predicted Points" = "red"
  )) +
  scale_fill_manual(name = "Interval", values = c("Prediction Interval" = "red")) +
  scale_size_continuous(guide = "none") + # Hides the size legend if it's too cluttered
   geom_point(data = Dtest, aes(x = year, y = total, color = "Test Data")) +
  scale_x_continuous(breaks = seq(min(Dtrain$year), max(predicted_dt$year), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

sum(Dtrain$year*Dtrain$year)

Dtrain$ols_fit <- predict(model)

# Assuming you want OLS prediction intervals too (optional, but good for comparison)
ols_pred <- predict(model, newdata = Dtest, interval = "prediction")
predicted_dt$ols_fit <- ols_pred[, "fit"]

ggplot() +
  # --- WLS Layers ---
  geom_ribbon(data = predicted_dt,
              aes(x = year, ymin = lwr, ymax = upr, fill = "WLS Interval"), 
              alpha = 0.1) +
 geom_ribbon(data = predicted_dt_ols,
              aes(x = year, ymin = lwr, ymax = upr, fill = "OLS Interval"), 
              alpha = 0.1,) +              
  geom_line(data = Dtrain, aes(x = year, y = wls_fit, color = "WLS Line"), linewidth = 1) +
  geom_point(data = predicted_dt, aes(x = year, y = fit, color = "WLS Prediction")) +
  
  # --- OLS Layers ---
  geom_line(data = Dtrain, aes(x = year, y = ols_fit, color = "OLS Line"), linewidth = 1) +
  geom_point(data = predicted_dt, aes(x = year, y = ols_fit, color = "OLS Prediction")) +
  
  # --- Data Points ---
  geom_point(data = Dtrain, aes(x = year, y = total, color = "Training Data")) +
  geom_point(data = Dtest, aes(x = year, y = total, color = "Test Data")) +
  
  # --- Scales & Legend Formatting ---
  scale_color_manual(name = "Model Components", values = c(
    "Training Data" = "black",
    "Test Data" = "blue",
    "WLS Line" = "red",
    "WLS Prediction" = "red",
    "OLS Line" = "darkgreen",
    "OLS Prediction" = "darkgreen"
  )) +
  scale_fill_manual(name = "Intervals", values = c("WLS Interval" = "red","OLS Interval" = "green")) +
  
  # --- Layout Adjustments ---
  scale_x_continuous(breaks = seq(min(Dtrain$year), max(predicted_dt$year), by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",           # Stacks Color and Fill legends
    legend.margin = margin(t = 10),    # Adds space above the legend
    plot.margin = margin(10, 20, 10, 10) # Gives the right side some breathing room
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) # Wraps legend into 2 rows

theta = matrix(c(0,0), nrow=2, ncol=1)
r = matrix(c(0.1,0,0,0.1), nrow = 2, ncol = 2)

x_vec <- Dtrain$year
y_vec <- Dtrain$total

for (i in seq_along(Dtrain$year)){
  if (i > 3) {
    break()
  }
  x <- matrix(c(1, x_vec[i]), nrow = 2, ncol = 1)
  r <- r + (x %*% t(x))
  error <- y_vec[i] - as.numeric(t(x) %*% theta)
  theta <- theta + (solve(r) %*% x * error)
  print(theta)
}

theta = matrix(c(0,0), nrow=2, ncol=1)
r = matrix(c(0.1,0,0,0.1), nrow = 2, ncol = 2)


year_mean <- mean(Dtrain$year)
x_vec <- Dtrain$year - year_mean
y_vec <- Dtrain$total

for (i in seq_along(Dtrain$year)){
  x <- matrix(c(1, x_vec[i]), nrow = 2, ncol = 1)
  r <- r + (x %*% t(x))
  error <- y_vec[i] - as.numeric(t(x) %*% theta)
  theta <- theta + (solve(r) %*% x * error)
  
}


final_slope <- theta[2,1]
final_intercept <- theta[1,1] - (final_slope * year_mean)

print(paste("RLS Intercept:", final_intercept))
print(paste("RLS Slope:", final_slope))

print(theta_estimates)

theta_df_1 <- data.frame(time=c(),theta_0 = c(), theta_1 = c())
lambda <- 0.7

theta = matrix(c(0,0), nrow=2, ncol=1)
r = matrix(c(0.1,0,0,0.1), nrow = 2, ncol = 2)


year_mean <- mean(Dtrain$year)
x_vec <- Dtrain$year - year_mean
y_vec <- Dtrain$total
for (i in seq_along(Dtrain$year)){
  x <- matrix(c(1, x_vec[i]), nrow = 2, ncol = 1)
  r <- lambda*r + (x %*% t(x))
  error <- y_vec[i] - as.numeric(t(x) %*% theta)
  theta <- theta + (solve(r) %*% x * error)
  rbind(theta_df_1,c(i,theta[2,1],theta[1,1]))
}

final_slope <- theta[2,1]
final_intercept <- theta[1,1] - (final_slope * year_mean)

print(paste("RLS Intercept:", final_intercept))
print(paste("RLS Slope:", final_slope))

theta_df_2 <- data.frame(time=c(),theta_0 = c(), theta_1 = c())
lambda <- 0.99

theta = matrix(c(0,0), nrow=2, ncol=1)
r = matrix(c(0.1,0,0,0.1), nrow = 2, ncol = 2)


year_mean <- mean(Dtrain$year)
x_vec <- Dtrain$year - year_mean
y_vec <- Dtrain$total
for (i in seq_along(Dtrain$year)){
  x <- matrix(c(1, x_vec[i]), nrow = 2, ncol = 1)
  r <- lambda*r + (x %*% t(x))
  error <- y_vec[i] - as.numeric(t(x) %*% theta)
  theta <- theta + (solve(r) %*% x * error)
  rbind(theta_df_2,c(i,theta[2,1],theta[1,1]))
}

final_slope <- theta[2,1]
final_intercept <- theta[1,1] - (final_slope * year_mean)

print(paste("RLS Intercept:", final_intercept))
print(paste("RLS Slope:", final_slope))

ggplot() +
geom_point(data=theta_df_1,x=theta_0,y=1:length(theta_0))


