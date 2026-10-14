# Used Car Price Prediction
# Initial data exploration
# 1. Import the dataset
cars <- read.csv(
  "data/Car details v3.csv",
  stringsAsFactors = FALSE
)
# 2. Inspect the dataset
head(cars)
dim(cars)
names(cars)
# 3. Check missing values
colSums(is.na(cars))
# 4. Convert vehicle specifications to numeric values
cars$mileage_num <- as.numeric(
  sub(" kmpl| km/kg", "", cars$mileage)
)
cars$engine_num <- as.numeric(
  sub(" CC", "", cars$engine)
)
cars$power_num <- as.numeric(
  sub(" bhp", "", cars$max_power)
)
# 5. Check the cleaned variables
summary(cars$mileage_num)
summary(cars$engine_num)
summary(cars$power_num)
# 6. Check missing values after conversion
colSums(is.na(cars[c(
  "mileage_num",
  "engine_num",
  "power_num"
)]))
# 7. Load the plotting package
library(ggplot2)
# 8. Plot selling price against kilometres driven
ggplot(cars, aes(x = km_driven, y = selling_price)) +
  geom_point(alpha = 0.4) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  coord_cartesian(
    xlim = c(0, 300000),
    ylim = c(0, 2000000)
  ) +
  labs(
    title = "Selling Price vs Distance Travelled",
    x = "Distance Travelled (km)",
    y = "Selling Price (₹)"
  ) +
  theme_minimal()
# 9. Plot selling price against fuel efficiency
ggplot(cars, aes(x = mileage_num, y = selling_price)) +
  geom_point(alpha = 0.4) +
  scale_y_continuous(labels = scales::label_comma()) +
  coord_cartesian(
    xlim = c(0, 35),
    ylim = c(0, 2000000)
  ) +
  labs(
    title = "Selling Price vs Fuel Efficiency",
    x = "Fuel Efficiency (km/litre or km/kg)",
    y = "Selling Price (₹)"
  ) +
  theme_minimal()
# 10. Examine the distribution of selling prices
ggplot(cars, aes(x = selling_price)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(
    labels = scales::label_comma()
  ) +
  coord_cartesian(xlim = c(0, 2000000)) +
  labs(
    title = "Distribution of Used Car Selling Prices",
    x = "Selling Price (₹)",
    y = "Number of Cars"
  ) +
  theme_minimal()
  
