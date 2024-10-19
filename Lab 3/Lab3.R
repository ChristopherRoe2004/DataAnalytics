###Set Up###
library(class)
library(ggplot2)
epi_data <- read.csv("epi2024results_DA_F24_lab03.csv")

###Exercise 1###

#subset data by regions#
#Southern Asia#
region1_data <- subset(epi_data, region == "Southern Asia")

#Global West#
region2_data <- subset(epi_data, region == "Global West")

#Southern Asia ECO Histogram#
hist(region1_data[["ECO"]], 
     freq = FALSE, 
     main = "Histogram of ECO for Southern Asia")
lines(density(region1_data[["ECO"]]))

#Global West ECO Histogram#
hist(region2_data[["ECO"]], 
     freq = FALSE, 
     main = "Histogram of ECO for Global West")
lines(density(region2_data[["ECO"]]))

#Southern Asia ECO QQ Plot#
qqnorm(region1_data$"ECO")
qqline(region1_data$"ECO")

#Global West ECO QQ Plot#
qqnorm(region2_data$"ECO")
qqline(region2_data$"ECO")

###Exercise 2###
#fit the model#
full_model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = epi_data)

#summary#
summary(full_model)

#Plot ECO against BDH#
plot(epi_data$"ECO", epi_data$"BDH",
     main = "Scatterplot of ECO vs BDH",
     xlab = "ECO",
     ylab = "BDH")

#add fitted line#
abline(lm(BDH ~ ECO, data = epi_data))

#for Global West#
GW_model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = region2_data)

#summary#
summary(GW_model)

#Plot ECO against BDH#
plot(region2_data$"ECO", region2_data$"BDH",
     main = "Scatterplot of ECO vs BDH",
     xlab = "ECO",
     ylab = "BDH")

#add fitted line#
abline(lm(BDH ~ ECO, data = region2_data))

#I believe that the full model is a better fit, as it has a lower r-squared value

###Exercise 3###

# Subset data by regions for kNN model
# Southern Asia, Global West, Eastern Europe #
region_knn_data1 <- subset(epi_data, region %in% c("Southern Asia", "Global West", "Eastern Europe"))

# Select 5 variables to use in the model (e.g., ECO, BDH, MKP, MHP, MPE) and region
region_knn_data1 <- region_knn_data1[, c("ECO", "BDH", "MKP", "MHP", "MPE", "region")]

# Remove rows with missing values
region_knn_data1 <- na.omit(region_knn_data1)

# Split data into training and test sets (70% training, 30% test)
set.seed(123)
train_index1 <- sample(1:nrow(region_knn_data1), 0.7 * nrow(region_knn_data1))
train_data1 <- region_knn_data1[train_index1, ]
test_data1 <- region_knn_data1[-train_index1, ]

# Train kNN model for the first set of regions (Southern Asia, Global West, Eastern Europe)
knn_model1 <- knn(train = train_data1[, 1:5], 
                  test = test_data1[, 1:5], 
                  cl = train_data1$region, 
                  k = 3)

# Contingency matrix for first kNN model
contingency_matrix1 <- table(test_data1$region, knn_model1)
contingency_matrix1

# Calculate accuracy of the first kNN model
correct_classifications1 <- sum(test_data1$region == knn_model1)
total_classifications1 <- length(knn_model1)
accuracy1 <- correct_classifications1 / total_classifications1
accuracy1

# Subset data for another set of regions: Asia-Pacific, Greater Middle East, Sub-Saharan Africa
region_knn_data2 <- subset(epi_data, region %in% c("Asia-Pacific", "Greater Middle East", "Sub-Saharan Africa"))

# Select the same 5 variables and region for the second set
region_knn_data2 <- region_knn_data2[, c("ECO", "BDH", "MKP", "MHP", "MPE", "region")]

# Remove rows with missing values
region_knn_data2 <- na.omit(region_knn_data2)

# Split data into training and test sets for the second set (70% training, 30% test)
set.seed(123)
train_index2 <- sample(1:nrow(region_knn_data2), 0.7 * nrow(region_knn_data2))
train_data2 <- region_knn_data2[train_index2, ]
test_data2 <- region_knn_data2[-train_index2, ]

# Train kNN model for the second set of regions (Asia-Pacific, Greater Middle East, Sub-Saharan Africa)
knn_model2 <- knn(train = train_data2[, 1:5], 
                  test = test_data2[, 1:5], 
                  cl = train_data2$region, 
                  k = 3)

# Contingency matrix for the second kNN model
contingency_matrix2 <- table(test_data2$region, knn_model2)
contingency_matrix2

# Calculate accuracy of the second kNN model
correct_classifications2 <- sum(test_data2$region == knn_model2)
total_classifications2 <- length(knn_model2)
accuracy2 <- correct_classifications2 / total_classifications2
accuracy2

# Compare accuracies
if (accuracy1 > accuracy2) {
  cat("The first kNN model is better with an accuracy of", accuracy1)
} else {
  cat("The second kNN model is better with an accuracy of", accuracy2)
}

###Exercise 4###
# Subset data for Group 1: "Southern Asia", "Global West", "Eastern Europe"
region_kmeans_data1 <- subset(epi_data, region %in% c("Southern Asia", "Global West", "Eastern Europe"))

# Select 5 variables for k-means model (e.g., ECO, BDH, MKP, MHP, MPE)
region_kmeans_data1 <- region_kmeans_data1[, c("ECO", "BDH", "MKP", "MHP", "MPE")]

# Remove rows with missing values
region_kmeans_data1 <- na.omit(region_kmeans_data1)

# Subset data for Group 2: "Asia-Pacific", "Greater Middle East", "Sub-Saharan Africa"
region_kmeans_data2 <- subset(epi_data, region %in% c("Asia-Pacific", "Greater Middle East", "Sub-Saharan Africa"))

# Select the same 5 variables for k-means model
region_kmeans_data2 <- region_kmeans_data2[, c("ECO", "BDH", "MKP", "MHP", "MPE")]

# Remove rows with missing values
region_kmeans_data2 <- na.omit(region_kmeans_data2)

###Fit k-means for both subsets and compare WCSS###

# Fit k-means model with 3 clusters for Group 1
kmeans_model1 <- kmeans(region_kmeans_data1, centers = 3)

# Fit k-means model with 3 clusters for Group 2
kmeans_model2 <- kmeans(region_kmeans_data2, centers = 3)

# Compare WCSS (within-cluster sum of squares)
wcss1 <- kmeans_model1$tot.withinss
wcss2 <- kmeans_model2$tot.withinss

###Fit k-means for multiple k values###

# Define a function to compute WCSS for a range of k values
compute_wcss <- function(data, max_k) {
  wcss_values <- numeric(max_k)
  for (k in 1:max_k) {
    kmeans_model <- kmeans(data, centers = k)
    wcss_values[k] <- kmeans_model$tot.withinss
  }
  return(wcss_values)
}

# Set the maximum number of clusters to try
max_k <- 10

# Compute WCSS for a range of k values for both groups
wcss_values1 <- compute_wcss(region_kmeans_data1, max_k)
wcss_values2 <- compute_wcss(region_kmeans_data2, max_k)

# Plot WCSS across k values for both groups
k_values <- 1:max_k

# Create data frames for plotting
df1 <- data.frame(k_values = k_values, WCSS = wcss_values1, Group = "Southern Asia, Global West, Eastern Europe")
df2 <- data.frame(k_values = k_values, WCSS = wcss_values2, Group = "Asia-Pacific, Greater Middle East, Sub-Saharan Africa")
wcss_df <- rbind(df1, df2)

# Plot
ggplot(wcss_df, aes(x = k_values, y = WCSS, color = Group)) +
  geom_line() +
  geom_point() +
  labs(title = "WCSS Across k Values", x = "Number of Clusters (k)", y = "WCSS") +
  theme_minimal()

#From the WCSS plot, the model with lower WCSS, Suthern Asia, Global West, and Eastern Europe, suggest a better fit#
