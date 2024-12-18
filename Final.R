library(readxl)
library(stats)
library(tidyverse)

data <- read_excel("ecommerce.xlsx")

ecommerce <- data[, -((ncol(data)-2):ncol(data))]

summary(ecommerce)

features <- ecommerce %>%
  select(starts_with("F"), starts_with("M"), r, f, m, tof)

normalized_features <- scale(features)

set.seed(13)
kmeans_result <- kmeans(normalized_features, centers = 4) # Adjust centers based on elbow method
ecommerce$cluster <- kmeans_result$cluster

aggregate(features, by = list(cluster = kmeans_result$cluster), FUN = mean)

clustered_data <- cbind(normalized_features, cluster = kmeans_result$cluster)

# Aggregate the data by cluster to get the means of each feature
cluster_summary <- aggregate(. ~ cluster, data = as.data.frame(clustered_data), FUN = mean)
print(cluster_summary)


variance_explained <- (kmeans_result$betweenss / kmeans_result$totss) * 100
print(paste("Variance Explained: ", round(variance_explained, 2), "%"))

# Business impact
total_revenue <- 50000000

# Number of customers in each cluster
cluster_sizes <- table(ecommerce$cluster)

# Proportion of customers in each cluster
cluster_proportions <- cluster_sizes / total_customers

# Define uplift percentages for each cluster
uplift_percentages <- c(0.03, 0.07, 0.10, 0.04)  # 3%, 7%, 10%, 4%

# Estimate incremental revenue from marketing for each cluster
incremental_revenue <- uplift_percentages * cluster_proportions * total_revenue 

# Calculate total incremental revenue
total_incremental_revenue <- sum(incremental_revenue)

# Print results
for (i in 1:length(uplift_percentages)) {
  print(paste("Estimated Incremental Revenue from Cluster", i, ": $", round(incremental_revenue[i], 2)))
}

print(paste("Total Estimated Incremental Revenue from Targeting All Clusters: $", round(total_incremental_revenue, 2)))

