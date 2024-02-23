#ANS part-a
library(readxl)
library(dplyr)
file_path <- "D:/New folder/vehicles.xlsx"

data<- read_excel(file_path)
# Extract the input variables
X <- data[, 1:18]

# Detect outliers using the Z-score method
outliers <- as.vector(which(apply(X, 2, function(x) any(abs(x) > 3))))

# Remove outliers
cleaned_X <- X[-outliers, ]

# Perform scaling using scale()
scaled_X <- scale(X)

#ANS part-b
data_subset<-scaled_X

library(NbClust)
library(cluster)
library(factoextra)
#THE FOUR AUTOMATED TOOLS
# 1. NBclust method
nb_results <- NbClust(data_subset, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nb_optimal_clusters <- nb_results$Best.nc
cat("Number of clusters suggested by NBclust:", nb_optimal_clusters, "\n")

# 2. Elbow method
fviz_nbclust(data_subset, kmeans, method = "wss") + ggtitle("Elbow Method")


# 3. Gap statistics method
gap_stat <- clusGap(data_subset, FUNcluster = kmeans, K.max = 10, B = 50)
print(gap_stat$Tab)
fviz_gap_stat(gap_stat)

# 4. Silhouette method
fviz_nbclust(data_subset, kmeans, method = "silhouette")

#ANS part c
library(cluster)
k<-2
kmeans_model <- kmeans(data_subset, centers = k, nstart = 10)
cluster_centers <- kmeans_model$centers
cat("Cluster Centers:\n")
print(cluster_centers)
cluster_assignments <- kmeans_model$cluster
# Display the clustered results
cat("Cluster Assignments:\n")
print(cluster_assignments)

# Calculate the within-cluster sum of squares (WSS)
wss <- kmeans_model$tot.withinss

# Calculate the between-cluster sum of squares (BSS)
tss <- sum(kmeans_model$tot.withinss) + sum(kmeans_model$betweenss)
bss <- tss - wss

print(paste("The value of BSS is:", round(bss, 4)))
print(paste("The value of WSS is:", round(wss, 4)))

bss_ratio <- bss / tss
cat("BSS/TSS Ratio:", bss_ratio, "\n")

#ANS part d
silhouette_scores <- silhouette(kmeans_model$cluster, dist(data_subset))
plot(silhouette_scores, main = "Silhouette Plot",border=NA,col="darkblue")

avg_sil_width <- mean(silhouette_scores[, "sil_width"])
print(paste("The average silhouette width is:", round(avg_sil_width, 4)))



# ANS 2 part e

pca <- prcomp(data_subset, scale = TRUE)
eigenvalues <- pca$sdev^2
eigenvectors <- pca$rotation

print(paste0(" These are the eigenvalues: ", eigenvalues))
print(paste0(" These are the eigenvectors: ", eigenvectors))

cumulative_score <- cumsum(eigenvalues) / sum(eigenvalues) * 100
cumulative_score
selected_pcs <- which(cumulative_score > 92)
transformed_data <- data_subset[, -19] %*% eigenvectors[, selected_pcs]

# ANS 2 part f
library(NbClust)
library(cluster)
#NBclust
nb_results <- NbClust(transformed_data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

#ELBOW
fviz_nbclust(transformed_data, kmeans, method = "wss") + ggtitle("Elbow Method")


#Gap Statistics
gap_stat <- clusGap(transformed_data, FUNcluster = kmeans, K.max = 10, B = 50)
print(gap_stat$Tab)

fviz_gap_stat(gap_stat)


#Silhouette
# 4. Silhouette method
fviz_nbclust(transformed_data, kmeans, method = "silhouette")


# ANS 2 part g
k <- 2 
kmeans_model <- kmeans(transformed_data,centers = k, nstart = 10)
cluster_centers <- kmeans_model$centers
cat("Cluster Centers:\n")
print(cluster_centers)
cluster_assignments <- kmeans_model$cluster
# Display the clustered results
cat("Cluster Assignments:\n")
print(cluster_assignments)

# Calculate the within-cluster sum of squares (WSS)
wss <- kmeans_model$tot.withinss

# Calculate the between-cluster sum of squares (BSS)
tss <- sum(kmeans_model$tot.withinss) + sum(kmeans_model$betweenss)
bss <- tss - wss

print(paste("The value of BSS is:", round(bss, 4)))
print(paste("The value of WSS is:", round(wss, 4)))

bss_ratio <- bss / tss
cat("BSS/TSS Ratio:", bss_ratio, "\n")

# ANS 2 part h
silhouette_scores <- silhouette(kmeans_model$cluster, dist(transformed_data))
plot(silhouette_scores, main = "Silhouette Plot",border=NA,col="darkblue")

avg_sil_width <- mean(silhouette_scores[, "sil_width"])
print(paste("The average silhouette width is:", round(avg_sil_width, 4)))


# ANS 2 part i
library(fpc)
library(ggplot2)

# Initialize empty vectors to store the index values and k values
index_values <- c()
k_values <- c()

# Iterate over different values of k and calculate the Calinski-Harabasz Index
for (k in 2:10) {
  kmeans_result <- kmeans(transformed_data, centers = k)
  diss_matrix <- dist(transformed_data)
  ch_index <- cluster.stats(diss_matrix, kmeans_result$cluster)$ch
  index_values <- c(index_values, ch_index)
  k_values <- c(k_values, k)
}

# Create a data frame of k values and index values
index_df <- data.frame(k = k_values, index = index_values)

# Create a line plot to illustrate the Calinski-Harabasz Index
ggplot(index_df, aes(x = k, y = index)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters (k)", y = "Calinski-Harabasz Index") +
  ggtitle("Calinski-Harabasz Index for Different Number of Clusters")