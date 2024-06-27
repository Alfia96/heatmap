library(readxl)
library(pheatmap)


# Load the Excel file
file_name <- "p-distance_TN.xls"
data <- read_excel(file_name)

# Inspect the data structure
print(head(data))

# As data is in lower triangular format and the first column/row are identifiers
matrix_data <- data[-1, -1] 
full_matrix <- as.matrix(matrix_data)
full_matrix[upper.tri(full_matrix)] <- t(full_matrix)[upper.tri(full_matrix)]
print(full_matrix)

# Check for NA, NaN, or Inf values
print(sum(is.na(full_matrix)))
print(sum(is.nan(full_matrix)))
print(sum(is.infinite(full_matrix)))

# Function to replace NA values in each row with the row mean 
replace_na_with_row_mean <- function(row) {
  row[is.na(row)] <- mean(row, na.rm = TRUE)
  return(row)
} 

# Apply the function to each row 
full_matrix <-t(apply(full_matrix, 1, replace_na_with_row_mean)) 

# Verify there are no more NA values 
print(sum(is.na(full_matrix)))

# Generate the heatmap
pheatmap(full_matrix, display_numbers = TRUE)


#Customize the heatmap
pheatmap(full_matrix, 
         display_numbers = TRUE,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         fontsize = 10,
         cellwidth = 15,
         cellheight = 15)
# Save the heatmap as a PNG
png("heatmap.png", width = 800, height = 800)
pheatmap(full_matrix, display_numbers = TRUE)
dev.off()

# Save the heatmap as a PDF
#pdf("heatmap.pdf", width = 8, height = 8)
#pheatmap(full_matrix, display_numbers = TRUE)
#dev.off()
# Hierarchical clustering
dist_matrix <- dist(full_matrix)
hc <- hclust(dist_matrix)
plot(hc)

# PCA analysis
pca <- prcomp(full_matrix, scale. = TRUE)
plot(pca$x, col = "blue", pch = 19)


