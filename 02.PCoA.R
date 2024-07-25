# Load the vegan packages
library(vegan)

# read data
dt_brain <- read.csv("data_norma_pca.txt",sep="\t", row.names=1, header=T)
as.data.frame(t(dt_brain))
head(dt_brain)

# Perform PERMANOVA analysis using the adonis2 function
dt.div <- adonis2(dt_brain ~ group, data = gp_brain, permutations = 999, method="bray")
# Output the analysis results
dt.div

# Calculate weighted Bray-Curtis distance
dt_dist <- vegdist(dt_brain, method="bray", binary=F)

# Perform classical multidimensional scaling (PCoA) to reduce dimensions
dt_pcoa <- cmdscale(dt_dist, k=3, eig=T)

# Convert PCoA results to a data frame
dt_pcoa_points <- as.data.frame(dt_pcoa$points)

# Calculate the sum of eigenvalues
sum_eig <- sum(dt_pcoa$eig)
# Calculate the percentage of variance explained by each PCoA axis
eig_percent <- round(dt_pcoa$eig/sum_eig*100,1)

# Name the columns of the PCoA result
colnames(dt_pcoa_points) <- paste0("PCoA", 1:3)

# Combine PCoA results with grouping information
dt_pcoa_result <- cbind(dt_pcoa_points, gp_brain)
head(dt_pcoa_result)

# Write the PCoA results to a CSV file
write.csv(dt_pcoa_result, file = "PCA_brain.csv", row.names = T)

# Load the ggplot2 library for plotting
library(ggplot2)
# read data
dt_pca <- read.csv("PCA_brain.csv")

# Create a ggplot object with PCoA1 and PCoA2 as the x and y axes, and color by group
p <- ggplot(dt_pcoa_result, aes(x=PCoA1, y=PCoA2, color=group)) +
  labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep="")) +
  geom_point(size = 4, alpha = 0.3, stroke = 0.5) + 
  scale_color_manual(values = c("W8136" = "red", "LA" = "blue"))+
  stat_ellipse(level= 0.9) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  xlim(-0.1, 0.1) +
  ylim(-0.1, 0.1)

# Save the plot as a PDF file with specified dimensions
ggsave("pca.pdf", plot = p, width = 4, height = 2.5)


