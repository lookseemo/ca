#############################################################################
# IE Business School
# Customer Analytics with R
# S8 - Customer Segmentation
# @JosepCurto | jcurto@faculty.ie.edu | 2018
# Version: 1.9
#############################################################################

# Clear console
cat("\f")

# Clear plots
graphics.off()

# Cleaning the environment
rm(list=ls())

# List of packages for session
.packages = c("ggplot2", "NbClust","ggfortify","GGally","cluster","factoextra","clustertend")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Load Data
data <- read.csv('data/s8.csv', header = T,sep=',')

# Review structure
str(data)

# Main Statistics
summary(data)

# Let's review what happens (correlation)
ggcorr(data, label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)

# Hopkins Statistic
hopkins(data,n = nrow(data)-1)

# What is happening?

## PCA

# Let's review the PCA
autoplot(prcomp(data))

# Another graph (color using one variable)
autoplot(prcomp(data), data = data, colour = 'Channel')

# One more graph (eigenvalues)
autoplot(prcomp(data), data = data, colour = 'Channel', loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

## Customer Segmentation
set.seed(123)

# Data normalization
testdata <- data 
testdata <- scale(testdata)

# Looking for the number of clusters. Option 1
wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(testdata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Looking for the number of clusters. Option 2
res<-NbClust(data, diss=NULL, distance = "euclidean", min.nc=2, max.nc=12, 
             method = "kmeans", index = "all")

# More information
res$All.index
res$Best.nc
res$All.CriticalValues
res$Best.partition

# K-Means Cluster Analysis
fit <- kmeans(testdata, 3)

# what contains this object
fit$centers
fit$size

# Average for every group
clusters <- aggregate(data,by=list(fit$cluster),FUN=mean)
# Review
clusters

# Add segmentation
data <- data.frame(data, fit$cluster)
# Review
head(data)

# a plot
ggplot(data, aes(Fresh, Delicassen, color = as.factor(fit$cluster))) + geom_point()

# Another plot
autoplot(kmeans(data, 3), data = data)

# What is happening?

# Another clustering technique
autoplot(fanny(data, 3), frame = TRUE)

# Another clustering technique
autoplot(pam(data, 3), frame = TRUE)

# What is happening?