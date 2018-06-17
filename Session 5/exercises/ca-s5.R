#############################################################################
# IE Business School
# Customer Analytics
# S5 - RFM Analysis
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
.packages = c("readr", "dplyr","ggplot2")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) suppressMessages(install.packages(.packages[!.inst]))

# Load packages into session 
suppressMessages(lapply(.packages, require, character.only=TRUE))

# Load external functions
source("RFM.R")

# Load data into a dataframe
df <- read_csv("data/s5.csv")

# Review data
str(df)
View(df)
summary(df)

#  Calculate RFM
dfRFM <- rfm(df,
           analysisDate = "2015/01/01", 
           customer = "customerID", 
           date = "date", 
           revenue = "amount")

# Let's review TOP 10
head(dfRFM, n=10)

# Let's bottom TOP 10
tail(dfRFM, n=10)

# Question: what we can observe?

# Let's review what we have
countRFM <- count(dfRFM, RFM_Score)
countRFM

# Let's create a graph
ggplot(dfRFM, aes(factor(RFM_Score))) +
  geom_bar() +
  ggtitle('Customer Distribution per RFM') +
  labs(x="RFM",y="# Customers") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold")) +
  theme(axis.text.x  = element_text(angle=90,vjust=0.5))

# Question: what we can observe?

# Let's create an easy segmentation
dfRFM$segment <- "NA"
dfRFM$segment[which(dfRFM$Recency > 365/2)] = "Inactive"
dfRFM$segment[which(dfRFM$Recency <= 365/2 & dfRFM$Recency > 365/4)] = "Sleeping"
dfRFM$segment[which(dfRFM$Recency <= 365/4)] = "Active"

# Let's review the outcome
customerSegmentation <- count(dfRFM, segment)
View(customerSegmentation)

# Let's create a graph
ggplot(dfRFM, aes(factor(RFM_Score),fill =factor(segment))) +
  geom_bar() +
  ggtitle('Customer Distribution per Recency') +
  labs(x="RFM",y="# Customers") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold")) +
  theme(axis.text.x  = element_text(angle=90,vjust=0.5,size=8)) + 
  facet_grid(. ~ segment) + theme(legend.position="none")

# Let's create another graph
ggplot(dfRFM, aes(factor(RFM_Score),fill =factor(segment))) +
  geom_bar() +
  ggtitle('Customer Distribution per Recency') +
  labs(x="RFM",y="# Customers") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold")) +
  theme(axis.text.x  = element_text(angle=90,vjust=0.5,size=8)) + 
  theme(legend.title=element_blank())

# Question: what we can observe?

# Let's keep the outcome
write.csv(dfRFM,"RFM.csv")