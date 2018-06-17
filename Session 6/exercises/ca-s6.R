#############################################################################
# IE Business School
# Customer Analytics
# S5 - NPS
# @JosepCurto | jcurto@faculty.ie.edu | 2018
# Version: 1.8
#############################################################################

# Clear console
cat("\f")

# Clear plots
graphics.off()

# Cleaning the environment
rm(list=ls())

# List of packages for session
.packages = c("NPS")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Data Set generation
x <- sample(0:10, prob=c(0.02, 0.01, 0.01, 0.01, 0.01, 0.03, 0.03, 0.09,
                         0.22, 0.22, 0.35), 1000, replace=TRUE)

# Data Exploration
summary(x)

# Question: Does it make sense to explore the data?

# Frequency table
prop.table(table(x))

# Histogram (v1)
hist(x, breaks=-1:10, col=c(rep("red",7), rep("yellow",2), rep("green", 2)))

# Question: What can we observe?

# Histogram (v2)
barplot(prop.table(table(x)),col=c(rep("red",7), rep("yellow",2), rep("green", 2)))

# NPS Calculation
nps(x) # equivalent nps(x, breaks = list(0:6, 7:8, 9:10))

# Question: What can we observe?

# Standard Error
nps.se(x)

# Variance
nps.var(x)