#############################################################################
# IE Business School
# Customer Analytics
# S4 - CLV
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
.packages = c("readxl","ggplot2")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, library, character.only=TRUE)

# Load data into a dataframe
df <- read_excel("data/s4.xlsx", sheet = "Ex2")

# Summary
summary(df)

# Question: What we can say about the summary?

# Graph: how the number of customers is evolving
ggplot(df, aes(x = t, y = active)) +
  geom_line() + ggtitle("Active Customer Evolution") +
  ylab("Customer") + xlab("Period") +
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=10))

# Graph: how the retention ratio is evolving
ggplot(df, aes(x = t, y = r)) + 
  geom_line() + ggtitle("Retention Ratio Evolution") + 
  ylab("Customer") + xlab("Period") +
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=10))

# Partial CLV
df$CLV <- (df$p-df$c)*df$r/(1+df$i)^(df$t-1)

# Now we have a new column
df

#  Graph: how the partial CLV is evolving
ggplot(df, aes(x = t, y = CLV)) + geom_line() + ggtitle("CLV evolution") + ylab("CLV") + xlab("Period") + theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=10))

# Question: What do we observe?

# Final step
CLV <- apply(df, 2, sum)
CLV[7]

# What does it mean this value?

# Question: What happens if retention ratio has a constant value of 0.80?