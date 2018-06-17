#############################################################################
# IE Business School
# Customer Analytics
# S7 - Association Analysis
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
.packages = c("arules", "arulesViz")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Load data
edata <- read.csv("data/s7.csv",stringsAsFactors=FALSE, sep=',')

# data preparation
edata$id_purchase <- factor(edata$id_purchase)
edata$product <- factor(edata$product)

# Some insights
length(unique(edata$id_purchase))
nrow(edata)/length(unique(edata$id_purchase))
sapply(split(edata$product,edata$product),length)
pList<-t(as.data.frame(lapply(split(edata$product,edata$product),length)))
head(pList[order(pList[,1], decreasing= T),], n = 1)
tail(pList[order(pList[,1], decreasing= T),], n = 1)

# Review data
dim(edata)
summary(edata)

# Prepare data
i <- split (edata$product, edata$id_purchase)

# Transform into transaction object
txn <- as(i,"transactions")
txn

# Explore transactions
summary(txn)
inspect(txn)

# Inspect binary incidence matrices
image(txn)

# Product frequency
itemFrequency(txn)

# Plot the frequency of items sets
itemFrequencyPlot(txn)

# Similarity between items
d <- dissimilarity(txn, method = "phi", which = "items")
d[is.na(d)] <- 1 # get rid of missing values
plot(hclust(d), cex=.5)

# Applying apropri algorithm
basket_rules <- apriori(txn,parameter = list(sup = 0.005, conf=0.01, minlen=2, target = "rules"))

# Understanding the output
summary(basket_rules)
inspect(head(basket_rules, n=27, by = "lift"))

# Find rules where the LHS and the RHS depend on each other.
inspect(basket_rules[is.significant(basket_rules, txn)])

# Find redundant rules.
inspect(basket_rules[is.redundant(basket_rules)])

# Find non redundant rules
inspect(basket_rules[!is.redundant(basket_rules)])

# We can create a dataframe to save all the metrics and analyze them in detail
df_rules <- interestMeasure(basket_rules, c("support", "chiSquare", "confidence", "conviction",
                                            "cosine", "coverage", "leverage", "lift", "oddsRatio"), txn)
df_rules

# Plotting the output
plot(basket_rules)

# Another (better) visualization
plot(basket_rules,
     method="graph",
     measure="confidence",
     shading="lift", control=list(type="items"))

# Question: What is happening?

# Refining our analysis
basket_rules.refined <- apriori(txn,parameter = list(minlen=2, sup = 0.05, conf = 0.2,target="rules"))
summary(basket_rules.refined)
inspect(basket_rules.refined)

# Finding the maximal
maximal <- is.maximal(basket_rules.refined)
inspect(basket_rules.refined[maximal])

# Plotting the output
plot(basket_rules.refined)

plot(basket_rules.refined,
     method="graph",
     measure="confidence",
     shading="lift", control=list(type="items"))

plot(basket_rules.refined, method = "graph",
     control = list(main = "Rules", type = "itemsets"))

# Another way
plot(basket_rules.refined, method = "grouped")

# Question: what is happening?
# Question: what do you recommend?