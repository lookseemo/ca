#############################################################################
# IE Business School
# Customer Analytics
# S9 - Cohort Analysis
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
.packages = c("readxl", "dplyr", "reshape","ggplot2")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Load data into a dataframe
df <- read_excel("data/s9.xlsx", sheet="2016")
df

# How many people complete the MOOC
Finished <- df$Finished[49]
Finished

# Question: Is this a good result?
Ratio <- df$Finished[49] / df$Started[49] *100
Ratio

# Ratio Evolution
RatioEvolution <- data.frame(day = df$Day, ratio=df$Finished / df$Started *100)
RatioEvolution

# Ratio Evolution Graph
g1 <- ggplot(RatioEvolution, aes(x = day, y = ratio)) + 
  geom_line() + ggtitle("Completition Ratio Evolution") + 
  ylab("Ratio") + xlab("Period") +
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=10))
g1

# Ratio Evolution Graph vs MOOC objectives
g1 + geom_vline(xintercept=35, colour="red") + geom_hline(yintercept=20, colour="red")

# How we research about the evolution
df_finished <- dplyr::select(df, contains("Finished"))
df_finished <- data.frame(day = df$Day, df_finished)
df_finished.chart <- melt(df_finished, id.vars = "day")
colnames(df_finished.chart) <- c('Day', 'Cohort', 'Students')

# Let's create a graph
p <- ggplot(df_finished.chart, aes(x=Day, y=Students, group=Cohort, colour=Cohort))
p + geom_line() + ggtitle('Students Completition per day and cohort')

# Question: What we observe?

# Let's create another graph
p1 <- ggplot(df_finished.chart, aes(x=Cohort, y=Students, group=Day, colour=Day))
p1 + geom_line() + ggtitle('Students Completition per day and cohort')

# Let's do the same for the completition ratio
df_finished_ratio <- as.data.frame(apply( df_finished, 2, function(x) x/df$Started*100 )) 
df_finished_ratio$day <- df_finished$day
df_finished_ratio.chart <- melt(df_finished_ratio, id.vars = "day")
colnames(df_finished_ratio.chart) <- c('Day', 'Cohort', 'Ratio')

# Let's create a graph
p2 <- ggplot(df_finished_ratio.chart, aes(x=Day, y=Ratio, group=Cohort, colour=Cohort))
p2 + geom_line() + ggtitle('Completition Ratio per day and cohort')

# Let's create another graph
p3 <- ggplot(df_finished_ratio.chart, aes(x=Cohort, y=Ratio, group=Day, colour=Day))
p3 + geom_line() + ggtitle('Completition Ratio per day and cohort')

# Question: What we observe?