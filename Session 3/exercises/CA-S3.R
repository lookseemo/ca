#############################################################################
#
# IE - IMBA
# Customer Analytics - S3
# Josep Curto | jcurto@faculty.ie.edu | 2018
# Version: 1.1
#
#############################################################################

# if/elseif/else
x <- c("What", "is", "truth")

if ("Truth" %in% x) {
  print("Truth is found the first time")
} else if ("truth" %in% x) {
  print("truth is found the second time")
} else{
  print("No truth found")
}

# ifelse
ifelse(
  "Truth" %in% x,
  print("Truth is found the first time"),
  ifelse(
    "truth" %in% x,
    print("truth is found the second time"),
    print("No truth found")
  )
)

# For loop
for (i in 1:50) {
  print(i)
}

# Combining if & for
for (i in 1:10) {
  if (i == 6) {
    next
  }
  if (i == 8) {
    break
  }
  print(i)
}

# While loop
i <- 10
while (i > 0) {
  print(i)
  i <- i - 1
  if (i == 8) {
    next
  } else{
    print("ok")
  }
}

# Repeat loop
v <- c("Hello IE", "looping")
counter <- 2

repeat {
  if (counter > 5) {
    break
  }
  print(c(v, counter))
  counter <- counter + 1
}

# Our first function
say.hello <- function(name) {
  print(sprintf("Hello %s", name))
}

hello.person <- function(first, last) {
  print(sprintf("Hello %s %s", first, last))
}

say.hello("Josep")

hello.person("Josep", "Curto")
hello.person(last = "Jobs", "Steve")
hello.person(last = "Gates")

#choosing the file
customerData <- read.csv(file.choose())

# Load a csv file
customerData <- read.csv("s3.csv")
customerData

#reading from a url
fileURL <- "http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat"
download.file(fileURL, destfile = "exampledata.dat")
data <- read.table("exampledata.dat", header = TRUE)
head(data)

#Validate that two datasets are the same
data2 <- read.csv(file = "exampledata.dat", header = TRUE, sep = " ")
all.equal(data, data2)

#Load data from a website
theurl <- "http://jaredlander.com/data/Tomato%20First.csv"
tomato <- read.table(file = theurl, header = TRUE, sep = ",")

#choosing the file
save(tomato, file = "tomato.data")
rm(tomato)
load(file = "tomato.data")

# Creating Graphs --------------------------------

.packages = c("ggplot2", "dplyr")
.inst <- .packages %in% installed.packages()
if (length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst])
lapply(.packages, library, character.only = TRUE)

library(ggplot2)
library(dplyr)
data("diamonds")

## We can use various options for geoms:
# point for scatterplots
# smooth for smoothing lines
# boxplot for boxplot
# path for segment based on the order in the data
# line for a line graph based on the x order
# stairstep for a stairstep plot

# Scatterplot
gg <- ggplot(diamonds, aes(x = carat, y = price,
                           color = cut)) + geom_point() + labs(title = "Scatterplot",
                                                               x = "Carat", y =
                                                                 "Price")
gg

# I can use log transformations directly
ggplot(diamonds, aes(x = carat, y = price, color = cut)) + geom_point() +
  scale_x_log10() + scale_y_log10()

#plot diamonds per carat changing the shape per cut
set.seed(27)
dsmall <- sample_n(diamonds, 100)

ggplot(dsmall, aes(x = carat, y = price, shape = cut)) + geom_point()

#line graphs
ggplot(dsmall, aes(x = carat, y = price)) + geom_line(alpha = I(0.2)) +
  geom_point(color = I("green"))

#geom_histogram() creates a histogram
ggplot(diamonds, aes(x = price)) + geom_histogram()
ggplot(diamonds, aes(x = price)) + geom_histogram(binwidth = 2000)

#use fill param to create a stacked histogram
ggplot(diamonds, aes(x = price, fill = clarity)) + geom_histogram()

#geom_density to plot density curves
ggplot(diamonds, aes(x = price, color = cut)) + geom_density()

#boxplots
ggplot(dsmall, aes(x = cut, y = price)) + geom_boxplot(outlier.color = I("red")) + geom_point()
ggplot(dsmall, aes(x = cut, y = price)) + geom_boxplot(outlier.color = I("red")) + geom_jitter()

#changing the y scale to accomodate for the outliers
ggplot(diamonds, aes(x = cut, y = price)) + geom_boxplot() + scale_y_log10()

#violin plots incjude information on the relative frequencies
ggplot(diamonds, aes(x = cut, y = price)) + geom_violin() + scale_y_log10()

viol <-
  ggplot(diamonds, aes(x = cut, y = price)) + geom_violin() + scale_y_log10() + facet_wrap( ~ clarity)
viol

# Using facets we can plot various subplots that represent the same graph
# using a categorial
# variable to split the plots, facets in grids

gg1 <-
  ggplot(diamonds, aes(x = price)) + geom_histogram(binwidth = 200) + facet_wrap( ~ clarity, scale =
                                                                                    "free_y")
print(gg1)
gg1 + facet_wrap(~ cut, ncol = 3)
gg1 + facet_grid(color ~ cut)

#using themes you can set the general style of your graph
gg1 + theme_minimal()
gg1 + theme_dark()

#saving with ggsave
ggsave(filename = "diamonds.png", viol)