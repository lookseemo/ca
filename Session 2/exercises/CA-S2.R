#############################################################################
# 
# IE - IMBA
# Customer Analytics - S2
# Josep Curto | jcurto@faculty.ie.edu | 2018
# Version: 1.1
# 
#############################################################################

# Our first R code
years <- c(1,3,5,8,10)
result <- exp(0.05*years)
print(result)

# We can improve the code
rate <- 0.05
result <- exp(rate*years)
print(result)

# Available packages for R
dim(available.packages())

# Installed packages for R
(.packages())

# Install with install.packages(name)
install.packages("ggplot2")

# Uninstall with remove.packages(name)
remove.packages("ggplot2")

# Load with library(name) - we install again the library - 
install.packages("ggplot2")
library("ggplot2")

# You can also use require() but it is a bad programming practice

# Package-level help: library(help=name)
library(help=ggplot2)

# Unload with detach(package:name)
detach("package:ggplot2", unload=TRUE)

# Loads the dataset in memory with data(name)
library(ggplot2)
data("diamonds")

# Shows datasets available
data()

# Exploring the dataset
View(diamonds)
head(diamonds)
tail(diamonds)

# Values assigment
a <- 5
b <- a
a <- 7
print(b)

# Dynamically type variables
x <- 5
class(x)
x <- "hello"
class(x)
x <- 5L
class(x)
x <- TRUE
class(x)
x[1]
class(x[1])

# Modes vs Class
crashCourseDate <- as.Date("2016-10-19")
typeof(crashCourseDate)
class(crashCourseDate)

#  Vectors
x <- c("a","b","c")
length(x)

x <- c(s1=0.5,s2=0.8,s3=0.1)
x
sort(x)
names(x)

y <- c(0.5,0.8,0.1)
names(y) <- c("s1","s2","s3")
y

# Statistical functions
mean(x)
var(x)
max(x)
min(x)
summary(x)

# Scalar is a vector
x <- 34
x[1]
x[2]
length(x)

# Vector creation
a <- c(1,2,3)
a
b <- seq(0,10, by=2)
b
c <- rep(b, times=5)
c

# Special Values
x <- c(1:3,NA)
is.na(x)

1/0
2^35760
-3^35760

log(-34)
0/0

x <- c(8,2,3,NULL)
x

x[2] <- NULL
x

x[2] <- NaN
x

# Vector subsetting
x <- c("a","b","c","d","e","f")
x[2]
x[length(x)]

# Vectors adding an element
x <- c(1,2,3)
x
x[3] <- 45
x
x[7] <- 48
x
append(x, c(1,23))
y <- c(45,23,32)
x <- c(x,y)
x
x <- append(x, 34, after=4)
x

# Lists
student <-list(name="Josep",surname="Curto Díaz", 
               male=TRUE, age=39,grades=list(5,6,7,8))
student[1]
student$age
student
student$hobby <- "Music"
student$skill <- "Mathematics"
student

# Matrices

m <- matrix(data=1:12, nrow=4, ncol=3, dimnames = list(c("r1","r2","r3","r4"),c("c1","c2","c3")), byrow=TRUE)
m[1,3]
m[1,]
m[,3]
m["r1","c2"]
m[c(1:2),c(1:3)]

m <- rbind(m, r5=c(13,14,15))
m

m <- cbind(m, c4=c(34,23,12,11,67))
m

c(m)

rownames(m) <- c("row1","row2","row3","row4","row5")
colnames(m) <- c("col1","col2","col3","col4")

m2 <- matrix(data=13:32, nrow=5, ncol=4, dimnames = list(c("r1","r2","r3","r4","r5"),c("c1","c2","c3","c4")), byrow=TRUE)
m2

cbind(m, m2)
rbind(m,m2)

x <- matrix(data=1:12, nrow=4, ncol=3, byrow=TRUE)
x
y[4] <- 4
dim(y) <- c(4,1)
y
t(x)
diag(4)