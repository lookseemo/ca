#############################################################################
# 
# IE - IMBA
# Customer Analytics - S3 - Best Practices
# Josep Curto | jcurto@faculty.ie.edu | 2018
# Version: 1.1
# 
#############################################################################

# How to load a function in a script

if (!exists("hello.person", mode="function")){
  source("functions.R")
}

hello.person("Michael","Jordan")

# How to measure the performance of a similar code

load("dataTB.rdatq")
.packages = c("microbenchmark")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, library, character.only=TRUE)

microbenchmark::microbenchmark(
  dataTickets$customer[which(dataTickets$final_price == max(dataTickets$final_price))], 
  dataTickets[dataTickets$final_price==max(dataTickets$final_price),]$customer, 
  times = 10000)