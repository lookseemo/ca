#############################################################################
# 
# IE - IMBA
# Customer Analytics - S2 - Best Practices
# Josep Curto | jcurto@faculty.ie.edu | 2018
# Version: 1.0
# 
#############################################################################

# How to clean graphs in memory
graphics.off()

# How to clean the environment
rm(list=ls())

# How to clean the terminal
cat("\f")

# How to prepare the enviroment
.packages = c("ggplot2")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, library, character.only=TRUE)