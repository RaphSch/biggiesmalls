# Give me a list of parameters
# And an index
# I will run the model



# Empty environment
rm(list = ls())

# set wd
setwd("D:/Documents/SCIENCE/PROJECTS/PhD/Biggiesmalls/CER")

# Load the function
source("biggiesmalls.R")

# Take arguments in
arguments = as.numeric(commandArgs(TRUE))
arguments = 1

# Create all combinations of parameters to run
#allParameters = expand.grid(
#  c(100,200,400), #island size
#  c(0.01,0.1,1), #competitive advantage
#  c(-0.01,-0.1,-1), #resource scaling
#  c(10,1000000), #initial body size
#  0.05, #standard deviation of initial body size
#  10, #initial population size
#  25000, #simulation time
#  0.05, #mutational standard deviation
#  0.01, #mutation rate
#  c(1,2,3) #replicates
#)

# Or arrange your own custom table of parameters to run
# It has to have 10 columns

missingcombs <- read.csv("missingcombis.csv")

# Pick the right combination to run (turn it into a vector)
parameters = do.call("c", missingcombs[arguments[1],])


# Run the model
biggiesmalls(parameters)

