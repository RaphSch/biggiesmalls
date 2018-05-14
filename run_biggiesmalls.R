# Give me a list of parameters
# And an index
# I will run the model

# Empty environment
rm(list = ls())

# Load the function
source("biggiesmalls.R")

# Take arguments in
arguments = as.numeric(commandArgs(TRUE))

# Parameter that can vary
islandSizes = c(100,200,400)
competitiveAdvantages = c(0.01,0.1,1)
resourceScalings = c(-0.01,-0.1,-1)
bodySizes0 = c(10, 1000000)
replicates = c(1,2,3)

# Pick parameter values to run
parameters = NULL
parameters[1] = islandSizes[arguments[1]] #island size
parameters[2] = competitiveAdvantages[arguments[2]] #competitive advantage
parameters[3] = resourceScalings[arguments[3]] #resource scaling
parameters[4] = bodySizes0[arguments[4]] #initial mean body size
parameters[5] = 0.05 #relative initial standard deviation in body size
parameters[6] = 10 #initial population size
parameters[7] = 25000 #simulation time
parameters[8] = 0.05 #mutational standard deviation
parameters[9] = 0.01 #mutation rate
parameters[10] = replicates[arguments[5]] #replicate

# Run the model
biggiesmalls(parameters)
