# Example script if you want to plot the mean body size
# You need to have created the biggiMeans data files already

rm(list = ls())

# Set working directory
setwd("where/outputs/are")

# Make a data frame with all possible parameter combinations

# All parameter values that should have been simulated
islandSizes <- c(100, 200, 400, 800, 1600, 3200, 6400)
competitiveAdvantages <- c(0,0.001,0.01,0.1,1,5)
resourceScalings <- c(0,-0.001,-0.01,-0.1,-0.5,-1,-2)
bodySizes0 <- c(1,10,100,1000,10000,100000,1000000,10000000)
sdBodySize0 <- 0.05 * bodySizes0
popSize0 <- 10
maxTime <- 50000
mutationalSd <- 0.05
mutationRate <- 0.01

# Create all possible combinations
allCombi <- expand.grid(islandSizes, competitiveAdvantages, resourceScalings, bodySizes0, sdBodySize0, popSize0, maxTime, mutationalSd, mutationRate)

allCombiFiles <- apply(allCombi, 1, function(x) paste(x, collapse = '_'))
allCombiFiles <- paste0("biggiMeans_", allCombiFiles)

# All simulations don't have the same exact timepoints
# So in order to merge together several replicates, it's good to pre-define fixed timepoints
# for which we will need to extract information in each of the simulations

# Fixed timepoints
fiexedTimepoints <- seq(0, maxTime, 50)

# Look for each combination in the directory
# And return an average of all replicates for that combination
simuls <- lapply(allCombiFiles, function(curr.file) {
  
  # Locate the combination in the directory
  i <- grep(curr.file, list.files())
  
  # If the combination exists...
  if(length(i) > 0) {
    
    # For each occurrence of the combination in the directory (i.e. for each replicate)
    simuls <- lapply(i, function(i) {
      
      # Load the replicate (it's a vector of mean body sizes)
      simul <- readRDS(list.file()[i])
      
      # Extract the timepoints
      timepoints <- as.numeric(names(simul))
      
      # Smooth the curve
      simulSmooth <- loess(simul ~ timepoints) # simulSmooth is now an object analogous to a linear model rather than a simple vector of numbers
      
      # Use the smoothed curve to predict the value of the mean body size at each of the fixed timepoints
      simulPredict <- predict(simulSmooth, fixedTimePoints) # simulPredict is a simple vector of numbers
      
      return(simulPredict)
      
    }) #end of for each replicate
    
    # Merge all replicates by column into a data frame
    simuls <- do.call("cbind", simuls)
    
    # Average all replicates
    avgSimuls <- apply(simuls, 1, mean, na.rm = T)
    
    return(avgSimuls)
    
  } else {
    
    return(NA)
    
  } #end of if combination exists
  
}) #end of for each combination

# The resulting object should be a list with one vector of mean body sizes for each parameter combination
# Each vector should be of the same length as there are fixed time points
# Except for some combinations that have not been found in your directory (e.g. you haven't simulated them or they have crashed i.e. gone extinct)
# These exceptions would be NAs instead of vectors

# That's all for now!
# Look at this script, try to run it and explore how you can manipulate the output to make your plots ;)
