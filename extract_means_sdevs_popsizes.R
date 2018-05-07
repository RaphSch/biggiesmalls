# Empty environment
rm(list = ls())

# Set the working directory
setwd("where/my/results/are")

# First, load each simulation output and produce lighter files containing either mean, variance or population size

# For each simulation
sapply(list.files(), function(curr.file) {
  
  # Load the output file
  simul <- readRDS(curr.file)
  
  # Calculate the means
  means <- sapply(simul, mean)
  
  # Calculate standard deviation
  stdevs <- sapply(simul, function(x) sqrt(var(x)))
  
  # Calculate population sizes
  popsizes <- sapply(simul, length)
  
  # Get the vector of time points
  timepoints <- names(simul)
  
  # Name elements of means, sdevs and popsizes with the timepoints
  names(means) <- timepoints
  names(stdevs) <- timepoints
  names(popsizes) <- timepoints
  
  # Save all these
  saveRDS(means, gsub("biggi", "biggiMeans", curr.file))
  saveRDS(stdevs, gsub("biggi", "biggiSdevs", curr.file))
  saveRDS(popsizes, gsub("biggi", "biggiPopsizes", curr.file))
  
}) #end of for each simulation

