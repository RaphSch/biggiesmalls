# BIGGIESMALLS
# This script models evolution of body size on an island
# Using an individual birth-death process
# Raphaël Scherrer
# 19/4/2018
#-------------------------------------------------------

# Empty the environment
rm(list = ls())


#### PARAMETERS ####

# Island
islandSize = 100

# Founder population
bodySize0 = 100 # initial body size mean
sdBodySize0 = 5 # initial standard deviation
popSize0 = 100 # initial population size

# parameter of the competition equation
competitiveAdvantage = 1

# parameters of the life history trait equations:
basalFecundity = 0.025
fecundityScaling = -0.26 
basalLongevity = 630 
longevityScaling = 0.17

# parameters of the carrying capacity equation:
basalCarryingCapacity = islandSize
carryingCapacityScaling = -0.01

# mutation rate and variance
mutationRate = 0.01
mutationalSd = 0.05

# simulation time parameters
maxTime = 1000
timeSpan = 0.01 * maxTime # time span between two recording intervals


#### FUNCTIONS ####

# Function to calculate the birth rate of an individual given its potential competitors
calc_birthRate = function(focalSize, bodySizes) {
  
  # Calculate the competition kernel of the current individual with each competitor in the population
  competitionKernels = sapply(bodySizes, calc_competitionKernel, focalSize)
  
  # Sum the competition kernels
  competition = sum(competitionKernels)
  
  # Calculate the fecundity of the individual
  fecundity = basalFecundity * focalSize ^ fecundityScaling
  
  # Calculate the carrying capacity of the individual
  carryingCapacity = basalCarryingCapacity * focalSize ^ carryingCapacityScaling
  
  # Calculate the birth rate
  birthRate = fecundity * (1 - competition / carryingCapacity)
  
  return(birthRate)
  
} #end of calc_birthRate function



# Function to calculate the competition kernel between two individuals
calc_competitionKernel = function(competitorSize, focalSize) {
  
  # Calculate body size difference
  bodySizeDifference = focalSize - competitorSize
  
  # Calculate competitive impact
  competition = 1 - 2 / (1 + exp(-competitiveAdvantage * bodySizeDifference))
  
  return(competition)
  
} #end of calc_competitionKernel function



# Function to calculate the death rate of an individual
calc_deathRate = function(focalSize) {
  
  # Calculate longevity
  longevity = basalLongevity * focalSize ^ longevityScaling
  
  # Calculate death rate
  deathRate = 1 / longevity
  
  return(deathRate)
  
}



#### MAIN FUNCTION ####

# Function to launch the simulation
biggiesmalls = function() {
  
  #### INITIALIZATION ####
  
  # Assert that all parameters are fine
  if(islandSize <= 0) stop("islandSize must be > 0")
  if(maxTime <= 0) stop("maxTime must be > 0")
  if(bodySize0 <= 0) stop("bodySize0 must be > 0")
  if(sdBodySize0 <= 0) stop("sdBodySize0 must be > 0")
  if(popSize0 <= 0) stop("popSize0 must be > 0")
  if(basalCarryingCapacity <= 0) stop("basalCarryingCapacity must be > 0")
  if(mutationRate < 0) stop("mutationRate must be >= 0")
  if(mutationalSd <= 0) stop("mutationalSd must be > 0")
  
  
  
  # Set an initial population of individual body sizes
  bodySizes = rnorm(popSize0, bodySize0, sdBodySize0)
  
  # Initialize population size
  popSize = popSize0
  
  # Set the time points
  timepoints = seq(0, maxTime, timeSpan)
  
  # Initialize storage list
  storageList = vector(mode = "list", length = length(timepoints))
  
  # Record initial population
  storageList[[1]] = bodySizes
  
  #### SIMULATION ####
  
  # Initialize time
  t = 0
  
  # Initialize timepoint index
  i = 2
  
  # Initialize next time point
  nextTimepoint = timepoints[i]
  
  # While time has not exceeded simulation time:
  while(t <= maxTime) {
    
    # Print elapsed time
    print(t)
    
    # Print population size
    print(length(bodySizes))
    
    # Calculate the rate of birth of each individual
    birthRates = sapply(bodySizes, calc_birthRate, bodySizes)
    
    # Make sure there are no negative birth rates
    birthRates[birthRates < 0] = 0
    
    # Calculate the rate of death of each individual
    deathRates = sapply(bodySizes, calc_deathRate)
    
    # Make sure there are no negative death rates
    deathRates[deathRates < 0] = 0
    
    # Calculate the total rate of events
    totalRate = sum(c(birthRates, deathRates))
    
    # Sample the next event from an exponential distribution
    dt = rexp(n = 1, rate = 1 / totalRate)
    
    # What event is happening? Equal probabilities of birth and death (supposes that pop size is not too different from carrying capacity)
    birthOrDeath = as.logical(rbinom(n = 1, size = 1, prob = 0.5))
    
    # Simulate a birth or death event
    if(birthOrDeath) {
      
      # If a birth happens
      
      # Calculate the probabilities of birth of each individual
      probabilities = c(birthRates / totalRate)
      
      # Sample who is giving birth
      id = sample.int(n = popSize, size = 1, prob = probabilities)
      
      # What is the body size of the newborn?
      newBodySize = bodySizes[id]
      # Sample mutation event
      isMutation = as.logical(rbinom(n = 1, size = 1, prob = mutationRate))
      
      # If a mutation happens...
      if(isMutation) {
        
        # Sample the relative effect of this mutation
        mutationalEffect = rnorm(n = 1, mean = 0, sd = mutationalSd)
        
        # Apply this mutation to the newborn
        newBodySize = newBodySize + mutationalEffect * newBodySize
        
      } #end of if mutation happens
      
      # Append the newborn to the population
      bodySizes[popSize + 1] = newBodySize
      
    } else {
      
      # Otherwise death occurs
      
      # Calculate the probabilities of death of each individual
      probabilities = c(deathRates / totalRate)
      
      # Sample who is dying
      id = sample.int(n = popSize, size = 1, prob = probabilities)
      
      # Kill that individual
      bodySizes = bodySizes[-id]
      
    } #end of birth or death event
    

    
    # Update population size
    popSize = length(bodySizes)
    
    # If the new population size is zero, pop has gone extinct
    # Return an NA
    if(popSize == 0) {
      
      print(paste("Population has gone extinct at t =", t))
      
      return(NA)
      
    }
    
    # Have we just reached a time point?
    if(t >= nextTimepoint) {
      
      # If yes, record the state of the population
      storageList[[i]] = bodySizes
      
      # Update the next time point
      nextTimepoint = timepoints[i + 1]
      
      # Update timepoint index
      i = i + 1
      
    }
    
    # Update time
    t = t + dt
    
  } #end of while loop through time
  
  
  
  #### OUTPUT ####
  
  # Save that in a file
  saveRDS(storageList, "simulation.rds")
  
  # Return the list of population compositions at each time point
  return(storageList)
  
  
} # end of function to run the program




#### RUN THE SIMULATION ####

output = biggiesmalls()
