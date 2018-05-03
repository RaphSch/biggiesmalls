# BIGGIESMALLS
# This script models evolution of body size on an island
# Using an individual birth-death process
# Raphaël Scherrer
# 19/4/2018
#-------------------------------------------------------

# Empty the environment
rm(list = ls())


#### PARAMETERS ####

arguments = as.numeric(commandArgs(TRUE))

# Island
islandSizes = c(100, 200, 400, 800, 1600, 3200, 6400)
islandSize = islandSizes[arguments[1]]

# Founder population
bodySizes0 = c(1,10,100,1000,10000,100000,1000000,10000000) # initial body size mean
bodySize0 = bodySizes0[arguments[2]]
sdBodySize0 = 0.05 * bodySize0 # initial standard deviation
popSize0 = 10 # initial population size

# parameter of the competition equation
competitiveAdvantages = c(0,0.001,0.01,0.1,1,5)
competitiveAdvantage = competitiveAdvantages[arguments[3]]

# parameters of the life history trait equations:
basalFecundity = 0.025
fecundityScaling = -0.26 
basalLongevity = 630
longevityScaling = 0.17

# parameters of the carrying capacity equation:
resource0 = islandSize
resourceScalings = c(0,-0.001,-0.01,-0.1,-0.5,-1,-2)
resourceScaling = resourceScalings[arguments[4]]

# mutation rate and variance
mutationRate = 0.01
mutationalSd = 0.05

# simulation time parameters
maxTime = 50000
timeSpan = 0.01 * maxTime # time span between two recording intervals
recordFreq = 10 # frequency of events at which to record the population (once every # events)

# Vector of important parameters
parameters = c(islandSize, competitiveAdvantage, bodySize0, sdBodySize0, popSize0, maxTime, mutationalSd, mutationRate)


#### ACCESSORY FUNCTION ####


# Function to calculate the competition experienced by an individual
calc_competition = function(focalSize, bodySizes) {
  
  # Find the focal individual in the population
  focalId = which(bodySizes == focalSize)[1]
  
  # Calculate the competitive interactions of the focal individual with all other individuals in the population
  competitions = sapply(bodySizes[-focalId], function(competitorSize, focalSize) {
    
    # Calculate body size difference
    bodySizeDifference = (focalSize - competitorSize) / focalSize
    
    # Calculate competition kernel
    competition = 2 * (1 - 1 / (1 + exp(-competitiveAdvantage * bodySizeDifference)))
    
    return(competition)
    
  }, focalSize)
  
  # Calculate the sum of competitive interactions
  competition = sum(competitions)
  
  return(competition)
  
} #end of the function calc_competition


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
  if(resource0 <= 0) stop("resource0 must be > 0")
  if(mutationRate < 0) stop("mutationRate must be >= 0")
  if(mutationalSd <= 0) stop("mutationalSd must be > 0")
  
  
  
  # Set an initial population of individual body sizes
  bodySizes = rnorm(popSize0, bodySize0, sdBodySize0)
  
  # Initialize population size
  popSize = popSize0
  
  # Initialize storage list
  storageList = list()
  
  # Record initial population
  storageList[[1]] = bodySizes
  
  # Record initial time point
  names(storageList)[1] = 0
  
  # Initilize storage index
  i = 2
  
  #### SIMULATION ####
  
  # Initialize time
  t = 0
  
  # Initialize event count
  count = 0
  
  # While time has not exceeded simulation time:
  while(t <= maxTime) {
    
    # Print elapsed time
    print(t)
    
    # Calculate fertility of each individual
    fecundities = basalFecundity * bodySizes ^ fecundityScaling
    
    # Calculate longevity of each individual
    longevities = basalLongevity * bodySizes ^ longevityScaling
    
    # Calculate death-by-senescence rate of each individual
    mortalities = 1 / longevities
    
    # Calculate the net growth rate of each individual (without competition)
    netGrowthRate = fecundities - mortalities
    
    # Calculate the impact of competition on each individual
    competitors = sapply(bodySizes, calc_competition, bodySizes)
    
    # Calculate the carrying capacity of each individual
    resources = resource0 * bodySizes ^ resourceScaling
    
    # Calculate the birth rate of each individual
    birthRates = netGrowthRate
    
    # Calculate the death rate of each individual
    deathRates = netGrowthRate * (1 + competitors) / resources
    
    # Pool all rates into one vector
    allRates = c(birthRates, deathRates)
    
    # Set negative rates to zero
    allRates[allRates < 0] = 0
    
    # Calculate the total rate of events
    totalRate = sum(allRates)
    
    # Sample the next event from an exponential distribution
    dt = rexp(n = 1, rate = totalRate)
    
    # Calculate the probability of each event
    probabilities = allRates / totalRate
    
    # Sample the event that happens
    id = sample.int(n = 2*popSize, size = 1, prob = probabilities)
    
    # If a birth happens...
    if(id <= popSize) {
      
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
      
      print("birth")
      
    } else {
      
      # Or if death happens, kill the sampled individual
      bodySizes = bodySizes[-(id - popSize)]
      
      print("death")
      
    } # end of birth or death
    

    # Update population size
    popSize = length(bodySizes)
    
    # If the new population size is zero, pop has gone extinct, stop the simulation
    if(popSize <= 1) {
      
      print(paste("Population has gone extinct at t =", t))
      
      return(NA)
      
    }
    
    # Update time
    t = t + dt
    
    # Update event count
    count = count + 1
    
    # Is it time to record the population?
    if(count %% recordFreq == 0) {
      
      # If yes, record the population
      storageList[[i]] = bodySizes
      
      # Record the time
      names(storageList)[i] = t
      
      # Update storage index
      i = i + 1
      
    }

  } #end of while loop through time
  
  
  
  #### OUTPUT ####
  
  # Create output file name
  outputname = paste0("biggi_", paste(parameters, collapse = '_'))
  
  # Guess replicate number by looking if file is already in the directory
  replicate = length(grep(outputname, list.files())) + 1
  
  # Update output file name accordingly
  outputname = paste0(outputname, "_", replicate, ".rds")
  
  # Save that in a file
  saveRDS(storageList, outputname)
  
  # Return the list of population compositions at each time point
  return(storageList)
  
  
} # end of function to run the program




#### RUN THE SIMULATION ####

output = biggiesmalls()

#### PLOT THE RESULTS ####

# time = as.numeric(names(output))
# 
# par(mfrow = c(2,2))
# 
# plot(time, sapply(output, length), main = "Population size", xlab = "Time", ylab = "")
# plot(time, sapply(output, mean), main = "Mean body size", xlab = "Time", ylab = "")
# plot(time, sapply(output, function(x) sqrt(var(x))), main = "Standard deviation in body size", xlab = "Time", ylab = "")




