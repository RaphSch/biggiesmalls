# Draft script for plotting

# Let's say you have run the script prepare_data_for_plotting
# You should then have a list in which each element corresponds to a parameter combination
# And contains a vector of as many body sizes as you specified time points in prepare_data_for_plotting
# Let's call this object bodySizeList

# First, what you want to plot is the outcome of evolution
# So you need to take only the end body size of the simulation
# E.g. take the average of the last 100 generations
# To do so, use the lapply function to your bodySizeList (sapply applies a function to each element in a list or a vector and returns a vector if possible)
sapply(bodySizeList, function(vec) mean(vec[(length(vec) - 100):length(vec)]))

# Of course you need to store that output in an object

# You now have, for each parameter combination, one value of final average body size
# So you can merge all that into a table that will be used for plotting
# tables are more handy than lists for that

# To that aim you need to create (again) a table with all parameter combinations
# Use the expand.grid function, as in prepare_data_for_plotting, to do that

# Let's say the table you just created is called allCombi
# You can append the vector of body sizes to that table
# Use cbind to do that, and store the resulting table into an object, which will be the plotting data frame

# What we want on the y axis is the ratio between final and initial body size
# Final body size is the 11th column (I beieve) of the table
# Initial body size is the 4th one
# So replace the 11th column by itself divided by the 4th
myTable[,11] = myTable[,11] / myTable[,4]

# and you have your ratio!


# Now, set the plotting windows
# If we have 3 values of resource scaling and 3 values of competititve advantage,
# then we want a 3 by 3 figure
par(mfrow = c(3,3))

# We will now loop through each figure
# We use a double loop: through the values of resource scaling first, then through the values of competitive advantage


for(i in 1:3) {
  for(j in 1:3) {
    
    # Set the current parameter values
    curr.alpha = unique(myTable[,column_competitiveAdvantage])[i]
    curr.kappa = unique(myTable[,column_resourceScaling])[j]
    
    # Subset the data with only these parameters
    curr.Table = myTable[myTable[,column_competitiveAdvantage] == curr.alpha,]
    curr.Table = curr.Table[curr.Table[,column_resourceScaling] == curr.kappa,]
    
    # Create an empty plot on which we will add the lines
    plot(curr.Table[,column_islandSize], curr.Table[,column_sizeRatio], type = "n", xlab = "Island size", ylab = "Body size ration (island/mainland)")
    
    # Draw a line of body size ratio versus island size, for each intial body size
    for(k in 1:2) {
      
      curr.initialBodySize = unique(currTable[,column_initialBodySize])[k]
      curr.subTable = currTable[currTable[,column_initialBodySize] == curr.initialBodySize,]
      lines(curr.subTable[,column_islandSize], curr.SubTable[,column_sizeRatio], col = k)
      
    } #end of loop through initial body sizes
  } #end of loop through resource scalings
} #end of loop through competitive advantages

