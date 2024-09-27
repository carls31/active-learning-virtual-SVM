############## Data preparation function for Statistics table #######################
# This function aggregates the input data based on the column "Dataname"



prepare.oa.stats <- function(x,fun){ # fun argument is passed on to the aggregate() function
  # aggregate date according to the "Dataname" column of the input dataframe
  storage <- aggregate(x[,3:6],list(x$Dataname),fun)
  # transpose and add column names for final formating
  result <- as.data.frame(round(t(storage[,2:5]),2))
  colnames(result) <- storage[,1]
  
  return(result)
}