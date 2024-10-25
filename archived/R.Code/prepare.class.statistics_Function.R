########### Function to prepare Accuracy values for output as .csv ############




prepare.class <- function(input,decider=c("Hag"),round=F){
  
  # create variable names
  var.names <- c("MEAN","SD")
  storage <- replicate(7,data.frame())
  counter <- 1
  
  # calculate mean and sd for all matrices
  MEAN <- lapply(input,FUN=apply,MARGIN=1,mean)
  SD <- lapply(input,FUN=apply,MARGIN=1,sd)
  
  # format data separated by location and method for output
  for (x in 1:length(decider)){
    for (y in 1:length(var.names)){
      
    # substitute data according to decider and var.names  
      # gets list contents from the MEAN or SD calculation 
      # according to the decider (in this case the location)
      work <- get(var.names[y])[grep(decider[x],names(get(var.names[y])))] 
        
        # create dataframe output
        output <- data.frame(matrix(NA,nrow=length(work[[1]]),ncol=length(work)))
        for(i in 1:length(work)){
          output[,i] <- work[[i]]
        }
        # set names
        colnames(output) <- names(work)
        rownames(output) <- names(work[[1]])
        
        # save output dataframes to storage
        storage[[counter]] <- output
        names(storage)[counter] <- paste0(decider[x],var.names[y])
        counter <- counter+1
    }}
  # round to 2 digits
  if(round){storage <- lapply(X=storage,FUN=round,digits=2)}
  return(storage)
}