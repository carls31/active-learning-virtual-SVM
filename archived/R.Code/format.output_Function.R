#############################################################################################################
############################### format data function ########################################################
# This function is only for convenience to format the output in a way we want to 
# use it in the table later on: Mean ± (SD)
# input is two data frames x=mean and y=sd from the calculation of step 2

format.output <- function(x,y){
  if(length(x)==length(y)){
    # check if first column is factor or character (most likely rownames) and remove if necessary
    if(is.character(x[,1])|is.factor(x[,1])){storage <- x[,-1];rownames(storage) <- x[,1]
                            x <- x[,-1];y <- y[,-1]}else{storage <- x}
    
    # change the content to the desired output as a string
    for (i in 1:length(x)){
    
    storage[,i] <- paste0(x[,i]," (\u00b1 ",y[,i],")") # this gives the ± symbol in R on windows:"\u00b1"
    }
  
  }else{stop("Inputs must have the same length.")}
  return(storage)
}