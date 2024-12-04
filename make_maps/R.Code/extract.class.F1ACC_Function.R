####### function to extract and calculate mean and sd for Precision and F1 for all classes from given .Rdata #######
# this function calculates and/or extracts the F1 and Precision (ACC) values 
# for every class in the .RData file


extract.accandF1<- function(input,# input should be a character vector with paths to the .Rdata files to evaluate
                       percent=F, # should the output be formatted in percent?
                       NrOfVars=28)# 2 (locations)*2(shape-scale)*4(SVM...)*2(F1-ACC)
{ 
# initialize data storage variables
  #vars <- c('accSVM','accSVM_M','accVSVM','accVSVM_SL')
  vars <- c('accSVM','accSVM_M','accSVM_SL_Un','accVSVM','accVSVM_SL','accVSVM_SL_Un_b','accVSVM_SL_vUn_b')
  storage <- replicate(NrOfVars,numeric())
  names(storage) <- NA
  for(x in 1:length(input)){
    # load data from .Rdata files one at a time
    load(input[x])
    
      
      for(i in 1:length(vars)){
        
        # create storage subname
        foldernameF1 <- paste0(sub("_.*.RData$","",sub(".*/","",input[x])),sub("acc","",vars[i]),"F1")
        foldernameACC <- paste0(sub("_.*.RData$","",sub(".*/","",input[x])),sub("acc","",vars[i]),"ACC")
        
        # apply name to storage list content
        names(storage)[which(is.na(names(storage)))[1]] <- foldernameF1
        names(storage)[which(is.na(names(storage)))[1]] <- foldernameACC
        
        # extract data and safe to storage
        if(grepl("Multi",input[x])){ # for multi class data
          storage[[foldernameF1]] <- cbind(storage[[foldernameF1]],round(get(vars[i])$byClass[,"F1"],7)) # get F1 for every class
          storage[[foldernameACC]] <- cbind(storage[[foldernameACC]],round(get(vars[i])$byClass[,"Precision"],7)) # get ACC for every class
        }
        
        if(grepl("Binary",input[x])){ # for binary class data
          
          # calculate ACC for class "other" (not given in the .RData output)
          other.prec <- (get(vars[i])$table[2,2]/(get(vars[i])$table[2,2]+get(vars[i])$table[2,1])) 
          # Calculate recall for class "other"
          other.recall <- (get(vars[i])$table[2,2]/(get(vars[i])$table[2,2]+get(vars[i])$table[1,2])) 
          # first the classified class, then the class "other"
          binacc <- rbind(round(get(vars[i])$byClass["Precision"],4),round(other.prec,4)) 
          # calculate F1 using recall and ACC
          binf1 <- rbind(round(get(vars[i])$byClass["F1"],4),round(2*other.prec*other.recall/(other.prec+other.recall),4)) 
          
          storage[[foldernameACC]] <- cbind(storage[[foldernameACC]],binacc) # saving ACC for both categories
          storage[[foldernameF1]] <- cbind(storage[[foldernameF1]],binf1) # saving F1 for both categories
          
        }
      }
  } 
  # turn the result into percentage if argument percent=TRUE
  if(percent){storage <- lapply(X=storage,FUN=function(x){x*100})}
  return(storage)
}

