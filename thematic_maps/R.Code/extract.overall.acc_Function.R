####### function to extract accuracy values from given .Rdata data #######
# input should be a character vector with paths to the .Rdata files to evaluate
# for this function to work, the data should be saved in a structer like this:
# e.g.: "./results_9/HagShapeMulti_accuracyMatrix_image_10samples.RData"
# the containing folder should be named results_XXX where XXX can be any number
library(stringr)

extract.acc<- function(input,# input and information on amount of cells in each class
                       col_cells=c(109620,54342,12574,24233,196928,283741),# # of cells according to reference for calculation of mean weighted F1
                       hag_cells=c(528497,495717,6740,5572,527491),
                       col_bin_cells=c(109620,571818),
                       hag_bin_cells=c(527491,1036526),
                       percent=F) # should the output be formatted in percent?
  { 
  # initialize output dataframe
  result <- data.frame("Run"=character(),
                       "Dataname"=character(),
                       "Kappa"=numeric(),
                       "F1"=numeric(),
                       "OA"=numeric(),
                       "AA"=numeric(), stringsAsFactors = F)
  row <- 1 #create row entry line
  for(x in 1:length(input)){
    # load the input file every step
    load(input[x])
    # the names of the variables as they are in the .RData files
    vars <- c('accSVM','accSVM_M','accSVM_SL_Un','accVSVM','accVSVM_SL','accVSVM_SL_Un_b','accVSVM_SL_vUn_b')
    
      
      for(i in 1:length(vars)){
        # for class "multi" data
        if(grepl("Multi",input[x])){
        result[row,1:2] <- c(gsub("results_","",str_extract(input[x],"results_[0-9]*")),#extract number of run
                             paste0(gsub("_.*$","",gsub(".*/","",input[x])),"_",gsub("acc","",vars[i])))# extract information on location and kind of algorithm used
        result[row,3:6] <- c(round(get(vars[i])$overall["Kappa"],digits = 4), # get kappa values
                       round(sum(get(vars[i])$byClass[,"F1"]*(if(grepl("Col",input[x])) # ifelse() and if(){}else{} do not act the same. ifelse only evaluates arguments based on the logical input vector. if the logical input only has 1 field, only the first field of the output arguments is evaluated.
                                                                                        {col_cells/sum(col_cells)
                                                                                  }else{
                                                                                        hag_cells/sum(hag_cells)})),4),#  get F1 values
                       round(get(vars[i])$overall["Accuracy"],digits = 4), # get overall accuracy values
                       round(sum(get(vars[i])$byClass[,"Precision"]/length(get(vars[i])$byClass[,"Precision"])),4)) # get average accuracy values
    row <- row+1
      }else{
      # for class "binary" data
      
        #extract number of run and information on location and kind of algorithm used
        result[row,1:2] <- c(gsub("results_","",str_extract(input[x],"results_[0-9]*")),paste0(gsub("_.*$","",gsub(".*/","",input[x])),"_",gsub("acc","",vars[i])))
                            # calculate intermediate results
                            other.prec <- get(vars[i])$table[2,2]/(get(vars[i])$table[2,2]+get(vars[i])$table[2,1])
                            other.recall <- get(vars[i])$table[2,2]/(get(vars[i])$table[2,2]+get(vars[i])$table[1,2])
                            other.f1 <- 2*other.prec*other.recall/(other.prec+other.recall)
                            
        result[row,3:6] <- c(round(get(vars[i])$overall["Kappa"],digits = 4), # get kappa values
                                  round(sum(c(get(vars[i])$byClass["F1"],other.f1)*if(grepl("Col",input[x])) # ifelse() and if(){}else{} do not act the same. ifelse only evaluates arguments based on the logical input vector. if the logical input only has 1 field, only the first field of the output arguments is evaluated.
                                                                            {col_bin_cells/sum(col_bin_cells)
                                                                            }else{
                                                                            hag_bin_cells/sum(hag_bin_cells)}),4), # get F1 values
                                 round(get(vars[i])$overall["Accuracy"],4), # get Overall Accuracy
                                 round(sum(get(vars[i])$byClass["Precision"],other.prec)/2,4)) # get average accuracy (unweighted)
        row <- row+1
      }
    }
    
  }
  # turn the result into percentage if argument percent=TRUE
  if(percent){result[,3:6] <- result[3:6]*100}
  return(result)
}

