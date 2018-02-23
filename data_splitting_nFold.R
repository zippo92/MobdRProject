#IL KFOLD consiste nel dividere le istanze in K split, e iterativamente una viene utilizzata per il validation/test
# e le altre per il training

split <- function(my_data, K){
  nFoldSplit <- c(); 
  
  #Per avere lo stesso bilanciamento di istanze positive e negative, prendo la stessa percentuale in ogni split
  output <- by(my_data, as.factor(cleaned_train$shares),function(x){
  smp_size <- floor(nrow(x)/K);
  print(smp_size)
  splitted_data <- list(); 
  #creo K segmenti differenti, della stessa dimensione (smp_size)
  for(i in 1:K){
      split_ind <- sample(seq_len(nrow(x)), size = smp_size)
      split <- x[split_ind, ]
      x<-x[-split_ind,];  
      splitted_data[[length(splitted_data)+1]] <- split
  }
  return(splitted_data)
  })
  
  
  splitted_data<-list();
  
  #Per ogni split, innanzitutto unisco le istanze positive dalle negative, poi scelgo un segmento di test e gli altri
  #saranno di training. 
  for(i in 1:K){
    test_set <- rbind(output[[1]][[i]], output[[2]][[i]]); 
    training_set <- data.frame();
    for(j in 1:K)
    {
      if(i != j)
      {
        training_set <- rbind(training_set,output[[1]][[j]], output[[2]][[j]]);
      }
    }
  
    #shuffle row-wise
    training_set <- training_set[sample(nrow(training_set)),]
    test_set <- test_set[sample(nrow(test_set)),]
    
    
    traintest <- list(training_set,test_set)
    names(traintest) <- c("train", "test"); 
    
    splitted_data[[length(splitted_data)+1]] <- traintest
    names(splitted_data)[i] <-paste("split_",i, sep="");
      
  }
return(splitted_data)

}

