#Normalizing the data
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Distance calculation
euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    
    d = d + (a[[i]]-b[[i]])^2
    
  }
  d = sqrt(d)
  return(d)
}

#kNN classifier function
knn_predict <- function(test_data, train_data, training_target_data,k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    cnt_one = 0              
    cnt_zero = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(training_target_data[j]))
      
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
eu$eu_char
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"eu_char"]) == "1"){
        cnt_one = cnt_one + 1
      }
      else
        cnt_zero = cnt_zero + 1
    }
    
    # Compares the no. of neighbors with class label
    if(cnt_one > cnt_zero){          
      
      pred <- c(pred, "1")
    }
    else if(cnt_one < cnt_zero){
      
      pred <- c(pred, "0")
    }
    
  }
  return(pred) #return pred vector
}

