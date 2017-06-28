nn3 <- function(trainob, testob, trainlab){
          predictlab <- rep(0, nrow(testob))       #initialize the predicted label of test object
          for(testrow in 1:nrow(testob)){         #go over all the test objects
            firstnearestDistance <- Inf           #initialize the distance of the first nearest neighbour
            firstnearestLabel <- 0                #initialize the label of the first nearest neighbour
            secondnearestDistance <- Inf          #initialize the distance of the second nearest neighbour
            secondnearestLabel <- 0               #initialize the label of the second nearest neighbour
            thirdnearestDistance <- Inf           #initialize the distance of the third nearest neighbour
            thirdnearestLabel <- 0                #initialize the label of the third nearest neighbour
            for(trainrow in 1:nrow(trainob)){     #go over all the training objects
              distance <- sum((testob[testrow, ] - trainob[trainrow, ])^2)    #calculate the Euclidean distance between test objects and training objects 
              if(distance < thirdnearestDistance){                    #check if the distance calculated is smaller than the third nearest distance existing
                thirdnearestDistance <- distance                      #the distance just calculated becomes the third nearest distance
                thirdnearestLabel <- trainlab[trainrow]                #the corresponding training label becomes the label of the third nearest neighbour
                if(distance < secondnearestDistance){                 #check if the distance calculated is smaller than the second nearest distance existing
                  thirdnearestDistance <- secondnearestDistance       #the second nearest distance becomes the third nearest distance
                  thirdnearestLabel <- secondnearestLabel             #the label of second nearest neighbour becomes the label of third nearest neighbour
                  secondnearestDistance <- distance                   #the distance just calculated becomes the second nearest distance
                  secondnearestLabel <- trainlab[trainrow]             #the corresponding training label becomes the label of the second nearest neighbour
                  if(distance < firstnearestDistance){                #check if the distance calculated is smaller than the first nearest distance existing
                    secondnearestDistance <- firstnearestDistance     #the first nearest distance becomes the second nearest distance
                    secondnearestLabel <- firstnearestLabel           #the label of first nearest neighbour becomes the label of second nearest neighbour
                    firstnearestDistance <- distance                  #the distance just calculated becomes the first nearest distance
                    firstnearestLabel <- trainlab[trainrow]            #the corresponding training label becomes the label of the first nearest neighbour
                  }
                }
              }
              if(secondnearestLabel == thirdnearestLabel){    #check if the label of second nearest neighbour equals to the label of third nearest neighbour
                predictlab[testrow] <- secondnearestLabel      #predict the label of the test object is the label of the second nearest neighbour(because of the majority vote)
              } else{                                         #if the condition is false
                predictlab[testrow] <- firstnearestLabel       #predict the label of the test object is the label of the first nearest neighbour
              }
            }
          }
          return(predictlab)                                  #return the labels of prediction
        }