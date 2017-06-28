USPSsubset <- read.table("USPSsubset.txt")
for(row in 1:nrow(USPSsubset[1:256,])){
  maximum <- max(USPSsubset[row, 1:256])
  minimum <- min(USPSsubset[row, 1:256])
  USPSsubset[row,] <- (USPSsubset[row,] - minimum) / (maximum - minimum)
}

train.X <- USPSsubset[1:365, 1:256]
test.X <- USPSsubset[366:465, 1:256]
train.Y <- USPSsubset[1:365, 257]
test.Y <- USPSsubset[366:465, 257]

source("NN3 using tangent distance.R")
#source("NN3 using euclidean distance.R")
predict.Y <- nn3(train.X, test.X, train.Y)  #implement my k = 3 nearest neighbour function using USPS dataset

right = 0                               #initialize the number of correct prediction
wrong = 0                               #initialize the number of wrong prediction
for(i in 1:length(predict.Y)){          #go over all elements
  if(predict.Y[i] == test.Y[i]){        #check if each labels between prediction and test match
    right = right + 1                   #the number of correct prediction plus one for the true condition
  } else{
    wrong = wrong + 1                   #the number of wrong prediction plus one for the false condition
  }
}

comparetable <- data.frame(predict.Y, test.Y, correctness = predict.Y == test.Y)  #generate a table to show the results

correctpercent <- right/length(predict.Y)    #calculate the percentage of correct prediction
print(correctpercent)                        #print out the result
