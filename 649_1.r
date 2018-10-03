trainset_class <- list()
prediction <- list()
priors <- rep(0,10)
averageset <- list()
summary <- matrix(rep(0,100),ncol=10,byrow=TRUE)
colnames(summary) <- seq(0,9)
rownames(summary) <- seq(0,9)

trainset = read.csv("C:\\Users\\Home\\Downloads\\649\\mnist_train.csv")
trainset[, 785] <- as.factor(trainset[,785])
#grey the data
trainset[,2:785][trainset[,2:785] <= 128] <- 0
trainset[,2:785][trainset[,2:785] > 128] <- 1


testset = read.csv("C:\\Users\\Home\\Downloads\\649\\mnist_test.csv")
testset[, 785] <- as.factor(testset[,785])
testset[,2:785][testset[,2:785] <= 128] <- 0
testset[,2:785][testset[,2:785] > 128] <- 1

for (i in seq(0,9)) {
  trainset_class[[i+1]] <- trainset[which(trainset[,1] == i),]
  priors[i+1] <- dim(trainset_class[[i+1]])[1]/60000
  averageset[[i+1]] <- apply(trainset_class[[i+1]][,1:784],2,mean)
}


for (k in 1:dim(testset)[1]) {
  
  posteriors <- rep(0,10)
  testset_item_num_zeros = 784 - sum(testset[k,2:784]+1)
  testset_item=testset[k,2:785]
  testset_item[testset_item==0] <- -1
  
  for (i in seq(0,9)) {
    #calculate P(x|y)
    probability <- matrix(unlist(testset[k,2:785]),nrow=1) %*% matrix(averageset[[i+1]],ncol=1) + testset_item_num_zeros
    posteriors[i+1] <- priors[i+1] + probability
  }
  predict = which.max(posteriors)
  summary[predict,as.integer(testset[k,1]+1)] = summary[predict,testset[k,1]+1]+1
  
}
summary <- as.table(summary)
print.table(summary)
cat("Accuracy: ",(sum(diag(summary)))/sum(summary))
