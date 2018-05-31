#install.packages('sparsediscrim', dependencies = TRUE)
require(mosaic)
require(xtable)
require(caret)
require(MASS)
require(kernlab)
require(glmnet)
require(pls)
require(sparsediscrim)
require(randomForest)
require(pROC)
library(data.table)
require(parallelSVM)
require(foreach)
require(parallel)
options(xtable.comment = FALSE)
#load("spam.Rda")
require(doParallel)
require(ranger)




#Split your data into folds
cvfold <- 10
fold_size <-nrow(chosen_data)/cvfold
set.seed(690)


spam <- mutate(chosen_data, spam = as.factor(spam), index = sample(rep(1:cvfold, each = fold_size)))
#name your repsonse response! lets make this reproducible
#future work
#make an empty matrix to hold the prediction errors
#this is 7 X 10 for the 7 classification methods and the 10 CV folds
if (selection == F) {
  method_names <- c("KNN", "RandomForest", "LDA", "SVMlinear","SVMpolynomial", "GBM","PCR","LASSO")
} else {
  method_names <- c("KNN", "RandomForest", "LDA", "SVMlinear")
}

method_probs <- array(dim=c(length(method_names), cvfold, fold_size))
error <- auc <- false.pos <- matrix(nrow = length(method_names), ncol = cvfold)
rownames(error) <- rownames(auc) <- rownames(false.pos) <- method_names










for (i in 1:cvfold){
  #### create a training and test set per fold #########
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  ######################################################  
  
  
  ############# Variable Selection ############
  if (selection==T) {
    logistics <- lapply(subset(train, select = -spam), 
                        function(x) summary(glm(train$spam ~ x, family= "binomial"))$coefficients[2,4] )
    log.index <- as.vector(logistics < 0.05)
    train <- train[, log.index]
    test <- test[, log.index]
  }
  ############################################       
  
  ### Create class label data set required for some packages #####
  training.set <- train
  training.set.y <- training.set$spam
  training.set$spam <- NULL
  test.set.y<-test.spam
  y<- as.factor(ifelse(training.set.y==0,'no','yes'))
  training.set$spam <- y
  test.yesno <- as.factor(ifelse(test.spam==0,'no','yes'))
  ################################################################       
  
  
  ## Create a general formula for models #####################
  allVars <- colnames(train)
  predictorVars <- allVars[!allVars%in%"spam"]
  predictorVars <- paste(predictorVars, collapse ="+")
  form=as.formula(paste("spam~",predictorVars,collapse="+"))
  ##########################################################
  
  
  
  
  
  
  
  
  
  
  ####  K-Nearest Neighbors  #############################################################
  ########################################################################################
  mname <- "KNN"
  method_index <- which(method_names%in%mname)
  
  trControl <- trainControl(method  = "repeatedcv",
                            number  = 10,
                            classProbs = T,
                            summaryFunction = twoClassSummary)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  knn.fit <- caret::train(spam ~ .,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = seq(1,sqrt(nrow(training.set))/2,by=2)), # this contains odd k up to k = sqrt(n)/2 where n = nobservations in training
                   trControl  = trControl,
                   metric = 'ROC',
                   preProcess = c("center","scale"),# I assume this does not matter for categorical data
                   data       = training.set)
  stopCluster(cl)
  
  
  knn.pred <- predict(knn.fit, test)
  knn.probability <- predict(knn.fit, test, type="prob")
  method_probs[method_index,i,]<-knn.probability[[2]]
  error[method_index, i] <- mean(knn.pred != test.yesno)  
  auc1 <- roc(test.spam, knn.probability[[2]])
  auc[method_index, i] <- auc1$auc
  false.pos[method_index, i] <- table(knn.pred, test.spam)[2,1]/(table(knn.pred, test.spam)[2,1] + table(knn.pred, test.spam)[1,1])
  ##############################################################################################
  ##############################################################################################

  
  
  
  
  
  
  ###### Random Forests, Ranger  ###########################################################################################################
  #################################################################################################################################
  mnames<-"RandomForest"
  method_index<-which(method_names%in%mnames)
  
  
  control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid", classProbs = TRUE, summaryFunction = twoClassSummary)
  tgrid <- expand.grid(
    .mtry=c(1:sqrt(ncol(training.set))),
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )
  metric <- "ROC"
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  rf_random <- train(spam~., 
                     data=training.set,
                     method="ranger", 
                     metric=metric, 
                     tuneGrid = tgrid, 
                     trControl=control)
  stopCluster(cl)
  
  p<-predict(rf_random$finalModel, test, response="prob")
  auc[method_index, i] <- roc(test.spam, p$predictions[,2])$auc
  method_probs[method_index, i, ] <- p$predictions[,2]
  ##################################################################################################################################
  ##################################################################################################################################
  
  
  
  

  
  
  #### Linear Discriminant Analysis   ##############################################################################################
  ##################################################################################################################################
  mnames<-"LDA"
  method_index<-which(method_names%in%mnames)
  lda.fit <- lda(spam ~., data = train)
  lda.pred <- predict(lda.fit, test)$class
  lda.prob <- predict(lda.fit, test)$posterior[,2]
  method_probs[method_index,i,]<-lda.prob
  error[method_index, i] <- mean(lda.pred != test.spam)
  auc[method_index, i] <- roc(test.spam, lda.prob)$auc
  ##################################################################################################################################
  ##################################################################################################################################
  
  
  
  
  
  
  
  #### SVM linear  #################################################################################################################
  ##################################################################################################################################
  mnames<-"SVMlinear"
  method_index<-which(method_names%in%mnames)
  
  train2<-train
  train2$fold <- caret::createFolds(1:nrow(train), k = 4, list = FALSE)
  ### PARAMETER LIST ###
  cost <- c(10,30,100)
  epsilon <- c(.02,.1,.2)
  parms <- expand.grid(cost = cost, epsilon = epsilon)
  ### LOOP THROUGH PARAMETER VALUES ###
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  result <- foreach(k = 1:nrow(parms), .combine = rbind) %do% {
    c <- parms[k, ]$cost
    e <- parms[k, ]$epsilon
    ### K-FOLD VALIDATION ###
    out <- foreach(j = 1:max(train2$fold), .combine = rbind, .inorder = FALSE) %dopar% {
      deve <- train2[train2$fold != j, ]
      test <- train2[train2$fold == j, ]
      mdl <- e1071::svm(form, data = deve, type = "C-classification", kernel = "linear", cost = c, epsilon = e, probability = TRUE)
      pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
      data.frame(y = test$spam, prob = attributes(pred)$probabilities[, 2])
    }
    ### CALCULATE SVM PERFORMANCE ###
    roc <- pROC::roc(as.factor(out$y), out$prob) 
    data.frame(parms[k, ], roc = roc$auc[1])
  }
  stopCluster(cl)
  result
  
  c<-result[which.max(result$roc),]$cost
  e<-result[which.max(result$roc),]$epsilon
  mdl <- e1071::svm(form, data = train2, type = "C-classification", kernel = "linear", cost = c, epsilon = e, probability = TRUE)
  pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
  method_probs[method_index, i, ] <- attributes(pred)$probabilities[, 2]
  auc[method_index, i]<-pROC::roc(test.spam,attributes(pred)$probabilities[, 2])$auc 
  ##################################################################################################################################
  ##################################################################################################################################
  

  
  
  
  
    if (selection == F) {
  
        ######  SVMpolynomial   ##########################################################################################################
        ##################################################################################################################################
        mnames<-"SVMpolynomial"
        method_index<-which(method_names%in%mnames)
        
      
        ## set folds for tuning
        train2 <- train
        train2$fold <- caret::createFolds(1:nrow(train2), k = 4, list = FALSE)
        ### PARAMETER LIST ###
        cost <- c(10, 100)
        gamma <- c(1, 2)
        parms <- expand.grid(cost = cost, gamma = gamma)
        ### LOOP THROUGH PARAMETER VALUES ###
        cl <- makeCluster(detectCores())
        registerDoParallel(cl)
        result <- foreach(k = 1:nrow(parms), .combine = rbind) %do% {
          c <- parms[k, ]$cost
          g <- parms[k, ]$gamma
          ### K-FOLD VALIDATION ###
          out <- foreach(j = 1:max(train2$fold), .combine = rbind, .inorder = FALSE) %dopar% {
            deve <- train2[train2$fold != j, ]
            test <- train2[train2$fold == j, ]
            mdl <- e1071::svm(form, data = deve, type = "C-classification", kernel = "polynomial", cost = c, gamma = g, probability = TRUE)
            pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
            data.frame(y = test$spam, prob = attributes(pred)$probabilities[, 2])
          }
          ### CALCULATE SVM PERFORMANCE ###
          roc <- pROC::roc(as.factor(out$y), out$prob) 
          data.frame(parms[k, ], roc = roc$auc[1])
        }
        stopCluster(cl)
        result
        c<-result[which.max(result$roc),]$cost
        g<-result[which.max(result$roc),]$gamma
        mdl <- e1071::svm(form, data = train2, type = "C-classification", kernel = "polynomial", cost = c, gamma = g, probability = TRUE)
        pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
        method_probs[method_index, i, ] <- attributes(pred)$probabilities[, 2]
        auc[method_index, i]<-pROC::roc(test.spam,attributes(pred)$probabilities[, 2])$auc 
        ##################################################################################################################################
        ##################################################################################################################################
        
        
        
        #gbm
        mnames<-"GBM"
        method_index<-which(method_names%in%mnames)
        
        
        train.set<-train
        train.set$spam<-NULL
        registerDoSEQ() # clears cluster registry from foreach in previous model
        
        gbm.fit <- caret::train(train.set, #just a normal data frame
                         y, #has to be in yes or no format for some reason
                         method = "gbm",
                         metric = "ROC",
                         trControl = trainControl(method = "cv", number = 10, summaryFunction =  twoClassSummary, classProbs = TRUE),
                         preProcess = c("center", "scale")
        )
        
        
        
        gbm.prob<-predict(object = gbm.fit, test, type='prob')
        gbm.pred<-predict(object = gbm.fit, test, type='raw')
        gbm.pred<-ifelse(gbm.pred=='yes',1,0)
        
        method_probs[method_index,i,]<-gbm.prob[,2]
        auc[method_index, i] <- roc(test.spam, gbm.prob[[2]])$auc
      
        
        #####  Principle Components Regression  ######################################################
        ##############################################################################################
        mname="PCR"
        method_index <- which(method_names%in%mname)
        
        nofactor.spam <- mutate(spam, spam = ifelse(spam == 1, 1, 0))
        nofactor.train <- subset(filter(nofactor.spam, index != i), select = -index)
        nofactor.test.spam <- filter(nofactor.spam, index == i)$spam
        nofactor.test <- subset(filter(nofactor.spam, index == i), select = - c(index, spam))
        train.matrix <- model.matrix(spam ~. , data = nofactor.train)[,-1]
        train.spam <- nofactor.train$spam
        pcr.fit <- pcr(spam ~., data = nofactor.train, scale = TRUE, validation = "CV")
        aa1 <- pcr.fit[["validation"]][["PRESS"]]
        #ncomp.pcr <- which(aa1==min(aa1)) #tuning value
        pcr.prob <- predict(pcr.fit, nofactor.test,  ncomp = which(aa1==min(aa1)))
        method_probs[method_index, i, ] <- pcr.prob
        pcr.pred <- ifelse(pcr.prob > .5, 1, 0)
        error[method_index, i] <- mean(pcr.pred != test.spam)
        auc1 <- roc(test.spam, pcr.prob)
        auc[method_index, i] <- auc1$auc
        ##################################################################################################################################
        ##################################################################################################################################
        
        
        
        
        
        ###### LASSO ############################################################################
        #########################################################################################
        mname = 'LASSO'
        method_index <- which(method_names%in%mname)
        
        #data set up for penalized logistic regressions
        x <- as.matrix(subset(train, select = -spam)) #removes spam from the dataframe
        x.test <- as.matrix(test)
        #alpda is a LASSO penalty
        plog.l.fit <- glmnet(x, train$spam, family = "binomial", alpha=1, standardize = TRUE)
        plog.l.prob <- predict(plog.l.fit, x.test, type = "response")
        plog.l.pred <- ifelse(plog.l.prob > .5, 1, 0)
        l.error <- c()
        l.false <- c()
        l.auc <- c()
        for (lambda in 1:ncol(plog.l.pred)){ #length(plog.l.fit$lambda) might be more intuitive
          l.error[lambda] <-  mean(plog.l.pred[,lambda] != test.spam) # can we use auc instead of error for lambda?
          auc1 <- roc(test.spam, plog.l.prob[,lambda]) #lets use auc instead of error for choosing lambda
          l.auc[lambda] <- auc1$auc
          l <- data.frame(pred = plog.l.pred[,lambda], true = test.spam)
          l.false[lambda] <- nrow(filter(l, pred == 1, true == 0))/nrow(filter(l, true == 0))
        }
        error[method_index, i] <- min(l.error) #chooses error for best penalty
        false.pos[method_index, i] <- l.false[which.min(l.error)]
        #which.lasso[i] <- which.max(l.auc)#chooses lambda
        auc1 <- roc(test.spam,plog.l.prob[,which.max(l.auc)])
        auc[method_index, i] <- auc1$auc
        #lambda.l[i] <- plog.l.fit$lambda[which.max(l.auc)] #identifies best lambda from model fit
        method_probs[method_index,i,]<-plog.l.prob[,which.max(l.auc)]
        ##########################################################################
        ##########################################################################
      }
  

  
  print(paste("Fold number", i, "is now finished.", sep = " "))
}





##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################




auc_results<-rowMeans(auc)
avg_probs<-colMeans(method_probs)
auc_fold <-c()
for (i in 1:cvfold){
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  auc1 <- roc(test.spam, avg_probs[i,])
  auc_fold[i] <- auc1$auc
}
mean_method_auc <- mean(auc_fold)


#change the file name example: CrossValidationTemp2.Rda
save(auc_results, method_probs, mean_method_auc, method_names,
     file = paste0(output.dir,"results.rda"))

