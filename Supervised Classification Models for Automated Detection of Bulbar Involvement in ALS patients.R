##############################################################################################################################################
# Title: Supervised Classification Models for Automated Detection of Bulbar Involvement in ALS patients
# Author details: Alberto Tena del Pozo
# Contact details: atena@cimne.upc.edu
##############################################################################################################################################



##############################################################################################################################################

#Install and load the required libraries

packages <- c("randomForest", "factoextra", "dplyr", "ROCR", "gplots", "ggplot2", "lattice", "MASS", "caret", "pROC", "devtools", "ggbiplot", "glmnet", "lme4", "broom", "e1071")
ipak(packages)

if("ggbiplot" %in% rownames(installed.packages()) == FALSE) {
  
  install_github("vqv/ggbiplot")
  library("ggbiplot")
  
}
##############################################################################################################################################



##############################################################################################################################################
#Options

#Select a model for analysis <- "SVM", "NN", "LDA", "LR", "NB" or "RF"
analysis <- "RF"

#Select the number of PC for analysis and PC1/PC2 plot (TRUE/FALSE)
n_pca <- 12
plot_pca <- TRUE

#bulbar_involvement column index
index <- n_pca+1

##Select the group for analysis: controls-bulbar (1), controls-NObulbar (2), NObulbar - bulbar (3)
#controls-NObulbar-bulbar (4) - Only to be printed in PCA biplot

groups <- 3

##Select gender: males ("M") females ("F") or both ("B")
sex <- "B"

#Confidence Interval Threshold
threshold <- .95

#Select the split percentage between train and test (0.6 for training by default)
split <- .70

#number of iteractions for model simulation
it <- 1

#Print confusion matrix (TRUE/FALSE)
confusion_matrix <- TRUE

##############################################################################################################################################



#############################################################################################################################################

##Handling missing values

d3 <- df3

d3$shimmer_apq11[26] <- 0.1940
d3$shimmer_apq11[69] <- 0.0780
d3$shimmer_apq11[70] <- 0.0780
d3$shimmer_apq11[70] <- 0.0780
d3$shimmer_apq11[91] <- 0.1358
d3$shimmer_apq11[92] <- 0.1358
d3$shimmer_apq11[245] <- 0.1144

d3$alsfrs_r[is.na(d3$alsfrs_r)] <- 48
d3$bulbar_involvement <- as.factor(d3$bulbar_involvement) 

#############################################################################################################################################



#############################################################################################################################################

#Filtering dataset for analysis

if (groups == 1 & sex != "B") {
  
  df <- d3 %>%
    filter(bulbar_involvement != "NO", gender == sex)
  
  df$bulbar_involvement <- droplevels(df$bulbar_involvement, exclude = "NO")
  
  df$bulbar_involvement <- relevel(df$bulbar_involvement, c("YES"))
  
  
  } else if (groups == 1 & sex == "B") {
    df <- d3 %>%
      filter(bulbar_involvement != "NO")
    df$bulbar_involvement <- droplevels(df$bulbar_involvement, exclude = "NO")
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("YES"))
    
  
  } else if (groups == 2 & sex != "B") {
    
    df <- d3 %>%
      filter(bulbar_involvement != "YES", gender == sex)
    df$bulbar_involvement <- droplevels(df$bulbar_involvement, exclude = "YES")
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("NO"))
    
  } else if (groups == 2 & sex == "B") {
    
    df <- d3 %>%
      filter(bulbar_involvement != "YES")
    df$bulbar_involvement <- droplevels(df$bulbar_involvement, exclude = "YES")
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("NO"))
    
  } else if (groups == 3 & sex != "B") {
    
    df <- d3 %>%
      filter(bulbar_involvement != "control", gender == sex)
    df$bulbar_involvement <- droplevels(df$bulbar_involvement, exclude = "control")
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("YES"))
    
  } else if (groups == 3 & sex == "B") {
    
    df <- d3 %>%
      filter(bulbar_involvement != "control")
    df$bulbar_involvement <- droplevels(df$bulbar_involvement, exclude = "control")
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("YES"))
    
  } else if (groups == 4 & sex != "B") {
    
    df <- d3 %>%
      filter(gender == sex)
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("YES"))
  
  } else if (groups == 4 & sex == "B") {
    
    df <- d3
    df$bulbar_involvement <- relevel(df$bulbar_involvement, c("YES"))

  } else {
    
    print("Insert a number between 1 and 4 for selecting groups")
    
}
##############################################################################################################################################



### Models implementation

for (i in 1:it) {
  
  #Compute the Principal Components
  pca.df <- prcomp(df[,c(4:23)], center = TRUE, scale. = TRUE)

  
  df.data <- pca.df$x[ , 1:n_pca]
  df.a <- cbind(df.data, as.numeric(df$bulbar_involvement))
  colnames(df.a)[n_pca+1] <- "bulbar_involvement"
  df.a[n_pca+1] <- as.factor(df.a[n_pca+1])
  
  #Train & Test dataset
  
  # Set seed
  set.seed(42)
  
  # Shuffle row indices: rows
  rows <- sample(nrow(df.a))
  
  # Randomly order data
  shuffled_df.a <- df.a[rows,]
  
  # Determine row to split on: ind
  ind <- round(nrow(shuffled_df.a) * split)
  
  # Create train
  train <- as.data.frame(shuffled_df.a[1:ind, ])
  
  # Create test
  test <- as.data.frame(shuffled_df.a[(ind + 1):nrow(shuffled_df.a), ])
  
  
  if (analysis == "SVM" & groups != 4) {
    
    train[,index] <- as.factor(train[,index])
    test[,index] <- as.factor(test[,index])
    
    #SVM model
    svm_cv <- tune("svm", bulbar_involvement ~ ., data = train, kernel = 'linear',
                   ranges = list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1)), probability=TRUE)
    
    svm_mod <- svm_cv$best.model
    
    #Prediction
    pred <- predict(svm_mod, newdata = test[,-index], probability=TRUE)
    
    y_pred <- attr(pred, "probabilities")
    

    } else if (analysis == "NN" & groups != 4) {
    
      train[,index] <- as.factor(train[,index])
      test[,index] <- as.factor(test[,index])
      
      #NN Model
      classifier <- neuralnet(bulbar_involvement~.,train, hidden=3,act.fct = "logistic",
                              linear.output = FALSE)
      
      
      #Prediction
      predict <- neuralnet::compute(classifier,test)
      
      
    } else if (analysis == "LDA" & groups != 4) {
      
    #LDA Model
    
    df.a.lda <- lda(bulbar_involvement~., data = train)
    
    #Prediction
    df.a.lda.predict <- predict(df.a.lda, newdata = test[-20])
    df.a.lda.predict.posteriors <- as.data.frame(df.a.lda.predict$posterior)
      
    } else if (analysis == "LR" & groups != 4) {
    
      train[,index] <- as.factor(train[,index])
      test[,index] <- as.factor(test[,index])
      
      #LR MODEL
      classifier <- glm(formula = bulbar_involvement~.,
                        binomial(link = 'logit'),
                        data = train, maxit = 50)
      
      #Prediction
      
      pred <- predict(classifier, newdata = test[ , -index], type = "response")
      
      
    } else if (analysis == "NB" & groups != 4) {
    
      train[,index] <- as.factor(train[,index])
      test[,index] <- as.factor(test[,index])
      
      #NB Model
      classifier <- naiveBayes(formula = bulbar_involvement~.,
                               family = binomial,
                               data = train)
      
      # Predict the outcomes using the model
      pred <- predict(classifier,test, type = "raw")
      
    } else if (analysis == "RF" & groups != 4){
  
      train[,index] <- as.factor(train[,index])
      test[,index] <- as.factor(test[,index])
      
      #RF Model
      classifier <- randomForest(formula = bulbar_involvement~.,
                             family = binomial,
                             data = train)
      #Prediction 
      pred <- predict(classifier,test[,-index], type = "prob")
  
    }

  
  #Plotting PCA
  if(plot_pca == TRUE) {
    
    #PC1 vs PC2
    g <- ggbiplot(pca.df, ellipse=TRUE, groups = df$bulbar_involvement)
    print(g)
    
    #Percentage of explained variances
    fviz_eig(pca.df)
    
    ##Cumulative sum of variance
    
    # variance
    pr_var = ( pca.df$sdev )^2 
    
    # % of variance
    prop_varex = pr_var / sum( pr_var )
    
    # Plot
    plot( prop_varex, xlab = "Principal Component", 
          ylab = "Proportion of Variance Explained", type = "b" )
    
    # Scree Plot
    plot( cumsum( prop_varex ), xlab = "Principal Component", 
          ylab = "Cumulative Proportion of Variance Explained", type = "b" )
    
  }

  ### CONFUSION MATRIX:
  
  if (confusion_matrix == TRUE & groups != 4) {
    
    
    if(analysis == "SVM") {
      
      if(groups == 1 | groups == 3){
        
        c_or_p1 <- ifelse(y_pred[ ,2] > threshold, "1", "2")
        
      } else if (groups == 2){
      
      c_or_p1 <- ifelse(y_pred[ ,1] > threshold, "1", "2")
      
      }
      p_class1 <- as.factor(c_or_p1)
      levels(p_class1)  = levels(test[, index])
      print(confusionMatrix(p_class1, test[ , index]))
      
    } else if (analysis == "NN") {
      
      c_or_p1 <- ifelse(predict$net.result [, 1] > threshold, "1", "2")
      p_class1 <- as.factor(c_or_p1)
      levels(p_class1)  = levels( test[, index])
      print(confusionMatrix(p_class1, test[, index]))  
      
      
    } else if (analysis == "LDA") {
      
      c_or_p1 <- ifelse(df.a.lda.predict.posteriors [ , 2] > threshold, "2", "1")
      test[["bulbar_involvement"]] <- as.factor(test[["bulbar_involvement"]])
      p_class1 <- as.factor(c_or_p1)
      levels(p_class1)  = levels(test[["bulbar_involvement"]])
      print(confusionMatrix(p_class1, test[["bulbar_involvement"]]))
      
    } else if (analysis == "LR") {
      
      c_or_p1 <- ifelse(pred > threshold, "1", "0")
      p_class1 <- as.factor(c_or_p1)
      levels(p_class1)  = levels( test[ , index])
      print(confusionMatrix(p_class1, test[ , index]))
      
    } else if (analysis == "NB") {
      
      c_or_p1 <- ifelse(pred [ , 1] > threshold, "1", "2")
      p_class1 <- as.factor(c_or_p1)
      levels(p_class1)  = levels( test[,13])
      print(confusionMatrix(p_class1, test[,13]))
      
    } else if (analysis == "RF") {
      
      c_or_p1 <- ifelse(pred [ , 1] > threshold, "1", "2")
      p_class1 <- as.factor(c_or_p1)
      levels(p_class1)  = levels( test[,index])
      print(confusionMatrix(p_class1, test[,index]))

  } 
  }
}  










