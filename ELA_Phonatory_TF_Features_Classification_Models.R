##############################################################################################################################################
# Title: 
# Author details: Alberto Tena del Pozo
# Contact details: atena@cimne.upc.edu
# Copyright statement:

# DataFrame info: 

##############################################################################################################################################



##############################################################################################################################################

#Install and load the required libraries
#???chwck to see if packages are installed

packages <- c("randomForest", "caTools", "factoextra", "dplyr", "ROCR", "gplots", "ggplot2", "lattice", "MASS", "caret", "pROC", "devtools", "ggbiplot", "glmnet", "lme4", "broom", "e1071")
ipak(packages)


##############################################################################################################################################
#Options

##Select the group for analysis: (1) C vs B, (2) C vs NB, (3) B vs NB, (4) C vs A


groups <- 1






##############################################################################################################################################



#Import the synthetic dataset with phonatory subsystem and time frequency_features freely available in https://github.com/atenad/ALS/blob/master/ALS_PHONATORY_TIME_FREQUENCY_SYNTHETIC_DATA.xlsx

library(readxl)
ph_tf <- read_excel("ALS_PHONATORY_TIME_FREQUENCY_SYNTHETIC_DATA.xlsx")
View(ph_tf)

ph_tf[ ,1:54] <- scale(ph_tf[ , c(1:54)], center= TRUE, scale=TRUE)


#Filtering the dataset for analysis and selecting the statistically significant features selected by implementing MANOVA in SPSS

if (groups == 1) {

df <- ph_tf %>%
  filter(Diagnostic != "NO")

df$Diagnostic <- droplevels(df$Diagnostic, exclude = "NO")
df$Diagnostic <- relevel(df$Diagnostic, c("YES"))

new_data <- df %>% dplyr::select("Jitter(absolute)", "Jitter(relative)", "Jitter(rap)", "Jitter(ppq5)", "Shimmer(dB)", "Shimmer(relative)", "Shimmer(apq3)", "Shimmer(apq5)", "Shimmer(apq11)", "Pitch(mean)", "Pitch(SD)", "Pitch(min)", "Pitch(max)", "HNR(mean)", "f_Cres6", "f_Cres7", "Enr_Bn2", "f_Med2", "f_Med6", "H_t", "momC_11", "Diagnostic")


} else if (groups == 2) {
  
  df <- ph_tf %>%
    filter(Diagnostic != "YES")
  
  df$Diagnostic <- droplevels(df$Diagnostic, exclude = "YES")
  df$Diagnostic <- relevel(df$Diagnostic, c("NO"))
  
  new_data <- df %>% dplyr::select("Jitter(relative)", "Shimmer(dB)", "Shimmer(relative)", "Shimmer(apq3)", "Shimmer(apq5)", "Shimmer(apq11)", "Pitch(SD)", "Pitch(max)", "HNR(mean)", "f_Cres1", "f_Med1", "IE_Bn2", "Diagnostic")
  
  
  
  
} else if (groups == 3) {
  
  
  df <- ph_tf %>%
    filter(Diagnostic != "control")
  
  df$Diagnostic <- droplevels(df$Diagnostic, exclude = "control")
  df$Diagnostic <- relevel(df$Diagnostic, c("YES"))
  
  new_data <- df %>% dplyr::select("Jitter(relative)", "Jitter(rap)", "Jitter(ppq5)", "Shimmer(dB)", "Shimmer(relative)", "Shimmer(apq3)", "Shimmer(apq5)", "Shimmer(apq11)", "Pitch(mean)", "Pitch(SD)", "Pitch(min)", "Pitch(max)", "HNR(mean)", "f_Cres1", "f_Cres2", "f_Cres6", "f_Med1", "f_Med2", "f_Med6", "IE_Bn2", "H_t", "momC_11", "Diagnostic")
  
  
  
  
} else if (groups == 4) {
  
  
  df <- ph_tf
  
  df$Diagnostic <- as.character(df$Diagnostic)
  
  df$Diagnostic[df$Diagnostic != "control"] <- "A"
  
  df$Diagnostic <- as.factor(df$Diagnostic)
  
  df$Diagnostic <- relevel(df$Diagnostic, c("A"))
  
  new_data <- df %>% dplyr::select("Jitter(relative)", "Jitter(rap)", "Jitter(ppq5)", "Shimmer(dB)", "Shimmer(relative)", "Shimmer(apq3)", "Shimmer(apq5)", "Shimmer(apq11)", "Pitch(mean)", "Pitch(SD)", "Pitch(min)", "Pitch(max)", "HNR(mean)", "f_Cres7", "f_Med1", "f_Med6", "Diagnostic")
  
  
  
  
  }




##################Classification Models####################


#################Random Forest####################


set.seed(42)


ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10, classProbs = TRUE, savePredictions = TRUE, summaryFunction=defaultSummary, sampling = "up")


rf_fit <- caret::train(Diagnostic ~ ., data = new_data,
                       method = "rf",
                       trControl = ctrl,
                       # family = binomial
                       #tuneGrid = rf_tunegrid
                       ntree = ntree,
                       #preProcess = c("center","scale")
                       #metric = "ROC"
)


rf_fit_stats <- thresholder(rf_fit,
                              threshold = seq(.5, 1, by = 0.05),
                              final = TRUE, statistics = c("F1", "Accuracy", "Sensitivity", "Specificity", "Kappa")
)


rf_fit_stats



################ SVM ###########################


set.seed(42)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10, classProbs = TRUE, savePredictions = TRUE, summaryFunction=defaultSummary, sampling = "up")

svm_fit <- caret::train(Diagnostic ~ ., data = new_data,
                        method = "svmLinear",
                        metric="Spec",
                        trControl = ctrl,
                        #tuneGrid = svm_grid, 
                        #preProcess = c("center","scale")
                        #metric = "F1"
)


svm_fit_stats <- thresholder(svm_fit,
                            threshold = seq(.5, 1, by = 0.05),
                            final = TRUE, statistics = c("F1", "Accuracy", "Sensitivity", "Specificity", "Kappa")
)


svm_fit_stats


################################### Logistic Regression ##############


ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10, classProbs = TRUE, savePredictions = TRUE, summaryFunction=defaultSummary, sampling = "up")

glm_fit <- caret::train(Diagnostic ~ ., data = new_data,
                        method = "glm",
                        #rep=5,
                        trControl = ctrl,
                        family = binomial(link = 'logit'),
                        #control=glm.control(maxit=100),
                        #preProcess = c("center","scale")
                        #metric = "ROC"
)

glm_fit_stats <- thresholder(glm_fit,
                            threshold = seq(.5, 1, by = 0.05),
                            final = TRUE, statistics = c("F1", "Accuracy", "Sensitivity", "Specificity", "Kappa")
)


glm_fit_stats



##################################### LDA ########################################

set.seed(42)


ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10, classProbs = TRUE, savePredictions = TRUE, summaryFunction=defaultSummary, sampling = "up")

lda_fit <- caret::train(Diagnostic ~ ., data = new_data,
                        method = "lda",
                        trControl = ctrl,
                        #preProcess = c("center","scale")
                        #metric = "ROC""
)

lda_fit_stats <- thresholder(lda_fit,
                            threshold = seq(.5, 1, by = 0.05),
                            final = TRUE, statistics = c("F1", "Accuracy", "Sensitivity", "Specificity", "Kappa")
)


lda_fit_stats

##################################### NN ########################################

set.seed(42)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=10, classProbs = TRUE, savePredictions = TRUE, summaryFunction=defaultSummary, sampling = "up")


nn_tunegrid <- expand.grid(size=c(3 ))



nn_fit <- caret::train(Diagnostic ~ ., data = new_data,
                       method = "mlp",
                       #rep=5,
                       trControl = ctrl,
                       tuneGrid = nn_tunegrid,
                      # preProcess = c("center","scale"),
                       act.fct = "logistic",
                       linear.output = FALSE,
                       #metric = "ROC"
)


nn_fit_stats <- thresholder(nn_fit,
                            threshold = seq(.5, 1, by = 0.05),
                            final = TRUE, statistics = c("F1", "Accuracy", "Sensitivity", "Specificity", "Kappa")
)


nn_fit_stats

