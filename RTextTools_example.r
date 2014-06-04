#!/usr/bin/Rscript

#####################
#Supervised Machine Learning example
#Go here for documentation:
#http://journal.r-project.org/archive/2013-1/collingwood-jurka-boydstun-etal.pdf
#####################

# LOAD THE RTextTools LIBRARY
#install.packages("RTextTools") #Needs to be R-2.14.1 or greater
library(RTextTools)

#CHANGE WORKING DIRECTORY TO YOUR WORKING DIRECTORY
setwd("/home/laura/Desktop/My Documents/D-Lab/textanalysis_workshop/R/")

data(USCongress)

# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix <- create_matrix(USCongress$text, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

# CREATE CONTAINER
container <- create_container(doc_matrix, USCongress$major, trainSize=1:4000,
                              testSize=4001:4449, virgin=FALSE)

# RUN NINE DIFFERENT TRAINING MODELS

#first models take little memory and are thus faster
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")

#the following models take more memory, we'll skip them today
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")


# CLASSIFY DATA USING TRAINED MODELS

# we'll only do this on the four low-memory models we used

SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)

# the remaining high-memory models we will skip

BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)


analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
                                    GLMNET_CLASSIFY,
                                    MAXENT_CLASSIFY))

summary(analytics)

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()


create_ensembleSummary(analytics@document_summary)


# N-FOLD CROSS-VALIDATION

SVM <- cross_validate(container, 4, "SVM")
GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
SLDA <- cross_validate(container, 4, "SLDA")


BAGGING <- cross_validate(container, 4, "BAGGING")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
TREE <- cross_validate(container, 4, "TREE")

#Export output to CSV file. You can export any of the analytics variables

write.csv(analytics@document_summary, "DocumentSummary.csv")
head(USCongress)
