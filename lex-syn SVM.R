
#Maryam Nasseri

# Text cleaning and classification extension of lex-syn project from a corpus of abstracts by EFL and NS groups. 
#Based on Nasseri (2020) project. support vector machine classification

#A brief version of code and results

library("tm") 
library("caret") 
library("kernlab") 
library("e1071")


#clean the training texts 
train <- VCorpus(DirSource("Training", encoding = "UTF-8"), readerControl=list(language="English"))
train <- tm_map(train, content_transformer(stripWhitespace))
train <- tm_map(train, content_transformer(tolower))
train <- tm_map(train, content_transformer(removeNumbers))
train <- tm_map(train, content_transformer(removePunctuation))


#clean the test texts 
test <- VCorpus(DirSource("Testing", encoding = "UTF-8"), readerControl=list(language="English"))
test <- tm_map(test, content_transformer(stripWhitespace))
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, content_transformer(removeNumbers))
test <- tm_map(test, content_transformer(removePunctuation))


#make document term matrices 
train.dtm <- as.matrix(DocumentTermMatrix(train, control=list(wordLengths=c(1,Inf))))
test.dtm <- as.matrix(DocumentTermMatrix(test, control=list(wordLengths=c(1,Inf))))


#dataframes
train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])


#correct labels
label.df <- data.frame(row.names(train.df))
colnames(label.df) <- c("filenames")
label.df<- substr(label.df$filenames, 3,4)
train.df$corpus<- label.df
labels.df<- data.frame(row.names(test.df))
colnames(labels.df) <- c("filenames")
labels.df<- substr(labels.df$filenames, 3,4)
test.df$corpus<- labels.df


#train the model with support vector machine, kernela parameter Radial Basis kernel function "Gaussian"
model<- ksvm(corpus~., data=train.df, kernel="rbfdot")
pred.df<- predict(model, test.df)
Levels: EF NS



#confusion matrix
confusionMatrix(t, mode="everything")

Confusion Matrix and Statistics

    
     EF NS
  EF  6  0
  NS 15 21
                                          
               Accuracy : 0.6429          
                 95% CI : (0.4803, 0.7845)
    No Information Rate : 0.5             
    P-Value [Acc > NIR] : 0.0442148       
                                          
                  Kappa : 0.2857          
                                          
 Mcnemar's Test P-Value : 0.0003006       
                                          
            Sensitivity : 0.2857          
            Specificity : 1.0000          
         Pos Pred Value : 1.0000          
         Neg Pred Value : 0.5833          
              Precision : 1.0000          
                 Recall : 0.2857          
                     F1 : 0.4444          
             Prevalence : 0.5000          
         Detection Rate : 0.1429          
   Detection Prevalence : 0.1429          
      Balanced Accuracy : 0.6429          
                                          
       'Positive' Class : EF    

# in the final project, the accuracy jumps to 90%

 

