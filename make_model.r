library(dplyr)
library(lubridate)
library(SnowballC)
library(httr)
library(tm)
library(gsheet)
library(e1071)
# Chatbot for guiding customers to required documentation

# Methdology
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
# 3. Train Support Vector Machines model with the training matrix
# 4. Propose a testing question
# 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
# 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
# 7. Predict the answer with the trained SVM model

# read data
#data = gsheet2tbl("https://docs.google.com/spreadsheets/d/1lq3tOwDrxD9ZEuKc_oYCI-2lb9octmcgbN2P274D6wk/edit#gid=0")
#saveRDS(data, file = "data")
data = readRDS("data")
data1 <- data %>% filter(Area=="design")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus1 = VCorpus(VectorSource(data1$Question))
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 = tm_map(corpus1, removeNumbers)
corpus1 = tm_map(corpus1, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus1 = tm_map(corpus1, stemDocument)
corpus1 = tm_map(corpus1, stripWhitespace)

# convert to DTM
dtm1 = DocumentTermMatrix(corpus1)

# convert to dataframe
dataset1 = as.data.frame(as.matrix(dtm1))


# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train1 = cbind(data1['Answers'], dataset1)

# 3. Train SVM model with the training matrix, specify type
svmfit1 = svm(Answers ~., data_train1, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
designpred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus1 = VCorpus(VectorSource(x))
  corpus1 = tm_map(corpus1, content_transformer(tolower))
  corpus1 = tm_map(corpus1, removeNumbers)
  corpus1 = tm_map(corpus1, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus1 = tm_map(corpus1, stemDocument)
  corpus1 = tm_map(corpus1, stripWhitespace)
  
  # convert to DTM
  dtm1 = DocumentTermMatrix(corpus1)
  
  # convert to dataframe
  data_test1 = as.data.frame(as.matrix(dtm1))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data1 = dataset1[1,]
  add_data1[add_data1 == 1] = 0
  data_test1=cbind(data_test1,add_data1)
  
  # 7. Predict the answer with the trained SVM model
  p1 = predict(svmfit1, data_test1)
  answer1 = as.character(p1)
  paste(answer1)
}

# Predict
#pred("Barcode scanner")
data2 <- data %>% filter(Area=="payments")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus2 = VCorpus(VectorSource(data2$Question))
corpus2 = tm_map(corpus2, content_transformer(tolower))
corpus2 = tm_map(corpus2, removeNumbers)
corpus2 = tm_map(corpus2, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus2 = tm_map(corpus2, stemDocument)
corpus2 = tm_map(corpus2, stripWhitespace)

# convert to DTM
dtm2 = DocumentTermMatrix(corpus2)

# convert to dataframe
dataset2 = as.data.frame(as.matrix(dtm2))


# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train2 = cbind(data2['Answers'], dataset2)

# 3. Train SVM model with the training matrix, specify type
svmfit2 = svm(Answers ~., data_train2, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
paymentspred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus2 = VCorpus(VectorSource(x))
  corpus2 = tm_map(corpus2, content_transformer(tolower))
  corpus2 = tm_map(corpus2, removeNumbers)
  corpus2 = tm_map(corpus2, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus2 = tm_map(corpus2, stemDocument)
  corpus2 = tm_map(corpus2, stripWhitespace)
  
  # convert to DTM
  dtm2 = DocumentTermMatrix(corpus2)
  
  # convert to dataframe
  data_test2 = as.data.frame(as.matrix(dtm2))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data2 = dataset2[1,]
  add_data2[add_data2 == 1] = 0
  data_test2=cbind(data_test2,add_data2)
  
  # 7. Predict the answer with the trained SVM model
  p2 = predict(svmfit2, data_test2)
  answer2 = as.character(p2)
  paste(answer2)
}

ebay_data <- data %>% filter(Area=="ebay")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
ebay_corpus = VCorpus(VectorSource(ebay_data$Question))
ebay_corpus = tm_map(ebay_corpus, content_transformer(tolower))
#ebay_corpus = tm_map(ebay_corpus, removeNumbers)
ebay_corpus = tm_map(ebay_corpus, removePunctuation)

# ebay_corpus = tm_map(ebay_corpus, removeWords, stopwords())
ebay_corpus = tm_map(ebay_corpus, stemDocument)
ebay_corpus = tm_map(ebay_corpus, stripWhitespace)

# convert to DTM
ebay_dtm = DocumentTermMatrix(ebay_corpus)

# convert to dataframe
ebay_dataset = as.data.frame(as.matrix(ebay_dtm))


# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
ebay_data_train = cbind(ebay_data['Answers'], ebay_dataset)

# 3. Train SVM model with the training matrix, specify type
ebay_svmfit = svm(Answers ~., ebay_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
ebaypred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  ebaycorpus = VCorpus(VectorSource(x))
  ebaycorpus = tm_map(ebaycorpus, content_transformer(tolower))
  #ebaycorpus = tm_map(ebaycorpus, removeNumbers)
  ebaycorpus = tm_map(ebaycorpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  ebaycorpus = tm_map(ebaycorpus, stemDocument)
  ebaycorpus = tm_map(ebaycorpus, removePunctuation)
  ebaycorpus = tm_map(ebaycorpus, stripWhitespace)
  
  # convert to DTM
  ebay_dtm = DocumentTermMatrix(ebaycorpus)
  
  # convert to dataframe
  ebay_data_test = as.data.frame(as.matrix(ebay_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  ebay_add_data = ebay_dataset[1,]
  ebay_add_data[ebay_add_data == 1] = 0
  ebay_data_test=cbind(ebay_data_test,ebay_add_data)
  
  # 7. Predict the answer with the trained SVM model
  ebayp = predict(ebay_svmfit, ebay_data_test)
  ebay_answer = as.character(ebayp)
  return(paste(ebay_answer))
}

data4 <- data %>% filter(Area=="inventory")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
corpus4 = VCorpus(VectorSource(data4$Question))
corpus4 = tm_map(corpus4, content_transformer(tolower))
corpus4 = tm_map(corpus4, removeNumbers)
corpus4 = tm_map(corpus4, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
corpus4 = tm_map(corpus4, stemDocument)
corpus4 = tm_map(corpus4, stripWhitespace)

# convert to DTM
dtm4 = DocumentTermMatrix(corpus4)

# convert to dataframe
dataset4 = as.data.frame(as.matrix(dtm4))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
data_train4 = cbind(data4['Answers'], dataset4)

# 3. Train SVM model with the training matrix, specify type

svmfit4 = svm(Answers ~., data_train4, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
inventorypred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  corpus4 = VCorpus(VectorSource(x))
  corpus4 = tm_map(corpus4, content_transformer(tolower))
  corpus4 = tm_map(corpus4, removeNumbers)
  corpus4 = tm_map(corpus4, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  corpus4 = tm_map(corpus4, stemDocument)
  corpus4 = tm_map(corpus4, stripWhitespace)
  
  # convert to DTM
  dtm4 = DocumentTermMatrix(corpus4)
  
  # convert to dataframe
  data_test4 = as.data.frame(as.matrix(dtm4))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  add_data4 = dataset4[1,]
  add_data4[add_data4 == 1] = 0
  data_test4=cbind(data_test4,add_data4)
  
  # 7. Predict the answer with the trained SVM model
  p4 = predict(svmfit4, data_test4)
  answer4 = as.character(p4)
  paste(answer4)
}

#ebaypred("error")

Amazon <- data %>% filter(Area=="amazon")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
Amazon_corpus = VCorpus(VectorSource(Amazon$Question))
Amazon_corpus = tm_map(Amazon_corpus, content_transformer(tolower))
Amazon_corpus = tm_map(Amazon_corpus, removeNumbers)
Amazon_corpus = tm_map(Amazon_corpus, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
Amazon_corpus = tm_map(Amazon_corpus, stemDocument)
Amazon_corpus = tm_map(Amazon_corpus, stripWhitespace)

# convert to DTM
Amazon_dtm = DocumentTermMatrix(Amazon_corpus)

# convert to dataframe
Amazon_dataset = as.data.frame(as.matrix(Amazon_dtm))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
Amazon_data_train = cbind(Amazon['Answers'], Amazon_dataset)

# 3. Train SVM model with the training matrix, specify type

Amazon_svmfit = svm(Answers ~., Amazon_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
amazonpred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  Amazon_corpus = VCorpus(VectorSource(x))
  Amazon_corpus = tm_map(Amazon_corpus, content_transformer(tolower))
  Amazon_corpus = tm_map(Amazon_corpus, removeNumbers)
  Amazon_corpus = tm_map(Amazon_corpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  Amazon_corpus = tm_map(Amazon_corpus, stemDocument)
  Amazon_corpus = tm_map(Amazon_corpus, stripWhitespace)
  
  # convert to DTM
  Amazon_dtm = DocumentTermMatrix(Amazon_corpus)
  
  # convert to dataframe
  Amazon_data_test = as.data.frame(as.matrix(Amazon_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  Amazon_add_data = Amazon_dataset[1,]
  Amazon_add_data[Amazon_add_data == 1] = 0
  Amazon_data_test=cbind(Amazon_data_test,Amazon_add_data)
  
  # 7. Predict the answer with the trained SVM model
  Amazon_p = predict(Amazon_svmfit, Amazon_data_test)
  Amazon_answer = as.character(Amazon_p)
  paste(Amazon_answer)
}

api <- data %>% filter(Area=="data")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
api_corpus = VCorpus(VectorSource(api$Question))
api_corpus = tm_map(api_corpus, content_transformer(tolower))
api_corpus = tm_map(api_corpus, removeNumbers)
api_corpus = tm_map(api_corpus, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
api_corpus = tm_map(api_corpus, stemDocument)
api_corpus = tm_map(api_corpus, stripWhitespace)

# convert to DTM
api_dtm = DocumentTermMatrix(api_corpus)

# convert to dataframe
api_dataset = as.data.frame(as.matrix(api_dtm))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
api_data_train = cbind(api['Answers'], api_dataset)

# 3. Train SVM model with the training matrix, specify type

api_svmfit = svm(Answers ~., api_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
datapred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  api_corpus = VCorpus(VectorSource(x))
  api_corpus = tm_map(api_corpus, content_transformer(tolower))
  api_corpus = tm_map(api_corpus, removeNumbers)
  api_corpus = tm_map(api_corpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  api_corpus = tm_map(api_corpus, stemDocument)
  api_corpus = tm_map(api_corpus, stripWhitespace)
  
  # convert to DTM
  api_dtm = DocumentTermMatrix(api_corpus)
  
  # convert to dataframe
  api_data_test = as.data.frame(as.matrix(api_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  api_add_data = api_dataset[1,]
  api_add_data[api_add_data == 1] = 0
  api_data_test=cbind(api_data_test,api_add_data)
  
  # 7. Predict the answer with the trained SVM model
  api_p = predict(api_svmfit, api_data_test)
  api_answer = as.character(api_p)
  paste(api_answer)
}

kogan <- data %>% filter(Area=="kogan")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
kogan_corpus = VCorpus(VectorSource(kogan$Question))
kogan_corpus = tm_map(kogan_corpus, content_transformer(tolower))
kogan_corpus = tm_map(kogan_corpus, removeNumbers)
kogan_corpus = tm_map(kogan_corpus, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
kogan_corpus = tm_map(kogan_corpus, stemDocument)
kogan_corpus = tm_map(kogan_corpus, stripWhitespace)

# convert to DTM
kogan_dtm = DocumentTermMatrix(kogan_corpus)

# convert to dataframe
kogan_dataset = as.data.frame(as.matrix(kogan_dtm))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
kogan_data_train = cbind(kogan['Answers'], kogan_dataset)

# 3. Train SVM model with the training matrix, specify type

kogan_svmfit = svm(Answers ~., kogan_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
koganpred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  kogan_corpus = VCorpus(VectorSource(x))
  kogan_corpus = tm_map(kogan_corpus, content_transformer(tolower))
  kogan_corpus = tm_map(kogan_corpus, removeNumbers)
  kogan_corpus = tm_map(kogan_corpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  kogan_corpus = tm_map(kogan_corpus, stemDocument)
  kogan_corpus = tm_map(kogan_corpus, stripWhitespace)
  
  # convert to DTM
  kogan_dtm = DocumentTermMatrix(kogan_corpus)
  
  # convert to dataframe
  kogan_data_test = as.data.frame(as.matrix(kogan_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  kogan_add_data = kogan_dataset[1,]
  kogan_add_data[kogan_add_data == 1] = 0
  kogan_data_test=cbind(kogan_data_test,kogan_add_data)
  
  # 7. Predict the answer with the trained SVM model
  kogan_p = predict(kogan_svmfit, kogan_data_test)
  kogan_answer = as.character(kogan_p)
  paste(kogan_answer)
}

catch <- data %>% filter(Area=="catch")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
catch_corpus = VCorpus(VectorSource(catch$Question))
catch_corpus = tm_map(catch_corpus, content_transformer(tolower))
catch_corpus = tm_map(catch_corpus, removeNumbers)
catch_corpus = tm_map(catch_corpus, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
catch_corpus = tm_map(catch_corpus, stemDocument)
catch_corpus = tm_map(catch_corpus, stripWhitespace)

# convert to DTM
catch_dtm = DocumentTermMatrix(catch_corpus)

# convert to dataframe
catch_dataset = as.data.frame(as.matrix(catch_dtm))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
catch_data_train = cbind(catch['Answers'], catch_dataset)

# 3. Train SVM model with the training matrix, specify type

catch_svmfit = svm(Answers ~., catch_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
catchpred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  catch_corpus = VCorpus(VectorSource(x))
  catch_corpus = tm_map(catch_corpus, content_transformer(tolower))
  catch_corpus = tm_map(catch_corpus, removeNumbers)
  catch_corpus = tm_map(catch_corpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  catch_corpus = tm_map(catch_corpus, stemDocument)
  catch_corpus = tm_map(catch_corpus, stripWhitespace)
  
  # convert to DTM
  catch_dtm = DocumentTermMatrix(catch_corpus)
  
  # convert to dataframe
  catch_data_test = as.data.frame(as.matrix(catch_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  catch_add_data = catch_dataset[1,]
  catch_add_data[catch_add_data == 1] = 0
  catch_data_test=cbind(catch_data_test,catch_add_data)
  
  # 7. Predict the answer with the trained SVM model
  catch_p = predict(catch_svmfit, catch_data_test)
  catch_answer = as.character(catch_p)
  paste(catch_answer)
}

myob <- data %>% filter(Area=="myob")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
myob_corpus = VCorpus(VectorSource(myob$Question))
myob_corpus = tm_map(myob_corpus, content_transformer(tolower))
myob_corpus = tm_map(myob_corpus, removeNumbers)
myob_corpus = tm_map(myob_corpus, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
myob_corpus = tm_map(myob_corpus, stemDocument)
myob_corpus = tm_map(myob_corpus, stripWhitespace)

# convert to DTM
myob_dtm = DocumentTermMatrix(myob_corpus)

# convert to dataframe
myob_dataset = as.data.frame(as.matrix(myob_dtm))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
myob_data_train = cbind(myob['Answers'], myob_dataset)

# 3. Train SVM model with the training matrix, specify type

myob_svmfit = svm(Answers ~., myob_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
myobpred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  myob_corpus = VCorpus(VectorSource(x))
  myob_corpus = tm_map(myob_corpus, content_transformer(tolower))
  myob_corpus = tm_map(myob_corpus, removeNumbers)
  myob_corpus = tm_map(myob_corpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  myob_corpus = tm_map(myob_corpus, stemDocument)
  myob_corpus = tm_map(myob_corpus, stripWhitespace)
  
  # convert to DTM
  myob_dtm = DocumentTermMatrix(myob_corpus)
  
  # convert to dataframe
  myob_data_test = as.data.frame(as.matrix(myob_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  myob_add_data = myob_dataset[1,]
  myob_add_data[myob_add_data == 1] = 0
  myob_data_test=cbind(myob_data_test,myob_add_data)
  
  # 7. Predict the answer with the trained SVM model
  myob_p = predict(myob_svmfit, myob_data_test)
  myob_answer = as.character(myob_p)
  paste(myob_answer)
}

xero <- data %>% filter(Area=="xero")
# 1. Convert training questions into document term matrix (sparse matrix with 1s and 0s)
#clean the text
xero_corpus = VCorpus(VectorSource(xero$Question))
xero_corpus = tm_map(xero_corpus, content_transformer(tolower))
xero_corpus = tm_map(xero_corpus, removeNumbers)
xero_corpus = tm_map(xero_corpus, removePunctuation)

# corpus = tm_map(corpus, removeWords, stopwords())
xero_corpus = tm_map(xero_corpus, stemDocument)
xero_corpus = tm_map(xero_corpus, stripWhitespace)

# convert to DTM
xero_dtm = DocumentTermMatrix(xero_corpus)

# convert to dataframe
xero_dataset = as.data.frame(as.matrix(xero_dtm))

# 2. Match the matrix of each training question with its corresponding answer to form a training matrix
xero_data_train = cbind(xero['Answers'], xero_dataset)

# 3. Train SVM model with the training matrix, specify type

xero_svmfit = svm(Answers ~., xero_data_train, kernel = "linear",  type = "C", cost = 100, scale = FALSE)

# 4. Propose a testing quesiton and build the prediction function
xeropred = function(x){
  
  # 5. Convert the testing question into document term matrix (sparse matrix with 1s and 0s)
  #clean the text
  xero_corpus = VCorpus(VectorSource(x))
  xero_corpus = tm_map(xero_corpus, content_transformer(tolower))
  xero_corpus = tm_map(xero_corpus, removeNumbers)
  xero_corpus = tm_map(xero_corpus, removePunctuation)
  
  # corpus = tm_map(corpus, removeWords, stopwords())
  xero_corpus = tm_map(xero_corpus, stemDocument)
  xero_corpus = tm_map(xero_corpus, stripWhitespace)
  
  # convert to DTM
  xero_dtm = DocumentTermMatrix(xero_corpus)
  
  # convert to dataframe
  xero_data_test = as.data.frame(as.matrix(xero_dtm))
  
  # 6. Merge the testing DTM with training DTM, with testing DTM 1s for all terms and training DTM 0s for all terms
  xero_add_data = xero_dataset[1,]
  xero_add_data[xero_add_data == 1] = 0
  xero_data_test=cbind(xero_data_test,xero_add_data)
  
  # 7. Predict the answer with the trained SVM model
  xero_p = predict(xero_svmfit, xero_data_test)
  xero_answer = as.character(xero_p)
  paste(xero_answer)
}
