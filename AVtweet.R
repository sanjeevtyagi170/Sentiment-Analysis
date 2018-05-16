tweet<-read.csv("traintweet.csv")
test<-read.csv("testtweet.csv")

#create a corpus for train and test data 
corpustweet<-Corpus(VectorSource(tweet$tweet))# train file corpus
corpustest<-Corpus(VectorSource(test$tweet)) #test file corpus




#Text Processing Training Dataset
corpustweet<-tm_map(corpustweet, stripWhitespace)
corpustweet<-tm_map(corpustweet,tolower)
f<-content_transformer(function(x, pattern) gsub(pattern," ", x))
corpustweet<-tm_map(corpustweet,f,"#")
corpustweet<-tm_map(corpustweet,f,"@")
corpustweet<-tm_map(corpustweet,removePunctuation)
corpustweet<-tm_map(corpustweet,removeNumbers)


#File saved in utf-8 format,if somehow program do not run properly
#Copy Unicode variable and its components from unicode.txt file 
#or from solution descriptiom because R didn't recognize 
#these special characters in default encoding
# unicode variable is used to remove unicode characters from twitter comments
#unicode<-c("?","?","?","?","?","?","~",""","?","T","z","'",""","'",".",
#           "'","?","s","???","?","o","?","?","?","?","???","f","^",",","?",
#           "???",">", "?","-","?","?",".","?","?","?",
#           "\u0081","\u0082","\u0083","\u0084","\u0085","\u0086",
#           "\u0087","\u0088","\u0089","\u008a","\u008b","\u008c","\u008d",
#           "\u008e","\u008f","\u0090","\u0091","\u0092","\u0093","\u0094",
#           "\u0095","\u0096","\u0097","\u0098","\u0099","\u009a","\u009b",
#           "\u009c","\u009d","\u009e","\u009f" )

f<-content_transformer(function(x, pattern) gsub(pattern," ", x))
for (i in unicode){
corpustweet<-tm_map(corpustweet,f,print(i))
}
corpustweet<-tm_map(corpustweet,removeWords,c("user",stopwords("english")))
corpustweet<-tm_map(corpustweet,stemDocument)
corpustweet[[286]]$content



#Text Processing Test Dataset
corpustest<-tm_map(corpustest, stripWhitespace)
corpustest<-tm_map(corpustest,tolower)
f<-content_transformer(function(x, pattern) gsub(pattern," ", x))
corpustest<-tm_map(corpustest,f,"#")
corpustest<-tm_map(corpustest,f,"@")
corpustest<-tm_map(corpustest,removePunctuation)
corpustest<-tm_map(corpustest,removeNumbers)
corpustest<-tm_map(corpustest,removeWords,c("user",stopwords("english")))
f<-content_transformer(function(x, pattern) gsub(pattern," ", x))
for (i in unicode){
  corpustest<-tm_map(corpustest,f,print(i))
}
corpustest<-tm_map(corpustest,stemDocument)


#Word Cloud 
#library(RColorBrewer)
#library(wordcloud)
#pal<-brewer.pal(9,"RdGy")
#cloud<-wordcloud(corpustweet,max.words=50,rot.per=.5,color=pal)

#Document Term Matrix
dtm<-DocumentTermMatrix(corpustweet)
dtmtest<-DocumentTermMatrix(corpustest)

#Sparse Training
sparsetrain<-removeSparseTerms(dtm,.9999)
sparsetweettrain<-as.data.frame(as.matrix(sparsetrain))
sparsetweettrain$label<-tweet$label
sparsetweettrain$id<-tweet$id

#Helpful Command use of grepl
colnames(negsparse)[grepl("bi(g*|o*|t*)",colnames(negsparse))]

#Negative words from the dataset
#where label==1 means a negative comment
negsparse<-subset(sparsetweettrain,label==1)
possparse<-subset(sparsetweettrain,label==0)

#removing all entries which have sum==zero columnwise
for (i in colnames(negsparse)) {
  if (sum(negsparse[,colnames(negsparse)==i])==0){
    negsparse[,colnames(negsparse)==i]=NULL
  } 
}
negnames<-colnames(negsparse)
negnames<-as.data.frame(as.matrix(negnames))

#columns reduced from 5888 to 2620 after executing above command

Unnecessary<-c("your","you","yet","yesterday","year","yea","yay","yeah",
               "wud","would","wouldnt","wont","will","whose","who","what",
               "where","whether","wasnt","when","upon","Unless","unlike",
               "until","tonight","today","time","thursday","that","one",
               "two","three","four","five","tomorrow","monday","tuesday",
               "wednesday","friday","saturday","though","thousand","this",
               "the","there","ten","take","sunday","summer","winter",
               "do(n*)t"
               )
#checking
df<-c("do","does","dont","can","doit","done","don't","door")
df[grep(Unnecessary[51],df)]


#Experimentation Splitting and Predicting on given dataset
#library(caTools)
#spl<-sample.split(sparsetweettrain,SplitRatio = .7)
#train<-subset(sparsetweettrain,spl==T)
#test<-subset(sparsetweettrain,spl==F)
#modeltest<-rpart(label~.,data = train,method = "class")
#prp(modeltest)
#pred<-predict(modeltest,newdata = test,type = "class")
#table(test$label,pred)

#Sparse test
sparsetest<-removeSparseTerms(dtmtest,.999)
sparsetweettest<-as.data.frame(as.matrix(sparsetest))
sparsetweettest$id<-test$id

#Model Building simple rpart with train dataset
library(rpart.plot)
library(rpart)
model1<-rpart(label~.,data=sparsetweettrain,method="class")
prp(model1)
predmod1<-predict(model1,newdata = bag1,type = "class")
sparsetweettest$label<-predmod1
label=predmod1
id=sparsetweettest$id
myoutput<-cbind(id,label)
myoutput[myoutput==1]<-0
myoutput[myoutput==2]<-1
write.csv(myoutput,"C:/Users/Ersam/test_predictions.csv",row.names = F)

#RandomForest MODEL
library(randomForest)
model2<-randomForest(label~.,data = sparsetweettrain)
predmod2<-predict(model2,newdata = sparsetweettest,type = "response")

