library("foreign")
emailspam=read.arff("http://www.cis.utas.edu.au/iWeb3/~soyeonh/spambase.arff")
#Data selection and pre-processing
colnames(emailspam)[37]<-"word_freq_2016"
emailspam$word_freq_george<-NULL
emailspam$word_freq_650<-NULL

#PATTERN EVALUATION

#patter evaluation jrip
library("RWeka")
library("nnet")
spam_jrip=JRip(spam~., data=emailspam)
eval_jrip<-evaluate_Weka_classifier(spam_jrip,numFolds=10,complexity=FALSE,seed=1,class=TRUE)
cf_jrip<-eval_jrip[4]$confusionMatrix
cf_jrip.rate=prop.table(cf_jrip)* 100
cf_jrip_rate<-round(cf_jrip.rate, digit=2)
#overall error rate of jrip
diag.index<-cbind(1:2, 1:2)
error.overalljrip=sum(cf_jrip.rate)-sum(cf_jrip.rate[diag.index])

#patter evaluation j48
spam_j48=J48(spam~.,data=emailspam)
eval_j48 <- evaluate_Weka_classifier(spam_j48, numFolds=10,complexity=FALSE,seed=1,
class=TRUE)
cf_j48<-eval_j48[4]$confusionMatrix
cf_j48.rate=prop.table(cf_j48)* 100
cf_j48_rate<-round(cf_j48.rate, digit=2)
error.overallj48=sum(cf_j48.rate)-sum(cf_j48.rate[diag.index])

#patter evaluation SVM
spam_svm=SMO(spam~., emailspam)
eval_svm <- evaluate_Weka_classifier(spam_svm, numFolds=10,complexity=FALSE,seed=1,
class=TRUE)
cf_svm<-eval_svm[4]$confusionMatrix
cf_svm.rate=prop.table(cf_svm)* 100
cf_svm_rate<-round(cf_svm.rate, digit=2)
error.overallsvm=sum(cf_svm.rate)-sum(cf_svm.rate[diag.index])

#pattern evaluation nnet
data.size<-nrow(emailspam)
set.seed(1111)
samp<-c(sample(1:data.size, data.size*0.7))
data.tr<- emailspam[samp,]
data.test<-emailspam[-samp,]
spam_nnet=nnet(spam~., data=data.tr,size=2, decay=5e-04,maxit=200)
predicted<-predict(spam_nnet,data.test,type="class")
actual<-data.test$spam
nnet_confusion.matrix<-table(actual,predicted)
nnet_rate=prop.table(nnet_confusion.matrix)*100
round(nnet_rate,digit=2)
error.overallnnet<-sum(nnet_rate)-sum(nnet_rate[diag.index])

#pattern evaluation k nearest neighbour
library("class")
class<-emailspam[samp, 56]
spam_knn<-knn(data.tr,data.test,class,k=3,prob=TRUE)
summary(spam_knn)
knn_confusion.matrix<-table(actual,spam_knn)
knn_rate=prop.table(knn_confusion.matrix)*100
round(knn_rate,digit=2)
error.overallknn<-sum(knn_rate)-sum(knn_rate[diag.index])

#pattern evaluation PART classifier
spam_part=PART(spam~., emailspam)
eval_part <- evaluate_Weka_classifier(spam_part, numFolds=10,complexity=FALSE,seed=1,
class=TRUE)
cf_part<-eval_part[4]$confusionMatrix
cf_part.rate=prop.table(cf_part)* 100
cf_part_rate<-round(cf_part.rate, digit=2)
error.overallpart=sum(cf_part.rate)-sum(cf_part.rate[diag.index])


##detect_spam FUNCTION (use it after excuating above code)

library(stringr)
detect_spam<-function(x){
split<-strsplit(x," ")
#print(split[[1]])
#calculate the words' frequency of occurrence that we need and place them in attr_values used for prediction. 
match_names<-c("make","address","all","3d","our","over","remove","internet","order"
,"mail","receive","will","people","report","addresses","free","business","email","you"
,"credit","your","font","000","money","hp","hpl","lab","labs","telnet","857","data","415",
"85","technology","2016","parts","pm","direct","cs","meeting","original","project","re","edu","table",
"conference")
attr_values<-emailspam[0,]
#I use "\\b" here to let the function can match the exact word, so 'our' will not be matched in the word 'hour'
for (i in 1:46){
attr_values[1,i]<-(100*length(grep(paste("\\b",match_names[i],"\\b",sep=""),split[[1]],ignore.case = TRUE)))/length(split[[1]])
}

#calculate char frequency
attr_values[1,47]=100*str_count(x, ";")/(str_count(x)-str_count(x," "))
attr_values[1,48]=100*str_count(x, "\\(")/(str_count(x)-str_count(x," "))
attr_values[1,49]=100*str_count(x, "\\[")/(str_count(x)-str_count(x," "))
attr_values[1,50]=100*str_count(x, "!")/(str_count(x)-str_count(x," "))
attr_values[1,51]=100*str_count(x, "\\$")/(str_count(x)-str_count(x," "))
attr_values[1,52]=100*str_count(x, "#")/(str_count(x)-str_count(x," "))
#calculate capital_run_length
count_capital<-sapply(regmatches(split[[1]], gregexpr("[A-Z]", split[[1]], perl=TRUE)), length)
if(max(count_capital)>0){
attr_values[1,53]=mean(count_capital[count_capital>0])
}
else {attr_values[1,53]=0}
attr_values[1,54]=max(count_capital)
attr_values[1,55]=sum(count_capital)
#we can check whether the attributes values are correct using print(attr_values[1,]) compare with the content 
#of the email
predicted<-predict(spam_j48,attr_values,type="class")
#print(predicted)
if(predicted==1){
print("spam")
}
else {
print("non spam")
}

}
data1<-readChar("spam1.txt",nchars=1e6)
detect_spam(data1)