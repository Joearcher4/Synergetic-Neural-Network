#Import the required libraries used in the code below
library(foreign)
library(far)
library(caret)
library(e1071)
library(class)
library(gmodels)
library('stringr')
#read in the data and define function words,special characters and punctuations
amazon_data<-read.arff("C:/Users/colum/Desktop/Stats Learning/Project/Amazon_initial_50_30_10000/Amazon_initial_50_30_10000_1.arff")
function_words<-as.matrix(read.csv("C:/Users/colum/Desktop/Stats Learning/Project/Function_words.csv",header = FALSE,sep=" "))
function_words[1,1] <- "a"
special_character<- c("~" , "@" , "#" , "%",  "&" , "-" , "_" , "=" ,"\\+", ">" , "<" , "\\[","\\]","\\{","\\}","/" ,  "|" )
punctuations<-c(",",".","\\?","!",":",";","'")
amazon_datat1<-as.matrix(amazon_data[,1:10000])
colnames(amazon_datat1)=NULL

#plot the histgrams of the data frequency

hist_gram1<-matrix(c(1:10),nrow=20,ncol=1)
for(i in 1:20)
{for (j in 1:1500)
{hist_gram1[i,1]<-hist_gram1[i,1]+length(which(amazon_datat1[j,]==i))}}
hist_gram1<-t(hist_gram1)
colnames(hist_gram1)=c(1:20)
barplot(hist_gram1[1,],main = "histgram of the frequency in the dataset")

#extract the column names of the data

n<-matrix(0,nrow=1500,ncol=1)
colomn_name<-colnames(amazon_data)
char_count<-nchar(colomn_name)
copy<-amazon_data[,1:3558]
char_count1<-nchar(colomn_name[1:3558])

colomn_name1<-matrix(colomn_name,nrow=10001,ncol=1)

#compute the frequency of 1-20 words

frequency_words<-rowSums(copy[,1:3558])                                    #frequency 
frequency_words1<-matrix(frequency_words,nrow=1500,ncol=1)

#Simpson's D word richness computing

vocabulary_richness<-matrix(0,nrow=1500,ncol=1)                            #vocabulary richness
for (i in 1:1500)
{vocabulary_richness[i,1]<-1-sum(amazon_data[i,which(amazon_data[i,1:3558]!=0)]**2)/(sum(amazon_data[i,which(amazon_data[i,1:3558]!=0)])**2)}

#short words computation

short_words<-rowSums(copy[,which(char_count1<4)])            #short words

#bigrams and trigrams computing

word_bigram<-rowSums(amazon_data[,3559:5927])                      #word bigrams
word_trigram<-rowSums(amazon_data[,5927:6566])                    #word trigrams

colomn_name[grep("0|1|2|3|4|5|6|7|8|9",colomn_name)]       #get colomn name with digit number and filter manually 
grep("0|1|2|3|4|5|6|7|8|9",colomn_name)                    #get index
digit_number<-rowSums(amazon_data[,grep("0|1|2|3|4|5|6|7|8|9",colomn_name)[1:14]])   #compute digits data

#computing frequency of the data with each letter 

myLetters<-letters[1:26]                                      
Letters<-matrix(0,nrow=1500,ncol=26)
for(i in letters)
{ non_zero_letter<-which(str_count(colomn_name[1:10000],i)!=0)
letter_count<-as.array(str_count(colomn_name[1:10000],i)[non_zero_letter])
subset_data<-as.matrix(subset(amazon_data,select=non_zero_letter))
colnames(subset_data)<-NULL
Letters[,match(i,letters)]<-subset_data%*%letter_count
}

#computing the function words

function_words_count<-matrix(0,nrow=1500,ncol=15)
for( i in function_words)
{function_words_index<-which(str_count(colomn_name[1:10000],i)!=0)
function_data<-as.matrix(subset(amazon_data,select=function_words_index))
}
function_words_data<-rowSums(function_data)
#for(i in length(colnames(function_data)))
#{function_words_count[,i]<-rowSums(function_data)}         

for(i in LETTERS)
{upper_case_character<-which(str_count(colomn_name[1:10000],i)!=0)
upper_case_subset<-as.matrix(subset(amazon_data,select=upper_case_character))
}

#computing the special characters

special_character_count<-matrix(0,nrow=1500,ncol=19)
for (i in special_character)
{special_character_index<-which(str_count(colomn_name[1:10000],i)!=0)
special_character_subset<-as.matrix(subset(amazon_data,select=special_character_index))
special_character_count[,match(i,special_character)]<-rowSums(special_character_subset)
}
special_character_data<-rowSums(special_character_count)

#computing the punctuations

punctuations_count<-matrix(0,nrow=1500,ncol=7)
for (i in punctuations)
{punctuations_index<-which(str_count(colomn_name[1:10000],i)!=0)
punctuations_subset<-as.matrix(subset(amazon_data,select=punctuations_index))
punctuations_count[,match(i,punctuations)]<-rowSums(punctuations_subset)
}
punctuations_data<-rowSums(punctuations_count)

#computing n-grams from n=2 to n=5

n_gram_count<-matrix(0,nrow=1500,ncol=4)
for(i in 2:5)
{n_gram_index<-which(char_count[6567:10000]==i)
n_gram_subset<-as.matrix(subset(amazon_data,select=n_gram_index))
n_gram_count[,i-1]<-rowSums(n_gram_subset)
}

#combine all the features we obtain before

final_data<-NULL
final_data<-cbind(frequency_words1,vocabulary_richness)
final_data<-cbind(final_data,short_words,word_bigram,word_trigram)
final_data<-cbind(final_data,digit_number,Letters)
final_data1<-cbind(final_data,function_words_data,special_character_data,n_gram_count)
#special_character_data,punctuations_data,
#final_data is the data we get. the first col is words frequency, the second col is vocabulary richness (Simpson's D method), the next three 
#cols are short_words,word bigram and trigram respectively. The sixth col is the digit number, the 7-th to 32-nd col are letters from "a" to "z"
#33rd-47th col are functions words(15 function words), 48th-66th are special characters(19 special characters),67th-73rd are punctuations(7 punctuations)
#74th-77th are n_gram,from 2-5.

#final_data_1 is the data with reduced features of 1500reviews*38features

#KNN with averaging 

key1<-rep(0,1500)
for (author in 1:50)
{key1[author*30-(0:29)]=author}
new_data<-cbind(key1,amazon_datat1)

testing_data = matrix(0,150,10001)
training_data = matrix(0,27*50,10001)
for(author in 1:50)
{
  testing_data[author*3-(0:2),] = as.matrix(new_data[(1:3)+(author-1)*30,])
  training_data[author*27-(0:26),] = as.matrix(new_data[(4:30)+(author-1)*30,])
}
new_author_data<-matrix(0,nrow=50,ncol=10001)
for(author in 1:50)
{
  new_author_data[author,]<-colMeans(training_data[author*27-(0:26),])
}

test_result<-matrix(0,nrow=150,ncol=1)
raw_result<-matrix(0,nrow=150,ncol=50)
n<-matrix(0,nrow=50,ncol=1)
for(k in 1:50)
{testing_data1<-testing_data[1:(3*k),]
for (test in 1:(3*k))
{for (j in 1:k)
{raw_result[test,j]<-sum((testing_data1[test,-1]-new_author_data[j,-1])^2)}}
for (test in 1:(3*k))
{test_result[test,]<-which.min(raw_result[test,1:k])}
for(i in 1:(3*k))
{if (test_result[i]==testing_data1[i,1])
{n[k,1]<-n[k,1]+1}}
}
n[1:50,]<-n[1:50]/(3*(1:50))
plot(1:50,n[1:50,1],xlab="number of authors used",ylab="percentage correct",main="Percentage Correct vs Number of Authors")

#Initialize the data from the original data set: the preprocessed data is used below in the k-nn and SVM
data<- amazon_data[,1:10000]
global_number_of_features = 10000
feats = global_number_of_features
#Initialize a matrix to hold the k-fold cross validation run for each author
overall <- matrix(0,50,10)
#To test the accuracy for various numbers of authors
for(auth_num in 1:50){
  data<- data[,1:feats]
  
  #produce a key with the correct labels for testing
  key <- rep(0,(3*auth_num))
  
  #Initialize all of the order parameters
  xi_all <- matrix(0,auth_num,(3*auth_num))
  xi_all_saved = rep(0,(3*auth_num))
  
  #BEGIN THE SNN
  
  #set up the 10-fold cross validation
  testing = matrix(0,(3*auth_num),feats)
  training = matrix(0,27*auth_num,feats)
  v = matrix(0,auth_num,feats)
  v_plus <- matrix(0,auth_num,feats)
  for(k in 1:10){
    for(author in 1:auth_num){
      testing[author*3-(0:2),] = as.matrix(data[3*k-(0:2)+(author-1)*30,])
      testing = testing - rowMeans(testing)
      testing = testing/sqrt(rowSums(testing^2))
      training[author*27-(0:26),] = as.matrix(data[(1:30)+(author-1)*30,][-(3*k-(0:2)),])
      #Use the SCAP algorithm to intialize the prototype vectors
      #implement it by averaging over all the example samples from each author and subtracting off the mean
      v[author,] = colMeans(training[author*27-(0:26),])
      v[author,] = v[author,]- mean(v[author,])
      v[author,] = v[author,]/sqrt(sum(v[author,]^2))
      key[author*3-(0:2)] = author
    }
    
    #Orthogonalize the prototype vectors
    for(index in 1:auth_num){
      v_plus_orig = v[index,]
      v_plus[index,] = v[index,]
      for(other in 1:auth_num){
        if(other==index){}else{
          v_plus[index,] = v_plus[index,] - as.vector(t(v[other,])%*%v_plus_orig)*v[other,]/49}}
      v_plus[index,] = v_plus[index,]/as.vector(t(v[index,]%*%v_plus[index,]))
    }
    
    #TEST on the testing vectors
    q0 = testing
    xi_all = v_plus%*%t(q0)
    xi_all = xi_all/sqrt(rowSums(xi_all^2))
    B = 1 #setting the same constants as in the paper
    C = 1
    lambda = rep(C,auth_num)
    #evolution <- matrix(0,auth_num,500)
    gamma = 0.01 #gamma is the iteration speed
    for(iter in 1:(3*auth_num)){
      xi = xi_all[,iter]
      for(i in 1:500){
        D = (B+C)*sum(xi^2)
        xi = xi+gamma*(lambda-D+B*xi^2)*xi} #evolution equation for the order parameters
      #evolution[,i] <- xi
      xi_all_saved[iter] <- which.max(xi)} #pick out the highest order parameter
    accuracy <- xi_all_saved - key
    s = rep("green",(3*auth_num)) #used for color coding the plot, green is wrongly classified
    s[accuracy[]==0]="blue" #sets the correctly classified to blue
    overall[auth_num,k] <- sum(accuracy[]==0)/(3*auth_num)}
  print(auth_num,k)
  plot(1:(3*auth_num),xi_all_saved, col = s, xlab="Index of Order Parameter", ylab="Author Indicated")}
plot(1:50,rowMeans(overall), xlab='Number of Authors Used', ylab = 'Percentage Correct', main='Percentage Correct vs Number of Authors')

#plotting with errors bars of the standard deviation, courtesy of https://stackoverflow.com/questions/13032777/scatter-plot-with-error-bars
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
sdev <- colSd(t(overall))
avg <- rowMeans(overall)
plot(1:50, avg,pch=19,
     ylim=range(c(0, avg+sdev)), xlab='Number of Authors Used', ylab='Percentage Correct',
     main='Percentage Correct vs Number of Authors'
)
arrows(1:50, avg-sdev, 1:50, avg+sdev, length=0.05, angle=90, code=3)

#Now try the k-NN
key_train <- rep(0,50*27)
for(i in 1:50){
  key_train[27*i-(1:26)] <- i}
knn_correct = rep(0,50)
for(kn in 100){
  prc_test_pred <- knn(train = training, test = testing,cl = key_train, k=kn)
  #CrossTable(x = key, y = prc_test_pred, prop.chisq=FALSE)
  knn_correct[kn] = sum((as.numeric(prc_test_pred)-key)==0)
  print(knn_correct[kn])}

#Implement the SVM
key <- rep(0,1500)
for(author in 1:50){
  key[author*30-(0:29)] = author}
final_data1<- cbind(key,final_data1)
for(author_num in c(50,40,30,20,10,5)){
  data_svm <- final_data1[1:(1500-30*(50-author_num)),]
  set.seed(3033) 
  index_svm <- createDataPartition(y=data_svm$key, p=0.7,list=FALSE)
  training_svm <- data_svm[index_svm,]
  testing_svm <- data_svm[-index_svm,]
  dim(training_svm)
  dim(testing_svm)
  training_svm[["key"]] = factor(training_svm[["key"]])
  trctrl <- trainControl(method = 'repeatedcv',number = 10, repeats = 3)
  set.seed(3233)
  svm_linear <- train(key~., data=training_svm, method = 'svmLinear', preProcess = c('center','scale'), tuneLength = 10)
  #svm_linear
  test_pred <- predict(svm_linear, newdata = testing_svm)
  #test_pred
  print(author_num)
  print(sum((as.numeric(testing_svm$key)-as.numeric(test_pred))==0)/dim(testing_svm)[1])}

