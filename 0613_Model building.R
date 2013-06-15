# #####################################Testing Data Set (IRIS)
# data<-data(iris)
# colnames(iris)[1]<-"y"
# names(iris)
# #RandomForest
# dim(data)
# data<-iris[-(1:50),]
# data[,5]<-factor(iris[-(1:50),5],levels=c("versicolor","virginica"))
# rownames(data)<-1:100
# lastvarnum<-dim(data)[2]
# colnames(data)[dim(data)[2]]<-"y"
# testnum<-sample(1:100,10)
# testdata1<-data[testnum,]
# testdata<-testdata1[,-lastvarnum]
# traindata<-testdata1[-testnum,]"
#####################################testing Data Set (IRIS)
path="/home/H24095016/SDM/KDD2009/"
y<-read.table(paste(path,"orange_large_train_appetency.labels"))
#orange_large_train_appetency.labels
#orange_large_train_churn.labels
#orange_large_train_toy.labels
randomnum<-sample(1:dim(data)[1],dim(data)[1]*0.3)
traindata<-data[-randomnum,]
testdata<-data[randomnum,]
install.packages("randomForest")
library(randomForest)
r.f<-randomForest(factor(y)~.,data=traindata,importance=TRUE,proximity=TRUE,na.action=na.omit)
#names(r.f)
##Predict Power
p.r.f<-predict(r.f,newdata=testdata[,-dim(testdata)[2]])
#varImpPlot(r.f)
#importance(r.f)
predictlabel<-as.vector(p.r.f)


##error rate function

errfinal<-function(predictdata,testdata,labels,col){
	label1<-labels[1]
	label2<-labels[2]
	b<-c(0,0,0,0)
	for(i in 1:length(predictdata)){
	if(predictdata[i]==label1 && as.vector(testdata[,col])[i]==label1){
	   b[1] = b[1]+1	
	}else if(predictdata[i]==label1 && as.vector(testdata[,col])[i]==label2){
		b[2] = b[2]+1
	}else if(predictdata[i]==label2 && as.vector(testdata[,col])[i]==label1){
		b[3] = b[3]+1
	}else if(predictdata[i]==label2 && as.vector(testdata[,col])[i]==label2){
		b[4] = b[4]+1
	}
}
b<-b/length(predictlabel)
b}

tablefinal<-function(x,classname){
   table.out=as.data.frame(matrix(NA,nrow=3,ncol=2))
   colnames(table.out)=c("Predicted"," ")
   rownames(table.out)=c("True",classname)
   table.out[1,]=classname
   table.out[2:3,1:2]=  round(x,1)
   table.out
}

lastvarnum<-dim(data)[2]
##test error rate table
a<-errfinal(predictlabel,testdata1,labels=c(1,-1),lastvarnum)
testerrtable<-tablefinal(a,r.f$classes)
testerrtable

##train error rate table
b<-errfinal(r.f$predict,traindata,labels=c(1,-1),lastvarnum)
trainerrtable<-tablefinal(b,r.f$classes)
testerrtable

###importance plot (only the firt 20 important variable)
r.f.i<-importance(r.f)
colnames(r.f.i)
impFinal<-r.f.i[,4]
sort(impFinal)
jpeg(file = "output1.jpg")
barplot(sort(impFinal[1:20],TRUE),horiz=TRUE,col="red",border="red")
dev.off()

jpeg(file = "output2.jpg")
varImpPlot(r.f)
dev.off()
