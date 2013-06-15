#read data
path="/home/H24095016/SDM/KDD2009/"
data1<-read.table(paste(path,"train.chunk1",sep=""),header=TRUE,sep="\t")
data2<-read.table(paste(path,"train.chunk2",sep=""),header=TRUE,sep="\t")
data3<-read.table(paste(path,"train.chunk3",sep=""),header=TRUE,sep="\t")
data4<-read.table(paste(path,"train.chunk4",sep=""),header=TRUE,sep="\t")
data5<-read.table(paste(path,"train.chunk5",sep=""),header=TRUE,sep="\t")
data<-as.data.frame(cbind(data1,data2,data3,data4,data5))

data[data==""] <- NA

#filter data: remove variables with single level
n=NULL
for (i in 1:dim(data)[2]){ if (nlevels(factor(data[,i]))!=1) { n[i]=i }	}
nn=which(is.na(n))
name=na.omit(names(data)[n])
datanew=subset(data,select=name)
dim(datanew)

#Rename datanew-> data
data=datanew

#seperate numerical and categorical data
num=NULL   #numerical var index
cat=NULL   #categorical var index
for(i in 1:dim(data)[2]){
	if (is.numeric(data[,i])==TRUE){num=cbind(num,i)}
		else {cat=cbind(cat,i)}
	}
datanum=data[,num] #numerical data
datacat=data[,cat] #categorical data

##numerical data
#split numerical data into 3 groups according to the NA's ratio
#abandon variables that consist of 95% and above's NA
dim(datanum)
nA<-nB<-nC<-NULL
A<-B<-C<-NULL
ratio1=0
ratio2=0.05
ratio3=0.95
	for(i in 1:dim(datanum)[2]){
	if((sum(is.na(datanum[,i]))/length(datanum[,i]))==ratio1) {
		A<-cbind(A,datanum[,i])
		nA<-rbind(nA,i)
		}
	if((sum(is.na(datanum[,i]))/length(datanum[,i]))<=ratio2 && (sum(is.na(datanum[,i]))/length(datanum[,i]))>ratio1) {
		B<-cbind(B,datanum[,i])
		nB<-rbind(nB,i)
		}
	if((sum(is.na(datanum[,i]))/length(datanum[,i]))<=ratio3 && (sum(is.na(datanum[,i]))/length(datanum[,i]))>ratio2) {
		C<-cbind(C,datanum[,i])
		nC<-rbind(nC,i)
		}
	}	
colnames(A)<-colnames(datanum[,nA])
colnames(B)<-colnames(datanum[,nB])
colnames(C)<-colnames(datanum[,nC])

##categorical data
#replace all blank entries by NA
as.data.frame(datacat,stringsAsFactors = TRUE)

#split categorical data into 3 groups according to the NA's ratio
dim(datacat)
nD<-nE<-nF<-NULL
D<-E<-F<-NULL
ratio1=0
ratio2=0.05
ratio3=0.95
	for(i in 1:dim(datacat)[2]){
	if((sum(is.na(datacat[,i]))/length(datacat[,i]))==ratio1) {
		D<-as.data.frame(cbind(D,datacat[,i],deparse.level = 1),stringsAsFactors = TRUE)
		nD<-rbind(nD,i)
		}
	if((sum(is.na(datacat[,i]))/length(datacat[,i]))<=ratio2 && (sum(is.na(datacat[,i]))/length(datacat[,i]))>ratio1) {
		E<-as.data.frame(cbind(E,datacat[,i],deparse.level = 1),stringsAsFactors = TRUE)
		nE<-rbind(nE,i)
		}
	if((sum(is.na(datacat[,i]))/length(datacat[,i]))<=ratio3 && (sum(is.na(datacat[,i]))/length(datacat[,i]))>ratio2) {
		F<-as.data.frame(cbind(F,datacat[,i],deparse.level = 1),stringsAsFactors = TRUE)
		nF<-rbind(nF,i)
		}
	}	
colnames(D)<-colnames(datacat[,nD])
colnames(E)<-colnames(datacat[,nE])
colnames(F)<-colnames(datacat[,nF])


##A & D(non NA) 

##imput B & E (NA less than 5%)
#use mean for numerical; use mode for categorical
install.packages("imputation")
library("imputation")
meanImpute(B)$x[2,]

imputMode<-function(E){
	Mode <- function(x) {
 	ux <- na.omit(unique(x))
  	ux[which.max(tabulate(match(x, ux)))]
	}
	for (j in 1:dim(E)[2]){
	E[which(is.na(E[,j])),j]<-Mode(E[,j])
	}
	E
	}
E=imputMode(E)

#imput C & F (NA more than 5% but less than 95%)
#use regression tree (use A,B,D,E to regress C & F)
install.packages("rpart")
library("rpart")


	Xnum=as.data.frame(cbind(A,D,E))
	Yhat=matrix(NA,dim(C)[1],dim(C)[2])
	newcolnames=paste("H",colnames(C),sep="")
	colnames(Yhat)<-newcolnames
	for (i in 1:dim(C)[2]){
	nNA=which(is.na(C[,i]))
	yXnum=as.data.frame(cbind(C[,i][-nNA],Xnum[-nNA,]))
	Xnump=as.data.frame(Xnum[nNA,])
	names(yXnum)[1]="y"
	ntree=rpart(y~.,data=yXnum,control=rpart.control(minsplit=200))
	Yhat[nNA,i]=predict(ntree,newdata=Xnump)
	Yhat[-nNA,i]=C[-nNA,i]
	}

	Xcat=as.data.frame(cbind(A,D,E,deparse.level = 1),stringsAsFactors = TRUE)
	Zhat=matrix(NA,dim(F)[1],dim(F)[2])
	newcolnames2=paste("H",colnames(F),sep="")
	colnames(Zhat)<-newcolnames2
	for (i in 1:dim(F)[2]){
	nNA=which(is.na(F[,i]))
	yXcat=as.data.frame(cbind(F[,i][-nNA],Xcat[-nNA,],deparse.level = 1),stringsAsFactors = TRUE)
	Xcatp=as.data.frame(Xcat[nNA,])
	names(yXcat)[1]="y"
	ctree=rpart(factor(y)~.,data=yXcat,control=rpart.control(minsplit=200),method="class")
	Zhat[nNA,i]=predict(ctree,newdata=Xcatp,type="class")
	Zhat[-nNA,i]=F[-nNA,i]
	}


#Complete Data
#A,B,C=Yhat,D,E,F=Zhat

dataX=cbind(A,B,C=Yhat,D,E,F=Zhat)

