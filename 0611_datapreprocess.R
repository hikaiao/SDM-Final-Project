
#Run Time 
tic <- Sys.time()

################Plx debug
#read data
path="~/Desktop/sdm/projectdata/"
data1<-read.table(paste(path,"train.chunk1",sep=""),header=TRUE,sep="\t")
data2<-read.table(paste(path,"train.chunk2",sep=""),header=TRUE,sep="\t")
data3<-read.table(paste(path,"train.chunk3",sep=""),header=TRUE,sep="\t")
data4<-read.table(paste(path,"train.chunk4",sep=""),header=TRUE,sep="\t")
data5<-read.table(paste(path,"train.chunk5",sep=""),header=TRUE,sep="\t")
data<-as.data.frame(cbind(data1,data2,data3,data4,data5))
################Plx debug


#######Save Point1
#write.table(data,file='point1.csv',header=TRUE,sep="\t")
###################

data[data==""] <- NA
#filter data: remove variables with single level
n=NULL
for (i in 1:dim(data)[2]){ if (nlevels(factor(data[,i]))!=1) { n[i]=i }	}
nn=which(is.na(n))
name=na.omit(names(data)[n])
datanew=subset(data,select=name)

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


#######Save Point2
#write.table(datanum,file='point2-1.csv',header=TRUE,sep="\t")
#write.table(datacat,file='point2-2.csv',header=TRUE,sep="\t")
###################


##numerical data
#split numerical data into 3 groups according to the NA's ratio
#abandon variables that consist of 95% and above's NA
A<-B<-C<-NULL
A<-lapply(datanum,function(x){
	if(sum(is.na(x))/length(x)==0){
		x}})
B<-lapply(datanum,function(x){
	if(sum(is.na(x))/length(x)<=0.05 && sum(is.na(x))/length(x)>0){
		x}})
C<-lapply(datanum,function(x){
	if(sum(is.na(x))/length(x)<=0.95 && sum(is.na(x))/length(x)>0.05){
		x}})


##FIND NOT NULL OBJECT VAR
true.data<-function(x){
	count<-NULL
	for(i in 1:length(x)){
		if(length(x[names(x)[i]][[1]])!=0){
			count<-rbind(count,names(x)[i])
			}
	}
	count
}
numA<-true.data(A)
numB<-true.data(B)
numC<-true.data(C)
A<-datanum[,numA]
B<-datanum[,numB]
C<-datanum[,numC]

##categorical data
#replace all blank entries by NA
as.data.frame(datacat,stringsAsFactors = TRUE)

#split categorical data into 3 groups according to the NA's ratio (the same as before process)
#dim(datacat)
####
D<-E<-F<-NULL
D<-lapply(datacat,function(x){
	if(sum(is.na(x))/length(x)==0){
		x}})
E<-lapply(datacat,function(x){
	if(sum(is.na(x))/length(x)<=0.05 && sum(is.na(x))/length(x)>0){
		x}})
F<-lapply(datacat,function(x){
	if(sum(is.na(x))/length(x)<=0.95 && sum(is.na(x))/length(x)>0.05){
		x}})
		
##FIND NOT NULL OBJECT VAR
true.data<-function(x){
	count<-NULL
	for(i in 1:length(x)){
		if(length(x[names(x)[i]][[1]])!=0){
			count<-rbind(count,names(x)[i])
			}
	}
	count
}
numD<-true.data(D);numE<-true.data(E);numF<-true.data(F);
D<-as.data.frame(datacat[,numD],stringsAsFactors = TRUE);
E<-as.data.frame(datacat[,numE],stringsAsFactors = TRUE);
F<-as.data.	frame(datacat[,numF],stringsAsFactors = TRUE);

##A & D(non NA) 

##imput B & E (NA less than 5%)
#use mean for numerical; use mode for categorical
install.packages("imputation")
library("imputation")

##B?
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
E

#######Save Point3
#write.table(A,file='point3-1.csv',header=TRUE,sep="\t")
#write.table(B,file='point3-2.csv',header=TRUE,sep="\t")
#write.table(C,file='point3-3.csv',header=TRUE,sep="\t")
#write.table(D,file='point3-4.csv',header=TRUE,sep="\t")
#write.table(E,file='point3-5.csv',header=TRUE,sep="\t")
#write.table(F,file='point3-6.csv',header=TRUE,sep="\t")
###################

#imput C & F (NA more than 5% but less than 95%)
#use regression tree (use A,B,D,E to regress C & F)
install.packages("rpart")
library("rpart")

#Xnum=as.data.frame(cbind(A,D,E)) <-- MISSING B
	Xnum=as.data.frame(cbind(A,B,D,E))
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
	
#######Save Point4
#write.table(Yhat,file='point4-1.csv',sep="\t")

#MISSING B
	Xcat=as.data.frame(cbind(A,B,D,E,deparse.level = 1),stringsAsFactors = TRUE)
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

#######Save Point5
#write.table(Zhat,file='point5-1.csv',sep="\t")

#Complete Data
#A,B,C=Yhat,D,E,F=Zhat

dataX=cbind(A,B,C=Yhat,D,E,F=Zhat)

#######Save Point6
write.table(dataX,file='point6.csv',sep="\t")
# Run time End
toc <- Sys.time()
comp.time <- toc - tic
comp.time 


