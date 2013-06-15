
##combining large data 
# large.data1<-read.table('~/Desktop/sdm/projectdata/large_train/orange_large_train.data.chunk1',header=TRUE,sep="\t")
# large.data2<-read.table('~/Desktop/sdm/projectdata/large_train/orange_large_train.data.chunk2',header=TRUE,sep="\t")
# large.data3<-read.table('~/Desktop/sdm/projectdata/large_train/orange_large_train.data.chunk3',header=TRUE,sep="\t")
# large.data4<-read.table('~/Desktop/sdm/projectdata/large_train/orange_large_train.data.chunk4',header=TRUE,sep="\t")
# large.data5<-read.table('~/Desktop/sdm/projectdata/large_train/orange_large_train.data.chunk5',header=TRUE,sep="\t")
# data<-data.frame(rbind(large.data1,large.data2,large.data3,large.data4,large.data5))

##small data
data<-read.table('~/Desktop/sdm/projectdata/orange_small_train.data',header=TRUE,sep="\t")
##Category A
a<-matrix(NA,nrow=3)
cbind(a,c(1,2,3))
dim(data)[2]

##Slice function
slice<-function(data,ratio){
	outputdata<-NULL
	j<-NULL
for(i in 1:dim(data)[2]){
	if((sum(is.na(data[,i]))/length(data[,i]))<=ratio) {
				outputdata<-cbind(outputdata,data[,i])
				j<-c(j,i)
			}
	}
	colnames(outputdata)<-colnames(data[,j])
	outputdata
}

#CategoryA (NOT ANY MISSING VALUE)
colnames(data_A)
data_A<-slice(data,0)
#CategoryB (NO MORE THAN 5% MISSING VALUE)
data_B<-slice(data,0.05))
#CategoryC (AT LEAST 95% MISSING VALUE)
data_B<-slice(data,0.95))


##Large data slice 
big_data_A<-NULL
big_data_B<-NULL
big_data_C<-NULL

for(i in 1:5){
	large.data<-NULL
	#ALL THE LARGE DATA SLICE MUST BE IN THE SAME DOCUMENT
	data_site<-paste(c("~/Desktop/sdm/projectdata/large_train/orange_large_train.data.chunk",i),collapse="")
	large.data<-read.table(data_site,header=TRUE,sep="\t")
	big_data_A<-cbind(big_data_A,slice(large.data,0))
	big_data_B<-cbind(big_data_A,slice(large.data,0.05))
	big_data_C<-cbind(big_data_A,slice(large.data,0.95))
}