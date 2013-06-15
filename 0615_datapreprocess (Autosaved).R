# E: no NA
# F: NA<10%
# G: NA 10~95%
# H: NA >95%
# Goal
# 某一level 出現 1-250次 group 成一組
# 250-499 group 成一組
# 500- 999 group 成一組
# 出現1000以上 保留巨原本及level name
~/Desktop/sdm/Save Point/point3-7.csv
dataE<-read.table("~/Desktop/sdm/Save Point/point4-5.csv",sep="\t")
dataF<-read.table("~/Desktop/sdm/Save Point/point4-6.csv",sep="\t")
dataG<-read.table("~/Desktop/sdm/Save Point/point3-7.csv",sep="\t")
dataH<-read.table("~/Desktop/sdm/Save Point/point3-8.csv",sep="\t")
dim(dataE);dim(dataF);dim(dataG);dim(dataH)

##Function
cat.combine<-function(dataE,gp1name,gp2name,gp3name){
data_E<-NULL
for(j in 1:dim(dataE)[2]){
	x<-dataE[,j]
	lnames<-levels(x)
	x<-as.matrix(x)
	for(i in 1:length(lnames)){
	check<-which(x==lnames[i])
	if(length(check)<250 && length(check)>=0){
		x[check]<-gp1name
		}else if(length(check)<500 && length(check)>=250){
		x[check]<-gp2name
		}else if(length(check)<1000 && length(check)>=500){
		x[check]<-gp3name
		}
		}
		data_E<-cbind(data_E,x)
		}
		as.data.frame(data_E)
}

data_E<-cat.combine(dataE,"gp1","gp2","gp3")
data_F<-cat.combine(dataF,"gp1","gp2","gp3")
data_G<-cat.combine(dataG,"gp1","gp2","gp3")
data_H<-cat.combine(dataH,"gp1","gp2","gp3")


