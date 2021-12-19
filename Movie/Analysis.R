
setwd("/Users/hiroki-tan/Dropbox (NAIST AHC)/work/SST2020/SRS_Agent/Movie/")

getwd()

X<-read.table("Data.csv",sep=",",header=TRUE)
dim(X)

###
male<-subset(X,X[,4]=="男性")
female<-subset(X,X[,4]=="女性")

high_age<-subset(X,X[,3]>=45)
low_age<-subset(X,X[,3]<45)


###

Age<-X[,3]

hist(female[,3],xlim=c(18,70),col="#ff00ff40",cex.lab=1.3,cex.axis=1.3,ylab="Number of Participants",main="",xlab="Ages",breaks = "Scott",add=FALSE)
hist(male[,3],xlim=c(18,70),col="#0000ff40",density=30,cex.lab=1.3,cex.axis=1.3,breaks = "Scott",add=TRUE)
legend("topright",legend=c("Females\nn=157\n","Males\nn=148\n"),col=c("#ff00ff40","#0000ff40"),pch=15,cex=1.3,box.lty = 0)


####################
##

Data1<-X #male
dir=""

# loop, 5-17 5: Acceptability as a Trainer, 6: : Acceptability as a Listener, 7: Realism, 8, Eeriness, 9: Trustworthiness, 10: Eye, 11: Face, 12: Fair, 13: Voice, 14: Age, 15: Clothes, 16: Likeability, 17: Familiarity

point=5

start=point
title="Familiarity"


Char<-Data1[,start+13]
Char<-cbind(Char,Data1[,start])


boxplot(Char,ylim=c(1,5),col=0,names=c("(h)", "(i)"),xlab="Character",main=title,cex.names=2.0,cex=2.0,cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
#dev.off()

wilcox.test(Char[,1],Char[,2],paired=TRUE)


shapiro.test(Char[,1])

# Kruskal-Wallis rank sum test
kruskal.test(x=list(Char[,1],Char[,2]))

