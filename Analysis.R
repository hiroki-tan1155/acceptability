
setwd("/Users/hiroki-tan/Dropbox (NAIST AHC)/work/SST2020/SRS_Agent/")

getwd()

X<-read.table("Ori_Data3.csv",sep=",",header=TRUE)

dim(X)

all<-read.table("Ori_Data3.csv",sep=",",header=TRUE)

###
male<-subset(X,X[,4]=="男性")
female<-subset(X,X[,4]=="女性")

high_age<-subset(X,X[,3]>=45)
low_age<-subset(X,X[,3]<45)

#high_SRS<-subset(X,(X[,4]=="男性" & X[,171]>=65) | (X[,4]=="女性" & X[,171]>=52))
#low_SRS<-subset(X,(X[,4]=="男性" & X[,171]<65) | (X[,4]=="女性" & X[,171]<52))

high_SRS<-subset(X, X[,171]>81) #81
low_SRS<-subset(X, X[,171]<=81)

high_TAS<-subset(X,X[,172]>58)
low_TAS<-subset(X,X[,172]<=58)

###


#data boundary=89-90

x<-X[,169]
y<-X[,160]

t.test(x,y,paired=TRUE)
wilcox.test(x,y,paired=TRUE)

boxplot(x,y)

Age<-X[,3]

hist(female[,3],xlim=c(18,70),col="#ff00ff40",cex.lab=1.3,cex.axis=1.3,ylab="Number of Participants",main="",xlab="Ages",breaks = "Scott",add=FALSE)
hist(male[,3],xlim=c(18,70),col="#0000ff40",density=30,cex.lab=1.3,cex.axis=1.3,breaks = "Scott",add=TRUE)
legend("topright",legend=c("Females\nn=157\n","Males\nn=148\n"),col=c("#ff00ff40","#0000ff40"),pch=15,cex=1.3,box.lty = 0)

## SRS and TAS ##

SRS<-X[,171]
TAS<-X[,172]


hist(SRS,xlim=c(0,180),col="tan1",cex.lab=1.3,cex.axis=1.3,ylab="Number of Participants",main="",breaks = "Scott",xlab="Social Responsiveness Scale")

hist(TAS,xlim=c(20,100),col="tan1",cex.lab=1.3,cex.axis=1.3,ylab="Number of Participants",main="",breaks = "Scott",xlab="Toronto Alexithymia Scale")



cor.test(TAS,SRS,method="spearman")

plot(SRS,TAS,pch=20,cex.lab=1.3,cex.axis=1.3,ylab="Toronto Alexithymia Scale",xlab="Social Responsiveness Scale")
result <- lm(TAS ~ SRS) 
abline(result) 

text(140, 30, "rho=0.67***",cex=1.5)

t.test(male[,171],female[,171])

####################
##

Data1<-low_age #male, low_SRS, low_age, X
dir=""

# loop, 90-98 90: Acceptability as a Trainer, 91: Acceptability as a Listener, 92: Eye, 93: face, 94: hair, 95: voice, 96: age, 97: Likeability, 98: Familiarity ##

point=91

start=point
title="Acceptability"
#name="Likeability"

Char<-Data1[,start]
for (i in 1:8){
	Char<-cbind(Char,Data1[,start+9])
	start=start+9
}

#finame=paste(dir,"/",name,".eps",sep="")
#postscript(finame, horizontal = FALSE, onefile = FALSE)
boxplot(Char,ylim=c(1,5),col=0,names=c("(a)", "(b)", "(c)","(d)","(e)","(f)","(g)", "(h)", "(i)"),xlab="Character",main=title,cex.names=2.0,cex=2.0,cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
#dev.off()

wilcox.test(Char[,1],Char[,3],paired=TRUE)

shapiro.test(Char[,1])

# Kruskal-Wallis rank sum test
kruskal.test(x=list(Char[,1],Char[,2],Char[,3],Char[,4],Char[,5],Char[,6],Char[,7],Char[,8],Char[,9]))

# correlation to SRS and age
SRS<-Data1[,171]
cor.test(Char[,1],SRS,method="spearman")   #Age, SRS, TAS

plot(Char[,6],SRS)

##

##
Data2<-high_age
dir="low_SRS"

# loop, 90-98 90: Acceptability as a trainer, 91: Acceptability as a listener, 92: eye, 93: face, 94: hair, 95: voice, 96: age, 97: Likable, 98: Familiarity ##

start=point

Char2<-Data2[,start]
for (i in 1:8){
	Char2<-cbind(Char2,Data2[,start+9])
	start=start+9
}

#finame=paste(dir,"/",name,".eps",sep="")
#postscript(finame, horizontal = FALSE, onefile = FALSE)
boxplot(Char2,ylim=c(1,5),col=0,names=c("(a)", "(b)", "(c)","(d)","(e)","(f)","(g)", "(h)", "(i)"),xlab="Character",main=title,cex.names=1.3,cex=1.3,cex.lab=1.3)
#dev.off()

wilcox.test(Char2[,1],Char2[,3],paired=TRUE)

##


character_name=1
wilcox.test(Char[,character_name],Char2[,character_name])

mean(Char[,character_name])
boxplot(Char[,character_name],Char2[,character_name],ylim=c(1,5),col=0,names=c("High SRS", "Low SRS"),xlab="",main=title,cex.names=2.0,cex=2.0,cex.lab=2.0,cex.axis=2.0,cex.main=2.0)

sum(Char)/(dim(Char)[1]*9)


### all correlation plots ###

start2_1=90
start2_2=91
start2_3=92
start2_4=93
start2_5=94
start2_6=95
start2_7=96
start2_8=97
start2_9=98

Cor1<-X[,start2_1]
Cor2<-X[,start2_2]
Cor3<-X[,start2_3]
Cor4<-X[,start2_4]
Cor5<-X[,start2_5]
Cor6<-X[,start2_6]
Cor7<-X[,start2_7]
Cor8<-X[,start2_8]
Cor9<-X[,start2_9]


for (i in 1:8){
	Cor1<-append(Cor1,X[,start2_1+9])
	start2_1=start2_1+9
	Cor2<-append(Cor2,X[,start2_2+9])
	start2_2=start2_2+9
	Cor3<-append(Cor3,X[,start2_3+9])
	start2_3=start2_3+9
	Cor4<-append(Cor4,X[,start2_4+9])
	start2_4=start2_4+9
	Cor5<-append(Cor5,X[,start2_5+9])
	start2_5=start2_5+9
	Cor6<-append(Cor6,X[,start2_6+9])
	start2_6=start2_6+9
	Cor7<-append(Cor7,X[,start2_7+9])
	start2_7=start2_7+9
	Cor8<-append(Cor8,X[,start2_8+9])
	start2_8=start2_8+9
	Cor9<-append(Cor9,X[,start2_9+9])
	start2_9=start2_9+9
}


cor.test(Cor1,Cor2,method="spearman")

# matrix
library(corrplot)
c<-cbind(Cor1,Cor2,Cor3,Cor4,Cor5,Cor6,Cor7,Cor8,Cor9)
c2<-cor(c,method="spearman")
par(family="HiraKakuPro-W3")
colnames(c2)<-c("Trainer","Listener","Eye","Face","Hair","Voice","Age","Likeability","Familiarity")
rownames(c2)<-c("Trainer","Listener","Eye","Face","Hair","Voice","Age","Likeability","Familiarity")
corrplot(c2, method = "color", addCoef.col = TRUE)

############################

## Realism (only female characters) * SRS: 2 way ANOVA, likable

Char1_hSRS=Char[,2]
Char1_lSRS=Char2[,2]

Char2_hSRS=Char[,1]
Char2_lSRS=Char2[,1]

Char3_hSRS=Char[,7]
Char3_lSRS=Char2[,7]

a1=c(Char[,2],Char[,1],Char[,7])
a2=c(Char2[,2],Char2[,1],Char2[,7])

# ANOVA
bunsan3<-data.frame(A=factor(c(rep("a1",339),rep("a2",576))),B=factor(c(rep("b1",113),rep("b2",113), rep("b3",113),rep("b1",192),rep("b2",192), rep("b3",192))),y=c(a1,a2))

bunsan3
#summary(aov(y~A+B,data=bunsan3))
summary(aov(y~A*B,data=bunsan3))

attach(bunsan3)
interaction.plot(A,B,y)


## correlation, full search ##


#for (category in c("SRS","TAS","Age","Gender")){

category="SRS"

name_list=""
r_list=""

if(category=="Gender"){
	Data1<-male
	Data2<-female
}else if(category=="Age"){
	Data1<-high_age
	Data2<-low_age
}else if(category=="SRS"){
	Data1<-high_SRS
	Data2<-low_SRS
}else if(category=="TAS"){
	Data1<-high_TAS
	Data2<-low_TAS
}

for (measure in 90:98){

	start=measure
	Char<-Data1[,start]
	for (i in 1:8){
		Char<-cbind(Char,Data1[,start+9])
		start=start+9
	}
	
	start=measure
	Char2<-Data2[,start]
	for (i in 1:8){
		Char2<-cbind(Char2,Data2[,start+9])
		start=start+9
	}

	for(character_name in 1:9){

		result<-wilcox.test(Char[,character_name],Char2[,character_name])
		pval <- result$p.value
		z <- qnorm(1-(pval/2))
		r <- z/sqrt(length(Char[,character_name])+length(Char2[,character_name]))
		
		name=paste(category,"-",measure,"-",character_name,sep="")
		name_list=append(name_list,name)
		r_list=append(r_list,r)
	}
}

#}


df<-data.frame(name_list,r_list,stringsAsFactors=FALSE)
T<-head(df[order(df$r_list,decreasing=TRUE),],n=20) #50: p=0.03

