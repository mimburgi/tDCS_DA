library(irr)

setwd("C:/Users/Mike/Downloads")

ebr1<-read.csv("tDCS-DA EBR Data_ Counting - B.csv", header = TRUE, na.strings = c("x", " "))
ebr2<-read.csv("tDCS-DA EBR Data_ Counting - A.csv", header = TRUE, na.strings = c("x", " "))

#make a dataframe of just change score numbers from each sheet
ebrchange<-as.data.frame(cbind(ebr1$Change.Score.2, ebr2$Change.Score))
#make a dataframe of just total blink numbers from each sheet
ebrtotal<-as.data.frame(cbind(ebr1$Blink.Total, ebr2$Blink.Total))
#get rid of the rows that are entirely NA values
ebrchange<-ebrchange[!is.na(ebrchange$V1),]
ebrtotal<-ebrtotal[!is.na(ebrtotal$V1),]


kappa2(ebrchange)
kappa2(ebrtotal)
