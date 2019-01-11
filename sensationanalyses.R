setwd("C://Users//Mike//Desktop//DAdat")
sensation<-read.csv("sensation.csv", stringsAsFactors = FALSE)
sensation<-subset(sensation, Subject.Number != 44 & Subject.Number != 47 & Subject.Number != 50)

anodalsubnums<-c(34,36,38,40,42,46,48,50,52,55,56,58,60,62,64,66)

sensation$condition<-"S"
sensation$condition[sensation$Subject.Number %in% anodalsubnums]<-"A"
sensation$totalsensation<-rowMeans(sensation[,c("Itching", "Pain", "Burning", "Warmth.Heat", "Pinching", "Metallic.Iron.Taste", "Fatigue")])
t.test(Itching ~ condition, data = sensation)
t.test(Pain ~ condition, data = sensation)
t.test(Burning ~ condition, data = sensation)
t.test(Pinching ~ condition, data = sensation)
t.test(Warmth.Heat ~ condition, data = sensation)
t.test(Metallic.Iron.Taste ~ condition, data = sensation)
t.test(Fatigue ~ condition, data = sensation)

# chi sq
sensationtable<-table(sensation$ShamDetect, sensation$condition)
chisq.test(sensationtable)
