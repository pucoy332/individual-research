

# Script to create Manhattan and QQ plots (including lambda value).

# Get the qqman library (install if needed before: install.packages("qqman")

library('qqman')

# Indicate working directory (with the slash at the end)

wd <- "C:/Users/user/Desktop/GWAS(2)/"

# Indicate the name of the file (without extension). The file will contain the chromosome, base pair position, and p-value. No headers and no missing values.

var<- "ld20.adclean.cont.linear"

# Get data and change format

data<-NULL
data<- read.table(file(paste0(wd, var,".txt")), header = F,stringsAsFactors=F)
colnames(data)<-c("CHROM","POS","PVALUE")

data$CHROM[which(data$CHROM=="X")]<-23
data$CHROM<-as.numeric(data$CHROM)
data$POS<-as.numeric(data$POS)
data$PVALUE<-as.numeric(data$PVALUE)

# Make Manhattan plot

png(filename = paste0(wd,"Manhattan", var,".png"), width = 31, height = 21, units="cm", res=500, type="cairo")
manhattan(x = data, chr = "CHROM", bp = "POS", p = "PVALUE",  col = c("darkblue","lightblue"), chrlabs = c(1:22))
dev.off()

# Make QQ plot including lambda

png(filename = paste0(wd,"QQ", var,".png"), width = 31, height = 31, units="cm", res=500, type="cairo")
alpha<-median(qchisq(1-data$PVALUE,1))/qchisq(0.5,1)
qq(data$PVALUE)
text(0.5,4, paste("lambda","=",  signif(alpha, digits = 3)) )
dev.off()
