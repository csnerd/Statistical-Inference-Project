install.packages('tinytex')
tinytex::install_tinytex()
library(ggplot2)
library(tinytex)

tinytex::pdflatex('simulationPart1')


set.seed(28) ##to ensure reproducibility, I am setting seed arbitrary on 28.

lambda <- 0.2
exp_means =NULL
for (i in 1 : 1000) 
  exp_means = c(exp_means , mean(rexp(40, 0.2)))


hist(exp_means)

simMeanMean <- mean(exp_means)
# theoretical exponential mean
theMean <- 1/lambda

abline(v=theMean, col="red", lwd=3)


hist(exp_means, breaks=20, prob=TRUE, xlab="Mean of exponentials", ylab="Frequency", 
     col="IndianRed3")
curve(dnorm(x, mean=mean(exp_means), sd=sd(exp_means)), col="blue", lwd=2, 
      lty = "dotted", add=TRUE, yaxt="n")
curve(dnorm(x, mean=5, sd=0.79), col="black", lwd=2, add=TRUE, yaxt="n")




dose5 <- filter(ToothGrowth, ToothGrowth$dose == 0.5) 
dose1 <- filter(ToothGrowth, ToothGrowth$dose == 1.0) 
dose2 <- filter(ToothGrowth, ToothGrowth$dose == 2.0)

OJ<-subset(ToothGrowth, ToothGrowth$supp == "OJ")
VC<-subset(ToothGrowth, ToothGrowth$supp == "VC")

cat("variance for OJ supp. :",var(OJ$len))

cat(";    variance for VC supp. :",var(VC$len))

cat(";    variance for dose 0.5 :",var(dose5$len))

cat(";   variance for dose 1 :",var(dose1$len))

cat(";    variance for dose 2 :",var(dose2$len))

t.test(OJ$len,VC$len, var.equal = F, paired = F)

t.test(dose5$len,dose1$len, var.equal = T, paired = F)

t.test(dose5$len,dose2$len, var.equal = F, paired = F)

t.test(dose1$len,dose2$len, var.equal = F, paired = F)

OJ5<-subset(ToothGrowth, ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5)
VC5<-subset(ToothGrowth, ToothGrowth$supp == "VC" & ToothGrowth$dose == 0.5)


cat("variance for OJ supp. :",var(OJ5$len))

cat(";    variance for VC supp. :",var(VC5$len))


t.test(VC5$len, OJ5$len, paired=F, var.equal = F)

OJ1<-subset(ToothGrowth, ToothGrowth$supp == "OJ" & ToothGrowth$dose == 1)
VC1<-subset(ToothGrowth, ToothGrowth$supp == "VC" & ToothGrowth$dose == 1)


cat("variance for OJ supp. :",var(OJ1$len))

cat(";    variance for VC supp. :",var(VC1$len))

t.test(VC1$len, OJ1$len, paired=F, var.equal = F)

OJ2<-subset(ToothGrowth, ToothGrowth$supp == "OJ" & ToothGrowth$dose == 2)
VC2<-subset(ToothGrowth, ToothGrowth$supp == "VC" & ToothGrowth$dose == 2)


cat("variance for OJ supp. :",var(OJ2$len))

cat(";    variance for VC supp. :",var(VC2$len))

t.test(VC2$len, OJ2$len, paired=F, var.equal = F)

