## NEW VARIABLES ##

ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new

NAs.ECO <- is.na(ECO)
rownums.ECO <- which(NAs.ECO)
ECO[rownums.ECO]

NAs.BDH <- is.na(BDH)
rownums.BDH <- which(NAs.BDH)
BDH[rownums.BDH]

ECO.complete <- ECO[!NAs.ECO]
BDH.complete <- BDH[!NAs.BDH]

summary(ECO.complete)
summary(BDH.complete)

boxplot(ECO.complete, BDH.complete, names=c("ECO.new","BDH.new"))

hist(ECO.complete, prob=TRUE, main="Histogram: ECO.new", xlab="ECO.new")
lines(density(ECO.complete, bw="SJ"))
rug(ECO.complete)
xECO <- seq(min(ECO.complete), max(ECO.complete), 1)
dECO <- dnorm(xECO, mean=mean(ECO.complete), sd=sd(ECO.complete))
lines(xECO, dECO)

hist(BDH.complete, prob=TRUE, main="Histogram: BDH.new", xlab="BDH.new")
lines(density(BDH.complete, bw="SJ"))
rug(BDH.complete)
xBDH <- seq(min(BDH.complete), max(BDH.complete), 1)
dBDH <- dnorm(xBDH, mean=mean(BDH.complete), sd=sd(BDH.complete))
lines(xBDH, dBDH)

plot(ecdf(ECO.complete), do.points=FALSE, verticals=TRUE, main="ECDF: ECO.new")
plot(ecdf(BDH.complete), do.points=FALSE, verticals=TRUE, main="ECDF: BDH.new")

qqnorm(ECO.complete, main="QQ Plot: ECO.new vs Normal"); qqline(ECO.complete)
qqnorm(BDH.complete, main="QQ Plot: BDH.new vs Normal"); qqline(BDH.complete)

qqplot(ECO.complete, BDH.complete,
       xlab="ECO.new Quantiles", ylab="BDH.new Quantiles",
       main="QQ Plot: ECO.new vs BDH.new")

shapiro.test(ECO.complete)
shapiro.test(BDH.complete)

ad.test(ECO.complete)
ad.test(BDH.complete)

ks.test(ECO.complete, BDH.complete)

wilcox.test(ECO.complete, BDH.complete)

hist(ECO.complete, col='lightsteelblue', main="Overlayed Histograms: ECO.new vs BDH.new",
     xlab="Value", freq=TRUE)
hist(BDH.complete, col='lightgreen', add=TRUE, freq=TRUE)
