library(actuar)
 #1a)
podatki <- read.delim("vzorec2.txt")
colnames(podatki) <- c("Podatki")
histogram_podatki <-hist(podatki$Podatki, main="Histogram odškodnin", xlab="Višina odškodnin", col='lightpink')

#1b)

poskus1 <- mde(podatki$Podatki, pweibull, start=list("shape" = 1, "scale" = 5), measure = 'CvM')

shape_weibull=poskus1$estimate[1]
scale_weibull=poskus1$estimate[2]

#1c)
histogram_podatki <-hist(podatki$Podatki, main="Histogram odškodnin",breaks=15, xlab="Višina odškodnin", col='lightpink',
                         probability = TRUE, ylim=c(0,0.7))
curve(dweibull(x, shape_weibull, scale_weibull), col='red', add=TRUE, lwd=2)


plot(ecdf(podatki$Podatki), xlab='Visina odskodnine', ylab='Porazdelitvena funkcija', main='Porazdelitvena funkcija odskodnin')
(curve(pweibull(x, shape_weibull, scale_weibull), col='red', add=TRUE, lwd=2))

#1d) N... BIN(n=20, p = 0.5)
n=20
p=0.5
upanje_N <- n*p
varianca_N <- n*p*(1-p)

upanje_Y <- scale_weibull*gamma(1+(1/shape_weibull))
varianca_Y <- (scale_weibull)^2*(gamma(1+(2/shape_weibull)) - (gamma(1+(1/shape_weibull)))^2)

#grande finale
upanje_S <- upanje_N * upanje_Y
varianca_S <- upanje_N*varianca_Y + (upanje_Y^2 * varianca_N)

############## 2. naloga

#2a)
h <- 0.25
d <- 10/h
diskritirano <- discretize(pweibull(x, shape_weibull, scale_weibull), from = 0, to = (h*d), by = h, method='rounding')

#2b)
plot(stepfun(seq(0, (d - 1) * h , h), diffinv(diskritirano)), main = "Weibullova porazdelitev", col='blue')
curve(pweibull(x, shape_weibull, scale_weibull), col='red', add=TRUE, lwd=2)

#2c)


