#1. naloga
#1a)
podatki_2014 <- read.csv("hist_EURIBOR_2014.csv")
podatki_2014 <- podatki_2014[c(1,2,24,44,65,85,107,127,150,172,193,216,237)]


podatki_2015 <- read.csv("hist_EURIBOR_2015.csv")
podatki_2015 <- podatki_2015[c(1,2,24,44,65,85,106,127,150,171,193,216,236)]

podatki_2016 <- read.csv("hist_EURIBOR_2016.csv")
podatki_2016 <- podatki_2016[c(1,2,23,43,65,85,107,129,150,173,195,217,238)]

#1b)

podatki <- cbind(podatki_2014, podatki_2015, podatki_2016)
podatki <- podatki[-c(14,27)]
podatki_pomozno <- podatki[,-1]
rownames(podatki_pomozno) <- podatki[,1]
podatki <- podatki_pomozno
podatki <- t(podatki)

#1c)

casovna_vrsta1 <- ts(podatki[,6], start = c(2014,1), frequency = 12)
casovna_vrsta2 <- ts(podatki[,8], start = c(2014,1), frequency = 12)
ts.plot(casovna_vrsta2,casovna_vrsta1, col = c("black", "red"), main = "Euribor", xlab = "Cas", ylab = "%")
legend(x = 2015, y = 0.5, legend = c("12 m", "6 m"),col = c("black", "red"), lty = 2, bty = "n")

#2.naloga
#2a) Izbrani datumi:  maj 2014, december 2015, september 2016

#2b)
podatki_datum <- podatki_pomozno[c(5,24, 33)]
cas = c(0.25,0.5,1,2,3,6,9,12)
podatki_datum <- data.frame(podatki_datum)
podatki_datum <- cbind(podatki_datum,cas)

graf_datum <- plot(y = podatki_datum[,1],
              x=cas,
              ylim=c(min(-0.4),max(0.6)),
              xlab="Dospetje [mesec]", 
              ylab="%", 
              col="red", 
              main="Casovna struktura Euribor")
lines(podatki_datum[,c(1)], x=cas,col="lightpink", type="o",pch = 10, text(11,0.5,"2.5.2014", col="lightpink"))
lines(podatki_datum[,c(2)], x=cas,col="lightblue", type="o",pch = 10, text(11,0.1,"1.12.2015", col="lightblue"))
lines(podatki_datum[,c(3)], x=cas,col="gold", type="o",pch = 10, text(11,-.15,"1.9.2016", col="gold"))
abline(a=0,b=0,lty=2)

# Komentar k nalogi 2b:
# Opazimo lahko, da se razlike visin med grafi ohranjajo skozi cas. V decembru 2015 lahko opazimo, da so
# bile obrestne mere vse do dospetja 9 mesecev negativne, pri dospetju 12 mesecov pa je postala obrestna mere spet pozitivna.
# Iz nasega primera bi bilo razvidno, da so obrestne mere narascajoca funkcija casa.

#3. naloga
#3a) 
terminska_obrestna <- c(0)
podatki_terminske <- podatki[,c(6,8)]
podatki_terminske <- cbind(podatki_terminske,terminska_obrestna)

i=1
while (i<=36)
{podatki_terminske[i,3] <- 1/(12-6)*((1+12*(as.numeric(as.character(podatki_terminske[i,2]))))/(1 + 6*(as.numeric(as.character(podatki_terminske[i,1]))))-1)
i = i+1
}

#3b)
Euribor6m <- podatki[,6]
podatki_terminske <- cbind(podatki_terminske,Euribor6m)
primerjava <- podatki_terminske[,c(0,4,3)]
napoved <- c(c(NA,NA,NA,NA,NA,NA),primerjava[-c(31:36),2])
primerjava[,2] <- napoved



primerjava_graf <- primerjava[c(7:36),]
primerjava_graf <- as.data.frame(primerjava_graf)
primerjava_graf[,1] <- as.numeric(as.character(primerjava_graf[,1]))
primerjava_graf[,2] <- as.numeric(as.character(primerjava_graf[,2]))

leto2014 <- primerjava_graf[c(1:6),]
leto2015 <- primerjava_graf[c(7:18),]
leto2016 <- primerjava_graf[c(19:30),]

#3c)
graf_terminske_regresija <- plot(primerjava_graf, 
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(-0.5,0.5),
              xlim=c(-0.5,0.5), 
              main="6m Euribor 2014-2016")
points(x=leto2014[,2], y = leto2014[,1], type = "p", col="red",pch = 16)
points(x=leto2015[,2], y = leto2015[,1], type = "p", col="blue",pch = 16)
points(x=leto2016[,2], y = leto2016[,1], type = "p", col="green",pch = 16)
abline(a=0,b=1, lty=2) 
abline(lm(primerjava_graf[,1]~primerjava_graf[,2]),lwd = 2, col = "black")
legend("topleft",bty = "n", c("2014","2015","2016"), pch=16, col =c("red","blue","green"))

#3d)

#2014

graf_terminske_2014 <- plot(leto2014,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano", 
              ylim=c(0,0.4),
              xlim=c(0,0.4),
              main="6m Euribor 2014")
points(x=leto2014[,2], y = leto2014[,1], type = "p", col="red",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(leto2014[,1]~leto2014[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2014"), pch = 16, col =c("red"))

# 2015
graf_terminske_2015 <- plot(leto2015,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(-0.3,0.3), 
              xlim=c(-0.3,0.3),
              main="6m Euribor 2015")
points(x=leto2015[,2], y = leto2015[,1], type = "p", col="blue",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(leto2015[,1]~leto2015[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2015"), pch = 16, col =c("blue"))

# 2016
graf_terminske_2016 <- plot(leto2016,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(-0.4,0.4),
              xlim=c(-0.4,0.4),
              main="6m Euribor 2016")
points(x=leto2016[,2], y = leto2016[,1], type = "p", col="green",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(leto2016[,1]~leto2016[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2016"), pch = 16, col =c("green"))

#3e)

# Ce bi hipoteza pricakovanj trga res veljala, bi morale biti tocke v grafih pri nalogah c) in d) na
# simetrali lihih kvadratov, saj bi to pomenilo, da sta pricakovana obrestna mera in opazovana obrestna
# mera enaki. 
# Ce pogledamo regresijske premice na grafih za posamezna leta opazimo,
# da nasi podatki hipotezo pricakovanj trga ovrzejo.