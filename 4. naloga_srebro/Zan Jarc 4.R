library(ggplot2)
### 1. naloga

# a)

podatki <- read.csv('srebro19.csv')

podatki <- podatki[120:1,]
podatki <- podatki[c(5)]
podatki$Close <- as.numeric(gsub("\\$", "", podatki$Close))

# b)

casovna_vrsta <- ts(podatki)
srebro_graf <- ts.plot(casovna_vrsta, xlab = "Time", ylab = "EUR", main = "Srebro")
points(casovna_vrsta, pch = 20)

### 2. naloga

# a) 
G <- function(vrsta, k) {
  len <- length(vrsta)
  glajenje <- c()
  for (i in 1:(len-k)) {
    glajenje[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zglajena_vrsta <- ts(glajenje)
  return(zglajena_vrsta)
}

# b)

glajenje5 <- G(casovna_vrsta, 5)
len <- length(casovna_vrsta)
napoved5 <- sum(casovna_vrsta[(len-5+1):len])/5

# c)

graf_zglajeno <- ts.plot(casovna_vrsta, glajenje5, xlab = "Time", ylab = "EUR", main="Drseče povprečje",lwd=2:1,col=1:10)
points(casovna_vrsta, pch = 20)

# d)
MSE <- function(vrsta, zglajena_vrsta, k) {
  T <- length(vrsta)
  delna_vsota <- 0
  for (i in (k+1): T) {
    delna_vsota <- delna_vsota + (vrsta[i] - zglajena_vrsta[i-k])^2
  }
  napaka <- (1/(T-k))*delna_vsota
  return(napaka)
}

MSE5 <- MSE(casovna_vrsta, glajenje5, 5)

# e)
glajenje15 <- G(casovna_vrsta, 15)
glajenje30 <- G(casovna_vrsta, 30)

napoved15 <- sum(casovna_vrsta[(len-15+1):len])/15
napoved30 <- sum(casovna_vrsta[(len-30+1):len])/30

MSE15 <- MSE(casovna_vrsta, glajenje15, 15)
MSE30 <- MSE(casovna_vrsta, glajenje30, 30)

par(mfrow = (c(2,2)))
graf_zglajeno15 <- ts.plot(casovna_vrsta, glajenje15, xlab = "Time", ylab = "EUR", main="Drseče povprečje reda 15",lwd=2:1,col=1:10)
points(casovna_vrsta, pch = 20)
graf_zglajeno30 <- ts.plot(casovna_vrsta, glajenje30, xlab = "Time", ylab = "EUR", main="Drseče povprečje reda 30",lwd=2:1,col=1:10)
points(casovna_vrsta, pch = 20)


### 3. naloga

# a)

EG <- function(vrsta, alpha) {
  glajena_vrsta <- c(vrsta[1])
  dolzina <- length(vrsta)
  for (i in 2:dolzina) {
    glajena_vrsta[i] <- alpha*vrsta[i] + (1-alpha)*glajena_vrsta[i-1]
  }
  return(ts(glajena_vrsta))
}


# b)
par(mfrow=(c(1,1)))
izbrani_alpha = 0.23

eks_glajeno <- EG(casovna_vrsta, izbrani_alpha)
graf_eks_glajeno<- ts.plot(casovna_vrsta, eks_glajeno, xlab = "Time", ylab = "EUR", main="Eksponento glajenje",lwd=2:1,col=1:10)
points(casovna_vrsta, pch = 20)
