data_2014 = read.csv("hist_EURIBOR_2014.csv")
data_2015 = read.csv("hist_EURIBOR_2015.csv")
data_2016 = read.csv("hist_EURIBOR_2016.csv")
data <- data.frame()
data <-rbind(data, c(data_2014$X2.01.2014))
names(data) = c('1w', '2w', '1m', '2m', '3m', '6m', '9m', '12m')
data <- rbind(data, c(data_2014$X3.02.2014))
data <- rbind(data, c(data_2014$X3.03.2014))
data <- rbind(data, c(data_2014$X1.04.2014))
data <- rbind(data, c(data_2014$X2.05.2014))
data <- rbind(data, c(data_2014$X2.06.2014))
data <- rbind(data, c(data_2014$X1.07.2014))
data <- rbind(data, c(data_2014$X1.08.2014))
data <- rbind(data, c(data_2014$X1.09.2014))
data <- rbind(data, c(data_2014$X1.10.2014))
data <- rbind(data, c(data_2014$X3.11.2014))
data <- rbind(data, c(data_2014$X1.12.2014))
data <- rbind(data, c(data_2015$X02.01.2015))
data <- rbind(data, c(data_2015$X02.02.2015))
data <- rbind(data, c(data_2015$X02.03.2015))
data <- rbind(data, c(data_2015$X01.04.2015))
data <- rbind(data, c(data_2015$X04.05.2015))
data <- rbind(data, c(data_2015$X01.06.2015))
data <- rbind(data, c(data_2015$X01.07.2015))
data <- rbind(data, c(data_2015$X03.08.2015))
data <- rbind(data, c(data_2015$X01.09.2015))
data <- rbind(data, c(data_2015$X01.10.2015))
data <- rbind(data, c(data_2015$X02.11.2015))
data <- rbind(data, c(data_2015$X01.12.2015))
data <- rbind(data, c(data_2016$X04.01.2016))
data <- rbind(data, c(data_2016$X01.02.2016))
data <- rbind(data, c(data_2016$X01.03.2016))
data <- rbind(data, c(data_2016$X01.04.2016))
data <- rbind(data, c(data_2016$X02.05.2016))
data <- rbind(data, c(data_2016$X01.06.2016))
data <- rbind(data, c(data_2016$X01.07.2016))
data <- rbind(data, c(data_2016$X01.08.2016))
data <- rbind(data, c(data_2016$X01.09.2016))
data <- rbind(data, c(data_2016$X03.10.2016))
data <- rbind(data, c(data_2016$X01.11.2016))
data <- rbind(data, c(data_2016$X01.12.2016))
row.names(data) <- c('X2.01.2014', 'X3.02.2014', 'X3.03.2014', 'X1.04.2014',
                     'X2.05.2014', 'X2.06.2014','X1.07.2014', 'X1.08.2014',
                     'X1.09.2014','X1.10.2014','X3.11.2014','X1.12.2014',
                     'X02.01.2015','X02.02.2015','X02.03.2015','X01.04.2015',
                     'X04.05.2015','X01.06.2015','X01.07.2015','X03.08.2015',
                     'X01.09.2015','X01.10.2015','X02.11.2015','X01.12.2015',
                     'X04.01.2016','X01.02.2016','X01.03.2016','X01.04.2016',
                     'X02.05.2016','X01.06.2016','X01.07.2016','X01.08.2016',
                     'X01.09.2016','X03.10.2016','X01.11.2016','X01.12.2016')
t_data_6 <- ts(data[,6], start = 2014, frequency=12)
t_data_12 <- ts(data[,8], start = 2014, frequency=12)

ts.plot(t_data_6, t_data_12, main='Euribor', ylab='%', col=c('red', 'black'))
legend('topright', c('6 mesečne ', '12 mesečne'), col=c('red',' black'), lwd=1)

#zanimivi datumi:  maj 2014, december 2015, september 2016
maj_2014<- data[5,]
december_2015 <- data[24,]
september_2016 <- data[33,]

cas = c(1/4, 1/2, 1, 2, 3, 6, 9, 12)
plot(cas,maj_2014,type="l",col="red", ylim=c(-1/2, 0.7))
lines(cas, december_2015,col="green")
lines(cas, september_2016, col='blue')

