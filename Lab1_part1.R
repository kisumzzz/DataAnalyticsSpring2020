days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)

RPI_Weather_Week
head(RPI_Weather_Week)

str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset = snowed=='T')
help("subset")

sorted.snowed <- order(RPI_Weather_Week['snowed']) # make a order for a coloum
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df

write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

epi <- read.csv(file.choose(),skip=1,header = T)
View(epi)
attach(epi)

tf <- is.na(epi)
E <- epi[!tf]

summary(EPI)
fivenum(EPI,na.rm = T)

stem(EPI)
hist(EPI,seq(30.,95.,1.0),prob=T)
lines(density(EPI,na.rm = T,bw=1.))
rug(EPI)

#excrise
plot(ecdf(EPI),do.points=F,verticals = T)
par(pty='s')
qqnorm(EPI)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")

hist(epi$DALY)
plot(ecdf(DALY),do.points=T,verticals=T)
stem(DALY)

hist(WATER_H)
plot(ecdf(WATER_H),do.points=F,verticals = T)

boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(EPI,ENVHEALTH,ECOSYSTEM,DALY,AIR_H,WATER_H,AIR_E,WATER_E,BIODIVERSITY)

help("distribution")

epiland <- EPI[!Landlock]
eland <- epiland[!is.na(epiland)]
hist(eland)
hist(eland,seq(30.,95.,1.0),prob=T)

nosurwa <- EPI[!is.na(No_surface_water)]
plot(ecdf(nosurwa),do.points=T,verticals=T)

epi_south_asia <- EPI[EPI_regions=='Asia']
