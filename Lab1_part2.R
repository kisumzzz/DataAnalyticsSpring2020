epi <- read.csv(file.choose(),header=T,skip=1)
View(epi)

plot(ecdf(epi),do.point=F,verticals=T)

help('qqnorm')
par(pty='s')
qqnorm(epi)

multi<-read.csv(file.choose(),header = T)
attach(multi)
mm <- lm(Homeowners~Immigrant)
mm

summary(mm)$coef 
plot(Homeowners~Immigrant)
# the function of dyplyer
# filter(fights,month==3,day==4,carrir=='AA)
# slice(filght,1:15) select rows
# arrange() reorder the rows
# select()
# summarise(flight, avg_air_time = mean(air_time,na.rm=T)
# sample_n(flight,15)
# sample_frac(flight,0.1) 10%

help(abline)
abline(mm,col=2,lwd=3) 

#chapter 2
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type = 'l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col='red')
points(pressure$temperature,pressure$pressure/2,col='blue')
qplot(pressure$temperature,pressure$pressure,geom = 'line')
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

# creating bar graphs
barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # it has warning because the cyl is continuous
qplot(factor(mtcars$cyl))

# bar graph of counts
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

# cerating histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10)
qplot(mpg,data = mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)

#creating box-plot
plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom='boxplot')
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()

qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom='boxplot')








