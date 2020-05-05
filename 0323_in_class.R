data("USArrests")

states=row.names(USArrests)
states

apply(USArrests , 2, mean)
apply(USArrests , 2, var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale=0)

pr.out$sdev

# PCA with iris dataset
data("iris")
head(iris)

irisdata1 <- iris[,1:4]
irisdata1

head(irisdata1)

principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)

plot(principal_components)

plot(principal_components, type = "l")

biplot(principal_components)

install.packages('MASS')
data(Boston, package="MASS")

pca_out <- prcomp(Boston,scale. = T)

pca_out
plot(pca_out)

help(biplot)
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc

head(boston_pc)
summary(boston_pc)