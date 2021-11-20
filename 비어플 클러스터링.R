library(tidyverse)
library(cluster)
library(NbClust)
library(factoextra)
library(fpc)

##1. 표본 생성##

# 선형 분리 가능
x11 = rnorm(50, 10, 2)
x12 = rnorm(50, 50, 4)
x13 = rnorm(50, 40, 2)
x14 = rnorm(50, 30, 4)
x15 = rnorm(50, 20, 2)

x21 = rnorm(50, 30, 4)
x22 = rnorm(50, 40, 2)
x23 = rnorm(50, 50, 4)
x24 = rnorm(50, 10, 2)
x25 = rnorm(50, 20, 4)

x1 = c(x11,x12,x13,x14,x15)
x2 = c(x21,x22,x23,x24,x25)

df1 = data.frame(x1,x2)
plot(df1)


# 선형 분리 어려움
y11 = rnorm(500, 0, 2)
y12 = runif(500, 0, 2*pi)

y21 = rnorm(500, 0, 2)
y22 = rnorm(500, 15, 1)

y1 = c(y11,y22*cos(y12))
y2 = c(y21,y22*sin(y12))

df2 = data.frame(y1,y2)
plot(df2)


# 선형 분리 어려움
z11 = runif(500, -7, 3)
z12 = runif(500, -3, 7)

z21 = 2*(z11+2)^2 +rnorm(500,0,3)
z22 = 70-2*(z12-2)^2 +rnorm(500,0,3)

z1 = c(z11,z12)
z2 = c(z21,z22)

df3 = data.frame(z1,z2)
plot(df3)


##2. hierachical clustering ##


#1
d1 = dist(df1, method = "euclidean")
hc1 = hclust(d1,method="average")
NbClust(df1,distance="euclidean",min.nc=2,max.nc=10,
        method="average")
plot(hc1)
rect.hclust(hc1, k=5)
df1$clust = cutree(hc1, k=5)
plot(df1$x1, df1$x2, col=df1$clust)


#2-1
d2 = dist(df2, method = "euclidean")
hc2 = hclust(d2,method="average")
NbClust(df2,distance="euclidean",min.nc=2,max.nc=10,
        method="average")
plot(hc2)
rect.hclust(hc2, k=5)
df2$clust = cutree(hc2, k=2)
plot(df2$y1, df2$y2, col=df2$clust)

#2-2
df2$clust = cutree(hc2, k=5)
plot(df2$y1, df2$y2, col=df2$clust)


#3-1
d3 = dist(df3, method = "euclidean")
hc3 = hclust(d3,method="average")
NbClust(df3,distance="euclidean",min.nc=2,max.nc=10,
        method="average")
plot(hc3)
rect.hclust(hc3, k=5)
df3$clust = cutree(hc3, k=2)
plot(df3$z1, df3$z2, col=df3$clust)

#3-2
df3$clust = cutree(hc3, k=6)
plot(df3$z1, df3$z2, col=df3$clust)



##3. k means clustering ##

#1
set.seed(1000)
kc = kmeans(df1, centers=5, nstart=25)
df1$clust = kc$cluster
plot(df1$x1, df1$x2, col=df1$clust)
points(kc$centers, pch=3, cex=2)


#2-1
set.seed(1000)
kc = kmeans(df2, centers=2, nstart=25)
df2$clust = kc$cluster
plot(df2$y1, df2$y2, col=df2$clust)
points(kc$centers, pch=3, cex=2)


#2-2
set.seed(1000)
kc = kmeans(df2, centers=4, nstart=25)
df2$clust = kc$cluster
plot(df2$y1, df2$y2, col=df2$clust)
points(kc$centers, pch=3, cex=2)



#3-1
set.seed(1000)
kc = kmeans(df3, centers=2, nstart=25)
df3$clust = kc$cluster
plot(df3$z1, df3$z2, col=df3$clust)
points(kc$centers, pch=3, cex=2)


#3-2
set.seed(1000)
kc = kmeans(df3, centers=5, nstart=25)
df3$clust = kc$cluster
plot(df3$z1, df3$z2, col=df3$clust)
points(kc$centers, pch=3, cex=2)



##4. DB scan ##

#1
set.seed(1000)
plot(df1$x1, df1$x2)
db1 = dbscan(df1, eps=3, MinPts=5)
df1$clust = db1$cluster
plot(df1$x1, df1$x2, col=df1$clust)

#2
set.seed(1000)
plot(df2$y1, df2$y2)
db2 = dbscan(df2, eps=3, MinPts=5)
df2$clust = db2$cluster
plot(df2$y1, df2$y2, col=df2$clust)

#3
set.seed(1000)
plot(df3$z1, df3$z2)
db3 = dbscan(df3, eps=2, MinPts=5)
df3$clust = db3$cluster
plot(df3$z1, df3$z2, col=df3$clust)


##5. kernel trick ##

#문제 2
newdf2 = df2[,1:2]
group = c(rep(1,500),rep(2,500))
plot(newdf2$y1, newdf2$y2, col=group)

#중심으로부터 떨어진 거리에 대한 커널을 사용해볼까?
centd = c(mean(newdf2$y1),mean(newdf2$y2))
newdf2$y3 = abs(newdf2$y1-centd[1]) + abs(newdf2$y2-centd[2])

set.seed(1000)
kc = kmeans(newdf2, centers=2, nstart=25)
newdf2$clust = kc$cluster
plot(newdf2$y1, newdf2$y2, col=newdf2$clust)

#문제 3
newdf3 = df3[,1:2]
group = c(rep(1,500),rep(2,500))
plot(newdf3$z1, newdf3$z2, col=group)

#이런 커널을 사용해볼까?
x=(-10:10); lines(x,35+25*sin(x/1.5))
newdf3$z3 = (35+25*sin(newdf3$z1/1.5) - newdf3$z2)

set.seed(1000)
kc = kmeans(newdf3, centers=5, nstart=25)
newdf3$clust = kc$cluster
plot(newdf3$z1, newdf3$z2, col=newdf3$clust)


