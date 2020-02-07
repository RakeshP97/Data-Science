setwd('/home/cim/pgt/mhac087/CS5100//Assignment3/')

#Read the data 
my_data <- read.table("nci.data.txt", header = FALSE)

#Tranpose the data
my_data <- t(my_data)

#Omit the Null records from  the exsiting table 
data <- na.omit(my_data)

set.seed(928)

m <- apply(data, 2, mean)

s <- apply(data, 2, sd)

#Normalization of the data
for (i in 1:dim(data)[1])
{
  data[, i] <- ((data[, i] - m[i]) / s[i])
}

#hierarchical agglomerative clustering with the following linkage: single, complete, average and centroid 
high_alg_clust = function(data, f)
{
  dm <- dist(data, method = "euclidean")
  if(!is.matrix(dm))
  {
    dm <- as.matrix(dm)
  }
  
  diag(dm) <- Inf
  nr <- nrow(dm)
  lin = -(1:nr)
  cust <- matrix(0, nrow = nr -1, ncol = 2)
  height <- rep(0, nr-1)
  for( h in seq(1,nr-1))
  {
    height[h] = min(dm)
    i = which(dm - height[h] ==0,arr.ind = TRUE )
    i = i[1, , drop = FALSE]
    pt <- lin[i]
    pt <- pt[order(pt)]
    cust[h, ] <- pt
    agr <- c(i,which(lin %in% lin[i [1, lin[i] > 0]]))
    lin[agr] <- h
    ds = apply(dm[i, ], 2, f)
    
    dm[min(i), ] = dm[, min(i)] = ds
    dm[min(i), min(i)] = Inf
    dm[min(i), max(i)] = Inf
    dm[max(i), ] = dm[, max(i)] = Inf
  }
  #clustering the Nodes 
  k = nrow(cust) + 1
  clustorder = rep(0, k)
  clustorder[1] = cust[k - 1, 1]
  clustorder[2] = cust[k - 1, 2]
  cnt = 2
  for (i in seq(k - 2, 1))
  {
    for (j in seq(1, cnt))
    {
      if (clustorder[j] == i)
      {
        clustorder[j] = cust[i, 1]
        if (j != cnt)
        {
          cnt = cnt + 1
          for (z in seq(cnt, j + 2))
            clustorder[z] = clustorder[z - 1]
          clustorder[j + 1] = cust[i, 2]
        } else
        {
          cnt = cnt + 1
          clustorder[cnt] = cust[i, 2]
        }
      }
    }
  }
  

  structure(
    list(
      merge = cust,
      height = height,
      order = -clustorder,
      labels = rownames(dm),
      method = method,
      call = match.call(),
      dist.method = "euclidean"
    ),
    class = "hclust"
  )
}


#2. Different agglomerative clustering
par(mfrow=c(1,5))

method<- 'single'
hc.single <- high_alg_clust(data, min)
plot(hc.single,main="Single Linkage",xlab="",sub ="",cex =.9)
rect.hclust(hc.single, k=9, border = 2:5)

method<- 'complete'
hc.complete <- high_alg_clust(data,max)
plot(hc.complete, main="Complete Linkage",xlab="",sub="",cex=.9)
rect.hclust(hc.complete, k =5, border = 2:5)

method <- 'average'
hc.average <- high_alg_clust(data,mean)
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)

rect.hclust(hc.average, k=5,border = 2:5)


method <- 'median'
hc.centriod <- high_alg_clust(data, median)
plot(hc.centriod,main="Centriod Linkage",xlab="",sub="",cex=.9)



rect.hclust(hc.centriod, k = 5, border = 2:5)
#3. performance of hierarchical agglomerative clustering
#The correlation between the distance matrix and the cophenetic distance is one metric to help assess


d0 <- dist(data, method = "euclidean")
c1 <- cophenetic(hc.single)
print(c('Sigle Linkage perfomance: ', cor(d0,c1)))

c2 <- cophenetic(hc.complete)
print(c('Complete Linkage perfomance: ', cor(d0,c2)))

c3 <- cophenetic(hc.average)
print(c('Average Linkage perfomance: ', cor(d0,c3)))

c4 <- cophenetic(hc.centriod)
print(c('Centriod Linkage perfomance: ', cor(d0,c4)))



#K_means clustering 

totwit <- rep(0,62)

bestkmenas = function(data)
{
  besttot <- Inf
  bestkvalue <- 1
  for(k in 2:63)
  {
    km.out <- kmeans(data, k, nstart=50)
    totwit[k-1] = km.out$tot.withinss
    if(km.out$tot.withinss < besttot)
    {
      besttot = km.out$tot.withinss
      bestkvalue <- k
    }
    print(km.out$tot.withinss)
  }
  print(c(bestkvalue, besttot))
  plot(2:63,totwit,col='red',type='l',lty = "dashed",xlab = "K-value", ylab = "Total Withinss", main="K vs tot.withinss")
}
bestkmenas(data)
