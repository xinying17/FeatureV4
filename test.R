MU1 = c(10,2,-5)
MU2 = MU1
SIG1 = matrix(c(1,.8,0.25,.8,1,0,0.25,0,1),nrow=3,ncol=3)
SIG2 = matrix(c(1,.1,0.4,.1,1,.9,0.4,.9,1),nrow=3,ncol=3)
n = 1000
data1_sim = mvrnorm(n, MU1, SIG1, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
data_train = data.frame(data1_sim)
plot(data_train)


p=0.1
corr=0
cor.method = "spearman"
network_corr <- cor(data_train,method = cor.method)


num_sample = nrow(data_train) - 2 # degrees of freedom

# T-test w/ H0: correlation == 0; H1: correlation != 0
network_corr_t <-
  network_corr / (sqrt(abs(1 - (network_corr ^ 2)) / (num_sample))) # t-stat for T-test
network_corr_t_p <-
  2 * (1 - pt(abs(network_corr_t), num_sample)) # p-value for T-test

# FDR adjustment for multiple hypothesis testing
network_p <-
  matrix(
    p.adjust(network_corr_t_p, 'fdr'), # adjusted p-values (as a vector)
    nrow = nrow(network_corr_t_p),
    ncol = ncol(network_corr_t_p)
  ) # p-values as a matrix

# p-value filter
network <- network_corr
if(p>0){
  network[network_p>min(p,max(network_p))] = 0
}

# correlation value filter
if(corr=='plus'){
  network <- abs(network)
}

diag(network) <- 0


# regression matrix
data_Cmean = colMeans(data_train)
m = length(data_Cmean)
B = matrix(0,m,m)
for(i in 1:m){
  for (j in 1:m){
    SXY = sum(data_train[,i]*data_train[,j]) - nrow(data_train)*data_Cmean[[i]]*data_Cmean[[j]]
    SXX = sum(data_train[,j]^2) - nrow(data_train)*data_Cmean[[j]]^2

    B[i,j] = data_Cmean[[i]]/data_Cmean[[j]]#SXY/SXX
  }
}
B[network_p>min(p,max(network_p))] = 0

# new laplacian
w = network
wx = w*B
d1 = colSums(network)
d2 = colSums(w*B^2)
D = diag((d1+d2)/2)
laplacian = D-wx


f = c(1,1,1)

fLf = 0
for(i in 1:m){
  for(j in 1:m){
    fLf = fLf + (f[[i]]-B[i,j]*f[[j]])^2*w[i,j]
  }
}
print(fLf)

fLxf = 2*f %*% laplacian %*% f
print(fLxf)

## linear regression

fit = lm(data_train[,1] ~ data_train[,2])
