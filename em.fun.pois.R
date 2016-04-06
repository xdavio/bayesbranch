#this is a newer version of em.R which takes data files from data and does broader analysis.

#this is for the poisson version. for hte original version of this code, see em.fun.R

#latent variable analysis

#functions

model.likelihood = function(x,theta) {
  #likelihood of shifted poisson
  #hardcoded poisson dist shifted by +1
  theta^(x-1) * exp(-theta) / factorial(x-1)
}

model.likelihood.s = function(x,theta) {
  #hardcoded poisson dist shifted by +1
  x / factorial(x - 1) * theta^(x-1) / (theta + 1) * exp(-theta)
}

sample.tree = function(tree,M=5) {
  N = length(tree) #number of generations
  #subsample size

  #don't sample the first generation
  foo.brood = array(0,c(N-1,M))
  for (i in 2:N) {
    foo = tree[[i]]
    if (sum(foo) < 3) {
      foo.brood[i-1,]=0
    } else {
      foo.brood[i-1,]=sample(rep(foo,foo),M)
    }
  }

  foo.brood
}

expectation <- function(theta.new,theta.old,s,n,y,n.i,l,z) {
    #the expectation upon which optimization is computed for EM alg
    foo <- rep(0,l)
    for (i in 1:l) {
        foo[i] <- sum(z[[i]] * log(model.likelihood.s(y[i],theta.new)) * model.likelihood.s(y[i],theta.old) ^ z[[i]]) / sum(model.likelihood.s(y[i],theta.old) ^ z[[i]])

    }
    -sum(foo)
}


#load all data files
files <- Sys.glob(file.path("datapois", "*Rdata")) #get char vector of data filenames
#files.split <- strsplit(files,"\\.")
files.split <- strsplit(files,"(\\.|tree)") 

files.process <- function(l) {
    #l is a list
    paste(c(l[2],".",l[3]),sep="",collapse="")
}
files.envname <- unlist(lapply(files.split,files.process))


build.em <- function(file) {
    load(file) #load the tree into the environment
    print(ls())
    
    #sample tree
    s <- as.vector(sample.tree(t,10))

    n <- length(s)
    y <- unique(s) #unique brood sizes
    n.i <- sapply(y,FUN=function(x) sum((x==s))) #count of each unique value
    l <- length(y)

    #calculate latent variable domain
    z <- list()
    for (i in 1:length(y)) {
        z[[i]] = ceiling(n.i[i] / y[i]):n.i[i]
    }


                                        #EM globals
    M <- 100 #iterations
    theta.em <- rep(0,M) #datavector

    theta.em[1] <- 1.5
    for (i in 2:M) {
        theta.em[i] <- optimize(expectation,c(0,50),theta.old=theta.em[i-1],s=s,n=n,y=y,n.i=n.i,l=l,z=z)$minimum
    }
    return(environment())
}

envs <- list() #holds the environment with each data file.
for (i in 1:length(files.envname)) {
    envs[[files.envname[i]]] <- build.em(files[[i]])
}
