#latent variable analysis

#functions
offspring = function(x) {
  rgeom(1,1-theta)+1
}

#likelihood of shifted geometric
model.likelihood = function(x,theta) {
  #hardcoded geometric dist shifted by +1
  return(theta^(x)*(1-theta)/theta)
}

model.likelihood.s = function(x,theta) {
  #hardcoded geometric dist shifted by +1
  return( x * theta^x * (1 - theta)^2 / theta )
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


#####
##get tree
load("tree.Rdata") #load the tree into the environment

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

expectation <- function(theta.new,theta.old) {
    foo <- rep(0,l)
    for (i in 1:l) {
        foo[i] <- sum(z[[i]] * log(model.likelihood.s(y[i],theta.new)) * model.likelihood.s(y[i],theta.old) ^ z[[i]]) / sum(model.likelihood.s(y[i],theta.old) ^ z[[i]])

    }
    -sum(foo)
}

#EM globals
M <- 100 #iterations
theta.em <- rep(0,M) #datavector

theta.em[1] <- .5
for (i in 2:M) {
    theta.em[i] <- optimize(expectation,c(0,1),theta.old=theta.em[i-1])$minimum
}







