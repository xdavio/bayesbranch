#metropolis hastings algorithm

#globals
init = 10 #ancestor size
theta = .4

#functions
offspring = function(x) {
  rgeom(1,1-theta)+1
}

#likelihood of shifted geometric
model.likelihood = function(x,theta) {
  #hardcoded geometric dist shifted by +1
  return(theta^(x)*(1-theta)/theta)
}

prior <- function(theta) {
    dbeta(theta,1,1)
}

#Simulate next generation
new.gen = function(prev.size) {
  sapply(1:prev.size,offspring)
}

simulate.tree = function(N,init=10) {
  #N-number of generations to simulate for
  
  family.sizes=list(); family.sizes[[1]]=init
  
  for (i in 2:(N+1)) {
    #loop through the generations
    family.sizes[[i]]=new.gen(sum(family.sizes[[i-1]]))
  }

  #note that each element of the list is aggregated by family size
  #so the number of elements in the vector is the previous generation size
  #and the sum of the elements of the vector is the current generation size
  family.sizes
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



#tree generation -- fixed
#t=simulate.tree(1,100000)
#save(t,file="tree.Rdata")
load("tree.Rdata") #load the tree into the environment

#sample tree
s <- as.vector(sample.tree(t,100))

n <- length(s)
y <- unique(s) #unique brood sizes
n.i <- sapply(y,FUN=function(x) sum((x==s))) #count of each unique value

ratio1 <- function(theta.new,theta.old) {
    c <- sum(n.i / y)
    out <- exp(4*n*sum(  sqrt(n.i/y/c) * (sqrt(model.likelihood(y,theta.new)) - sqrt(model.likelihood(y,theta.old))) ))*prior(theta.new)/prior(theta.old)
    out
}

sig=2
ratio2 <- function(theta.new,theta.old) {
    x.old <- log(theta.old/(1-theta.old))
    x.new <- log(theta.new/(1-theta.new))
    dnorm(x.old,x.new,sig)/dnorm(x.new,x.old,sig) #old given new / new given old
}


proposal <- function(theta.old) {
    x.old <- log(theta.old/(1-theta.old))
    x.new <- rnorm(1,x.old,sig)
    theta.new <- exp(x.new)
    theta.new <- theta.new/(1 + theta.new)
    theta.new
}

iter=50000
theta.post <- rep(0,iter)
theta.post[1] <- .5
for (j in 2:iter) {
    theta.old <- theta.post[j-1]
    theta.new <- proposal(theta.old)
    r1 <- ratio1(theta.new,theta.old)
    r2 <- ratio2(theta.new,theta.old)
    a <- r1*r2
    if (a >= 1) {
        theta.post[j] <- theta.new
        next
    } else {
        theta.post[j] <- ifelse(runif(1) < a,theta.new,theta.old)
    }
    
}



