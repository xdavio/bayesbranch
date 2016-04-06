#gibbs sampler
require("BiasedUrn") #need this for multivariate hypergeometric distribution

#functions
offspring = function(theta) {
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


prior.density <- function(theta) {
    dbeta(theta,alpha,beta)
}

gibbs.sampler <- function(t, g = 1, alpha = 1, beta = 1, M = 5, P = 10000) {
    #pass this function a tree. it'll sample it and then do gibbs sampling. 

    #M - sample size
    #g - the number of generations. don't change
    #t - the tree to sample from

    #P - number of gibbs iterations
    
    #sample tree
    s <- as.vector(sample.tree(t,M))

    n <- length(s)
    y <- unique(s) #unique brood sizes
    n.i <- sapply(y,FUN=function(x) sum((x==s))) #count of each unique value
    l <- length(y)

    #bounds on z
    z.lower <- ceiling(n.i / y)
    z.upper <- n.i

    #calculate latent variable domain
    z <- list()
    for (i in 1:length(y)) {
        z[[i]] = ceiling(n.i[i] / y[i]):n.i[i]
    }

    #gibbs sampler parameter initialization
    z. <- sapply(z,max) #initialize z | x
    o <- rep(y,z.) #initialize o | x, z
    N <- sum(z.) #initialize N | 
    th <- rbeta(1,alpha + sum(z. * (y - 1)), beta + 2*sum(z.) ) # model likelihood * prior | x,z => simulate from given beta distribution

    #first iteration of gibbs sampler
    th.gibbs <- rep(0,P)
    for (p in 1:P) {

        #N given the rest
        N <- ceiling(max(sum(z.),as.numeric(t[[1]])^g * 1/(1-th),sum(z.lower)))

        #z given the rest
        for (i in 1:l) {
            #looping through the unique values of y
            if (y[i] == 1) {
                z.[i] = n.i[i]
                next
            }
            s.i <- o[which(o == y[i])]
            for (j in 1:1000) {
                f <- rMWNCHypergeo(1,s.i[1:min(length(s.i),32)],n.i[i],odds=1)
                f <- length(which(f != 0))
                if (f <= z.upper[i] && f >= z.lower[i]) break
            }
            z.[i] <- f
        }

        print(z.)

        #o given the rest
        o <- c(rep(y,z.), replicate(N - sum(z.),offspring(th)) )

        #th given the rest
        th <- rbeta(1,alpha + sum(z. * (y - 1)), beta + 2 * sum(z.) )
        th.gibbs[p] <- th
    }

    plotset = round(4 * P / 5, 0)

    out = th.gibbs[plotset:P]
    return(out)
}
