#gibbs sampler
require("BiasedUrn") #need this for multivariate hypergeometric distribution
source("utils.R")
require(ggplot2)
require(plyr)
require(dplyr)
require(xtable)


gibbs.sampler <- function(t, g = 1, alpha = 1, beta = 1, M = 5, P = 10000, hush = FALSE) {
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

        if (hush == FALSE) {
            print(z.)
        }

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


gibbs.sampler.O <- function(t, g = 1, alpha = 1, beta = 1, M = 5, P = 10000) {
    #This is the exact same as "gibbs.sampler" but it uses a slightly different update for the parameter theta given the rest of the parameters. See *** in the outer loop
    
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
        #***
        th <- rbeta(1,alpha + sum(o - 1), beta +  length(o) )
        th.gibbs[p] <- th
    }

    plotset = round(4 * P / 5, 0)

    out = th.gibbs[plotset:P]
    return(out)
}


make.df <- function(data) {
    #data is a list of a bunch of thetas
    noenvs = length(data)
    noobs = sum(unlist(lapply(data, length)))
    df = data.frame(th = rep(NA, noenvs * noobs),
                    para = rep(NA, noenvs * noobs),
                    n = rep(NA, noenvs * noobs))
                
    for (i in 1:noenvs) {
        start = (i - 1) * noobs + 1
        end = i * noobs
        df$th[start:end] = data[[i]]
        df$para[start:end] = rep(features[i,1], noobs)
        df$n[start:end] = rep(features[i,2], noobs)
    }

    df
}

make.plot <- function(df, loc) {
    ggplot(df, aes(x = th)) +
        geom_histogram() +
        facet_grid(n ~ para) +
        xlab("Theta") +
        ylab("Count") +
        ggtitle("Histogram of Posterior of Theta")
    ggsave(loc)
}


make.summary <- function(df) {
    df.sum = df %>%
        transform(bias = th - para) %>%
        group_by(para,n) %>%
        summarize(mean = mean(th),
                  sd = sd(th),
                  bias = mean(bias),
                  mse = mean(bias^2) + var(th))

    df.sum
}

sink.it <- function(df, loc) {
    sink(file = loc)
    print(xtable(df), include.rownames = FALSE)
    sink()
}
