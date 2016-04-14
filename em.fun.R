#this is a newer version of em.R which takes data files from data and does broader analysis.
#latent variable analysis

source("utils.R") 
require(ggplot2)
require(plyr)
require(dplyr)

expectation <- function(theta.new,theta.old,s,n,y,n.i,l,z) {
    #the expectation upon which optimization is computed for EM alg
    foo <- rep(0,l)
    for (i in 1:l) {
        foo[i] <- sum(z[[i]] * log(model.likelihood.s(y[i],theta.new)) * model.likelihood.s(y[i],theta.old) ^ z[[i]]) / sum(model.likelihood.s(y[i],theta.old) ^ z[[i]])

    }
    -sum(foo)
}


#load all data files
## files <- Sys.glob(file.path("data", "*Rdata")) #get char vector of data filenames
## files.split <- strsplit(files,"\\.") 

## files.process <- function(l) {
##     #l is a list
##     paste(c(l[2],".",l[3]),sep="",collapse="")
## }
## files.envname <- unlist(lapply(files.split,files.process))


build.em <- function(file) {
    load(file) #load the tree into the environment
    #print(ls())
    
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

    theta.em[1] <- .5
    for (i in 2:M) {
        theta.em[i] <- optimize(expectation,c(0,1),theta.old=theta.em[i-1],s=s,n=n,y=y,n.i=n.i,l=l,z=z)$minimum
    }
    return(environment())
}


################
## Runs the script
################
runscript <- function() {
    envs <- list() #holds the environment with each data file.
    for (i in 1:length(files.envname)) {
        envs[[files.envname[i]]] <- build.em(files[[i]])
    }


##plotting function

#make everything long
#extract features
## features = matrix(as.numeric(unlist(sapply(files.envname, function(x) strsplit(x, "\\.")))),length(files),2,byrow=T)
## features[,1] <- features[,1] / 10

    noenvs = length(envs)
    noobs = length(envs[[1]]$theta.em)
    df = data.frame(th = rep(NA, noenvs * noobs),
                    para = rep(NA, noenvs * noobs),
                    n = rep(NA, noenvs * noobs))
                
    for (i in 1:noenvs) {
        #print(features[i,])
        start = (i - 1) * noobs + 1
        end = i * noobs
        df$th[start:end] = envs[[i]]$theta.em
        df$para[start:end] = rep(features[i,1], noobs)
        df$n[start:end] = rep(features[i,2], noobs)
    }

#get the last value
    getlast <- function(df) {
        n = nrow(df)
        df.new = data.frame(df[n,])
        df.new
    }
    df.red <- ddply(df, ~ n + para, getlast)
    df.red
}



#ggplot(df, aes(x = th)) + geom_histogram() + facet_grid(para ~n)
