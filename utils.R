require("BiasedUrn") #need this for multivariate hypergeometric distribution
require("xtable")
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


get.files <- function() {
    files <- Sys.glob(file.path("data", "*Rdata")) #get char vector of data filenames
    files.split <- strsplit(files,"\\.") 

    files.process <- function(l) {
        #l is a list
        paste(c(l[2],".",l[3]),sep="",collapse="")
    }
    files.envname <- unlist(lapply(files.split,files.process))

    features = matrix(as.numeric(unlist(sapply(files.envname, function(x) strsplit(x, "\\.")))),length(files),2,byrow=T)
    features[,1] <- features[,1] / 10

    return(list(files = files,
                files.envname = files.envname,
                features = features))
}


sink.it <- function(df, loc) {
    sink(file = loc)
    print(xtable(df), include.rownames = FALSE)
    sink()
}
