#latent variable analysis

#globals
init = 10 #ancestor size
theta = .8

#read script values
cmd <- commandArgs() #dump shell arguments
cmd <- cmd[3:length(cmd)] #cut off original parameters

if (!is.na(cmd[1])) {
    foo <- function(x) {
        gregexpr(pattern="=",text=x)[[1]][1]
    }
    cmd.eq <- sapply(cmd,foo) #location of the equals in the cmd array

    assigner <- function(x) {
        a <- substring(cmd[x], 1, cmd.eq[x] - 1)
        b <- substring(cmd[x], cmd.eq[x] + 1, 1000)
        assign(x=a, value=as.numeric(b), envir=.GlobalEnv)
    }
    sapply(1:length(cmd),assigner)

    #th <- as.numeric(commandArgs[3])
}


#functions
offspring = function(x) {
  rgeom(1,1-theta)+1
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

#tree generation -- fixed
t=simulate.tree(1,init)
save(t,file=paste("tree",theta,".",init,".Rdata",sep=""))
