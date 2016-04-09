source("em.fun.R")

#get files
f = get.files()
files = f$files
files.envname = f$files.envname
features = f$features

template = runscript()

wrap <- function() {
    runscript()[,1]
}

N = 1000
M = length(files)

df = replicate(N, wrap()) #simulated data operation
long = matrix(NA,N*M,3)
for (i in 1:M) {
    start = N*(i-1) + 1
    end = N*i
    long[start:end,1] = rep(template$para[i],N) #para
    long[start:end,2] = rep(template$n[i],N)    #n
    long[start:end,3] = df[i,]                  #theta
}
colnames(long) <- c("para","n","theta")
df.long = data.frame(long)


make.sum <- function(df) {
    m = mean(df$theta)
    s = sd(df$theta)
    bias = m - df$para[1]
    mse = bias^2 + s^2
    
    df.new = data.frame(mean = m, sd = s, bias = bias, mse = mse)
}
df.sum <- ddply(df.long, ~ para + n, make.sum) #xtable this and push it to tex
sink.it(df.sum, "emtable.tex")


make.plot <- function(df, loc) {
    ggplot(df, aes(x = theta)) +
        geom_histogram() +
        facet_grid(n ~ para) +
        xlab("Theta") +
        ylab("Count") +
        ggtitle("Histogram of Posterior of Theta")
    ggsave(loc)
}

make.plot(df.long,"emplot.pdf")
