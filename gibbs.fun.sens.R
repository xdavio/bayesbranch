source("utils.R")
source("gibbs.fun.R")

## f = get.files()
## files = f$files
## files.envname = f$files.envname
## features = f$features
file  = "data/tree0.8.10.Rdata"

#run the gibbs sampler for each data file in the data/ folder

#alphas = c(0,1,4,10)
#betas = c(0,1,4,10)
alphas = 0:10
betas = 0:10
P = 1000

load(file)
out = matrix(0,length(alphas),length(betas))
i = 0
for (a in alphas) {
    i = i + 1
    j = 0
    for (b in betas) {
        j = j + 1
        th = gibbs.sampler(t, M = 10, P = P, alpha = a, beta = b, hush = TRUE)
        b = mean(th) - .8
        v = var(th)
        mse = b^2 + v
        out[i,j] = mse
        print(j)
    }
}




colnames(out) <- betas
rownames(out) <- alphas
pdf("heatmap.pdf")
heatmap(out,xlab = "Alpha", ylab="Beta",main = "Title of Heatmap of MSE")
dev.off()

sink(file = "sensitivity.tex")
print(xtable(out*100,caption="MSE times 100. Rows are values of alpha; columns are values of beta."))
sink()
