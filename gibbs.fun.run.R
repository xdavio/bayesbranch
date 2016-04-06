source("utils.R")
source("gibbs.fun.R")

f = get.files()
files = f$files
files.envname = f$files.envname
features = f$features

#run the gibbs sampler for each data file in the data/ folder
data = list()
for (file in files) {
    load(file)
    data[file] = list(th = gibbs.sampler(t, M = 10, P = 1000)) #temp list which appends onto data

}

#collect data into long style data frame.
df = make.df(data)
make.plot(df, "gibbs.pdf")
df.sum = make.summary(df)
sink.it(df.sum, "gibbstable.tex")


##bump up the sample size and repeat. Cut out the tree with only 10 in the initial generation size.

