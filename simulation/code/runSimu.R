library(parallel) # one of the core R packages
library(doParallel)
library(foreach)
library(iterators)


parallel::detectCores()
n.cores <- parallel::detectCores()
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
doParallel::registerDoParallel(cl = my.cluster)

path="./simulation/code/"
source(paste(path,"R_pac_2.R",sep=""))
source(paste(path,"simu.R",sep=""))

## j is the j-th parameter setting
for (j in 1:24) {
  out = simu(simu_time = 500,scenario = simu_table[j,2],multiple_num = simu_table[j,1],
             path=path,finite_pop_norm=finite_pop_norm)
  save(out,file=paste("simulation/results/",j,".RData",sep=""))
}

