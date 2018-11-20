###

#Rhine river observations - Cluster parallel computing
#Erwin Rottler, University of Potsdam

###

n_cores <- 45 #number of cores used for parallel computing

#stop cluster
stopCluster(my_clust)

#Make cluster for parallel computing
my_clust <- makeCluster(n_cores)
clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, alptempr, lmomco, ncdf4, rEchseSnow, sp, raster))
registerDoParallel(my_clust)

