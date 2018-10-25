### TILL ONLY 	  
#subbasin-level targeting. 
library(snow)

cl <- makeCluster(60)

source('/home/blb/swat/bilevel_targeting_swat/Raccoon/rcode_tillonly/tillonly_sub_optim.R')
source('/home/blb/swat/bilevel_targeting_swat/Raccoon/rcode_tillonly/tillonly_sub_gacode.R')


aaaa <- moboa(suggestions=NULL,
                 popSize=120, iters=50, 
                 mutationChance=NA,
                 elitism=NA,
                 monitorFunc=NULL,
                 showSettings=FALSE, verbose=FALSE)

		  
		  
