### TAX + TILL 	  
#subbasin-level targeting. 
library(snow)

cl <- makeCluster(8)

source('/home/blb/swat/bilevel_targeting_swat/Raccoon/rcode_taxtill_withseeds/taxtill_sub_optim_4.R')
source('/home/blb/swat/bilevel_targeting_swat/Raccoon/rcode_taxtill_withseeds/taxtill_sub_gacode_4.R')

		  
tax_seeds = read.csv('tax_seeds.csv')
till_seeds = read.csv('till_seeds.csv')

##These lines of code are new and use 8 of the following individuals
##in "outiter_50.csv" to initialize the setup. 
initialIndivs = rbind(
					  tax_seeds[c(2,10,25,35,50,75,100,120),3:114],
					  till_seeds[c(2,10,25,35,50,75,100,120),3:114]
					  )
 

aaaa <- moboa(suggestions=NULL,
                 popSize=24, iters=50, 
                 mutationChance=NA,
                 elitism=NA,
                 monitorFunc=NULL,
                 showSettings=FALSE, verbose=FALSE,
                 initialIndivs)

