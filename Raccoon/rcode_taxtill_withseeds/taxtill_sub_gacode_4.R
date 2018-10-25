

##Just FYI 
#tillage results with no tax. 
#cost 13778361  no3loadings 9153287



getObjs <- function(input_taxes_tills) {
	
	#example input
	#input_taxes_tills = c(runif(112,1,2),runif(112,0,1))

	
	#old example
	#input_taxes = runif(112,1,2)
	
	#setwd("/home/blb/swat/swat_sweden/")
	source('/home/blb/swat/bilevel_targeting_swat/Raccoon/rcode_taxtill_withseeds/taxtill_sub_optim_4.R')

	lookupfile = read.csv('/home/blb/swat/bilevel_targeting_swat/Raccoon/lookup_qfix.csv')

	inputs <- getSwatInputandProfit(input_taxes_tills,lookupfile)

	for (i in 1:3640) {
		if (is.na(inputs$swat_input[i])) {
			inputs$swat_input[i] = 999.0
		}
	}
	for (i in 1:3640) {
		if (inputs$swat_input[i] < 0) {
			inputs$swat_input[i] = 999.0
		}
	}
	
	for (j in 3641:7280) {
		if (is.na(inputs$swat_input[j])) { inputs$swat_input[j] = 4 }
	}

	
	#inputs$swat_input_kg_ha[((is.na(inputs$swat_input_kg_ha)) | (inputs$swat_input_kg_ha<0))] = 999.0
	swat_inputs = inputs$swat_input
	
	taxObj = inputs$taxObj[1] + inputs$basinprofit_diffs[1]
	
	#basinprofit = sum(inputs$indivprofit[swat_inputs<990]*inputs$area_acres[swat_inputs<990],na.rm=T)
	#taxObj = sum(inputs$taxObj[swat_inputs<990]*inputs$area_acres[swat_inputs<990],na.rm=T)
	
	
	nitrateObj = getNo3Outputs(swat_inputs)

	#write inputs to file. 
	#write.csv(inputs,'inputs_.csv',quote=F,row.names=F)

return(c(taxObj,nitrateObj))
}




# It optimizes a FLOAT chromosome using a genetic algorithm.
#
# string           = string to optimize
# popSize          = the population size
# iters            = number of generations
# mutationChance   = chance that a var in the string gets mutated
moboa <- function(suggestions=NULL,
                 popSize=64, iters=50, 
                 mutationChance=NA,
                 elitism=NA,
                 monitorFunc=NULL,
                 showSettings=FALSE, verbose=FALSE,
                 initialIndivs) {
    
#   clusterApply(cl, runif(length(cl),max=10000000), set.seed)
    
#    x11()
##   x11()
    ################# MULTIPLE OBJECTIVE PARAMETERS #############################################
    numparam = 112*2;   
    numobj = 2;
    totpop = 2*popSize
    vars = numparam;
#    hybrid_num = 20    ## BOA
    #############################################################################################       
    # TODO: should do a variaty of sanity checks first
    if (verbose) cat("Testing the sanity of parameters...\n");
    if (popSize < 5) {
        stop("The population size must be at least 5.");
    }
    if (iters < 1) {
        stop("The number of iterations must be at least 1.");
    }
    if (is.na(mutationChance)) {
        mutationChance = 1/(vars + 1)
    }
    if (is.na(elitism)) {
        elitism = floor(popSize/5)
    }
    if (!(elitism < popSize)) {
        stop("The population size must be greater than the elitism.")
    }

    if (vars > 0) {
        if (verbose) cat("Starting with random values in the given domains...\n");
        # start with an random population
        
        ############   read flows and indexes #################        
#        obsflow = matrix(scan("blueobs.csv"),byrow=T,ncol=1)
#        tindex = (1:length(totflow))
#        tindex = matrix(scan("flow_index.csv"),byrow=T,ncol=1)
#        non_index = matrix(scan("nondriven_index.csv"),byrow=T,ncol=1)
#        drv_index = matrix(scan("driven_index.csv"),byrow=T,ncol=1)
#        sum = 0
#        mflow = mean(obsflow[tindex,])
#        for (i in 1:length(tindex)) {
#          sum = sum + (obsflow[tindex[i],] - mflow)^2;
#        }
#        denom = sum/length(tindex)
        
        
         ############   read limits for calibration variables #################        
        nvars = 2
        nobsn = 112
#        nohru = 193
        var_limits = array(0,c(nvars,2))  
             
        var_limits[1,] = matrix(c(1.0,2.0),byrow=T,ncol=2) ## tax 
        var_limits[2,] = matrix(c(0.0,1.0),byrow=T,ncol=2) ##till

        stringMin = c(replicate(nobsn,var_limits[1,1]),
					  replicate(nobsn,var_limits[2,1]) )
        stringMax = c(replicate(nobsn,var_limits[1,2]),
					  replicate(nobsn,var_limits[2,2]) )

        evalVals = matrix(rep(NA, numobj*2*popSize),ncol=2);
	iter_space = 1  # initialize counter for hybrid operation                
        oldpop = matrix(nrow=popSize, ncol=vars)
        mergedPop = matrix(nrow=2*popSize, ncol=vars)
        simflow = matrix(nrow=2, ncol=2*popSize)
#        nash = matrix(nrow=popSize,ncol=1)
        # fill values
        for (var in 1:vars) {
            oldpop[,var] = stringMin[var] +
                               runif(popSize)*(stringMax[var]-stringMin[var]);
        }
        oldpop[1,] = c( rep(1.0,112),rep(0.1,112) )
        
        
        ####Initialize with taxOnly individuals
        oldpop[2,] = c(as.numeric(initialIndivs[1,1:112]),rep(0.1,112) )
        oldpop[3,] = c(as.numeric(initialIndivs[2,1:112]),rep(0.1,112) )
        oldpop[4,] = c(as.numeric(initialIndivs[3,1:112]),rep(0.1,112) )
        oldpop[5,] = c(as.numeric(initialIndivs[4,1:112]),rep(0.1,112) )
        oldpop[6,] = c(as.numeric(initialIndivs[5,1:112]),rep(0.1,112) )
        oldpop[7,] = c(as.numeric(initialIndivs[6,1:112]),rep(0.1,112) )
        oldpop[8,] = c(as.numeric(initialIndivs[7,1:112]),rep(0.1,112) )
        oldpop[9,] = c(as.numeric(initialIndivs[8,1:112]),rep(0.1,112) )
        oldpop[10,] = c(rep(1.0,112),as.numeric(initialIndivs[9,1:112]) )
        oldpop[11,] = c(rep(1.0,112),as.numeric(initialIndivs[10,1:112]) )
        oldpop[12,] = c(rep(1.0,112),as.numeric(initialIndivs[11,1:112]) )
        oldpop[13,] = c(rep(1.0,112),as.numeric(initialIndivs[12,1:112]) )
        oldpop[14,] = c(rep(1.0,112),as.numeric(initialIndivs[13,1:112]) )
        oldpop[15,] = c(rep(1.0,112),as.numeric(initialIndivs[14,1:112]) )
        oldpop[16,] = c(rep(1.0,112),as.numeric(initialIndivs[15,1:112]) )
        oldpop[17,] = c(rep(1.0,112),as.numeric(initialIndivs[16,1:112]) )
        
        
        
#        pop_500 <-matrix(scan("newparam_250.csv"),byrow=T,ncol=96)
#        oldpop <- t(pop_500)
#        mergedPop[1:popSize,] <- oldpop[1:popSize,];
#        for (var in 1:vars) {
#            mergedPop[(popSize+1):(2*popSize),var] = stringMin[var] +
#                               runif(popSize)*(stringMax[var]-stringMin[var]);
#        }
#     browser()
        if (verbose) cat(paste("Initial evaluation of oldpop...\n"));
#browser()
#        simflow[,1:popSize] =  apply(oldpop,1, getPredflow)


        simflow[,1:popSize] =  parRapply(cl,oldpop,getObjs)
		
		evalVals = t(simflow)
#        evalVals[1:popSize,1] = apply(simflow[,1:popSize],2,geteval,obsflow, drv_index)
#        evalVals[1:popSize,2] = apply(simflow[,1:popSize],2,geteval,obsflow, non_index)
#        evalVals[1:popSize,1] = parApply(cl,simflow[,1:popSize],2,geteval1,al_totflow, tindex)
#        evalVals[1:popSize,2] = parApply(cl,simflow[,1:popSize],2,geteval2,hol_totflow, tindex)
 	############   iteration starts   ####################################        

        for (iter in 1:iters) {
            if (verbose) cat(paste("Starting iteration", iter, "\n"));
	    
	    ##############################################################################
            #           PNX CROSS-OVER
            ##############################################################################
 
#                if (verbose) 
#                  cat("Creating newpop...\n")
                newpop = matrix(nrow = popSize, ncol = vars)
                newEvalVals = rep(NA, popSize)
#                if (verbose) 
#                  cat("  sorting results...\n")
                sortedEvaluations = evalVals[1:popSize,]
                sortedPopulation = oldpop
		nc = 2  ## larger => smaller dispersion of children
                y1 = rep(NA,vars)
		
		for (child in 1:popSize) {
                    parentIDs = sample(1:popSize, 2)     ## randomly choose 2 parents
                    parents = sortedPopulation[parentIDs, ]
		  for (j in 1:vars)  {
		      y1[j] <- stringMin[j] - 1    ## set initial value outside range
#		      while (y1[j] <= stringMin[j] | y1[j] >= stringMax[j]) {
		      while (y1[j] < stringMin[j] | y1[j] > stringMax[j]) {				  
		         w <- runif(n=1,min=1e-5,max=1)
		         if (w <= 0.5) {
		              y1[j] <- rnorm(1,parents[1,j],abs(parents[2,j]-parents[1,j])/nc)
		            }
		         else {
		              y1[j] <- rnorm(1,parents[2,j],abs(parents[1,j]-parents[2,j])/nc)
		            }
		        }
		    }
		  ## add to new population
		  newpop[child, ] = y1
		}  	    

            ##############################################################################
            #                             mutation                                       #
            ##############################################################################
                if (mutationChance > 0) {
                  if (verbose) 
                    cat("  applying mutations... ")
                  mutationCount = 0
                  for (object in (elitism + 1):popSize) {
                    for (var in 1:vars) {
                      if (runif(1) < mutationChance) {
                        dempeningFactor = (iters - iter)/iters
                        direction = sample(c(-1, 1), 1)
                        mutationVal = stringMax[var] - stringMin[var] * 
                          0.67
                        mutation = oldpop[object, var] + 
                          direction * mutationVal * dempeningFactor
                        if (mutation < stringMin[var]) 
                          mutation = stringMin[var] + runif(1) * 
                            (stringMax[var] - stringMin[var])
                        if (mutation > stringMax[var]) 
                          mutation = stringMin[var] + runif(1) * 
                            (stringMax[var] - stringMin[var])
                        newpop[object, var] = mutation
                        newEvalVals[object] = NA
                        mutationCount = mutationCount + 1
                      }
                    }
                  }
                  if (verbose) 
                    cat(paste(mutationCount, "mutations applied\n"))
                }

            ##############################################################################
            #                               merge populations                            #
            ##############################################################################

		  mergedPop <- rbind(oldpop,newpop)
#	   browser()
#	         iter_space = iter_space + 1

#           }  #  iter_space != hybrid_num): ENCLOSE X-OVER AND MUTATION
	   
############################# END OF X-OVER AND MUTATION #####################	

	    # calculate each object
#	browser()
               if (iter == 1) {
                  if (verbose) cat("Running SWAT & Calculating flows... ");
                  simflow[,(popSize+1):(2*popSize)] <-  parRapply(cl,newpop, getObjs)
                  if (verbose) cat("Calculating evaluation values... ");
                  evalVals[(popSize+1):(2*popSize),1] <- t(simflow)[(popSize+1):(2*popSize),1]
                  evalVals[(popSize+1):(2*popSize),2] <- t(simflow)[(popSize+1):(2*popSize),2]
                  #evalVals[(popSize+1):(2*popSize),1] <- apply(simflow[,(popSize+1):(2*popSize)],2, geteval,obsflow,drv_index)
                  #evalVals[(popSize+1):(2*popSize),2] <- apply(simflow[,(popSize+1):(2*popSize)],2, geteval,obsflow,non_index)
                  PriorevalVals = evalVals;            
               }  
               else {   
                  PriorevalVals = evalVals         
                  if (verbose) cat("Running SWAT & Calculating flows... ");
                  simflow[,(popSize+1):(2*popSize)] <-  parRapply(cl,newpop, getObjs)
                  if (verbose) cat("Calculating evaluation values... ");
                  evalVals[(popSize+1):(2*popSize),1] <- t(simflow)[(popSize+1):(2*popSize),1]
                  evalVals[(popSize+1):(2*popSize),2] <- t(simflow)[(popSize+1):(2*popSize),2]
                  
                  #evalVals[(popSize+1):(2*popSize),1] <- apply(simflow[,(popSize+1):(2*popSize)],2, geteval,obsflow,drv_index)
                  #evalVals[(popSize+1):(2*popSize),2] <- apply(simflow[,(popSize+1):(2*popSize)],2, geteval,obsflow,non_index)
               if (verbose) cat(".");
               }
#	browser()	  
            #bestEvals[iter] = min(evalVals);
            #meanEvals[iter] = mean(evalVals);
            if (verbose) cat(" done.\n");
            
              ### NSGA GOES HERE ###################################### 
#    browser()        
			   #    browser()        
			   NSpopulation = nsga2(evalVals,popSize)

               oldpop = as.matrix(mergedPop[NSpopulation,] )
               evalVals[1:popSize,] = evalVals[NSpopulation,]
               simflow[,1:popSize] <- simflow[,NSpopulation]
                       
        
               #evalVals[1:popSize,] <- evalVals[NSpopulation$newpop_index,]
               #simflow[,1:popSize] <- simflow[,NSpopulation$newpop_index]
                       
            ###############  DISPLAY POPULATION  ##########################
#            if (!is.null(monitorFunc)) {
#                if (verbose) cat("Sending current state to rgba.monitor()...\n");
             #  report on GA settings
             #  result = list(type="floats chromosome",
             #                stringMin=stringMin, stringMax=stringMax,
             #                popSize=popSize, iter=iter, iters=iters,
             #                population=oldpop, elitism=elitism, mutationChance=mutationChance,
             #                evaluations=evalVals, best=bestEvals, mean=meanEvals);
             #  class(result) = "rbga";
                
#  browser()	          
#                monitorFunc(evalVals[1:popSize,],iter)
#                PriorMonitor(PriorevalVals[1:popSize,],iter)
                
            
 #           } 
            
#           nash <- apply(simflow[,1:popSize],2,getNash, obsflow, denom, tindex)

            #############################################################
            ####        Save the population here....               ######       
            #############################################################
            #nashname <- paste("nash_",iter,".csv",sep="");
            outputname <- paste("outiter_",iter,".csv",sep="");
            #evalname <- paste("evaliter_",iter,".csv",sep="");
            #pflowname <- paste("predflow_",iter,".csv",sep="");
            #write(nash,file=nashname,ncol=1)
            
            outputs = cbind(evalVals[1:popSize,],oldpop)
            write.csv(outputs,file=outputname, quote=F,row.names=F);
            rm(outputs)
#            write(t(evalVals[1:popSize,]), file=evalname ,sep = " ",ncol=2);
#            for (itercount in c(10,50,100,250,iters)) {
#                if (itercount == iter) {
#                    write(t(simflow[,1:popSize]),file=pflowname, sep = " ",ncol=popSize);
#                } #for itercount
#            } # if itercount                                  
        }  ### for (iter in 1:iters)
    }  ### if (vars > 0)

    # report on GA settings
    
#browser()    
#    test = list(type="floats chromosome", 
#                  stringMin=stringMin, stringMax=stringMax,
#                  popSize=popSize, iters=iters, 
#                  population=oldpop, 
#                  evaluations=evalVals, network=ttt.nw);
#    class(test) = "rbga";

#    dev.off(which=2)

#    return(test);
}



###############################################################################################

#swat2005 <- function(vars_Rga, numparam)
#
#{
#
#        r <- .Fortran("swat2005",
#            as.double(vars_Rga),
#            as.integer(numparam),
#            #### totflow_Rga = double(1500),
#            rchdy2_Rga = double(61400)
#            )
#         
#        list(rchdy2_ga = r$rchdy2_Rga
#        )
# }

###############################################################################################
nsga2 <- function(evalVals,npop) {    
    fnd <- fnd_sort(evalVals)
    sfill <- fill_f(fnd,npop)

     crowd_d <- crowding(matrix(evalVals[fnd[[sfill$i]],],byrow=T,ncol=dim(evalVals)[2]))
    ## crowding sort
     crowd_order <- order(crowd_d,decreasing=T)
     Pt1 <- sfill$Pt1
     fill_length <- npop - length(Pt1)
     if (fill_length > 0) {
     	 crowd_list <- crowd_order[1:fill_length]
     	 for (i in crowd_list) {
     	     Pt1 <- c(Pt1,fnd[[sfill$i]][i])
     	 }
     }
     returnVal <- Pt1
     return(returnVal)
}

 #nsga2 <- function(popSize,numparam,evalVals) {
#        	   
#                objnum = 2   # number of objectives
#				library(nsga2R)
#				ranking <- fastNonDominatedSorting(evalVals)
#				rnkIndex <- integer(popSize)
#				i <- 1
#				while (i <= length(ranking)) {
#				rnkIndex[ranking[[i]]] <- i
#				i <- i + 1
#				}
#
#
##    .Fortran("ranking",
# #       		 as.integer(popSize),
#  #      		 as.integer(numparam),
#   #     		 as.double(evalVals),
#    #    		 newpop_index = integer(popSize)
#     #   		 )
#
 #       		 ##list(newpop_index = r$newpop_index)
  #      		 return(rnkIndex)     
#}

###############################################################################################

monitor <- function(obj,iters) {
    # plot the objectives
#    xlim = c(obj$stringMin[1], obj$stringMax[1]);
#    ylim = c(obj$stringMin[2], obj$stringMax[2]);
    dev.set(which=2)
    plot(x = obj[,1], y = obj[,2], pch=19, col = 3,
             main = paste("Iteration ",iters),
             ylab = "objective 2",
             xlab = "objective 1");

}

###############################################################################################


####################### fill function ####################################
fill_f <- function(x,npop)  {
    Pt1 <- vector()
    i <- 1
    tot_len <- 0
    while ((tot_len + length(x[[i]])) <= npop) {
	Pt1 <- c(Pt1,x[[i]])
	tot_len <- length(Pt1)
	i <- i + 1
    }

    returnVal <- list(Pt1=Pt1,i=i)
    return(returnVal)
}

############################################################################
## crowding distance assignment

crowding <- function(front) {
   eye_d <- vector(mode='numeric',length(front[,1]))
   l <- length(front[,1])
   nobj <- length(front[1,])
   flist <- attributes(front)$dimnames[[1]]
   names(eye_d) <- flist
   for (m in 1:nobj) {
       eye <- front
       eye_order <- flist[order(front[,m])]
       eye_d[eye_order[1]] <- pi/0
       eye_d[eye_order[l]] <- pi/0
       if (l > 2) {
           eye_count <- 1
           for (i in eye_order[2:(l-1)]) {
               eye_count <- eye_count + 1
               eye_d[i] <- eye_d[i] + (eye[eye_order[eye_count+1],m] 
           	       - eye[eye_order[eye_count-1],m])/(diff(range(eye,m)))
           }
	}
   }
   returnVal <- eye_d
   return(returnVal)
}

#############################################################################
## fast-non-dominated-sort

fnd_sort <- function(x) {
  npop <- length(x[,1])
  Sp <- vector(mode='list',npop)
  np <- vector(mode='numeric',npop)
  Fi <- vector(mode='list',npop)
  for (p in 1:npop) {
      for (q in 1:npop) {
          dom_flag <- dominance(x[p,],x[q,])
          if (dom_flag == 1) # if p dominates q
              { Sp[[p]] <- c(Sp[[p]],q) }
          if (dom_flag == 2) # if q dominates p
              { np[p] <- np[p] + 1 }
      }  ## end of q loop
      if (np[p] == 0)
          { pRank = 1
            Fi[[pRank]] <- c(Fi[[pRank]],p) }
  }  ## end of p loop
  #####################################################
  ## FILL OTHER FRONTS
  #####################################################
  front_count <- 1
  nq <- np
  while (length(Fi[[front_count]]) != 0) {
     Q <- vector()
     for (p in Fi[[front_count]]) {
        for (q in Sp[[p]]) {
            nq[q] <- nq[q] -1
            if (nq[q] == 0) {
                qRank <- front_count + 1
                Q <- c(Q,q) }
        }  ## end q loop
     }  ## end p loop
     front_count <- front_count + 1
     Fi[[front_count]] <- Q
  }  ## end while loop
  
  returnVal <- Fi[1:(front_count-1)]
  return(returnVal)
}  ## end of fnd_sort function

#############################################################################
## fast-non-dominated-sort

dominance <- function(ind1,ind2)  {## ind1 and ind2 are rows in the population matrix
    ##  set for minimization (change "<" in next 4 lines to maximize)
    pdq1 <- as.numeric(ind1 <= ind2)    # no worse in all objectives
    pdq2 <- as.numeric(ind1 < ind2)
    qdp1 <- as.numeric(ind2 <= ind1)    # no worse in all objectives
    qdp2 <- as.numeric(ind2 < ind1)
###    cat(pdq1,pdq2,qdp1,qdp2,"\n",file="pdq.txt",append=T)
    if (sum(pdq1) > 0 && sum(pdq2) > 0)  # if p dominates q
       {dflag <- 1}
    if (sum(qdp1) > 0 && sum(qdp2) > 0)  # if q dominates p
       {dflag <- 2}
    if (sum(pdq1) > 0 && sum(qdp1) > 0)  # p non-inferior to q
       {dflag <- 3}
    returnVal <- dflag
    return(returnVal)
 }





PriorMonitor <- function(obj,iters) {
    # plot the objectives
#    xlim = c(obj$stringMin[1], obj$stringMax[1]);
#    ylim = c(obj$stringMin[2], obj$stringMax[2]);
#    dev.set(which=2)
    points(x = obj[,1], y = obj[,2], col = 2)


}

###############################################################################################
# get predicted flow for use in network evaluation
###############################################################################################

getPredflow <- function(string) {

## browser()
    ttt <- seq(1,4198)
    ttt <- string[1:4198]
    temp = swat2005(ttt,4198)
    predflow = temp$rchdy2_ga[1:61400] 
    returnVal = predflow
    returnVal
}
###############################################################################################
# get Nash-Sutcliffe values
###############################################################################################

getNash <- function(string, obsflow, denom, index) {

#    index = (1:length(obsflow))
    pflow <- seq(1:length(obsflow))
    pflow <- string[1:length(obsflow)]
    numer <- sum((obsflow[index] - pflow[index])^2)
    returnVal <- 1 - (numer/length(index))/denom
    returnVal
}

###############################################################################################
# optimize value 
###############################################################################################

geteval <- function(string, obsflow, index) 
{

##    returnVal = NA;   
    
    pflow <- seq(1:61400)
    pflow <- string[1:61400]
    ssn <- sum((obsflow[index] - pflow[index])^2)   
    returnVal <- sqrt(ssn/length(index))
    returnVal
}

getsum <- function(string, index = tindex) 
{
    
    pflow <- seq(1:length(index))
    pflow <- string[1:length(index)]
    sumflow <- sum(pflow[index])   
    returnVal <- sumflow
    returnVal
}


