

#BL BARNHART 2018-04-04
#This is for running SWAT and optimizing fertilizer reductions at the 
#subbasin scale. In particular, below consists of two functions.
#1. getSwatInputandProfit(): this calculates the fertilizer inputs 
#into swat given certain tax and q levels. 
#2. runSWAT() calculates no3 outputs from SWAT given fertilizer inputs. 


################################################
########## SETTING UP INPUT PARAMETERS #########
################################################
#Input Taxes
#x = runif(112,min,max)
#lookupfile = read.csv('/home/blb/swat/swat_sweden/lookup.csv')

getSwatInputandProfit <- function(input_taxes_tills,lookupfile) {
#example
#input_taxes_tills = c(runif(112,1,2),runif(112,0,1))

#old
#subbasin-scale runif(112,1,2)
#basinwide #input_taxes = array(runif(1,1,2),c(112))

lookup=lookupfile
#lookup = read.csv('/home/blb/swat/swat_sweden/lookup.csv')
q = lookup$V9
area = lookup$V10*247.105 #convert from km2 to acres
tills = lookup$V7 #3 is mulch till, 4 is no-till.

#Constants
row = 50.98 #lb/ac
dc = 0 #if previous crop is corn, this is equal to 1; 0 if otherwise. 
wN = 0.25 #$/lb from Duff and Smith (2004)
a13 = -0.0028545 #bu.ac./(lb)^2
p1 = 2.18 #$/bu Johanns (2012) year 2003 
b16 = 0.21 #$/bu 
a12 = 0.74044 #bu/lb

#Initialize Array
swat_input = array(NA,c(7280)) #initialize 3640 hru parameters for swat input
qvaluesused = array(NA,c(3640,1))
areaused = array(NA,c(3640,1))
inputtaxesused = array(NA,c(3640,1))
subbasinnumber = array(NA,c(3640,1))

#current config; all CORN and SOYB set to mulch till. 
s2nopolicy = 1 #1 if mulch till; 0 if otherwise
s3nopolicy = 0 #1 if no-till; 0 if otherwise

### SUB TILL SETUP. IF input_taxes_tills > 0.5, THEN implement no-till
s2policy = array(NA,c(3640))
s3policy = array(NA,c(3640))

#Calculate Fertilizer Application @ Max Profit 

#Note that this fertilizer is only applied in the CORN year of the
#CORN/SOY rotation. Therefore, dc and dcc always = 0. 
counter = 1
for (j in 1:112) { 
	sub = lookup[lookup$V2==j,]
	
	for (k in 1:length(sub$V1)) {
		if (sub$V8[k] %in% c('CORN','SOYB')) {
			swat_input[counter] = as.double(
				row*dc + ((1.04 * input_taxes_tills[j] * wN)/(2*a13*q[counter]*(p1-b16))) +
				-(a12/(2*a13))
				)
			
			#add till
			if (input_taxes_tills[j+112] < 0.5) {
				s2policy[counter] = 1
				s3policy[counter] = 0
				swat_input[counter+3640] = input_taxes_tills[j+112]
				}
			if (input_taxes_tills[j+112] > 0.5) {
				s2policy[counter] = 0
				s3policy[counter] = 1
				swat_input[counter+3640] = input_taxes_tills[j+112]
				}
			 			
			qvaluesused[counter,1] = q[counter]
			areaused[counter,1] = area[counter]
			inputtaxesused[counter,1] = input_taxes_tills[j]
			subbasinnumber[counter,1] = j
			counter = counter + 1
		}
		else {
			swat_input[counter] = NA
			swat_input[counter+3640] = NA
#			subbasinnumber[counter,1] = j
			counter = counter + 1
		}
		
	}
}

#Convert from lb/ac to kg/ha using 1.12085 conversion
#swat_input[swat_input<990] = swat_input[swat_input<990]* 1.12085 
for (i in 1:3640) {
	if (!is.na(swat_input[i])) { 
		swat_input[i] = swat_input[i]*1.12085
		if (swat_input[i] < 0) { swat_input[i]=NA }
	}
}

#swat_input[!is.na(swat_input)] = swat_input[!is.na(swat_input)]*1.12085
###################################################
############# END OF SETTING UP SECTION 
###################################################

###################################################
############## CALCULATE TOTPROFIT
#corn yield
a10 = 177.0309 #bu/ac
a11 = -28.4758 #bu/ac
a12 = 0.74044 #bu/lb
a13 = -0.0028545 #bu.ac./(lb^2)
row = 50.98 #lb/ac
gam11 = 0.978
gam12 = 0.932
gam13 = 0.984
gam14 = 0.970

#dc is set below. 
dcc = 0 #if last 2 crops were corn, then 1; 0 otherwise.


b10 = 183.62 #$/ac
b11 = -6.37 #$/ac
b12 = -6.55 #$/ac
b13 = 20.05 #$/ac
b14 = 12.66 #$/ac
b15 = 3.06 #$/ac
b16 = 0.21 #$/bu
wN = 0.25 #$/lb

a20 = 47.3876 #bu/ac
a21 = 11.78437 #bu/ac
a22 = 19.6716 #bu/ac
gam21 = 0.974 
gam22 = 0.951

b20 = 143.80 #$/ac
b21 = -1.33 #$/ac
b22 = -5.80 #$/ac
b23 = 0.19 #$/bu

p1 = 2.18 #$/bu
p2 = 6.08 #$/bu
ycorn_np = array(NA,c(3640))
costcorn_np = array(NA,c(3640))
ysoy_np = array(NA,c(3640))
costsoy_np = array(NA,c(3640))
totprofit_np = array(NA,c(3640))

ycorn_wp = array(NA,c(3640))
costcorn_wp = array(NA,c(3640))
ysoy_wp = array(NA,c(3640))
costsoy_wp = array(NA,c(3640))
totprofit_wp = array(NA,c(3640))

indivtaxobj = array(NA,c(3640))


#with no tax and till. 
for (j in 1:3640) { 
#	if ((!is.na(swat_input[j])))  {
		dc = 0; #since last crop was SOYB
		ycorn_np[j] = qvaluesused[j]*((a10*gam11^(dc*s2nopolicy)*gam12^(dc*s3nopolicy)*gam14^((1-dc)*s3nopolicy)) + 
			a11*dc + a12*(swat_input[j]-(row*dc)) + a13*((swat_input[j]-row*dc)^2))
		
		costcorn_np[j] = b10 + s2nopolicy*b11 + s3nopolicy*b12 + dc*(b13+s2nopolicy*b14+s3nopolicy*b15) +
				b16*ycorn_np[j] + 1.04*inputtaxesused[j]*wN*swat_input[j]
		
		dc = 1; #since last crop was CORN
		ysoy_np[j] = qvaluesused[j]*((a20*(gam21^s2nopolicy)*(gam22^s3nopolicy)) + (a21*dc) + (a22*dcc))

		costsoy_np[j] = b20 + (s2nopolicy*b21) + (s3nopolicy*b22) + (b23*ysoy_np[j])
		
		totprofit_np[j] = (p1*ycorn_np[j] + p2*ysoy_np[j]) - (costcorn_np[j] + costsoy_np[j])
		
		indivtaxobj[j] = (inputtaxesused[j]-1)*wN*swat_input[j]
#	}
}

#with tax and till. 
for (j in 1:3640) { 
#	if ((!is.na(swat_input[j])))  {
		dc = 0; #since last crop was SOYB
		ycorn_wp[j] = qvaluesused[j]*((a10*gam11^(dc*s2policy[j])*gam12^(dc*s3policy[j])*gam14^((1-dc)*s3policy[j])) + 
			a11*dc + a12*(swat_input[j]-(row*dc)) + a13*((swat_input[j]-row*dc)^2))
		
		costcorn_wp[j] = b10 + s2policy[j]*b11 + s3policy[j]*b12 + dc*(b13+s2policy[j]*b14+s3policy[j]*b15) +
				b16*ycorn_wp[j] + 1.04*inputtaxesused[j]*wN*swat_input[j]
		
		dc = 1; #since last crop was CORN
		ysoy_wp[j] = qvaluesused[j]*((a20*(gam21^s2policy[j])*(gam22^s3policy[j])) + (a21*dc) + (a22*dcc))

		costsoy_wp[j] = b20 + (s2policy[j]*b21) + (s3policy[j]*b22) + (b23*ysoy_wp[j])
		
		totprofit_wp[j] = (p1*ycorn_wp[j] + p2*ysoy_wp[j]) - (costcorn_wp[j] + costsoy_wp[j])
		
#		indivtaxobj[j] = (inputtaxesused[j]-1)*wN*swat_input[j]
#	}
}

profit_diffs = array(NA,c(3640))
#Compare profits 
for (i in 1:3640) {
	profit_diffs[i] = totprofit_wp[i]*areaused[i,1] - totprofit_np[i]*areaused[i,1]
}

basinprofit_diffs = abs(sum(profit_diffs,na.rm=TRUE))

#basinprofit = sum(totprofit*areaused[,1],na.rm=TRUE)
taxObj = sum(indivtaxobj*areaused[,1],na.rm=TRUE)
  
dfout <- data.frame(swat_input,taxObj,basinprofit_diffs)#,totprofit,basinprofit,inputtaxesused,qvaluesused,subbasinnumber,areaused)
colnames(dfout) <- c("swat_input","taxObj","basinprofit_diffs")#,"indivprofit","basinprofit","tax","q","sub","area_acres")
return(dfout)
}




getNo3Outputs <- function(swat_input) {
###################################################
########## RUN SWAT ###############################
###################################################		
#INPUTS ARE input_swat FROM PREVIOUS PORTION. 

#The .so was compiled with Intel Fortran x64. I have to invoke the 
#following system code to allow the "dyn.load" command to work.  
#system('source /opt/intel/bin/compilervars.sh intel64')

#Load SWAT as a standard object file (.so)
dyn.load('/home/blb/swat/bilevel_targeting_swat/Raccoon/src_swat/swat2009_i64_calibrate.so')

#Set directory Path to the SWAT directory
setwd("/home/blb/swat/bilevel_targeting_swat/Raccoon/swat_inputs_Raccoon/")
output <- .Fortran("swat2009", 
				vars_Rga = swat_input,
				nvars = as.integer(7280),
				rchdy2_Rga = double(731)
				)
###################################################
no3outputs = sum(output$rchdy2_Rga)/2
return(no3outputs)
}












