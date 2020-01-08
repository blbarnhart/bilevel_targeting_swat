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
swat_input_nopolicy = array(NA,c(3640))
qvaluesused = array(NA,c(3640,1))
areaused = array(NA,c(3640,1))
inputtaxesused = array(NA,c(3640,1))
inputtillsused = array(NA,c(3640,1))
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
			swat_input_nopolicy[counter] = as.double(
				row*dc + ((1.04 * 1.0 * wN)/(2*a13*q[counter]*(p1-b16))) +
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
			inputtillsused[counter,1]= input_taxes_tills[j+3640]
			subbasinnumber[counter,1] = j
			counter = counter + 1
		}
		else {
			swat_input[counter] = NA
			swat_input[counter+3640] = NA
#			subbasinnumber[counter,1] = j
			swat_input_nopolicy[counter] = NA
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

for (i in 1:3640) {
	if (!is.na(swat_input_nopolicy[i])) { 
		swat_input_nopolicy[i] = swat_input_nopolicy[i]*1.12085
		if (swat_input_nopolicy[i] < 0) { swat_input_nopolicy[i]=NA }
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
		ycorn_np[j] = qvaluesused[j,1]*((a10*gam11^(dc*s2nopolicy)*gam12^(dc*s3nopolicy)*gam13^((1-dc)*s2nopolicy)*gam14^((1-dc)*s3nopolicy)) + 
			a11*dc + a12*(swat_input[j]-(row*dc)) + a13*((swat_input[j]-row*dc)^2))
		
		costcorn_np[j] = b10 + s2nopolicy*b11 + s3nopolicy*b12 + dc*(b13+s2nopolicy*b14+s3nopolicy*b15) +
				b16*ycorn_np[j] + 1.04*inputtaxesused[j]*wN*swat_input[j]
		
		dc = 1; #since last crop was CORN
		ysoy_np[j] = qvaluesused[j,1]*((a20*(gam21^s2nopolicy)*(gam22^s3nopolicy)) + (a21*dc) + (a22*dcc))

		costsoy_np[j] = b20 + (s2nopolicy*b21) + (s3nopolicy*b22) + (b23*ysoy_np[j])
		
		totprofit_np[j] = (p1*ycorn_np[j] + p2*ysoy_np[j]) - (costcorn_np[j] + costsoy_np[j])
		
		indivtaxobj[j] = (inputtaxesused[j]-1)*wN*swat_input[j]
#	}
}

#with tax and till. 
for (j in 1:3640) { 
#	if ((!is.na(swat_input[j])))  {
		dc = 0; #since last crop was SOYB
		ycorn_wp[j] = qvaluesused[j,1]*((a10*gam11^(dc*s2policy[j])*gam12^(dc*s3policy[j])*gam13^((1-dc)*s2policy)*gam14^((1-dc)*s3policy[j])) + 
			a11*dc + a12*(swat_input[j]-(row*dc)) + a13*((swat_input[j]-row*dc)^2))
		
		costcorn_wp[j] = b10 + s2policy[j]*b11 + s3policy[j]*b12 + dc*(b13+s2policy[j]*b14+s3policy[j]*b15) +
				b16*ycorn_wp[j] + 1.04*inputtaxesused[j]*wN*swat_input[j]
		
		dc = 1; #since last crop was CORN
		ysoy_wp[j] = qvaluesused[j,1]*((a20*(gam21^s2policy[j])*(gam22^s3policy[j])) + (a21*dc) + (a22*dcc))

		costsoy_wp[j] = b20 + (s2policy[j]*b21) + (s3policy[j]*b22) + (b23*ysoy_wp[j])
		
		totprofit_wp[j] = (p1*ycorn_wp[j] + p2*ysoy_wp[j]) - (costcorn_wp[j] + costsoy_wp[j])
		
#		indivtaxobj[j] = (inputtaxesused[j]-1)*wN*swat_input[j]
#	}
}

profit_diffs = array(NA,c(3640))
dollars_spenton_taxes = array(NA,c(3640))
profit_diffs_out = array(NA,c(3640))
#tprofit_wp_out = array(NA,c(3640))
#tprofit_np_out = array(NA,c(3640))
yield_diffs = array(NA,c(3640))
fert_diffs = array(NA,c(3640))
inputtaxesused_out = array(NA,c(3640))


#Compare profits 
profit_diffs = array(NA,c(3640))
#Compare profits 
for (i in 1:3640) {
	profit_diffs[i] = totprofit_wp[i]*areaused[i,1] - totprofit_np[i]*areaused[i,1]
}

basinprofit_diffs = abs(sum(profit_diffs,na.rm=TRUE))

#basinprofit = sum(totprofit*areaused[,1],na.rm=TRUE)
taxObj = sum(indivtaxobj*areaused[,1],na.rm=TRUE)
 
#for (i in 1:3640) {
#	dollars_spenton_taxes[i] = indivtaxobj[i]*areaused[i,1] #$ spent on taxes.
#	
#	profit_diffs[i] = ( (((totprofit_wp[i]*areaused[i,1])/2) + dollars_spenton_taxes[i]) - 
#							(((totprofit_np[i]*areaused[i,1])/2) + 0) ) / 
#									(((totprofit_np[i]*areaused[i,1])/2) + 0) #%diff
#
#	tprofit_wp_out[i] = totprofit_wp[i]*areaused[i,1]
#	tprofit_np_out[i] = totprofit_np[i]*areaused[i,1]
#	yield_diffs[i] = (((ycorn_wp[i] + ysoy_wp[i])*areaused[i,1]) - 
#						((ycorn_np[i] + ysoy_np[i])*areaused[i,1])) / 
#							((ycorn_np[i] + ysoy_np[i])*areaused[i,1])) # changes in $. Need to divide by area.
#	fert_diffs[i] = ((swat_input[i]/1.12085)*(areaused[i,1])) - ((swat_input_nopolicy[i]/1.12085)*(areaused[i,1])) #changes in lbs. need to divide by area. maybe. 
#	inputtaxesused_out[i] = inputtaxesused[i,1] #tax multiplier between 1 and 2. 
#	inputtillsused_out[i] = inputtillsused[i,1]
#}

#basinprofit_diffs = abs(sum(profit_diffs,na.rm=TRUE))

 #basinprofit = sum(totprofit*areaused[,1],na.rm=TRUE)
#taxObj = sum(indivtaxobj*areaused[,1],na.rm=TRUE)
 
 
#indivprofit = totprofit*areaused[,1]
#basinprofit = sum(indivprofit,na.rm=TRUE)
#taxObj = sum(indivtaxobj,na.rm=TRUE)
#basinprofit_diffs = sum(profit_diffs,na.rm=TRUE)   


dfout <- data.frame(qvaluesused[,1],areaused[,1],subbasinnumber,seq(1,3640,1),
			totprofit_wp*areaused[,1],ycorn_wp*areaused[,1], ysoy_wp*areaused[,1], 
			(ycorn_wp + ysoy_wp)*areaused[,1],(swat_input[1:3640]/1.12085)*areaused[,1], 
			indivtaxobj*areaused[,1]) 
			
colnames(dfout) <- c("qvalues","area_ac","sub","hru",
						"totprofit_dols_yr","ycorn_bu","ysoy_bu","ycornsoy_bu","fert_lb","taxes_dols")



#dfout <- data.frame(inputtaxesused_out,inputtillsused_out,
#			qvaluesused,areaused,subbasinnumber,hrunumber,
#			yield_diffs, fert_diffs, profit_diffs_out,dollars_spenton_taxes)
			
#colnames(dfout) <- c("taxmultiplier","tillvalues",
#			"qvalues","areaused","sub","hru","yield_diffs_lb

 
#dfout <- data.frame(swat_input[1:3640],swat_input[3641:7280],
#				indivtaxobj,taxObj,tprofit_wp_out,tprofit_np_out,profit_diffs,basinprofit_diffs,inputtaxesused,
#				qvaluesused,subbasinnumber,areaused,
#				ycorn_wp,ycorn_np,costcorn_wp,costcorn_np,ysoy_wp,ysoy_np,costsoy_wp,costsoy_np,areaused,hrunumber)
#colnames(dfout) <- c("swat_input_kg_ha","swat_input_till",
#				"indivtaxObj","taxObj","tprofit_wp","tprofit_np","profit_diffs","basinprofit_diffs","tax",
#					"q","sub","area_acres",
#					"ycorn_wp","ycorn_np","costcorn_wp","costcorn_np","ysoy_wp","ysoy_np",
#					"costsoy_wp","costsoy_np","areas","hrunumber")



#dfout <- data.frame(swat_input,indivtaxobj,taxObj,indivprofit,basinprofit,inputtaxesused,
#				qvaluesused,subbasinnumber,areaused,
#				ycorn,costcorn,ysoy,costsoy,areaused)
#colnames(dfout) <- c("swat_input_kg_ha","indivtaxObj","taxObj","indivprofit","basinprofit","tax",
#					"q","sub","area_acres",
#					"ycorn","costcorn","ysoy","costsoy","areas")
return(dfout)

#dfout <- data.frame(swat_input,taxObj,basinprofit_diffs)#,totprofit,basinprofit,inputtaxesused,qvaluesused,subbasinnumber,areaused)
#colnames(dfout) <- c("swat_input","taxObj","basinprofit_diffs")#,"indivprofit","basinprofit","tax","q","sub","area_acres")
#return(dfout)




}



######## Get outputs for No Policy 
#Outputs === 2019-07-05
look = read.csv('/home/blb/swat/swat_sweden/lookup_qfix.csv')
inputs = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_taxtill_nondom_results5.csv')

i = 1
input_taxes_tills = as.numeric(inputs[i,3:226])
out = getSwatInputandProfit(input_taxes_tills,look) 
out2 = out[complete.cases(out), ]

write.csv(out2,"hruresults_taxtill_nopolicy_1.csv",quote=F,row.names=F)


######## Get outputs for tax+till at point 29 (for spatial plotting).
#Outputs === 2019-07-05
look = read.csv('/home/blb/swat/swat_sweden/lookup_qfix.csv')
inputs = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_taxtill_nondom_results5.csv')

i = 29
input_taxes_tills = as.numeric(inputs[i,3:226])
out = getSwatInputandProfit(input_taxes_tills,look) 
out2 = out[complete.cases(out), ]

write.csv(out2,"hruresults_taxtill_single_point_29.csv",quote=F,row.names=F)


######## Get outputs for tax only at point 62 (for spatial plotting).
#Outputs === 2019-07-05
look = read.csv('/home/blb/swat/swat_sweden/lookup_qfix.csv')
inputs = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_taxonly_nondom_results5.csv')

i = 62
first = as.numeric(inputs[i,3:114])
second = rep(0.1,112)
input_taxes_tills = c(first,second)
out = getSwatInputandProfit(input_taxes_tills,look) 
out2 = out[complete.cases(out), ]

write.csv(out2,"hruresults_taxonly_single_point_62.csv",quote=F,row.names=F)


######## Get outputs for till only at point 42  (for spatial plotting).
#Outputs === 2019-07-05
look = read.csv('/home/blb/swat/swat_sweden/lookup_qfix.csv')
inputs = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_tillonly_nondom_results5.csv')

i = 42
first = as.numeric(inputs[i,3:114])
second = rep(0.1,112)
input_taxes_tills = c(first,second)
out = getSwatInputandProfit(input_taxes_tills,look) 
out2 = out[complete.cases(out), ]

write.csv(out2,"hruresults_tillonly_single_point_42.csv",quote=F,row.names=F)



#Use this to pick out particular point from frontier by ordering them. 
#out = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_taxtill_nondom_results5.csv')
#out = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_taxonly_nondom_results5.csv')
out = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_tillonly_nondom_results5.csv')


#3578353, 9058067 tax only
#104925.47, 9057305 till only
#62264.14, 9058335 tax+till
out[order(out$V2),c(1,2)]












############# SUBBASIN RESULTS send-results-5
#temp = data.frame(1,1,1,1,1,1,1,1,1,1,1)
#names(temp) <- c("i","qvalues","area_ac","sub","hru",
#						"totprofit_dols_yr","ycorn_bu","ysoy_bu","ycornsoy_bu","fert_lb","taxes_dols")
#
#look = read.csv('/home/blb/swat/swat_sweden/lookup_qfix.csv')
#inputs = read.csv('/home/blb//swat/swat_sweden/results/send_results_5/objs_taxtill_nondom_results5.csv')
#
#for (i in 1:120) {
#	input_taxes_tills = as.numeric(inputs[i,3:226])
#	
#	out = getSwatInputandProfit(input_taxes_tills,look) 
#
#	finalout = data.frame(i,out)
#	
#	finalout2 = rbind(temp,finalout)
#	
#	write.csv(finalout2,"finalout.csv",quote=F,row.names=F)
#	temp = finalout2
#	rm(finalout,finalout2)
#}
						  













############# SUBBASIN RESULTS
temp = data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
names(temp) <- c("indivFert","indivTill","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","tprofit_wp","tprofit_np","profit_diffs","basinprofit_diffs",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn_wp","indivYCorn_np",
						"indivCornCost_wp","indivCornCost_np",
						"indivYSoy_wp","indivYSoy_np","indivSoyCost_wp","indivSoyCost_np",
						"Subbasin","IndividualNumber","areas","hrunumber")

look = read.csv('/home/blb/swat/swat_sweden/lookup_qfix.csv')
inputs = read.csv('/home/blb//swat/swat_sweden/results/taxtill/jerry_4_120/outiter_50.csv')
for (i in 1:120) {
	input_taxes_tills = as.numeric(inputs[i,3:226])
	
	out = getSwatInputandProfit(input_taxes_tills,look) 

	#out$swat_input_kg_ha[((is.na(out$swat_input_kg_ha)) | (out$swat_input_kg_ha<0))] = 999.0

	#basinprofit = sum(out$indivprofit[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#taxObj = sum(out$taxObj[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#nitrateObj = getNo3Outputs(swat_inputs)

	finalout = data.frame(out$swat_input_kg_ha, #indiv fert
						  out$swat_input_till,
						  out$indivtaxObj,           #indiv tax
						  out$taxObj,               #basin tax
						  inputs[i,2],          #basin no3output
						  out$tprofit_wp,
						  out$tprofit_np,
						  out$profit_diffs,      #indiv profit
						  out$basinprofit_diffs,      #basin profit
						  out$tax,              #indiv tax multiplier
						  out$q,                #indiv q value
						  out$ycorn_wp,            #indiv ycorn
						  out$ycorn_np,
						  out$costcorn_wp,         #indiv corn cost
						  out$costcorn_np,
						  out$ysoy_wp,             #indiv ysoy
						  out$ysoy_np,
						  out$costsoy_wp,          #indiv soy cost
						  out$costsoy_np,
						  out$sub,              #subbasin number
						  i,                    #individual number
						  out$areas,            #areas  
						  out$hrunumber)        #hrunumber
	
	names(finalout) <- c("indivFert","indivTill","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","tprofit_wp","tprofit_np","profit_diffs","basinprofit_diffs",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn_wp","indivYCorn_np",
						"indivCornCost_wp","indivCornCost_np",
						"indivYSoy_wp","indivYSoy_np",
						"indivSoyCost_wp","indivSoyCost_np",
						"Subbasin","IndividualNumber","areas","hrunumber")
	finalout2 = rbind(temp,finalout)
	write.csv(finalout2,"finalout.csv",quote=F,row.names=F)
	temp = finalout2
	rm(finalout,finalout2)
}
						  



############# BASIN RESULTS
temp = data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
names(temp) <- c("indivFert","indivTill","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","profit_diffs","basinprofit_diffs",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn_wp","indivYCorn_np",
						"indivCornCost_wp","indivCornCost_np",
						"indivYSoy_wp","indivYSoy_np","indivSoyCost_wp","indivSoyCost_np",
						"Subbasin","IndividualNumber","areas","hrunumber")

look = read.csv('/home/blb/swat/swat_sweden/lookup.csv')
basinnotillobjs = read.csv('~/swat/swat_sweden/taxtill_basin_notill.csv')
basinwithtillobjs = read.csv('~/swat/swat_sweden/taxtill_basin_withtill.csv')

twen = array(seq(1.0,2.0,.05),dim=c(21,1))

for (i in 1:21) {
	input_taxes_tills = c(rep(twen[i,1],112),rep(0.0,112))
	
	out = getSwatInputandProfit(input_taxes_tills,look) 

	#out$swat_input_kg_ha[((is.na(out$swat_input_kg_ha)) | (out$swat_input_kg_ha<0))] = 999.0

	#basinprofit = sum(out$indivprofit[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#taxObj = sum(out$taxObj[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#nitrateObj = getNo3Outputs(swat_inputs)

	finalout = data.frame(out$swat_input_kg_ha, #indiv fert
						  out$swat_input_till,
						  out$indivtaxObj,           #indiv tax
						  out$taxObj,               #basin tax
						  basinwithtillobjs[i,2],          #basin no3output
						  out$profit_diffs,      #indiv profit
						  out$basinprofit_diffs,      #basin profit
						  out$tax,              #indiv tax multiplier
						  out$q,                #indiv q value
						  out$ycorn_wp,            #indiv ycorn
						  out$ycorn_np,
						  out$costcorn_wp,         #indiv corn cost
						  out$costcorn_np,
						  out$ysoy_wp,             #indiv ysoy
						  out$ysoy_np,
						  out$costsoy_wp,          #indiv soy cost
						  out$costsoy_np,
						  out$sub,              #subbasin number
						  i,                    #individual number
						  out$areas,            #areas
						  out$hrunumber)        #hru number       
	
	names(finalout) <- c("indivFert","indivTill","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","profit_diffs","basinprofit_diffs",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn_wp","indivYCorn_np",
						"indivCornCost_wp","indivCornCost_np",
						"indivYSoy_wp","indivYSoy_np",
						"indivSoyCost_wp","indivSoyCost_np",
						"Subbasin","IndividualNumber","areas","hrunumber")
	finalout2 = rbind(temp,finalout)
	write.csv(finalout2,"finalout.csv",quote=F,row.names=F)
	temp = finalout2
	rm(finalout,finalout2)
	
	input_taxes_tills = c(rep(twen[i,1],112),rep(1.0,112))
	
	out = getSwatInputandProfit(input_taxes_tills,look) 

	#out$swat_input_kg_ha[((is.na(out$swat_input_kg_ha)) | (out$swat_input_kg_ha<0))] = 999.0

	#basinprofit = sum(out$indivprofit[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#taxObj = sum(out$taxObj[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#nitrateObj = getNo3Outputs(swat_inputs)

	finalout = data.frame(out$swat_input_kg_ha, #indiv fert
						  out$swat_input_till,
						  out$indivtaxObj,           #indiv tax
						  out$taxObj,               #basin tax
						  basinnotillobjs[i,2],          #basin no3output
						  out$profit_diffs,      #indiv profit
						  out$basinprofit_diffs,      #basin profit
						  out$tax,              #indiv tax multiplier
						  out$q,                #indiv q value
						  out$ycorn_wp,            #indiv ycorn
						  out$ycorn_np,
						  out$costcorn_wp,         #indiv corn cost
						  out$costcorn_np,
						  out$ysoy_wp,             #indiv ysoy
						  out$ysoy_np,
						  out$costsoy_wp,          #indiv soy cost
						  out$costsoy_np,
						  out$sub,              #subbasin number
						  i,                    #individual number
						  out$areas,            #areas
						  out$hrunumber)       
	
	names(finalout) <- c("indivFert","indivTill","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","profit_diffs","basinprofit_diffs",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn_wp","indivYCorn_np",
						"indivCornCost_wp","indivCornCost_np",
						"indivYSoy_wp","indivYSoy_np",
						"indivSoyCost_wp","indivSoyCost_np",
						"Subbasin","IndividualNumber","areas","hrunumber")
	finalout2 = rbind(temp,finalout)
	write.csv(finalout2,"finalout.csv",quote=F,row.names=F)
	temp = finalout2
	rm(finalout,finalout2)
}
						  





#RUN THIS AFTER
out = read.csv('finalout.csv')

out$indivFert_lbs_ac = out$indivFert*0.892179

out2 = out[-1,]


#for (i in 1:length(out2$indivFert)) {
#	if (!is.na(out2$indivFert[i]) < 0) (out2[i,] = NA)
#}


out3 = out2[complete.cases(out2), ]

out3[out3$indivFert <0,] = NA

out4 = out3[complete.cases(out3), ]



write.csv(out4,'nov_taxtill_subbasin_targeting_nonNAoutputs.csv',quote=F,row.names=F)

write.csv(out4,'addhru/taxtill_subbasin_targeting_nonNAoutputs.csv',quote=F,row.names=F)














