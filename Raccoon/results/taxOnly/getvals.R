getSwatInputandProfit <- function(input_taxes,lookupfile) {
#subbasin-scale runif(112,1,2)
#basinwide #input_taxes = array(runif(1,1,2),c(112))

lookup=lookupfile
#lookup = read.csv('/home/blb/swat/swat_sweden/lookup.csv')
q = lookup$V9
area = lookup$V10*247.105 #convert from km2 to acres

#Constants
row = 50.98 #lb/ac
dc = 0 #if previous crop is corn, this is equal to 1; 0 if otherwise. 
wN = 0.25 #$/lb from Duff and Smith (2004)
a13 = -0.0028545 #bu.ac./(lb)^2
p1 = 2.18 #$/bu Johanns (2012) year 2003 
b16 = 0.21 #$/bu 
a12 = 0.74044 #bu/lb

#Initialize Array
swat_input = double(3640) #initialize 3640 hru parameters for swat input
qvaluesused = array(NA,c(3640,1))
areaused = array(NA,c(3640,1))
inputtaxesused = array(NA,c(3640,1))
subbasinnumber = array(NA,c(3640,1))
#Calculate Fertilizer Application @ Max Profit 

#Note that this fertilizer is only applied in the CORN year of the
#CORN/SOY rotation. Therefore, dc and dcc always = 0. 
counter = 1
for (j in 1:112) { 
	sub = lookup[lookup$V2==j,]
	
	for (k in 1:length(sub$V1)) {
		if (sub$V8[k] %in% c('CORN','SOYB')) {
			swat_input[counter] = as.double(
				row*dc + ((1.04 * input_taxes[j] * wN)/(2*a13*q[counter]*(p1-b16))) +
				-(a12/(2*a13))
				)
			qvaluesused[counter,1] = q[counter]
			areaused[counter,1] = area[counter]
			inputtaxesused[counter,1] = input_taxes[j]
			subbasinnumber[counter,1] = j
			counter = counter + 1
		}
		else {
			swat_input[counter] = NA
#			subbasinnumber[counter,1] = j
			counter = counter + 1
		}
		
	}
}

#Convert from lb/ac to kg/ha using 1.12085 conversion
#swat_input[swat_input<990] = swat_input[swat_input<990]* 1.12085 
swat_input[!is.na(swat_input)] = swat_input[!is.na(swat_input)]*1.12085
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
#current config; all CORN and SOYB set to mulch till. 
s2 = 1 #1 if mulch till; 0 if otherwise
s3 = 0 #1 if no-till; 0 if otherwise

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
ycorn = array(NA,c(3640))
costcorn = array(NA,c(3640))
ysoy = array(NA,c(3640))
costsoy = array(NA,c(3640))
totprofit = array(NA,c(3640))
indivtaxobj = array(NA,c(3640))
hrunumber = array(NA,c(3640))

for (j in 1:3640) { 
#	if ((!is.na(swat_input[j])))  {
		dc = 0; #since last crop was SOYB
		ycorn[j] = qvaluesused[j]*((a10*gam11^(dc*s2)*gam12^(dc*s3)*gam14^((1-dc)*s3)) + 
			a11*dc + a12*(swat_input[j]-(row*dc)) + a13*((swat_input[j]-row*dc)^2))
		
		costcorn[j] = b10 + s2*b11 + s3*b12 + dc*(b13+s2*b14+s3*b15) +
				b16*ycorn[j] + 1.04*inputtaxesused[j]*wN*swat_input[j]
		
		dc = 1; #since last crop was CORN
		ysoy[j] = qvaluesused[j]*((a20*(gam21^s2)*(gam22^s3)) + (a21*dc) + (a22*dcc))

		costsoy[j] = b20 + (s2*b21) + (s3*b22) + (b23*ysoy[j])
		
		totprofit[j] = (p1*ycorn[j] + p2*ysoy[j]) - (costcorn[j] + costsoy[j])
		
		indivtaxobj[j] = ((inputtaxesused[j]-1)*wN*swat_input[j])*areaused[j,1]
		hrunumber[j] = j
#	}
}
indivprofit = totprofit*areaused[,1]
basinprofit = sum(indivprofit,na.rm=TRUE)
taxObj = sum(indivtaxobj,na.rm=TRUE)
  
dfout <- data.frame(swat_input,indivtaxobj,taxObj,indivprofit,basinprofit,inputtaxesused,
				qvaluesused,subbasinnumber,areaused,
				ycorn,costcorn,ysoy,costsoy,areaused,hrunumber)
colnames(dfout) <- c("swat_input_kg_ha","indivtaxObj","taxObj","indivprofit","basinprofit","tax",
					"q","sub","area_acres",
					"ycorn","costcorn","ysoy","costsoy","areas","hrunumber")
return(dfout)
}

############# SUBBASIN RESULTS
temp = data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
names(temp) <- c("indivFert","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","indivProfit","basinProfit",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn","indivCornCost",
						"indivYSoy","indivSoyCost","Subbasin","IndividualNumber","areas","hrunumber")


look = read.csv('/home/blb/swat/swat_sweden/lookup.csv')
inputs = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/outiter_100.csv')
for (i in 1:60) {
	input_taxes = as.numeric(inputs[i,3:114])
	
	out = getSwatInputandProfit(input_taxes,look) 

	#out$swat_input_kg_ha[((is.na(out$swat_input_kg_ha)) | (out$swat_input_kg_ha<0))] = 999.0

	#basinprofit = sum(out$indivprofit[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#taxObj = sum(out$taxObj[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#nitrateObj = getNo3Outputs(swat_inputs)

	finalout = data.frame(out$swat_input_kg_ha, #indiv fert
						  out$indivtaxObj,           #indiv tax
						  out$taxObj,               #basin tax
						  inputs[i,2],          #basin no3output
						  out$indivprofit,      #indiv profit
						  out$basinprofit,      #basin profit
						  out$tax,              #indiv tax multiplier
						  out$q,                #indiv q value
						  out$ycorn,            #indiv ycorn
						  out$costcorn,         #indiv corn cost
						  out$ysoy,             #indiv ysoy
						  out$costsoy,          #indiv soy cost
						  out$sub,              #subbasin number
						  i,                    #individual number
						  out$areas,            #areas
						  out$hrunumber)        #hru number       
	
	names(finalout) <- c("indivFert","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","indivProfit","basinProfit",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn","indivCornCost",
						"indivYSoy","indivSoyCost","Subbasin","IndividualNumber","areas","hrunumber")
	finalout2 = rbind(temp,finalout)
	write.csv(finalout2,"finalout.csv",quote=F,row.names=F)
	temp = finalout2
	rm(finalout,finalout2)
}
						  





############# BASIN RESULTS
temp = data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
names(temp) <- c("indivFert","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","indivProfit","basinProfit",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn","indivCornCost",
						"indivYSoy","indivSoyCost","Subbasin","IndividualNumber","areas","hrunumber")


look = read.csv('/home/blb/swat/swat_sweden/lookup.csv')
inputs = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/basinoutputs_iter_21.csv')

twen = array(seq(1,2,0.05))

for (i in 1:21) {
	input_taxes = as.numeric(rep(twen[i],112))
	
	out = getSwatInputandProfit(input_taxes,look) 

	#out$swat_input_kg_ha[((is.na(out$swat_input_kg_ha)) | (out$swat_input_kg_ha<0))] = 999.0

	#basinprofit = sum(out$indivprofit[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#taxObj = sum(out$taxObj[swat_inputs<990]*out$area_acres[swat_inputs<990],na.rm=T)
	#nitrateObj = getNo3Outputs(swat_inputs)

	finalout = data.frame(out$swat_input_kg_ha, #indiv fert
						  out$indivtaxObj,           #indiv tax
						  out$taxObj,               #basin tax
						  inputs[i,2],          #basin no3output
						  out$indivprofit,      #indiv profit
						  out$basinprofit,      #basin profit
						  out$tax,              #indiv tax multiplier
						  out$q,                #indiv q value
						  out$ycorn,            #indiv ycorn
						  out$costcorn,         #indiv corn cost
						  out$ysoy,             #indiv ysoy
						  out$costsoy,          #indiv soy cost
						  out$sub,              #subbasin number
						  i,                    #individual number
						  out$areas,            #areas
						  out$hrunumber)        #hru number  
	
	names(finalout) <- c("indivFert","indivTaxesPaid","basinTaxesPaid",
						"streamNO3output","indivProfit","basinProfit",
						"indivTaxMultiplier",
						"indivQvalue","indivYCorn","indivCornCost",
						"indivYSoy","indivSoyCost","Subbasin","IndividualNumber",
						"areas","hrunumber")
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



write.csv(out4,'addedhru/taxOnly_basin_targeting_nonNAoutputs.csv',quote=F,row.names=F)

#write.csv(out4,'addedhru/taxOnly_sub_targeting_nonNAoutputs.csv',quote=F,row.names=F)




