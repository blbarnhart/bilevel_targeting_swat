







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
