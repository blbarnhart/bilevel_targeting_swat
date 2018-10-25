




out = read.csv('finalout.csv')

out2 = out[2:length(out$indivFert),]


#for (i in 1:length(out2$indivFert)) {
#	if (!is.na(out2$indivFert[i]) < 0) (out2[i,] = NA)
#}


out3 = out2[complete.cases(out2), ]

out3[out3$indivFert <0,] = NA

out4 = out3[complete.cases(out3), ]



write.csv(out4,'taxOnly_basin_targeting_nonNAoutputs.csv',quote=F,row.names=F)

#write.csv(out4,'taxOnly_sub_targeting_nonNAoutputs.csv',quote=F,row.names=F)



par(mfrow=c(3,2))
hist(out4$indivFert)
