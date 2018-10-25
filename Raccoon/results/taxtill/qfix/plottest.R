



pdf("comparison.pdf")
plot(out2$V1,out2$V2,xlim=c(0,1.75e7))
points(out2_old$V1,out2_old$V2,col='blue')
points(out$V1,out$V2,col='red')
points(out_old$V1,out_old$V2,col='green')

legend('topright',
	c("taxOnly_qfix","taxOnly_old","tax+till_qfix","tax+till_old"), 
	col=c('black','blue','red','green'),pch=1)
dev.off()

