


#new tax only 50 gen
out1 = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/qfix/outiter_50.csv')

#old tax only 100 gen
out2 = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/outiter_100.csv')

#new taxtill 50gen
out3 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/qfix/outiter_taxtill_50.csv')

#old taxtill 100gen
out4 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/taxtill_gen100.csv')

#new taxtill 100genUpdate
out5 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/qfix/update/outiter_56.csv')

out10 = read.csv('/home/blb/swat/swat_sweden/tillonly/outiter_10.csv')

#pdf("comparison_50_100gen_updated.pdf")

plot(out1$V1, out1$V2, col='black', xlim=c(0, 1.75e7))
points(out2$V1, out2$V2, col='blue')
points(out3$V1, out3$V2, col='red')
points(out4$V1, out4$V2, col='green')

out3_100gen = read.csv('/home/blb/swat/swat_sweden/results/taxtill/qfix/100_gen/taxtill_2_100.csv')

points(out3_100gen$V1,out3_100gen$V2,pch=19,col='red')

points(out5$V1,out5$V2, pch=19, col='orange')

points(out10[,1],out10[,2],col='brown',pch=19)


legend('topright',c('tax only - qfix - 50 gens',
					'tax only - old - 100 gens',
					'tax+till - old - 100 gens',
					'tax+till - qfix - 50 gens',
					'tax+till - qfix - 100 gens',
					'tax+till - qfix - 56 gens updated'),
					col = c('black','blue','green','red','red','orange'),
					pch = c(1,1,1,1,19,19)
					) 
					
dev.off()




################################



#new tax only 50 gen
#out1 = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/qfix/outiter_50.csv')
out1 = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/taxonly_2/outiter_50.csv')

#old tax only 100 gen
#out2 = read.csv('/home/blb/swat/swat_sweden/results/taxOnly/outiter_100.csv')

#new taxtill 50gen
#out3 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/qfix/outiter_taxtill_50.csv')

#out3_100gen = read.csv('/home/blb/swat/swat_sweden/results/taxtill/qfix/100_gen/taxtill_2_100.csv')

#old taxtill 100gen
#out4 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/taxtill_gen100.csv')

#new taxtill 56genUpdate
out5 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/qfix/update/outiter_56.csv')

#new tillonly 10gen
#out10 = read.csv('/home/blb/swat/swat_sweden/tillonly/outiter_10.csv')
out50 = read.csv('/home/blb/swat/swat_sweden/results/tillonly/outiter_50.csv')

#new injection tax+till results 
out11 =read.csv('/home/blb/swat/swat_sweden/results/injection/outiter_8.csv')
out41 =read.csv('/home/blb/swat/swat_sweden/results/injection/next/outiter_41.csv')


#taxtill output from jerry3 using taxtill src. Note that doesn't seem to match the tillonly.
#FOUND MISTAKE IN JERRY3 == IT IS FIXED IN JERRY4
out61 = read.csv('/home/blb/swat/swat_sweden/results/taxtill/jerry_3/outiter_50.csv')


#test of taxtill output from jerry4 using tillonly src with injections. 
#THIS INCLUDES FIX FROM ABOVE
out71 = read.csv('/home/blb/swat/swat_sweden/taxtill_jerry4/outiter_1.csv')


out1$V2[1] = 9203057

#pdf("policy_comparison_afterInjection_2018-10-23.pdf")

#pdf("/media/blb/New\ Volume/Brad/policy_comparison_afterInjection_2018-10-23.pdf")

plot(out50[,1],format(out50[,2],scientific=TRUE),col='brown',pch=19,
	xlim=c(0, 1.5e7),ylim=c(8.2e6,9.3e6),
	xlab="Policy Cost (USD)", ylab="Nitrogen Loadings")
points(out1$V1, out1$V2, col='pink',pch=19)
points(out61$V1, out61$V2, col='orange', pch=19)
#points(out5$V1,out5$V2, pch=19, col='orange')
#points(out41$V1,out41$V2, pch=19, col='orange')
points(out71$V1, out71$V2, col='black',pch=19)



legend('topright',c('till only',
					'tax only',
					'tax + till'),
					col = c('brown','black','orange'),
					pch = c(19,19,19)
					) 
					
#dev.off()



#new injection tax+till results 
i1 = read.csv('outiter_1.csv')
i2 = read.csv('outiter_2.csv')
i3 = read.csv('outiter_3.csv')
i4 = read.csv('outiter_4.csv')
i5 = read.csv('outiter_5.csv')
i6 = read.csv('outiter_6.csv')
i7 = read.csv('outiter_7.csv')
i8 = read.csv('outiter_8.csv')

plot(i1$V1,i1$V2)
points(i2$V1,i2$V2)
points(i3$V1,i3$V2)
points(i4$V1,i4$V2)
points(i5$V1,i5$V2)
points(i6$V1,i6$V2)
points(i7$V1,i7$V2)
points(i8$V1,i8$V2,col='red',pch=19)










