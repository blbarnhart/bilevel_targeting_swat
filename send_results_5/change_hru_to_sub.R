








out = read.csv('hruresults_nopolicy_1.csv')
q = aggregate(out$indivQvalue,by=list(out$Subbasin),FUN=mean)

profit_nopolicy = aggregate(out$tprofit_wp,by=list(out$Subbasin),FUN=sum)$x
fert_nopolicy = aggregate(out$indivFert_lbs_ac,by=list(out$Subbasin),FUN=mean)$x
taxes_paid_nopolicy = aggregate(out$indivTaxesPaid,by=list(out$Subbasin),FUN=mean)$x
yields_nopolicy = aggregate((out$indivYCorn_wp + out$indivYSoy_wp),by=list(out$Subbasin),FUN=mean)$x


out = read.csv('hruresults_taxtill_29.csv')
profit_taxtill = aggregate(out$tprofit_wp,by=list(out$Subbasin),FUN=sum)$x
fert_taxtill = aggregate(out$indivFert_lbs_ac,by=list(out$Subbasin),FUN=mean)$x
taxes_paid_taxtill = aggregate(out$indivTaxesPaid,by=list(out$Subbasin),FUN=sum)$x
yields_taxtill = aggregate((out$indivYCorn_wp + out$indivYSoy_wp),by=list(out$Subbasin),FUN=mean)$x


out = read.csv('hruresults_taxonly_62.csv')
profit_taxonly = aggregate(out$tprofit_wp,by=list(out$Subbasin),FUN=sum)$x
fert_taxonly = aggregate(out$indivFert_lbs_ac,by=list(out$Subbasin),FUN=mean)$x
taxes_paid_taxonly = aggregate(out$indivTaxesPaid,by=list(out$Subbasin),FUN=sum)$x
yields_taxonly = aggregate((out$indivYCorn_wp + out$indivYSoy_wp),by=list(out$Subbasin),FUN=mean)$x


out = read.csv('hruresults_tillonly_42.csv')
profit_tillonly = aggregate(out$tprofit_wp,by=list(out$Subbasin),FUN=sum)$x
fert_tillonly = aggregate(out$indivFert_lbs_ac,by=list(out$Subbasin),FUN=mean)$x
taxes_paid_tillonly = aggregate(out$indivTaxesPaid,by=list(out$Subbasin),FUN=sum)$x
yields_tillonly = aggregate((out$indivYCorn_wp + out$indivYSoy_wp),by=list(out$Subbasin),FUN=mean)$x


#data.frame(fert_nopolicy,fert_taxonly,fert_tillonly,fert_taxtill)



#profits
profit_diffs = data.frame(100*(profit_taxonly-profit_nopolicy)/profit_nopolicy,
					 100*(profit_tillonly-profit_nopolicy)/profit_nopolicy,
					 100*(profit_taxtill-profit_nopolicy)/profit_nopolicy)
					 
names(profit_diffs) <- c("ptaxonly","ptillonly","ptaxtill")
profit_diffs #$ to %


#fertilizer
fert_lb_ac = data.frame(fert_nopolicy,fert_taxonly,fert_tillonly,fert_taxtill)
fert_diffs = data.frame(100*(fert_taxonly-fert_nopolicy)/fert_nopolicy,
						 100*(fert_tillonly-fert_nopolicy)/fert_nopolicy,
						 100*(fert_taxtill-fert_nopolicy)/fert_nopolicy)
names(fert_diffs) <- c("ferttaxonly","ferttillonly","ferttaxtill")
fert_diffs #lb/ac to %


#yields
yields_bu_ac = data.frame(yields_nopolicy,yields_taxonly,yields_tillonly,yields_taxtill)
yield_diffs = data.frame(100*(yields_taxonly-yields_nopolicy)/yields_nopolicy,
						 100*(yields_tillonly-yields_nopolicy)/yields_nopolicy,
						 100*(yields_taxtill-yields_nopolicy)/yields_nopolicy)
names(yield_diffs) <- c("ytaxonly","ytillonly","ytaxtill")
yield_diffs #bu/ac to %

						 

#taxes paid 
taxes_nopolicy = 0 
taxes_paid_tillonly = 0
taxespaid = data.frame(taxes_paid_nopolicy,taxes_paid_taxonly,taxes_paid_tillonly,taxes_paid_taxtill)
names(taxespaid) <- c("tp_nopolicy","tptaxonly","tptillonly","tptaxtill")
taxespaid #$/ac


write.csv(profit_diffs,'profit_percentages.csv',quote=F,row.names=F)
write.csv(yield_diffs,'yield_percentages.csv',quote=F,row.names=F)
write.csv(fert_diffs,'fert_percentages.csv',quote=F,row.names=F)























