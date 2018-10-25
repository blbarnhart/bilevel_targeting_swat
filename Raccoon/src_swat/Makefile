# SWAT MAKEFILE FOR INTEL COMPILER
#
F95=ifort
#FFLAGS= -c -g -fPIC -fpe:0 -check bounds -check pointer -debug all
#FFLAGS=-O -fpe:0 -threads
#LDFLAGS=-static $(FFLAGS)
## use for debugging
FFLAGS= -c -g -fpe:0 -align sequence -fPIC -threads -fltconsistency -real-size 64 -debug all
LDFLAGS= -g -shared -fPIC -shared-intel -heap-arrays

##
## use for optimizing
#FFLAGS= -c -align sequence -fPIC -threads -fltconsistency -real-size 64
#LDFLAGS= -shared -fPIC -shared-intel -heap-arrays
###########################################################################
##### swat2009_i64_calibrate and Komar options
###########################################################################
##FFLAGS= -c -align sequence -fPIC -threads -fltconsistency -real-size 64
## swat2009_i64_calibrate options
##FFLAGS= -O2 -ipo -debug none -fpe:0 -fp-speculation strict -zero
## Komar options
##FFLAGS= -O2 -ipo -debug none -fpe:0 -fp-model precise -zero
###########################################################################

### N.B. -ipo WILL PREVENT LOADING INTO R
##########################################
## exp. 1  -fp-model precise -zero (from Komar) ; have to drop '-real-size 64 '
#FFLAGS= -c -g -fpe:0 -align sequence -fPIC -threads -fltconsistency -debug all -fp-model precise -zero
## FAILS ##FFLAGS= -c -g -fPIC -debug none -fpe:0 -fp-model precise -zero

## FAILS ## FFLAGS= -c -g -fPIC -debug none -fpe:0 -fp-speculation strict -zero

#FFLAGS= -c -fPIC -fpe:0 -ipo
##FFLAGS= -ipo-c -fPIC -fpe:0
##LDFLAGS= -g -shared -fPIC -shared-intel -heap-arrays

## delete readatmodep.f
SRC= main.n2.f\
     freearray_09.f\
     addh.f\
     albedo.f\
     allocate_parms.f\
     alph.f\
     analyse.f\
     anfert.f\
     apex_day.f\
     apply.f\
     ascrv.f\
     atri.f\
     aunif.f\
     autocal.f\
     autoirr.f\
     automet.f\
     aveval.f\
     bacteria.f\
     batchin.f\
     batchmain.f\
     bayes.f\
     BIOZONE.F\
     buffer.f\
     burnop.f\
     calresidu.f\
     canopyint.f\
     caps.f\
     carbon_new.f\
     changepar.f\
     chkcst.f\
     clgen.f\
     clicon.f\
     command.f\
     conapply.f\
     confert.f\
     copyfile.f\
     countobs.f\
     crackflow.f\
     crackvol.f\
     curno.f\
     dailycn.f\
     decay.f\
     dormant.f\
     dstn1.f\
     ee.f\
     eiusle.f\
     enrsb.f\
     erfc.f\
     estimate_ksat.f\
     etact.f\
     etpot.f\
     expo.f\
     fert.f\
     filter.f\
     filtw.f\
     finalbal.f\
     functn.f\
     gasdev.f\
     gcycl.f\
     getallo2.f\
     getallo.f\
     getpnt2.f\
     getpnt.f\
     goc.f\
     grass_wway.f\
     graze.f\
     grow.f\
     gwmod.f\
     gw_no3.f\
     gwnutr.f\
     h2omgt_init.f\
     harvestop.f\
     harvgrainop.f\
     harvkillop.f\
     header.f\
     headout.f\
     hhnoqual.f\
     hhwatqual.f\
     hmeas.f\
     hruaa.f\
     hruallo.f\
     hruday.f\
     hrumon.f\
     hrupond.f\
     hruyr.f\
     hydroinit.f\
     impndaa.f\
     impndday.f\
     impnd_init.f\
     impndmon.f\
     impndyr.f\
     indexx.f\
     irrigate.f\
     irr_rch.f\
     irr_res.f\
     irrsub.f\
     jdt.f\
     killop.f\
     lakeq.f\
     latsed.f\
     layersplit.f\
     lwqdef.f\
     ndenit.f\
     newtillmix.f\
     nfix.f\
     nitvol.f\
     nlch.f\
     nminrl.f\
     noqual.f\
     npup.f\
     nrain.f\
     nup.f\
     nuts.f\
     oat.f\
     objfunctn.f\
     openwth.f\
     operatn.f\
     orgncswat.f\
     orgn.f\
     parasolc.f\
     parasol.f\
     parasoli.f\
     parasolo.f\
     parasolu.f\
     parstt2.f\
     percmacro.f\
     percmain.f\
     percmicro.f\
     pestlch.f\
     pestw.f\
     pesty.f\
     pgen.f\
     pgenhr.f\
     pkq.f\
     plantmod.f\
     plantop.f\
     pmeas.f\
     pminrl.f\
     pond.f\
     pothole.f\
     psed.f\
     qman.f\
     ran1.f\
     ranked.f\
     rchaa.f\
     rchday.f\
     rchinit.f\
     rchmon.f\
     rchuse.f\
     rchyr.f\
     readatmodep.f\
     readbsn.f\
     readchan.f\
     readchm.f\
     readcnst.f\
     readfcst.f\
     readfert.f\
     readfig.f\
     readfile.f\
     readgw.f\
     readhru.f\
     readinpt.f\
     readlup.f\
     readlwq.f\
     readmetf.f\
     readmgt.f\
     readmon.f\
     readops.f\
     readpest.f\
     readplant.f\
     readpnd.f\
     readres.f\
     readrte.f\
     readseptic.f\
     readseptwq.f\
     readsol.f\
     readsub.f\
     readswq.f\
     readtill.f\
     readurban.f\
     readwgn.f\
     readwus.f\
     readwwq.f\
     readyr.f\
     reccnst.f\
     recday.f\
     rechour.f\
     recmon.f\
     recyear.f\
     regres.f\
     rerun.f\
     rerunfile.f\
     rerunps.f\
     resbact.f\
     resetlu.f\
     res.f\
     resinit.f\
     resnut.f\
     response.f\
     rewind_init.f\
     rhgen.f\
     rootfr.f\
     route.f\
     routres.f\
     rsedaa.f\
     rseday.f\
     rsedmon.f\
     rsedyr.f\
     rtbact.f\
     rtday.f\
     rteinit.f\
     rthmusk.f\
     rthourly.f\
     rthpest.f\
     rthsed.f\
     rtmusk.f\
     rtout.f\
     rtover.f\
     rtpest.f\
     rtsed_bagnold.f\
     rtsed.f\
     rtsed_kodatie.f\
     rtsed_Molinas_Wu.f\
     rtsed_yangsand.f\
     sample1.f\
     sample.f\
     sat_excess.f\
     saveconc.f\
     save.f\
     scestat.f\
     schedule_ops.f\
     sensin.f\
     sensmain.f\
     sim_initday.f\
     sim_inityr.f\
     simulate.f\
     slrgen.f\
     smeas.f\
     snom.f\
     soil_chem.f\
     soil_par.f\
     soil_phys.f\
     soil_write.f\
     solp.f\
     solt.f\
     sort1.f\
     sort3.f\
     sorteer2.f\
     sorteer3.f\
     sorteer4.f\
     sorteer5.f\
     sorteer6.f\
     sorteer.f\
     std1.f\
     std2.f\
     std3.f\
     stdaa.f\
     storeinitial.f\
     structure.f\
     subaa.f\
     subbasin.f\
     subday.f\
     submon.f\
     substor.f\
     subwq.f\
     subyr.f\
     sumv.f\
     sunglasr.f\
     sunglass.f\
     sunglasu.f\
     surface.f\
     surfst_h2o.f\
     surfstor.f\
     surq_daycn.f\
     surq_greenampt.f\
     swbl.f\
     sweep.f\
     swu.f\
     tair.f\
     telobjre.f\
     telobs.f\
     telpar.f\
     tgen.f\
     theta.f\
     tillfactor.f\
     tillmix.f\
     tmeas.f\
     tran.f\
     transfer.f\
     tstr.f\
     ttcoef.f\
     urban.f\
     varinit.f\
     vbl.f\
     virtual.f\
     volq.f\
     vrval.f\
     washp.f\
     watbal.f\
     watqual2.f\
     watqual.f\
     wattable.f\
     watuse.f\
     weatgn.f\
     wetlan.f\
     wmeas.f\
     wndgen.f\
     writeaa.f\
     writea.f\
     writed.f\
     writem.f\
     writeswatfile.f\
     writeswatmain.f\
     xisquare.f\
     xiunc.f\
     xmon.f\
     ysed.f\
     zero0.f\
     zero1.f\
     zero2.f\
     zeroini.f

MOD=parm.mod

EXE=swat2009

OBJ=$(SRC:.f=.o)

#all: $(EXE)
all: $(OBJ) 
	#ifort $(LDFLAGS) -o swat2009_i64_calibrate.so $(OBJ)
	# debug 
	ifort $(LDFLAGS) -g -o swat2009_i64_calibrate.so $(OBJ) 

clean:
	rm -f *.o parm.mod swat2009_i64_calibrate.so

$(MOD): modparm.f
	$(F95) $(FFLAGS) $<

$(OBJ): $(MOD)

$(EXE): $(OBJ) modparm.o
	$(F95) $(LDFLAGS) $(OBJ) -o $@
	## debug
	#$(F95) $(LDFLAGS) $(OBJ) $@

.f.o: modparm.f
	$(F95) $(FFLAGS) $< -o $@
	## debug
	#$(F95) $(FFLAGS) $< $@

