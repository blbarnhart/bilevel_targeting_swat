cp swat2009 /home/pokey/calapooia2009/lc_subs/Scenarios/Default/txtinout_unix

[pokey@localhost txtinout_unix]$ ./swat2009
                SWAT2009
               Rev. 428
      Soil & Water Assessment Tool
               PC Version
 Program reading from file.cio . . . executing

forrtl: severe (24): end-of-file during read, unit 127, file /home/pokey/calapooia2009/lc_subs/Scenarios/Default/txtinout_unix/ATMO.ATM

readatmodep.f
      subroutine readatmodep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the atmospheric deposition values

######## WORKAROUND

mv atmo.atm ATMO.ATM



PAYPAL PASSWORD

se45fg&kijn
se45fg&kijn

cp swat2009 /home/pokey/calapooia2009/lc_subs/Scenarios/Default/txtinout_unix

cp swat2009 /home/pokey/calapooia2009/lower_cal3/Scenarios/Default/txtinout_unix

scp -pr 128.193.230.72:/home/pokey/blue_src_code/srcode_calibrate/ .

scp -pr 128.193.230.72:/home/pokey/blue_overfit/add_garch/ /home/pokey/blue


[pokey@localhost src]$ pwd
/home/pokey/swat_dev10/src

cp -aR src src_calibrate

##########################################################################
## R code
/home/pokey/blue/add_garch/start_calibrate.R
/home/pokey/blue/add_garch/Mauro_garch2.R

## Blue SWAT2005
/home/pokey/srcode_calibrate

####### new FORTRAN code
cp main.f main.autocal.calibrate.f

diff main.autocal.calibrate.f /home/pokey/srcode_calibrate/main.autocal.validate.f > diff1.R

#######
modparm.calibrate.f

               real,dimension(2557,24) :: hflow_pga  !! adjust for number of observations

#######
readgw.f

diff readgw.f /home/pokey/srcode_calibrate/readgw.f > diff2.R

#######
readhru.f

#######
readmgt.f

#######
readrte.f

#######
readsol.f

#######
readsub.f

#######
### flow for calibration
rtout.f

############################################################################
## uses rch_day2 in reachout.f
############################################################################
scp -pr 128.193.230.72:/mnt/usb/centos5/whitt/sufi/Thur_swat3/ /home/pokey/


## /home/pokey/swat_dev10/src_calibrate

make -f make_i64.gmk

cp swat2009_i64_calibrate.so /home/pokey/calapooia2009/lower_cal3/Scenarios/Default/txtinout_unix









