[pokey@nfsprc src_dev10_blue]$ cat ../003590001.mgt
 .mgt file Watershed HRU:505 Subbasin:359 HRU:1 Luse:RYEG Soil: OR63933-1 Slope: 0-9999 7/25/2010 12:00:00 AM ArcSWAT 2.3.4
               0    | NMGT:Management code
Initial Plant Growth Parameters
               0    | IGRO: Land cover status: 0-none growing; 1-growing
               0    | PLANT_ID: Land cover ID number (IGRO = 1)
            0.00    | LAI_INIT: Initial leaf are index (IGRO = 1)
            0.00    | BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)
            0.00    | PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)
General Management Parameters
            0.20    | BIOMIX: Biological mixing efficiency
           79.00    | CN2: Initial SCS CN II value
            1.00    | USLE_P: USLE support practice factor
            0.00    | BIO_MIN: Minimum biomass for grazing (kg/ha)
           0.000    | FILTERW: width of edge of field filter strip (m)
Urban Management Parameters
               0    | IURBAN: urban simulation code, 0-none, 1-USGS, 2-buildup/washoff
               0    | URBLU: urban land type
Irrigation Management Parameters
               0    | IRRSC: irrigation code
               0    | IRRNO: irrigation source location
           0.000    | FLOWMIN: min in-stream flow for irr diversions (m^3/s)
           0.000    | DIVMAX: max irrigation diversion from reach (+mm/-10^4m^3)
           0.000    | FLOWFR: : fraction of flow allowed to be pulled for irr
Tile Drain Management Parameters
           0.000    | DDRAIN: depth to subsurface tile drain (mm)
           0.000    | TDRAIN: time to drain soil to field capacity (hr)
           0.000    | GDRAIN: drain tile lag time (hr)
Management Operations:
               1    | NROT: number of years of rotation
Operation Schedule:
  2  1           1   44           433.59980   0.00     0.00000 0.00   0.00  0.00
  3  1           3    5            97.47000   1.00
  7  1           5                  0.00000
 10  1           3    5           389.86000   1.00
 

  readmgt.f 
!!    cfrt_id(:,:,:)  |none           |fertilizer/manure id number from database
!!    cfrt_kg(:,:,:)  |kg/ha          |amount of fertilzier applied to HRU on a
!!                                    |given day

!!    frt_kg(:,:,:)   |kg/ha          |amount of fertilizer applied to HRU
!!    ifert(:,:,:)    |julian date    |date of fertilizer application

!!    ifn         |none          |number of fertilizer application in year

line 598
              ifrttyp(iro,ifn,ihru) = mgt1i
              frt_kg(iro,ifn,ihru) = mgt4
!!    iro         |none          |counter for years of rotation
!!    ifn         |none          |number of fertilizer application in year
!!    ihru       |none             |HRU number

[pokey@nfsprc src_dev10_blue]$ grep "hru1(inum1)" *.f
subbasin.f:      ihru = hru1(inum1)
[pokey@nfsprc src_dev10_blue]$ grep "inum1 =" *.f
command.f:        inum1 = 0
command.f:        inum1 = inum1s(idum)
rewind_init.f:        inum1 = 0
rewind_init.f:        inum1 = inum1s(idum)
route.f:          if (i_subhw == 0 .and. inum1 == inum2) then

command.f:      do idum = 1, mhyd
!!    mhyd        |none          |maximum number of hydrographs
!!    inum1s(:)   |none          |For ICODES equal to
!!                               |0: not used
!!                               |1: subbasin number
!!                               |2: reach number
!!                               |3: reservoir number
!!                               |4: reach or res # flow is diverted from
!!                               |5: hydrograph storage location of 1st
!!                               |   dataset to be added
!!                               |7,8,9,10,11,14: file number
#######################################################################3

main.f:      call readfig

readfig.f:   call readsub

readsub.f:   call readmgt

##### in readsub.f, line 265
          call readmgt
             ! using i (subbasin number), substitute fertilizer app. values here
          
 
idb

swat2009 <- function(vars_Rga, numparam)

{

        r <- .Fortran("swat2009",
            as.double(vars_Rga),
            as.integer(numparam),
            #### totflow_Rga = double(1500),
            rchdy2_pga = double(5000)
            )
         
        list(rchdy2_pga = r$rchdy2_pga
        )
 }
/home/pokey/calapooia2009/lower_cal3/Scenarios/Default/txtinout_unix

###############################################################################################
run1 <- swat2009(oldpop,14556)

## to debug with idb
R -d idb

## then: restart
        load *.so, etc
	pause
	load source code
	resume,
	etc,


################################################################
## from /home/pokey/calapooia2009/lower_cal_big/Scenarios/Default/txtinout_mop/notes2009.R

source('swatrun.R') 

pop500 <- read.table('popiter_500.csv')
eval500 <- read.table('evaliter_500.csv')
dyn.load('swat2009_i64_calibrate.so')
ind_1_500 <- swat2009(pop500[1,],3429)

### zip code of each subwatershed
subwatershed_zip.csv

modparm.f
      integer, parameter :: numsub = 381  !! from output.std
      integer, parameter :: nohru = 533   !! from output.std
      integer, parameter :: numobs = 2200 !! number of observations in time series

            !!!!! link from GAMS to fertilizer application   
               double precision :: frt_kg_pga(numsub)

main.autocal.calibrate.f
!!!!!!!!!!!  fertilizer application from GAMS
            do ga_j = 1,numsub
               frt_kg_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo


############### test run Sunday, Aug.1, 2010
pop_fert <- c(pop500[1,],rep(300,times=381))  # 300 kg/ha, 381 subbasins
> length(pop_fert)
[1] 14937

R -d idb

## then: restart (target w/ arrow)
         >dyn.load('swat2009_i64_calibrate.so')
	 stop     (red light)
	 file > open source file
           set breakpoints,...
	 resume   (green light)
	 etc,

dyn.load('swat2009_i64_calibrate.so')


ind_1_500 <- swat2009(pop_fert,(14556+381))















