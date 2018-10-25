      include 'modparm.f'
      subroutine swat2009(vars_Rga,nvars,rchdy2_Rga)
!!    this is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    comment changes to test merging with trunk and c:\branch_test code
!!    two lines added to c:\branch_test code

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    values(1)   |year          |year simulation is performed
!!    values(2)   |month         |month simulation is performed
!!    values(3)   |day           |day in month simulation is performed
!!    values(4)   |minutes       |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    values(5)   |hour          |hour simulation is performed
!!    values(6)   |minutes       |minute simulation is performed
!!    values(7)   |seconds       |second simulation is performed
!!    values(8)   |milliseconds  |millisecond simulation is performed
!!    zone        |NA            |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: date_and_time
!!    SWAT: getallo, allocate_parms, readfile, readfig
!!    SWAT: readbsn, std1, readwwq, readinpt, std2, storeinitial
!!    SWAT: openwth, headout, simulate, finalbal, writeaa, pestw 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      integer ga_i,ga_j
      !  variables passed to subroutine
      integer nvars
      double precision vars_Rga(nvars)
      double precision rchdy2_Rga(1,numobs)
      
      rchdy2_Rga = 0.0

      prog = "SWAT  August '09 VER 2009/Rev. 428"

      write (*,1000)
 1000 format(1x,"               SWAT2009               ",/,             &
     &          "               Rev. 428               ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          " Program reading from file.cio . . . executing",/)

!! assign GA values
c            ga_i = 1
c            do ga_j = 1,nohru
c               phu_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1 
c            enddo   
            
                          
c            do ga_j = 1,nohru
c               solk_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               awhc_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               solcrk_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c              chn2_pga(ga_j) = vars_Rga(ga_i)
c             ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               ovn_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               canmx_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               esco_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               epco_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               revapmn_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               alpha_bf_pga(ga_j) = vars_Rga(ga_i)
c              ga_i = ga_i + 1               
c            enddo
c            do ga_j = 1,nohru
c               gwdelay_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo

c            do ga_j = 1,nohru
c               gw_revap_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
            
!               surlag_pga = vars_Rga(ga_i)
!               ga_i = ga_i + 1  
                                        
!               msk_co1_pga = vars_Rga(ga_i)
!               ga_i = ga_i + 1               
            
!               msk_co2_pga = vars_Rga(ga_i)
!               ga_i = ga_i + 1               
            
!               msk_x_pga = vars_Rga(ga_i)
!               ga_i = ga_i + 1               
            
!               trnsrch_pga = vars_Rga(ga_i)
!               ga_i = ga_i + 1               
            
!               evrch_pga = vars_Rga(ga_i)
!               ga_i = ga_i + 1     
            
                         
c            do ga_j = 1,nohru
c               slsubbsn_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               slsoil_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               hruslp_pga(ga_j) = vars_Rga(ga_i)
c              ga_i = ga_i + 1               
c            enddo
            

c               timp_pga = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
            
c               smfmn_pga = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
            
c               smfmx_pga = vars_Rga(ga_i)
c               ga_i = ga_i + 1   
            
                           
c            do ga_j = 1,numsub
c               chl1_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chs1_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chw1_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chn1_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chk1_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chl2_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chs2_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chw2_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
c            do ga_j = 1,numsub
c               chd_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chk2_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               chwdr_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,numsub
c               alpha_bnk_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            

c            do ga_j = 1,nohru
c               gwqmn_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
c            do ga_j = 1,nohru
c               rchrgdp_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
 
!could not find this variable (below)            
c            do ga_j = 1,nohru
c               gwspyld_pga(ga_j) = vars_Rga(ga_i)
c               ga_i = ga_i + 1               
c            enddo
            
!!!!!!!!!!!  fertilizer application from GAMS
      ga_i = 1
      do ga_j = 1,nohru
        frt_kg_pga(ga_j) = vars_Rga(ga_i)
        ga_i = ga_i + 1               
      enddo

      do ga_j = 1,nohru
        tillchoice_pga(ga_j) = vars_Rga(ga_i)
        ga_i = ga_i + 1
      enddo
            
!! process input
		


      call getallo
      call allocate_parms
      call readfile
      call readbsn
!!!!!!  values for readbsn!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      smfmn = smfmn_pga
c      smfmx = smfmx_pga
c      timp = timp_pga
c      surlag = surlag_pga
c      msk_co1 = msk_co1_pga
c      msk_co2 = msk_co2_pga
c      msk_x =   msk_x_pga
c      trnsrch = trnsrch_pga
c      evrch = evrch_pga      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call readwwq
      if (fcstyr > 0 .and. fcstday > 0) call readfcst
      call readplant             !! read in the landuse/landcover database
      call readtill              !! read in the tillage database
      call readpest              !! read in the pesticide database
      call readfert              !! read in the fertilizer/nutrient database
      call readurban             !! read in the urban land types database
      call readseptwq            !! read in the septic types database     
      call readlup
      call readfig
      call readatmodep
      call readinpt
      call std1
      call std2
      call openwth
      call headout



      if (isproj == 2) then 
        hi_targ = 0.0
      end if

!! save initial values
      if (isproj == 1) then
        scenario = 2
        call storeinitial
      else if (fcstcycles > 1) then
        scenario =  fcstcycles
        call storeinitial
      else
        scenario = 1
      endif
        if (iclb /= 4) then
      do iscen = 1, scenario

     
        !! simulate watershed processes
        call simulate

        !! perform summary calculations
        call finalbal
        call writeaa
        call pestw

        !!reinitialize for new scenario
        if (scenario > iscen) call rewind_init
      end do
         end if
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)

      do ga_i = 1,numobs
         rchdy2_Rga(1,ga_i) = rchdy2_pga(1,ga_i)  ! from rtout.f
c         rchdy2_Rga(2,ga_i) = rchdy2_pga(2,ga_i)  ! from rtout.f
c         rchdy2_Rga(3,ga_i) = rchdy2_pga(3,ga_i)  ! from rtout.f
c         rchdy2_Rga(4,ga_i) = rchdy2_pga(4,ga_i)  ! from rtout.f
c         rchdy2_Rga(5,ga_i) = rchdy2_pga(5,ga_i)  ! from rtout.f
c         rchdy2_Rga(6,ga_i) = rchdy2_pga(6,ga_i)  ! from rtout.f
c         rchdy2_Rga(7,ga_i) = rchdy2_pga(7,ga_i)  ! from rtout.f
c         rchdy2_Rga(8,ga_i) = rchdy2_pga(8,ga_i)  ! from rtout.f
      end do

      call freearray
      
      do i = 1, 2000
        close (i)
      end do

      write (*,1001)
 1001 format (/," Execution successfully completed ")
	

        iscen=1
        if (iclb > 0) call automet
!	stop
       return
      end
