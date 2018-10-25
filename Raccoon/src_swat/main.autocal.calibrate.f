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
      double precision rchdy2_Rga(numobs)

      prog = "SWAT  August '09 VER 2009/Rev. 428"

      write (*,1000)
 1000 format(1x,"               SWAT2009               ",/,             &
     &          "               Rev. 428               ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          " Program reading from file.cio . . . executing",/)

!! assign GA values
            ga_i = 1
            do ga_j = 1,nohru
               phu_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1 
            enddo   
            
                          
            do ga_j = 1,nohru
               solk_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               awhc_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               solcrk_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chn2_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               ovn_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               canmx_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               esco_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               epco_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               revapmn_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               alpha_bf_pga(ga_j) = vars_Rga(ga_i)
              ga_i = ga_i + 1               
            enddo
            do ga_j = 1,nohru
               gwdelay_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo

            do ga_j = 1,nohru
               gw_revap_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            
               surlag_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1  
                                        
               msk_co1_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            
               msk_co2_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            
               msk_x_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            
               trnsrch_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            
               evrch_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1     
            
                         
            do ga_j = 1,nohru
               slsubbsn_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               slsoil_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               hruslp_pga(ga_j) = vars_Rga(ga_i)
              ga_i = ga_i + 1               
            enddo
            

               timp_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            
               smfmn_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            
               smfmx_pga = vars_Rga(ga_i)
               ga_i = ga_i + 1   
            
                           
            do ga_j = 1,numsub
               chl1_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chs1_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chw1_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chn1_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chk1_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chl2_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chs2_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chw2_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            do ga_j = 1,numsub
               chd_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chk2_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               chwdr_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,numsub
               alpha_bnk_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            

            do ga_j = 1,nohru
               gwqmn_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               rchrgdp_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
            do ga_j = 1,nohru
               gwspyld_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
!!!!!!!!!!!  fertilizer application from GAMS
            do ga_j = 1,numsub
               frt_kg_pga(ga_j) = vars_Rga(ga_i)
               ga_i = ga_i + 1               
            enddo
            
!! process input
		


      call getallo
      call allocate_parms
      call readfile
      call readbsn
!!!!!!  values for readbsn!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      smfmn = smfmn_pga
      smfmx = smfmx_pga
      timp = timp_pga
      surlag = surlag_pga
      msk_co1 = msk_co1_pga
      msk_co2 = msk_co2_pga
      msk_x =   msk_x_pga
      trnsrch = trnsrch_pga
      evrch = evrch_pga      
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
         rchdy2_Rga(ga_i) = rchdy2_pga(ga_i)
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
