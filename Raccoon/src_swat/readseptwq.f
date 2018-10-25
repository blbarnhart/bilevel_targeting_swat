      subroutine readseptwq

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine reads input parameters from the sept wq database
!!     (septwq.dat). Information is used when a hru has septic tank.
       
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     msdb        |none            |maximum number of sept wq data database
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     sptq(:)          |m3/d/cap   |Flow rate of the septic tank effluent 
!!                      |           |per capita
!!     sptbodin   (:)   |mg/l       |Biological Oxygen Demand of the septic 
!!                      |           |tank effluent  
!!     spttssconc(:)    |mg/l       |Concentration of total suspended solid in the 
!!                      |           |septic tank effluent
!!     spttnconc(:)     |mg/l       |Concentration of total nitrogen 
!!                      |           |in the septic tank effluent
!!     sptnh4conc(:)    |mg/l       |Concentration of total phosphorus 
!!                      |           |of the septic tank effluent
!!     sptno3conc(:)    |mg/l       |Concentration of nitrate 
!!                      |           |in the septic tank effluent
!!     sptno2conc(:)    |mg/l       |Concentration of nitrite 
!!                      |           |in the septic tank effluent
!!     sptorgnconc(:)   |mg/l       |Concentration of organic nitrogen in 
!!                      |           |the septic tank effluent
!!     spttpconc(:)     |mg/l       |Concentration of total phosphorus in 
!!                      |           |the septic tank effluent  
!!     sptminp(:)       |mg/l       |Concentration of mineral phosphorus in
!!                      |           |the septic tank effluent
!!     sptorgp(:)       |mg/l       |concentration of organic phosphorus in the 
!!                      |           |septic tank effluent
!!     sptfcoli(:)      |mg/l       |concentration of the fecal caliform in the 
!!                      |           |septic tank effluent
!!   	 idspttype(:)     |           |1 - Standard; 2 - Advanced; 3 - Failing    
!!     sptfulname (:)   !           ! septic tank full name description 
!!
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof         |none            |end of file flag
!!     ist         |none            |counter which represents the array
!!                                  |storage number of the septic data
!!                                  |the array storage number is used by the
!!                                  |model to access data for a specific 
!!                                  |sept type
!!     isnum     |none            |number of septic system database  (reference
!!                                  |only)
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    
    
!!     This routine was developed by CS. Inputs for this routine is from Table 2.
      use parm

      
      character* 4 sptnames(26)
      character*4 sptname
      character*80 titlesep
      character*70 sptfulname

      integer :: ist, isnum, eof

      real :: sptq,sptbodin,spttssconc,spttnconc,sptnh4conc,sptno3conc
      real :: sptno2conc,sptorgnconc,spttpconc,sptminp,sptorgp,sptfcoli

      isnum = 0
      eof = 0

!!    septic database filename present in file.cio
      if (septdb /= '             ') then
        open (171,file=septdb)
	

!!    read title lines
         do ii = 1, 4
          read (171,5999) titlesep
        end do
    
     
      do 
       
	  sptname = ""
	  idspttype = 0 
 	  sptq = 0.
	  sptbodin = 0.
	  spttssconc = 0.
	  spttnconc = 0.
	  sptnh4conc = 0.
	  sptno3conc = 0.
	  sptno2conc = 0.
	  sptorgnconc = 0.
	  spttpconc = 0.
	  sptminp = 0.
	  sptorgp = 0.
	  sptfcoli = 0.
     

        read (171,6000,iostat=eof)ist,sptname,sptfulname,idspttype,
     &  sptq,sptbodin,spttssconc,spttnconc,sptnh4conc,sptno3conc,
     &  sptno2conc,sptorgnconc,spttpconc,sptminp,sptorgp,sptfcoli

	  if (eof < 0) exit
        if (ist == 0) exit

!	  isnum(ist) = isptnums

	  sptnames(ist) = sptname
	  idspts(ist) = idspttype
 	  sptqs(ist) = sptq
	  sptbodins(ist) = sptbodin
	  spttssconcs(ist) = spttssconc
	  spttnconcs(ist) = spttnconc
	  sptnh4concs(ist) = sptnh4conc
	  sptno3concs(ist) = sptno3conc
	  sptno2concs(ist) = sptno2conc
	  sptorgnconcs(ist) = sptorgnconc
	  spttpconcs(ist) = spttpconc
	  sptminps(ist) = sptminp
	  sptorgps(ist) = sptorgp
	  sptfcolis(ist) = sptfcoli


!!     check values ---- Check values for ranges
!!      sptq(ist) = sptq
	  sptbodins(ist) = sptbodin
	  spttssconcs(ist) = spttssconc
	  spttnconcs(ist) = spttnconc
	  sptnh4concs(ist) = sptnh4conc
	  sptno3concs(ist) = sptno3conc
	  sptno2concs(ist) = sptno2conc
	  sptorgnconcs(ist) = sptorgnconc
	  spttpconcs(ist) = spttpconc
	  sptminps(ist) = sptminp
	  sptorgps(ist) = sptorgp
	  sptfcolis(ist) = sptfcoli

        
      end do
      else
      return
      endif

      close (171)
      return
 5999 format (a)
 6000 format (i3,1x,a4,1x,a70,i4/4x,10f8.3/4x,f8.3,f11.1)
 
      end
