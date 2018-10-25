      subroutine readrte

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the reach (main channel) input file 
!!    (.rte). This file contains data related to channel attributes. Only
!!    one reach file should be made for each subbasin. If multiple HRUs are
!!    modeled within a subbasin, the same .rte file should be listed for all
!!    HRUs in file.cio

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bnk(:)  |days        |alpha factor for bank storage recession curve
!!    alpha_bnke(:) |none        |Exp(-alpha_bnk(:))
!!    chside(:)     |            |change in horizontal distance per unit
!!                               |  vertical distance (0.0 - 5)
!!                               |0 = for vertical channel bank
!!                               |5 = for channel bank with gentl side slope
!!    ch_bnk_bd(:)  |(g/cc)      |bulk density of channel bank sediment (1.1-1.9)
!!    ch_bed_bd(:)  |(g/cc)      |bulk density of channel bed sediment (1.1-1.9)
!!    ch_bnk_kd(:)  |            |erodibility of channel bank sediment by jet test
!!                               | (Peter Allen needs to give more info on this)
!!    ch_bed_kd(:)  |            |erodibility of channel bed sediment by jet test
!!                               | (Peter Allen needs to give more info on this)
!!    ch_bnk_d50(:) |            |D50(median) particle size diameter of channel
!!                               |  bank sediment (0.001 - 20)
!!    ch_bed_d50(:) |            |D50(median) particle size diameter of channel
!!                               |  bed sediment (micrometers) (0.001 - 20)
!!    ch_cov1(:)    |none        |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_cov2(:)    |none        |channel cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)       |m           |average depth of main channel
!!    ch_di(:)      |m           |initial depth of main channel
!!    ch_eqn        |            |sediment routine methods: 
!!                               | 0 = original SWAT method
!!                               | 1 = Bagnold's
!!                               | 2 = Kodatie
!!                               | 3 = Molinas WU
!!                               | 4 = Yang
!!    ch_erod(:)    |none        |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_k(2,:)     |mm/hr       |effective hydraulic conductivity of 
!!                               |main channel alluvium
!!    ch_l2(:)      |km          |length of main channel
!!    ch_li(:)      |km          |initial length of main channel
!!    ch_n(2,:)     |none        |Manning's "n" value for the main channel
!!    ch_onco(:)    |ppm         |channel organic n concentration
!!    ch_opco(:)    |ppm         |channel organic p concentration
!!    ch_s(2,:)     |m/m         |average slope of main channel
!!    ch_si(:)      |m/m         |initial slope of main channel
!!    ch_w(2,:)     |m           |average width of main channel
!!    ch_wdr(:)     |m/m         |channel width to depth ratio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line of .rte file (not used elsewhere)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ SUBROUTINES/FUNCTIONS ~ ~ ~ ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
 

      use parm

      character (len=80) :: titldum
      integer :: eof
	  real :: bnksize, bedsize
  
      eof = 0
      do
      read (103,5000) titldum
      read (103,*) ch_w(2,i)
!        ch_w(2,i) = ch_w(2,i) * chw2_pga(i)
        
      read (103,*) ch_d(i) 
 !       ch_d(i) = ch_d(i) * chd_pga(i)
        
      read (103,*) ch_s(2,i) 
!        ch_s(2,i) = ch_s(2,i) * chs2_pga(i)
        
      read (103,*) ch_l2(i)
!        ch_l2(i) = ch_l2(i) * chl2_pga(i)
        
      read (103,*) ch_n(2,i) 
!        ch_n(2,i) = chn2_pga(i)
        
      read (103,*) ch_k(2,i) 
 !       ch_k(2,i) = chk2_pga(i)
        
      read (103,*) ch_cov1(i) 
      read (103,*,iostat=eof) ch_cov2(i) 
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_wdr(i)
!        ch_wdr(i) = ch_wdr(i) * chwdr_pga(i)
      if (eof < 0) exit
      
      read (103,*,iostat=eof) alpha_bnk(i)
!        alpha_bnk(i) = alpha_bnk_pga(i)
      if (eof < 0) exit
      
      read (103,*,iostat=eof) icanal(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_onco(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_opco(i)
      if (eof < 0) exit
	read (103,*,iostat=eof) chside(i)
	if (eof < 0) exit
      read (103,*,iostat=eof) ch_bnk_bd(i)
	if (eof < 0) exit
	read (103,*,iostat=eof) ch_bed_bd(i)
	if (eof < 0) exit
      read (103,*,iostat=eof) ch_bnk_kd(i)
	if (eof < 0) exit
      read (103,*,iostat=eof) ch_bed_kd(i)
	if (eof < 0) exit
      read (103,*,iostat=eof) ch_bnk_d50(i)
	if (eof < 0) exit
      read (103,*,iostat=eof) ch_bed_d50(i)
	if (eof < 0) exit
	read (103,5000,iostat=eof) tc_bnk(i)
	if (eof < 0) exit
	read (103,5000,iostat=eof) tc_bed(i)
	if (eof < 0) exit
      read (103,5100,iostat=eof) (ch_erodmo(i,mo), mo = 1,12)
	if (eof < 0) exit
	read (103,*,iostat=eof) ch_eqn(i)
      exit
      end do

!!    set default values for parameters
      if (tc_bnk(i) <= 1.e-6) tc_bnk(i) = 0.001
	  if (tc_bed(i) <= 1.e-6) tc_bed(i) = 0.001
      if (ch_s(2,i) <= 0.) ch_s(2,i) = .0001
      if (ch_n(2,i) <= 0.01) ch_n(2,i) = .01
      if (ch_n(2,i) >= 0.70) ch_n(2,i) = 0.70
      if (ch_l2(i) <= 0.) ch_l2(i) = .0010
      if (ch_wdr(i) <= 0.) ch_wdr(i) = 3.5
	if (chside(i) <= 1.e-6) chside(i) = 2.0
      if (tc_bnk(i) <= 0.) tc_bnk(i)=0. !! Critical shear stress (N.m^2)
      if (tc_bed(i) <= 0.) tc_bed(i)=0. !! Critical shear stress (N.m^2)

      if (ch_eqn(i) <= 0) then
        ch_eqn(i)=0 !! SWAT Default sediment routing routine
        if (ch_cov1(i) <= 0.0) ch_cov1(i) = 0.0
        if (ch_cov2(i) <= 0.0) ch_cov2(i) = 0.0
        if (ch_cov1(i) >= 1.0) ch_cov1(i) = 1.0
        if (ch_cov2(i) >= 1.0) ch_cov2(i) = 1.0
	else 
        if (ch_cov1(i) <= 0.0) ch_cov1(i) = 1.0
        if (ch_cov2(i) <= 0.0) ch_cov2(i) = 1.0
        if (ch_cov1(i) >= 25.) ch_cov1(i) = 25.
        if (ch_cov2(i) >= 25.) ch_cov2(i) = 25.
	end if
	  

!!    Bank material is assumed to be silt type partcile if not given.
      if (ch_bnk_d50(i) <= 1.e-6) ch_bnk_d50(i) = 50 !! Units are in Micrometer
	if (ch_bnk_d50(i) > 10000) ch_bnk_d50(i) = 10000


      bnksize = ch_bnk_d50(i)/1000.  !! Units conversion Micrometer to Millimeters
!!    Channel sediment particle size distribution
!!    Clayey bank
	if (bnksize <= 0.005) then
	  ch_bnk_cla(i) = 0.65
        ch_bnk_sil(i) = 0.15
	  ch_bnk_san(i) = 0.15
	  ch_bnk_gra(i) = 0.05
	end if

!!    Silty bank
	if (bnksize > 0.005 .and. bnksize <= 0.05) then
        ch_bnk_sil(i) = 0.65
	  ch_bnk_cla(i) = 0.15
	  ch_bnk_san(i) = 0.15
	  ch_bnk_gra(i) = 0.05
	end if

!!    Sandy bank
	if (bnksize > 0.05 .and. bnksize <= 2.) then
	  ch_bnk_san(i) = 0.65
        ch_bnk_sil(i) = 0.15
	  ch_bnk_cla(i) = 0.15
	  ch_bnk_gra(i) = 0.05
	end if
      
!!    Gravel bank
	if (bnksize > 2.) then
	  ch_bnk_gra(i) = 0.65
	  ch_bnk_san(i) = 0.15
        ch_bnk_sil(i) = 0.15
	  ch_bnk_cla(i) = 0.05
	end if

!!    Bed material is assumed to be sand type partcile if not given.
      if (ch_bed_d50(i) <= 1.e-6) ch_bed_d50(i) = 500 !! Units are in Micrometer
      if (ch_bed_d50(i) > 10000) ch_bed_d50(i) = 10000. 

!!    Channel sediment particle size distribution
!!    Clayey bed
      bedsize = ch_bed_d50(i)/1000.  !! Units conversion Micrometer to Millimeters
	if (bedsize <= 0.005) then
	  ch_bed_cla(i) = 0.65
        ch_bed_sil(i) = 0.15
	  ch_bed_san(i) = 0.15
	  ch_bed_gra(i) = 0.05
	end if

!!    Silty bed
	if (bedsize > 0.005 .and. bedsize <= 0.05) then
        ch_bed_sil(i) = 0.65
	  ch_bed_cla(i) = 0.15
	  ch_bed_san(i) = 0.15
	  ch_bed_gra(i) = 0.05
	end if

!!    Sandy bed
	if (bedsize > 0.05 .and. bedsize <= 2.) then
	  ch_bed_san(i) = 0.65
        ch_bed_sil(i) = 0.15
	  ch_bed_cla(i) = 0.15
	  ch_bed_gra(i) = 0.05
	end if
      
!!    Gravel bed
	if (bedsize > 2.) then
	  ch_bed_gra(i) = 0.65
	  ch_bed_san(i) = 0.15
        ch_bed_sil(i) = 0.15
	  ch_bed_cla(i) = 0.05
	end if

!!    Bulk density of channel bank sediment 
	if (ch_bnk_bd(i) <= 1.e-6) ch_bnk_bd(i) = 1.40 !! Silty loam bank

!!    Bulk density of channel bed sediment
	if (ch_bed_bd(i) <= 1.e-6) ch_bed_bd(i) = 1.50  !! Sandy loam bed


!!    An estimate of Critical shear stress if it is not given (N/m^2)
!!	Critical shear stress based on silt and clay %
!!	Critical Shear Stress based on Julian and Torres (2005)
!!    Units of critical shear stress (N/m^2)
	SC = 0.
	if  (tc_bnk(i) <= 1.e-6) then
	  SC = (ch_bnk_sil(i) + ch_bnk_cla(i)) * 100.
        tc_bnk(i) = (0.1 + (0.1779*SC) + (0.0028*(SC)**2)               &
     &                              - ((2.34E-05)*(SC)**3)) * ch_cov1(i)
      end if

	if  (tc_bed(i) <= 1.e-6) then
	  SC = (ch_bed_sil(i) + ch_bed_cla(i)) * 100.
        tc_bed(i) = (0.1 + (0.1779*SC) + (0.0028*(SC)**2)               &
     &                              - ((2.34E-05)*(SC)**3)) * ch_cov2(i)
      end if

!!  An estimate of channel bank erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
      if (ch_bnk_kd(i) <= 1.e-6) then
        ch_bnk_kd(i) = 0.2 / sqrt(tc_bnk(i))
	end if

!!  An estimate of channel bed erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
      if (ch_bed_kd(i) <= 1.e-6) then
        ch_bed_kd(i) = 0.2 / sqrt(tc_bed(i))
	end if
     
      sumerod = 0.
      do mo = 1, 12
        sumerod = sumerod + ch_erodmo(i,mo)
      end do

      if (sumerod < 1.e-6) then
        do mo = 1, 12
          ch_erodmo(i,mo) = ch_cov1(i)
        end do
      end if

!!    set default values for mike van liew
      if (ch_onco(i) <= 0.) ch_onco(i) = ch_onco_bsn
      if (ch_opco(i) <= 0.) ch_opco(i) = ch_opco_bsn
!!    set default values for mike van liew


!!    initialize variables for channel degradation
      ch_di(i) = ch_d(i)
      ch_li(i) = ch_l2(i)
      ch_si(i) = ch_s(2,i)
      ch_wi(i) = ch_w(2,i)

      close (103)
      return
 5000 format (a)
5100  format (12f6.2)
      end
