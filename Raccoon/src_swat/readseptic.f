      subroutine readseptic

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the septic input file (.sep).  This file
!!    contains information related to septic tanks modeled or defined at the 
!!    watershed level

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adj_pkr     |none          |peak rate adjustment factor in the subbasin.

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
	integer :: eof

!!    initialize variables
      eof = 0


!! read septic parameters
      do
      read (172,1000) titldum
      read (172,1000) titldum
	read (172,*,iostat=eof) ipop_sep(ihru)
      if (eof < 0) exit
      read (172,*,iostat=eof) isep_typ(ihru)
      if (eof < 0) exit      
      read (172,*,iostat=eof) bz_z(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) bz_thk(ihru)
      if (eof < 0) exit
      read (172,*,iostat=eof) bz_area(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) bio_bd(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_bod_dc(ihru)
	if (eof < 0) exit   
      read (172,*,iostat=eof) coeff_bod_conv(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_fc1(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_fc2(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_fecal(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_plq(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_mrt(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_rsp(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_slg1(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_slg2(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_nitr(ihru)
	if (eof < 0) exit
      read (172,*,iostat=eof) coeff_denitr(ihru)
	exit
	end do


!!    set default values for undefined parameters
      if (bz_z(ihru) <= 1.e-6) bz_z(ihru) = 600.
      if (bz_thk(ihru) <= 1.e-6) bz_thk(ihru) = 20.
      if (bz_area(ihru) <= 1.e-6) bz_area(ihru) = 10.
      if (bio_bd(ihru) <= 1.e-6) bio_bd(ihru) = 1000.
      if (coeff_bod_dc(ihru) <= 1.e-6) coeff_bod_dc(ihru) = 108.
      if (coeff_bod_conv(ihru) <= 1.e-6) coeff_bod_conv(ihru) = 0.42
      if (coeff_fc1(ihru) <= 1.e-6) coeff_fc1(ihru) = 30.0
      if (coeff_fc2(ihru) <= 1.e-6) coeff_fc2(ihru) = 0.7
      if (coeff_fecal(ihru) <= 1.e-6) coeff_fecal(ihru) = 15.0
      if (coeff_plq(ihru) <= 1.e-6) coeff_plq(ihru) = 0.10
      if (coeff_mrt(ihru) <= 1.e-6) coeff_mrt(ihru) = 0.29
      if (coeff_rsp(ihru) <= 1.e-6) coeff_rsp(ihru) = 0.18
      if (coeff_slg1(ihru) <= 1.e-6) coeff_slg1(ihru) = 4. * 10. **(-4)
      if (coeff_slg2(ihru) <= 1.e-6) coeff_slg2(ihru) = 1.5
      if (coeff_nitr(ihru) <= 1.e-6) coeff_nitr(ihru) = 1.0
      if (coeff_denitr(ihru) <= 1.e-6) coeff_denitr(ihru) = 0.05
 
!     m3/d = m3/d/cap*population
      sptqs(ihru) = sptqs(isep_typ(ihru)) * ipop_sep(ihru)

      
      close (172)
1000  format (a)
      return
      end
