      subroutine biozone
	    
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine hydraulic and biological processes occuring 
!!    in the biozone at the HRU level.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    ihru          |none          |HRU number
!!    iida          |day           |day being simulated
!!    nhru          |none          |number of HRUs in watershed
!!    sol_fc(:,:)   |mm H2O        |amount of water available to plants in soil 
!!                                 |layer at field capacity (fc - wp),Index:(layer,HRU)
!!    sol_k(:,:)    |mm/hr         |saturated hydraulic conductivity of soil 
!!                                 |layer. Index:(layer,HRU)
!!    sol_nly(:)    |none          |number of soil layers 
!!    sol_por(:,:)  |none          |total porosity of soil layer expressed as a 
!!                                 |fraction of the total volume, Index:(layer,HRU)
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer,Index:(layer,HRU)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                  |none          |                       

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_fecal(:)  |mg/L          |Fecal coliform concentration in biozone
!!    bio_bd        |mg/L(=kg/m^3) |density of biomass 
!!    bio_bod(:)    |kg/ha         |BOD concentration in biozone
!!    biom(:)       |kg/ha         |biomass of live bacteria in biozone       
!!    bz_lyr(:)     |none          |soil layer where biozone exists
!!    bz_perc(:)    |mm            |percolation out of the biozone 
!!    bio_rt(:)     |kg/ha         |daily change in biomass of live bacteria
!!    bz_swc(:)     |none          |saturated water content in the biozone 
!!    bz_thk        |mm            |thickness of biozone                    
!!    bz_z          |mm            |depth to the bottom of biozone
!!    coeff_bod_dc  |none          |BOD decay rate coefficient, cm^3/s
!!    coeff_bod_conv|none          |BOD to live bacteria biomass conversion factor; alpha
!!    coeff_denitr  |none          |Denitrification rate coefficient
!!    coeff_fc1     |none          |field capacity calibration parameter 1
!!    coeff_fc2     |none          |field capacity calibration parameter 2  
!!    coeff_fecal   |m^3/d         |Fecal coliform bacteria decay rate coefficient  
!!    coeff_mrt     |m^3/d         |mortality calibration parameter, phi          
!!    coeff_nitr    |none          |Nitrification rate coefficient
!!    coeff_plq     |none          |plqm calibration parameter          
!!    coeff_rsp     |m^3/d         |respiration calibration parameter, gamma          
!!    coeff_slg1    |none          |slough-off calibration parameter; eta
!!    coeff_slg2    |none          |slough-off calibration parameter; delta
!!    mrt_rt(:)     |kg/ha         |mortality of bacteria
!!    plqm          |kg/ha         |plaque in biozone
!!    plq_rt(:)     |kg/ha         |daily change in plaque
!!    rsp_rt(:)     |kg/ha         |resparation of bacteria
!!    slg_rt(:)     |kg/ha         |sloughed off bacteria
!!    sptqs         |m^3/day       |total flow rate from OWS effluent to  
!!								 |drainfield in biozone
!!    sptqc         |m^3/cap/day   |flow rate from OWS effluent to drainfield 
!!								 |per bz_capita in biozone
!!    spt_ts        |mg/L          |total solids in STE
!!    vp(:)         |cm/sec        |pore velocity in biozone

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    This code was developed by J. Jeong in Blackland Research Center in Temple, TX.
!!    The algorithm is based on Chapter 4. Biozone Algorithm in Siegrist et al., 2005

      use parm
	      
      real ntr_rt
      integer bz_lyr
      rtof = 0.5

	j = ihru
	isp = isep_typ(j)

!! determine the soil layer in which biozone is located
      bz_lyr = i_sep(j)
			  
!! Biozone hydraulic conductivity(mm/hr)    - Eq.4-9
!! Percolation out of biozone (cm^3/d)  
!!  These calculations are in perc

!! Saturated water content in the biozone - Eq. 4-7    ! mm = mm - kg/ha / (t/m^3 * 10000)
      sol_ul(bz_lyr,j) = sol_ul(bz_lyr,j) - plqm(j) 
     & 				/ (sol_bd(bz_lyr,j) *10000.)

!! Respiration rate(mg/s)  Eq. 4-2   ! kg/ha = m^3/d * kg/ha / (ha*mm*10)
      rsp_rt = coeff_rsp(j) * biom(j) / (sol_up(bz_lyr,j) *
     &	     10. * bz_thk(j) * hru_ha(j))

!! Mortality rate(mg/s) Eq. 4-3      ! kg/ha = m^3/d * kg/ha / (ha*mm*10)
      mrt_rt = coeff_mrt(j) * biom(j) / (sol_up(bz_lyr,j) *  
     &         10. * bz_thk(j) * hru_ha(j))

!! Slough-off rate(mg/s) Eq.4-4      ! eqn is too complex - simplify similar to respiration/mortality 
      slg_rt = coeff_slg1(j) * biom(j) / (sol_up(bz_lyr,j) *  
     &         10. * bz_thk(j) * hru_ha(j))

!! Build up of plqm Eq.4-5           ! kg/ha (perday) = kg/ha + dimensionless * m^3/d * mg/l / (1000*ha)
      plq_rt = (mrt_rt - slg_rt) + coeff_plq(j) * sptqs(isp) *  
     &                   spttssconcs(isp) / (1000. * hru_ha(isp))  

!! Add build up to plqm  ! kg/ha = kg/ha + kg/ha 
      plqm(j) = plqm(j) + plq_rt
	
!! Field capacity in the biozone Eq. 4-6  ! 
      sol_fc(bz_lyr,j) = sol_fc(bz_lyr,j) + coeff_fc1(j) * 
     &       ((sol_ul(bz_lyr,j) - sol_fc(bz_lyr,j)) / bz_thk(j)) 
     &       ** coeff_fc2(j) * bio_rt / (10000. * sol_bd(bz_lyr,j))

!! Water content  Eq. 4-12  ! mm = mm + m^3 / (10*ha)
      sol_st(bz_lyr,j) = sol_st(bz_lyr,j) + (sptqs(isp)
     &  / (10. * hru_ha(j))) 

!! BOD concentration(mg/L) Eq.4-13, 4-14 ! kg/ha = dimensionless kg/ha /(1000 *t/m^3 *mm) 
      bod_rt = coeff_bod_dc(j) * biom(j) / (10000. * sol_bd(bz_lyr,j) * 
     &           bz_thk(j))    
      bio_bod(j) = bio_bod(j) * Exp(-bod_rt)

!! compute mass balance of live bacteria in biozone(mg/d) Eq. 4-1 ! kg/ha = m^3 * mg/L/(1000.*ha)
	bio_rt = coeff_bod_conv(j) * sptqs(isp) * spttssconcs(isp) /
     &	   (1000. * hru_ha(j)) - bz_perc(j) * bio_bod(j) / bz_thk(j)
     &        - (rsp_rt + mrt_rt + slg_rt)   
	
!! total live biomass in biozone(mg)    
	biom(j) = biom(j) + bio_rt

!!Add nutrients to appropriate soil pools
      xx = (sptqs(isp) * bio_bd(j) / 1000.)
      sol_no3(bz_lyr,j) = sol_no3(bz_lyr,j) + xx * sptno3concs(isp) +   &
     &    sptno2concs(isp)  
      sol_nh3(bz_lyr,j) = sol_nh3(bz_lyr,j) + xx * sptnh4concs(isp) 
      sol_orgn(bz_lyr,j) = sol_orgn(bz_lyr,j) + xx * sptorgnconcs(isp) *& 
     &    rtof
      sol_fon(bz_lyr,j) = sol_fon(bz_lyr,j) + xx * sptorgnconcs(isp) *  &
     &    (1-rtof)
      sol_orgp(bz_lyr,j) = sol_orgp(bz_lyr,j) + xx * sptorgps(isp) *    &
     &    rtof
      sol_fop(bz_lyr,j) = sol_fop(bz_lyr,j) + xx * sptorgps(isp) *      &
     &    (1-rtof)
      sol_solp(bz_lyr,j) = sol_solp(bz_lyr,j) + xx * sptminps(isp)      &

!! Ammonium,NH4,(mg/L) Eq.4-13, 4-14  ! kg/ha = dimensionless kg/ha /(1000 *t/m^3 *mm) 
      ntr_rt = coeff_nitr(j) * biom(j) / (10000. * sol_bd(bz_lyr,j) *   &
     &               bz_thk(j))   
      sol_nh3(bz_lyr,j) = sol_nh3(bz_lyr,j) * Exp(-ntr_rt)

!! Nitrate,NO3,(mg/L) Eq.4-13, 4-14   ! kg/ha = dimensionless kg/ha /(1000 *t/m^3 *mm) 
      dentr_rt = coeff_denitr(j) * biom(j) / (10000. * sol_bd(bz_lyr,j) &
     &      * bz_thk(j))   
      sol_no3(bz_lyr,j) = sol_no3(bz_lyr,j) * Exp(-dentr_rt)
	       
!! Fecal coliform(mg/L) Eq.4-13, 4-14
!	  fecal_rt = coeff_fecal / (bz_swc(kk) * bz_thk/10.* bz_area) * biom(kk) !decay rate
!	  if(kk==1) then
!	    bio_fecal(kk) = bio_fecali * exp(-fecal_rt) !mg/l
!	  else
!	    bio_fecal(kk) = bio_fecal(kk-1) * exp(-fecal_rt) !mg/l
!	  endif
	       
!! print out time seriese results
!      write(100,1000) kk,biom(kk),bio_rt(kk),bz_swc(kk),bz_wc(kk),  &
!       bz_fc(kk),plqm(kk),plq_rt(kk),rsp_rt(kk)*86400.,            &
!       mrt_rt(kk)*86400.,slg_rt(kk)*86400.,bio_bod(kk),            &
!       bz_k(kk),vp(kk),perc1,perc2,bz_perc(kk),bio_amn(kk),bio_ntr(kk),&
!       bio_fecal(kk),bod_rt,ntr_rt,dentr_rt,fecal_rt

!$$$$$$ 1000  format(i15,30es15.5)
!$$$$$$ 2000  format(30a15)
      return
      end subroutine
