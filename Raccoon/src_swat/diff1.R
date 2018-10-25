1,2c1,5
< !      include 'modparm.f'
<       program main
---
>       include 'modparm.f'
>       subroutine swat2005(vars_Rga,nvars,rchdy21_Rga)
> !      program main
> 
> !!    ~ ~ ~ PURPOSE ~ ~ ~
4,6c7
< !!    model, and writes output.
< !!    comment changes to test merging with trunk and c:\branch_test code
< !!    two lines added to c:\branch_test code
---
> !!    model, and writes output
10c11
< !!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
---
> !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
61a63,64
>       
>       prog = "SWAT  Sept '05 VERSION2005"
63,67c66,67
<       prog = "SWAT  August '09 VER 2009/Rev. 428"
< 
<       write (*,1000)
<  1000 format(1x,"               SWAT2009               ",/,             &
<      &          "               Rev. 428               ",/,             &
---
> !!      write (*,1000)
>  1000 format(1x,"               SWAT2005               ",/,             &
68a69
> !     &          "              UNIX Version            ",/,             &
71a73,260
> !! assign GA values
>             ga_i = 1
>             do ga_j = 1,nohru
>                phu_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1 
>             enddo   
>             
>                           
>             do ga_j = 1,nohru
>                solk_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                awhc_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                solcrk_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chn2_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                ovn_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                canmx_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                esco_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                epco_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                revapmn_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                alpha_bf_pga(ga_j) = vars_Rga(ga_i)
>               ga_i = ga_i + 1               
>             enddo
>             do ga_j = 1,nohru
>                gwdelay_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
> 
>             do ga_j = 1,nohru
>                gw_revap_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             
>                surlag_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1  
>                                         
>                msk_co1_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             
>                msk_co2_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             
>                msk_x_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             
>                trnsrch_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             
>                evrch_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1     
>             
>                          
>             do ga_j = 1,nohru
>                slsubbsn_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                slsoil_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                hruslp_pga(ga_j) = vars_Rga(ga_i)
>               ga_i = ga_i + 1               
>             enddo
>             
> 
>                timp_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             
>                smfmn_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             
>                smfmx_pga = vars_Rga(ga_i)
>                ga_i = ga_i + 1   
>             
>                            
>             do ga_j = 1,numsub
>                chl1_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chs1_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chw1_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chn1_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chk1_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chl2_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chs2_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chw2_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             do ga_j = 1,numsub
>                chd_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chk2_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                chwdr_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,numsub
>                alpha_bnk_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
> 
>             do ga_j = 1,nohru
>                gwqmn_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                rchrgdp_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
>             do ga_j = 1,nohru
>                gwspyld_pga(ga_j) = vars_Rga(ga_i)
>                ga_i = ga_i + 1               
>             enddo
>             
75d263
< 
78a267,286
> c	allocate and initialize for uncertainty
> 	if (iclb.eq.9) then
> 	allocate(stprain(mhru,2))
> 	allocate(stptemp(mhru,2))
> 	allocate(stprad(mhru,2))
> 	allocate(stprhd(mhru,2))
> 	allocate(stpfrt(mhru,2))
> 	allocate(stppst(mhru,2))
> 	allocate(stpcst(mrecc,18,2))
> 	allocate(isamp(7))
> 	stpcst=0.
> 	stprain=0.
> 	stprad=0.
> 	stptemp=0.
> 	stprhd=0.
> 	stpfrt=0.
> 	stppst=0.
> 	isamp=0
> 	end if  
> 
79a288,298
> !!!!!!  values for readbsn!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
>       smfmn = smfmn_pga
>       smfmx = smfmx_pga
>       timp = timp_pga
>       surlag = surlag_pga
>       msk_co1 = msk_co1_pga
>       msk_co2 = msk_co2_pga
>       msk_x =   msk_x_pga
>       trnsrch = trnsrch_pga
>       evrch = evrch_pga      
> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
82c301
<       call readplant             !! read in the landuse/landcover database
---
>       call readcrop              !! read in the landuse/landcover database
87,88d305
<       call readseptwq            !! read in the septic types database     
<       call readlup
97,102d313
< 
< 
<       if (isproj == 2) then 
<         hi_targ = 0.0
<       end if
< 
129c340,352
<       do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
---
>          
>         ga_j = 1
>         rchdy21_Rga = 0
>         do dsim = 1,4018   !!! number of days: pcp.pcp lines/24
>            do hsim =1,24
>               rchdy21_Rga(ga_j) = hflow_pga(dsim,hsim)
>               ga_j= ga_j +1
>            end do
>         enddo 
>         
>       call freearray
>          
>       do i = 1, 2000
132,133c355
<       close(124)
<       write (*,1001)
---
> !      write (*,1001)
139c361,362
< 	stop
---
> !	stop
>        return
