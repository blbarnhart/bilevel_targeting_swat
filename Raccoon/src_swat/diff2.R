73a74,75
>          delay(ihru) = gwdelay_pga(ihru)
> 
74a77,78
>          alpha_bf(ihru) = alpha_bf_pga(ihru)
> 
75a80,81
>          gwqmn(ihru) = gwqmn_pga(ihru)
> 
76a83,84
>          gw_revap(ihru) = gw_revap_pga(ihru)
> 
77a86,87
>          revapmn(ihru) = revapmn_pga(ihru)
> 
78a89,90
>          rchrg_dp(ihru) = rchrgdp_pga(ihru)
> 
80a93
> 
81a95
>          gw_spyld(ihru) = gwspyld_pga(ihru)
82a97
> 
96,110c111
<       if (hlife_ngw <= 0.) hlife_ngw = 365.
< 
< !!    set default values for mike van liew
<       if (hlife_ngw <= 0.) hlife_ngw = hlife_ngw_bsn
< !!    set default values for mike van liew
< 
< !!    perform additional calculations
<       alpha_bfe(ihru) = Exp(-alpha_bf(ihru))
<       gw_delaye(ihru) = Exp(-1./(delay(ihru) + 1.e-6))
<       shallst_n(ihru) = shallst_n(ihru) * shallst(ihru) / 100.
<       gw_nloss(ihru) = Exp(-.693 / hlife_ngw)
< 
< !! assign values to channels
<       ch_revap(i) = gw_revap(ihru)
< 
---
>       if (hlife_ngw <= 0.) hlife_ngw = 365. 
