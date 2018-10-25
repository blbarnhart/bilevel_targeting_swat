      subroutine latsed

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the sediment load contributed in lateral flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_km(:)   |km^2          |area of HRU in square kilometers
!!    ihru        |none          |HRU number
!!    lat_sed(:)  |g/L           |sediment concentration in lateral flow
!!    latq(:)     |mm H2O        |total lateral flow in soil profile for the
!!                               |day in HRU
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j

      j = 0
      j = ihru

      !! update sediment yield for sediment in lateral flow
      sedyld(j) = sedyld(j) +                                           &
     &                      (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j)

      sanyld(j) = sanyld(j) +                                           &
     &         (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_san(j)
      silyld(j) = silyld(j) +                                           &
     &         (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_sil(j)
      clayld(j) = clayld(j) +                                           &
     &         (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_cla(j)
      sagyld(j) = sagyld(j) +                                           &
     &         (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_sag(j)
      lagyld(j) = lagyld(j) +                                           &
     &         (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_lag(j)

      if (sedyld(j) < 0.) sedyld(j) = 0.
      if (sedyld(j) < 0.) sanyld(j) = 0.0
      if (sedyld(j) < 0.) silyld(j) = 0.0
      if (sedyld(j) < 0.) clayld(j) = 0.0
      if (sedyld(j) < 0.) sagyld(j) = 0.0
      if (sedyld(j) < 0.) lagyld(j) = 0.0

      return
      end

