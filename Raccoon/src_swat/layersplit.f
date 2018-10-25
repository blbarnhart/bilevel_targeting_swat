      subroutine layersplit(dep_new)

      use parm
      integer nly
	nly = sol_nly(ihru)

!!    create a septic layer
!! changed all sol_zmx(ihru) in subroutine to dep_new 1/27/09 gsm 
      if (dep_new > 0.001) then
        flag = 0
        do j = 1, nly - 1
          xx = 0.
          yy = 0.
          xx = Abs(dep_new - sol_z(j,ihru))
          yy = Abs(dep_new - sol_z(j+1,ihru))
          !! if values are within 51 mm of one another, reset boundary
          if (xx < 10. .and. yy > 10.) then
            sol_z(j,ihru) = dep_new
 !! added the new_layer = j statement below 1/27/09 gsm
!$$$$$$             new_layer = j
            exit
          end if

          !! set a soil layer at sol_zmx and adjust all lower layers
          if (sol_z(j,ihru) > dep_new) then
            flag = 1
            sol_nly(ihru) = sol_nly(ihru) + 1
            nly = nly + 1
            jj = 0
            jj = j + 1
            do n = nly, jj, -1
              sol_z(n,ihru) = sol_z(n-1,ihru)
              sol_bd(n,ihru) = sol_bd(n-1,ihru)
              sol_awc(n,ihru) = sol_awc(n-1,ihru)
              sol_k(n,ihru) = sol_k(n-1,ihru)
              sol_cbn(n,ihru) = sol_cbn(n-1,ihru)
              sol_clay(n,ihru) = sol_clay(n-1,ihru)
              sol_ec(n,ihru) = sol_ec(n-1,ihru)
              sol_no3(n,ihru) = sol_no3(n-1,ihru)
              sol_orgn(n,ihru) = sol_orgn(n-1,ihru)
              sol_orgp(n,ihru) = sol_orgp(n-1,ihru)
              sol_solp(n,ihru) = sol_solp(n-1,ihru)
              sol_mc(n,ihru) = sol_mc(n-1,ihru)
              sol_mn(n,ihru) = sol_mn(n-1,ihru)
              sol_mp(n,ihru) = sol_mp(n-1,ihru)

			  sol_n(n,ihru) = sol_n(n-1,ihru)

            end do
            sol_z(j,ihru) = dep_new
	      iseptic = n 
          end if
          if (flag == 1) exit
        end do
      end if
      end        
       
       