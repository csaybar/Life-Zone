module holdridge_calc

integer,parameter,public::dp=4
public::scan_find

contains

    function VI_find(ic)result(VI)
     integer,intent(in)::ic
     character(len=20)::VI
            !
         if (IC <  2) then
           VI="Polar_Desert"
           return
         end if
         !
         if(ic == 2) VI="Dry_Tundra"
         if(ic == 3) VI="Moist_Tundra"
         if(ic == 4) VI="Wet_Tundra"
         if(ic == 5) VI="Rain_Tundra"
          !
         if(ic == 6 .or. ic == 11 .or. ic == 17 .or. ic == 24 .or. ic == 31) VI="Desert"
         if(ic == 7) VI="Dry_Scrub"
         if(ic == 8 .or. ic == 14 .or. ic == 21 .or. ic == 28 .or. ic == 36 ) VI="Moist_Forest"
         if(ic == 9 .or. ic == 15 .or. ic == 22 .or. ic == 29 .or. ic == 37) VI="Wet_Forest"
         if(ic == 10.or. ic == 16 .or. ic == 23 .or. ic == 30 .or. ic == 38) VI="Rain_Forest"
         if(ic == 27 .or. ic == 35) VI="Dry_Forest"  
         if(ic == 26 .or. ic == 33) VI="Thorn_Woodland"
         if(ic == 12 .or. ic == 18 .or.ic == 25 .or. ic == 32) VI="Desert_Scrub"
         if(ic == 13) VI="Steppe"
         if(ic == 19) VI="Thorn_Steppe"
         if(ic == 20) VI="Dry_Forest"  
         if(ic == 34) VI="Very_Dry_Forest"
         return
      end function VI_find
!------------------------------------------------
 subroutine scan_find(BT,PE,R,LAT,ic,ZONE,VI)
   real(kind=dp),intent(in)::BT,R,LAT,PE
   integer,intent(out)::ic
   character(len=*),intent(out)::zone,VI
   real(kind=dp),dimension(91)::PE91,Tmin91,Tmax91,Cls91,Prcp91
   real(kind=dp),dimension(30,3)::dem
   real(kind=dp)::dist,d,pe2,r2,tmin,tmax
   integer::j,iz
   !
   PE91=(/0.750,0.375,0.188,1.500,0.750,0.375,0.188,0.750,0.375,0.188,3.000,1.500, &
       0.750,0.375,0.188,3.000,1.500,0.750,0.375,0.188,1.500,0.750,0.375,0.188,  &
       6.000,3.000,1.500,0.750,0.375,0.188,6.000,3.000,1.500,0.750,0.375,0.188,  &
       3.000,1.500,0.750,0.375,0.188,12.000,6.000,3.000,1.500,0.750,0.375,0.188, &
       12.000,6.000,3.000,1.500,0.750,0.375,0.188,6.000,3.000,1.500,0.750,0.375, &
       0.188,12.000,6.000,3.000,1.500,0.750,0.375,0.188,24.000,12.000,6.000,3.000, &
       1.500,0.750,0.375,0.188,24.000,12.000,6.000,3.000,1.500,0.750,0.375,0.188, &
       12.000,6.000,3.000,1.500,0.750,0.375,0.188/)
   !
   cls91=(/-1,0,1,2,3,4,5,-1,0,1,6,7,8,9,10,6,7,8,9,10,2,3,4,5,11,12,13, &
        14,15,16,11,12,13,14,15,16,6,7,8,9,10,17,18,19,20,21,22,23,17, &
        18,19,20,21,22,23,11,12,13,14,15,16,24,25,26,27,28,29,30,31,32, &
        33,34,35,36,37,38,31,32,33,34,35,36,37,38,24,25,26,27,28,29,30/)
   !
   Prcp91= (/62.5,187.5,375.0,62.5,187.5,375.0,750.0,62.5,187.5,375.0,62.5, &
         187.5,375.0,750.0,1500.0,62.5,187.5,375.0,750.0,1500.0,62.5,187.5, &
         375.0,750.0,62.5,187.5,375.0,750.0,1500.0,3000.0,62.5,187.5,375.0, &
         750.0,1500.0,3000.0,62.5,187.5,375.0,750.0,1500.0,62.5,187.5,375.0, &
         750.0,1500.0,3000.0,6000.0,62.5,187.5,375.0,750.0,1500.0,3000.0,6000.0, &
         62.5,187.5,375.0,750.0,1500.0,3000.0,62.5,187.5,375.0,750.0,1500.0,3000.0, &
         6000.0,62.5,187.5,375.0,750.0,1500.0,3000.0,6000.0,12000.0,62.5,187.5,375.0, &
         750.0,1500.0,3000.0,6000.0,12000.0,62.5,187.5,375.0,750.0,1500.0,3000.0,6000.0/)
   !
    Tmin91(1:3)=0
    Tmin91(4:15)=1.5
    Tmin91(16:30)=3
    Tmin91(31:48)=6
    Tmin91(49:61)=12
    Tmin91(62:76)=18
    Tmin91(77:91)=24
    !
    Tmax91(1:3)=1.5
    Tmax91(4:15)=3
    Tmax91(16:30)=6
    Tmax91(31:48)=12
    Tmax91(49:61)=18
    Tmax91(62:76)=24
    Tmax91(77:91)=100
    !
    !PE=(BT*58.93)/R
    pe2=log(pe+1)/log(2.0)
    r2=log(r+1)/log(2.0)
    !
   IF ((BT <= 1.5) .AND. (PE <= 0.3)) THEN
     IF (LAT > 80.0) THEN
      ZONE='HIGHPOLAR_REGION'
      IC=0
      VI=VI_find(ic)
      return
    ELSE
      ZONE='NIVAL_ZONE'
      IC=1
      VI=VI_find(ic)
      return
    END IF
  END IF
 IF ((BT > 0.3  .AND.  BT <= 0.75) .AND. (PE > 0.3)) THEN
   IF (LAT > 67.37)THEN
     ZONE='POLAR_REGION'
     IC=0
     VI=VI_find(ic)
     return
   ELSE
    ZONE='SUBNIVAL'
   ! VT='AEOLIAN_ZONE'
    IC=1
    VI=VI_find(ic)
    return
   ENDIF
END IF
  !
 IF((BT > 0.75 .and. BT <= 1.5) .and. (PE > 0.3)) THEN
   IF (LAT > 67.37) THEN
     ZONE='POLAR_REGION'
     IC=0
     VI=VI_find(ic)
     return
   ELSE
      IF(PE > 0.5) THEN
      ! VT='COLD DESERT'
       ZONE='SUBNIVAL'
      IC=1
      VI=VI_find(ic)
      return
     ELSE
       ZONE='SUBNIVAL' 
      !VT='PERIGLACIAL_VEGETATION'
      IC=1
      VI=VI_find(ic)
      return
     END IF
   ENDIF
 END IF
 !
  ! 
   dem=0
!
       iz=0
     do j=1,91
       tmin=Tmin91(j)
       tmax=Tmax91(j)
       if(BT > tmin .and. BT <= tmax) then
         iz=iz+1
         dem(iz,1)=cls91(j) 
         dem(iz,2)=log(pe91(j)+1)/log(2.0)
         dem(iz,3)=log(prcp91(j)+1)/log(2.0)
       end if
     end do   
      !
     ic=-99
     dist=10000000
      do j=1,iz
        d=sqrt((R2-dem(j,3))**2+(PE2-dem(j,2))**2)
        if(d <= dist) then
          ic=int(dem(j,1))
          dist=d
        end if
      end do
      VI=VI_find(ic)
      !
   IF(ic >= 2 .AND. ic <= 5) THEN
    IF ((LAT > 63.75) .AND. (LAT <= 67.37)) THEN
     ZONE='SUBPOLAR'
     return
    ELSE
     ZONE='ALPINE'
     return
    END IF
  END IF
  !
    IF (ic >= 6 .AND. ic <= 10) THEN
      IF (LAT > 56.50 .AND. LAT<=63.75) THEN
       ZONE='BOREAL'
       return
      ELSE
       ZONE='SUBALPINE'
      ENDIF
    END IF  
  !
  if(ic >= 6 .and. ic <= 10) then 
   IF (LAT > 42.0 .AND. LAT <= 56.50) THEN
    ZONE='BOREAL'
    return
   ELSE
    ZONE='MONTANE'
    return
   ENDIF
  END IF
  !
  if (ic >= 11 .and. ic <= 16) then 
     ZONE='COOL_TEMPERATE'
     return
  end if
  !
    IF(ic >= 17 .AND. ic <= 23) THEN
      ZONE='WARM_TEMPERATE'
      return
    END IF
      !
   if(ic >= 24 .AND. ic <= 30) then   
     IF (LAT > 13.0 .AND. LAT <= 27.5) THEN
      ZONE='SUBTROPICAL'
      return
     ELSE
      ZONE='LOWER_MONTANE'
      return
     ENDIF
    END IF
    !
    IF(BT > 24.0)ZONE='TROPICAL'
      !
      return
   end subroutine scan_find
  end module holdridge_calc
!!
  program test
  use holdridge_calc
  integer,parameter::n=106800,nx=3
  real(kind=dp),dimension(n,nx)::test_input
  real(kind=dp)::R,BT,LAT,PE
  character(len=20)::Zone,VI
  character(len=10)::head1,head2,head3
  integer::ic
  
!
   open(unit=10,file="test_input.txt")
   read(unit=10,fmt=*)head1,head2,head3
     do j=1,n
       read(unit=10,fmt=*)test_input(j,1:nx)
     end do
     close(10) 
   open(unit=10,file="HLZ_out.txt")
   write(unit=10,fmt="(4a9,a6,a6,a20,a6,a20)")"LAT","BT","PE","Rain","IIASA"," ","Zone"," ","VI"
   do j=1,n
    LAT=test_input(j,1) !latitude
    BT =  test_input(j,2) !Bio-temp
    R=     test_input(j,3)    ! rain
    PE =         (BT*58.93)/R !Potential evap
      call scan_find(BT,PE,R,LAT,ic,Zone,VI)
     write(unit=10,fmt="(4f9.1,i6,a6,a20,a6,a20)")LAT,BT,PE,R,ic," ",trim(Zone)," ",trim(VI)
   end do
   close(10)  
end program test
      
