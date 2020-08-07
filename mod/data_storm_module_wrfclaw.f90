! ==============================================================================
! model_storm_module 
!
! Module contains routines for constructing a wind and pressure field based on a
! provided set of data files.
!
! ==============================================================================
!                   Copyright (C) Clawpack Developers 2017
!  Distributed under the terms of the Berkeley Software Distribution (BSD) 
!  license
!                     http://www.opensource.org/licenses/
! ------------------------------------------------------------------------------
! 2020/01/31, v1, NF added what are written in wrf_storm_module.f90
!                 --> storm_location, set_wrf_storm_fields etc.
! ==============================================================================
module data_storm_module

    implicit none
    save

    logical, private :: module_setup = .false.

    ! Internal tracking variables for storm
	logical, private :: DEBUG = .true. 
    !logical, private :: DEBUG = .false. 

    ! Tolerance for floating point inequalities
	real(kind=8), parameter :: eps = 1.0e-8 

    ! WRF storm type definition
    ! Specified wind & pressure field 
    type data_storm_type
        ! Size of spatial grids,
        !  corresponds to lengths of lat/lon arrays
        integer :: num_lats
        integer :: num_lons

        ! Location of storm field values
        ! longitude and latitude arrays
        ! start from SW corner
        real(kind=8) :: ll_lon, ur_lon
        real(kind=8) :: ll_lat, ur_lat
        real(kind=8) :: dx, dy
        !real(kind=8), allocatable :: lat(:)
        !real(kind=8), allocatable :: lon(:)

        ! We keep two time snapshots in memory 
        !  for interpolation, with t_next > t > t_prev
        real(kind=8) :: t_next, t_prev
        real(kind=8), allocatable :: u_next(:,:)
        real(kind=8), allocatable :: v_next(:,:)
        real(kind=8), allocatable :: p_next(:,:)
        real(kind=8), allocatable :: u_prev(:,:)
        real(kind=8), allocatable :: v_prev(:,:)
        real(kind=8), allocatable :: p_prev(:,:)
        ! The values will be updated incrementally:
        !  when t>t_next, replace older states with newer states
        !  and read in next snapshot.
        
        ! These will be used during the interpolation step
        !  for times between t_prev and t_next
        real(kind=8) :: t
        real(kind=8), allocatable :: u(:,:)
        real(kind=8), allocatable :: v(:,:)
        real(kind=8), allocatable :: p(:,:)

        ! The estimate center of the storm 
        !  is also stored for interpolation
        integer :: eye_next(2)
        integer :: eye_prev(2)

        ! Keep track of how many snapshots have been read
        integer :: last_storm_index

        ! Store the storm data file for repeated reading
        character(len=4096) :: data_path

    end type data_storm_type

contains

    ! Setup routine for the WRF storm model
    ! Open data files, get parameters, allocate memory,
    !  and read in first two time snapshots of storm data
    subroutine set_storm(storm_data_path, storm, storm_spec_type, log_unit)

    use geoclaw_module, only: coordinate_system, ambient_pressure
    use amr_module, only: t0

        implicit none

        ! Subroutine I/O
        character(len=*), optional :: storm_data_path
        type(data_storm_type), intent(in out) :: storm
        integer, intent(in) :: storm_spec_type, log_unit

    ! Local storage
        integer, parameter :: l_file = 701
        integer :: i, j, iostatus
        !character(len=4096) :: storm_data_path
        real(kind=8) :: ll_lon, ll_lat, ur_lon, ur_lat, dx, dy

        ! Reading buffer variables
        character(len=100) :: dummy_read
        integer :: readsize

        ! Storm type only works on lat-long coordinate systems
        !if (coordinate_system /= 2) then
        !    stop "explicit storm type does only works on lat-long coordinates."
        !endif

        ! We need to count two things:
        !   number of latitude coords (ny)
        !   number of longitude coords (nx)
        ! We don't need number of time snapshots (nt)
        !   since we will load snapshots only as needed
        ! The datafiles for lat & lon contain nx*ny values
        ! The datafiles for u, v, p contain nx*ny*nt values

        ! Open SuWAT configuration file
        if (present(storm_data_path)) then
            storm%data_path = storm_data_path
        else
            storm%data_path = './'
        endif
        storm_data_path = trim(storm%data_path) // "1d.con"
        print *,'Reading storm config data file ',trim(storm_data_path)
        open(unit=l_file,file=storm_data_path,status='old', &
                action='read',iostat=iostatus)
        if (iostatus /= 0) then
            print *, "Error opening SuWAT config data file. status = ", iostatus
            stop 
        endif            

        ! Read config file for num_lons, num_lats
        ! Line 1 
        read(l_file, *, iostat=iostatus) storm%num_lons, storm%num_lats

        ! Allocate memory for lat & lon coords and u/v/p fields
        allocate(storm%u_prev(storm%num_lons,storm%num_lats))
        allocate(storm%v_prev(storm%num_lons,storm%num_lats))
        allocate(storm%p_prev(storm%num_lons,storm%num_lats))
        allocate(storm%u_next(storm%num_lons,storm%num_lats))
        allocate(storm%v_next(storm%num_lons,storm%num_lats))
        allocate(storm%p_next(storm%num_lons,storm%num_lats))
        allocate(storm%u(storm%num_lons,storm%num_lats))
        allocate(storm%v(storm%num_lons,storm%num_lats))
        allocate(storm%p(storm%num_lons,storm%num_lats))

        ! Line 2, lower left corner and dy
        read(l_file, *, iostat=iostatus) storm%ll_lon, storm%ll_lat, storm%dy
        ! Line 3, upper right corner and dx
        read(l_file, *, iostat=iostatus) storm%ur_lon, storm%ur_lat, storm%dx
        close(l_file)

        ! TEMPORARY
        ! Shift the storm to match bathymetry
        !storm%lat = storm%lat + 0.15
        !storm%lon = storm%lon + 0.05

        ! This is used to speed up searching for correct storm data
        !  (using ASCII datafiles)
        storm%last_storm_index = 0

        ! Read in the first storm data snapshot as 'next'
        !  and increment storm%last_storm_index to 1
        call read_wrf_storm(storm,t0)

        ! Check if starting time of simulation
        !  is before the first storm data snapshot
        if (t0 < storm%t_next - eps) then
            print *, "Simulation start time precedes storm data. Using clear skies."
            if (DEBUG) print *, "t0=", t0, "first storm t:",storm%t_next
            storm%t_prev = t0
            storm%u_prev = 0
            storm%v_prev = 0
            storm%p_prev = ambient_pressure
            storm%eye_prev = storm%eye_next
        else
            ! Read in the second storm data snapshot as 'next',
            !  update 'prev' with old 'next' data,
            !  and increment storm%last_storm_index to 2
            call read_wrf_storm(storm,t0)
        endif

        ! Initialize current storm module data
        storm%t = t0
        ! Interpolate wind & pressure fields using prev and next snapshots
        call storm_interpolate(storm)

        !stop "Data-derived storm are not yet implemented!"

        if (.not. module_setup) then

            module_setup = .true.

        end if

    end subroutine set_storm

    ! ==========================================================================
    !  real(kind=8) pure date_to_seconds(year,months,days,hours,minutes,seconds)
    !    Convert time from year, month, day, hour, min, sec to seconds since the
    !    beginning of the year.
    ! ==========================================================================
    !pure real(kind=8) function date_to_seconds(year,months,days,hours,minutes, &
                                               !seconds) result(time)
    pure integer function date_to_seconds(year,months,days,hours,minutes) result(time)
      
        implicit none

        ! Input
        integer, intent(in) :: year, months, days, hours, minutes

        ! Local storage
        integer :: total_days

        ! Count number of days
        total_days = days

        ! Add days for months that have already passed
        if (months > 1) total_days = total_days + 31
        if (months > 2) then
            if (int(year / 4) * 4 == year) then
                total_days = total_days + 29
            else
                total_days = total_days + 28
            endif
        endif
        if (months > 3)  total_days = total_days + 30
        if (months > 4)  total_days = total_days + 31
        if (months > 5)  total_days = total_days + 30
        if (months > 6)  total_days = total_days + 31
        if (months > 7)  total_days = total_days + 30
        if (months > 8)  total_days = total_days + 31
        if (months > 9)  total_days = total_days + 30
        if (months > 10) total_days = total_days + 31
        if (months > 11) total_days = total_days + 30

        ! Convert everything to seconds since the beginning of the year
        time = (total_days - 1) * 86400 + hours * 3600 + minutes * 60

    end function date_to_seconds

    ! ==========================================================================
    !  read_wrf_storm_data_file()
    !    Opens storm data file and reads next storm entry
    !    Currently only for ASCII file
    !  This file will probably need to be modified
    !   to suit the input dataset format.
    ! ==========================================================================
    subroutine read_wrf_storm_file(data_path,storm_array,num_lats,last_storm_index,timestamp)

        implicit none

        ! Subroutine I/O
        real(kind=8), intent(in out) :: storm_array(:,:)
        character(len=*), intent(in) :: data_path
        integer, intent(in) :: num_lats, last_storm_index
        integer, intent(inout) :: timestamp

        ! Local storage
        integer :: j, k, iostatus
        integer :: yy, mm, dd, hh, nn
        integer, parameter :: data_file = 701

        ! Open the input file
        !
        open(unit=data_file,file=data_path,status='old', &
                action='read',iostat=iostatus)
        if (iostatus /= 0) then
            print *, "Error opening data file: ",trim(data_path)
            print *, "Status = ", iostatus
            stop 
        endif            
        ! Advance to the next time step to be read in
        ! Skip entries based on total number previously read
        do k = 1, last_storm_index
            do j = 1, num_lats + 1
                read(data_file, *, iostat=iostatus)
                ! Exit loop if we ran into an error or we reached the end of the file
                if (iostatus /= 0) then
                    print *, "Unexpected end-of-file reading ",trim(data_path)
                    print *, "Status = ", iostatus
                    if (DEBUG) print *, "k, laststormindex = ", k, last_storm_index
                    if (DEBUG) print *, "j, num_lats = ", j, num_lats
                    timestamp = -1
                    close(data_file) 
                    return
                endif
            enddo
        enddo
        ! Read in next time snapshot 
        ! example:
        ! ____108000 7908251800 (EDIT from here)
        read(data_file, 600, iostat=iostatus) & 
            yy, mm, dd, hh, nn
    600 FORMAT(11x,i2,i2,i2,i2,i2)
        !read(data_file, (11x,i2,i2,i2,i2,i2), iostat=iostatus) & 
            !yy, mm, dd, hh, nn
        do j = 1, num_lats
            read(data_file, *, iostat=iostatus) storm_array(:,j) 
            ! Exit loop if we ran into an error or we reached the end of the file
            if (iostatus /= 0) then
                print *, "Unexpected end-of-file reading ",trim(data_path)
                print *, "Status = ", iostatus
                if (DEBUG) print *, "j, num_lats = ", j, num_lats
                timestamp = -1
                close(data_file) 
                return
            endif
        enddo

        ! Convert datetime to seconds
        timestamp = date_to_seconds(yy,mm,dd,hh,nn)
        close(data_file) 

    end subroutine read_wrf_storm_file
   
    ! ==========================================================================
    !  read_wrf_storm_data()
    !    Reads storm fields for next time snapshot
    !    Currently only for ASCII files
    ! ==========================================================================

    subroutine read_wrf_storm(storm,t)

        use geoclaw_module, only: ambient_pressure

        implicit none

        ! Subroutine I/O
        type(data_storm_type), intent(in out) :: storm
        real(kind=8), intent(in) :: t

        ! Local storage
        character(len=4096) :: data_path
        real(kind=8) :: lowest_p

        ! Reading buffer variables
        integer :: timestamp

        ! Overwrite older storm states with newer storm states
        storm%t_prev = storm%t_next
        storm%u_prev = storm%u_next 
        storm%v_prev = storm%v_next 
        storm%p_prev = storm%p_next 
        storm%eye_prev = storm%eye_next
        
        ! Current time t currently unused in favor of storm%last_storm_index.
        ! This should probably be changed in the future.

        ! Read the u-velocity file
        data_path = trim(storm%data_path) // "u10_d01.swt"
        call read_wrf_storm_file(data_path,storm%u_next,storm%num_lats,storm%last_storm_index,timestamp)
        ! Error handling: set to clear skies if file ended
        if (timestamp == -1) then
            storm%u_next = 0
            storm%t_next = storm%t_next + 365*24*60
        else
            ! Save timestamp (sec) of next snapshot
            storm%t_next = timestamp
        endif

        ! Read v-velocity file
        data_path = trim(storm%data_path) // "v10_d01.swt"
        call read_wrf_storm_file(data_path,storm%v_next,storm%num_lats,storm%last_storm_index,timestamp)
        ! Error handling: set to clear skies if file ended
        if (timestamp == -1) then
            storm%v_next = 0
        endif

        ! Read pressure file
        data_path = trim(storm%data_path) // "slp_d01.swt"
        call read_wrf_storm_file(data_path,storm%p_next,storm%num_lats,storm%last_storm_index,timestamp)
        ! Error handling: set to clear skies if file ended
        if (timestamp == -1) then
            storm%p_next = ambient_pressure
            storm%eye_next = [0,0]
        else
            ! Convert pressure units: mbar to Pa
            storm%p_next = storm%p_next * 1.0e2
            ! Estimate storm center location based on lowest pressure
            ! (only the array index is saved)
            storm%eye_next = MINLOC(storm%p_next)
            ! If no obvious low pressure area, set storm center to 0 instead
            lowest_p = storm%p_next(storm%eye_next(1),storm%eye_next(2))
            if (lowest_p > ambient_pressure*0.99) then
                storm%eye_next = [0,0]
            endif
        endif

        ! Update number of storm snapshots read in
        storm%last_storm_index = storm%last_storm_index + 1
        if (DEBUG) print *, "last_storm_index=", storm%last_storm_index

    end subroutine read_wrf_storm
    ! ==========================================================================
    !  storm_location(t,storm)
    !    Interpolate location of hurricane in the current time interval
    ! ==========================================================================
    function storm_location(t,storm) result(location)

        use amr_module, only: rinfinity

        implicit none

        ! Input
        real(kind=8), intent(in) :: t
        type(data_storm_type), intent(in out) :: storm

        ! Output
        real(kind=8) :: location(2)

        ! Local storage
        real(kind=8) :: alpha
        integer :: eye(2)

        ! Estimate location based on two nearest snapshots 

        ! If no data at this time, return infinity
        if ((t < storm%t_prev - eps) .OR. (t > storm%t_next + eps)) then
            location = [rinfinity,rinfinity]
        else if ((storm%eye_prev(1) == 0) .AND. (storm%eye_next(1) == 0)) then
            location = [rinfinity,rinfinity]
        else
            ! Otherwise check if there is a low pressure system
            !  and if so interpolate eye location from snapshots
            if (storm%eye_prev(1) == 0) then
                eye = storm%eye_next
            else if (storm%eye_next(1) == 0) then
                eye = storm%eye_prev
            else
                ! Determine the linear interpolation parameter (in time)
                if (storm%t_next-storm%t_prev < eps) then
                    print *, "t_next = ", storm%t_next,"t_prev = ", storm%t_prev
                    print *, "t = ", t, "storm%t = ", storm%t
                    alpha = 0
                else
                    alpha = (t-storm%t_prev) / (storm%t_next-storm%t_prev)
                endif
                ! Estimate location index of storm center at time t
                eye = storm%eye_prev + NINT((storm%eye_next - storm%eye_prev) * alpha)
            endif
            ! Convert to lat-lon
            location(1) = storm%ll_lon + (eye(1)-1)*storm%dx
            !location(2) = storm%ll_lat + (eye(2)-1)*storm%dy
            location(2) = storm%ur_lat - (eye(2)-1)*storm%dy
        endif

        !stop "Data-derived storm are not yet implemented!"

    end function storm_location

    ! ==========================================================================
    !  storm_direction
    !   Angle off of due north that the storm is traveling
    ! ==========================================================================
    real(kind=8) function storm_direction(t, storm) result(theta)

        implicit none

        ! Input
        real(kind=8), intent(in) :: t
        type(data_storm_type), intent(in) :: storm

        stop "Data-derived storm are not yet implemented!"

    end function storm_direction

    ! ==========================================================================
    !  Use the 1980 Holland model to set the storm fields
    ! ==========================================================================
    subroutine set_HWRF_fields(maux, mbc, mx, my, xlower, ylower,    &
                          dx, dy, t, aux, wind_index,           &
                          pressure_index, storm)

  
        implicit none

        ! Time of the wind field requested
        integer, intent(in) :: maux,mbc,mx,my
        real(kind=8), intent(in) :: xlower,ylower,dx,dy,t

        ! Storm description, need in out here since we may update the storm
        ! if at next time point
        type(data_storm_type), intent(in out) :: storm

        ! Array storing wind and presure field
        integer, intent(in) :: wind_index, pressure_index
        real(kind=8), intent(inout) :: aux(maux,1-mbc:mx+mbc,1-mbc:my+mbc)

        stop "HWRF data input is not yet implemented!"

    end subroutine set_HWRF_fields

    ! ==========================================================================
    !  set_wrf_storm_fields() added
    ! ==========================================================================
    subroutine set_wrf_storm_fields(maux, mbc, mx, my, xlower, ylower,    &
        dx, dy, t, aux, wind_index,           &
        pressure_index, storm)

        !use geoclaw_module, only: ambient_pressure
        use geoclaw_module, only: g => grav, rho_air, ambient_pressure
        use geoclaw_module, only: coriolis, deg2rad
        use geoclaw_module, only: spherical_distance

        use geoclaw_module, only: rad2deg  


        implicit none

        ! Time of the wind field requested
        integer, intent(in) :: maux,mbc,mx,my
        real(kind=8), intent(in) :: xlower,ylower,dx,dy,t

        ! Storm description, need in out here since we may update the storm
        ! if at next time point
        type(data_storm_type), intent(in out) :: storm

        ! Array storing wind and presure field
        integer, intent(in) :: wind_index, pressure_index
        real(kind=8), intent(inout) :: aux(maux,1-mbc:mx+mbc,1-mbc:my+mbc)

        ! Local storage
        real(kind=8) :: x, y
        integer :: i,j,k,l

        if (t < storm%t_prev - eps) then
        print *, "Simulation time precedes storm data in memory. &
            Race condition?"
        print *, "t=",t,"< t_prev=",storm%t_prev,"t_next=",storm%t_next
        endif

        if (t > storm%t_next + eps) then
        ! Load two snapshots into memory, at times t_next and t_prev
        !$OMP CRITICAL (READ_STORM)
        do while (t > storm%t_next + eps)
        ! update all storm data, including value of t_next
        if (DEBUG) print *,"loading new storm snapshot ",&
                        "t=",t,"old t_next=",storm%t_next
        call read_wrf_storm(storm,t)
        if (DEBUG) print *,"new t_next=",storm%t_next
        ! If storm data ends, the final storm state is used.
        enddo
        !$OMP END CRITICAL (READ_STORM)
        endif

        ! Interpolate storm data in time
        ! t_prev <= t <= t_next
        if (t > storm%t + eps) then
        !$OMP CRITICAL (INTERP_STORM)
        if (t > storm%t + eps) then
        ! Update storm data by interpolation
        call storm_interpolate(storm)
        ! Update current time in storm module (race condition?)
        storm%t = t
        endif
        !$OMP END CRITICAL (INTERP_STORM)
        endif

        ! Set fields
        ! Determine lat/long of each cell in layer,
        !  determine corresponding storm cell indices
        !  (or nearest cell index if out-of-bound)
        !  then get value of corresponding storm data cell.
        do j=1-mbc,my+mbc
        y = ylower + (j-0.5d0) * dy     ! Degrees latitude
        k = get_lat_index(y,storm) ! storm index of latitude
        ! Check for out-of-bounds condition
        if (k == 0) then
        ! Out of bounds
        do i=1-mbc,mx+mbc
        ! Set storm components to ambient condition
        aux(pressure_index,i,j) = ambient_pressure
        aux(wind_index,i,j)   = 0.0
        aux(wind_index+1,i,j) = 0.0
        enddo
        else
        ! Within latitude range
        do i=1-mbc,mx+mbc
        x = xlower + (i-0.5d0) * dx   ! Degrees longitude
        l = get_lon_index(x,storm) ! storm index of longitude
        ! Check for out-of-bounds condition
        if (l == 0) then
        ! Out of bounds
            ! Set storm components to ambient condition
            aux(pressure_index,i,j) = ambient_pressure
            aux(wind_index,i,j)   = 0.0
            aux(wind_index+1,i,j) = 0.0
        else
        ! Within longitude range
            ! Set pressure field
            aux(pressure_index,i,j) = storm%p(l,k)
            ! Set velocity components of storm 
            aux(wind_index,i,j)   = storm%u(l,k)
            aux(wind_index+1,i,j) = storm%v(l,k)
        endif
        enddo
        endif
        enddo

    end subroutine set_wrf_storm_fields

    ! ==========================================================================
    !  integer pure get_lat_index(lat)
    !    Returns index of latitude array of the storm data
    !    corresponding to input lat.
    ! ==========================================================================
    pure integer function get_lat_index(lat,storm) result(i)
      
        implicit none

        ! Input
        real(kind=8), intent(in) :: lat
        type(data_storm_type), intent(in) :: storm

        ! Out-of-bound conditions:
        ! EDIT changed to 0.
        ! Will apply ambient conditions in calling function
        if (lat < storm%ll_lat) then
            !i = 1
            i = 0
        else if (lat > storm%ur_lat) then
            !i = storm%num_lats
            i = 0
        else
        ! Determine index based on spacing
            i = 1 + NINT((lat - storm%ll_lat) / storm%dy)
        ! REVERSE top to bottom, based on file format
            i = storm%num_lats + 1 - i                   !!!!!!!!!!!!!!!!!!!!!!
        endif

    end function get_lat_index

    ! ==========================================================================
    !  integer pure get_lon_index(lon)
    !    Returns index of longitude array of the storm data
    !    corresponding to input lon.
    ! ==========================================================================
    pure integer function get_lon_index(lon,storm) result(i)
      
        implicit none

        ! Input
        real(kind=8), intent(in) :: lon
        type(data_storm_type), intent(in) :: storm

        ! Out-of-bound conditions:
        ! EDIT changed to 0.
        ! Will apply ambient conditions in calling function
        if (lon < storm%ll_lon) then
            !i = 1
            i = 0
        else if (lon > storm%ur_lon) then
            !i = storm%num_lons
            i = 0
        else
        ! Determine index based on spacing
            !i = 1 + NINT((lon - storm%ll_lon) / storm%dx)
            i = 1 + INT((lon - storm%ll_lon) / storm%dx)
        endif

    end function get_lon_index

    ! ==========================================================================
    !  storm_interpolate()
    !  Determines intermediate storm values
    !   for time t between t_prev and t_next.
    !  If distinct storms are present at both times,
    !   the storm centers are shifted to an intermediate point
    !   and a weighted average is taken.
    !  Otherwise, no shift occurs and the
    !   weighted average is performed in place.
    ! ==========================================================================
    subroutine storm_interpolate(storm)

        implicit none

        ! Storm description, need "in out" here since will update the storm
        ! values at time t
        type(data_storm_type), intent(in out) :: storm

        ! Check if there are distinct storm "eyes"
        ! If not, interpolate wind & pressure fields in place.
        ! If so, spatially shift storm snapshots then interpolate. 
        if (storm%eye_prev(1) == 0 .or. storm%eye_next(1) == 0) then
            call storm_inplace_interpolate(storm)
        else
            call storm_shift_interpolate(storm)
            ! Optional: no weighted average, only shift prev snapshot in space
            !call storm_shift_only(storm)
        endif

    end subroutine storm_interpolate

    ! ==========================================================================
    !  storm_inplace_interpolate()
    !  Determines intermediate storm values
    !   for time t between t_prev and t_next
    !   based on simple weighted average.
    ! ==========================================================================
    subroutine storm_inplace_interpolate(storm)

        implicit none

        ! Storm description, need "in out" here since will update the storm
        ! values at time t
        type(data_storm_type), intent(in out) :: storm

        ! Local storage
        real(kind=8) :: alpha

        ! This is just a simple weighted average.
        ! Note that this a poor approach for a tropical cyclone:
        !  intensity is smoothed out between intervals
        !  so intermediate values may appear less intense
        ! For a more realistic storm field, use storm_shift_interpolate()

        ! Determine the linear interpolation parameter (in time)
        alpha = (storm%t-storm%t_prev) / (storm%t_next-storm%t_prev)

        ! Take weighted average of two storm fields
        storm%u = storm%u_prev + &
                (storm%u_next - storm%u_prev) * alpha
        storm%v = storm%v_prev + &
                (storm%v_next - storm%v_prev) * alpha
        storm%p = storm%p_prev + &
                (storm%p_next - storm%p_prev) * alpha

    end subroutine storm_inplace_interpolate

    ! ==========================================================================
    !  storm_shift_interpolate()
    !  Determines intermediate storm values
    !   for time t between t_prev and t_next
    !   both in time (linearly) and in space (approximate) 
    ! ==========================================================================
    subroutine storm_shift_interpolate(storm)

        implicit none

        ! Storm description, need "in out" here since will update the storm
        ! values at time t
        type(data_storm_type), intent(in out) :: storm

        ! Local storage
        real(kind=8) :: alpha
        integer :: i,j
        integer :: pi,pj,ni,nj
        integer :: prev_shift(2), next_shift(2)

        ! Determine the linear interpolation parameter (in time)
        alpha = (storm%t-storm%t_prev) / (storm%t_next-storm%t_prev)

        ! Estimate relative location of storm center at time t
        ! Note: The spatial grid is constant in time
        !  so we don't translate to lat-long
        prev_shift = NINT((storm%eye_next - storm%eye_prev) * alpha)
        next_shift = NINT((storm%eye_next - storm%eye_prev) * (alpha - 1))

        ! Now shift the two storm fields onto the intermediate
        !  storm center and use time-weighted average of their values.
        do j = 1,storm%num_lats
            ! If index would be out of bounds, use edge value
            pj = MIN(MAX(1,j-prev_shift(2)),storm%num_lats)
            nj = MIN(MAX(1,j-next_shift(2)),storm%num_lats)
            do i = 1,storm%num_lons
                ! If index would be out of bounds, use edge value
                pi = MIN(MAX(1,i-prev_shift(1)),storm%num_lons)
                ni = MIN(MAX(1,i-next_shift(1)),storm%num_lons)
                ! Perform shift & interpolate
                storm%u(i,j) = storm%u_prev(pi,pj) + &
                    (storm%u_next(ni,nj)-storm%u_prev(pi,pj)) * alpha
                storm%v(i,j) = storm%v_prev(pi,pj) + &
                    (storm%v_next(ni,nj)-storm%v_prev(pi,pj)) * alpha
                storm%p(i,j) = storm%p_prev(pi,pj) + &
                    (storm%p_next(ni,nj)-storm%p_prev(pi,pj)) * alpha
            enddo
        enddo
                

    end subroutine storm_shift_interpolate

    ! ==========================================================================
    !  storm_shift_only()
    !  Determines intermediate storm values
    !   for time t between t_prev and t_next
    !   by shifting storm data towards next position
    !  By not taking averages, this preserves large values
    ! ==========================================================================
    subroutine storm_shift_only(storm)

        implicit none

        ! Storm description, need "in out" here since will update the storm
        ! values at time t
        type(data_storm_type), intent(in out) :: storm

        ! Local storage
        real(kind=8) :: alpha
        integer :: i,j
        integer :: pi,pj
        integer :: prev_shift(2)

        ! Determine the linear interpolation parameter (in time)
        alpha = (storm%t-storm%t_prev) / (storm%t_next-storm%t_prev)

        ! Estimate relative location of storm center at time t
        ! Note: The spatial grid is constant in time
        !  so we don't translate to lat-long
        prev_shift = NINT((storm%eye_next - storm%eye_prev) * alpha)

        ! Now shift the earlier storm field
        !  onto the intermediate storm center
        do j = 1,storm%num_lats
            ! If index would be out of bounds, use edge value
            pj = MIN(MAX(1,j-prev_shift(2)),storm%num_lats)
            do i = 1,storm%num_lons
                ! If index would be out of bounds, use edge value
                pi = MIN(MAX(1,i-prev_shift(1)),storm%num_lons)
                ! Perform shift
                storm%u(i,j) = storm%u_prev(pi,pj)
                storm%v(i,j) = storm%v_prev(pi,pj)
                storm%p(i,j) = storm%p_prev(pi,pj)
            enddo
        enddo
                
    end subroutine storm_shift_only

end module data_storm_module






