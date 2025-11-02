program primtall_kalkulator
    implicit none
    integer :: sum_up, sum_down, i ! Intern logikk for løkkene
    integer :: antall_primtall ! Hvor mange primtall skal letes etter 
    integer, allocatable :: array_prim(:) ! Array med alle primtall
    logical :: primtall = .true. ! Flag om primtall funnet - OBS, det kan være feil logikk av denne
    real(8) :: start_tid, stopp_tid ! Klokke
    logical :: run = .true.

    allocate(array_prim(0)); ! Initialisering av slutt array
    
    ! Den skal finne x antall primtall, ikke primtall
    ! mellom 2 og antall_primtall. Problem med multicore direktiv.

    write(*, '(A)', advance='no') "Hvor mange primtall vil du ha: "
    read *, antall_primtall ! Lese antall primtall
    ! antall_primtall = 1000000 ! debug
    call legg_til_array(array_prim, 2)
    ! Ta tiden
    call CPU_TIME(start_tid)
    
    sum_up = 3
    do while (run)
        sum_down = sum_up
        primtall = .true.
        ! Forsøk på å få den til å bruke flere kjerner, ser ut til å feile.
        ! Men den fungerer bedre enn første forsøk ettersom den finner antall prim, ikke mellom 3 og x.
        if(mod(sum_down, 2) == 0 .and. sum_down > 2) primtall = .false.
        if(primtall) then 
            multi: do i = 3, int(sqrt(real(sum_down)))
                ! Hvis tallet kan deles og får et integer tilbake, er det ikke prim.
                    if (mod(sum_up, i) == 0) then
                        primtall = .false.
                        exit
                    end if
                end do multi
            end if
                if(primtall) then 
                    !print *, "primtall treff:", sum_up
                    call legg_til_array(array_prim, sum_up)
                    
                    if(size(array_prim) >= antall_primtall) then 
                        run = .false.
                    end if
                end if
                sum_up = sum_up + 2 ! Tell oppover og finn neste primtall.
            end do
        
! Ferdig å kalkulere primtall.

call CPU_TIME(stopp_tid)

call sort_array(array_prim, size(array_prim))

!print *, "Antall primtall funnet: ", size(array_prim) 
do i = 1, size(array_prim)
    !print *, "Primtall: ", i, ":", array_prim(i)
    print *, array_prim(i)
end do

print *, "Antall primtall: ", size(array_prim) 
if (stopp_tid-start_tid < 60.0) then
    print '("Kalkuleringstid : ",f15.6," sekunder.")',stopp_tid-start_tid
else if (stopp_tid-start_tid < 3600.0) then
    print '("Kalkuleringstid : ",f15.6," minutter.")',(stopp_tid-start_tid)/60.0
else
    print '("Kalkuleringstid : ",f15.6," timer.")',(stopp_tid-start_tid)/3600.0
end if

contains
! Sortere array fra 0-x
subroutine sort_array(array, size)
    implicit none
    integer, intent(inout) :: array(:)  ! Array som skal sorteres
    integer, intent(in) :: size         ! Størrelsen på arrayet
    integer :: i, j, min_idx, temp

    ! Utvalgssortering (Selection Sort)
    do i = 1, size-1
      min_idx = i
      do j = i+1, size
        if (array(j) < array(min_idx)) then
          min_idx = j
        end if
      end do
      ! Bytt elementene
      temp = array(i)
      array(i) = array(min_idx)
      array(min_idx) = temp
    end do
  end subroutine sort_array

  ! Subrutine for å legge til element i dynamisk array
  subroutine legg_til_array(array, verdi)
    
    integer, allocatable, intent(inout) :: array(:)
    integer, intent(in) :: verdi
    integer, allocatable :: temp(:)
    !print *, "Array: ", array, "Verdi: ", verdi
    allocate(temp(size(array) + 1))
    temp(:size(array)) = array
    temp(size(array) + 1) = verdi
    call move_alloc(temp, array)
  end subroutine legg_til_array

end program primtall_kalkulator
