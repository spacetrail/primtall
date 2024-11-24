program primtall_kalkulator
    use omp_lib !OpenMP bibliotek
    implicit none
    !logical :: stop = .false.
    !real :: rest 
    !integer :: verdi
    integer :: sum_up, sum_down, i
    integer :: antall_primtall
    integer, allocatable :: array_prim(:)
    logical :: primtall = .true.
    real :: start_tid, stopp_tid, brukt_tid
    
    allocate(array_prim(0));

    ! Logisk feil en plass
    ! Den rapporterte 4 som primtall
    ! så på en måte må primtall flagget være gyldig under bestemte
    ! omstendigheter. Skal se på det i morgen.
    ! Samtidig, den skal finne x antall primtall, ikke primtall
    ! mellom 2 og antall_primtall

    print *, "Hvor mange primtall vil du ha: "
    read *, antall_primtall ! Lese antall primtall
    !antall_primtall = 1000

    ! Ta tiden
    call CPU_TIME(start_tid)

    !$OMP PARALLEL DO PRIVATE(sum_down, sum_up, primtall) SHARED(antall_primtall)
    !sum_up = 2 ! Starter på 2 og jobber oppover.
    do sum_up = 2, antall_primtall
        sum_down = sum_up
        !primtall = .true.
        !print *, "sum_up: ", sum_up
        do while (sum_down > 1)
            if (sum_up /= sum_down .and. mod(sum_up, sum_down) == 0) then
                !print *, "Tråd", omp_get_thread_num(), " fant ut at", sum_up, " ", sum_down, "ikke er et primtall"
                !$OMP CRITICAL
                primtall = .false.
                !$OMP END CRITICAL
                exit
            else
                !print *, "Tråd", omp_get_thread_num(), " fant ut at", sum_up, " ", sum_down, " er et primtall"
                !$OMP CRITICAL
                primtall = .true.
                !$OMP END CRITICAL
            end if
            !$OMP ATOMIC
            sum_down = sum_down - 1
            !$OMP END ATOMIC
            !print *, "Tråd", omp_get_thread_num(), "jobber med sum_down=", sum_down
        end do
        if(primtall) then 
           !$OMP CRITICAL
            print *, "primtall treff:", sum_up
            call legg_til_array(array_prim, sum_up)
            !$OMP END CRITICAL
            !if(size(array_prim) >= antall_primtall) then 
            !    print *, "Traff exit,", size(array_prim), "Antall prim: ", antall_primtall
            !    call legg_til_array(array_prim, sum_up)
            !    stop = .true.
                !exit
            !else 
            !    sum_up = sum_up + 1 ! Tell oppover og finn neste primtall.
            !end if
            
        end if
    end do
! Ferdig å kalkulere primtall.
!$OMP END PARALLEL DO

call CPU_TIME(stopp_tid)
brukt_tid = stopp_tid - start_tid
print '("tid : ",f6.3," sekunder.")',stopp_tid-start_tid

call sort_array(array_prim, size(array_prim))

!print *, "Antall primtall funnet: ", size(array_prim) 
do i = 1, size(array_prim)
    print *, "Primtall: ", i, ":", array_prim(i)
end do

print *, "Antall primtall mellom 0-", antall_primtall, "er: ", size(array_prim) 

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
