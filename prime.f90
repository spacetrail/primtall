program primtall_kalkulator
    use omp_lib !OpenMP bibliotek
    implicit none
    integer :: sum_up, sum_down, i
    integer :: antall_elementer, verdi
    integer :: antall_primtall
    real :: rest 
    logical :: primtall = .true.
    logical :: stop = .false.
    integer, allocatable :: array_prim(:)
    allocate(array_prim(0));
    
    print *, "Hvor mange primtall vil du ha: "
    !read *, antall_primtall ! Lese antall primtall
    antall_primtall = 1000
    !$OMP PARALLEL DO PRIVATE(sum_down, sum_up) SHARED(array_prim)
    !sum_up = 2 ! Starter på 2 og jobber oppover.
    do sum_up = 2, antall_primtall
        sum_down = sum_up
        !primtall = .true.
        print *, "sum_up: ", sum_up
        do while (sum_down > 1)
            if (sum_up /= sum_down .and. mod(sum_up, sum_down) == 0) then
                print *, "Tråd", omp_get_thread_num(), " fant ut at", sum_up, " ", sum_down, "ikke er et primtall"
                primtall = .false.
                exit
            else
                print *, "Tråd", omp_get_thread_num(), " fant ut at", sum_up, " ", sum_down, " er et primtall"
                primtall = .true.
            end if
            sum_down = sum_down - 1
            !print *, "Tråd", omp_get_thread_num(), "jobber med sum_down=", sum_down
        end do
        if(primtall) then 
           !$OMP CRITICAL
            print *, "Kommer jeg noen gang inn på denne?"
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

!print *, "Antall primtall funnet: ", size(array_prim) 
do i = 1, size(array_prim)
    print *, "Primtall: ", i, ":", array_prim(i)
end do

print *, "Antall primtall mellom 0-", antall_primtall, "er: ", size(array_prim) 

contains

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
