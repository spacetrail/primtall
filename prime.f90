program primtall_kalkulator
    use omp_lib !OpenMP bibliotek
    implicit none
    integer :: sum_up, sum_down, i
    integer :: antall_elementer, verdi
    integer :: antall_primtall
    real :: rest 
    logical :: primtall = .true.
    integer, allocatable :: array_prim(:)
    allocate(array_prim(0));
    
    print *, "Hvor mange primtall vil du ha: "
    read *, antall_primtall ! Lese antall primtall

    sum_up = 2 ! Starter på 2 og jobber oppover.
    do
        sum_down = sum_up
        primtall = .true.
        do
            ! Vi kom oss ned til 1, kan ikke dele mer.
            if(sum_down < 2) then
                ! Fant et primtall
                if(primtall) then
                    antall_elementer = antall_elementer + 1
                    print *, "Fant primtall: ", sum_up
                    call legg_til_array(array_prim, sum_up)
                    exit
                end if
            end if
            ! Hopp over å dele med seg selv
            if(sum_up <= sum_down) then
                sum_down = sum_down - 1
                cycle
            end if
            rest = mod(sum_up, sum_down) ! Finne rest
            ! Betyr det er delelig med et tall, dermed ikke primtall.
            if(rest == 0.0) then
                primtall = .false.
                exit
            ! vet ikke, funker uten
            !else 
            !    primtall = .true.
            end if
            sum_down = sum_down - 1 ! Tell nedover, 10/10, 10/9, 10/8 osv.
        end do
        if(size(array_prim) >= antall_primtall) then 
            print *, "Traff exit,", size(array_prim), "Antall prim: ", antall_primtall
            exit
        else 
            sum_up = sum_up + 1 ! Tell oppover og finn neste primtall.
        end if
    end do
! Ferdig å kalkulere primtall.
print *, "Ferdig"

!print *, "Antall primtall funnet: ", size(array_prim) 
do i = 1, size(array_prim)
    print *, "Primtall: ", i, ":", array_prim(i)
end do

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
