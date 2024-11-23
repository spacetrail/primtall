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
    read *, antall_primtall
    !antall_primtall = 10

    sum_up = 2
    do
        sum_down = sum_up
        !print *, "Første loop"
        primtall = .true.
        do
            !print *, "Andre loop"
            !print *, "sum_up: ", sum_up, "sum_down", sum_down 
            if(sum_down < 2) then
                !print *, "Er det primtall?: ", sum_up, primtall
                if(primtall) then
                    antall_elementer = antall_elementer + 1
                    !verdi = antall_elementer
                    !print *, sum_down
                    !print *, "Legger til sum_down:", sum_up
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
            !else 
            !    primtall = .true.
            end if
            sum_down = sum_down - 1
        end do
        !sum_up = sum_up + 1
        if(size(array_prim) >= antall_primtall) then 
            print *, "Traff exit,", size(array_prim), "Antall prim: ", antall_primtall
            exit
        else 
            sum_up = sum_up + 1
        end if
    end do
print *, "Ferdig"

print *, size(array_prim)


do i = 1, size(array_prim)
    print *, "Primtall: ", i, ":", array_prim(i)
end do

!print *, array_prim(2)

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
