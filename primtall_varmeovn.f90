! ============================================================
!  PRIMTALL-VARMEOVN v2 - statistikk-utgave
!  Kompilering:
!    gfortran -O2 -fopenmp primtall_varmeovn.f90 -o primtall_varmeovn
!
!  Kjoring (typiske eksempler):
!    ./primtall_varmeovn                      ! fortsett fra sjekkpunkt, kun statistikk
!    ./primtall_varmeovn 1000000000           ! start fra gitt tall (ignorerer sjekkpunkt)
!    ./primtall_varmeovn 0 100000 150         ! blokkstorrelse 100 000, anta 150 W CPU
!    ./primtall_varmeovn 0 100000 150 logg    ! old attferd: skriv ogsaa primtall-filer
!
!  Filer som skrives:
!    primtall_sjekkpunkt.txt  - gjenopptakbar fremdrift (neste kandidat, antall, storste)
!    primtall_stats.txt       - LITEN "fancy" statusfil, overskrives hver blokk (~1 KB)
!    primtall_statslogg.txt   - en linje per blokk for senere plotting (~60 B/blokk)
!    primtall_000001.txt ...  - KUN i "logg"-modus (standard av naa!)
! ============================================================

module flagg_mod
  implicit none
  logical :: stopp_flagg = .false.
end module flagg_mod

module verktoy
  implicit none
  integer, parameter :: ik = selected_int_kind(18)   ! 64-bit heltall
contains

  ! Sjekker om n er primtall ved proevedivisjon (2, 3, deretter 6k+-1)
  pure function er_primtall(n) result(p)
    integer(kind=ik), intent(in) :: n
    logical :: p
    integer(kind=ik) :: i
    if (n < 2_ik) then
       p = .false.
    else if (n < 4_ik) then
       p = .true.
    else if (mod(n, 2_ik) == 0_ik .or. mod(n, 3_ik) == 0_ik) then
       p = .false.
    else
       p = .true.
       i = 5_ik
       do while (i * i <= n)
          if (mod(n, i) == 0_ik .or. mod(n, i + 2_ik) == 0_ik) then
             p = .false.
             exit
          end if
          i = i + 6_ik
       end do
    end if
  end function er_primtall

  ! Enkel rekursiv quicksort for blokkens primtall
  recursive subroutine sorter(a, lo, hi)
    integer(kind=ik), intent(inout) :: a(:)
    integer, intent(in) :: lo, hi
    integer :: i, j
    integer(kind=ik) :: pivot, tmp
    if (lo >= hi) return
    pivot = a((lo + hi) / 2)
    i = lo
    j = hi
    do
       do while (a(i) < pivot)
          i = i + 1
       end do
       do while (a(j) > pivot)
          j = j - 1
       end do
       if (i >= j) exit
       tmp = a(i); a(i) = a(j); a(j) = tmp
       i = i + 1
       j = j - 1
    end do
    call sorter(a, lo, j)
    call sorter(a, j + 1, hi)
  end subroutine sorter

end module verktoy

program primtall_varmeovn
  use omp_lib
  use flagg_mod
  use verktoy
  implicit none

  ! ---------- Statusvariabler ----------
  integer(kind=ik) :: neste = 3_ik          ! neste kandidat som skal testes
  integer(kind=ik) :: totalt_antall = 0_ik   ! antall primtall funnet totalt
  integer(kind=ik) :: storste = 2_ik         ! storste primtall funnet
  integer(kind=ik) :: totalt_kandidater = 0_ik
  integer(kind=ik) :: tvilling_par = 0_ik   ! antall tvillingprimtall-par
  integer(kind=ik) :: storste_gap = 0_ik    ! storste hull mellom primtall
  integer(kind=ik) :: forrige_blokk_siste = 0_ik  ! for gap over blokk-grensen

  ! ---------- Konfigurasjon ----------
  integer(kind=ik) :: start_arg = 0_ik
  integer(kind=ik) :: blokk_str = 100000_ik
  real :: antatt_watt = 150.0
  logical :: logg_modus = .false.
  integer(kind=ik) :: per_fil = 1000000_ik

  ! ---------- Arbeidsvariabler ----------
  integer(kind=ik) :: blokk_start, blokk_slutt, k, n
  integer(kind=ik), allocatable :: blokk_primer(:), lokale(:), flettet(:)
  integer(kind=ik) :: antall_blokk, nlok, fpos
  integer :: fil_nr, fil_enhet, io_status
  integer(kind=ik) :: skrevet_fil = 0_ik
  character(len=64) :: filnavn
  character(len=19) :: tidsstempel
  character(len=8)  :: dato
  character(len=10) :: klokke
  integer(kind=ik) :: klokke_start, klokke_na, klokke_rate, klokke_maks
  real(kind=8) :: sek_blokk, sek_total, fart, kwh
  integer :: i, antall_argumenter
  character(len=32) :: arg

  ! ---------- SIGTERM/SIGINT-handler: flagg for pent stopp ----------
  external :: sigterm_handler
  call signal(15, sigterm_handler)   ! SIGTERM (kill/pkill/systemd)
  call signal(2, sigterm_handler)    ! SIGINT  (Ctrl+C i forgrunnen)

  ! ---------- Les kommandolinjeargumenter ----------
  antall_argumenter = command_argument_count()
  if (antall_argumenter >= 1) then
     call get_command_argument(1, arg); read(arg, *) start_arg
  end if
  if (antall_argumenter >= 2) then
     call get_command_argument(2, arg); read(arg, *) blokk_str
  end if
  if (antall_argumenter >= 3) then
     call get_command_argument(3, arg); read(arg, *) antatt_watt
  end if
  if (antall_argumenter >= 4) then
     call get_command_argument(4, arg)
     logg_modus = (trim(arg) == 'logg')
  end if
  if (antall_argumenter >= 5) then
     call get_command_argument(5, arg); read(arg, *) per_fil
  end if

  ! ---------- Les sjekkpunkt (med mindre start er gitt eksplisitt) ----------
  if (start_arg > 0_ik) then
     neste = start_arg
     if (mod(neste, 2_ik) == 0_ik) neste = neste + 1_ik
  else
     open(newunit=fil_enhet, file='primtall_sjekkpunkt.txt', status='old', &
          action='read', iostat=io_status)
     if (io_status == 0) then
        read(fil_enhet, *, iostat=io_status) neste, totalt_antall, storste
        close(fil_enhet)
        if (io_status /= 0) then
           neste = 3_ik; totalt_antall = 0_ik; storste = 2_ik
        end if
     else
        neste = 3_ik; totalt_antall = 0_ik; storste = 2_ik
     end if
  end if

  call system_clock(count_rate=klokke_rate, count_max=klokke_maks)
  call system_clock(count=klokke_start)

  allocate(blokk_primer(blokk_str), flettet(blokk_str))

  if (logg_modus) then
     ! Finn neste ledige filnummer (eksisterende filer overskrives aldri)
     fil_nr = 1
     do
        write(filnavn, '("primtall_", i6.6, ".txt")') fil_nr
        open(newunit=fil_enhet, file=filnavn, status='old', action='read', &
             iostat=io_status)
        if (io_status /= 0) exit
        close(fil_enhet)
        fil_nr = fil_nr + 1
     end do
     skrevet_fil = 0_ik
  end if

  write(*, '(A)') '=== PRIMTALL-VARMEOVN v2 starter ==='
  write(*, '(A, I0)') ' Traader i bruk : ', omp_get_max_threads()
  write(*, '(A, I0)') ' Fortsetter fra : ', neste
  write(*, '(A, I0)') ' Blokkstorrelse : ', blokk_str
  if (logg_modus) then
     write(*, '(A)') ' Modus          : statistikk + primtall-filer'
  else
     write(*, '(A)') ' Modus          : kun statistikk (fancy, men liten!)'
  end if

  ! ================= HOVEDLOKKE =================
  hovedlokke: do while (.not. stopp_flagg)

     blokk_start = neste
     blokk_slutt = neste + blokk_str - 1_ik
     antall_blokk = 0_ik

     call system_clock(count=klokke_na)
     sek_total = real(klokke_na - klokke_start, kind=8) / real(klokke_rate, kind=8)

     ! ---- Parallell soek over blokken ----
     !$omp parallel private(lokale, nlok, k, n)
     allocate(lokale(blokk_str / 8_ik + 16_ik))
     nlok = 0_ik
     !$omp do schedule(dynamic, 512)
     do k = blokk_start, blokk_slutt, 2_ik        ! kun oddetall
        if (er_primtall(k)) then
           nlok = nlok + 1_ik
           lokale(nlok) = k
        end if
     end do
     !$omp end do
     !$omp critical
     blokk_primer(antall_blokk + 1_ik : antall_blokk + nlok) = &
          lokale(1_ik : nlok)
     antall_blokk = antall_blokk + nlok
     !$omp end critical
     deallocate(lokale)
     !$omp end parallel

     if (antall_blokk > 0_ik) then
        call sorter(blokk_primer, 1, int(antall_blokk))

        ! Tvillingpar og storste gap (ogsaa over blokk-grensen)
        do i = 1, int(antall_blokk)
           if (i > 1) then
              if (blokk_primer(i) - blokk_primer(i - 1) == 2_ik) &
                   tvilling_par = tvilling_par + 1_ik
              if (blokk_primer(i) - blokk_primer(i - 1) > storste_gap) &
                   storste_gap = blokk_primer(i) - blokk_primer(i - 1)
           else if (forrige_blokk_siste > 0_ik) then
              if (blokk_primer(1) - forrige_blokk_siste == 2_ik) &
                   tvilling_par = tvilling_par + 1_ik
              if (blokk_primer(1) - forrige_blokk_siste > storste_gap) &
                   storste_gap = blokk_primer(1) - forrige_blokk_siste
           end if
        end do
        forrige_blokk_siste = blokk_primer(int(antall_blokk))

        totalt_antall = totalt_antall + antall_blokk
        storste = max(storste, blokk_primer(int(antall_blokk)))

        ! ---- Logg primtall til fil (kun i logg-modus) ----
        if (logg_modus) then
           write(filnavn, '("primtall_", i6.6, ".txt")') fil_nr
           open(newunit=fil_enhet, file=filnavn, position='append', &
                action='write')
           do i = 1, int(antall_blokk)
              write(fil_enhet, '(I0)') blokk_primer(i)
              skrevet_fil = skrevet_fil + 1_ik
              if (skrevet_fil >= per_fil .and. i < int(antall_blokk)) then
                 close(fil_enhet)
                 fil_nr = fil_nr + 1
                 skrevet_fil = 0_ik
                 write(filnavn, '("primtall_", i6.6, ".txt")') fil_nr
                 open(newunit=fil_enhet, file=filnavn, position='append', &
                      action='write')
              end if
           end do
           close(fil_enhet)
        end if
     end if

     totalt_kandidater = totalt_kandidater + (blokk_slutt - blokk_start + 2_ik) / 2_ik
     neste = blokk_slutt + 2_ik

     ! ---- Sjekkpunkt (flush umiddelbart) ----
     open(newunit=fil_enhet, file='primtall_sjekkpunkt.txt', action='write')
     write(fil_enhet, '(I0, 1X, I0, 1X, I0)') neste, totalt_antall, storste
     close(fil_enhet)

     ! ---- Fancy, men liten statistikkfil ----
     call date_and_time(date=dato, time=klokke)
     tidsstempel = dato(1:4) // '-' // dato(5:6) // '-' // dato(7:8) // ' ' // &
                   klokke(1:2) // ':' // klokke(3:4) // ':' // klokke(5:6)
     call system_clock(count=klokke_na)
     sek_total = real(klokke_na - klokke_start, kind=8) / real(klokke_rate, kind=8)
     fart = real(totalt_kandidater, kind=8) / max(sek_total, 1.0d-9)
     kwh = sek_total * antatt_watt / 3600000.0d0

     open(newunit=fil_enhet, file='primtall_stats.txt', action='write')
     write(fil_enhet, '(A)') '+=============================================+'
     write(fil_enhet, '(A)') '|        PRIMTALL-VARMEOVN  -  STATUS        |'
     write(fil_enhet, '(A)') '+=============================================+'
     write(fil_enhet, '(A, A19, A)')    ' | Oppdatert          : ', tidsstempel, ' |'
     write(fil_enhet, '(A, I0)')   ' | Neste kandidat     : ', neste
     write(fil_enhet, '(A, I0)')   ' | Kandidater testet  : ', totalt_kandidater
     write(fil_enhet, '(A, I0)')   ' | Primtall funnet    : ', totalt_antall
     write(fil_enhet, '(A, I0)')   ' | Storst primtall    : ', storste
     write(fil_enhet, '(A, I0)')   ' | Tvillingpar        : ', tvilling_par
     write(fil_enhet, '(A, I0)')   ' | Storste gap        : ', storste_gap
     write(fil_enhet, '(A, F0.1)') ' | Fart (kand./sek)   : ', fart
     write(fil_enhet, '(A, F0.2)') ' | Kjooretid (t)      : ', sek_total
     write(fil_enhet, '(A, F0.3)') ' | Est. energi (kWh)  : ', kwh
     write(fil_enhet, '(A, F0.1)') ' | Est. varme (W)      : ', antatt_watt
     write(fil_enhet, '(A)') ' | Varmeovn-status     : CPU-en jobber videre!  |'
     write(fil_enhet, '(A)') '+---------------------------------------------+'
     close(fil_enhet)

     ! ---- Elinjes historikk for plotting (ca. 60 byte per blokk) ----
     open(newunit=fil_enhet, file='primtall_statslogg.txt', position='append', &
          action='write')
     write(fil_enhet, '(A, ",", I0, ",", I0, ",", I0, ",", I0, ",", F0.1, ",", F0.3)') &
          tidsstempel, totalt_kandidater, totalt_antall, storste, tvilling_par, fart, kwh
     close(fil_enhet)

     write(*, '(A, I0, A, I0, A)') ' Blokk ferdig: ', totalt_antall, &
          ' primtall totalt, storste so langt ', storste, '.'

  end do hovedlokke

  ! ---------- Pent stopp: skriv siste sjekkpunkt ----------
  open(newunit=fil_enhet, file='primtall_sjekkpunkt.txt', action='write')
  write(fil_enhet, '(I0, 1X, I0, 1X, I0)') neste, totalt_antall, storste
  close(fil_enhet)

  write(*, '(A)') '=== Stoppet pent (SIGTERM mottatt). Sjekkpunkt lagret. ==='

end program primtall_varmeovn

! SIGTERM-handler: bare flagg, ingen I/O i signal-kontekst
subroutine sigterm_handler()
  use flagg_mod
  implicit none
  stopp_flagg = .true.
end subroutine sigterm_handler
