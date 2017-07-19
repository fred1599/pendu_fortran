program pendu

    ! Initialize variables
    implicit none
    character(len=32) :: secret
    character, dimension(:), allocatable :: word
    character :: let
    logical :: trouve
    integer :: i, msec, n, time_info(8)
    integer :: lines

    ! Initialize seed
    call date_and_time(values=time_info)
    msec = 1000 * time_info(7) + time_info(8)
    call random_seed(size=n)
    call random_seed(put=(/ ( i * msec, i = 1, n ) /))

    ! Get random number / line search in file
    lines = get_lines("dico.txt")
    n = my_rand(1, lines)
    call search_line("dico.txt", n, secret)

    ! Get word to completed
    allocate(word(len_trim(secret)))
    word = '*'

    trouve = .false.
    do while (.not. trouve)
        print*, word
        print*, "proposer une lettre: "
        read (*, '(a)') let
        call check(secret, word, let)
        trouve = all(word/='*')
    end do

    print*, word

    deallocate(word)

contains
    subroutine check(s, w, letter)
        character, dimension(:), intent(out) :: w
        character(len=*), intent(in) :: s
        character, intent(out) :: letter
        integer :: i, j

        call lower(letter)

        do i=1, len_trim(s)
            if (s(i:i) == letter) then
                w(i) = letter
            end if
        end do
    end subroutine check

    subroutine lower(l)
        character, intent(inout) :: l
        if (ichar(l) >= 65 .and. ichar(l) < 90) then
            l = char(ichar(l)+32)
        end if
    end subroutine lower

    integer function my_rand(mini, maxi)
        integer, intent(in) :: mini, maxi
        real :: r
        call random_number(r)
        my_rand = int(r*maxi+mini)
    end function

    integer function get_lines(s)
        character(len=*), intent(in) :: s
        integer :: n, io

        open(11,file=s, iostat=io, status='old')
        if (io/=0) stop 'Cannot open file!'

        do
            read(11,*,iostat=io)
            if (io/=0) exit
            n = n + 1
        end do

        close(11)

        get_lines = n

    end function get_lines

    subroutine search_line(s, n, l)
        character(len=*), intent(in) :: s
        character(len=*), intent(out) :: l
        integer, intent(in) :: n
        integer :: i, io

        open(11,file=s, iostat=io, status='old')
        if (io/=0) stop 'Cannot open file!'

        do i=1, n
            read(11,'(A)',iostat=io) l
            if (io/=0) exit
        end do

        close(11)

    end subroutine search_line

end program pendu
