module cliente
    implicit none
    private

    ! CLIENTE
    type, public :: client
        character(:), allocatable ::  name
        integer:: smallImg, bigImgs, steps, totalImgs, totalSmallImgs, totalBigImgs,id
    end type client

end module cliente
