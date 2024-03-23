module user
    implicit none
    private

    type, public :: user_type
        integer :: dpi
        character(:), allocatable ::  name, password
    end type user_type

end module user
