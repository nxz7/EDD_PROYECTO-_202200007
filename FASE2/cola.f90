module cola_module
    !use bstdef
    implicit none
    private

    type, public :: cola_node
        private
        integer :: value
        type(cola_node), pointer :: next     
    end type cola_node

    type, public :: cola
        private
        type(cola_node), pointer :: head => null()
        type(cola_node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: cola_clear
    end type cola

contains
    subroutine append(this, value)
        class(cola), intent(inout) :: this
        integer, intent(in) :: value

        type(cola_node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

        !print *, 'Append ', value
    end subroutine append

    subroutine delete(this, valor)
        class(cola), intent(inout) :: this
        type(cola_node), pointer :: temp
        integer, intent(out) :: valor

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            return
        end if

        print *, 'Delete ', this%head%value
        valor = this%head%value
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(cola_node), pointer :: current

        current => this%head

        print *, '//-------COLA------//'

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print

    subroutine cola_clear(this)
        class(cola), intent(inout) :: this
        type(cola_node), pointer :: current, next_node
    
        current => this%head
    
        do while (associated(current))
            next_node => current%next
            deallocate(current)
            current => next_node
        end do
    
        this%head => null()
        this%tail => null()
    
        print *, '******************'
    end subroutine cola_clear
    
end module cola_module
