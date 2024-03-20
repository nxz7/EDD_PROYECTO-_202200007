module lista_avl
    implicit none
    private

    type, public ::node_l
        private
        integer :: value
        type(node_l), pointer :: next     
    end type node_l

    type, public :: linked_list
        private
        type(node_l), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: search
        procedure :: clear 
    end type linked_list

contains

    subroutine push(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value

        type(node_l), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        !print *, 'pushed ', value
    end subroutine push

    subroutine append(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value

        type(node_l), pointer :: temp
        type(node_l), pointer :: current

        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if

        print *, 'appended ', value
    end subroutine append

    subroutine delete(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value
        type(node_l), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            print *, 'Se eliminó el valor ', value
        else
            print *, 'No se encontró el valor ', value
        end if

    end subroutine delete

    function search(this, value) result(retval)
        class(linked_list), intent(in) :: this
        integer, intent(in) :: value

        type(node_l), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(linked_list), intent(in) :: this
        type(node_l), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print
    
    subroutine clear(this)
        class(linked_list), intent(inout) :: this
        type(node_l), pointer :: current, next_node
        
        current => this%head

        do while (associated(current))
            next_node => current%next
            deallocate(current)
            current => next_node
        end do
        
        this%head => null()
        print *, 'Lista eliminada'
    end subroutine clear

end module lista_avl