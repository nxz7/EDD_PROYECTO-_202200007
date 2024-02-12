module cola_recepcion
    implicit none

    type :: cliente
        integer :: id, img_g, img_p
        character(len=:), allocatable :: nombre
        type(cliente), pointer :: sig
        type(cliente), pointer :: prev
    end type cliente

    type :: cola_doble
        type(cliente), pointer :: head => null()
        type(cliente), pointer :: tail => null()
    contains
        procedure :: add_cliente
        procedure :: show_clientes
        procedure :: clear
        procedure :: is_empty
        procedure :: remove_cliente
    end type cola_doble

contains

    subroutine add_cliente(this, id, img_g, img_p, nombre)
        class(cola_doble), intent(inout) :: this
        integer, intent(in) :: id, img_g, img_p
        character(len=:), allocatable, intent(in) :: nombre
        type(cliente), pointer :: actual
        allocate(actual)
        actual%id = id
        actual%img_g = img_g
        actual%img_p = img_p
        allocate(actual%nombre, source=nombre)
        actual%sig => null()
        actual%prev => this%tail

        if (.not. associated(this%head)) then
            this%head => actual
        end if

        if (associated(this%tail)) then
            this%tail%sig => actual
        end if

        this%tail => actual
    end subroutine add_cliente

    subroutine show_clientes(this)
        class(cola_doble), intent(inout) :: this
        type(cliente), pointer :: actual
        actual => this%head
        do
            if (.not. associated(actual)) then
                exit
            end if
            print *, 'ID:', actual%id, ', img_g:', actual%img_g, ', img_p:', actual%img_p, ', nombre:', trim(actual%nombre)
            actual => actual%sig
        end do
    end subroutine show_clientes

    subroutine clear(this)
        class(cola_doble), intent(inout) :: this
        type(cliente), pointer :: current_node, next_node
        current_node => this%head
        do
            if (.not. associated(current_node)) then
                exit
            end if
            next_node => current_node%sig
            deallocate(current_node%nombre)
            deallocate(current_node)
            current_node => next_node
        end do
        this%head => null()
        this%tail => null()
    end subroutine clear

    logical function is_empty(this)
        class(cola_doble), intent(in) :: this
        is_empty = .not. associated(this%head)
    end function is_empty

    subroutine remove_cliente(this)
        class(cola_doble), intent(inout) :: this
        type(cliente), pointer :: removed_node
        if (associated(this%head)) then
            removed_node => this%head
            this%head => removed_node%sig
            if (associated(this%head)) then
                this%head%prev => null()
            else
                this%tail => null()
            end if
            deallocate(removed_node%nombre)
            deallocate(removed_node)
        endif
    end subroutine remove_cliente

end module cola_recepcion
