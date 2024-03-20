module lista_cliente_img
    use avldef
    implicit none
    private

    type, public ::node_img
        private
        character(:), allocatable :: cliente
        type(avl) :: avl_img
        type(node_img), pointer :: next     
    end type node_img

    type, public :: lista_img
        private
        type(node_img), pointer :: head => null()
    contains
        procedure :: push
        procedure :: print
        procedure :: delete
        procedure :: search
    end type lista_img

contains

subroutine push(this, cliente, avl_tree)
    class(lista_img), intent(inout) :: this
    character(len=*), intent(in) :: cliente
    type(avl), intent(in) :: avl_tree

    type(node_img), pointer :: temp
    allocate(temp)
    temp%cliente = cliente
    temp%avl_img = avl_tree
    temp%next => null()

    if (.not. associated(this%head)) then
        this%head => temp
    else
        temp%next => this%head
        this%head => temp
    end if

    !print *, 'pushed ', cliente
end subroutine push



    subroutine delete(this, cliente)
        class(lista_img), intent(inout) :: this
        character(len=*), intent(in) :: cliente
        type(node_img), pointer :: current, previous

        current => this%head
        previous => null()

        ! BUSCA NODO
        do while (associated(current) .and. current%cliente /= cliente)
            previous => current
            current => current%next
        end do

        ! SI SE ENCNTRO EL NODO
        if(associated(current) .and. current%cliente == cliente) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            print *, 'Se eliminó el valor ', cliente
        else
            print *, 'No se encontró el valor ', cliente
        end if

    end subroutine delete

    subroutine search(this, cliente, avl_tree)
        class(lista_img), intent(in) :: this
        character(len=*), intent(in) :: cliente
        type(avl), intent(out) :: avl_tree
    
        type(node_img), pointer :: current
    
        current => this%head
    
        do while (associated(current))
            if (current%cliente == cliente) then
                avl_tree = current%avl_img ! DEVOLVER EL ARBOL
                exit
            end if
            current => current%next
        end do
    
    end subroutine search
    
    

    subroutine print(this)
        class(lista_img), intent(in) :: this
        type(node_img), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%cliente
            current => current%next
        end do 
    end subroutine print
    
end module lista_cliente_img