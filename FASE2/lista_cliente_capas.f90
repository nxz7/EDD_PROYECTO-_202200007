module lista_cliente_capas
    use bstdef
    implicit none
    private

    type, public ::node_capa
        private
        character(:), allocatable :: cliente
        type(bst) :: binario_capas
        type(node_capa), pointer :: next     
    end type node_capa

    type, public :: lista_capas
        private
        type(node_capa), pointer :: head => null()
    contains
        procedure :: push
        procedure :: print
        procedure :: delete
        procedure :: search
    end type lista_capas

contains

subroutine push(this, cliente, bi_tree)
    class(lista_capas), intent(inout) :: this
    character(len=*), intent(in) :: cliente
    type(bst), intent(in) :: bi_tree

    type(node_capa), pointer :: temp
    allocate(temp)
    temp%cliente = cliente
    temp%binario_capas = bi_tree
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
        class(lista_capas), intent(inout) :: this
        character(len=*), intent(in) :: cliente
        type(node_capa), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%cliente /= cliente)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
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

    subroutine search(this, cliente, bi_tree)
        class(lista_capas), intent(in) :: this
        character(len=*), intent(in) :: cliente
        type(bst), intent(inout) :: bi_tree
    
        type(node_capa), pointer :: current
    
        current => this%head
    
        do while (associated(current))
            if (current%cliente == cliente) then
                bi_tree = current%binario_capas ! asignar
                exit
            end if
            current => current%next
        end do
    
    end subroutine search
    
    

    subroutine print(this)
        class(lista_capas), intent(in) :: this
        type(node_capa), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%cliente
            current => current%next
        end do 
    end subroutine print
    
end module lista_cliente_capas