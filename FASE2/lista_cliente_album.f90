module lista_cliente_album
    use lista_album
    implicit none
    private

    type, public ::node_album_lista
        private
        character(:), allocatable :: cliente
        type(List_of_list) :: album_lista
        type(node_album_lista), pointer :: next     
    end type node_album_lista

    type, public :: lista_album_img
        private
        type(node_album_lista), pointer :: head => null()
    contains
        procedure :: push
        procedure :: print
        procedure :: delete
        procedure :: search
    end type lista_album_img

contains

subroutine push(this, cliente, listalbum)
    class(lista_album_img), intent(inout) :: this
    character(len=*), intent(in) :: cliente
    type(List_of_list), intent(in) :: listalbum
    type(node_album_lista), pointer :: temp
    allocate(temp)
    temp%cliente = cliente
    temp%album_lista = listalbum
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
        class(lista_album_img), intent(inout) :: this
        character(len=*), intent(in) :: cliente
        type(node_album_lista), pointer :: current, previous

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

    subroutine search(this, cliente, listalbum)
        class(lista_album_img), intent(in) :: this
        character(len=*), intent(in) :: cliente
        type(List_of_list), intent(inout) :: listalbum
    
        type(node_album_lista), pointer :: current
    
        current => this%head
    
        do while (associated(current))
            if (current%cliente == cliente) then
                listalbum = current%album_lista ! asignar
                exit
            end if
            current => current%next
        end do
    
    end subroutine search
    
    

    subroutine print(this)
        class(lista_album_img), intent(in) :: this
        type(node_album_lista), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%cliente
            current => current%next
        end do 
    end subroutine print
    
end module lista_cliente_album