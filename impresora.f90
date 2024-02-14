!cola de impresion
module impresora
    implicit none
    private

    !NODO
    type, public :: node_p
        private
        character(len=:), allocatable :: tipo, cliente
        integer :: cant
        type(node_p), pointer :: next
        type(node_p), pointer :: prev
    end type node_p

    !LISTA
    type, public :: impresora_list
    private
    type(node_p), pointer :: head => null()
    integer :: count = 0  ! NUMERO DE NODOS ---> POR SI SIRVE MAS ADELANTE
contains
    procedure :: enqueue
    procedure :: print
    procedure :: dequeue
    procedure :: update_count_and_print
    procedure :: get_count
    procedure :: get_top_info_impresora
end type impresora_list

contains

subroutine enqueue(self, cant, tipo, cliente)
    class(impresora_list), intent(inout) :: self
    integer, intent(in) :: cant
    character(len=*), intent(in) :: tipo, cliente
    type(node_p), pointer :: current       
    type(node_p), pointer :: temp

    allocate(temp)
    temp%tipo = tipo
    temp%cliente = cliente
    temp%cant = cant
    temp%next => null()
    temp%prev => self%head

    if (.not. associated(self%head)) then
        self%head => temp
    else         
        current => self%head
        do while (associated(current%next))
            current => current%next
        end do
        current%next => temp
        temp%prev => current
    end if

    self%count = self%count + 1  ! Increment the node count
    call self%update_count_and_print()  ! Update and print the count
    print *, "Se ha insertado correctamente el valor: ", tipo//", " // cliente
end subroutine enqueue

subroutine print(self)
    class(impresora_list), intent(inout) :: self
    type(node_p), pointer :: current

    if (.not. associated(self%head)) then
        print *, "La lista está vacía."
        return
    end if

    current => self%head

    do while (associated(current))
        print *, "Tipo: ", current%tipo, ", Cliente: ", current%cliente, ", Cant: ", current%cant

        current => current%next
    end do
end subroutine print

subroutine dequeue(self)
    class(impresora_list), intent(inout) :: self
    type(node_p), pointer :: current

    if(.not. associated(self%head)) then
        print *, "La lista está vacía."
        return
    end if

    current => self%head

    self%head => current%next
    
    ! PARA QUE NO SE ELIMINE EL ULTIMO SI NO SI NO EL PRIMERO -- COLA!
    if (associated(current%next)) then
        current%next%prev => null()
    end if

    deallocate(current)
    self%count = self%count - 1  
    call self%update_count_and_print()  
end subroutine dequeue

subroutine update_count_and_print(self)
    class(impresora_list), intent(inout) :: self
    print *, "Número de nodos en la lista:", self%count
end subroutine update_count_and_print

subroutine get_count(self, list_count)
    class(impresora_list), intent(in) :: self
    integer, intent(out) :: list_count
    type(node_p), pointer :: current

    list_count = 0  ! Initialize count

    if (.not. associated(self%head)) return  ! SI ESTA VACIA LA LISTA

    current => self%head
    list_count = 1  ! CUENTA DESDE 1

    ! Traverse the list to count nodes
    do while (associated(current%next))
        list_count = list_count + 1
        current => current%next
    end do
end subroutine get_count

subroutine get_top_info_impresora(self, top_tipo, top_cliente, top_cant)
    class(impresora_list), intent(in) :: self
    character(len=:), allocatable, intent(out) :: top_tipo, top_cliente
    integer, intent(out) :: top_cant

    type(node_p), pointer :: top_node

    if (.not. associated(self%head)) then
        print *, "ESTA VACIA LA LISTA DE IMPRESIONES"
        return
    end if

    top_node => self%head
    top_tipo = top_node%tipo
    top_cliente = top_node%cliente
    top_cant = top_node%cant
end subroutine get_top_info_impresora

end module impresora
