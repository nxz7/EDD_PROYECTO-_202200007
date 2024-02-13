module ventanillas
    implicit none
    private

    !NODO
    type, public :: node
        private
        character(:), allocatable :: value
        integer :: id
        integer :: img_p
        integer :: img_g
        type(node), pointer :: next
        type(node), pointer :: prev
    end type node

    !LISTA
    type, public :: simple_linked_list
    private
    type(node), pointer :: head => null()
    integer :: count = 0  ! Counter for the number of nodes
contains
    procedure :: append
    procedure :: print
    procedure :: delete
    procedure :: update_count_and_print
    procedure :: get_count
end type simple_linked_list

contains

subroutine append(self, value, id, img_p, img_g)
    class(simple_linked_list), intent(inout) :: self
    character(len=*), intent(in) :: value
    integer, intent(in) :: id, img_p, img_g
    type(node), pointer :: current       
    type(node), pointer :: temp

    allocate(temp)
    temp%value = value
    temp%id = id
    temp%img_p = img_p
    temp%img_g = img_g
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
    print *, "Se ha insertado correctamente el valor: ", value
end subroutine append

subroutine print(self)
    class(simple_linked_list), intent(inout) :: self
    type(node), pointer :: current

    if (.not. associated(self%head)) then
        print *, "La lista está vacía."
        return
    end if

    current => self%head

    do while (associated(current))
        print *, "Value: ", current%value, " ID: ", current%id, &
        " Img_p: ", current%img_p, " Img_g: ", current%img_g

        current => current%next
    end do
end subroutine

subroutine delete(self, value)
    class(simple_linked_list), intent(inout) :: self
    character(len=*), intent(in) :: value
    type(node), pointer :: current

    if(.not. associated(self%head)) then
        print *, "La lista está vacía."
        return
    end if

    current => self%head

    do while (associated(current) .and. current%value /= value)
        current => current%next
    end do

    if (associated(current) .and. current%value == value) then
        if (associated(current%prev)) then
            ! El nodo a eliminar no es el primero
            current%prev%next => current%next
        else
            ! El nodo a eliminar es el primero
            self%head => current%next
        end if
        
        ! Si el nodo a eliminar no es el último
        if (associated(current%next)) then
            current%next%prev => current%prev
        end if

        deallocate(current)
        self%count = self%count - 1  ! Decrement the node count
        call self%update_count_and_print()  ! Update and print the count
        print *, "Se ha eliminado correctamente el valor: ", value
    else
        print *, "El valor no se encuentra en la lista."
    end if
end subroutine

subroutine update_count_and_print(self)
    class(simple_linked_list), intent(inout) :: self
    print *, "Número de nodos en la lista:", self%count
end subroutine

subroutine get_count(self, list_count)
    class(simple_linked_list), intent(in) :: self
    integer, intent(out) :: list_count
    type(node), pointer :: current

    list_count = 0  ! Initialize count

    if (.not. associated(self%head)) return  ! If the list is empty, return 0

    current => self%head
    list_count = 1  ! Start counting from 1 as the head node exists

    ! Traverse the list to count nodes
    do while (associated(current%next))
        list_count = list_count + 1
        current => current%next
    end do
end subroutine get_count


end module ventanillas