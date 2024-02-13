module cola_recepcion
    implicit none
    private

    ! NODO
    type, public :: node
        private
        integer :: id, img_p, img_g
        character(:), allocatable :: value
        type(node), pointer :: next
        type(node), pointer :: prev
    end type node

    ! LISTA
    type, public :: linked_list
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null() 
    contains
        procedure :: enqueue    
        procedure :: dequeue    
        procedure :: print
        procedure :: delete
        procedure :: get_top_info 
    end type linked_list

contains

    subroutine enqueue(self, id, img_g, img_p, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: id, img_g, img_p
        character(len=*), intent(in) :: value
        type(node), pointer :: temp

        allocate(temp)
        temp%id = id
        temp%img_g = img_g
        temp%img_p = img_p
        temp%value = value
        temp%next => null()
        temp%prev => self%tail ! New element points to current tail

        if (.not. associated(self%head)) then
            self%head => temp
        else
            self%tail%next => temp ! Previous tail points to new tail
        end if

        self%tail => temp ! Update tail to point to the new tail node

        print *, "Se ha insertado correctamente el valor: ", value
    end subroutine enqueue

    subroutine dequeue(self)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: removed_node

        if (.not. associated(self%head)) then
            print *, "La cola está vacía."
            return
        end if

        removed_node => self%head

        if (associated(removed_node%next)) then
            self%head => removed_node%next
            removed_node%next%prev => null()
        else
            self%head => null()
            self%tail => null() ! Update tail since the queue becomes empty
        end if

        deallocate(removed_node)
        print *, "Se ha eliminado correctamente el primer valor de la cola."
    end subroutine dequeue

    subroutine print(self)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La cola está vacía."
            return
        end if

        current => self%head

        print *, "Elementos de la cola:"
        do while (associated(current))
            print *, 'ID:', current%id, ', img_g:', current%img_g, ', img_p:', current%img_p, ', value:', current%value
            current => current%next
        end do
    end subroutine print

    subroutine delete(self, value)
        class(linked_list), intent(inout) :: self
        character(len=*), intent(in) :: value
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La cola está vacía."
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
            print *, "Se ha eliminado correctamente el valor: ", value
        else
            print *, "El valor no se encuentra en la cola."
        end if

    end subroutine delete

    subroutine get_top_info(self, info, info_value)
        class(linked_list), intent(in) :: self
        character(len=*), intent(in) :: info
        integer, intent(out) :: info_value
        
        type(node), pointer :: top_node
    
        if (.not. associated(self%head)) then
            print *, "La cola está vacía."
            return
        end if
    
        top_node => self%head
    
        select case (trim(adjustl(info)))
        case ("id")
            info_value = top_node%id
        case ("img_g")
            info_value = top_node%img_g
        case ("img_p")
            info_value = top_node%img_p
        case default
            print *, "La información solicitada no es válida."
        end select
    end subroutine get_top_info
    

end module cola_recepcion
