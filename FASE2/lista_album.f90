module lista_album
    implicit none
    private

    type :: sub_node
        integer ::  index
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node_album
        character(:), allocatable :: value
        type(node_album), pointer :: next => null()
        type(node_album), pointer :: prev => null()
        type(sub_node), pointer :: list => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: print_G
    end type node_album

    type, public :: List_of_list
        type(node_album), pointer :: head => null()
        type(node_album), pointer :: tail => null()
    contains
        procedure :: insert
        procedure :: printList
        procedure :: delete
        procedure :: printIndicesFirst
    end type List_of_list

contains

    subroutine insert(self, value, index)
        class(List_of_list), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer, intent(in):: index

        type(node_album), pointer :: aux
        type(node_album), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            allocate(aux)
            aux%value = value
            self%head => aux
            self%tail => aux
            call aux%append(index)
        else
            if(value < self%head%value) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%value = value
                call new%append(index)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(value < aux%next%value) then
                        if(value == aux%value) then
                            call aux%append(index)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%value = value
                            call new%append(index)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(value == aux%value) then
                    call aux%append(index)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%value = value
                    call new%append(index)
                end if
            end if
        end if
    end subroutine insert




    subroutine printList(self)
        class(List_of_list) :: self
        type(node_album), pointer :: aux
        type(sub_node), pointer :: sub_aux

        
        aux => self%head
        do while(associated(aux))
            
            print *, 'Cliente:', aux%value

            
            sub_aux => aux%list
            do while(associated(sub_aux))
                
                print *, '  imagen:', sub_aux%index
                sub_aux => sub_aux%next
            end do

            ! siguiente
            aux => aux%next

            
            print *, ""
        end do
    end subroutine printList
    
    

    subroutine delete(self, value)
        class(List_of_list), intent(inout) :: self
        character(len=*), intent(in) :: value

        type(node_album), pointer :: current

        if(.not. associated(self%head)) then
            return
        end if

        current => self%head
        do while(associated(current))
            if(current%value == value) then
                if(associated(current%prev)) then
                    current%prev%next => current%next
                else
                    self%head => current%next
                end if

                if(associated(current%next)) then
                    current%next%prev => current%prev
                else
                    self%tail => current%prev
                end if

                deallocate(current)
                return
            end if
            current => current%next
        end do
    end subroutine delete

    !Subnodo
    subroutine append(self, index)
        class(node_album), intent(inout) :: self
        integer, intent(in):: index

        type(sub_node), pointer :: aux
        type(sub_node), pointer :: new

        allocate(new)
        new%index = index

        if(.not. associated(self%list)) then
            self%list => new
        else
            aux => self%list
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => new
        end if
    end subroutine append

    subroutine print(self)
        class(node_album), intent(inout) :: self

        type(sub_node), pointer :: aux
        aux => self%list

        do while(associated(aux))
            print *, '  imagen:', aux%index
            aux => aux%next
        end do
    end subroutine print

    subroutine print_G(self, unit)
        class(node_album), intent(inout) :: self
        integer, intent(in) :: unit
    
        type(sub_node), pointer :: auxs
        auxs => self%list
    
        do while(associated(auxs))
            write(unit, "(A,I0)") "->", auxs%index
            auxs => auxs%next
        end do
    end subroutine print_G
    
    
    subroutine printIndicesFirst(self,filename)
    class(List_of_list), intent(in) :: self
    type(node_album), pointer :: aux
    type(sub_node), pointer :: auxs
    character(len=*), intent(in) :: filename
    integer :: unit
    aux => self%head
    open(unit, file=filename, status='replace')

    write(unit, "(A)") "digraph G {"
    write(unit, "(A)") "  node [color=lightblue2, style=filled];"
    write(unit, "(A)") "    { rank=same; album"
    do while(associated(aux))
        !print *, aux%index
        write(unit, "(A,A)") aux%value,";"
        aux => aux%next
    end do
    
    write(unit, "(A)") "}"

    write(unit, "(A)") "album"
    aux => self%head
    do while(associated(aux))
        !print *, "UNO"
        write(unit, "(A,A)")"->",aux%value
        aux => aux%next
    end do
    
    aux => self%head
    do while(associated(aux))
        write(unit, "(A)") aux%value
        !call aux%print_G(unit)
        auxs => aux%list
        do while(associated(auxs))
            write(unit, "(A,A,I0)") "->", aux%value ,auxs%index
            auxs => auxs%next
        end do

        aux => aux%next
    end do
write(unit, "(A)") "}"
close(unit)
        
! Generar el archivo PNG utilizando Graphviz
!call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
end subroutine printIndicesFirst
end module lista_album
