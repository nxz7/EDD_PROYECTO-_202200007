module cola_recepcion
    use cliente
    implicit none
    private

    ! NODO
    type, public :: node
        private
        type(client), pointer :: value
        type(node), pointer :: next
    end type node

    ! LISTA
    type, public :: queue
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()  ! New pointer to the end of the queue
    contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: print
        procedure :: top_info
        procedure :: get_top_name
    end type queue

contains

    subroutine enqueue(self, id, smallImg, bigImgs, name)
        class(queue), intent(inout) :: self
        integer, intent(in) :: id
        integer, intent(in) :: smallImg, bigImgs
        character(len=*), intent(in) :: name
        type(node), pointer :: tempNode
        type(client), pointer :: tempClient   

        ! Create the client and insert it into the node
        allocate(tempClient)
        allocate(tempNode)
        tempClient%id = id
        tempClient%name = name
        tempClient%smallImg = smallImg
        tempClient%bigImgs = bigImgs
        tempClient%steps = 0 !id+totalImgs+sallImg+2*bigImgs
        tempClient%totalImgs = smallImg + bigImgs
        tempClient%totalSmallImgs = smallImg
        tempClient%totalBigImgs = bigImgs
        tempNode%value => tempClient
        tempNode%next => null() 

        if (.not. associated(self%head)) then
            self%head => tempNode
            self%tail => tempNode  ! If queue is empty, both head and tail point to the new node
        else
            self%tail%next => tempNode  ! Add the new node to the end of the queue
            self%tail => tempNode  ! Update tail to point to the new node
        end if

        print *, "Value inserted successfully: ", name
    end subroutine enqueue

    subroutine dequeue(self, exitClient)
        class(queue), intent(inout) :: self
        type(client), pointer :: exitClient
        type(node), pointer :: removed_node

        if (.not. associated(self%head)) then
            print *, "The queue is empty."
            return
        end if

        removed_node => self%head
        exitClient => removed_node%value
        self%head => removed_node%next  ! Move head to the next node

        if (.not. associated(self%head)) then
            self%tail => null()  ! If the queue becomes empty, update tail to null
        end if

        deallocate(removed_node)
        print *, "The client has left the queue successfully."
    end subroutine dequeue

    subroutine print(self)
        class(queue), intent(inout) :: self
        type(node), pointer :: current
        type(client), pointer :: content

        if (.not. associated(self%head)) then
            print *, "The queue is empty."
            return
        end if

        current => self%head

        print *, "Queue elements:"
        do while (associated(current))
            content => current%value
            print *, 'ID:', content%id , ', img_g:', content%bigImgs, ', img_p:', content%smallImg, ', name:', content%name
            current => current%next
        end do
    end subroutine print

    subroutine top_info(self, id, num_g, num_p)
        class(queue), intent(in) :: self
        integer, intent(out) :: id
        integer, intent(out) :: num_g, num_p

        type(client), pointer :: topClient

        if (.not. associated(self%head)) then
            print *, "The queue is empty."
            return
        end if

        topClient => self%head%value

        id = topClient%id
        num_g = topClient%bigImgs
        num_p = topClient%smallImg
    end subroutine top_info

    subroutine get_top_name(self, name)
        class(queue), intent(in) :: self
        character(len=:), allocatable, intent(out) :: name
    
        type(client), pointer :: topClient
    
        if (.not. associated(self%head)) then
            print *, "The queue is empty."
            return
        end if
    
        topClient => self%head%value
        name = topClient%name
    end subroutine get_top_name
    
end module cola_recepcion
