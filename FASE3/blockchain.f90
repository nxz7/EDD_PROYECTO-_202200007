module blockchain
    implicit none
    private

    type, public ::nodeBlock
        private
        integer :: index, nonce
        character(:), allocatable ::  TIMESTAMP, previous_hash, hash, rook_merkle
        character(len=100), allocatable :: dataF(:)
        type(nodeBlock), pointer :: next     
    end type nodeBlock

    type, public :: block_list
        private
        type(nodeBlock), pointer :: head => null()
    contains
        procedure :: push
        procedure :: print
        procedure :: delete
        procedure :: search
        procedure :: gg
    end type block_list

contains

    subroutine push(this, index, nonce, TIMESTAMP, previous_hash, hash, dataF, rook_merkle)
        class(block_list), intent(inout) :: this
        integer, intent(in) :: index, nonce
        character(:), allocatable, intent(in) ::  TIMESTAMP, previous_hash, hash, rook_merkle
        character(len=100), allocatable, intent(in) :: dataF(:)

        type(nodeBlock), pointer :: temp
        allocate(temp)
        temp%index = index
        temp%nonce = nonce
        temp%TIMESTAMP = TIMESTAMP
        temp%previous_hash = previous_hash
        temp%hash = hash
        temp%dataF = dataF
        temp%rook_merkle = rook_merkle
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        !print *, 'pushed ', index
    end subroutine push


    subroutine delete(this, index)
        class(block_list), intent(inout) :: this
        integer, intent(in) :: index
        type(nodeBlock), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%index /= index)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
        if(associated(current) .and. current%index == index) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            print *, 'Se eliminó el valor ', index
        else
            print *, 'No se encontró el valor ', index
        end if

    end subroutine delete

    function search(this, index) result(retval)
        class(block_list), intent(in) :: this
        integer, intent(in) :: index

        type(nodeBlock), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%index == index) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(block_list), intent(in) :: this
        type(nodeBlock), pointer :: current

        current => this%head

        do while (associated(current))
            print *, "************************************************"
            print *, current%index, current%nonce, current%TIMESTAMP, current%previous_hash
            print *, current%hash, current%dataF, current%rook_merkle
            current => current%next
        end do 
    end subroutine print

    subroutine gg(this)
        class(block_list), intent(in) :: this
        integer :: file_unit
        type(nodeBlock), pointer :: current
        file_unit = 4
        current => this%head
    
        open(newunit=file_unit, file='blockchain.dot', status='replace')

        write(file_unit, '(A)') 'digraph blockchain {'
        write(file_unit, '(A)') '    rankdir=LR;'
        write(file_unit, '(A)') ' node [shape=plaintext, color= lightgreen; ];'
        do while (associated(current))
write(file_unit, '(A,I0,A)') 'node',current%index,'[label=<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">'
write(file_unit, '(A)') '        <TR><TD>ID</TD></TR>'
write(file_unit, '(A,I0,A)') '        <TR><TD>',current%index , '</TD></TR>'
            write(file_unit, '(A)') '        <TR><TD>Data</TD></TR>'
            write(file_unit, '(A,A,A)') '        <TR><TD>' // current%dataF // '</TD></TR>'
            write(file_unit, '(A)') '        <TR><TD>RootMerkle</TD></TR>'
            write(file_unit, '(A,A,A)') '        <TR><TD>' // trim(adjustl(current%rook_merkle)) // '</TD></TR>'
            write(file_unit, '(A)') '        <TR><TD>HASH</TD></TR>'
            write(file_unit, '(A,A,A)') '        <TR><TD>' // trim(adjustl(current%hash)) // '</TD></TR>'
            write(file_unit, '(A)') '        <TR><TD>PREV HASH</TD></TR>'
            write(file_unit, '(A,A,A)') '        <TR><TD>' // trim(adjustl(current%previous_hash)) // '</TD></TR>'


            write(file_unit, '(A)') '        <TR><TD>Nonce</TD></TR>'
            write(file_unit, '(A,I0,A)') '        <TR><TD>' ,current%nonce,  '</TD></TR>'
            write(file_unit, '(A)') '        <TR><TD>Timestamps</TD></TR>'
            write(file_unit, '(A,A,A)') '        <TR><TD>' // trim(adjustl(current%TIMESTAMP)) // '</TD></TR>'

            write(file_unit, '(A)') '    </TABLE>>];'
    
            
            if (associated(current%next)) then
                write(file_unit, '(A,I0,A,I0,A)') '    node',current%next%index, ' -> node' ,current%index, ';'
            end if
    
            current => current%next
        end do 
    
        
        write(file_unit, '(A)') '}'
        close(file_unit)
        call execute_command_line('dot -Tsvg blockchain.dot > blockchain.svg')
        call execute_command_line('start blockchain.svg')
    end subroutine gg
    

    
end module blockchain