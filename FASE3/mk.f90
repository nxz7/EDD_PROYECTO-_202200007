module mk_inf
    implicit none
    private

    type, public :: node_mk
        private
        integer ::  id
        character(:), allocatable ::  departamento, direccion, password
        type(node_mk), pointer :: next     
    end type node_mk

    type, public :: mk_suc
        private
        type(node_mk), pointer :: head => null()
        type(node_mk), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: search
        procedure :: merge_nodes
    end type mk_suc

contains

    subroutine append(this, id, departamento, direccion, password)
        class(mk_suc), intent(inout) :: this
        integer, intent(in) :: id
        character(:), allocatable, intent(in) ::  departamento, direccion, password
        type(node_mk), pointer :: temp

        allocate(temp)
        temp%id = id
        temp%departamento = departamento
        temp%direccion = direccion
        temp%password = password
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

    end subroutine append

    subroutine print(this)
        class(mk_suc), intent(in) :: this
        type(node_mk), pointer :: current
        integer :: counter
        
        current => this%head
        counter = 0
        
        print *, '****************************'
        print *, 'SUCURSALES POR TRABAJOS'
        print *, '****************************'
    
        do while (associated(current))
            print *, '****************************'
            print *, 'ID: ', current%id
            print *, 'departamentp: ', current%departamento
            print *, 'direccion: ', current%direccion
            current => current%next
            counter = counter + 1
        end do 
    end subroutine print
    
    subroutine search(this, id, found, departamento, direccion, password)
        class(mk_suc), intent(in) :: this
        integer, intent(in) :: id
        logical, intent(out) :: found
        character(len=:), allocatable, intent(out) :: departamento, direccion, password
        type(node_mk), pointer :: current
    
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                found = .true.
                departamento = current%departamento
                direccion = current%direccion
                password = current%password
                return
            end if
            current => current%next
        end do
        found = .false.
    end subroutine search

    subroutine merge_nodes(this, input_array, result)
        class(mk_suc), intent(in) :: this
        integer, dimension(:), intent(in) :: input_array
        character(len=100), allocatable, intent(out) :: result(:)
        integer, dimension(:), allocatable :: valid_ids
        type(node_mk), pointer :: current
        integer :: i, count, id1, id2, valid_count
        character(len=100) :: merged_string
        logical :: found1, found2
        character(len=:), allocatable :: departamento1, direccion1, password1, departamento2, direccion2, password2
    
        count = size(input_array)
        if (count < 2) then
            print *, "Input array must contain at least two elements"
            return
        end if
    
        ! C IDs
        valid_count = 0
        do i = 1, count
            if (input_array(i) /= 0) then
                valid_count = valid_count + 1
            end if
        end do
    
        ! --------------------
        allocate(valid_ids(valid_count))
    
        ! memoria
        valid_count = 0
        do i = 1, count
            if (input_array(i) /= 0) then
                valid_count = valid_count + 1
                valid_ids(valid_count) = input_array(i)
            end if
        end do
    
        ! res
        allocate(result(valid_count-1))
    
        ! merge
        do i = 1, valid_count - 1
            id1 = valid_ids(i)
            id2 = valid_ids(i+1)
    
            ! lo busca
            current => this%head
            found1 = .false.
            found2 = .false.
    
            ! Search for the first node
            do while (.not. found1 .and. associated(current))
                call this%search(id1, found1, departamento1, direccion1, password1)
                if (.not. found1) then
                    print *, "Node with ID ", id1, " not found"
                    exit ! sale
                end if
                current => current%next
            end do
    
            ! se pasa a la siguiente
            if (.not. found1) then
                cycle
            end if
    
            ! buscar
            current => this%head
            do while (.not. found2 .and. associated(current))
                call this%search(id2, found2, departamento2, direccion2, password2)
                if (.not. found2) then
                    print *, "Node with ID ", id2, " not found"
                    exit !dddddddddddd
                end if
                current => current%next
            end do
    
            ! siguiente
            if (.not. found2) then
                cycle
            end if
    
            ! Merge
            write(merged_string, '(I0,A,I0,A)') id1, trim(direccion1), id2, trim(direccion2)
    
            
            if (len_trim(merged_string) > 0) then
                result(i) = merged_string
            endif
        end do
    
        
        deallocate(valid_ids)
    end subroutine merge_nodes
    
    

end module mk_inf