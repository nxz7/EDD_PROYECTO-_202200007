module hash_table_m
    implicit none
    private
    integer :: M = 7
    real :: R = 0.618034
    integer, parameter :: long = selected_int_kind(4)
    integer, parameter :: dp = selected_real_kind(15)

    type, public :: HashTable
        integer :: elements = 0
        integer, allocatable :: table(:)

    contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure, private :: linealProbe
        procedure :: grafico
    end type HashTable
contains
    subroutine insert(self, key)
        class(HashTable), intent(inout) :: self
        type(HashTable) :: ret
        integer(long), intent(in) :: key
        integer, allocatable :: temp(:)
        integer :: pos

        if(.not. allocated(self%table)) then
            allocate(self%table(0:M-1))
            self%table(:) = -1
        end if

        pos = hash_index(key)

        if(self%table(pos) /= -1 .and. self%table(pos) /= key) then
            call self%linealProbe(pos)
        end if

        self%table(pos)=key
        self%elements = self%elements + 1
        if(self%elements * 1.0_dp/M > 0.75) then
            temp = self%table
            deallocate(self%table)
            ret = rehashing(temp)
            self%table = ret%table
            self%elements = ret%elements
        end if
    end subroutine insert

    function rehashing(temp) result(val)
        integer, intent(in) :: temp(:)
        integer :: i
        type(HashTable) :: val
        
        M = M*2
        allocate(val%table(0:M-1))
        val%table(:) = -1
        do i = 1, size(temp)
            if(temp(i) /= -1) then
                call val%insert(int(temp(i), kind=long))
            end if
        end do
    end function rehashing

    subroutine linealProbe(self, pos)
        class(HashTable), intent(inout) :: self
        integer, intent(inout) :: pos

        do while(self%table(pos) /= -1)
            pos = pos + 1
            pos = mod(pos, M)
        end do
    end subroutine linealProbe

    function hash_index(key) result(i)
        integer(long), intent(in) :: key
        integer :: i

        i = dispersion(key)
    end function hash_index

    function dispersion(x) result(v)
        integer(long), intent(in) :: x
        real :: t
        integer :: v

        t = R*x - floor(R*x)
        v = floor(M*t)
    end function dispersion

    subroutine print(self)
        class(HashTable), intent(inout) :: self
        print *, self%table
    end subroutine 
    
    subroutine search(self, val)
        class(HashTable), intent(inout) :: self
        integer, intent(in) :: val
        integer :: pos

        pos = hash_index(int(val, kind=long))
        if(self%table(pos) /= val) then
            do while(self%table(pos) /= val)
                pos = pos + 1
                pos = mod(pos, M)
            end do
        end if
        print *, self%table(pos)
    end subroutine search


    subroutine grafico(self, filename)
        class(HashTable), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer :: i
        integer :: unit
    
        unit = 10  
        open(unit, file=filename, status='replace')
    
        write(unit, '(A)') 'digraph HashTable {'
        write(unit, '(A)') '  node [shape=plaintext];'
        write(unit, '(A)') '  hash_table [label=<'
        write(unit, '(A)') '    <TABLE BORDER="1" CELLBORDER="1" CELLSPACING="0">'
    
        
        write(unit, '(A)') '      <TR>'
        write(unit, '(A)') '        <TD></TD>' 
        do i = 0, size(self%table) - 1
            write(unit, '(A,I0,A)') '        <TD>', i, '</TD>'
        end do
        write(unit, '(A)') '      </TR>'
    
        
        write(unit, '(A)') '      <TR>'
        write(unit, '(A)') '        <TD>tecnicos</TD>' ! Label for the row
        do i = 0, size(self%table) - 1
            if (self%table(i) /= -1) then
                write(unit, '(A,I0,A)') '        <TD>', self%table(i), '</TD>'
            else
                write(unit, '(A)') '        <TD>-1</TD>'
            end if
        end do
        write(unit, '(A)') '      </TR>'
    
        write(unit, '(A)') '    </TABLE>'
        write(unit, '(A)') '  >];'
        write(unit, '(A)') '}'
    
        close(unit)
    end subroutine grafico
    
    

end module hash_table_m

program main
    use hash_table_m
    implicit none

    type(HashTable) :: table
    integer, parameter :: long = selected_int_kind(4)
    character(len=100) :: filename

    ! Define the filename for the DOT file
    filename = 'hash_table.dot'

    call table%insert(int(5, kind=long))
    call table%insert(int(522, kind=long))
    call table%insert(int(16, kind=long))
    call table%insert(int(1, kind=long))
    call table%insert(int(18, kind=long))
    call table%insert(int(29, kind=long))
    call table%insert(int(42, kind=long))
    call table%insert(int(500, kind=long))
    call table%insert(int(60, kind=long))
    call table%insert(int(70, kind=long))
    call table%insert(int(1, kind=long))
    call table%print()
    call table%search(5)
    call table%grafico(filename)
    call execute_command_line('dot -Tsvg hash_table.dot > hash_table.svg')
	call execute_command_line('start hash_table.svg')

end program main