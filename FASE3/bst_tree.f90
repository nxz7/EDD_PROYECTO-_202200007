module bst_tree
	implicit none
	integer :: id = 1
	type :: node
		integer :: value
		integer :: uid 
		type(node), pointer :: left => null()
		type(node), pointer :: right => null()
        character(:), allocatable ::  departamento, direccion, password
	end type

	type :: bst
		type(node), pointer :: root => null()
		contains
		procedure :: add
		procedure :: add_rec
		procedure :: preorder
		procedure :: inorder
		procedure :: postorder
		procedure :: dotgen
		procedure :: dotgen_rec
        procedure :: search_node
	end type

contains
subroutine add(this, value, departamento, direccion, password)
	class(bst), intent(inout) :: this
	integer, intent(in) :: value
    character(:), allocatable, intent(in) ::  departamento, direccion, password 

	type(node), pointer :: tmp
	if(associated(this%root)) then
		call this%add_rec(value, this%root, departamento, direccion, password)
	else
		allocate(tmp)
		tmp%value = value
        tmp%departamento = departamento
        tmp%direccion = direccion
        tmp%password = password
		tmp%uid = id
		id = id + 1
		this%root => tmp
	end if	
end subroutine add

subroutine add_rec(this, value, tmp, departamento, direccion, password)
	class(bst), intent(inout) :: this
	integer, intent(in) :: value
    character(:), allocatable, intent(in) ::  departamento, direccion, password 
	class(node), intent(inout) :: tmp
	if (value < tmp%value) then
		if (associated(tmp%left)) then
			call this%add_rec(value, tmp%left, departamento, direccion, password)
		else
			allocate(tmp%left)
			tmp%left%value = value
            tmp%left%departamento = departamento
        tmp%left%direccion = direccion
        tmp%left%password = password
			tmp%left%uid = id
			id = id + 1
		end if
	else
		if (associated(tmp%right)) then
			call this%add_rec(value, tmp%right, departamento, direccion, password)
		else
			allocate(tmp%right)
			tmp%right%value = value
            tmp%right%departamento = departamento
        tmp%right%direccion = direccion
        tmp%right%password = password
			tmp%right%uid = id
			id = id + 1
		end if
	end if	
end subroutine add_rec

subroutine preorder(this, tmp)
	class(bst), intent(in) :: this
	class(node), intent(in), pointer :: tmp
	if( .not. associated(tmp)) then
		return
	end if
	write (*, '(1I3)', advance='no') (tmp%value)
	call this%preorder(tmp%left)
	call this%preorder(tmp%right)
end subroutine preorder

subroutine inorder(this, tmp)
	class(bst), intent(in) :: this
	class(node), intent(in), pointer :: tmp
	if( .not. associated(tmp)) then
		return
	end if
	call this%inorder(tmp%left)
	write (*, '(1I3)', advance='no') (tmp%value)
	call this%inorder(tmp%right)
end subroutine inorder

subroutine postorder(this, tmp)
	class(bst), intent(in) :: this
	class(node), intent(in), pointer :: tmp
	if( .not. associated(tmp)) then
		return
	end if
	call this%postorder(tmp%left)
	call this%postorder(tmp%right)
	write (*, '(1I3)', advance='no') (tmp%value)
end subroutine postorder

subroutine dotgen(this, tmp, unit)
   	class(bst), intent(in) :: this
    	class(node), intent(in), pointer :: tmp
    	integer, intent(in) :: unit
	write(unit, '(A)') 'graph{'
	call this%dotgen_rec(tmp, unit)
	write(unit, '(A)') '}'
end subroutine dotgen

subroutine dotgen_rec(this, tmp, unit)
   	class(bst), intent(in) :: this
 	class(node), intent(in), pointer :: tmp
    	integer, intent(in) :: unit
	if (.not. associated(tmp)) then
        	return
    	end if
	write (unit, '(A,I5,A,I5,A,A)') ' ', tmp%uid, ' [label="', tmp%value, tmp%password, '"];'
    print *, tmp%password
	if (associated(tmp%left)) then
        	write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%left%uid, ';'
    	end if
	if (associated(tmp%right)) then
        	write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%right%uid, ';'
    	end if
	call this%dotgen_rec(tmp%left, unit)
    	call this%dotgen_rec(tmp%right, unit)
end subroutine dotgen_rec

!search node

subroutine search_node(this, value, password, found)
    class(bst), intent(in) :: this
    integer, intent(in) :: value
    character(len=*), intent(in) :: password
    logical, intent(out) :: found
    
    type(node), pointer :: current_node
    
    found = .false.
    current_node => this%root
    
    do while (associated(current_node))
        if (current_node%value == value .and. trim(current_node%password) == trim(password)) then
            found = .true.
            return
        else if (value < current_node%value) then
            current_node => current_node%left
        else
            current_node => current_node%right
        end if
    end do
end subroutine search_node


end module bst_tree




program bstree
	use bst_tree
	implicit none
	type(bst) :: t	
	integer :: unit
    	character(len=100) :: filename
        character(:), allocatable ::  departamento, direccion, password, password2, password3, password4, password5, password6
        logical :: found
	filename = 'output.dot'
departamento = 'departamento'
direccion = 'direccion'
password = "p1"
password2="p2"
    password3="p3"
    password4="p4"
    password5="p5"
    password6="p6"

    	open(unit, file=filename, status='replace')	

	call t%add(25,departamento, direccion, password)
	call t%add(10,departamento, direccion, password2)
	call t%add(35,departamento, direccion, password3)
	call t%add(5,departamento, direccion, password4)
	call t%add(20,departamento, direccion, password5)
	call t%add(30,departamento, direccion, password6)
	
    	print *, 'Generating Dot file...'
    	call t%dotgen(t%root, unit)
    	close(unit)
	print *, 'Dot file generated:', trim(filename)
    call execute_command_line('dot -Tsvg output.dot > output.svg')
	call execute_command_line('start output.svg')


    print *, 'BUSCAR'
call t%search_node(30, password5, found)
print *, found
end program bstree