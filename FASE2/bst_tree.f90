module bst_tree
    use matrix_spar
    use cola_module

    implicit none
    integer :: id = 1
    type :: node_bi
        integer :: value
        integer :: uid
        character(:), allocatable :: cliente 
        type(node_bi), pointer :: left => null()
        type(node_bi), pointer :: right => null()
        type(matrix) :: matriz
    end type

    type :: bst
        type(node_bi), pointer :: root => null()
        contains
        procedure :: add
        procedure :: add_rec
        procedure :: preorder
        procedure :: inorder
        procedure :: postorder
        procedure :: dotgen
        procedure :: dotgen_rec
        procedure :: search_by_value
        procedure :: clear_binario
        !procedure :: image_preorder
    end type

    contains
        subroutine add(this, value, cliente, matriz_disp)
            class(bst), intent(inout) :: this
            integer, intent(in) :: value
            character(len=*), intent(in) :: cliente
            type(matrix), intent(in) :: matriz_disp
            type(node_bi), pointer :: tmp
            if(associated(this%root)) then
                call this%add_rec(value, cliente, matriz_disp, this%root)
            else
                allocate(tmp)
                tmp%value = value
                tmp%cliente = cliente
                tmp%uid = id
                tmp%matriz = matriz_disp
                id = id + 1
                this%root => tmp
                !print *, value
                !call tmp%matriz%print()
            end if
        end subroutine add

        subroutine add_rec(this, value, cliente, matriz_disp, tmp)
            class(bst), intent(inout) :: this
            integer, intent(in) :: value
            character(len=*), intent(in) :: cliente
            type(matrix), intent(in) :: matriz_disp
            class(node_bi), intent(inout) :: tmp
            if (value < tmp%value) then
                if (associated(tmp%left)) then
                    call this%add_rec(value, cliente, matriz_disp, tmp%left)
                else
                    allocate(tmp%left)
                    tmp%left%value = value
                    tmp%left%uid = id
                    tmp%left%cliente = cliente
                    tmp%left%matriz = matriz_disp
                    id = id + 1
                    !print *, value
                    !call tmp%matriz%print()
                end if
            else
                if (associated(tmp%right)) then
                    call this%add_rec(value, cliente, matriz_disp,tmp%right)
                else
                    allocate(tmp%right)
                    tmp%right%value = value
                    tmp%right%uid = id
                    tmp%right%cliente = cliente
                    tmp%right%matriz = matriz_disp
                    id = id + 1
                    !print *, value
                    !call tmp%matriz%print()
                end if
            end if
        end subroutine add_rec

        subroutine preorder(this, tmp, cola_pre)
            class(bst), intent(in) :: this
            class(node_bi), intent(in), pointer :: tmp
            type(cola), intent(inout)::cola_pre
            integer :: i

            if( .not. associated(tmp)) then
                return
            end if
            write (*, '(1I3)', advance='no') (tmp%value)
            i= tmp%value
            call cola_pre%append(i)
            !call cola_pre%print()
            print *, "-----------------------------------"
            call this%preorder(tmp%left, cola_pre)
            call this%preorder(tmp%right, cola_pre)
        end subroutine preorder

        subroutine inorder(this, tmp, cola_in)
            class(bst), intent(in) :: this
            class(node_bi), intent(in), pointer :: tmp
            type(cola), intent(inout)::cola_in
            integer :: i
            if( .not. associated(tmp)) then
                return
            end if
            call this%inorder(tmp%left,cola_in)
            write (*, '(1I3)', advance='no') (tmp%value)
            i= tmp%value
            call cola_in%append(i)
            call this%inorder(tmp%right, cola_in)
        end subroutine inorder

        subroutine postorder(this, tmp, cola_post)
            class(bst), intent(in) :: this
            class(node_bi), intent(in), pointer :: tmp
            type(cola), intent(inout)::cola_post
            integer :: i
            if( .not. associated(tmp)) then
                return
            end if
            call this%postorder(tmp%left, cola_post)
            call this%postorder(tmp%right, cola_post)
            write (*, '(1I3)', advance='no') (tmp%value)
            i= tmp%value
            call cola_post%append(i)
        end subroutine postorder

        subroutine dotgen(this, tmp, unit)
            class(bst), intent(in) :: this
                class(node_bi), intent(in), pointer :: tmp
                integer, intent(in) :: unit
            write(unit, '(A)') 'graph{'
            call this%dotgen_rec(tmp, unit)
            write(unit, '(A)') '}'
        end subroutine dotgen

        subroutine dotgen_rec(this, tmp, unit)
            class(bst), intent(in) :: this
            class(node_bi), intent(in), pointer :: tmp
                integer, intent(in) :: unit
            if (.not. associated(tmp)) then
                    return
                end if
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' [label="', tmp%value, '"];'
            !print *, tmp%value
            !call tmp%matriz%print()
            if (associated(tmp%left)) then
                    write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%left%uid, ';'
                end if
            if (associated(tmp%right)) then
                    write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%right%uid, ';'
                end if
            call this%dotgen_rec(tmp%left, unit)
                call this%dotgen_rec(tmp%right, unit)
        end subroutine dotgen_rec
!-------------------------------------
        subroutine search_by_value(this, valor, matrix_resultado)
            class(bst), intent(in) :: this
            integer, intent(in) :: valor
            type(matrix), intent(inout) :: matrix_resultado
            
            call search_by_value_rec(this%root, valor, matrix_resultado)
        end subroutine search_by_value
        
        recursive subroutine search_by_value_rec(tmp, valor, matrix_resultado)
        class(node_bi), intent(in), pointer :: tmp
        integer, intent(in) :: valor
        type(matrix), intent(inout) :: matrix_resultado
        
        if (associated(tmp)) then
            print *, "BUSCANDO CAPA ID:", tmp%value
            if (tmp%value == valor) then
                !print *, "LIMPIAR:"
                call matrix_resultado%clear_matrix() 

                !print *, "UNIR:"
                call matrix_resultado%unir_matrix(tmp%matriz)
                
                return
            else if (valor < tmp%value) then
                if (associated(tmp%left)) then
                    call search_by_value_rec(tmp%left, valor, matrix_resultado)
                else
                    print *, "NO ESTA ASOCIADO."
                end if
            else
                if (associated(tmp%right)) then
                    call search_by_value_rec(tmp%right, valor, matrix_resultado)
                else
                    print *, "NO ESTA ASOCIADO"
                end if
            end if
        else
            print *, "ESA CAPA NO EXISTE PARA ESTE CLIENTE."
        end if
    end subroutine search_by_value_rec
    
        
!----------------------------------------- 
        subroutine clear_binario(this)
            class(bst), intent(inout) :: this
            if (associated(this%root)) then
                call clear_binodes(this%root)
                this%root => null()
            end if
        end subroutine clear_binario

                recursive subroutine clear_binodes(tmp)
                    type(node_bi), pointer :: tmp
                    if (associated(tmp)) then
                        call clear_binodes(tmp%left)
                        call clear_binodes(tmp%right)
                        deallocate(tmp)
                    end if
                end subroutine clear_binodes
        
!--------------------GENERAR IMAGEN PREORDEN

!-----------------------------------------                
                
end module bst_tree