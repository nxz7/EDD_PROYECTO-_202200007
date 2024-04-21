module suc
    implicit none
    private

    type, public :: node_s
        private
        integer :: value, id, costos, ganancias
        type(node_s), pointer :: next     
    end type node_s

    type, public :: sucursal
        private
        type(node_s), pointer :: head => null()
        type(node_s), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: bubble_sort
        procedure :: add_value
        procedure :: add_costos
        procedure :: add_ganancias
        procedure :: sum_ganancias
        procedure :: sum_costos
    end type sucursal

contains

    subroutine append(this, value, id, costos, ganancias)
        class(sucursal), intent(inout) :: this
        integer, intent(in) :: value, id, costos, ganancias

        type(node_s), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%id = id
        temp%costos = costos
        temp%ganancias = ganancias
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

        print *, 'agregando sucursales: ', id
    end subroutine append

    subroutine print(this)
        class(sucursal), intent(in) :: this
        type(node_s), pointer :: current
        integer :: counter
        
        current => this%head
        counter = 0
        
        print *, '****************************'
        print *, 'TOP 5 SUCURSALES POR TRABAJOS'
        print *, '****************************'
    
        do while (associated(current) .and. counter < 5)
            print *, '****************************'
            print *, "Trabajos",current%value
            print *, 'ID: ', current%id
            print *, 'Costos: ', current%costos
            print *, 'Ganancias: ', current%ganancias
            current => current%next
            counter = counter + 1
        end do 
    end subroutine print
    

    subroutine bubble_sort(this)
        class(sucursal), intent(inout) :: this
        type(node_s), pointer :: current, next_node, temp_node
        integer :: temp_value, temp_id, temp_costos, temp_ganancias
    
        if (.not. associated(this%head)) then
            print *, 'NO HAY TECNICO ES LA SUCURSAL'
            return
        end if
    
        current => this%head
        do while(associated(current))
            next_node => current%next
            do while(associated(next_node))
                if (current%value < next_node%value) then
                    temp_value = current%value
                    current%value = next_node%value
                    next_node%value = temp_value
                    

                    temp_id = current%id
                    current%id = next_node%id
                    next_node%id = temp_id
    
                    temp_costos = current%costos
                    current%costos = next_node%costos
                    next_node%costos = temp_costos
                    
                    temp_ganancias = current%ganancias
                    current%ganancias = next_node%ganancias
                    next_node%ganancias = temp_ganancias
                    
                end if
                next_node => next_node%next
            end do
            current => current%next
        end do
    end subroutine bubble_sort
    
    subroutine add_value(this, id)
        class(sucursal), intent(inout) :: this
        integer, intent(in) :: id
        type(node_s), pointer :: current
        
        current => this%head
        
        do while (associated(current))
            if (current%id == id) then
                current%value = current%value + 1
                exit
            end if
            current => current%next
        end do
    end subroutine add_value
    
    subroutine add_costos(this, id, amount_to_add)
        class(sucursal), intent(inout) :: this
        integer, intent(in) :: id, amount_to_add
        type(node_s), pointer :: current
        
        current => this%head
        
        do while (associated(current))
            if (current%id == id) then
                current%costos = current%costos + amount_to_add
                exit
            end if
            current => current%next
        end do
    end subroutine add_costos
    
    subroutine add_ganancias(this, id, amount_to_ganancias)
        class(sucursal), intent(inout) :: this
        integer, intent(in) :: id, amount_to_ganancias
        type(node_s), pointer :: current
        
        current => this%head
        
        do while (associated(current))
            if (current%id == id) then
                current%ganancias = current%ganancias + amount_to_ganancias
                exit
            end if
            current => current%next
        end do
    end subroutine add_ganancias
    
    subroutine sum_ganancias(this, result)
        class(sucursal), intent(in) :: this
        integer, intent(out) :: result
        type(node_s), pointer :: current
        
        result = 0
        current => this%head
        
        do while (associated(current))
            result = result + current%ganancias
            current => current%next
        end do
    end subroutine sum_ganancias

    subroutine sum_costos(this, result)
        class(sucursal), intent(in) :: this
        integer, intent(out) :: result
        type(node_s), pointer :: current
        
        result = 0
        current => this%head
        
        do while (associated(current))
            result = result + current%costos
            current => current%next
        end do
    end subroutine sum_costos


end module suc

