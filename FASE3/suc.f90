module suc
    implicit none
    private

    type, public :: node_s
        private
        integer :: value, id, costos, ganancias
        type(node_s), pointer :: next     
    end type node_s

    type, public :: cola
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
    end type cola

contains

    subroutine append(this, value, id, costos, ganancias)
        class(cola), intent(inout) :: this
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

        print *, 'Append ', value
    end subroutine append

    subroutine print(this)
        class(cola), intent(in) :: this
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
        class(cola), intent(inout) :: this
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
        class(cola), intent(inout) :: this
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
        class(cola), intent(inout) :: this
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
        class(cola), intent(inout) :: this
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
    
    !METODOS QUE VAYAN SUMANDO A GANANCIAS Y COSTOS SEGU LA SUCURSAL Y UNO QUE HAGA LAS GANANCIAS


end module suc

program main
    use suc
    implicit none

    type(cola) :: my_cola

    call my_cola%append(5,1,100,200)
    call my_cola%append(3,2,300,400)
    call my_cola%append(7,3,500,600)
    call my_cola%append(1,4,500,400)
    call my_cola%append(9,5,500,500)

    call my_cola%add_value(3)
    call my_cola%add_costos(3,100)
    call my_cola%add_ganancias(5,500)
    call my_cola%bubble_sort()

    call my_cola%print()
end program main