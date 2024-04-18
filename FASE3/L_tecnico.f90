module L_tecnico
    implicit none
    private

    type, public :: node
        private
        integer :: value  
        integer :: dpi, telefono, sucursal
        character(:), allocatable ::  name, apellido, genero, direccion
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: bubble_sort
        procedure :: print_5
        procedure :: find_and_print
        procedure :: increment_value
    end type cola

contains

    subroutine append(this, value,dpi, telefono,name, apellido, genero, direccion, sucursal)
        class(cola), intent(inout) :: this
        integer, intent(in) :: value
        integer, intent(in) :: dpi, telefono,sucursal 
        character(:), allocatable, intent(in) ::  name, apellido, genero, direccion

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%dpi = dpi
        temp%telefono = telefono
        temp%name = name
        temp%apellido = apellido
        temp%genero = genero
        temp%direccion = direccion
        temp%sucursal = sucursal
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

        print *, '--- ', value
    end subroutine append

    subroutine print(this, target_sucursal)
        class(cola), intent(in) :: this
        integer, intent(in) :: target_sucursal
        type(node), pointer :: current
    
        current => this%head
    
        print *, '********************************************'
        print *, 'Listado de técnicos de la sucursal ', target_sucursal, ':'
        print *, '********************************************'
    
        do while (associated(current))
            if (current%sucursal == target_sucursal) then
                print *, '>trabajos:', current%value
                print *, '>DPI:', current%dpi
                print *, '>Telefono:', current%telefono
                print *, '>Nombre:', current%name
                print *, '>Apellido:', current%apellido
                print *, '>Genero:', current%genero
                print *, '>Direccion:', current%direccion
                print *, '>Sucursal:', current%sucursal
                print *, '********************************************'
            end if
            current => current%next
        end do 
    end subroutine print
    

    subroutine bubble_sort(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: current, next_node, temp_node
        integer :: temp_value, temp_dpi, temp_telefono
        character(:), allocatable :: temp_name, temp_apellido, temp_genero, temp_direccion
        integer :: temp_sucursal
    
        if (.not. associated(this%head)) then
            print *, 'NO HAY TECNICO ES LA SUCURSAL'
            return
        end if
    
        current => this%head
        do while(associated(current))
            next_node => current%next
            do while(associated(next_node))
                if (current%value < next_node%value) then
                    ! Swap values
                    temp_value = current%value
                    current%value = next_node%value
                    next_node%value = temp_value
                    
                    ! Swap other attributes
                    temp_dpi = current%dpi
                    current%dpi = next_node%dpi
                    next_node%dpi = temp_dpi
    
                    temp_telefono = current%telefono
                    current%telefono = next_node%telefono
                    next_node%telefono = temp_telefono
                    
                    temp_name = current%name
                    current%name = next_node%name
                    next_node%name = temp_name
                    
                    temp_apellido = current%apellido
                    current%apellido = next_node%apellido
                    next_node%apellido = temp_apellido
                    
                    temp_genero = current%genero
                    current%genero = next_node%genero
                    next_node%genero = temp_genero
                    
                    temp_direccion = current%direccion
                    current%direccion = next_node%direccion
                    next_node%direccion = temp_direccion
                    
                    temp_sucursal = current%sucursal
                    current%sucursal = next_node%sucursal
                    next_node%sucursal = temp_sucursal
                end if
                next_node => next_node%next
            end do
            current => current%next
        end do
    end subroutine bubble_sort
    
!falta buscar por nombre y sumar contador


    subroutine print_5(this, target_sucursal)
        class(cola), intent(in) :: this
        integer, intent(in) :: target_sucursal
        type(node), pointer :: current
        integer :: count
    
        current => this%head
        count = 0
    
        print *, '********************************************'
        print *, 'TOP 5 TECNICOS DE LA SUCURSAL:', target_sucursal, ':'
        print *, '********************************************'
    
        do while (associated(current) .and. count < 5)
            if (current%sucursal == target_sucursal) then
                print *, 'TRABAJOS:', current%value
                print *, 'DPI:', current%dpi
                print *, 'Telefono:', current%telefono
                print *, 'Nombre:', current%name
                print *, 'Apellido:', current%apellido
                print *, 'Genero:', current%genero
                print *, 'Dirección:', current%direccion
                print *, 'Sucursal:', current%sucursal
                print *, '********************************************'
                count = count + 1
            end if
            current => current%next
        end do 
    end subroutine print_5
    
    subroutine find_and_print(this, target_dpi, target_sucursal)
        class(cola), intent(in) :: this
        integer, intent(in) :: target_dpi, target_sucursal
        type(node), pointer :: current
        
        current => this%head
        
        print *, '********************************************'
        print *, 'bucando tecnico con DPI:', target_dpi, 'en la sucursal:', target_sucursal
        print *, '********************************************'
    
        do while (associated(current))
            if (current%dpi == target_dpi .and. current%sucursal == target_sucursal) then
                print *, 'TRABAJOS:', current%value
                print *, 'DPI:', current%dpi
                print *, 'Telefono:', current%telefono
                print *, 'Nombre:', current%name
                print *, 'Apellido:', current%apellido
                print *, 'Genero:', current%genero
                print *, 'Dirección:', current%direccion
                print *, 'Sucursal:', current%sucursal
                print *, '********************************************'
                return
            end if
            current => current%next
        end do 
        
        print *, 'No se encontró ningún técnico con DPI:', target_dpi, 'en la sucursal:', target_sucursal
        print *, '********************************************'
    end subroutine find_and_print
    

    !incrementa trabajos de cliente y sucursal
        subroutine increment_value(this, target_dpi, target_sucursal)
            class(cola), intent(inout) :: this
            integer, intent(in) :: target_dpi, target_sucursal
            type(node), pointer :: current
            
            current => this%head
            
            do while (associated(current))
                if (current%dpi == target_dpi .and. current%sucursal == target_sucursal) then
                    current%value = current%value + 1
                    return
                end if
                current => current%next
            end do
            
            print *, 'No se encontró ningún técnico con DPI:', target_dpi, 'en la sucursal:', target_sucursal
        end subroutine increment_value


end module L_tecnico

program main
    use L_tecnico
    implicit none

    type(cola) :: my_cola
    character(:), allocatable ::  name, apellido,genero, direccion
    apellido=  'Perez'
    genero = 'M'
    direccion = 'Zona 1'
    name='Juan'
    call my_cola%append(5, 557, 12345678, name, apellido, genero, direccion,7)
    call my_cola%append(3, 336, 12345678, name, apellido, genero, direccion, 6)
    call my_cola%append(2, 225, 12345678, name, apellido, genero, direccion, 5)
    call my_cola%append(11, 117, 12345678, name, apellido, genero, direccion,7)
    call my_cola%append(8, 887, 12345678, name, apellido, genero, direccion,7)
    call my_cola%append(2, 227, 12345678, name, apellido, genero, direccion,7)
    call my_cola%append(4, 447, 12345678, name, apellido, genero, direccion, 7)
    call my_cola%append(14, 14147, 12345678, name, apellido, genero, direccion, 7)
    call my_cola%append(15, 15157, 12345678, name, apellido, genero, direccion,7)
    call my_cola%append(7, 777, 12345678, name, apellido, genero, direccion,7)
    call my_cola%append(9, 995, 12345678, name, apellido, genero, direccion,5)
    !call my_cola%print(7)

    call my_cola%bubble_sort()


    call my_cola%print_5(7)

    call my_cola%find_and_print(777, 7)
    call my_cola%increment_value(15157, 7)
    call my_cola%increment_value(15157, 7)
    call my_cola%bubble_sort()
    call my_cola%print_5(7)

    call my_cola%find_and_print(995, 7)
end program main