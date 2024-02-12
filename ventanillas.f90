module ventanillas
    implicit none
    
    type :: cliente
        integer :: numero, id, img_g, img_p
        character(len=:), allocatable :: nombre
        type(stack), pointer :: client_stack_ptr => null() ! apunta a la pila de elementos asociada con el cliente
        type(cliente), pointer :: next
    end type cliente
    !LISTA SIMPLE DE VENTANILLAS
    type :: ventanillas_node
        integer :: numero
        type(cliente), pointer :: head => null()
    end type ventanillas_node
!PILA DENTRO DE CADA NODO DE LOS CLIENTES
    type :: stack_node
        character(len=:), allocatable :: nombre_p
        type(stack_node), pointer :: next
    end type stack_node
    
    type :: stack
        type(stack_node), pointer :: top => null()
    end type stack

contains

    subroutine push(stack_ptr, nombre_p)
        type(stack), pointer :: stack_ptr
        character(len=:), allocatable, intent(in) :: nombre_p
        type(stack_node), pointer :: new_node
        
        allocate(new_node)
        new_node%nombre_p = nombre_p
        new_node%next => stack_ptr%top
        stack_ptr%top => new_node
    end subroutine push

    subroutine pop(stack_ptr, nombre_p)
        type(stack), pointer :: stack_ptr
        character(len=:), allocatable, intent(out) :: nombre_p
        type(stack_node), pointer :: temp
        
        if (associated(stack_ptr%top)) then
            nombre_p = stack_ptr%top%nombre_p
            temp => stack_ptr%top
            stack_ptr%top => stack_ptr%top%next
            deallocate(temp)
        else
            nombre_p = ''
        end if
    end subroutine pop

    subroutine add_cliente_to_ventanillas(ventanillas_ptr, id, img_g, img_p, nombre)
        type(ventanillas_node), pointer :: ventanillas_ptr
        integer, intent(in) :: id, img_g, img_p
        character(len=:), allocatable, intent(in) :: nombre
        type(cliente), pointer :: new_client, current_client
        
        allocate(new_client)
        new_client%numero = 0
        new_client%id = id
        new_client%img_g = img_g
        new_client%img_p = img_p
        allocate(new_client%nombre, source=nombre)
        
        new_client%next => null() ! inicializar el puntero next del nuevo cliente
        
        ! memoria para la pila de elementos asociada con el cliente
        allocate(new_client%client_stack_ptr)
        
        if (.not. associated(ventanillas_ptr%head)) then
            ventanillas_ptr%head => new_client
        else
            current_client => ventanillas_ptr%head
            do while (associated(current_client%next))
                current_client => current_client%next
            end do
            current_client%next => new_client
        end if
        
        ventanillas_ptr%numero = ventanillas_ptr%numero + 1
    end subroutine add_cliente_to_ventanillas

    subroutine add_element_to_stack(ventanillas_ptr, nombre, element)
        type(ventanillas_node), pointer :: ventanillas_ptr
        character(len=:), allocatable, intent(in) :: nombre, element
        type(cliente), pointer :: current_client
        logical :: found
        
        found = .false.
        
        current_client => ventanillas_ptr%head
        do while (associated(current_client))
            if (trim(current_client%nombre) == trim(nombre)) then
                call push(current_client%client_stack_ptr, element) ! anadir elementos a la pila del cliente
                found = .true.
                exit
            end if
            current_client => current_client%next
        end do
        
        if (.not. found) then
            print *, "Node with specified 'nombre' not found."
        end if
    end subroutine add_element_to_stack

    subroutine display_clients(ventanillas_ptr)
        type(ventanillas_node), intent(in) :: ventanillas_ptr
        type(cliente), pointer :: current_client
        type(stack_node), pointer :: current_element
        integer :: counter
        
        current_client => ventanillas_ptr%head
        counter = 1
        print *, 'Ventanillas List:'
        do while (associated(current_client))
            print *, 'Client ', counter, ':'
            print *, '  ID:', current_client%id
            print *, '  Img_g:', current_client%img_g
            print *, '  Img_p:', current_client%img_p
            print *, '  Nombre:', trim(current_client%nombre)
            
            ! MOSTRAR LOS ELEMENTOS DE LA PILA ASOCIADA CON EL CLIENTE
            print *, '  Stack Contents:'
            if (associated(current_client%client_stack_ptr)) then
                current_element => current_client%client_stack_ptr%top
                do while (associated(current_element))
                    print *, '    ', trim(current_element%nombre_p)
                    current_element => current_element%next
                end do
            else
                print *, '    Pila vacia'
            end if
            
            current_client => current_client%next
            counter = counter + 1
        end do
    end subroutine display_clients
end module ventanillas