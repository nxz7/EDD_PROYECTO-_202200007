program Menu
    use cola_recepcion
    use ventanillas
    implicit none
!-----------LISTAS-------------------------
    type(cola_doble) :: cola_inicio
    type(ventanillas_node), pointer :: ventanillas_list
        !--VARIABLES QUE SE USAN-
    character(len=:), allocatable :: client_name
    character(len=:), allocatable :: pila
    character(len=1) :: opcion
    character(len=:), allocatable :: nombre

    allocate(ventanillas_list)

    ! Display the initial menu
    print *, "-----------------------------------"
    print *, "FASE 1 EDD - 202200007"
    print *, "-----------------------------------"
    print *, "MENU PRINCIPAL"
    print *, "1. CARGA DE CLIENTES"
    print *, "2. CANTIDAD DE VENTANILLAS"
    print *, "3. EJECUTAR PASO"
    print *, "4. ESTADO EN MEMORIA DE LAS ESTRUCTURAS DE DATOS"
    print *, "5. REPORTES"
    print *, "6. ACERCA DE -INFO CREADOR"
    print *, "7. Salir"
    print *, "-----------------------------------"

    ! inicia el loop de mostrar menu
    do
        print *, "Ingrese una opcion: "
        read(*,*) opcion

        select case(opcion)
            case('1')
                print *, "ESCRIBA LA RUTA DEL ARCHIVO DE CLIENTES:"
                ! Add items to the queue
                nombre = 'primero'
                call cola_inicio%add_cliente(1, 3, 2, nombre)
                call add_cliente_to_ventanillas(ventanillas_list, 1, 3, 2, nombre)
                pila = 'pila 1 -1'
                call add_element_to_stack(ventanillas_list, nombre, pila)
                pila = 'pila 1 -2'
                call add_element_to_stack(ventanillas_list, nombre, pila)
                

                nombre = 'segundo'
                call cola_inicio%add_cliente(2, 1, 0, nombre)
                call add_cliente_to_ventanillas(ventanillas_list, 2, 1, 0, nombre)
                pila = 'pila 2 -1'
                call add_element_to_stack(ventanillas_list, nombre, pila)
                

                nombre = 'tercero'
                call cola_inicio%add_cliente(3, 2, 1, nombre)
                call add_cliente_to_ventanillas(ventanillas_list, 3, 2, 1, nombre)
                
                ! Show the contents of the queue
                print *, "Queue contents after adding items:"
                call cola_inicio%show_clientes()
                
                ! Remove an item from the queue
                call cola_inicio%remove_cliente()
                
                ! Show the contents of the queue after removal
                print *, "Queue contents after removing an item:"
                call cola_inicio%show_clientes()

            case('2')
                print *, "ESCRIBA LA CANTIDAD DE VENTANILLAS A CREAR"
            case('3')
                print *, "EJECUTANDO PASO..."
                call display_clients(ventanillas_list)
            case('4')
                print *, "ESTADO DE LAS ESTRUCTURAS DE DATOS:"
            case('5')
                print *, "REPORTES DE CLIENTES:"
            case('6')
                print *, "----------------------------------------------"
                print *, "NATALIA MARIEL CALDERON ECHEVERRIA "
                print *, "202200007"
                print *, "----------------------------------------------"
            case('7')
                ! Handle option 7
                exit
            case default
                print *, "ERROR - OPCION INVALIDA"
        end select
    end do

end program Menu

