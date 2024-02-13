program Menu
    use cola_recepcion
    use ventanillas
    implicit none
!-----------LISTAS-------------------------
    type(simple_linked_list) :: lista_v
    type(linked_list) :: list
    integer :: id_value
    integer :: img_pf
    integer :: img_pg
    integer :: numItems
    integer :: ventanillas  
    character(len=1) :: opcion
    !character(:), allocatable :: nombre_p

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
                call list%enqueue(1,0,3,"Hola")
                call list%enqueue(2,2,2,"Mundo")
                call list%enqueue(3,3,3,"1")
                call list%enqueue(4,4,4,"FIN")
                call list%enqueue(5,5,5,"5")
                call list%enqueue(6,6,6,"6")
                call list%print()
            
                call list%dequeue()
                call list%print()

            case('2')
                print *, "¿Cuantas ventanillas quiere crear?"
                read(*, *) ventanillas
            case('3')
                print *, "EJECUTANDO PASO..."
                call list%get_top_info("id",id_value)
                call list%get_top_info("img_p",img_pf)
                call list%get_top_info("img_g",img_pg)
                ! VERIFICACION
                print *, "ID of the top node in the queue:", id_value
            
            
                ! ENTRA EN EL LOOP DE AGREGAR VENTANAS ---  EL LOOP LE FALTA LA PILA, IMAGNES Y MOVER
                call lista_v%get_count(numItems)
                do while (numItems < ventanillas)
                    call lista_v%append("ventana", id_value, img_pf, img_pg)
                    call list%dequeue()
                    call list%get_top_info("id",id_value)
                    call list%get_top_info("img_p",img_pf)
                    call list%get_top_info("img_g",img_pg)
                    !call lista_v%append("prueba2",id_value,img_pf,img_pg)
            
                
                    call lista_v%get_count(numItems)
            
                    ! MIRAR EL NUMERO, SI LA CANTIDAD DE VENTANILLAS ES IGUAL A LA CANTIDAD QUE SE HA CREADO
                    if (numItems >= ventanillas) then
                        print *, "No mas ventanas"
                        exit ! Exit the loop
                    end if
                end do
            
                print *, "VENTANAS"
                call lista_v%print()
                call lista_v%get_count(numItems)
                print *, "COLA:"
                call list%print()
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