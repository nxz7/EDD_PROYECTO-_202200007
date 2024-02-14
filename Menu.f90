program Menu
    use cola_recepcion
    use ventanillas
    use cliente
    use impresora
    implicit none
!-----------LISTAS-------------------------
    type(simple_linked_list) :: lista_v
    type(queue) :: list
    type(impresora_list) :: impresora
    integer:: id_value
    integer :: img_pf
    integer :: img_pg
    integer :: numItems
    integer :: ventanillas  
    character(len=1) :: opcion
    type(client), pointer :: temp
    character(:), allocatable ::  name_p
    integer :: cant
    character(:), allocatable :: tipo, cliente
    integer :: contador_pasos = 0

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
                ! id, smallImg, bigImgs, name)
                call list%enqueue(1,0,2,"Hola")
                call list%enqueue(2,2,1,"Mundo")
                call list%enqueue(3,3,2,"1")
                call list%enqueue(4,4,1,"FIN")
                call list%enqueue(5,5,2,"5")
                call list%enqueue(6,6,1,"6")
                call list%enqueue(1,0,2,"Hola")
                call list%enqueue(2,2,1,"Mundo")
                call list%enqueue(3,3,3,"1")
                call list%enqueue(4,4,4,"FIN")
                call list%enqueue(5,5,5,"5")
                call list%enqueue(6,6,6,"6")
                call list%print()

                call list%dequeue(temp)
                print *, temp%name
                call list%print()

            case('2')
                print *, "¿Cuantas ventanillas quiere crear?"
                read(*, *) ventanillas
                print *, "-------------------------------------------------------------------"
            case('3')
                print *, "-------------------------------------------------------------------"
                contador_pasos= contador_pasos +1
                print *, "EJECUTANDO PASO...", contador_pasos
                print *, "-------------------------------------------------------------------"
                call list%top_info(id_value, img_pf, img_pg)
                call list%get_top_name(name_p)
                !VERIFICACION
                print *, "ID DEL CLIENTE A ENTRAR A LA VENTANILLA:", id_value
            
                ! Ver las ventanas
                call lista_v%get_count(numItems)
                if (numItems < ventanillas) then
                    ! AÑADIR A VENTANILLAS
                    call lista_v%append(name_p, id_value, img_pf, img_pg)
                    call list%dequeue(temp)            
            
                    call lista_v%get_count(numItems)
            
                else
                    print *, "-------------------ventanillas llenas-------------------"
                endif
            
                !IMPRESIONES, TIEMPO DE ESPERA EN VENTANILLA
                call lista_v%update_img_pg()

                !PASA A IMPRIMIR
                call lista_v%remove_and_enqueue_to_impresora(impresora)
                print *, "----------------------PASO TERMINADO------------------------"
            
            case('4')
                print *, "ESTADO DE LAS ESTRUCTURAS DE DATOS:"
                print *, "VENTANAS"
                call lista_v%print()
                call lista_v%get_count(numItems)
                print *, "COLA:"
                call list%print()
                print *, "impresora"
                call impresora%print()
            case('5')
                print *, "REPORTES DE CLIENTES:"
            case('6')
                print *, "----------------------------------------------"
                print *, "NATALIA MARIEL CALDERON ECHEVERRIA "
                print *, "202200007"
                print *, "----------------------------------------------"
            case('7')
                ! salir
                exit
            case default
                print *, "ERROR - OPCION INVALIDA"
        end select
    end do

end program Menu