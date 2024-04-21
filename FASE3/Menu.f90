subroutine json_tecnico(filename, tecnicos, table, sucursal)
    use json_module
    use L_tecnico
    use hash_table_m
        implicit none
    
        type(tec), intent(inout) ::  tecnicos
        type(HashTable), intent(inout)  :: table
        character(len=*), intent(in) :: filename
        integer, intent(in) :: sucursal
        
        type(json_file) :: json   
        type(json_value), pointer :: listPointer, personPointer, attributePointer  
        type(json_core) :: jsonc  
        character(:), allocatable ::  nameL, apellidoL,generoL, direccionL
        integer :: dpiL, telefonoL

        integer, parameter :: long = selected_int_kind(18)
        integer :: i, size        
        logical :: found
    
        call json%initialize()    
        call json%load(filename=filename)  
        !call json%print()         
        
        call json%info('',n_children=size)
    
        call json%get_core(jsonc)               
        call json%get('', listPointer, found)
    
        do i = 1, size                          
            call jsonc%get_child(listPointer, i, personPointer, found = found)  
            call jsonc%get_child(personPointer, 'dpi', attributePointer, found = found)                     
            if (found) then                     
                call jsonc%get(attributePointer, dpiL)         
            end if
            call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)                     
            if (found) then                      
                call jsonc%get(attributePointer, nameL)            
            end if
    
            call jsonc%get_child(personPointer, 'apellido', attributePointer, found = found)                     
            if (found) then                      
                call jsonc%get(attributePointer, apellidoL)            
            end if
    
            call jsonc%get_child(personPointer, 'genero', attributePointer, found = found)                   
            if (found) then                      
                call jsonc%get(attributePointer, generoL)          
            end if
    
            call jsonc%get_child(personPointer, 'direccion', attributePointer, found = found)                   
            if (found) then                      
                call jsonc%get(attributePointer, direccionL)          
            end if
    
            call jsonc%get_child(personPointer, 'telefono', attributePointer, found = found)                   
            if (found) then                      
                call jsonc%get(attributePointer, telefonoL)          
            end if
            call tecnicos%append(0, dpiL, telefonoL, nameL, apellidoL, generoL, direccionL, sucursal)
            
            call table%insert(int(dpiL, kind=long))
        end do
    
        call json%destroy()  
    
end subroutine json_tecnico


subroutine json_sucursal(filename, suc, bstA)
    use json_module
    use suc
    use bst_tree
        implicit none
    
        type(sucursal), intent(inout) ::  suc
        type(bst), intent(inout)  :: bstA
        character(len=*), intent(in) :: filename
        
        type(json_file) :: json   
        type(json_value), pointer :: listPointer, personPointer, attributePointer  
        type(json_core) :: jsonc  
        character(:), allocatable ::  departamentoL, direccionL, passwordL
        integer :: idL
        integer, parameter :: long = selected_int_kind(18)
        integer :: i, size        
        logical :: found
    
        call json%initialize()    
        call json%load(filename=filename)  
        !call json%print()         
        
        call json%info('',n_children=size)
    
        call json%get_core(jsonc)               
        call json%get('', listPointer, found)
    
        do i = 1, size                          
            call jsonc%get_child(listPointer, i, personPointer, found = found)  
            call jsonc%get_child(personPointer, 'id', attributePointer, found = found)                     
            if (found) then                     
                call jsonc%get(attributePointer, idL)         
            end if
            call jsonc%get_child(personPointer, 'departamento', attributePointer, found = found)                     
            if (found) then                      
                call jsonc%get(attributePointer, departamentoL)            
            end if
    
            call jsonc%get_child(personPointer, 'direccion', attributePointer, found = found)                     
            if (found) then                      
                call jsonc%get(attributePointer, direccionL)            
            end if
    
            call jsonc%get_child(personPointer, 'password', attributePointer, found = found)                   
            if (found) then                      
                call jsonc%get(attributePointer, passwordL)          
            end if
    
            call suc%append(0,idL,0,0)
            
            call bstA%add(idL,departamentoL, direccionL, passwordL)
        end do
    
        call json%destroy()  
    
end subroutine json_sucursal



program Menu

!--------------declaraciones ----------------
	use bst_tree
    use hash_table_m
    use L_tecnico
    use suc
    implicit none

    type(tec) :: tecnicos
    type(HashTable) :: table
    type(sucursal) ::  suc
    type(bst)  :: bstA
    
!---------------------------------------------

    character(len=100) :: username, password, id_password
    integer :: choice_menu, choice_sub_menu, dpi_tecnico

    !entrar a sucursal----------
    integer :: id_sucursal,unit
    integer :: total_ganancias, total_costos, totalTotal
    character(len=100) :: pass_sucursal
    logical :: found_suc
!------------------------------
    character(len=100) :: filenameSucursal, filenameTecnico,sucursarlBst,TablaHash
unit=0
    sucursarlBst = "sucursales_bst.dot"
    TablaHash = "tabla_hash.dot"

    print *, "nombre - credencial principal:"
    read(*,*) username
    print *, "contraseña - credencial principañ:"
    read(*,*) password

    ! Check username and password
    if (trim(username) == "EDD1S2024" .and. trim(password) == "ProyectoFase3") then
        print *, "ESTAS LOGGEADO!"
        ! Display main menu
        do
            print *, "******Menu Principal******"
            print *, "1. Cargar archivos"
            print *, "2. Sucursales"
            print *, "3. Reportes"
            print *, "0. Exit"
            print *, "*************************"
            print *, "elegir:"
            read(*,*) choice_menu

            select case(choice_menu)
                case (1)
                    call cargar_archivos_menu()
                case (2)
                    print *, "ID - SUCURSAL:"
                    read(*,*) id_sucursal
                    print *, "contraseña - SUCURSAL:"
                    read(*,*) id_password
                    call bstA%search_node(id_sucursal,id_password, found_suc)
                    print *, found_suc
                        if (found_suc) then

                            call sucursales_menu()

                        else
                            print *, "Las credenciales de la sucursal no son correctas"
                        end if

                case (3)
                    call reportes_menu()
                case (0)
                    exit
                case default
                    print *, "Seleccione una opcion entra 0 y 3."
            end select
        end do
    else
        print *, "Las credenciales no son correctas"
    end if

contains

    subroutine cargar_archivos_menu()
        integer :: choice

        do
            print *, "****Cargar Archivos Menu****"
            print *, "1. Cargar sucursales"
            print *, "2. Cargar rutas"
            print *, "3. Regresar al menu anterior"
            print *, "0. Exit"
            print *, "****************************"
            print *, ">>> seleccione:"
            read(*,*) choice

            select case(choice)
                case (1)
                    print *, "Cargar sucursales"
                    print *, "ingrese nombre del archivo:"
                    read(*,*) filenameSucursal
                    call json_sucursal(filenameSucursal, suc, bstA)

                    open(unit, file=sucursarlBst, status='replace')	
                    print *, 'generando grafico de sucursales BST...'
                    call bstA%dotgen(bstA%root, unit)
                    close(unit)
                    print *, 'grafico de sucursales BST:', trim(sucursarlBst)
                    call execute_command_line('dot -Tsvg sucursales_bst.dot > sucursales_bst.svg')
                    call execute_command_line('start sucursales_bst.svg')

                case (2)
                    print *, "Cargar rutas"
                    
                case (3)
                    print *, "REGRESANDO AL MENU PRINCIPAL..."
                    exit
                case (0)
                    exit
                case default
                    print *, "SELECCION FUERA DEL RANGO."
            end select
        end do
    end subroutine cargar_archivos_menu

    subroutine sucursales_menu()
        integer :: choice

        do
            print *, "********Sucursales Menu*********"
            print *, "1. Cargar tecnicos"
            print *, "2. Generar ruta optima - GRAFOS"
            print *, "3. Informacion sobre tecnico de la sucursal"
            print *, "4. Listado de tecnicos de la sucursal"
            print *, "5. TOP 5 TECNICOS - info de costos y ganancias del recorrdio"
            print *, "6. GRAFO DE TABLA HASH"
            print *, "0. Exit"
            print *, "*******************************"
            print *, ">>> seleccione:"
            read(*,*) choice

            select case(choice)
                case (1)
                    print *, "1. Cargar tecnicos"
                    print *, "ingrese nombre del archivo:"
                    read(*,*) filenameTecnico
                    call json_tecnico( filenameTecnico, tecnicos,table, id_sucursal)
                case (2)
                    print *, "2. Generar ruta optima - GRAFOS"
!FALTA TODO LO DE GENERAR RUTA MAS OPTIMA --- SUMAR 
                    !prueba
                    call suc%add_value(3)
                    call suc%add_value(4)
                    call suc%add_costos(1,150)
                    call suc%add_costos(3,500)
                    call suc%add_ganancias(5,500)
                    call suc%add_value(1)
                    call suc%add_costos(3,100)
                    call suc%add_ganancias(6,700)

                    !prueba
                case (3)
                    print *, "3. Informacion sobre tecnico de la sucursal"
                    print *, "dpi del tecnico:"
                    read(*,*) dpi_tecnico
                    call tecnicos%find_and_print(dpi_tecnico, id_sucursal)
                case (4)
                    print *, "4. Listado de tecnicos de la sucursal"
                    call tecnicos%print(id_sucursal)
                case (5)
                    print *, "5. TOP 5 TECNICOS - info de costos y ganancias del recorrdio"
                    call tecnicos%print_5(id_sucursal)
                    !FALTA INFO DE COSTOS Y GANANCIAS
                case (6)
                    print *, "6. GRAFO DE TABLA HASH"
                    call table%print()
                    !call table%search(5)
                
                    print *, "GRAFICANDO LA TABLA HASH"
                    call table%grafico(TablaHash)
                    print *, "---> GENERADO "
                    call execute_command_line('dot -Tsvg tabla_hash.dot > tabla_hash.svg')
                    call execute_command_line('start tabla_hash.svg')
                case (0)
                    exit
                case default
                    print *, "SELECCION FUERA DEL RANGO."
            end select
        end do
    end subroutine sucursales_menu

    subroutine reportes_menu()
        integer :: choice

        do
            print *, "*********Reportes Menu********"
            print *, "1. ARBOL DE MERCKLE"
            print *, "2. BLOCK -CHAIN "
            print *, "3. SUCURSALES Y SUS RUTAS - grafo y  arbol B"
            print *, "4. TABLA HASH"
            print *, "5. TOP 5 TECNICOS - info de costos y ganancias del recorrdio"
            print *, "6. TOP 5 SUCURSALES - GANANCIAS, COSTOS Y GANANCIAS TOTALES"
            print *, "0. Exit"
            print *, "*******************************"
            print *, ">>> seleccione:"
            read(*,*) choice

            select case(choice)
                case (1)
                    print *, "1. ARBOL MERCKLE"
                case (2)
                    print *, "2. BLOCKCHAIN"
                case (3)
                    print *, "3. SUCURSALES Y SUS RUTAS - grafo y arbol B"

                    !-------------arbol B de las sucursales
                    open(unit, file=sucursarlBst, status='replace')	
                    print *, 'generando grafico de sucursales BST...'
                    call bstA%dotgen(bstA%root, unit)
                    close(unit)
                    print *, 'grafico de sucursales BST:', trim(sucursarlBst)
                    call execute_command_line('dot -Tsvg sucursales_bst.dot > sucursales_bst.svg')
                    call execute_command_line('start sucursales_bst.svg')
                case (4)
                    print *, "4. TABLA HASH"
                    call table%print()
                    !call table%search(5)
                
                    print *, "GRAFICANDO LA TABLA HASH"
                    call table%grafico(TablaHash)
                    print *, "---> GENERADO "
                    call execute_command_line('dot -Tsvg tabla_hash.dot > tabla_hash.svg')
                    call execute_command_line('start tabla_hash.svg')
                case (5)
                    print *, "5. TOP 5 TECNICOS - info de costos y ganancias del recorrdio"
                case (6)
                    print *, "6. TOP 5 SUCURSALES - GANANCIAS, COSTOS Y GANANCIAS TOTALES"


                    call suc%bubble_sort()
                    call suc%print()
                    call suc%sum_ganancias(total_ganancias)
                    call suc%sum_costos(total_costos)
                    
                    ! Print the results
                    print *,"****************************"
                    print *, 'Total Ganancias:', total_ganancias
                    print *, 'Total Costos:', total_costos
                    totalTotal=total_ganancias-total_costos
                    print *, 'Ganancias TOTALES:', totalTotal
                    print *,"****************************"
                case (0)
                    exit
                case default
                    print *, "SELECCION FUERA DEL RANGO."
            end select
        end do
    end subroutine reportes_menu

end program Menu
