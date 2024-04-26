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


subroutine json_sucursal(filename, suc, bstA, mk_sucur)
    use json_module
    use suc
    use bst_tree
    use mk_inf
    use sha256_module

        implicit none
    
        type(sucursal), intent(inout) ::  suc
        type(mk_suc), intent(inout) :: mk_sucur
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
                passwordL=sha256(passwordL)         
            end if
    
            call suc%append(0,idL,0,0)
            call mk_sucur%append(idL,departamentoL, direccionL, passwordL)
            call bstA%add(idL,departamentoL, direccionL, passwordL)
        end do
    
        call json%destroy()  
    
end subroutine json_sucursal



subroutine json_rutas(filename, adjacency_list)
    use json_module
    use ruta_grafo

    implicit none

    character(len=*), intent(in) :: filename

    type(json_file) :: json   
    type(json_value), pointer :: listPointer, layerPointer, pixelsPointer, pixelPointer, attributePointer  
    type(json_core) :: jsonc
    
    type(adyacencia), intent(inout) :: adjacency_list

    integer :: i, j, size, s1, s2, distancia, imp_mantenimiento      
    logical :: found
    logical :: id_found

    call json%initialize()    
    call json%load(filename=filename)  

    ! Print JSON information for debugging
    call json%info('', n_children=size)
    !print *, "Total :", size

    call json%get_core(jsonc)              
    call json%get('', listPointer, found)

    ! Check if 'grafo' field is found
    call jsonc%get_child(listPointer, "grafo", pixelsPointer, found=found) 
    !print *, "'grafo' -", found

    do i = 1, size                          
                        
        call jsonc%get_child(listPointer, i, layerPointer, found=found)  
        !print *, "Layer", i, "-", found

        if (found) then                      
            j = 0 
            do while (.true.) 
                j = j + 1
                call jsonc%get_child(pixelsPointer, j, pixelPointer, found=found) 
                !print *, "Pixel", j, "-", found

                if (.not. found) exit 
                
                call jsonc%get_child(pixelPointer, 's1', attributePointer, found=found) 
                if (found) then
                    call jsonc%get(attributePointer, s1)    
                end if
                !print *, "s1:", found

                call jsonc%get_child(pixelPointer, 's2', attributePointer, found=found) 
                if (found) then
                    call jsonc%get(attributePointer, s2)       
                end if
                !print *, "s2 :", found

                call jsonc%get_child(pixelPointer, 'distancia', attributePointer, found=found) 
                if (found) then
                    call jsonc%get(attributePointer, distancia)    
                end if
                !print *, "distancia :", found

                call jsonc%get_child(pixelPointer, 'imp_mantenimiento', attributePointer, found=found) 
                if (found) then
                    call jsonc%get(attributePointer, imp_mantenimiento)    
                end if
                !print *, "imp_mantenimiento :", found

                call adjacency_list%insert(s1, s2, distancia, imp_mantenimiento)
                
            end do
        end if
    end do

    call json%destroy()                    
end subroutine json_rutas




program Menu

!--------------declaraciones ----------------
	use bst_tree
    use hash_table_m
    use L_tecnico
    use suc
    use ruta_grafo
    use mk_inf
    use sha256_module
use merkle_tree

    implicit none

    type(tec) :: tecnicos
    type(HashTable) :: table
    type(sucursal) ::  suc
    type(bst)  :: bstA
    type(adyacencia):: adjacency_list
    type(mk_suc):: mk_sucur
    type(merkle) :: merkle_arb

!---------------------------------------------

    character(len=100) :: username, password, id_password
    integer :: choice_menu, choice_sub_menu, dpi_tecnico
    !-----------------mk
    character(len=100), allocatable :: merged_result(:)

    !-------------rutas
integer :: llega_s, sale_s, tecnico_s, distancia_s, mantenimiento_s,total_rutas, contador
    !---------
    integer, dimension(:), allocatable :: path_array, path_man
    !entrar a sucursal----------
    integer :: id_sucursal,unit
    integer :: total_ganancias, total_costos, totalTotal
    character(len=100) :: pass_sucursal
    logical :: found_suc
    integer :: id_p
    integer :: unit_7
!------------------------------
    character(:), allocatable ::  sha, dato_merkle, usersha, passwordsha,ppsha_user, ppsha_pass
    character(len=:), allocatable :: val
!--------------------------------    
    character(len=100) :: filenameSucursal, filenameTecnico,sucursarlBst,TablaHash, filenameRutas
unit=0
    sucursarlBst = "sucursales_bst.dot"
    TablaHash = "tabla_hash.dot"

    print *, "nombre - credencial principal:"
    read(*,*) username
    usersha=sha256(username)
    print *, "contraseña - credencial principal:"
    read(*,*) password
    passwordsha=sha256(password)
    !print *, "SHA256 de credenciales:"
    !print *, "usuario:", usersha
    !print *, "password:", passwordsha
    ppsha_user="8BC9A3846746B37F15990191FA8186567353BE0A05427B87C15F32D92F5FA48D"
    ppsha_pass="6187B89B36C6570624E24D9CAB8C9FA43B176FE02C781C8587556F0D3F4AA430"
    ! EDD1S2024 >>>>>>>>> ProyectoFase3
    if (trim(usersha) == ppsha_user .and. trim(passwordsha) == ppsha_pass) then
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
                    id_password=sha256(id_password)
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
                    call json_sucursal(filenameSucursal, suc, bstA,mk_sucur)
                    print *, ">>>>>>>>>>>>>>>>>>>>>"
                    print *, "sucursales:"
                    call mk_sucur%print()
                    print *, ">>>>>>>>>>>>>>>>>>>>>"
                    call mk_sucur%write_to_json()

                    open(unit, file=sucursarlBst, status='replace')	
                    print *, 'generando grafico de sucursales BST...'
                    call bstA%dotgen(bstA%root, unit)
                    close(unit)
                    print *, 'grafico de sucursales BST:', trim(sucursarlBst)
                    call execute_command_line('dot -Tsvg sucursales_bst.dot > sucursales_bst.svg')
                    call execute_command_line('start sucursales_bst.svg')

                case (2)
                    print *, "Cargar rutas"
                    print *, "ingrese nombre del archivo:"
                    read(*,*) filenameRutas
                    call json_rutas(filenameRutas, adjacency_list)
                    print *, "sucursales y conexiones:"
                    call adjacency_list%printList()
                    call adjacency_list%write_routes_to_json()
                    print *, "*******************************"
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
                    print *, "ingrese dpi del tecnico:"
                    read(*,*) tecnico_s
                    print *, "ingrese id sucursal LLEGADA:"
                    read(*,*) llega_s
                    print *, "ingrese id sucursal SALIDA:"
                    read(*,*) sale_s
                    call tecnicos%increment_value(tecnico_s, id_sucursal)
                    call suc%add_value(id_sucursal)
                    print *,"trabajo asignado"

                    


                    call adjacency_list%shortestDistance(sale_s, llega_s, path_array, distancia_s, mantenimiento_s,contador)
                    print *, "-----------INFORMACION DEL RECORRIDO OPTIMO---------------"
                    print *, "(gastos)distancia: Q", distancia_s*80
                    print *, "(ganancia) mantenimiento: Q", mantenimiento_s*100
                    print *, "total ganancias: Q", mantenimiento_s*100-distancia_s*80
                    total_rutas=mantenimiento_s*100-distancia_s*80
                    !print *, "Se ha generado la imagen del grafo."
                    print *, "*************> generando grafico de ruta"
                    print *, "--<distancia_min.png>----> grafo del recorrido."
                    call adjacency_list%graphWithColor(path_array)
                    print *, path_array
                    print *, size(path_array)
                    print *, "*************> las cadenas:"
                    call mk_sucur%merge_nodes(path_array, merged_result)
                    print *, merged_result

                    deallocate(path_array)

                    print *, "------------------------------------------------------------"

                    call suc%add_costos(id_sucursal,distancia_s*80)
                    call suc%add_ganancias(id_sucursal,mantenimiento_s*100)


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
                    call tecnicos%bubble_sort()
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
                    !arbooooooooooooooool merckle
                    print *, ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ", contador
                    print *, 'MERKLE:'
                    
                    do id = 1, contador-1
                        if (len_trim(merged_result(id)) > 0 .and. merged_result(id) /= '0') then
                            print *, merged_result(id)
                            val = merged_result(id)
                            call merkle_arb%add(val)
                        endif
                    end do
                    print *, ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> "

                    open(unit_7, file="merkle_graph.dot", status='replace')	
                    call merkle_arb%generate()
                    print *, 'GENERANDO ARBOL MERKLE'
                        call merkle_arb%dotgen_merkle(unit_7)
                        close(unit_7)
                    call execute_command_line('dot -Tsvg merkle_graph.dot > merkle_graph.svg')
                    call execute_command_line('start merkle_graph.svg')
                
                    ! Deallocate input array
                    deallocate(merged_result)

                case (2)
                    print *, "2. BLOCKCHAIN"
                    !blockchain
                case (3)
                    print *, "3. SUCURSALES Y SUS RUTAS - grafo y arbol B"
                    print *, "--<graph.svg>----> mapa de sucursales"
                    !-------------sucursales---------------
                    call adjacency_list%graph()
                    !------------rutas  grafo
                    !print *, "--<distancia_min.svg>----> grafo del recorrido."
                    !call adjacency_list%graphWithColor(path_array)

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
                    print *, "5. TOP 5 TECNICOS "
                    call tecnicos%bubble_sort()
                    call tecnicos%print_5(id_sucursal)
                case (6)
                    print *, "6. TOP 5 SUCURSALES - GANANCIAS, COSTOS Y GANANCIAS TOTALES"

                    call suc%bubble_sort()
                    call suc%print()
                    call suc%sum_ganancias(total_ganancias)
                    call suc%sum_costos(total_costos)
                    
                    ! Print the results
                    print *,"*************Reportes****dinero***********"
                    print *, 'Total Ganancias: Q', total_ganancias
                    print *, 'Total Costos: Q', total_costos
                    totalTotal=total_ganancias-total_costos
                    print *, 'Ganancias TOTALES: Q', totalTotal
                    print *,"******************************************"
                case (0)
                    exit
                case default
                    print *, "SELECCION FUERA DEL RANGO."
            end select
        end do
    end subroutine reportes_menu

end program Menu
