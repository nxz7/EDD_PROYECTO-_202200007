subroutine json_capas(filename, bst_arbol, bst_matrix, username)
    use json_module
    use bstdef
    use matrix_spar

    implicit none

    character(len=*), intent(in) :: filename

    type(json_file) :: json   
    type(json_value), pointer :: listPointer, layerPointer, pixelsPointer, pixelPointer, attributePointer  
    type(json_core) :: jsonc
    
    type(bst), intent(inout) ::  bst_arbol
    type(matrix), intent(inout) :: bst_matrix
    character(len=*), intent(inout) ::username
    character(:), allocatable :: color, color_bst

    integer :: i, j, size, id_capa, fila, columna, fila_bst, columna_bst,id_bst         
    logical :: found
    logical :: id_found

    call json%initialize()    
    call json%load(filename=filename)  
    !call json%print()         
    
    call json%info('',n_children=size)

    call json%get_core(jsonc)              
    call json%get('', listPointer, found)

    do i = 1, size                          
        id_found = .false.                 
        call jsonc%get_child(listPointer, i, layerPointer, found = found)  
        call jsonc%get_child(layerPointer, 'id_capa', attributePointer, found = found)  

        if (found) then                      
            call jsonc%get(attributePointer, id_capa)  
            id_found = .true.                
            print *, 'id_capa:', id_capa     
        end if
        
        call jsonc%get_child(layerPointer, 'pixeles', pixelsPointer, found = found) 
        
        if (found) then                      
            j = 0 
            
            do while (.true.) 
                j = j + 1
                
                call jsonc%get_child(pixelsPointer, j, pixelPointer, found = found) 
                
                if (.not. found) exit 
                
                call jsonc%get_child(pixelPointer, 'color', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, color) 
                    print *, 'color:', trim(color)
                    color_bst = trim(color)        
                end if
                
                call jsonc%get_child(pixelPointer, 'fila', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, fila)
                    print *, 'fila:',fila
                    fila_bst = fila          
                end if
                
                call jsonc%get_child(pixelPointer, 'columna', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, columna) 
                    print *, 'columna:', columna
                    columna_bst = columna      
                end if
                !call matriz1%insert(3, 4, "GREEN")
                call bst_matrix%insert(fila_bst, columna_bst, trim(color_bst))
                
            end do
        end if
        
        if (id_found) then                    
            print *, 'id_capa:', id_capa
            id_bst = id_capa
            call bst_arbol%add(id_bst,username,bst_matrix)
            !call bst_matrix%print()
            call bst_matrix%clear_matrix() 
        end if
    end do

    call json%destroy()                    
end subroutine json_capas

subroutine json_imagenes(filename, avl_arbol, lista_avl, username)
        use json_module
        use avldef
        use lista_avl
        !use lista_cliente_img

        !implicit none
    
        character(len=*), intent(in) :: filename

        type(json_file) :: json
        type(json_value), pointer :: listPointer, itemPointer, idPointer, capasPointer, capaPointer
        type(json_core) :: jsonc

        type(avl), intent(inout) ::  avl_arbol
        type(linked_list), intent(inout) :: lista_avl
        type(linked_list):: lista_avl_2
        !type(lista_img), intent(inout) :: image_list
        character(len=*), intent(inout) ::username

        integer :: i, j, size, id_json, capa, capas_size, capa_avl, id_avl
        logical :: found
    
        call json%initialize()
        call json%load(filename=filename)
    
        call json%get_core(jsonc)
        call json%get('', listPointer, found)
        print *, '--------------------'
        if (found) then
            call json%info('', n_children=size)
    
            do i = 1, size
                call jsonc%get_child(listPointer, i, itemPointer, found)
    
                if (found) then
                    call jsonc%get_child(itemPointer, 'id', idPointer, found)
    
                    if (found) then
                        call jsonc%get(idPointer, id_json)
                        print *, 'id', id_json
                    end if
    
                    call jsonc%get_child(itemPointer, 'capas', capasPointer, found)
    
                    if (found) then
                        call jsonc%info(capasPointer, n_children=capas_size)
    
                        do j = 1, capas_size
                            call jsonc%get_child(capasPointer, j, capaPointer, found)
    
                            if (found) then
                                call jsonc%get(capaPointer, capa)
                                print *, capa
                                capa_avl = capa
                                call lista_avl%push(capa_avl)
                            end if
                        end do
                        id_avl=id_json
                        print *, '------------'
                        !call lista_avl%print()
                        lista_avl_2=lista_avl
                        call avl_arbol%add(id_avl,username, lista_avl_2)
                        print *, '----PRUEBA------'
                        !call lista_avl_2%print()
                        call lista_avl%clear()
                        print *, 'id', id_json
                    end if
                end if
            end do
        end if
        !call image_list%push(username, avl_arbol)
        call json%destroy()
end subroutine json_imagenes

subroutine json_album(filename,lista_album)
    use json_module
    use lista_album
    implicit none

    character(len=*), intent(in) :: filename
    type(json_file) :: json
    type(json_value), pointer :: listPointer, itemPointer, idPointer, capasPointer, capaPointer
    type(json_core) :: jsonc
    character(:), allocatable :: id, id_album
    integer :: i, j, size, img, capas_size, img_album
    type(List_of_list), intent(inout) :: lista_album
    logical :: found

    call json%initialize()
    call json%load(filename=filename)

    call json%get_core(jsonc)
    call json%get('', listPointer, found)
    print *, '--------------------'
    if (found) then
        call json%info('', n_children=size)

        do i = 1, size
            call jsonc%get_child(listPointer, i, itemPointer, found)

            if (found) then
                call jsonc%get_child(itemPointer, 'nombre_album', idPointer, found)

                if (found) then
                    call jsonc%get(idPointer, id)
                    print *, 'nombre_album', id
                end if

                call jsonc%get_child(itemPointer, 'imgs', capasPointer, found)

                if (found) then
                    call jsonc%info(capasPointer, n_children=capas_size)

                    do j = 1, capas_size
                        call jsonc%get_child(capasPointer, j, capaPointer, found)

                        if (found) then
                            call jsonc%get(capaPointer, img)
                            print *, img
                            img_album=img
                            call lista_album%insert( id, img_album)
                        end if
                    end do
                    print *, 'nombre_album', id
                end if
            end if
        !call lista_album%printList()
        end do
    end if

    call json%destroy()
end subroutine json_album

subroutine json_cliente(filename)
    use json_module
    use btree
    use user
    implicit none

    character(len=*), intent(in) :: filename
    type(json_file) :: json
    type(json_value), pointer :: listPointer, itemPointer, dpiPointer, nombrePointer, passwordPointer
    type(json_core) :: jsonc
    type(user_type) :: user

    integer :: i, size, dpi_int
    logical :: found
    character(:), allocatable :: dpi, nombre, password

    call json%initialize()
    call json%load(filename=filename)

    call json%get_core(jsonc)
    call json%get('', listPointer, found)
    !print *, 'Root found:', found

    if (found) then
        call json%info('', n_children=size)
        !print *, 'Number of children:', size
        do i = 1, size

            call jsonc%get_child(listPointer, i, itemPointer, found)
            print *, '-------------------'
            if (found) then
                user = user_type(0,"","") 
                call jsonc%get_child(itemPointer, 'dpi', dpiPointer, found)
                if (found) then
                    call jsonc%get(dpiPointer, dpi)
                    read(dpi, *) dpi_int
                    print *, 'dpi:', trim(dpi), ', dpi_int:', dpi_int
                    user%dpi = dpi_int
                end if

                call jsonc%get_child(itemPointer, 'nombre_cliente', nombrePointer, found)
                if (found) then
                    call jsonc%get(nombrePointer, nombre)
                    print *, 'nombre_cliente:', trim(nombre)
                    user%name = trim(nombre)
                end if

                call jsonc%get_child(itemPointer, 'password', passwordPointer, found)
                if (found) then
                    call jsonc%get(passwordPointer, password)
                    print *, 'password:', trim(password)
                    user%password = trim(password)
                end if
                call insert(user)
                print *, '--------lista-------'
                call lista_clientes(root)
                print *, '---------------'
            end if
            
        end do
        
    end if

    call json%destroy()
end subroutine json_cliente


program Menu
	use avldef
	use lista_avl
	use lista_cliente_img
	!---binario--------
	use bstdef
    use lista_cliente_capas
    use matrix_spar
    use cola_module
	use lista_album
    use lista_cliente_album
	!arbol b de 5---
    use btree
    use user

	!----------

	implicit none
	!-------avl-imagenes----------------
	type(avl) ::  tdos,avl_t, avl_tree2
	type(linked_list) :: lista
	type(linked_list) :: listad
	type(linked_list) :: listat,res

	type(lista_img) :: image_list
	!-------------------------------------
	!----binario-capas-----
	type(bst) :: binario_tree, bin_graficar, binario_cap, binario_rec, binario_post, binario_in
	type(lista_capas):: lista_c
	type(matrix) :: matrizfin,matriz_preorden,matriz_recorrido,matriz_postorden
	type(matrix) :: matriz_inorden
	type(cola) :: cola_prez, cola_posto, cola_inor, reporte_pre, reporte_post, reporte_in
	!-----------------
	!------album-------
	type(List_of_list) :: lista_albumd, segunda
	type(lista_album_img) :: lista_album_imgd
!-------arbolB-clientes

	type(user_type) :: my_user12,buscarUser,my_z,us_encontrado
	!------------
    type(avl)::  avl_arbol_m
    type(linked_list):: lista_avl_m
    type(lista_img) :: image_list_m
    type(bst) ::  bst_arbol_m,binario_esp, binario_reporte
    type(matrix) :: bst_matrix_m,matriz_esparcida, matriz_capa
    type(lista_capas):: lista_capas_m
	!------------------
    character(len=255) :: file_capas, file_imagenes, file_album, file_cliente
	character(len=1) :: opcion, choice_images, sub_choice
    character(len=300) :: username, password, insertar_nombre, insertar_password
	logical :: loggedIn, encontrado,exit_images_menu, primer_recorrido
	!unitnum
	integer :: unit,num_capa,unitNum, buscardpi,insertar_dpi, username_int
	character(len=100) :: filename,filebinario
	integer :: valor_rec
	
	!----------------avl-imagenes------------------------
	integer :: pr_id, recorrido, itera, numero_capa
	!pruebasssssss	
	pr_id=5
	filename = 'avl_arb.dot'
	filebinario='arbol_binario.dot'
	unit = 1
	unitNum = 2

	!----------------------------------------------------

	print *, "-----------------------------------"
	print *, "FASE 2 EDD - 202200007"
	print *, "-----------------------------------"
	print *, "MENU PRINCIPAL"
	print *, "1. INICIO DE SESION"
	print *, "2. REGISTRAR USUARIO"
	print *, "3. Salir"
	print *, "-----------------------------------"
	loggedIn = .false.
	encontrado = .true.
	primer_recorrido = .true.
	do
		print *, "Ingrese una opcion: "
		read(*,*) opcion
		select case(opcion)
			case('1')
				print *, "-----------------------INICIO DE SESION-----------------------"
				print *, "INGRESE DPI -en caso de administrador las credenciales respectivas "
				read(*,*) username
				print *, "Ingrese la contraseña: "
				read(*,*) password
				if (username == 'admin' .and. password == 'EDD2024') then
					loggedIn = .true.
					!----------------- MENU ADMINISTRADOR
					if (loggedIn .eqv. .true.) then
						print *, "-----------------------------------"
						print *, "PROGRAM MENU - ADMINISTRADOR"
						print *, "1. CARGA MASIIVA DE CLIENTES"
						print *, "2. ARBOL B - CLIENTES"
						print *, "3. REPORTES"
						print *, "4. INSERTAR CLIENTE"
						print *, "5. MODIFICAR CLIENTE"
						print *, "6. ELIMINAR CLIENTE"
						print *, "7. salir del modo administrador"
						print *, "-----------------------------------"
						do
							print *, "Ingrese una opcion: "
							read(*,*) opcion
							select case(opcion)
								case('1')
									print *, "CARGA MASIIVA DE CLIENTES"
									print *, "ingrese nombre del archivo:"
									read(*,*) file_cliente
									call json_cliente(trim(file_cliente))
								case('2')
									print *, "ARBOL B - CLIENTES"
									open(unit=unitNum, file='btree.dot', status='replace')
									write(unitNum, '(A)') 'digraph btree {'
									write(unitNum, '(A)') 'node [color=pink, style=filled]'
									call grafo_b(root, unitNum)
									write(unitNum, '(A)') '""} '
									close(unitNum)
									call execute_command_line('dot -Tsvg btree.dot > btree.svg')
									call execute_command_line('start btree.svg')
									print *, "************************"
									print *, "arbol b generado con exito"
									print *, "************************"
								case('3')
									print *, "REPORTES"
									print *, '-----recorrido niveles------'
									call lista_clientes(root)
									print *, '----------------------------'
									print *, "----------------------------"
									print *, "ingrese dpi a buscar--------"
									read(*,*) buscardpi
									buscarUser = searchUser(root, buscardpi) ! dpi INT -buscra lo del dpi que se ingreso
									if (buscarUser%dpi /= -1) then
										print *, "nombre: ", buscarUser%name
										print *, "dpi: ", buscarUser%dpi
										print *, "password: ", buscarUser%password
									else
										print *, "no existe en el sistema."
									end if
									print *, "----------------------------"
								case('4')
									print *, "INSERTAR CLIENTE"
									print *, "ingrese dpi:"
									read(*,*) insertar_dpi
									
									print *, "ingrese nombre:"
									read(*,*) insertar_nombre
									print *, "ingrese contrasena:"
									read(*,*) insertar_password
									my_user12%dpi = insertar_dpi
									my_user12%name = trim(insertar_nombre)
									my_user12%password = trim(insertar_password)
									print *, "*******************************"
									print *, "******usuario ingresado********"
									print *, "nombre: ", my_user12%name
									print *, "dpi: ", my_user12%dpi
									print *, "password: ", my_user12%password
									call insert(my_user12)
									print *, "*******************************"

									print *, "****VISUALIZACION DE ESTRUCTURA MODIFICADA**"
									fileName = 'btree_modificado.dot'
									open(unit=unitNum, file=fileName, status='replace')
									write(unitNum, '(A)') 'digraph btree {'
									write(unitNum, '(A)') 'node [color=yellow, style=filled]'
									call grafo_b_mod(root, unitNum)
									write(unitNum, '(A)') '""} '
									close(unitNum)
									call execute_command_line('dot -Tsvg btree_modificado.dot > btree_modificado.svg')
									call execute_command_line('start btree_modificado.svg')
								

								case('5')
									print *, "MODIFICAR CLIENTE"
								case('6')
									print *, "ELIMINAR CLIENTE"
								case('7')
									print *, "SALIENDO DE MODO ADMINISTRADOR..."
				
									print *, "---> regresando"
									print *, "-----------------------------------"
									print *, "MENU PRINCIPAL"
									print *, "1. INICIO DE SESION"
									print *, "2. REGISTRAR USUARIO"
									print *, "3. Salir"
									print *, "-----------------------------------"
									loggedIn = .false.
									exit
								case default
									print *, "ERROR - OPCION INVALIDA"
							end select
						end do
					end if
					!----------------- MENU ADMINISTRADOR
				else if (.not. loggedIn) then
					print *, "buscando en el sistema"
					read(username, *) username_int
					print *, username_int
					us_encontrado= searchUser(root, username_int)
					!----------comprobar que exista-------
					if (username_int == us_encontrado%dpi .and. password == us_encontrado%password) then
						encontrado = .true.
					else
						encontrado = .false.
					end if
			
					!encontrado = .false.
					if (encontrado .eqv. .true.) then
						print *, "-----------------------------------"
						print *, "PROGRAM MENU - CLIENTE"
						print *, "1. CARGA MASIIVA DE CAPAS"
						print *, "2. CARGA MASIVA DE IMAGENES"
						print *, "3. CARGA MASIVA DE ALBUMES"
						print *, "4. VISUALIZAR ESTRUCTURAS"
						print *, "5. GENERAR IMAGENES"
						print *, "6. REPORTES"
						print *, "7. salir del MODO CLIENTE"
						print *, "-----------------------------------"
						do
							print *, "Ingrese una opcion: "
							read(*,*) opcion
							select case(opcion)
								case('1')
									print *, " CARGA MASIIVA DE CAPAS"
									print *, "capas:"
									read(*,*) file_capas
									call json_capas(trim(file_capas),bst_arbol_m, bst_matrix_m, username)
									call lista_capas_m%push(username,bst_arbol_m)
									
									
									!call binario_tree%inorder(binario_tree%root)
								case('2')
									print *, "CARGA MASIVA DE IMAGENES"
									print *, "imagenes:"
									read(*,*) file_imagenes
									call json_imagenes(trim(file_imagenes), avl_arbol_m, lista_avl_m, username)
									call image_list_m%push(username, avl_arbol_m)
									call avl_arbol_m%inorder(avl_arbol_m%root)
								case('3')
									print *, "CARGA MASIVA DE ALBUMES"
									print *, "album:"
									read(*,*) file_album
									call json_album(trim(file_album),lista_albumd)
									call lista_album_imgd%push(username,lista_albumd)
									print *, username

								case('4')
									print *, "VISUALIZAR ESTRUCTURAS"
									!--------------avl-imagenes------------------------
									print *, "avl----------------------------:"
									open(unit, file=filename, status='replace')
									call image_list_m%search(username, avl_tree2)
									!call avl_tree2%preorder(avl_tree2%root)
									!call avl_tree2%get_list_by_value(30,res)
									!call res%print()
									print *, 'generando representacion de avl...'
									call avl_tree2%dotgen(avl_tree2%root, unit)
									close(unit)
									print *, 'dot file con nombre:', trim(filename)
									call execute_command_line('dot -Tsvg avl_arb.dot > avl_arb.svg')
									call execute_command_line('start avl_arb.svg')
									print *, '...'
									call avl_tree2%avl_clear()
									print *, '...'
									!-------------------ALBUMES----------------------
									call lista_album_imgd%search(username, segunda)
									call segunda%printIndicesFirst("albumest.dot")
									call execute_command_line('dot -Tsvg albumest.dot > albumest.svg')
									call execute_command_line('start albumest.svg')

									!-----------------------------------------
									!---------- visualizar arbol b- capas--------------
									print *, "arbol binario -----------------------------:"
									open(unit, file=filebinario, status='replace')
									call lista_capas_m%search(username, bin_graficar)
									print *, 'generando arbol binario de capas...'
									call bin_graficar%dotgen(bin_graficar%root, unit)
									close(unit)
									print *, 'Dot file con nombre:', trim(filebinario)
									call execute_command_line('dot -Tsvg arbol_binario.dot > arbol_binario.svg')
									call execute_command_line('start arbol_binario.svg')
									!--------------------------------------------------
									!call cola_prez%print()
									!matriz esparcida -----------------------------
									print *, "matriz esparcida -----------------------------:"
									print *, "numer de capa a graficar:"
									read(*,*) num_capa
									call lista_capas_m%search(username, binario_esp)
									call binario_esp%search_by_value(num_capa,matriz_esparcida)
									call matriz_esparcida%print_sparse("matriz_esp.dot")
									call execute_command_line('dot -Tsvg matriz_esp.dot > matriz_esp.svg')
									call execute_command_line('start matriz_esp.svg')
									!-----------------------------------
								case('5')
									print *, "GENERAR IMAGENES"
									print *, "GENERATE IMAGES MENU"
									print *, "-----------------------------------"
									print *, "1. recorrido limitado"
									print *, "2. arbol de imagenes"
									print *, "3. capas"
									print *, "4. salir del menu de imagenes"
									print *, "-----------------------------------"
							
									exit_images_menu = .false.
							
									do while (.not. exit_images_menu)
										print *, "ELEGIR:"
										read(*, *) choice_images
							
										
										select case (choice_images)
											case ('1')
												print *, "--------RECORRIDO LIMITADO-----------"
												print *, "1. PREORDEN"
												print *, "2. POSTORDEN"
												print *, "3. INORDEN"
												print *, "4. SALIR DEL RECORRIDO LIMITADO"
												print *, "-----------------------------------"
												read(*, *) sub_choice
												select case(sub_choice)
													case ('1')
														print *, "---> recorrido limitado"
														print *, "--> PREORDEN"
														print *, "-> ingrese cantidad de capas"
														read(*, *) recorrido
														!recorrido=recorrido+1
														call lista_capas_m%search(username, binario_rec)
														call binario_rec%preorder(binario_rec%root,cola_prez)
														do while (recorrido > 0)
															if (primer_recorrido .eqv. .true.) then
																call cola_prez%delete(valor_rec)
																print *, valor_rec
																call binario_rec%search_by_value(valor_rec,matriz_preorden)
																
																primer_recorrido = .false.
																!call matriz_preorden%print()
																recorrido= recorrido-1
																print *, "primer recorrido"
																print *, recorrido
															else
																!call lista_c%search(username, binario_rec)
																call cola_prez%delete(valor_rec)
																print *, valor_rec
																call binario_rec%search_by_value(valor_rec,matriz_recorrido)
																
																!call matriz_recorrido%print()
																!print *, "---------"
																call matriz_preorden%unir_matrix(matriz_recorrido)
																
																!call matriz_preorden%print()
																call matriz_recorrido%clear_matrix()
																print *, "recorido preorden"
																recorrido= recorrido-1
																print *, recorrido
															endif
														end do
		
														call matriz_preorden%graficar('imagen_preorden.dot')
														call execute_command_line('dot -Tsvg imagen_preorden.dot > imagen_preorden.svg')
														call execute_command_line('start imagen_preorden.svg')
														call cola_prez%cola_clear()
														!call binario_rec%clear_binario()
														primer_recorrido = .true.	
														print *, "-----------------------------------"
														print *, "1. recorrido limitado"
														print *, "2. arbol de imagenes"
														print *, "3. capas"
														print *, "4. salir del menu de imagenes"
														print *, "-----------------------------------"
														call matriz_preorden%clear_matrix()
														print *, "-----------------------------------"
													case ('2')
														print *, "POSTORDEN"
														print *, "---> recorrido limitado"
														print *, "--> POSTORDEN"
														print *, "-> ingrese cantidad de capas"
														read(*, *) recorrido
														!recorrido=recorrido+1
														call lista_capas_m%search(username, binario_post)
														call binario_post%postorder(binario_post%root,cola_posto)
														do while (recorrido > 0)
															if (primer_recorrido .eqv. .true.) then
																call cola_posto%delete(valor_rec)
																print *, valor_rec
																call binario_post%search_by_value(valor_rec, matriz_postorden)
																call matriz_postorden%print()
																primer_recorrido = .false.
																recorrido= recorrido-1
																print *, "primer recorrido"
																print *, recorrido
															else
																
																call cola_posto%delete(valor_rec)
																print *, valor_rec
																call binario_post%search_by_value(valor_rec,matriz_recorrido)
																
																call matriz_postorden%unir_matrix(matriz_recorrido)
																call matriz_postorden%print()
																
																call matriz_recorrido%clear_matrix()
																print *, "recorido preorden"
																recorrido= recorrido-1
																print *, recorrido
															endif
														end do
		
														call matriz_postorden%graficar('imagen_postorden.dot')
														call execute_command_line('dot -Tsvg imagen_postorden.dot > imagen_postorden.svg')
														call execute_command_line('start imagen_postorden.svg')
														call cola_posto%cola_clear()
														!call binario_post%clear_binario()
														primer_recorrido = .true.	
														print *, "-----------------------------------"
														print *, "1. recorrido limitado"
														print *, "2. arbol de imagenes"
														print *, "3. capas"
														print *, "4. salir del menu de imagenes"
														print *, "-----------------------------------"
														call matriz_postorden%clear_matrix()
														print *, "-----------------------------------"
													case ('3')
														print *, "INORDEN"
														
														print *, "---> recorrido limitado"
														print *, "--> INORDEN"
														print *, "-> ingrese cantidad de capas"
														read(*, *) recorrido
														!recorrido=recorrido+1
														call lista_capas_m%search(username, binario_in)
														call binario_in%inorder(binario_in%root,cola_inor)
														do while (recorrido > 0)
															if (primer_recorrido .eqv. .true.) then
																call cola_inor%delete(valor_rec)
																print *, valor_rec
																call binario_in%search_by_value(valor_rec, matriz_inorden)
																
																primer_recorrido = .false.
																recorrido= recorrido-1
																print *, "primer recorrido"
																print *, recorrido
															else
																
																call cola_inor%delete(valor_rec)
																print *, valor_rec
																call binario_in%search_by_value(valor_rec,matriz_recorrido)
																
																call matriz_inorden%unir_matrix(matriz_recorrido)
																
																call matriz_recorrido%clear_matrix()
																print *, "recorido preorden"
																recorrido= recorrido-1
																print *, recorrido
															endif
														end do
		
														call matriz_inorden%graficar('imagen_inorden.dot')
														call execute_command_line('dot -Tsvg imagen_inorden.dot > imagen_inorden.svg')
														call execute_command_line('start imagen_inorden.svg')
														call cola_inor%cola_clear()
														!call binario_int%clear_binario()
														primer_recorrido = .true.	
														print *, "-----------------------------------"
														print *, "1. recorrido limitado"
														print *, "2. arbol de imagenes"
														print *, "3. capas"
														print *, "4. salir del menu de imagenes"
														print *, "-----------------------------------"
														
														call matriz_inorden%clear_matrix()
														print *, "-----------------------------------"	

													case ('4')
														print *, "GENERATE IMAGES MENU"
														print *, "-----------------------------------"
														print *, "1. recorrido limitado"
														print *, "2. arbol de imagenes"
														print *, "3. capas"
														print *, "4. salir del menu de imagenes"
														print *, "-----------------------------------"
														exit
												end select

											case ('2')
												print *, "2. arbol de imagenes"
											case ('3')
												print *, "3. capas"
												print *, "numero de capas que desea graficar"
												read(*, *) recorrido
												print *, recorrido
												itera=recorrido
																			!recorrido=recorrido+1
												call lista_capas_m%search(username, binario_cap)
					
												do while (recorrido > 0)
													if (itera == recorrido) then
														print *, "-> ingrese numero de vcapa"
														read(*, *) numero_capa
														call binario_cap%search_by_value(numero_capa,matrizfin)
														print *, "-------generando imagen-------"
														call matrizfin%graficar('imagen.dot')
														call execute_command_line('dot -Tsvg imagen.dot > imagen.svg')
														call execute_command_line('start imagen.svg')
														recorrido=recorrido-1
														print *, "<<<<<"
														print *, recorrido
													else
														print *, "-> ingrese numero de vcapa"
														read(*, *) numero_capa
														call binario_cap%search_by_value(numero_capa,matriz_capa)
														call matrizfin%unir_matrix(matriz_capa)
															print *, "-----actualizando imagen----"
															call matrizfin%graficar('imagen.dot')
															call execute_command_line('dot -Tsvg imagen.dot > imagen.svg')
															call execute_command_line('start imagen.svg')
															call matriz_capa%clear_matrix()
															print *, "capa agregada --> exitosamente"
															recorrido= recorrido-1
															print *, recorrido
													endif
												end do
												print *, "******** IMAGEN GENERADA POR CAPAS *************"
												call matrizfin%clear_matrix()									
											case ('4')
												print *, "-----------------------------------"
												print *, "PROGRAM MENU - CLIENTE"
												print *, "1. CARGA MASIIVA DE CAPAS"
												print *, "2. CARGA MASIVA DE IMAGENES"
												print *, "3. CARGA MASIVA DE ALBUMES"
												print *, "4. VISUALIZAR ESTRUCTURAS"
												print *, "5. GENERAR IMAGENES"
												print *, "6. REPORTES"
												print *, "7. salir del MODO CLIENTE"
												print *, "-----------------------------------"
												exit_images_menu = .true.
											case default
												print *, "Invalid choice."
										end select
									end do
									!-----------------------------------------------
								case('6')
									print *, "REPORTES"
									print *, "------------------------------"
									print *, "RECORRIDOS DE CAPAS"
									call lista_capas_m%search(username, binario_reporte)
									print *, "---------preorden--------"
									call binario_reporte%preorder(binario_rec%root,reporte_pre)
									call reporte_pre%cola_clear()
									print *, "---------postorden--------"
									call binario_reporte%postorder(binario_rec%root,reporte_post)
									call reporte_post%cola_clear()
									print *, "---------inorden--------"
									call binario_reporte%inorder(binario_rec%root,reporte_in)
									call reporte_in%cola_clear()
									print *, "------------------------------"
								case('7')
									print *, "salir del MODO CLIENTE..."
									call avl_arbol_m%avl_clear()
									print *, "1"
									
									print *, "2"
									call binario_cap%clear_binario()
									print *, "23"
									print *, "23"
									call binario_post%clear_binario()
									print *, "24"
									call binario_in%clear_binario()
									print *, "23"
									call binario_esp%clear_binario()
									print *, "23"
									call binario_reporte%clear_binario()
									print *, "23"
									call binario_rec%clear_binario()
									
									print *, "3"
									call bin_graficar%clear_binario()
									print *, "4"
									call bst_arbol_m%clear_binario()
									print *, "5"

									print *, "---> regresando"
									print *, "-----------------------------------"
									print *, "MENU PRINCIPAL"
									print *, "1. INICIO DE SESION"
									print *, "2. REGISTRAR USUARIO"
									print *, "3. Salir"
									print *, "-----------------------------------"
									!encontrado = .false.
									exit
								case default
									print *, "ERROR - OPCION INVALIDA"
							end select
						end do
					end if

				else
					print *, "Nombre de usuario o contraseña incorrectos."
				end if
				

			case('2')
				print *, "-----------------------REGISTRO DE USUARIOS------------------------"
				print *, "nuevo usuario ---> REGISTRO"
				print *, "ingrese dpi:"
				read(*,*) insertar_dpi
									
				print *, "ingrese nombre:"
				read(*,*) insertar_nombre
				print *, "ingrese contrasena:"
				read(*,*) insertar_password
				my_z%dpi = insertar_dpi
				my_z%name = trim(insertar_nombre)
				my_z%password = trim(insertar_password)
				print *, "*******************************"
				print *, "******usuario ingresado********"
				print *, "nombre: ", my_z%name
				print *, "dpi: ", my_z%dpi
				print *, "password: ", my_z%password
				call insert(my_z)
				print *, "*******************************"

			case('3')
				! salir
				exit
			case default
				print *, "ERROR - OPCION INVALIDA"
		end select

	end do

end program Menu



module btree
    use user
    implicit none

    integer, parameter :: MAXI = 4, MINI = 2

    type nodeDELb
        type(BarbolNode), pointer :: ptr => null()
    end type nodeDELb

    type BarbolNode
        type(user_type), dimension(MAXI+1) :: val
        integer :: num = 0
        type(nodeDELb) :: link(0:MAXI)
    end type BarbolNode

    type(BarbolNode), pointer :: root => null()

contains

    subroutine insert(val)
        type(user_type), intent(in) :: val
        type(BarbolNode), pointer :: child

        allocate(child)
        if (setValue(val, root, child)) then
            root => createNode(val, child)
        end if
    end subroutine insert

    recursive function setValue(val, node, child) result(res)
        type(user_type), intent(in) :: val
        type(BarbolNode), pointer, intent(inout) :: node
        type(BarbolNode), pointer, intent(inout) :: child
        type(BarbolNode), pointer :: newnode        
        integer :: pos
        logical :: res
        
        if (.not. associated(node)) then
            allocate(node)
            node%val(1) = val
            child => null()
            res = .true.
            return
        end if
        
        if (val%dpi < node%val(1)%dpi) then
            pos = 0
        else
            pos = node%num
            do while (val%dpi < node%val(pos)%dpi .and. pos > 1)
                pos = pos - 1
            end do
            if (val%dpi == node%val(pos)%dpi) then
                print *, "cliente ya existe"
                res = .false.
                return
            end if
        end if
        
        if (setValue(val, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call insertNode(val, pos, node, child)
            else
                call splitNode(val, pos, node, child, newnode)
                child => newnode
                res = .true.
                return
            end if
        end if
        res = .false.
    end function setValue

    subroutine insertNode(val, pos, node, child)
        type(user_type), intent(in) :: val
        type(BarbolNode), pointer, intent(inout) :: node
        type(BarbolNode), pointer, intent(in) :: child
        integer :: j,pos
        
        j = node%num
        do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
        end do
        node%val(j + 1) = val
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(val, pos, node, child, newnode)
        type(user_type), intent(in) :: val
        type(BarbolNode), pointer, intent(inout) :: node, newnode
        type(BarbolNode), pointer, intent(in) :: child
        integer :: median, i, j,pos
        
        allocate(newnode)
        if (pos > MINI) then
            median = MINI + 1
        else
            median = MINI
        end if
        j = median + 1
        do while (j <= MAXI)
            newnode%val(j - median) = node%val(j)
            newnode%link(j - median)%ptr => node%link(j)%ptr
            j = j + 1
        end do
        node%num = median
        newnode%num = MAXI - median
        if (pos <= MINI) then
            call insertNode(val, pos, node, child)
        else
            call insertNode(val, pos - median, newnode, child)
        end if        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(val, child) result(newNode)
        type(user_type), intent(in) :: val
        type(BarbolNode), pointer, intent(in) :: child
        type(BarbolNode), pointer :: newNode
        integer :: i
        
        allocate(newNode)
        newNode%val(1) = val
        newNode%num = 1
        newNode%link(0)%ptr => root
        newNode%link(1)%ptr => child
        do i = 2, MAXI
            newNode%link(i)%ptr => null()
        end do
    end function createNode


    recursive subroutine listado_ts(myNode)
    type(BarbolNode), pointer, intent(in) :: myNode
    integer :: i
    
    if (associated(myNode)) then
        write (*, '(A)', advance='no') ' [ '
        i = 0
        !print *, myNode%num
        do while (i < myNode%num)
            write (*,'(A)', advance='no') myNode%val(i+1)%name
            i = i + 1
        end do
        do i = 0, myNode%num
            call listado_ts(myNode%link(i)%ptr)    
        end do
        write (*, '(A)', advance='no') ' ] '
    end if
    
end subroutine listado_ts


recursive subroutine grafo_b(myNode,unit)
type(BarbolNode), pointer, intent(in) :: myNode
integer, intent(in) :: unit
integer :: i
logical :: first_time_entered = .true.
character(len=300) :: concatenated_names = ''

if (associated(myNode)) then

    if (first_time_entered) then
        print *, "primero"
        write (unit, '(A)', advance='no') ' arbolB ->'
    else
        write (unit, '(A,A,A,A)', advance='no') ' ',trim(concatenated_names),'  ',"->"
        write (unit, '(A)', advance='no') '  '
    end if
    

    i = 0
    do while (i < myNode%num)
        if (first_time_entered) then
            concatenated_names = trim(concatenated_names) // myNode%val(i+1)%name //' '
            i=myNode%num
        else

            write (unit, '(A)', advance='no') myNode%val(i+1)%name
        end if
        i = i + 1
    end do

    if (first_time_entered) then
        write (unit, '(A)', advance='no') '   '
        write (unit, '(A)', advance='no') trim(concatenated_names)
        write (unit, '(A)', advance='no') '  '
        first_time_entered = .false.
    end if

    do i = 0, myNode%num
        call grafo_b(myNode%link(i)%ptr,unit)    
    end do
    write (unit, '(A)', advance='no') '  '

end if
!first_time_entered = .true.
end subroutine grafo_b

recursive subroutine lista_clientes(amp)
type(BarbolNode), pointer, intent(in) :: amp
integer :: i

if (associated(amp)) then
    i = 0
    do while (i < amp%num)
        write (*,'(A)', advance='no') amp%val(i+1)%name
        write (*,'(A)', advance='no') ","
        i = i + 1
    end do
    do i = 0, amp%num
        call lista_clientes(amp%link(i)%ptr)    
    end do
end if

end subroutine lista_clientes

recursive function searchUser(node, dpi) result(foundUser)
    type(BarbolNode), pointer, intent(in) :: node
    integer, intent(in) :: dpi
    type(user_type) :: foundUser
    integer :: i, position

    if (associated(node)) then
        position = 1
        do while (position <= node%num .and. dpi > node%val(position)%dpi)
            position = position + 1
        end do

        if (position <= node%num .and. dpi == node%val(position)%dpi) then
            foundUser = node%val(position)
        else
            if (position == 1) then
                foundUser = searchUser(node%link(0)%ptr, dpi)
            else
                foundUser = searchUser(node%link(position - 1)%ptr, dpi)
            end if
        end if
    end if
end function searchUser


recursive subroutine grafo_b_mod(myNode,unit)
type(BarbolNode), pointer, intent(in) :: myNode
integer, intent(in) :: unit
integer :: i
logical :: first_time_entered = .true.
character(len=300) :: concatenated_names = ''

if (associated(myNode)) then

    if (first_time_entered) then
        print *, "primero"
        write (unit, '(A)', advance='no') ' arbolB ->'
    else
        write (unit, '(A,A,A,A)', advance='no') ' ',trim(concatenated_names),' ',"->"
        write (unit, '(A)', advance='no') ' '
    end if
    

    i = 0
    do while (i < myNode%num)
        if (first_time_entered) then
            concatenated_names = trim(concatenated_names) // myNode%val(i+1)%name //' '
            i=myNode%num
        else
            write (unit, '(A)', advance='no') myNode%val(i+1)%name

        end if
        i = i + 1
    end do

    if (first_time_entered) then
        write (unit, '(A)', advance='no') '  '
        write (unit, '(A)', advance='no') trim(concatenated_names)
        write (unit, '(A)', advance='no') ' '
        first_time_entered = .false.
    end if

    do i = 0, myNode%num
        call grafo_b_mod(myNode%link(i)%ptr,unit)    
    end do
    write (unit, '(A)', advance='no') ' '

end if
!first_time_entered = .true.
end subroutine grafo_b_mod
end module btree
