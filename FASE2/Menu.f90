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
    character(len=100) :: username, password
	logical :: loggedIn, encontrado,exit_images_menu, primer_recorrido
	integer :: unit,num_capa
	character(len=100) :: filename,filebinario
	integer :: valor_rec

	!----------------avl-imagenes------------------------
	integer :: pr_id, recorrido, itera, numero_capa
	!pruebasssssss	
	pr_id=5
	filename = 'avl_arb.dot'
	filebinario='arbol_binario.dot'
	unit = 1

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
				print *, "Ingrese el nombre de usuario: "
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
								case('2')
									print *, "ARBOL B - CLIENTES"
								case('3')
									print *, "REPORTES"
								case('4')
									print *, "INSERTAR CLIENTE"
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
					print *, username
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

			case('3')
				! salir
				exit
			case default
				print *, "ERROR - OPCION INVALIDA"
		end select

	end do

end program Menu