program Menu
	use avldef
	use lista_avl
	use lista_cliente_img
	!---binario--------
	use bstdef
    use lista_cliente_capas
    use matrix_spar
    use cola_module
	!-------------

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
	type(matrix) :: matriz1,matriz2,matrizfin,matriz_preorden,matriz_recorrido,matriz3,matriz_postorden
	type(matrix) :: matriz_inorden
	type(cola) :: cola_prez, cola_posto, cola_inor
	!-----------------

	character(len=1) :: opcion, choice_images, sub_choice
    character(len=100) :: username, password
	logical :: loggedIn, encontrado,exit_images_menu, primer_recorrido
	integer :: unit
	character(len=100) :: filename,filebinario
	integer :: valor_rec

	!----------------avl-imagenes------------------------
	integer :: pr_id, recorrido
	!pruebasssssss	
	pr_id=5
	filename = 'output.dot'
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
									call matriz1%insert(3, 0, "BLUE")
									call matriz1%insert(3, 1, "BLUE")  
									call matriz1%insert(3, 2, "RED")
									call matriz1%insert(3, 3, "BLUE")
									call matriz1%insert(3, 4, "GREEN")
									call matriz1%insert(3, 5, "RED")
									call matriz1%insert(3, 6, "BLUE")
									call matriz1%print()
									print *, '1'			
									call matriz2%insert(0, 4, "BLACK")
									call matriz2%insert(1, 4, "BLACK")  
									call matriz2%insert(2, 4, "BLACK")
									call matriz2%insert(3, 4, "BLACK")
									call matriz2%insert(4, 4, "BLACK")
									call matriz2%insert(5, 4, "BLACK")
									print *, '2'
									call matriz3%insert(0, 0, "ORANGE")
									call matriz3%insert(1, 1, "ORANGE")  
									call matriz3%insert(2, 2, "ORANGE")
									call matriz3%insert(3, 3, "ORANGE")
									call matriz3%insert(4, 4, "ORANGE")
									call matriz3%insert(6, 6, "ORANGE")
									print *, '3'
									!--------AGREGARLAS AL ARBOL BINARIO
									call binario_tree%add(25,username, matriz2)
									call binario_tree%add(10,username, matriz1)
									call binario_tree%add(35,username,matriz1)
									call binario_tree%add(5,username,matriz1)
									call binario_tree%add(20,username,matriz3)
									call binario_tree%add(30,username,matriz2)
									call binario_tree%add(40,username,matriz1)
									call lista_c%push(username,binario_tree)
									print *, 'capas agregadas"'

									call matriz1%clear_matrix()
									call matriz2%clear_matrix()
									call matriz3%clear_matrix()
									
									!call binario_tree%inorder(binario_tree%root)
								case('2')
									print *, "CARGA MASIVA DE IMAGENES"
									call lista%push(1)
									call lista%push(2)
									call lista%push(3)	
									call listad%push(4)
									call listad%push(5)
									call listad%push(4)
									call listat%push(7)
									call listat%push(6)
									call listat%push(5)
									call avl_t%add(pr_id,username, lista)
									call avl_t%add(10,username, listad)
									call avl_t%add(15,username, listat)
									call avl_t%add(20, username, lista)
									call avl_t%add(25,username, listad)
									call avl_t%add(30,username, listat)
									print *, 'imagenes agregadas'
									!call avl_t%preorder(avl_t%root)
									call image_list%push(username, avl_t)
									pr_id = pr_id + 8
									
								case('3')
									print *, "CARGA MASIVA DE ALBUMES"
								case('4')
									print *, "VISUALIZAR ESTRUCTURAS"
									!--------------avl-imagenes------------------------
									open(unit, file=filename, status='replace')
									call image_list%search(username, avl_tree2)
									call avl_tree2%preorder(avl_tree2%root)
									!call avl_tree2%get_list_by_value(30,res)
									!call res%print()
									print *, 'generando representacion de avl...'
									call avl_tree2%dotgen(avl_tree2%root, unit)
									close(unit)
									print *, 'dot file con nombre:', trim(filename)
									call execute_command_line('dot -Tsvg output.dot > output.svg')
									call execute_command_line('start output.svg')
									!----------------------------------------------------

									!---------- visualizar arbol b- capas--------------
									open(unit, file=filebinario, status='replace')
									call lista_c%search(username, bin_graficar)
									print *, 'generando arbol binario de capas...'
									call bin_graficar%dotgen(bin_graficar%root, unit)
									close(unit)
									print *, 'Dot file con nombre:', trim(filebinario)
									call execute_command_line('dot -Tsvg arbol_binario.dot > arbol_binario.svg')
									call execute_command_line('start arbol_binario.svg')
									!--------------------------------------------------
									!call cola_prez%print()
									!visualizar capas -----------------------------
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
														call lista_c%search(username, binario_rec)
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
														call lista_c%search(username, binario_post)
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
														call lista_c%search(username, binario_in)
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
												call lista_c%search(username, binario_cap)
												!busca el numero de capa y su matriz
												
												!call matriz3%print()
												call binario_cap%search_by_value(35,matrizfin)
												print *, "--------------"
												call matrizfin%print()
												!call matrizfin%unir_matrix(matriz3)
												print *, "--------------"
												call matrizfin%print()
												call matrizfin%graficar('imagen.dot')
												call execute_command_line('dot -Tsvg imagen.dot > imagen.svg')
												call execute_command_line('start imagen.svg')
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
								case('7')
									print *, "salir del MODO CLIENTE..."
									call avl_t%avl_clear()
									print *, "1"
									call avl_tree2%avl_clear()
									print *, "2"
									call binario_cap%clear_binario()
									call binario_rec%clear_binario()
									call binario_post%clear_binario()
									call binario_in%clear_binario()
									
									print *, "3"
									call bin_graficar%clear_binario()
									print *, "4"
									call binario_tree%clear_binario()
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