subroutine json_capas(filename, bst_arbol, bst_matrix, username)
    use json_module
    use bst_tree
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
            call bst_matrix%print()
            call bst_matrix%clear_matrix() 
        end if
    end do

    call json%destroy()                    
end subroutine json_capas

subroutine json_imagenes(filename, avl_arbol, lista_avl, username)
        use json_module
        use avl_tree
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
                        call lista_avl%print()
                        lista_avl_2=lista_avl
                        call avl_arbol%add(id_avl,username, lista_avl_2)
                        print *, '----PRUEBA------'
                        call lista_avl_2%print()
                        call lista_avl%clear()
                        print *, 'id', id_json
                    end if
                end if
            end do
        end if
        !call image_list%push(username, avl_arbol)
        call json%destroy()
end subroutine json_imagenes

subroutine json_album(filename)
    use json_module
    implicit none

    character(len=*), intent(in) :: filename
    type(json_file) :: json
    type(json_value), pointer :: listPointer, itemPointer, idPointer, capasPointer, capaPointer
    type(json_core) :: jsonc
    character(:), allocatable :: id
    integer :: i, j, size, img, capas_size
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
                        end if
                    end do

                    print *, 'nombre_album', id
                end if
            end if
        end do
    end if

    call json%destroy()
end subroutine json_album

subroutine json_cliente(filename)
    use json_module
    implicit none

    character(len=*), intent(in) :: filename
    type(json_file) :: json
    type(json_value), pointer :: listPointer, itemPointer, dpiPointer, nombrePointer, passwordPointer
    type(json_core) :: jsonc
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
                call jsonc%get_child(itemPointer, 'dpi', dpiPointer, found)
                if (found) then
                    call jsonc%get(dpiPointer, dpi)
                    read(dpi, *) dpi_int
                    print *, 'dpi:', trim(dpi), ', dpi_int:', dpi_int
                end if

                call jsonc%get_child(itemPointer, 'nombre_cliente', nombrePointer, found)
                if (found) then
                    call jsonc%get(nombrePointer, nombre)
                    print *, 'nombre_cliente:', trim(nombre)
                end if

                call jsonc%get_child(itemPointer, 'password', passwordPointer, found)
                if (found) then
                    call jsonc%get(passwordPointer, password)
                    print *, 'password:', trim(password)
                end if
            end if
        end do
    end if

    call json%destroy()
end subroutine json_cliente


program main
    use avl_tree
    use lista_avl
    use lista_cliente_img
    use bst_tree
    use matrix_spar
    use lista_cliente_capas
    
    implicit none
    type(avl)::  avl_arbol_m, avl_tree2
    type(linked_list):: lista_avl_m, res
    type(lista_img) :: image_list_m
    type(bst) ::  bst_arbol_m
    type(matrix) :: bst_matrix_m
    type(lista_capas):: lista_capas_m
    character(len=255) ::username_m
    integer :: unit

    character(len=255) :: file_capas, file_imagenes, file_album, file_cliente
    username_m = "user1"
    unit=1

    !us2 = "natalia"


    print *, "capas:"
    read(*,*) filename
    call json_capas(trim(filename),bst_arbol_m, bst_matrix_m, username_m)
    call lista_capas_m%push(username_m,bst_arbol_m)
	
	!call bst_arbol_m%clear_binario()

    print *, "imagenes:"
    read(*,*) file_imagenes
    call json_imagenes(trim(file_imagenes), avl_arbol_m, lista_avl_m, username_m)
    call image_list_m%push(username_m, avl_arbol_m)

    call image_list_m%print()
    !call avl_arbol_m%avl_clear()

    print *, "album:"
    read(*,*) file_album
    call json_album(trim(file_album))
    print *, "cliente:"
    read(*,*) file_cliente
    call json_cliente(trim(file_cliente))
    

    open(unit, file='prueba.dot', status='replace')
    call image_list_m%search(username_m, avl_tree2)
    call avl_tree2%preorder(avl_tree2%root)
    !call avl_tree2%get_list_by_value(7,res)
    !call res%print()
    print *, 'generando representacion de avl...'
    call avl_tree2%dotgen(avl_tree2%root, unit)
    close(unit)
    print *, 'generado'
    call execute_command_line('dot -Tsvg prueba.dot > prueba.svg')
    call execute_command_line('start prueba.svg')

end program main
