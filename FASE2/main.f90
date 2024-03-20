subroutine json_capas(filename)
    use json_module
    implicit none

    character(len=*), intent(in) :: filename

    type(json_file) :: json   ! Declare a variable of type json_file
    type(json_value), pointer :: listPointer, layerPointer, pixelsPointer, pixelPointer, attributePointer  ! Declare pointers to json_value variables
    type(json_core) :: jsonc  ! Declare a variable of type json_core to access basic JSON functions
    character(:), allocatable :: color, fila, columna  ! Declare character strings to be allocated dynamically

    integer :: i, j, size, id_capa        ! Declare integer variables
    logical :: found
    logical :: id_found

    call json%initialize()    ! Initialize the JSON module
    call json%load(filename=filename)  ! Load the JSON file
    !call json%print()         ! Print the content of the JSON file (optional)
    
    call json%info('',n_children=size)

    call json%get_core(jsonc)               ! Get the JSON core to access its basic functions
    call json%get('', listPointer, found)

    do i = 1, size                          ! Loop over the number of elements in the JSON
        id_found = .false.                  ! Reset id_found flag for each layer
        call jsonc%get_child(listPointer, i, layerPointer, found = found)  ! Get the i-th child of listPointer
        call jsonc%get_child(layerPointer, 'id_capa', attributePointer, found = found)  ! Get the value associated with the key 'id_capa'

        if (found) then                      ! If the value associated with the key 'id_capa' is found
            call jsonc%get(attributePointer, id_capa)  ! Get the value and assign it to the variable 'id_capa'
            id_found = .true.                ! Set id_found to true
            print *, 'id_capa:', id_capa     ! Print the id_capa
        end if
        
        call jsonc%get_child(layerPointer, 'pixeles', pixelsPointer, found = found) ! Get the value associated with the key 'pixeles'
        
        if (found) then                      ! If the value associated with the key 'pixeles' is found
            j = 0 ! Initialize pixel index
            
            do while (.true.) ! Loop until all pixels are processed
                j = j + 1
                
                call jsonc%get_child(pixelsPointer, j, pixelPointer, found = found) ! Get the j-th pixel element
                
                if (.not. found) exit ! If pixel not found, exit loop
                
                call jsonc%get_child(pixelPointer, 'color', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, color) 
                    print *, 'color:', trim(color)        
                end if
                
                call jsonc%get_child(pixelPointer, 'fila', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, fila)
                    print *, 'fila:', trim(fila)          
                end if
                
                call jsonc%get_child(pixelPointer, 'columna', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, columna) 
                    print *, 'columna:', trim(columna)      
                end if
            end do
        end if
        
        if (id_found) then                    
            print *, 'id_capa:', id_capa     
        end if
    end do

    call json%destroy()                    
end subroutine json_capas

subroutine json_imagenes(filename)
        use json_module
        implicit none
    
        character(len=*), intent(in) :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, itemPointer, idPointer, capasPointer, capaPointer
        type(json_core) :: jsonc
        integer :: i, j, size, id, capa, capas_size
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
                        call jsonc%get(idPointer, id)
                        print *, 'id', id
                    end if
    
                    call jsonc%get_child(itemPointer, 'capas', capasPointer, found)
    
                    if (found) then
                        call jsonc%info(capasPointer, n_children=capas_size)
    
                        do j = 1, capas_size
                            call jsonc%get_child(capasPointer, j, capaPointer, found)
    
                            if (found) then
                                call jsonc%get(capaPointer, capa)
                                print *, capa
                            end if
                        end do
    
                        print *, 'id', id
                    end if
                end if
            end do
        end if
    
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
    implicit none
    character(len=255) :: filename, file_imagenes, file_album, file_cliente
    print *, "capas:"
    read(*,*) filename
    call json_capas(trim(filename))
    print *, "imagenes:"
    read(*,*) file_imagenes
    call json_imagenes(trim(file_imagenes))
    print *, "album:"
    read(*,*) file_album
    call json_album(trim(file_album))
    print *, "cliente:"
    read(*,*) file_cliente
    call json_cliente(trim(file_cliente))
end program main
