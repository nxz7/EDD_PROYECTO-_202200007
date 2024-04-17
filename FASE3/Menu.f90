program Menu
    implicit none
    character(len=20) :: username, password
    integer :: choice_menu, choice_sub_menu


    print *, "nombre - credencial principal:"
    read(*,*) username
    print *, "contraseña - credencial principañ:"
    read(*,*) password

    ! Check username and password
    if (trim(username) == "edd" .and. trim(password) == "2024") then
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
                    call sucursales_menu()
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
                    ! Add your code here for loading sucursales
                case (2)
                    print *, "Cargar rutas"
                    ! Add your code here for loading rutas
                case (3)
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
                case (2)
                    print *, "2. Generar ruta optima - GRAFOS"
                case (3)
                    print *, "3. Informacion sobre tecnico de la sucursal"
                case (4)
                    print *, "4. Listado de tecnicos de la sucursal"
                case (5)
                    print *, "5. TOP 5 TECNICOS - info de costos y ganancias del recorrdio"
                case (6)
                    print *, "6. GRAFO DE TABLA HASH"
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
            print *, "*********Sucursales Menu********"
            print *, "1. ARBOL DE MERCKLE"
            print *, "2. BLOCK -CHAIN "
            print *, "3. SUCURSALES Y SUS RUTAS"
            print *, "4. TABLA HASH?"
            print *, "5. TOP 5 SUCURSALES CON MAS TRABAJOS SOLICITADOS"
            print *, "6. GANANCIAS, COSTOS Y GANANCIAS TOTALES"
            print *, "0. Exit"
            print *, "*******************************"
            print *, ">>> seleccione:"
            read(*,*) choice

            select case(choice)
                case (1)
                    print *, "1. Cargar tecnicos"
                case (2)
                    print *, "2. Generar ruta optima - GRAFOS"
                case (3)
                    print *, "3. Informacion sobre tecnico de la sucursal"
                case (4)
                    print *, "4. Listado de tecnicos de la sucursal"
                case (5)
                    print *, "5. TOP 5 TECNICOS - info de costos y ganancias del recorrdio"
                case (6)
                    print *, "6. GRAFO DE TABLA HASH"
                case (0)
                    exit
                case default
                    print *, "SELECCION FUERA DEL RANGO."
            end select
        end do
    end subroutine reportes_menu

end program Menu
