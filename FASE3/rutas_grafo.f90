module ruta_grafo
    implicit none
    private

    type :: sub_node
        integer :: llega, distancia, mantenimiento
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node_sucursal
        integer :: salida
        type(node_sucursal), pointer :: next => null()
        type(node_sucursal), pointer :: prev => null()
        type(sub_node), pointer :: list => null()
    contains
        procedure :: append
        procedure :: print
    end type node_sucursal

    type, public :: adyacencia
        type(node_sucursal), pointer :: head => null()
        type(node_sucursal), pointer :: tail => null()
    contains
        procedure :: insert
        procedure :: printList
        procedure :: graph
        procedure :: shortestDistance
        procedure :: graphWithColor

    end type adyacencia

    contains

    subroutine insert(self, salida, llega, distancia, mantenimiento)
        class(adyacencia), intent(inout) :: self
        integer, intent(in) :: llega
        integer, intent(in) :: salida, distancia, mantenimiento

        type(node_sucursal), pointer :: aux
        type(node_sucursal), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            allocate(aux)
            aux%salida = salida
            self%head => aux
            self%tail => aux
            call aux%append(llega, distancia, mantenimiento)
        else
            if(salida < self%head%salida) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%salida = salida
                call new%append(llega, distancia, mantenimiento)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(salida < aux%next%salida) then
                        if(salida == aux%salida) then
                            call aux%append(llega, distancia, mantenimiento)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%salida = salida
                            call new%append(llega, distancia, mantenimiento)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(salida == aux%salida) then
                    call aux%append(llega, distancia, mantenimiento)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%salida = salida
                    call new%append(llega, distancia, mantenimiento)
                end if
            end if
        end if
    end subroutine insert

    subroutine printList(self)
        class(adyacencia) :: self
        type(node_sucursal), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'sucursal: ', aux%salida
            print *, 'conecta con: '
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    subroutine append(self, string,dist, mant)
        class(node_sucursal), intent(inout) :: self
        integer, intent(in) :: string, dist, mant

        type(sub_node), pointer :: aux
        type(sub_node), pointer :: new

        allocate(new)
        new%llega = string
        new%distancia = dist
        new%mantenimiento = mant

        if(.not. associated(self%list)) then
            self%list => new
        else
            aux => self%list
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => new
        end if
    end subroutine append

    subroutine print(self)
        class(node_sucursal), intent(inout) :: self

        type(sub_node), pointer :: aux
        aux => self%list

        do while(associated(aux))
            print *,"---->" ,aux%llega,"- distancia:", aux%distancia,"- mantenimiento:" ,aux%mantenimiento
            aux => aux%next
        end do
    end subroutine 

    subroutine graph(self)
        class(adyacencia), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: command
        type(node_sucursal), pointer :: current_node
        type(sub_node), pointer :: current_subnode

        !command = "dot -Tpng ./graph.dot -o ./graph.png"
        io = 1

        ! Abrir archivo DOT
        open(newunit=io, file="./graph.dot")
        write(io, *) "digraph G {"

        ! Recorrer la lista principal
        current_node => self%head
        do while(associated(current_node))
            ! la sucursal de donde sale
            write(io, *) current_node%salida, "[label = """, current_node%salida, """]"

            !a donde tiene conexiones
            current_subnode => current_node%list
            do while(associated(current_subnode))
                ! -------------------------
write(io, *) current_node%salida, " -> ", current_subnode%llega, " [label = """, current_subnode%distancia, """, dir = normal];"
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do

        write(io, *) "}"
        close(io)

        ! imagen
        !call execute_command_line(command, exitstat=i)
        call execute_command_line('dot -Tsvg graph.dot > graph.svg')
        call execute_command_line('start graph.svg')

        if (i /= 0) then
            print *, "ERROR"
        else
            print *, "GRAFICO DE LAS RUTAS CREADO."
        end if
    end subroutine graph

    subroutine shortestDistance(self, source, destination, path_array, costo_distancia, costo_mantenimiento,contador)
        class(adyacencia), intent(in) :: self
        integer, intent(in) :: source, destination
        integer, dimension(:), allocatable, intent(inout) :: path_array !todos los nodos que visita
        integer, intent(inout):: contador
        integer, intent(inout) :: costo_distancia, costo_mantenimiento

        integer, dimension(:), allocatable :: dist, previous, visited
        integer :: i, j, current_node, min_dist, n
        integer :: total_maintenance
        type(node_sucursal), pointer :: current
        type(sub_node), pointer :: neighbor
    
        ! Initialize variables
        current_node = source
        total_maintenance = 0
        n = 0
        contador=0
        current => self%head
        do while (associated(current))
            n = n + 1
            current => current%next
        end do
    
        ! des
        current => self%head
        do while (associated(current))
            neighbor => current%list
            do while (associated(neighbor))
                if (neighbor%llega > n) then
                    n = neighbor%llega
                end if
                neighbor => neighbor%next
            end do
            current => current%next
        end do
    
        print *, "sucursales:", n
    
        allocate(dist(n))
        allocate(previous(n))
        allocate(visited(n))
    
        
        dist(:) = HUGE(dist)
        previous(:) = 0
        visited(:) = 0
    
        dist(source) = 0
    
        do i = 1, n
            ! no visitados
            min_dist = HUGE(dist)
            do j = 1, n
                if (visited(j) == 0 .and. dist(j) < min_dist) then
                    min_dist = dist(j)
                    current_node = j
                end if
            end do
    
            ! Marcar los visitados
            visited(current_node) = 1
    
            ! act
            current => self%head
            do while (associated(current))
                if (current%salida == current_node) then
                    neighbor => current%list
                    do while (associated(neighbor))
                        if (dist(current_node) + neighbor%distancia < dist(neighbor%llega)) then
                            dist(neighbor%llega) = dist(current_node) + neighbor%distancia
                            previous(neighbor%llega) = current_node
                            ! aa
                            total_maintenance = total_maintenance + neighbor%mantenimiento
                        end if
                        neighbor => neighbor%next
                    end do
                end if
                current => current%next
            end do
        end do
    
        ! guardar las sucursales
        allocate(path_array(n))
        current_node = destination
        i = 1
        do while (current_node /= source)
            if (current_node < 1 .or. current_node > n) then
                print *, "Error:", current_node
                exit
            end if
            print *,  current_node
            path_array(i) = current_node
            contador=contador+1
            i = i + 1
            current_node = previous(current_node)
        end do
        path_array(i) = source
        print *, source
        contador=contador+1
    
        ! Print the path
        print *, "CAMINO CON MENOR DISTANCIA(COSTO) DE: ", source, " -> ", destination, "// DISTANCIA ", dist(destination)
        print *, "COSTO TOTAL DE MANTENIMIENTO:", total_maintenance-6
        costo_distancia=dist(destination)
        costo_mantenimiento=total_maintenance-6
        ! Deallocate 
        deallocate(dist, previous, visited)
    end subroutine shortestDistance
    

    subroutine graphWithColor(self, color_array)
        class(adyacencia), intent(in) :: self
        integer, dimension(:), intent(in) :: color_array ! azul
        integer :: io, i
        character(len=100) :: command
        type(node_sucursal), pointer :: current_node
        type(sub_node), pointer :: current_subnode, subsub
    
        !command = "dot -Tpng ./distancia_min.dot -o ./distancia_min.png"
        io = 1
        !print *, "color_array: ", color_array
        ! Open DOT file
        open(newunit=io, file="./distancia_min.dot")
        write(io, *) "digraph G {"
        write(io, *) "label=""Distancia minima"""
        ! Traverse 
        current_node => self%head
        do while(associated(current_node))
            ! si esta en el array azul
            !print *, current_node%salida
            if (any(current_node%salida == color_array )) then
                write(io, *) current_node%salida, "[label = """, current_node%salida, """, style=filled, color = lightblue]"
            else
                subsub => current_node%list
                do while(associated(subsub))
                    if (any(subsub%llega == color_array)) then
                        write(io, *) subsub%llega, "[label = """, subsub%llega, """, style=filled, color = lightblue]"
                    else
                        write(io, *) current_node%salida, "[label = """, current_node%salida, """]"
                    end if
                    subsub => subsub%next
                end do
            end if
    
            ! Connect nodes with edges
            current_subnode => current_node%list
            do while(associated(current_subnode))
write(io, *) current_node%salida, " -> ", current_subnode%llega, " [label = """, current_subnode%distancia, """, dir = normal];"
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do
    
        write(io, *) "}"
        close(io)
    
        ! Generate image
        !call execute_command_line(command, exitstat=i)
        call execute_command_line('dot -Tsvg distancia_min.dot > distancia_min.svg')
        call execute_command_line('start distancia_min.svg')
    
        if (i /= 0) then
            print *, "ERROR"
        else
            print *, "grafo con el recorrido creado."
        end if
    end subroutine graphWithColor
    

end module ruta_grafo

