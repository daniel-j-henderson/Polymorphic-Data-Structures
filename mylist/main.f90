program main
    use key_mod
    use intkey_mod
    use stringkey_mod
    use list_mod
    implicit none
    class(key), pointer :: keyptr
    integer :: i, j, k=4
    integer, dimension(4) :: exps = (/10, 100, 500, 1000/)
    type(list) :: mylist
    real, dimension(:), allocatable :: results
    real :: temp
    allocate(results(2*k))
    do i=1, k 
    
    call cpu_time(temp)
    keyptr => string_key(1, "test "//char(1+70))
    call mylist%insert(keyptr)
    !call mylist%insert(string_key(1, "test "//char(1+70)))
    do j=2,exps(i)
    select type (keyptr)
    type is (string_key)
        call keyptr%setValue("test "//char(j+70))
        call keyptr%setID(j)
    class default
    end select
    call mylist%insert(keyptr)
    end do
    deallocate(keyptr)
    do j=1, exps(i)
        keyptr => int_key(j, 2*j)
        call mylist%insert(keyptr)
        deallocate(keyptr)
    end do
    call cpu_time(results((i-1)*2+1))
    results((i-1)*2+1) = results((i-1)*2+1) - temp
    
    call cpu_time(temp)
    do while (mylist%size() > 0) 
        keyptr => mylist%removeFirst()
        select type (keyptr)
        type is (int_key)
    !        print *, keyptr%getValue()
        type is (string_key)
     !       print *, keyptr%getValue()
        class default
        end select
        deallocate(keyptr) 
    end do
    call cpu_time(results((i-1)*2+2))
    results((i-1)*2+2) = results((i-1)*2+2) - temp
    end do

    do i=1,k
    write (*,*) "Num Elements: ", 2*exps(i)
    write (*,*) "    insert time total: ", results((i-1)*2 + 1), "    avg: ", results((i-1)*2 + 1) / exps(i), "\n"
    write (*,*) "    remove time total: ", results((i-1)*2 + 2), "    avg: ", results((i-1)*2 + 2) / exps(i), "\n"
    end do

end program main
