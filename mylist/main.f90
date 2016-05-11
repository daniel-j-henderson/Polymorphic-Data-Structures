program main
    use key_mod
    use intkey_mod
    use stringkey_mod
    use list_mod
    implicit none

    class(key), pointer :: keyptr, tptr
    integer :: i, j, k=4
    integer, dimension(4) :: exps = (/10, 100, 500, 1000/)
    type(list) :: mylist
    class(node), pointer :: curr
    real, dimension(:), allocatable :: results
    integer(kind=8) :: cnt, tmp
    integer(kind=8) :: cntrate


    allocate(results(4*k))
    do i=1, k 
    
! Add some int, char keys
    call system_clock(cnt, cntrate)
    keyptr => string_key(1, "test "//char(1+70))
    call mylist%insert(keyptr)
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
    call system_clock(tmp)
    results((i-1)*4+1) = (tmp - cnt) / float(cntrate)

! Individual remove of specific keys
    tptr => int_key(0, 6)
    keyptr => mylist%remove(tptr)
    if (associated(keyptr)) then
        select type (keyptr)
        type is (int_key)
        print *, keyptr%getValue()
        class default
        end select
    else 
        print *, "Remove Failed"
    end if
    tptr => string_key(0, "test O") 
    keyptr => mylist%remove(tptr)
    if (associated(keyptr)) then
        select type (keyptr)
        type is (string_key)
        print *, keyptr%getValue()
        class default
        end select
    else
        print *, "Remove 2 failed"
    end if

!Test contains
    tptr => int_key(0, 6)
    keyptr => int_key(0, 8)
    write (*,*) "Contains 6, 8: ", mylist%contain(tptr), mylist%contain(keyptr) ! F, T
    deallocate(tptr, keyptr)
    tptr => string_key(0, "test O")
    keyptr => string_key(0, "test P")
    write (*,*) "Contains O, P: ", mylist%contain(tptr), mylist%contain(keyptr)    
    deallocate(tptr, keyptr)

!Time longest contains
    call system_clock(cnt)
    tptr => int_key(0, exps(i))
    keyptr => int_key(0, exps(i+1))
    write (*,*) "Contains n, n+1: ", mylist%contain(tptr), mylist%contain(keyptr) ! F, T
    deallocate(tptr, keyptr)
    call system_clock(tmp)
    results((i-1)*4+3) = (tmp - cnt) / float(cntrate)

!Traverse the list both ways
    call system_clock(cnt)
    curr => mylist%head
    j=0
    do while (associated(curr))
        j = j+1
        curr => curr%getNext()
    end do
    if (j /= mylist%size()) then
        write(*,*) "ERROR: Broken list, forward traversal"
    end if

    curr => mylist%tail
    j=0
    do while (associated(curr))
        j = j+1
        curr => curr%getPrev()
    end do
     if (j /= mylist%size()) then
        write(*,*) "ERROR: Broken list, backward traversal"
    end if
    call system_clock(tmp)
    results((i-1)*4+4) = (tmp - cnt) / float(cntrate)

!Remove all list elements in queue order
    call system_clock(cnt)
    do while (mylist%size() > 0) 
        keyptr => mylist%removeFirst()
        select type (keyptr)
        type is (int_key)
        type is (string_key)
        class default
        end select
        deallocate(keyptr) 
    end do
    call system_clock(tmp)
    results((i-1)*4+2) = (tmp - cnt) / float(cntrate)
    end do

    do i=1,k
    write (*,*) "Num Elements: ", 2*exps(i)
    write (*,*) "      insert time total: ", results((i-1)*4 + 1), "    avg: ", results((i-1)*4 + 1) / exps(i)
    write (*,*) "      remove time total: ", results((i-1)*4 + 2), "    avg: ", results((i-1)*4 + 2) / exps(i)
    write (*,*) "   double contains time: ", results((i-1)*4 + 3), " single: ", results((i-1)*4 + 3) / 2
    write (*,*) "  double traversal time: ", results((i-1)*4 + 4), "    avg: ", results((i-1)*4 + 2) / exps(i)
    end do

end program main
