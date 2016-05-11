module list_mod
    use node_mod
    use key_mod
    implicit none
    type list
        class(node), pointer :: head => null()
        class(node), pointer :: tail => null()
        integer, public :: s = 0
        contains
        procedure :: insert
        procedure :: isEmpty
        procedure :: contain
        procedure :: size
        procedure :: remove
        procedure :: removeFirst
        procedure :: removeLast
    end type list


    contains


    subroutine insert(this, value)
        class(list) :: this
        class(key), pointer :: value
        class(node), pointer :: new
        if (this%s == 0) then 
            this%head => node(value, this%tail, this%head)
            this%head%prev => null()
            this%head%next => null()
            this%tail => this%head
            this%s = this%s + 1
        else 
            new => null()
            new => node(value, new, this%tail)
            call this%tail%setNext(new)
            this%tail => new
            this%s = this%s + 1
        end if
    end subroutine insert

    function contain(this, t)
        class(list) :: this
        class(key), pointer :: t
        class(node), pointer :: curr
        class(key), pointer :: temp
        logical :: contain
        integer :: i
        contain = .false.
        i = this%s
        curr => this%head
        do while(i > 0)
            temp => curr%getKey()
            if (t == temp) then
                contain = .true.
                exit
            else
                curr => curr%getNext()
                i = i-1
            end if
            deallocate(temp)
        end do
        if (associated(temp)) then
            deallocate(temp)
        end if
    end function contain

    function remove(this, t)
        class(list) :: this
        class(key), pointer :: t
        class(key), pointer :: remove
        class(node), pointer :: curr 
        class(node), pointer :: temp, temp2
        integer :: i
        i = this%s
        curr => this%head 
        do while (i > 0)
            remove => curr%getKey()
            if (remove == t) then
                temp => curr%getPrev()
                temp2 => curr%getNext()
                call temp%setNext(temp2)
                temp => curr%getNext()
                temp2 => curr%getPrev()
                call temp%setPrev(temp2)
                call curr%delete()
                deallocate(curr)
                this%s = this%s - 1
                exit
            else
                deallocate(remove)
                curr => curr%getNext()
                i = i-1
            end if
        end do
    end function remove
        
        

    function isEmpty(this)
        class(list) :: this
        logical :: isEmpty
        if (this%s == 0) then 
            isEmpty = .true.
        else 
            isEmpty = .false.
        end if
    end function isEmpty
    
    function size(this)
        class(list) :: this
        integer :: size
        size = this%s
    end function size

    function removeFirst(this)
        class(list) :: this
        class(key), pointer :: removeFirst
        class(node), pointer :: temp
        removeFirst => this%head%getKey()

        temp => this%head
        this%head => this%head%getNext()
        call temp%delete()
        deallocate(temp)
        this%s = this%s - 1
    end function removeFirst

    function removeLast(this)
        class(list) :: this
        class(key), pointer :: removeLast
        class(node), pointer :: temp
        allocate(removeLast, source=this%tail%getKey())
        temp => this%tail
        this%tail => this%tail%getPrev()
        deallocate(temp)
        this%s = this%s - 1
    end function removeLast

end module list_mod


