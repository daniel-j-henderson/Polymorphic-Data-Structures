module list_mod
    use node_mod
    use key_mod
    implicit none
    type list
        class(node), private, pointer :: head => null()
        class(node), private, pointer :: tail => null()
        integer :: s = 0
        contains
        procedure :: insert
        procedure :: isEmpty
        procedure :: size
        procedure :: removeFirst
        procedure :: removeLast
        !procedure :: getIndex
        procedure :: printList
    end type list

    contains

    subroutine insert(this, value)
        class(list) :: this
        class(key), pointer :: value
        class(node), pointer :: new
        if (this%s == 0) then !.not. associated(this%head)) then
            this%head => node(value, this%tail, this%head)
            this%tail => this%head
            this%s = this%s + 1
        else 
            new => this%tail%getNext()
            new => node(value, new, this%tail)
            call this%tail%setNext(new)
            this%tail => new
            this%s = this%s + 1
        end if
    end subroutine insert

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
        !allocate(removeFirst, source=this%head%getValue())
        removeFirst => this%head%getValue()

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
        allocate(removeLast, source=this%tail%getValue())
        temp => this%tail
        this%tail => this%tail%getPrev()
        deallocate(temp)
        this%s = this%s - 1
    end function removeLast

    subroutine printList(this)
        class(list) :: this
        class(node), pointer :: itr
        itr => this%head
        do while (associated(itr))
            call itr%printNode()
            itr => itr%getNext()
        end do
    end subroutine printList
            
end module list_mod


