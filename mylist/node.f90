module node_mod
    use key_mod
implicit none
    type node
        class(key), pointer :: mykey => null()
        class(node), pointer :: next => null()
        class(node), pointer :: prev => null()
        contains
        procedure :: getNext
        procedure :: getPrev
        procedure :: setKey
        procedure :: getkey
        procedure :: setNext
        procedure :: setPrev
        procedure :: delete
    end type node

    interface node
        module procedure constructor
    end interface

    contains

    subroutine delete(this)
        class(node) :: this
        deallocate(this%mykey)
        this%next => null()
        this%prev => null()
    end subroutine delete

    function getNext(this)
        class(node) :: this
        class(node), pointer :: getNext
        getNext => this%next
    end function getNext    
    
    function getPrev(this)
        class(node) :: this
        class(node), pointer :: getPrev
        getPrev => this%prev
    end function getPrev    

    subroutine setKey(this, val)
        class(node) :: this
        class(key) :: val
        if (associated(this%mykey)) then
            deallocate(this%mykey)
        end if
        allocate(this%mykey, source=val)
    end subroutine setKey

    function getKey(this)
        class(node) :: this
        class(key), pointer :: getKey
        allocate(getKey, source=this%mykey)
    end function getKey

    subroutine setNext(this, n)
        class(node), pointer :: n
        class(node) :: this
        this%next => n
    end subroutine setNext

    subroutine setPrev(this, n)
        class(node), pointer :: n
        class(node) :: this
        this%prev => n
    end subroutine setPrev

    function constructor(value, n, p)
        class(node), pointer :: constructor
        class(key), pointer :: value
        class(node), pointer :: n, p
        allocate(constructor)
        allocate(constructor%mykey, source=value)
        constructor%next => n
        constructor%prev => p
    end function constructor
end module node_mod
