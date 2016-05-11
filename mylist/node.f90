module node_mod
    use key_mod
    use intkey_mod
    use stringkey_mod
implicit none
    type node
        class(key), pointer :: mykey => null()
        class(node), private, pointer :: next => null()
        class(node), private, pointer :: prev => null()
        contains
        procedure :: getNext
        procedure :: getPrev
        procedure :: setValue
        procedure :: getValue
        procedure :: setNext
        procedure :: setPrev
        procedure :: delete
        procedure :: printNode
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

    subroutine printNode(this)
        class(node) :: this
        class(key), pointer :: temp
        temp => this%mykey
        select type(temp)
        type is (int_key)
            print *, temp%value
        type is (string_key)
            print *, temp%getValue()
        class default
           print *, temp%getID()
        end select
    end subroutine printNode
    
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

    subroutine setValue(this, val)
        class(node) :: this
        class(key) :: val
        if (associated(this%mykey)) then
            deallocate(this%mykey)
        end if
        allocate(this%mykey, source=val)
    end subroutine setValue

    function getValue(this)
        class(node) :: this
        class(key), pointer :: getValue
        allocate(getValue, source=this%mykey)
    end function getValue

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
