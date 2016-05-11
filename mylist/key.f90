module key_mod
  type :: key
    integer :: id
    contains
      procedure :: setID
      procedure :: getID
!     procedure :: getValue
  end type key

  interface key
    module procedure keyconstructor
  end interface
  contains
!    function getValue(this)
!        class(key) :: this
!        integer :: getValue
!        getValue = this%id
!    end function getValue
    
    function keyconstructor(i)
        integer :: i
        class(key), pointer :: keyconstructor
        allocate(keyconstructor)
        keyconstructor%id = i
    end function keyconstructor

    subroutine setID(this, i)
        class(key) :: this
        this%id = i
    end subroutine setID

    function getID(this)
        class(key) :: this
        integer :: getID
        getID = this%id
    end function getID

end module key_mod

!module anykey_mod
!    use key_mod
!    type, extends (key) :: any_key
!    class(*), pointer :: value
!    contains 
!        procedure :: getValue => getAnyKeyVal
!        procedure :: setValue => setAnyKeyVal
!        procedure :: setAnyKeyEqual
!        procedure :: anyKeyEquals
!        generic :: assignment(=) => setAnyKeyEqual
!        generic :: operator(==) => anyKeyEquals
!    end type any_key
!
!    interface any_key
!        module procedure anyconstructor
!    end interface
!   
!    contains
!        
!        function anyconstructor(i, val)
!            class(*) :: val
!            integer :: i
!            class(any_key), pointer :: anyconstructor
!            allocate(anyconstructor)
!            allocate(anyconstructor%value, source=val)
!            anyconstructor%id = i
!        end function anyconstructor
!
!        function getAnyKeyVal(this)
!            class(*), pointer :: getAnyKeyVal
!            class(any_key) :: this
!            getAnyKeyVal => this%value
!        end function getAnyKeyVal
!
!        subroutine setAnyKeyVal(this, val)
!            class(any_key) :: this
!            class(*) :: val
!            if (associated(this%value)) then
!                deallocate(this%value)
!            end if
!            allocate(this%value, source=val)
!        end subroutine setAnyKeyVal
!
!        subroutine setAnyKeyEqual(this, other)
!            class(any_key), intent(out) :: this
!            class(key), intent(in) :: other
!            if (associated(this%value)) then
!                deallocate(this%value)          
!            end if
!            allocate(this%value, source = other%getValue())
!        end subroutine setAnyKeyEqual
!
!        function anyKeyEquals(this, other)
!            class(any_key), intent(in) :: this
!            class(key), intent(in) :: other
!            logical :: anyKeyEquals
!            class(*), allocatable :: temp
!            allocate(temp, source = other%getValue())
!            if(same_type_as(this%value, temp)) then
!            if (this%value == temp) then
!                anyKeyEquals = .true.
!            else
!                anyKeyEquals = .false.
!            end if
!            end if
!        end function anyKeyEquals
!                    
!end module anykey_mod 

module stringkey_mod
  use key_mod
  type, extends (key) :: string_key 
    character(len=100) :: value
    contains
      procedure :: setValue => setStrKeyVal
      procedure :: getValue => getStrKeyVal
      procedure :: setStrKeyEqual
      procedure :: strKeyEquals
      generic :: assignment(=) => setStrKeyEqual
      generic :: operator(==) => strKeyEquals
  end type string_key

  interface string_key
    module procedure strconstructor
  end interface

  contains


    function strconstructor(i, val)
        class(string_key), pointer :: strconstructor
        character(len=*) :: val
        integer :: i
        allocate(strconstructor)
        strconstructor%value = trim(val)
        strconstructor%id = i
    end function strconstructor

    subroutine setStrKeyVal(this, val)
      class(string_key) :: this
      character(len=*) :: val
      this%value = val
    end subroutine setStrKeyVal

    function getStrKeyVal(this)
      class(string_key) :: this
      character(len=100) getStrKeyVal
      getStrKeyVal = this%value
    end function getStrKeyVal

    subroutine setStrKeyEqual(this, other)
      class(string_key), intent(out) :: this
      class(string_key), intent(in) :: other
      this%value = other%value
    end subroutine setStrKeyEqual

    function strKeyEquals(this, other)
      class(string_key), intent(in) :: this
      class(string_key), intent(in) :: other
      logical :: strKeyEquals
      if (this%value == other%value) then
        strKeyEquals = .true.
      else 
        strKeyEquals = .false.
      endif
    end function strKeyEquals

end module stringkey_mod

module intkey_mod
  use key_mod
  type, extends (key) :: int_key 
    integer :: value
    contains
      procedure :: setValue => setIntKeyVal
      procedure :: getValue => getIntKeyVal
      procedure :: setIntKeyEqual
      procedure :: intKeyEquals
      generic :: assignment(=) => setIntKeyEqual
      generic :: operator(==) => intKeyEquals
  end type int_key

  interface int_key
    module procedure intconstructor
  end interface

  contains


    function intconstructor(i, val)
        class(int_key), pointer :: intconstructor
        integer :: val
        integer :: i
        allocate(intconstructor)
        intconstructor%value = val
        intconstructor%id = i
    end function intconstructor

    subroutine setIntKeyVal(this, val)
      class(int_key) :: this
      integer :: val
      this%value = val
    end subroutine setIntKeyVal

    function getIntKeyVal(this)
      class(int_key) :: this
      integer :: getIntKeyVal
      getIntKeyVal = this%value
    end function getIntKeyVal

    subroutine setIntKeyEqual(this, other)
      class(int_key), intent(out) :: this
      class(int_key), intent(in) :: other
      this%value = other%value
    end subroutine setIntKeyEqual

    function intKeyEquals(this, other)
      class(int_key), intent(in) :: this
      class(int_key), intent(in) :: other
      logical :: intKeyEquals
      if (this%value == other%value) then
        intKeyEquals = .true.
      else 
        intKeyEquals = .false.
      endif
    end function intKeyEquals

end module intkey_mod
