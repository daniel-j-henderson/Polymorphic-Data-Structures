module key_mod
  type, abstract :: key
    integer :: id
    contains
      procedure :: setID
      procedure :: getID
      procedure(eq), deferred :: equals
      generic :: operator(==) => equals
  end type key

  abstract interface
      function eq(this, other)
        import
        class(key), intent(in) :: this
        class(key), intent(in) :: other
        logical :: eq
      end function eq
  end interface

  contains

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


module stringkey_mod
  use key_mod
  type, extends (key) :: string_key 
    character(len=100) :: value
    contains
      procedure :: setValue => setStrKeyVal
      procedure :: getValue => getStrKeyVal
      procedure :: setStrKeyEqual
      procedure :: equals => strKeyEquals
      generic :: assignment(=) => setStrKeyEqual
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
      class(key), intent(in) :: other
      logical :: strKeyEquals
      select type(other)
      type is (string_key)
      if (this%value == other%value) then
        strKeyEquals = .true.
      else 
        strKeyEquals = .false.
      endif
      class default
        strKeyEquals = .false.
      end select
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
      procedure :: equals => intKeyEquals
      generic :: assignment(=) => setIntKeyEqual
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
      class(key), intent(in) :: other
      logical :: intKeyEquals
      select type (other)
      type is (int_key)
      if (this%value == other%value) then
        intKeyEquals = .true.
      else 
        intKeyEquals = .false.
      endif
      class default
      intKeyEquals = .false.
      end select
    end function intKeyEquals

end module intkey_mod
