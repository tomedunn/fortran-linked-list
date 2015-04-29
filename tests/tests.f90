!===============================================================================
program main
!===============================================================================
  use linked_list_type
  implicit none
  ! local variables:
  logical, allocatable :: results(:)

  write(*,'("type NODE:")')
  call TEST_type_node(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("type LIST:")')
  call TEST_type_list(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

contains

!===============================================================================
! TEST_type_list:
!
  subroutine TEST_type_list( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 11
    ! local variables:
    type(list)   :: L
    type(node)   :: nVal
    character(5) :: chVal
    complex      :: cVal
    integer      :: iVal
    logical      :: lVal
    real         :: rVal

    allocate(results(nTests))
    results = .false.

    L = list()
    results(01) = L%len() == 0

    ! character
! NOTE: CHARACTER ASSIGNMENT IN LIST WON'T WORK DUE TO GFORTRAN COMPILER BUG
    call append(L, 'word')
    results(02) = L%len() == 1
    call get_item(L, 1, chVal)
    results(03) = chVal == 'word'
! END NOTE

    ! complex
    call append(L, (1.0,1.0))
    results(04) = L%len() == 2
    call get_item(L, 2, cVal)
    results(05) = cVal == (1.0,1.0)

    ! integer
    call append(L, 1)
    results(06) = L%len() == 3
    call get_item(L, 3, iVal)
    results(07) = iVal == 1

    ! logical
    call append(L, .true.)
    results(08) = L%len() == 4
    call get_item(L, 4, lVal)
    results(09) = lVal .eqv. .true.

    ! real
    call append(L, 1.0)
    results(10) = L%len() == 5
    call get_item(L, 5, rVal)
    results(11) = rVal == 1.0
  end subroutine TEST_type_list
!===============================================================================

!===============================================================================
! TEST_type_node:
!
  subroutine TEST_type_node( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 21
    ! local variables:
    integer    :: stat
    type(node) :: nVal
    character(5) :: chVal
    complex    :: cVal
    integer    :: iVal
    logical    :: lVal
    real       :: rVal

    allocate(results(nTests))

    ! character:
    chVal = 'word'
! NOTE: CHARACTER ASSIGNMENT IN LIST WON'T WORK DUE TO GFORTRAN COMPILER BUG
    nVal = node(chVal)
    results(01) = chVal == nVal
    results(02) = nVal  == chVal
    call get_item(nVal, cVal, stat=stat)
    results(03) = stat == -1
    call get_item(nVal, chVal, stat=stat)
    results(04) = stat == 0 .and. chVal == 'word'
! END NOTE

    ! complex:
    cVal = (1,1)
    nVal = node(cVal)
    results(05) = cVal == nVal
    results(06) = nVal == cVal
    call get_item(nVal, iVal, stat=stat)
    results(07) = stat == -1
    call get_item(nVal, cVal, stat=stat)
    results(08) = stat == 0 .and. cVal == (1,1)

    ! integer:
    iVal = 1
    nVal = node(iVal)
    results(09) = iVal == nVal
    results(10) = nVal == iVal
    call get_item(nVal, lVal, stat=stat)
    results(11) = stat == -1
    call get_item(nVal, iVal, stat=stat)
    results(12) = stat == 0 .and. iVal == 1

    ! logical:
    lVal = .true.
    nVal = node(lVal)
    results(13) = lVal == nVal
    results(14) = nVal == lVal
    call get_item(nVal, rVal, stat=stat)
    results(15) = stat == -1
    call get_item(nVal, lVal, stat=stat)
    results(16) = stat == 0 .and. lVal .eqv. .true.

    ! real:
    rVal = 1.0
    nVal = node(rVal)
    results(17) = rVal == nVal
    results(18) = nVal == rVal
    call get_item(nVal, cVal, stat=stat)
    results(19) = stat == -1
    call get_item(nVal, rVal, stat=stat)
    results(20) = stat == 0 .and. rVal == 1.0

    nVal = node()
    call get_item(nVal, cVal, stat=stat)
    results(21) = stat == -2
  end subroutine TEST_type_node
!===============================================================================
end program main
!===============================================================================
