module vll_classdef
  use vf_classdef
  implicit none
  type vll_class
    type(vf_class), dimension(4) :: vf
    real(dp) :: gam
  contains
    procedure :: vind => vllclass_vind
    procedure :: assignP => vllclass_assignP
    procedure :: shiftdP  => vllclass_shiftdP
    procedure :: rot  => vllclass_rot
    procedure :: calclength => vllclass_calclength
    procedure :: strain => vllclass_strain
  end type vll_class

contains

  function vllclass_vind(this,P) result(vind)
    ! Calculates induced velocity by unit gam vortex ring
  class(vll_class) :: this
    real(dp), dimension(3) :: P, vind
    real(dp), dimension(4,3) :: vind_mat
    integer :: i

    vind=0._dp

    vind_mat(1,:)=this%vf(i)%vind(P) 
    vind_mat(3,:)=this%vf(i)%vind(P) 
    vind=sum(vind_mat,1)

  end function vllclass_vind

  ! Panel coordinates
  ! o---------> Y along span
  ! |
  ! |   1-----------4
  ! |   |     4     |
  ! |   |           |
  ! |   |1         3|
  ! |   |           |
  ! |   |     2     |
  ! |   2-----------3
  ! |
  ! V X along chord

  subroutine vllclass_assignP(this,n,P)   ! for assigning coordinates to nth corner
  class(vll_class) :: this
    integer, intent(in) :: n
    real(dp), dimension(3) :: P

    select case (n)
    case (1)
      this%vf(4)%fc(:,2)=P
      this%vf(1)%fc(:,1)=P
    case (2)
      this%vf(1)%fc(:,2)=P
      this%vf(2)%fc(:,1)=P
    case (3)
      this%vf(2)%fc(:,2)=P
      this%vf(3)%fc(:,1)=P
    case (4)
      this%vf(3)%fc(:,2)=P
      this%vf(4)%fc(:,1)=P
    case default
      error stop 'n may only take values 1,2,3 or 4'
    end select

  end subroutine vllclass_assignP

  subroutine vllclass_shiftdP(this,n,dshift)   ! for shifting coordinates of nth corner by dshift distance (usually for Udt convection)
  class(vll_class) :: this
    integer, intent(in) :: n
    real(dp), intent(in), dimension(3) :: dshift

    select case (n)
    case (1)
      this%vf(4)%fc(:,2)=this%vf(4)%fc(:,2)+dshift
      this%vf(1)%fc(:,1)=this%vf(1)%fc(:,1)+dshift
    case (2)             
      this%vf(1)%fc(:,2)=this%vf(1)%fc(:,2)+dshift
      this%vf(2)%fc(:,1)=this%vf(2)%fc(:,1)+dshift
    case (3)            
      this%vf(2)%fc(:,2)=this%vf(2)%fc(:,2)+dshift
      this%vf(3)%fc(:,1)=this%vf(3)%fc(:,1)+dshift
    case (4)           
      this%vf(3)%fc(:,2)=this%vf(3)%fc(:,2)+dshift
      this%vf(4)%fc(:,1)=this%vf(4)%fc(:,1)+dshift
    case default
      error stop 'n may only take values 1,2,3 or 4'
    end select

  end subroutine vllclass_shiftdP

  subroutine vllclass_rot(this,Tmat)
    ! Rotate vortex ring using Tmat
  class(vll_class) :: this
    real(dp), intent(in), dimension(3,3) :: Tmat
    integer :: i

    do i=1,4
      this%vf(i)%fc(:,1)=matmul(Tmat,this%vf(i)%fc(:,1))
      this%vf(i)%fc(:,2)=matmul(Tmat,this%vf(i)%fc(:,2))
    enddo

  end subroutine vllclass_rot

  subroutine vllclass_calclength(this,isoriginal)
  class(vll_class) :: this
    logical, intent(in) :: isoriginal
    integer :: i
    do i=1,4
      call this%vf(i)%calclength(isoriginal)
    enddo
  end subroutine vllclass_calclength

  subroutine vllclass_strain(this)
  class(vll_class) :: this
    integer :: i
    do i=1,4
      call this%vf(i)%strain()
    enddo
  end subroutine vllclass_strain

end module vll_classdef
