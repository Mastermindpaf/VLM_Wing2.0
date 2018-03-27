module wakepanel_classdef
  use vll_classdef
  implicit none
  type wakepanel_class
    type(vll_class) :: vll
    integer :: tag                   ! for identifying panel to be wing or wake
  end type wakepanel_class

contains

  ! VR coordinates
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

end module wakepanel_classdef
