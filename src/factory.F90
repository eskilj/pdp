module factory_mod
  use actor_mod
  private

  public :: product
  type product
    character(len=20) :: product_name
    class(actor), pointer :: product_class
  end type product

  type, public :: Factory
    private
      type(product), dimension(:), allocatable :: products
    contains
      procedure :: init_factory
      ! procedure :: create_actor
      procedure :: close
  end type Factory

  contains

  subroutine init_factory(self, sim_products)
    class(Factory), intent(inout) :: self
    type(product), dimension(:), intent(in) :: sim_products
    ! print *, SIZE(sim_products)
    allocate(self%products(SIZE(sim_products)))
    self%products = sim_products
  end subroutine init_factory

  subroutine close(self)
    class(Factory), intent(inout) :: self
    deallocate(self%products)
  end subroutine close

  ! function create_actor(self, actor_name) result(ptr)
  !   class(Factory) :: self
  !   character :: actor_name
  !   class(actor), pointer :: ptr

  !   select CASE (actor_name)
  !     CASE ("squirrel")
  !       print *, "make sq."
  !     CASE DEFAULT
  !       print *, "make sq."
  !   end select

  ! end function create_actor

  ! subroutine new_factory(self, )

  ! end subroutine new_factory

end module factory_mod