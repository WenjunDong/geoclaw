
    !*****************************************************************
    !*
    !* This program  is used for set up precision
    !*
    !*****************************************************************
    module Set_Precision !Sets the default precision for floating point numbers

        implicit none

        public :: sngl, dbl, quad, Prec

        integer, parameter :: Sngl = Selected_Real_kind( 6, 37)
        integer, parameter :: Quad = Selected_Real_kind(33, 4931)
        integer, parameter :: Dbl  = Selected_Real_Kind(15, 307)!double precision
        integer, parameter :: Prec = Dbl ! Set default precision to double precision
        integer, parameter :: Charl = 30

    end module Set_Precision

    module Set_variable !Sets the default precision for floating point numbers

        use Set_Precision, only: Prec
        !use sediment_module, only: nyc,nxc!,gmax,lmax
        !use amr_module


        implicit none
        !Real(kind=Prec), dimension(1:nyc,1:50,5) :: ub
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc,gmax) :: ub_cr,us_cr1,us_cr2,ws
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc,gmax) :: cu,cub,cv,cvb,ccg,ccbg,Susg,Subg,Svbg,Svsg
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc,gmax) :: ceqbg,ceqsg,Tsg
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc) :: z0
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc) :: z0bed,sedero
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc,gmax) :: Sus,Sub,Svs,Svb
        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc,lmax):: dzbed
        !Real(kind=Prec), dimension(:,:,:,:),allocatable :: pbbed
        !Real(kind=Prec), dimension(:,:),allocatable :: totalthick
        !integer, dimension(:,:),allocatable :: totalnum

        !nx      = node(ndihi,mptr) - node(ndilo,mptr) + 1

        Real(kind=Prec), dimension(:,:,:),allocatable,save :: ub_cr,us_cr1,us_cr2,ws
        Real(kind=Prec), dimension(:,:,:),allocatable,save :: cu,cub,cv,cvb,ccg,ccbg,Susg,Subg,Svbg,Svsg
        Real(kind=Prec), dimension(:,:,:),allocatable,save :: ceqbg,ceqsg,Tsg
        Real(kind=Prec), dimension(:,:),allocatable,save :: z0,zb
        Real(kind=Prec), dimension(:,:),allocatable,save :: z0bed,sedero
        Real(kind=Prec), dimension(:,:,:),allocatable,save :: Sus,Sub,Svs,Svb,percd
        Real(kind=Prec), dimension(:,:,:,:),allocatable,save :: pbbed
        Real(kind=Prec), dimension(:,:,:),allocatable,save :: dzbed
        Real(kind=Prec), dimension(:,:),allocatable,save :: totalthick
        integer, dimension(:,:),allocatable,save :: totalnum

        !nx  = node(ndihi,mptr) - node(ndilo,mptr) + 1

        contains

        subroutine test

            implicit none
            integer :: test_num
            !print *, nyc,nxc
            test_num = 0
        end subroutine

    end module Set_variable

















