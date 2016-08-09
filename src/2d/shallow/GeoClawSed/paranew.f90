
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

        implicit none

        !Real(kind=Prec), dimension(1-mbc:mx+mbc,1-mbc:my+mbc,gmax) :: ub_cr,us_cr1,us_cr2,ws
        !Real(kind=Prec), dimension(:,:,:),allocatable :: cu,cub,cv,cvb,ccg,ccbg,Susg,Subg,Svbg,Svsg
        !Real(kind=Prec), dimension(:,:,:),allocatable :: ceqbg,ceqsg,Tsg
        !Real(kind=Prec), dimension(:,:),allocatable :: z0
        !Real(kind=Prec), dimension(:,:),allocatable :: z0bed,sedero
        !Real(kind=Prec), dimension(:,:,:),allocatable :: Sus,Sub,Svs,Svb
        !Real(kind=Prec), dimension(:,:,:),allocatable :: dzbed


        Real(kind=Prec), dimension(:,:,:),allocatable :: ub_cr,us_cr1,us_cr2,ws
        Real(kind=Prec), dimension(:,:,:),allocatable :: cu,cub,cv,cvb,ccg,ccbg,Susg,Subg,Svbg,Svsg
        Real(kind=Prec), dimension(:,:,:),allocatable :: ceqbg,ceqsg,Tsg
        Real(kind=Prec), dimension(:,:),allocatable :: z0,zb
        Real(kind=Prec), dimension(:,:),allocatable :: z0bed,sedero
        Real(kind=Prec), dimension(:,:,:),allocatable :: Sus,Sub,Svs,Svb
        Real(kind=Prec), dimension(:,:,:,:),allocatable :: pbbed
        Real(kind=Prec), dimension(:,:,:),allocatable :: dzbed
        Real(kind=Prec), dimension(:,:),allocatable :: totalthick
        integer, dimension(:,:),allocatable :: totalnum
    end module Set_variable

















