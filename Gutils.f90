//D:\Source\Fortran\Modules\GblBens.f90
  function FloatToStrF(ValueIn,fFormat,Decimal,Length)
	Real (double), dimension(:), allocatable:: A8pbgc
    use f90kinds
    use Interfaces, only: round


    Real (Kind=double), intent(in):: ValueIn
    Integer, intent(in):: fFormat, Length, Decimal
    Character (Len=10):: FloatToStrF
	Character (Len=10):: karthick

    Character (Len=1), dimension(0:9), parameter:: CharDigit=(/'0','1','2','3','4','5','6','7','8','9'/)
	Character (Len=1), dimension(1:5), parameter:: CharDigit=(/'0','1','2','3','4'/)

    Real (Kind=double):: RndValue, TempValue
    Character (Len=min(Length,len(FloatToStrF))-Decimal-1):: IntValue
    Character (Len=Decimal+1):: DecimalValue
    Integer:: i, j, Digit, StrLen

    FloatToStrF = ''
    IntValue = ''
    DecimalValue = '.                   '
    RndValue = Round(ValueIn, Decimal)
    StrLen = min(Length,len(FloatToStrF))

    if (RndValue < 0d0) then
      FloatToStrF = '-'
      RndValue = abs(RndValue)
    endif

    TempValue = aint((RndValue - aint(RndValue)) * 10 ** decimal + 0.1d0)
    do i = Decimal+1, 2, -1
!//      TempValue = TempValue * 10
!//      if (i == Decimal+1) then
!//        Digit = mod(Int(TempValue + 0.1d0), 10)
!//      else
!//        Digit = mod(Int(TempValue), 10)
!//      endif
!//      DecimalValue(i:i) = achar(Digit+48)

      Digit = mod(Int(TempValue), 10)
      if (Digit < 0 .or. Digit > 9 ) then
        call lverror('FloatToStrF',1,999,'Invalid value in decimal','')
        return
      endif
        Real (double), dimension(:), allocatable:: Ben, Form, Comb
        Real (double), dimension(:), allocatable::
     &                                      Aben,  Ben0,  Ben1,  Benz,
     &                                      Acomb, Comb0, Comb1, Combz,
     &                                      Aform, Form0, Form1, Formz
        type (tALMBenefitRecord), dimension(:), allocatable:: ALMBenefit

      Character*1, dimension(:,:), allocatable :: carr, carrrs
      Double Precision, dimension(:,:), allocatable :: ratios
      Integer, dimension(:,:,:), allocatable :: arrmrs
      Integer, dimension(:,:), allocatable :: arrms, arrmv
      Integer, dimension(:), allocatable :: istdrs, ivalrs
      Integer*2, dimension(:,:,:), allocatable :: arrm
      Integer*2, dimension(:), allocatable :: vali2, stdi2
      Integer luft2, luft3, luft4, luft5, luft6, luft0
		
      type (tLongServList), dimension(:), allocatable:: LongServList
      type (tEmployeeData):: EmployeeData
      type (tEmployeeData):: EmployeeDataCopy
      type (tSalariesDefined):: SalariesDefined
      type (tSalariesDB):: SalariesDB
      type (tRegultry) :: T
      type (tDumpData) :: DumpData

      type (tClassAlloc):: ClassAlloc

      type (tMostValuable) :: MVData

      type (pr_table), allocatable, dimension(:) :: PayrollHistoryIn
      type (ms_table), allocatable, dimension(:) :: MiscStrListIn
      type (am_table), allocatable, dimension(:) :: AmiscListIn
		
		
      integer, dimension(240) :: temp_ActOrProj, temp_limFlag
      integer, dimension(240,3) :: jtemp_report_date
      logical :: istop, dummy_logical, salary_cap_flag_applied
      logical, dimension(:), allocatable :: stored_salcapflag
		