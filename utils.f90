  function FloatToStrF(ValueIn,fFormat,Decimal,Length)

    use f90kinds
    use Interfaces, only: round

    Real (Kind=double), intent(in):: ValueIn
    Integer, intent(in):: fFormat, Length, Decimal
    Character (Len=10):: FloatToStrF
	Character (Len=10):: karthick

    Character (Len=1), dimension(0:9), parameter:: CharDigit=(/'0','1','2','3','4','5','6','7','8','9'/)

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
