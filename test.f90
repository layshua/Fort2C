Do k = 1, Ubound (BenefitStruct,2)                      ! Combs, ValBens(Ret,Dth,Dis,Wth)
    Do i = 1, Ubound (BenefitStruct,1)
      Call InitBenInfo(BenefitStruct(i,k)%Ans, AllValues=.true.)
      Do j = 1, 4
        Call InitBenInfo(BenefitStruct(i,k)%Piece(j), AllValues=.true.)
      End Do  ! j

 Call lverror('AllocateComboStructures', 1, iastat,'Error allocating BenefitStruct array.','');
          Select Case (Trim (bentype))
          Case ('BENEFIT')
            ibentype = 1
          Case ('FORMULA')
            ibentype = 2
          Case ('COMBINE')
            ibentype = 3
          Case ('RETIREMENT')
            ibentype = 4
          Case ('DEATH')
            ibentype = 5
          Case ('DISABILITY')
            ibentype = 6
          Case ('WITHDRAWAL')
            ibentype = 7
          Case Default
            ibentype = 0
          End Select
      End Do  ! j
Module Combos

Use Globals
Use gblbens
Use f90kinds
Use Lvutility
Use nfsvcvr
Use cbuamod
Use GlobErr, Only: iErrCode

Implicit None

!// All declarations in this module are private unless explicity declared as public.
Private

Public :: Combination,                                                                    &
          ValBenefit,                                                                     &
          CombSet,                                                                        &
          AllocateComboStructures,                                                        &
          DeallocateComboStructures,                                                      &
          ReadMenuVarComb,                                                                &
          CombSetFAS,                                                                     &
          SaveNonCBBenInfo,                                                               &
          SaveCBBenInfo,                                                                  &
          SaveBenInfo,                                                                    &
          SaveFormInfo,                                                                   &
          AgeLoopAdj,                                                                     &
          CombFinal,                                                                      &
          CombFinalFAS,                                                                   &
          InitializeCombArrays,                                                           &
          DebugCombos,                                                                    &
          Combs, Rets, Dths, Disbs, Wths, Ret0s, Ret1s, ARets, RetZs, Fld35

Include 'Params.INC'
Include 'COMM1.INC'
Include 'COMM3.INC'
Include 'COMM4.INC'
Include 'product.inc'

Type(tCombMenu), Pointer:: Combination(:) => null()
Type(tCombMenu), Pointer:: ValBenefit(:,:) => null()
Real (Double), Dimension(:, :), Allocatable:: Fld35
Logical:: DebugCombos

!// Constants
Integer, Parameter:: Combs=1, Rets=2, Dths=3, Disbs=4, Wths=5, Ret0s=2, Ret1s=3, ARets=4, RetZs=5

!// remainder of variables are private to the module
Type (tCombMenu), Dimension(:, :), Allocatable, Target:: BenefitStruct
Type (tBenInfo), Dimension(:), Allocatable:: BenVars, FormVars
Real (Double), Dimension(:, :), Allocatable:: ARETXnonCB, CRETXnonCB, HBENnonCB
Integer, Dimension(:, :), Allocatable:: flag
Integer:: iastat, lcMaxBenBlks, lcMaxForms, lcMaxCombs
Logical:: UseFas35Overrides
Logical, Dimension(:, :), Allocatable:: UseFld35
Character (Len=80):: ErrMessage

!/----------------------------------------------------------------------------------------
Contains
!/----------------------------------------------------------------------------------------

  Function AllocateComboStructures() Result (result)

    Implicit None

!// Dummy arguments
    Logical:: result

!//  Local variables
    Integer:: MenFSOvr


    If (DebugCombos) Then
      Call debug ('** Entry into AllocateComboStructures')
    End If

    result = .False.
    iastat = 0
    If (.not. Allocated(BenefitStruct)) Then
      Allocate (BenefitStruct(Max (Max (9,MaxBens),MaxCombs),5),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating BenefitStruct array.','');
        Return
      End If

      ValBenefit  => BenefitStruct(:, 2:)
      Combination => BenefitStruct(:, 1)
    End If

    If (.not. Allocated(BenVars)) Then
      lcMaxBenBlks = Ubound (ben,1)
      Allocate (BenVars(lcMaxBenBlks),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating BenVars array.','');
        Return
      End If
    End If

    If (.not. Allocated(FormVars)) Then
      lcMaxForms = Ubound (form,1)
      Allocate (FormVars(lcMaxForms),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating FormVars array.','');
        Return
      End If
    End If

    lcMaxCombs = Ubound (comb,1)

    If (.not. Allocated(ARETXnonCB)) Then
      Allocate (ARETXnonCB(Max (9,MaxBens), 4),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating ARETXnonCB array.','');
        Return
      End If
    End If

    If (.not. Allocated(CRETXnonCB)) Then
      Allocate (CRETXnonCB(Max (9,MaxBens), 4),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating CRETXnonCB array.','');
        Return
      End If
    End If

    If (.not. Allocated(HBENnonCB)) Then
      Allocate (HBENnonCB(Max (9,MaxBens), 4),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating HBENnonCB array.','');
        Return
      End If
    End If

    If (.not. Allocated(flag)) Then
      Allocate (flag(Max (9, MaxBens, lcMaxCombs), 5),stat=iastat)
      If (iastat /= 0) Then
        Call lverror('AllocateComboStructures', 1, iastat,'Error allocating flag array.','');
        Return
      End If
    End If

    If (MenuLoaded) Then
      If (.not. GetMenuItem('menfsovr', MenFSOvr)) Then
        Call exception(999,Trim (LVUtilityErrMsg))
        Return
      End If
    End If

    UseFas35Overrides = .false.
    If (MenFSOvr == 2) Then
      UseFas35Overrides = .true.

      If (.not. Allocated(Fld35)) Then
        Allocate (Fld35(4, Max (9,MaxBens)),stat=iastat)
        If (iastat /= 0) Then
          Call lverror('AllocateComboStructures', 1, iastat,'Error allocating Fld35 array.','');
          Return
        End If
      End If

      If (.not. Allocated(UseFld35)) Then
        Allocate (UseFld35(4, Max (9,MaxBens)),stat=iastat)
        If (iastat /= 0) Then
          Call lverror('AllocateComboStructures', 1, iastat,'Error allocating UseFld35 array.','');
          Return
        End If
      End If
    End If


    result = .true.

    If (DebugCombos) Then
      Call debug ('** Exit from AllocateComboStructures')
    End If

  End Function AllocateComboStructures

!/----------------------------------------------------------------------------------------
!/ Function ReadMenuVarComb reads menu items that relate to the benefit combinations
!/ This was previously done in LVcode, but is now called from ReadMenuVariables in modules
!/----------------------------------------------------------------------------------------

  Function ReadMenuVarComb() Result (result)

  Implicit None

!//  Local variables



!// Local variables
  Integer:: i, j, k, iBenType, BenNum, ServNum, PiecesUsed
  Character (Len=20):: bentype, CharVar, Switch
  Character (Len=80):: SwitchLabel
  Integer:: TempBound, CombineEquiv, CombType, CombSubType, ServRed
  Real (Double):: benpct, svcdim, svclim, fxyrsc
  Logical:: result


  If (DebugCombos) Then
    Call debug ('** Entry into ReadMenuVarComb')
  End If
  TempBound = 0
  i = 0
  j = 0

!// WARNING: If all the variables aren't here, lv3lib will silently produce no output!
  If (UseFas35Overrides) Then
    UseFld35 = .false.
  End If

  BenefitStruct%Use = .False.
  BenefitStruct%CombType = 0
  BenefitStruct%CombSubType = 0
  BenefitStruct%CombineEquiv = 0
  BenefitStruct%BenEquiv = 0
  BenefitStruct%ServRed = 0
  BenefitStruct%svcdim = 0d0
  BenefitStruct%svclim = 0d0
  BenefitStruct%fxyrsc = 0d0
  BenefitStruct%ServNum = 0
  BenefitStruct%BenEquivNum = 0
  BenefitStruct%bentype = 0
  BenefitStruct%ibencomb = 0
  BenefitStruct%BenPct = 1d0
  BenefitStruct%CBadjust = 1d0

  Do k = 1, Ubound (BenefitStruct,2)
    Do i = 1, Ubound (BenefitStruct,1)
      Call InitBenInfo(BenefitStruct(i,k)%Ans, AllValues=.true.)
      Do j = 1, 4
        Call InitBenInfo(BenefitStruct(i,k)%Piece(j), AllValues=.true.)
      End Do  ! j

      BenefitStruct(i,k)%ServRedPct = 1d0
    End Do  ! i
  End Do  ! k

  Do k = 1, Ubound (BenVars,1)
    Call InitBenInfo(BenVars(k), AllValues=.true.)
  End Do  ! k

  Do k = 1, Ubound (FormVars,1)
    Call InitBenInfo(FormVars(k), AllValues=.true.)
  End Do  ! k

  Do k = 1, Ubound (Bft,2) + 1      ! Combs, ValBens(Ret,Dth,Dis,Wth)

    If (k == 1) Then
      TempBound = lcMaxCombs
    Else
      TempBound = MaxBens
    End If

    Do i = 1, TempBound    ! array size of switch

      iBenType = 0
      BenNum = 0
      ServNum = 0
      CombineEquiv = 0
      CombType = 0
      CombSubType = 0
      ServRed = 0
      benpct = 0d0
      svcdim = 0d0
      svclim = 0d0
      fxyrsc = 0d0

!// Need the switch label, too.
      Switch = Trim (mendescrip(k,1)) // '(' // Trim (IntToStr(i, 3)) // ')'
      If (.not. GetMenuItem(Switch, BenefitStruct(i,k)%Use, SwitchLabel)) Then
        If (LVutilityErrMsg(:48) == 'Switch setting not specified as either ON or OFF') Then
          BenefitStruct(i,k)%Use = .false.
        Else
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If
      End If

!// Ignore benefits that are switched on but the label is empty.
      If (len_Trim (SwitchLabel) == 0) Then
        BenefitStruct(i,k)%Use = .false.
      End If

      If (.not. BenefitStruct(i,k)%Use) Then
        Cycle
      End If

      If (k == 1 .and. Debug_On) Then
        If (Combination(i)%Use) Then
          Call debug('   Combination Use = True "' //mendescrip(k,1)//'"(' //inttostr(i,2) //')')
        Else
          Call debug('   Combination Use = False "' //mendescrip(k,1)//'"(' //inttostr(i,2) //')')
        End If
      End If

      If (.not. GetMenuItem('benpct',BenPct,k,i)) Then
        Call exception(999,Trim (LVUtilityErrMsg))
        Return
      End If

!// If user leaves BenPct blank, it is assumed BenPct is 100%
      If (Abs (BenPct) < 0.0011d0) Then
        BenPct = 100d0
      End If

      BenefitStruct(i,k)%BenPct = BenPct / 100d0

      If (k > 1) Then
        If (.not.  GetMenuItem(MenDescrip(k,10),CombineEquiv,i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If
      Else
        CombineEquiv = 1  !// always combine if processing COMB
      End If

      BenefitStruct(i,k)%CombineEquiv = CombineEquiv

      If (CombineEquiv == 1) Then
!// read the combination related menu items

        If (.not. GetMenuItem(MenDescrip(k,2),CombType,i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If

        If (.not. GetMenuItem(MenDescrip(k,CombType+2),CombSubType,i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If

        PiecesUsed = 0
        Do j = 1, 4      ! A,B,C,D
          If (.not. GetMenuItem('bentype',bentype,k,j,i)) Then
            Call exception(999,Trim (LVUtilityErrMsg))
            Return
          End If

          If (.not. GetMenuItem('ibencomb',BenNum,k,j,i)) Then
            Call exception(999,Trim (LVUtilityErrMsg))
            Return
          End If

          If (BenNum == 0) Then
            Cycle
          End If

!// assign correct benefit type value from benefit block drop down boxes
          Call capall(bentype)

          If (Debug_On) Then
            Call Debug('  bentype = ' //bentype)
          End If

          Select Case (Trim (bentype))
          Case ('BENEFIT')
            ibentype = 1
          Case ('FORMULA')
            ibentype = 2
          Case ('COMBINE')
            ibentype = 3
          Case ('RETIREMENT')
            ibentype = 4
          Case ('DEATH')
            ibentype = 5
          Case ('DISABILITY')
            ibentype = 6
          Case ('WITHDRAWAL')
            ibentype = 7
          Case Default
            ibentype = 0
          End Select

          BenefitStruct(i,k)%Piece(j)%PieceType = iBenType
          BenefitStruct(i,k)%Piece(j)%BenNum = BenNum
          PiecesUsed = PiecesUsed + 1
        End Do  ! j

!// only read service reduction variables if that menu option is chosen

        If (.not.  GetMenuItem(MenDescrip(k,9),ServRed,i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If

        If (ServRed > 1) Then
          If (.not. GetMenuItem('isvc',ServNum,k,i)) Then
            Call exception(999,Trim (LVUtilityErrMsg))
            Return
          End If

          If (.not. GetMenuItem('svcdim',svcdim,k,i)) Then
            Call exception(999,Trim (LVUtilityErrMsg))
            Return
          End If

          If (.not. GetMenuItem('svclim',svclim,k,i)) Then
            Call exception(999,Trim (LVUtilityErrMsg))
            Return
          End If

          If (.not. GetMenuItem('fxyrsc',fxyrsc,k,i)) Then
            Call exception(999,Trim (LVUtilityErrMsg))
            Return
          End If
        End If

        BenefitStruct(i,k)%CombType = CombType
        BenefitStruct(i,k)%CombSubType = CombSubType
        BenefitStruct(i,k)%ServRed = ServRed
        BenefitStruct(i,k)%svcdim = svcdim
        BenefitStruct(i,k)%ServNum = ServNum
        BenefitStruct(i,k)%svclim = svclim
        BenefitStruct(i,k)%fxyrsc = fxyrsc

!// Ignore benefits that are switched on but no benefit pieces are defined.
        If (PiecesUsed == 0) Then
          BenefitStruct(i,k)%Use = .false.
        End If

      Else ! CombineEquiv != 1, so not a combination

!// Ignore benefits that are switched on but not included in a plan.  Applies only
!// to benefits defined as equivalent to another.
        If (jpln(i, k - 1) == 0) Then
          BenefitStruct(i,k)%Use = .false.
        End If

        If (.not. GetMenuItem(MenDescrip(k,11),BenefitStruct(i,k)%BenEquiv,i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If

        If (.not. GetMenuItem('ibeneqiv',BenefitStruct(i,k)%BenEquivNum,k,i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        End If

      End If

      If (UseFas35Overrides) Then
        If (.not. GetMenuItem('fld35', CharVar, k-1, i)) Then
          Call exception(999,Trim (LVUtilityErrMsg))
          Return
        Else
          If (len_Trim (CharVar) > 0) Then
            UseFld35(k-1, i) = .true.
          End If
        End If
      End If

    End Do  ! i
  End Do  ! k

  result = .True.

  If (DebugCombos) Then
    Call debug ('** Exit from ReadMenuVarComb')
  End If

End Function ReadMenuVarComb

!/----------------------------------------------------------------------------------------

  Subroutine DeallocateComboStructures ()

    Implicit None

!// Dummy arguments

!//  Local variables


    If (DebugCombos) Then
      Call debug ('** Entry into DeallocateComboStructures')
    End If

!// First, disassociate the array pointers
    Nullify (ValBenefit, Combination)

    If (Allocated(BenefitStruct)) Then
      Deallocate (BenefitStruct, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating BenefitStruct array.','')
      End If
    End If

    If (Allocated(BenVars)) Then
      Deallocate (BenVars, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating BenVars array.','')
      End If
    End If

    If (Allocated(FormVars)) Then
      Deallocate (FormVars, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating FormVars array.','')
      End If
    End If

    If (Allocated(ARETXnonCB)) Then
      Deallocate (ARETXnonCB, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating ARETXnonCB array.','')
      End If
    End If

    If (Allocated(CRETXnonCB)) Then
      Deallocate (CRETXnonCB, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating CRETXnonCB array.','')
      End If
    End If

    If (Allocated(HBENnonCB)) Then
      Deallocate (HBENnonCB, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating HBENnonCB array.','')
      End If
    End If

    If (Allocated(flag)) Then
      Deallocate (flag, stat=iastat)
      If (iastat /= 0) Then
        Call lverror('DeallocateComboStructures', 1, iastat,'Error deallocating flag array.','')
      End If
    End If

    If (DebugCombos) Then
      Call debug ('** Exit from DeallocateComboStructures')
    End If

  End Subroutine DeallocateComboStructures

!/----------------------------------------------------------------------------------------

  Subroutine InitializeCombArrays ()

    Implicit None

!// Dummy arguments

!//  Local variables
    Integer:: i, j, k


    If (DebugCombos) Then
      Call debug ('** Entry into InitializeCombArrays')
    End If

    Do k = 1, Ubound (BenefitStruct,2)
      Do i = 1, Ubound (BenefitStruct,1)
        Call InitBenInfo(BenefitStruct(i,k)%Ans)
        Do j = 1, 4
          Call InitBenInfo(BenefitStruct(i,k)%Piece(j))
        End Do  ! j

        BenefitStruct(i,k)%ServRedPct = 1d0
      End Do  ! i
    End Do  ! k

    Do k = 1, Ubound (BenVars,1)
      Call InitBenInfo(BenVars(k))
    End Do  ! k

    Do k = 1,Ubound (FormVars,1)
      Call InitBenInfo(FormVars(k))
    End Do  ! k

    ARETXnonCB = 0d0
    CRETXnonCB = 0d0
    HBENnonCB  = 0d0

    If (DebugCombos) Then
      Call debug ('** Exit from InitializeCombArrays')
    End If

  End Subroutine InitializeCombArrays

!/----------------------------------------------------------------------------------------

Function CombSet(k, CombineEquiv, BenVariant) Result (result)
!// k: 1-COMB, 2-RET, 3-DTH, 4-DIS, 5-WTH
!// CombineEquiv: 0-COMBINE, 1-EQUIV
!// BenVariant: 1 - Rets 2 = Ret0s, 3 = Ret1s, 4 = ARets, 5 = RetZs

  Implicit None

!// Dummy arguments
  Integer:: k, CombineEquiv, BenVariant
  Logical:: result

!// Constants
  Integer, Parameter:: ProcessCombine=0, ProcessEquiv=1

!// Local variables
  Integer:: i, j, PieceType, BenNum, TempBound
  Real (Double):: TempProjBen

!// PieceType: For an equivalence, PieceType is 1,2,3,4 RET,DTH,DIS,WTH
!//            For a combination, PieceType is
!// 1 - Benefit
!// 2 - Formula
!// 3 - Combination
!// 4 - Retirement
!// 5 - Dth
!// 6 - Dis
!// 7 - Wth


  If (DebugCombos) Then
    Call debug ('** Entry into CombSet')
  End If

  result = .false.

  flag = 0
!//  i is the switch number, k is the type = 1,2,3,4,5 COMB,RET,DTH,DIS,WITH
!//  Asign benefit blocks to the correct combination/valuation benefits

  If (k == Combs) Then
    TempBound = lcMaxCombs
  Else
    TempBound = MaxBens
  End If

!// Calculate benefit combinations based on user's menu selections
  Do i = 1, TempBound
    If (.not. BenefitStruct(i,k)%Use) Then
      Cycle
    End If

!// If processing combinations but benefit is defined as an equivalent then skip it
    If (CombineEquiv == ProcessCombine .and. k /= Combs .and. ValBenefit(i,k-1)%CombineEquiv == 2) Then
      Cycle
    End If

!// If processing equivalents but benefit is defined as a combination then skip it
    If (CombineEquiv == ProcessEquiv) Then
      If (k == Combs) Then
        Cycle
      Else If (ValBenefit(i,k-1)%CombineEquiv == 1) Then
        Cycle
      End If
    End If

    BenefitStruct(i,k)%Ans%PieceType = k + 2
    BenefitStruct(i,k)%Ans%BenNum = i

!// Process Fas35 overrides, if provided
!// These values are overrides to the entire definition so don't need ApplyBenReduction or AgeLoopAdj
    If (k /= Combs .and. BenVariant == ARets .and. UseFas35Overrides) Then

!// Arrays UseFld35 and Fld35 are only Allocated if UseFas35Overrides is true
      If (UseFld35(k-1, i)) Then
        ValBenefit(i, k-1)%Ans%ABenNonCB = Fld35(k-1, i)
        ValBenefit(i, k-1)%Ans%ABenCB    = 0d0
        ValBenefit(i, k-1)%Ans%ARetNonCB = Fld35(k-1, i)
        ValBenefit(i, k-1)%Ans%ARetCB    = 0d0
        ARETXnonCB(i, k-1) = ValBenefit(i, k-1)%Ans%ARetNonCB
        ARetx(i, k-1) = ARETXnonCB(i, k-1)
        Cycle
      End If
    End If

!// processing equivalent benefits
    If (k /= Combs .and. ValBenefit(i,k-1)%CombineEquiv == 2) Then
      PieceType = ValBenefit(i,k-1)%BenEquiv
      BenNum = ValBenefit(i,k-1)%BenEquivNum

      If (PieceType > 0 .and. BenNum > 0) Then
        Call SetBenEqual(ValBenefit(BenNum,PieceType)%Ans,ValBenefit(i,k-1)%Ans, BenVariant)
        Call ApplyBenReduction(i, k, BenVariant)
      Else
        Call InitBenInfo(ValBenefit(i,k-1)%Ans)
      End If
    Else

!// The following is for benefits that are defined as a combination of other benefit blocks and/or benefits

!// This loop goes through each of the benefit combination pieces (A,B,C,D) and combines them
!// according to their type (PieceType)
!// PieceType
!// 1 - Benefit
!// 2 - Formula
!// 3 - Combination
!// 4 - Retirement
!// 5 - Death
!// 6 - Disability
!// 7 - Withdrawal

      Do j = 1, 4
        PieceType = BenefitStruct(i,k)%Piece(j)%PieceType
        BenNum = BenefitStruct(i,k)%Piece(j)%BenNum

        If (PieceType == 0 .or. BenNum == 0) Then
!// No block type, zero out piece
          Call InitBenInfo(BenefitStruct(i,k)%Piece(j))
        Else If (PieceType == 3) Then
!// Set Piece (BenComb) equal to a Combination
           Call SetBenEqual(Combination(BenNum)%Ans,BenefitStruct(i,k)%Piece(j))
        Else If (PieceType > 3) Then
!// Set Piece (BenComb) equal to an already declared ValBenefit
          Call SetBenEqual(ValBenefit(BenNum,PieceType-3)%Ans,BenefitStruct(i,k)%Piece(j))
        Else If (PieceType == 1) Then
!// Set piece equal to a BEN value
          Call SetBenEqual(BenVars(BenNum), BenefitStruct(i,k)%Piece(j))
        Else
!// Seq piece equal to a FORM
          BenefitStruct(i,k)%Piece(j)%ARETnonCB = FormVars(BenNum)%ARETnonCB
          BenefitStruct(i,k)%Piece(j)%HBENnonCB = FormVars(BenNum)%HBENnonCB
          BenefitStruct(i,k)%Piece(j)%CRETnonCB = FormVars(BenNum)%CRETnonCB
          BenefitStruct(i,k)%Piece(j)%ProjBenNonCB = FormVars(BenNum)%ProjBenNonCB
          BenefitStruct(i,k)%Piece(j)%Ben0nonCB = FormVars(BenNum)%Ben0nonCB
          BenefitStruct(i,k)%Piece(j)%Ben1nonCB = FormVars(BenNum)%Ben1nonCB
          BenefitStruct(i,k)%Piece(j)%BenZnonCB = FormVars(BenNum)%BenZnonCB
          BenefitStruct(i,k)%Piece(j)%ABenNonCB = FormVars(BenNum)%ABenNonCB
        End If
      End Do  ! j

!// Set combination as prescribed in MNU
      Call CalcAns(BenefitStruct(i,k), BenVariant)

      Write (ErrMessage,'("CombSet, at age ",i2," @ k = ",i1," @ i = ",i2, ", for type = ", i1)') iLoopAge, k, i, BenVariant
      Call CheckNDP(Trim (ErrMessage) // ', after CalcAns')
      If (ierrcode /= 0) Then
        Return
      End If

      Call ApplyBenReduction(i, k, BenVariant)

      Call CheckNDP(Trim (ErrMessage) // ', after ApplyBenReduction')
      If (ierrcode /= 0) Then
      Return
      End If
    End If !/ Equivalence vs. Combination if statement


!// Only apply this for the calculations that occur in BenAtI. If the BenVariant indicates
!// it was called from CombSetFAS then those arrays are redefined there.
    If (BenVariant < Ret0s) Then
!// Set COMB array and temporary values (non-annuitized Cash Balance benefits) for valuation benefit arrays
      TempProjBen = BenefitStruct(i,k)%Ans%ProjBenCB + BenefitStruct(i,k)%Ans%ProjBenNonCB
      If (k == Combs) Then
        COMB(i) = TempProjBen
      Else
        BFT(i,k-1) = TempProjBen
      End If
    End If

!// If BenVariant is Retz then the call came from entry Answrs, which is after the age loop so don't Call AgeLoopAdj.
    If (k > Combs .and. BenVariant /= RetZs) Then
      Call AgeLoopAdj(i, k - 1, BenVariant)
      Call CheckNDP(Trim (ErrMessage) // ', after AgeLoopAdj. Check 415.')
      If (ierrcode /= 0) Then
        Return
      End If
     End If

  End Do  ! i

  result = .true.

  If (DebugCombos) Then
    Call debug ('** Exit from CombSet')
  End If

End Function CombSet

!/----------------------------------------------------------------------------------------

Subroutine CombSetFAS (k,BenVariant)

!// k: 1-COMB, 2-RET, 3-DTH, 4-DIS, 5-WTH
!// BenVariant: code that tells system to process original, accrued, projected or FAS benefits

!// This procedure is only called from entry SchedB or Answrs during LVval runs.  No need to make
!// the array assignments product specific.

  Implicit None

!// Dummy arguments
  Integer:: k, TempBound, i, BenVariant

!// Local variables


  If (DebugCombos) Then
    Call debug ('** Entry into CombSetFAS')
  End If

  If (k == 1) Then
    TempBound = lcMaxCombs
  Else
    TempBound = MaxBens
  End If

!// First, calculate the appropriate values based on the menu settings, processing
!// combinations then equivalent benefits
  Do i = 0, 1
    If (.not. CombSet(k, i, BenVariant)) Then
      Return
    End If
  End Do  ! i

  Do i = 1, TempBound
    Select Case (BenVariant)
    Case (Ret0s)
      If (k == Combs) Then
        Comb0(i) = BenefitStruct(i,k)%Ans%Ben0nonCB + BenefitStruct(i,k)%Ans%Ben0CB
      Else
        Ret0x(i,k-1) = BenefitStruct(i,k)%Ans%Ben0nonCB + BenefitStruct(i,k)%Ans%Ben0CB * BenefitStruct(i,k)%CBadjust
      End If
    Case (Ret1s)
      If (k == Combs) Then
        Comb1(i) = BenefitStruct(i,k)%Ans%Ben1nonCB + BenefitStruct(i,k)%Ans%Ben1CB
       Else
        Ret1x(i,k-1) = BenefitStruct(i,k)%Ans%Ben1nonCB + BenefitStruct(i,k)%Ans%Ben1CB * BenefitStruct(i,k)%CBadjust
      End If
    Case (ARets)
      If (k == Combs) Then
        AComb(i) = BenefitStruct(i,k)%Ans%ABenNonCB + BenefitStruct(i,k)%Ans%ABenCB
      Else
        ARetx(i,k-1) = BenefitStruct(i,k)%Ans%ABenNonCB + BenefitStruct(i,k)%Ans%ABenCB * BenefitStruct(i,k)%CBadjust
        VRetx(i,k-1) = ARetx(i,k-1)
      End If
    Case (RetZs)
      If (k == Combs) Then
        CombZ(i) = BenefitStruct(i,k)%Ans%BenZNonCB + BenefitStruct(i,k)%Ans%BenZCB
      Else If (k == Rets) Then
        RetZ(i) = BenefitStruct(i,k)%Ans%BenZnonCB + BenefitStruct(i,k)%Ans%BenZCB * BenefitStruct(i,k)%CBAdjust
      Else If (k == Dths) Then
        DthZ(i) = BenefitStruct(i,k)%Ans%BenZnonCB + BenefitStruct(i,k)%Ans%BenZCB * BenefitStruct(i,k)%CBAdjust
      Else If (k == Disbs) Then
        DisZ(i) = BenefitStruct(i,k)%Ans%BenZnonCB + BenefitStruct(i,k)%Ans%BenZCB * BenefitStruct(i,k)%CBAdjust
      Else If (k == Wths) Then
        WthZ(i) = BenefitStruct(i,k)%Ans%BenZnonCB + BenefitStruct(i,k)%Ans%BenZCB * BenefitStruct(i,k)%CBAdjust
      End If
    Case Default
    End Select
  End Do  ! i

  If (DebugCombos) Then
    Call debug ('** Exit from CombSetFas')
  End If

End Subroutine CombSetFAS

!/----------------------------------------------------------------------------------------

Subroutine CombFinal (CombEquivType)

!// CombFinal routine is used to reset the non-CB projected benefits to the value of the COMB/BFT
!// arrays in case user modified such values through EPP

    Implicit None

!// Dummy arguments
    Integer:: CombEquivType

!// Local variables
    Integer:: i, k, CBNum
    Character (Len=80):: ErrMessage
    Logical:: NonCBOnly


    If (DebugCombos) Then
      Call debug ('** Entry into CombFinal')
    End If

    flag = 0

    Call SaveBenInfo
    Call SaveFormInfo

!// Save any COMBs that changed
    Do i = 1, lcMaxCombs
      If (Abs (BenefitStruct(i,1)%Ans%ProjBenNonCB + BenefitStruct(i,1)%Ans%ProjBenCB - COMB(i)) > 0.001d0) Then
        BenefitStruct(i,1)%Ans%ProjBenNonCB = COMB(i)
        BenefitStruct(i,1)%Ans%ProjBenCB = 0d0
      End If
    End Do  ! i

!// If called from entry Comb,b then don't need to process valuation benefits
    If (CombEquivType == 1) Then
      Return
    End If

!// Save and Rets, Dths, Dis, or Wths that changed
    Do k = 2, 5
      Do i = 1, MaxBens
!// Now make sure that not all ValBenefits are looked at (only equivalences or combinations)
        If ((CombEquivType == 2 .and. ValBenefit(i,k-1)%CombineEquiv == 1) .or. CombEquivType == 3) Then

!// Only change the projected benefit values if there is a difference (most likely due to EPP)

!// Do not update non-CB value if processing cash balance under new method
          NonCBOnly = .true.
          If (BenefitStruct(i,k)%Ans%CBBenNum > 0) Then
            CBNum = BenefitStruct(i,k)%Ans%CBBenNum
            If (MenuValuesCBUA(CBNum)%CashBalOld == 0) Then
              NonCBOnly = .false.
            End If
          End If

!// In case the new Cash Balance benefits are included, and EPP is used to change the
!// Valuation Benefit, either the Cash Balance portion remains as calculated and the difference
!// is added to the NonCB part of the benefit or the entire amount is put into the CB portion.
!//                                                                             !
!// -- It should be noted in the Release Notes, LynchVal variable list, and/or Help Screens
!// that in general, the new Cash Balance benefits only work on their own.  Applying EPP
!// to the valuation benefits that are dependent on such benefits OR combining them with
!// non-CB benefits may not produce desired results
          If (Abs (BenefitStruct(i,k)%Ans%ProjBenNonCB + BenefitStruct(i,k)%Ans%ProjBenCB - BFT(i,k-1)) > 0.001d0) Then

            If (NonCBOnly) Then
              flag(i,k) = 1
              BenefitStruct(i,k)%Ans%ProjBenNonCB = bft(i, k-1)
              BenefitStruct(i,k)%Ans%ProjBenCB = 0d0
            End If
          End If
        End If
      End Do  ! i
    End Do  ! k

    Do k = 2, 5
      Do i = 1, MaxBens
        If (flag(i,k) == 1) Then
          Write (ErrMessage,'("CombFinal, at age ",i2," @ k = ",i1," @ i = ",i2)') iLoopAge, k, i
          Call CheckNDP(Trim (ErrMessage) // ', before AgeLoopAdj')
          If (ierrcode /= 0) Then
            Return
          End If

!// AgeLoopAdj is only called again (remember, it is done in CombSet) if there
!// was a change picked up in the projected benefit
          Call AgeLoopAdj(i, k - 1, 1)

          Call CheckNDP(Trim (ErrMessage) // ', after AgeLoopAdj. Check 415.')
          If (ierrcode /= 0) Then
            Return
          End If
        End If
      End Do  ! i
    End Do  ! k

  If (DebugCombos) Then
    Call debug ('** Exit from CombFinal')
  End If

End Subroutine CombFinal

!/----------------------------------------------------------------------------------------

Subroutine CombFinalFAS (BenVariant)

!// k: 1-COMB, 2-RET, 3-DTH, 4-DIS, 5-WTH
!// BenVariant: code that tells system to process original, accrued, projected or FAS benefits

!// CombFinalFAS routine is used to reset the non-CB projected benefits to the value of for
!// FAS benefit arrays in case user modified such values through EPP

  Implicit None

!// Dummy arguments
  Integer:: BenVariant

!//  Local variables
  Integer:: k, TempBound, i, CBNum
  Real (Double):: NewValue, CurrValue
  Logical:: NonCBOnly


  If (DebugCombos) Then
    Call debug ('** Entry into CombFinalFAS')
  End If

  Call SaveBenInfo
  Call SaveFormInfo

  Do k = Combs, Wths

    If (k == 1) Then
      TempBound = lcMaxCombs
    Else
      TempBound = MaxBens
    End If

!// Flag used to indicate that AgeLoopAdj needs to be called for a benefit that was
!// modified via EPP.  Flag is not set for k = Combs since AgeLoopAdj does not apply.
    flag = 0

!// Calculate benefit combinations based on user's EPP
    Do i = 1, TempBound
!// Do not update non-CB value if processing cash balance under new method
      NonCBOnly = .true.
      If (BenefitStruct(i,k)%Ans%CBBenNum > 0) Then
        CBNum = BenefitStruct(i,k)%Ans%CBBenNum
        If (MenuValuesCBUA(CBNum)%CashBalOld == 0) Then
          NonCBOnly = .false.
        End If
      End If

      If (.not. NonCBOnly) Then
        Cycle
      End If

      Select Case (BenVariant)
      Case (Ret0s)
        CurrValue = BenefitStruct(i,k)%Ans%Ben0nonCB + BenefitStruct(i,k)%Ans%Ben0CB

        If (k == Combs) Then
          NewValue = Comb0(i)
        Else
          NewValue = Ret0x(i,k-1)
        End If

        If (Abs (CurrValue - NewValue) > 0.001d0) Then
          BenefitStruct(i,k)%Ans%Ben0nonCB = NewValue
          BenefitStruct(i,k)%Ans%Ben0CB = 0d0

          If (k > Combs ) Then
            flag(i,k) = 1
          End If
        End If

      Case (Ret1s)
        CurrValue = BenefitStruct(i,k)%Ans%Ben1nonCB + BenefitStruct(i,k)%Ans%Ben1CB

        If (k == Combs) Then
          NewValue = Comb1(i)
        Else
          NewValue = Ret1x(i,k-1)
        End If

        If (Abs (CurrValue - NewValue) > 0.001d0) Then
          BenefitStruct(i,k)%Ans%Ben1nonCB = NewValue
          BenefitStruct(i,k)%Ans%Ben1CB = 0d0

          If (k > Combs ) Then
            flag(i,k) = 1
          End If
        End If

      Case (ARets)
        CurrValue = BenefitStruct(i,k)%Ans%ABennonCB + BenefitStruct(i,k)%Ans%ABenCB

        If (k == Combs) Then
          NewValue = AComb(i)
        Else
          NewValue = ARetx(i,k-1)
        End If

        If (Abs (CurrValue - NewValue) > 0.001d0) Then
          BenefitStruct(i,k)%Ans%ABennonCB = NewValue
          BenefitStruct(i,k)%Ans%ABenCB = 0d0

          If (k > Combs ) Then
            flag(i,k) = 1
          End If
        End If

      Case (RetZs)
        CurrValue = BenefitStruct(i,k)%Ans%BenZnonCB + BenefitStruct(i,k)%Ans%BenZCB

        Select Case (k)
        Case (Combs)
          NewValue = CombZ(i)
        Case (Rets)
          NewValue = RetZ(i)
        Case (Dths)
          NewValue = DthZ(i)
        Case (Disbs)
          NewValue = DisZ(i)
        Case (Wths)
          NewValue = WthZ(i)
        End Select

        If (Abs (CurrValue - NewValue) > 0.001d0) Then
          BenefitStruct(i,k)%Ans%BenZnonCB = NewValue
          BenefitStruct(i,k)%Ans%BenZCB = 0d0

          If (k > Combs ) Then
            flag(i,k) = 1
          End If
        End If

      End Select
    End Do  ! i  !// end of benefit loop, process all before calculating AgeLoopAdj


! AgeLoopAdj is only called again if there was a change picked up in the benefit
    Do i = 1, TempBound
      If (flag(i,k) == 1) Then
        Write (ErrMessage,'("CombFinalFas, at age ",i2," @ k = ",i1," @ i = ",i2, ", for type = ",i1)') iLoopAge, k, i, BenVariant
        Call CheckNDP(Trim (ErrMessage) // ', before AgeLoopAdj')
        If (ierrcode /= 0) Then
          Return
        End If

!// If BenVariant is Retz then the call came from entry Answrs, which is after the age loop so don't Call AgeLoopAdj.
        If (BenVariant /= RetZs) Then
          Call AgeLoopAdj(i, k - 1, BenVariant)
        End If

        Call CheckNDP(Trim (ErrMessage) // ', after AgeLoopAdj. Check 415.')
        If (ierrcode /= 0) Then
          Return
        End If
      End If

    End Do  ! i

  End Do  ! k = Combs, Wths

  If (DebugCombos) Then
    Call debug ('** Exit from CombFinalFAS')
  End If

End Subroutine CombFinalFAS

!/----------------------------------------------------------------------------------------

Subroutine CalcAns (C, BenVariant)

  Implicit None

!// Dummy arguments
  Type (tCombMenu):: C ! For 'C'ombo
  Integer:: BenVariant

!// Local variables
  Type(tBenInfo) :: AA, BB, CC, DD, Ans
  Integer:: CombSubType, PieceCount


  If (DebugCombos) Then
    Call debug ('** Entry into CalcAns')
  End If


!// Use a local copies of the structure pieces to pass into the calculation Functions.
!// After the calculations only the desired components are saved to the actual structure.
!// This allows control over the components that need to be calculated based on the type of
!// calculation without compromising the results of the other components.
  CombSubType = C%CombSubType
  Call InitBenInfo(Ans, AllValues=.true.)
  Call SetBenEqual(C%Ans, Ans)
  Call InitBenInfo(AA, AllValues=.true.)
  Call InitBenInfo(BB, AllValues=.true.)
  Call InitBenInfo(CC, AllValues=.true.)
  Call InitBenInfo(DD, AllValues=.true.)

!// Check the number of arguments provided.  If only one or two then ignore the CombSubType and use the default calculation.
  PieceCount = 0
  If (C%Piece(1)%BenNum /= 0) Then
    PieceCount = PieceCount + 1
    Call SetBenEqual(C%Piece(1), AA)
  End If

  If (C%Piece(2)%BenNum /= 0) Then
    PieceCount = PieceCount + 1

    If (PieceCount == 1) Then
      Call SetBenEqual(C%Piece(2), AA)
    Else
      Call SetBenEqual(C%Piece(2), BB)
    End If
  End If

  If (C%Piece(3)%BenNum /= 0) Then
    PieceCount = PieceCount + 1

    If (PieceCount == 1) Then
      Call SetBenEqual(C%Piece(3), AA)
    Else If (PieceCount == 2) Then
      Call SetBenEqual(C%Piece(3), BB)
    Else
      Call SetBenEqual(C%Piece(3), CC)
    End If
  End If

  If (C%Piece(4)%BenNum /= 0) Then
    PieceCount = PieceCount + 1

    If (PieceCount == 1) Then
      Call SetBenEqual(C%Piece(4), AA)
    Else If (PieceCount == 2) Then
      Call SetBenEqual(C%Piece(4), BB)
    Else If (PieceCount == 3) Then
      Call SetBenEqual(C%Piece(4), CC)
    Else
      Call SetBenEqual(C%Piece(4), DD)
    End If
  End If

  If (PieceCount <= 2) Then
    CombSubType = 1
  End If

  If (PieceCount == 1) Then
    Call SetBenEqual(AA, Ans)
  Else

    Select Case (C%CombType)
    Case (1)                      ! Add
      Select Case (CombSubType)
      Case (1)
        Ans = CombosADD(AA, BB, CC, DD)
      Case (2)
        Ans = CombosADD(AA,CombosMax(BB,CC,DD))
      Case (3)
        Ans = CombosADD(AA,CombosMin(BB,CC,DD))
      Case Default
        Ans = CombosADD(AA,BB,CC,DD)
      End Select

    Case (2)                      ! Subtract
      Select Case (CombSubType)
      Case (1)
        Ans = CombosDIFF(AA,BB)
      Case (2)
        Ans = CombosDIFF(CombosADD(AA,BB),CC)
      Case (3)
        Ans = CombosDIFF(CombosDIFF(AA,BB),CC)
      Case (4)
        Ans = CombosDIFF(CombosADD(AA,BB), CombosADD(CC,DD))
      Case (5)
        Ans = CombosDIFF(CombosADD(AA,BB),CombosDIFF(CC,DD))
      Case (6)
        Ans = CombosDIFF(AA,CombosMAX(BB,CC,DD))
      Case (7)
        Ans = CombosDIFF(AA,CombosMIN(BB,CC,DD))
      Case (8)
        Ans = CombosDIFF(CombosMAX(AA,BB),CC)
      Case Default
        Ans = CombosDIFF(AA,BB)
      End Select

    Case (3)                      !  Max
      Select Case (CombSubType)
      Case (1)
        Ans = CombosMAX(AA,BB,CC,DD)
      Case (2)
        Ans = CombosMAX(AA,CombosADD(BB,CC,DD))
      Case (3)
        Ans = CombosMAX(CombosADD(AA,BB),CombosADD(CC,DD))
      Case (4)
        Ans = CombosMAX(AA,CombosDIFF(BB,CombosDIFF(CC,DD)))
      Case (5)
        Ans = CombosMAX(CombosDIFF(AA,BB),CombosDIFF(CC,DD))
      Case (6)
        Ans = CombosMAX(CombosDIFF(AA,BB),CombosADD(CC,DD))
      Case (7)
        Ans = CombosMAX(AA,CombosMIN(BB,CC,DD))
      Case (8)
        Ans = CombosMAX(CombosADD(AA,BB),CombosMIN(CC,DD))
      Case (9)
        Ans = CombosMAX(CombosDIFF(AA,BB),CombosMIN(CC,DD))
      Case Default
        Ans = CombosMAX(AA,BB,CC,DD)
      End Select

    Case (4)                      !  Min
      Select Case (CombSubType)
      Case (1)
        Ans = CombosMIN(AA,BB,CC,DD)
      Case (2)
        Ans = CombosMIN(AA,CombosADD(BB,CC,DD))
      Case (3)
        Ans = CombosMIN(CombosADD(AA,BB),CombosADD(CC,DD))
      Case (4)
        If (PieceCount == 3) Then
          Ans = CombosMIN(AA,CombosDIFF(BB,CC))
        Else
          Ans = CombosMIN(AA,CombosDIFF(BB,CombosDIFF(CC,DD)))
        End If
      Case (5)
        If (PieceCount == 3) Then
          Ans = CombosMIN(CombosDIFF(AA,BB), CC)
        Else
          Ans = CombosMIN(CombosDIFF(AA,BB),CombosDIFF(CC,DD))
        End If
      Case (6)
        Ans = CombosMIN(AA,CombosMAX(BB,CC,DD))
      Case (7)
        Ans = CombosMIN(CombosADD(AA,BB),CombosDIFF(CC,DD))
      Case (8)
        If (PieceCount == 3) Then
          Ans = CombosMax(CombosDIFF(AA,BB),CC)
        Else
          Ans = CombosMIN(CombosMax(CombosDIFF(AA,BB),CC),DD)
        End If
      Case Default
        Ans = CombosMIN(AA,BB,CC,DD)
      End Select

    Case (5)                      !  Multiply
      Select Case (CombSubType)
      Case (1)
        Ans = CombosMULT(AA,BB,CC,DD)
      Case (2)
        Ans = CombosMULT(AA,CombosADD(BB,CC,DD))
      Case (3)
        If (PieceCount == 3) Then
          Ans = CombosMULT(AA, CombosDIFF(BB,CC))
        Else
          Ans = CombosMULT(AA, CombosDIFF(CombosADD(BB,CC), DD))
        End If
      Case (4)
        Ans = CombosMULT(AA,CombosMAX(BB,CC,DD))
      Case (5)
        Ans = CombosMULT(AA,CombosMIN(BB,CC,DD))
      Case Default
        Ans = CombosMULT(AA,BB,CC,DD)
      End Select

    Case (6)                      !  Divide
      Select Case (CombSubType)
      Case (1)
        If (PieceCount == 2) Then
          Ans = CombosDIV(AA,BB)
        Else If (PieceCount == 3) Then
          Ans = CombosDIV(CombosADD(AA,BB),CC)
        Else
          Ans = CombosDIV(CombosADD(AA,BB,CC),DD)
        End If
      Case (2)
        If (PieceCount == 3) Then
          Ans = CombosDIV(CombosDIFF(AA,BB),CC)
        Else
          Ans = CombosDIV(CombosDIFF(CombosADD(AA,BB),CC),DD)
        End If
      Case (3)
        If (PieceCount == 3) Then
          Ans = CombosDIV(CombosMAX(AA,BB),CC)
        Else
          Ans = CombosDIV(CombosMAX(AA,BB,CC),DD)
        End If
      Case (4)
        If (PieceCount == 3) Then
          Ans = CombosDIV(CombosMIN(AA,BB),CC)
        Else
          Ans = CombosDIV(CombosMIN(AA,BB,CC),DD)
        End If
      Case Default
        If (PieceCount == 2) Then
          Ans = CombosDIV(AA,BB)
        Else If (PieceCount == 3) Then
          Ans = CombosDIV(CombosADD(AA,BB),CC)
        Else
          Ans = CombosDIV(CombosADD(AA,BB,CC),DD)
        End If
      End Select

    Case Default
      Select Case (CombSubType)
      Case (1)
        Ans = CombosADD(AA,BB,CC,DD)
      Case (2)
        Ans = CombosADD(AA,CombosMax(BB,CC,DD))
      Case (3)
        Ans = CombosADD(AA,CombosMin(BB,CC,DD))
      Case Default
        Ans = CombosADD(AA,BB,CC,DD)
      End Select
    End Select
  End If

!// Selectively update only the components that are desired based on the type of calculation.

  Select Case (BenVariant)
  Case (Ret0s)
    C%Ans%Ben0CB    = Ans%Ben0CB
    C%Ans%Ben0nonCB = Ans%Ben0nonCB
    C%Ans%HBENcb    = Ans%HBENcb
    C%Ans%HBENnoncb = Ans%HBENnoncb
  Case (Ret1s)
    C%Ans%Ben1CB    = Ans%Ben1CB
    C%Ans%Ben1nonCB = Ans%Ben1nonCB
    C%Ans%CRETcb    = Ans%CRETcb
    C%Ans%CRETnoncb = Ans%CRETnoncb
  Case (ARets)
    C%Ans%ABenCB    = Ans%ABenCB
    C%Ans%ABennonCB = Ans%ABennonCB
    C%Ans%ARETcb    = Ans%ARETcb
    C%Ans%ARETnoncb = Ans%ARETnoncb
  Case (RetZs)
    C%Ans%BenZCB    = Ans%BenZCB
    C%Ans%BenZnonCB = Ans%BenZnonCB
  Case Default
    C%Ans%ProjBenCB    = Ans%ProjBenCB
    C%Ans%ProjBenNonCB = Ans%ProjBenNonCB
  End Select

  C%Ans%CBBenNum         = Ans%CBBenNum
  C%Ans%CBbalLS          = Ans%CBbalLS
  C%Ans%BALICR           = Ans%BALICR
  C%Ans%SvcAmt           = Ans%SvcAmt
  C%Ans%CashBalERFmethod = Ans%CashBalERFmethod


  If (DebugCombos) Then
    Call debug ('** Exit from CalcAns')
  End If

 End Subroutine CalcAns

!/----------------------------------------------------------------------------------------

Subroutine ApplyBenReduction (i, k, BenVariant)

!// Applies benefit percentage and specified service limitations to the tCombMenu's ans benefit block
!// This is the second step of calculating a benefit block, the first step calculates the benefit variables
!// Based on no benefit or service reductions

  Implicit None

!// Dummy arguments
  Integer:: i, k, BenVariant

!// Constants
  Integer, Parameter:: AtLoopAge=1, AtValAge=2, AtValAgePlus1=3

  Include 'Params.INC'

!// Local variables
  Real (Double):: recip
  Integer:: iLimLoopAge, j


  If (DebugCombos) Then
    Call debug ('** Entry into ApplyBenReduction')
  End If

  iLimLoopAge = Min (ipMaxActAge, Max (ipMinActAge, iLoopAge))

  Do j = 1, RetZs ! BEN, BEN0, BEN1, ABEN, BENZ

!// BenVariant = 9 indicates to process all benefit types.  This is done for benefits
!// defined as equivalences.  For benefits defined as combinations, only want to process
!// the specified type of benefit.
    If (BenVariant < 9 .and. j /= BenVariant) Then
      Cycle
    End If

!// Determine the type of service used in proration per benefit basis
    Select Case (j)
      Case (:1)
        BenefitStruct(i,k)%Ans%SvcAmt = TCSIX(BenefitStruct(i,k)%ServNum)
      Case (Ret0s)
        BenefitStruct(i,k)%Ans%SvcAmt = PCSX(BenefitStruct(i,k)%ServNum)
      Case (Ret1s)
        BenefitStruct(i,k)%Ans%SvcAmt = PCS1X(BenefitStruct(i,k)%ServNum)
      Case (ARets)
        BenefitStruct(i,k)%Ans%SvcAmt = PCSX(BenefitStruct(i,k)%ServNum)
      Case (RetZs)
        BenefitStruct(i,k)%Ans%SvcAmt = TCSX(BenefitStruct(i,k)%ServNum)
     End Select

!// Code added to include SVCLIM and SVCDIM variables in the numerator
    BenefitStruct(i,k)%Ans%SvcAmt = Dim(BenefitStruct(i,k)%Ans%SvcAmt, BenefitStruct(i,k)%svcdim)
    If ((BenefitStruct(i,k)%Ans%SvcAmt > BenefitStruct(i,k)%svclim) .and. (BenefitStruct(i,k)%svclim  > 0.001d0)) Then
      BenefitStruct(i,k)%Ans%SvcAmt = BenefitStruct(i,k)%svclim
    End If

!// Compute percentage of benefit based on service limitation specified in menu
    Select Case (BenefitStruct(i,k)%ServRed)
      Case (1) ! no limit
        BenefitStruct(i,k)%ServRedPct(j, AtLoopAge) = 1d0
      Case (2) ! Multiply by TCSI/Max (TCS,yrs)
        If (BenefitStruct(i,k)%svclim > 0.001d0 .and. BenefitStruct(i,k)%svclim < 69.999d0) Then
          BenefitStruct(i,k)%ServRedPct(j, AtLoopAge) = BenefitStruct(i,k)%Ans%SvcAmt *           &
                                                        Recip (Max (Min (Dim(TCSX(BenefitStruct(i,k)%ServNum),&
                                                                          BenefitStruct(i,k)%svcdim),&
                                                                  BenefitStruct(i,k)%svclim), &
                                                              BenefitStruct(i,k)%fxyrsc))
        Else
          BenefitStruct(i,k)%ServRedPct(j, AtLoopAge) = BenefitStruct(i,k)%Ans%SvcAmt *           &
                                                        Recip (Max (Dim(TCSX(BenefitStruct(i,k)%ServNum), &
                                                                      BenefitStruct(i,k)%svcdim),    &
                                                                  BenefitStruct(i,k)%fxyrsc))
        End If
      Case (3) ! Multiply by Min (TCSI,yrs)/yrs
        BenefitStruct(i,k)%ServRedPct(j, AtLoopAge) = Min (BenefitStruct(i,k)%Ans%SvcAmt,          &
                                                          BenefitStruct(i,k)%fxyrsc) *            &
                                                      Recip (BenefitStruct(i,k)%fxyrsc)
      Case Default
!// This will happen for equivalent benefits, initialize to 1 since using benefit
!// amount after service reduction (if any) has been applied to original benefit.
        BenefitStruct(i,k)%ServRedPct(j, AtLoopAge) = 1d0
    End Select

    If (iLimLoopAge <= iValAge) Then
      BenefitStruct(i,k)%ServRedPct(j, AtValAge) = BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
    Else If (iLimLoopAge <= iValAge + 1) Then
      BenefitStruct(i,k)%ServRedPct(j, AtValAgePlus1) = BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
    End If

!// Apply benefit and service limitations to all benefit variables
    Select Case (j)
      Case (:1)
        BenefitStruct(i,k)%Ans%ProjBenNonCB = BenefitStruct(i,k)%Ans%ProjBenNonCB *       &
                                              BenefitStruct(i,k)%BenPct *                 &
                                              BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
        BenefitStruct(i,k)%Ans%ProjBenCB    = BenefitStruct(i,k)%Ans%ProjBenCB     *      &
                                              BenefitStruct(i,k)%BenPct *                 &
                                              BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
      Case (Ret0s)
        BenefitStruct(i,k)%ANS%HBENnonCB = BenefitStruct(i,k)%Ans%HBENnonCB     *         &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtValAge)
        BenefitStruct(i,k)%ANS%HBENcb =    BenefitStruct(i,k)%Ans%HBENcb        *         &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtValAge)

        BenefitStruct(i,k)%Ans%Ben0nonCB = BenefitStruct(i,k)%Ans%Ben0nonCB       *       &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
        BenefitStruct(i,k)%Ans%Ben0CB    = BenefitStruct(i,k)%Ans%Ben0CB             *    &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
      Case (Ret1s)
        BenefitStruct(i,k)%Ans%CRETnonCB = BenefitStruct(i,k)%Ans%CRETnonCB     *         &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtValAgePlus1)
        BenefitStruct(i,k)%Ans%CRETcb =    BenefitStruct(i,k)%Ans%CRETcb        *         &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtValAgePlus1)

        BenefitStruct(i,k)%Ans%Ben1nonCB = BenefitStruct(i,k)%Ans%Ben1nonCB       *       &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
        BenefitStruct(i,k)%Ans%Ben1CB =    BenefitStruct(i,k)%Ans%Ben1CB             *    &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
      Case (ARets)
        BenefitStruct(i,k)%Ans%ARETNonCB = BenefitStruct(i,k)%Ans%ARETnonCB     *         &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtValAge)
        BenefitStruct(i,k)%Ans%ARETcb =    BenefitStruct(i,k)%Ans%ARETcb        *         &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtValAge)

        BenefitStruct(i,k)%Ans%ABennonCB = BenefitStruct(i,k)%Ans%ABennonCB       *       &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
        BenefitStruct(i,k)%Ans%ABenCB =    BenefitStruct(i,k)%Ans%ABenCB             *    &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
      Case (RetZs)
        BenefitStruct(i,k)%Ans%BenZnonCB = BenefitStruct(i,k)%Ans%BenZnonCB       *       &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
        BenefitStruct(i,k)%Ans%BenZCB =    BenefitStruct(i,k)%Ans%BenZCB             *    &
                                           BenefitStruct(i,k)%BenPct *                    &
                                           BenefitStruct(i,k)%ServRedPct(j, AtLoopAge)
    End Select

  End Do  ! j

  If (DebugCombos) Then
    Call debug ('** Exit from ApplyBenReduction')
  End If

End Subroutine ApplyBenReduction

!/----------------------------------------------------------------------------------------

Subroutine AgeLoopAdj (iben, itype, BenVariant)

  Use Globals
  Use GblBens
  Use BenLimit, Only: Calc415Limits, Precise415Calc, FormAdjFactor415

  Implicit None

! Dummy arguments
  Integer:: itype, iben, BenVariant


!// Global variables
  Real (8), Dimension(6,3):: pvpayc
  Real (8):: gro
  Common /pvpay/ pvpayc, gro

  Integer :: It, J1, J2, Istr, Kfreq, Lmes, Mayy10
  Integer, Dimension(3) :: Ks
  Common /SPLITCN/ It, Ks, J1, J2, Istr, Kfreq, Lmes, Mayy10

!// Function var
  Real (Double):: recip, VSelp

!//  Local variables
  Integer:: AnnuityType, InterestAge, ConvAge, GetLSDeferralAge, JDIS, CheckAgeForFAS
  Real (Double):: BALICR, ConvFactor, RedFactor, tPx, vint, Age

!// ERF declaration added for Cash Balance EFF Functionality, same declarations are in lvcode for the Subroutine PLAN
  Real(Double), Dimension(20):: ErfSS, ErfDefs, ErfDefrs, ErfDefds, ErfDefis
  Common /ERFSScommon/ ErfSS, ErfDefs, ErfDefrs, ErfDefds, ErfDefis


  If (DebugCombos) Then
    Call debug ('** Entry into AgeLoopAdj')
  End If

  AnnuityType = jann(iben,itype)
  ValBenefit(iben,itype)%CBadjust = 1.0d0

  If (AnnuityType == 0) Then
    Return
  End If

  If (itype == 3) Then
    jdis = 2
  Else
    jdis = 1
  End If

!// Need to use the correct as of age when filling the HBen and Cret arrays.  If the
!// FAS 35 results show the Current accrual instead of the Old unit credit the need the age prior to valuation.
  If (Mayy10 == 2) Then
    CheckAgeForFAS = iValAge
  Else
    CheckAgeForFAS = iValAge - 1
  End If

  If (product /= 'LVADMIN') Then
!// Cash Balance benefits have the option of being projected with the interest
!// crediting rate to NRA unless the payout is the balance itself
    balicr = ValBenefit(iben,itype)%Ans%balicr

    If (ProcessLumpSums) Then
      ConvAge = Max (iLoopAge, GetLSDeferralAge(1, iLoopAge, iben, itype, AnnuityType))
    Else
      If (AnnuityType == 1) Then
        ConvAge =  iLoopAge
      Else
        ConvAge = Max (iLoopAge,jdefn(iben,itype))
      End If
    End If

    If (CBUACount > 0 .and. ValBenefit(iben,itype)%Ans%CBBenNum > 0 .and. Allocated(MenuValuesCBUA) .and. &
        ValBenefit(iben,itype)%Ans%CBBenNum <= lcMaxBenBlks) Then
!// Only apply ERFs to Cash Balance benefits projected to NRA.  Therefore, the ERF will have
!// to be backed out of the Cash Balance portion of the benefit given CashBalERFMethod = 0
      If (ValBenefit(iben,itype)%Ans%CashBalERFMethod == 0) Then
        InterestAge = ConvAge
        If (iErf04(iben,itype) > 0) Then
          RedFactor = erfss(iErf04(iben,itype))
        Else
          RedFactor = 1.0d0
        End If
      Else
        RedFactor = 1.0d0
        InterestAge = iDecAge
        ConvAge = iDecAge
      End If

      If (ProcessLumpSums .and. ValBenefit(iben,itype)%Ans%CBbalLS == 1) Then
!// Cash Balance is not being converted
        ConvFactor = 1.0d0

!// need to recalculate the 415 limit to be a lump sum instead of annuity
!// Temporarily set the annuity type to lump sum then recalculate 415 for this benefit
        If (.not. Precise415Calc) Then
          age = Dble (ConvAge)
          If (itype == 1) Then
            age = Max (age, eligr)
          End If

          tefmax(iben, itype) = tefmax(iben, itype) * FormAdjFactor415(age, 5, 0, 65d0)

        Else
          jann(iben, itype) = 3

          If (maximumpass == 1) Then
            Call Calc415Limits(1d0, itype, iben)
          Else
            Call Calc415Limits(Gro, itype, iben)
          End If

          jann(iben, itype) = AnnuityType
        End If
      Else
        ConvFactor = MenuValuesCBUA(ValBenefit(iben,itype)%Ans%CBBenNum)%ConvFactors(ConvAge)
      End If

      ValBenefit(iben,itype)%CBadjust = ((1 + balicr) ** (InterestAge - iLoopAge)) * Recip (ConvFactor * RedFactor)
    End If

    tPx = aldefer(ConvAge - 15,jsex,jdis) * Recip (aldefer(iLoopAge - 15,jsex,jdis))

    If (iyieldsw > 0 .and. ConvAge /= iValAge) Then
      vint = vselp(ConvAge - iValAge) ** ((iValAge - ConvAge) * Recip ((iValAge - ConvAge) * 1d0))
    Else If (iyieldsw == 0 .and. iLStype(iben,itype) > 1 .and. ConvAge /= iLoopAge) Then
      vint = vir(ConvAge - iLoopAge) ** ((iLoopAge - ConvAge) * Recip ((iLoopAge - ConvAge) * 1d0))
    Else
      vint = 1.0d0
    End If

    CBBalDis(iben,itype) = tPx * vint
  End If

! Set Non-CB benefits based on age
  If (iLoopAge == CheckAgeForFAS) Then
    ValBenefit(iben,itype)%Ans%HBennoncb = ValBenefit(iben,itype)%Ans%ProjBenNonCB
    ValBenefit(iben,itype)%Ans%HBencb = ValBenefit(iben,itype)%Ans%ProjBenCB
    HBENnonCB(iben,itype) = ValBenefit(iben,itype)%Ans%HBennoncb
    ValBenefit(iben,itype)%Ans%CRETnoncb = ValBenefit(iben,itype)%Ans%ProjBenNonCB
    ValBenefit(iben,itype)%Ans%CRETcb = ValBenefit(iben,itype)%Ans%ProjBenCB
    CRETXnonCB(iben,itype) = ValBenefit(iben,itype)%Ans%CRETnoncb
  End If

  If (iLoopAge == CheckAgeForFAS + 1) Then
    ValBenefit(iben,itype)%Ans%CRETnoncb = ValBenefit(iben,itype)%Ans%ProjBenNonCB
    ValBenefit(iben,itype)%Ans%CRETcb = ValBenefit(iben,itype)%Ans%ProjBenCB
    CRETXnonCB(iben,itype) = ValBenefit(iben,itype)%Ans%CRETnoncb
  End If

  If (BenVariant == ARets) Then
    If (iLoopAge == iValAge) Then
      If (flag(iben, itype + 1) == 1) Then  !// Value comes from EPP so get it from ABen
        ValBenefit(iben,itype)%Ans%ARETnoncb = ValBenefit(iben,itype)%Ans%ABenNonCB
        ValBenefit(iben,itype)%Ans%ARETcb = ValBenefit(iben,itype)%Ans%ABenCB
      Else
        ValBenefit(iben,itype)%Ans%ARETnoncb = ValBenefit(iben,itype)%Ans%ProjBenNonCB
        ValBenefit(iben,itype)%Ans%ARETcb = ValBenefit(iben,itype)%Ans%ProjBenCB
      End If
    Else                                    !// Allows for redefinition of ARet in EPP after the valuation age
      ValBenefit(iben,itype)%Ans%ARETnoncb = ValBenefit(iben,itype)%Ans%ABenNonCB
      ValBenefit(iben,itype)%Ans%ARETcb = ValBenefit(iben,itype)%Ans%ABenCB
    End If

    ARETXnonCB(iben,itype) = ValBenefit(iben,itype)%Ans%ARETnoncb
    aretx(iben,itype) = ARETXnonCB(iben,itype)  + ValBenefit(iben,itype)%Ans%ARETcb * ValBenefit(iben,itype)%CBadjust
  End If

  hben(iben,itype) = HBENnonCB(iben,itype)   + ValBenefit(iben,itype)%Ans%HBENcb * ValBenefit(iben,itype)%CBadjust
  cretx(iben,itype) = CRETXnonCB(iben,itype)  + ValBenefit(iben,itype)%Ans%CRETcb * ValBenefit(iben,itype)%CBadjust
  bft(iben,itype) = ValBenefit(iben,itype)%Ans%ProjBenNonCB + ValBenefit(iben,itype)%Ans%ProjBenCB * ValBenefit(iben,itype)%CBadjust
  planben(iben,itype) = bft(iben,itype)


!// Determination of % of benefit attributable to Cash Balance (as Balance) under lump sums
!// and the discounting that is applied to such benefit
  If (ProcessLumpSums .and. ValBenefit(iben,itype)%Ans%CBbalLS == 1) Then
    CBBalPct(iben,itype) = ValBenefit(iben,itype)%Ans%ProjBenCB * ValBenefit(iben,itype)%CBadjust * &
                            Recip (bft(iben,itype))
  Else
    CBBalPct(iben,itype) = 0d0
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from AgeLoopAdj')
  End If

End Subroutine AgeLoopAdj

!/----------------------------------------------------------------------------------------

Subroutine SaveCBBenInfo (iben, CRETin, ARETin, HBENin, ProjBenIn)

  Implicit None

  Integer:: iben
  Real (Double):: CRETin, ARETin, HBENin, ProjBenIn


  If (DebugCombos) Then
    Call debug ('** Entry into SaveCBBenInfo')
  End If

  If (CBUACount > 0 .and. iben > 0 .and. Allocated(MenuValuesCBUA)) Then
    If (MenuValuesCBUA(iben)%MENBTYP == 6) Then
      BenVars(iben)%CBbalLS = MenuValuesCBUA(iben)%CBbalLS
      BenVars(iben)%BALICR = MenuValuesCBUA(iben)%BalInt * 0.01d0
      BenVars(iben)%CashBalERFmethod = MenuValuesCBUA(iben)%CashBalERFmethod
      BenVars(iben)%CBBenNum = iben
    End If
  End If

  If (BenVars(iben)%PieceType == 0) Then
    BenVars(iben)%PieceType = 1
  End If

  If (BenVars(iben)%BenNum == 0) Then
    BenVars(iben)%BenNum = iben
  End If

  BenVars(iben)%CRETcb = CRETin
  BenVars(iben)%ARETcb = ARETin
  BenVars(iben)%HBENcb = HBENin
  BenVars(iben)%ProjBenCB = ProjBenIn
  BenVars(iben)%AbenCB = ARETin
  BenVars(iben)%Ben0CB = ARETin
  BenVars(iben)%Ben1CB = CRETin

  If (product /= 'LVADMIN') Then
    BenVars(iben)%BenZCB = BenZ(iben)
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from SaveCBBenInfo')
  End If

End Subroutine SaveCBBenInfo

!/----------------------------------------------------------------------------------------

Subroutine SaveNonCBBenInfo (iben)

  Implicit None

!// Dummy arguments
  Integer:: iben

!// Global variables
  Integer :: It, J1, J2, Istr, Kfreq, Lmes, Mayy10
  Integer, Dimension(3) :: Ks
  Common /SPLITCN/ It, Ks, J1, J2, Istr, Kfreq, Lmes, Mayy10

!// Local variables
  Integer:: CheckAgeForFAS


  If (DebugCombos) Then
    Call debug ('** Entry into SaveNonCBBenInfo')
  End If

  BenVars(iben)%ProjBenNonCB = BEN(iben)

  If (Product == 'LVADMIN') Then
    BenVars(iben)%ProjBenCB = 0d0
  End If

  If (BenVars(iben)%PieceType == 0) Then
    BenVars(iben)%PieceType = 1
  End If

  If (BenVars(iben)%BenNum == 0) Then
    BenVars(iben)%BenNum = iben
  End If

!// Need to use the correct as of age when filling the HBen and Cret arrays.  If the
!// FAS 35 results show the Current accrual instead of the Old unit credit the need the age prior to valuation.
  If (Mayy10 == 2) Then
    CheckAgeForFAS = iValAge
  Else
    CheckAgeForFAS = iValAge - 1
  End If

  If (Product /= 'LVADMIN') Then
    BenVars(iben)%AbenNonCB = ABEN(iben)
    BenVars(iben)%BenZnonCB = BENZ(iben)
    BenVars(iben)%Ben0nonCB = BEN0(iben)
    BenVars(iben)%Ben1nonCB = BEN1(iben)

    If (iLoopAge == CheckAgeForFAS) Then
      BenVars(iben)%ARETnoncb = ABen(iben)
    Else If (iLoopAge == CheckAgeForFAS + 1) Then
      BenVars(iben)%HBENnoncb = BenVars(iben)%ARETnoncb
      BenVars(iben)%CRETnoncb = Ben1(iben)
    End If

  End If

  If (DebugCombos) Then
    Call debug ('** Exit from SaveNonCBBenInfo')
  End If

End Subroutine SaveNonCBBenInfo

!/----------------------------------------------------------------------------------------

Subroutine SaveBenInfo ()

  Implicit None

  Integer:: iben


  If (DebugCombos) Then
    Call debug ('** Entry into SaveBenInfo')
  End If

  Do iben = 1, lcMaxBenBlks

!// Don't process the new cash balance benefits here
    If (CBUACount > 0 .and. Allocated(MenuValuesCBUA) .and. iben <= MaxBenBlks) Then
      If (MenuValuesCBUA(iben)%MENBTYP == 6 .and.                                         &
          MenuValuesCBUA(iben)%CashBalOld == 0) Then
        Cycle
      End If
    End If

    Call SaveNonCBBenInfo(iben)

  End Do  ! iben

  If (DebugCombos) Then
    Call debug ('** Exit from SaveBenInfo')
  End If

End Subroutine SaveBenInfo

!/----------------------------------------------------------------------------------------

Subroutine SaveFormInfo ()

  Implicit None

!// Dummy arguments
  Integer:: iben

!// Global variables
  Integer :: It, J1, J2, Istr, Kfreq, Lmes, Mayy10
  Integer, Dimension(3) :: Ks
  Common /SPLITCN/ It, Ks, J1, J2, Istr, Kfreq, Lmes, Mayy10

!// Local variables
  Integer:: CheckAgeForFAS


  If (DebugCombos) Then
    Call debug ('** Entry into SaveFormInfo')
  End If

  Do iben = 1, lcMaxForms
    If (FormVars(iben)%PieceType == 0) Then
      FormVars(iben)%PieceType = 2
    End If

    If (FormVars(iben)%BenNum == 0) Then
      FormVars(iben)%BenNum = iben
    End If

!// Need to use the correct as of age when filling the HBen and Cret arrays.  If the
!// FAS 35 results show the Current accrual instead of the Old unit credit the need the age prior to valuation.
    If (Mayy10 == 2) Then
      CheckAgeForFAS = iValAge
    Else
      CheckAgeForFAS = iValAge - 1
    End If

    FormVars(iben)%ProjBenNonCB = FORM(iben)

    If (product /= 'LVADMIN') Then
      FormVars(iben)%Ben0nonCB = FORM0(iben)
      FormVars(iben)%Ben1nonCB = FORM1(iben)
      FormVars(iben)%BenZnonCB = FORMZ(iben)
      FormVars(iben)%ABenNonCB = AFORM(iben)

      If (iLoopAge == CheckAgeForFAS) Then
        FormVars(iben)%ARETnonCB = AFORM(iben)
      Else If (iLoopAge == CheckAgeForFAS + 1) Then
        FormVars(iben)%HBENnonCB = FormVars(iben)%ARETnonCB
        FormVars(iben)%CRETnonCB = FORM1(iben)
      End If

    End If
  End Do  ! iben

  If (DebugCombos) Then
    Call debug ('** Exit from SaveFormInfo')
  End If

End Subroutine SaveFormInfo

!/----------------------------------------------------------------------------------------

Subroutine InitBenInfoToOne (B)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: B


  If (DebugCombos) Then
    Call debug ('** Entry into InitBenInfoToOne')
  End If

  B%PieceType        = 1
  B%BenNum           = 1
  B%CRETnoncb        = 1d0
  B%CRETcb           = 1d0
  B%HBENnoncb        = 1d0
  B%HBENcb           = 1d0
  B%ARETnoncb        = 1d0
  B%ARETcb           = 1d0
  B%ProjBenNonCB     = 1d0
  B%ProjBenCB        = 1d0
  B%CBBenNum         = 1
  B%CBBALLS          = 1
  B%BALICR           = 1d0
  B%Ben0CB           = 1d0
  B%Ben0nonCB        = 1d0
  B%Ben1CB           = 1d0
  B%Ben1nonCB        = 1d0
  B%BenZCB           = 1d0
  B%BenZnonCB        = 1d0
  B%ABenCB           = 1d0
  B%ABennonCB        = 1d0
  B%SvcAmt           = 1d0
  B%CashBalERFmethod = 1

  If (DebugCombos) Then
    Call debug ('** Exit from InitBenInfoToOne')
  End If

End Subroutine InitBenInfoToOne

!/----------------------------------------------------------------------------------------

Subroutine InitBenInfo (B, AllValues)

   Implicit None

!// Dummy arguments
  Type (tBenInfo):: B
  Logical, Optional:: AllValues


  If (DebugCombos) Then
    Call debug ('** Entry into InitBenInfo')
  End If

  If (Present (AllValues)) Then
    If (AllValues) Then
      B%PieceType        = 0
      B%BenNum           = 0
      B%CBBenNum         = 0
      B%CBBALLS          = 0
      B%CashBalERFmethod = 0
    End If
  End If

  B%CRETnoncb        = 0d0
  B%CRETcb           = 0d0
  B%HBENnoncb        = 0d0
  B%HBENcb           = 0d0
  B%ARETnoncb        = 0d0
  B%ARETcb           = 0d0
  B%ProjBenNonCB     = 0d0
  B%ProjBenCB        = 0d0
  B%BALICR           = 0d0
  B%Ben0CB           = 0d0
  B%Ben0nonCB        = 0d0
  B%Ben1CB           = 0d0
  B%Ben1nonCB        = 0d0
  B%BenZCB           = 0d0
  B%BenZnonCB        = 0d0
  B%ABenCB           = 0d0
  B%ABennonCB        = 0d0
  B%SvcAmt           = 0d0

  If (DebugCombos) Then
    Call debug ('** Exit from InitBenInfo')
  End If

End Subroutine InitBenInfo

!/----------------------------------------------------------------------------------------
!/Puts A in B, think SetBenEqual(src,dest)
!/----------------------------------------------------------------------------------------
Subroutine SetBenEqual (A, B, BenVariantIn)

  Implicit None

  Type (tBenInfo):: A, B
  Integer, Optional:: BenVariantIn

!// Local variables
  Integer:: BenVariant


  If (DebugCombos) Then
    Call debug ('** Entry into SetBenEqual')
  End If

  If (B%PieceType == 0) Then
    B%PieceType = A%PieceType
  End If

  If (B%BenNum == 0) Then
    B%BenNum = A%BenNum
  End If

  If (Present (BenVariantIn)) Then
    BenVariant = BenVariantIn
  Else
    BenVariant = 9
  End If

  Select Case (BenVariant)
  Case (1)
    B%ProjBenCB = A%ProjBenCB
    B%ProjBenNonCB = A%ProjBenNonCB
  Case (Ret0s)
    B%Ben0CB    = A%Ben0CB
    B%Ben0nonCB = A%Ben0nonCB
    B%HBENcb    = A%HBENcb
    B%HBENnoncb = A%HBENnoncb
  Case (Ret1s)
    B%Ben1CB    = A%Ben1CB
    B%Ben1nonCB = A%Ben1nonCB
    B%CRETcb    = A%CRETcb
    B%CRETnoncb = A%CRETnoncb
  Case (ARets)
    B%ABenCB    = A%ABenCB
    B%ABennonCB = A%ABennonCB
    B%ARETcb    = A%ARETcb
    B%ARETnoncb = A%ARETnoncb
  Case (RetZs)
    B%BenZCB    = A%BenZCB
    B%BenZnonCB = A%BenZnonCB
  Case Default                 !// Process all if BenVariant is 9
    B%HBENcb = A%HBENcb
    B%ARETcb = A%ARETcb
    B%CRETcb = A%CRETcb
    B%Ben0CB = A%Ben0CB
    B%Ben1CB = A%Ben1CB
    B%BenZCB = A%BenZCB
    B%ABenCB = A%ABenCB
    B%ProjBenCB = A%ProjBenCB
    B%HBENnoncb = A%HBENnoncb
    B%ARETnoncb = A%ARETnoncb
    B%CRETnoncb = A%CRETnoncb
    B%Ben0nonCB = A%Ben0nonCB
    B%Ben1nonCB = A%Ben1nonCB
    B%BenZnonCB = A%BenZnonCB
    B%ABennonCB = A%ABennonCB
    B%ProjBenNonCB = A%ProjBenNonCB
  End Select

  B%CBBenNum = A%CBBenNum
  B%CBbalLS = A%CBbalLS
  B%BALICR = A%BALICR
  B%SvcAmt = A%SvcAmt
  B%CashBalERFmethod = A%CashBalERFmethod

  If (DebugCombos) Then
    Call debug ('** Exit from SetBenEqual')
  End If

End Subroutine SetBenEqual

!/----------------------------------------------------------------------------------------

Function CombosADD(A, B, C, D) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, C, D, result
  Optional :: B, C, D

!// Local variables
  Character (Len=1):: StartWith


  If (DebugCombos) Then
    Call debug ('** Entry into CombosADD')
  End If

  StartWith = ''
  Call InitBenInfo(result, .true.)

  If (A%BenNum /= 0) Then
    Call SetBenEqual(A, result)
    StartWith = 'A'
  End If

  If (Present (B)) Then
    If (B%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(B, result)
        StartWith = 'B'
      Else
        result = AddBenInfo(result, B)
      End If
    End If
  End If

  If (Present (C)) Then
    If (C%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(C, result)
        StartWith = 'C'
      Else
        result = AddBenInfo(result, C)
      End If
    End If
  End If

  If (Present (D)) Then
    If (D%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(D, result)
        StartWith = 'D'
      Else
        result = AddBenInfo(result, D)
      End If
    End If
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from CombosADD')
  End If

End Function CombosADD

!/----------------------------------------------------------------------------------------

Function AddBenInfo(A, B) Result (result)

  Implicit None

  ! Dummy arguments
  Type (tBenInfo):: A, B, result


  If (DebugCombos) Then
    Call debug ('** Entry into AddBenInfo')
  End If
  Call InitBenInfo(result, .true.)

!// Cash Balance benefits
  result%CRETcb = A%CRETcb + B%CRETcb
  result%HBENcb = A%HBENcb + B%HBENcb
  result%ARETcb = A%ARETcb + B%ARETcb
  result%ProjBenCB = A%ProjBenCB + B%ProjBenCB
  result%Ben0CB = A%Ben0CB + B%Ben0CB
  result%Ben1CB = A%Ben1CB + B%Ben1CB
  result%BenZCB = A%BenZCB + B%BenZCB
  result%ABenCB = A%ABenCB + B%ABenCB

!// Non-Cash Balance benefits
  result%CRETnoncb = A%CRETnoncb + B%CRETnoncb
  result%HBENnoncb = A%HBENnoncb + B%HBENnoncb
  result%ARETnoncb = A%ARETnoncb + B%ARETnoncb
  result%ProjBenNonCB = A%ProjBenNonCB + B%ProjBenNonCB
  result%Ben0nonCB = A%Ben0nonCB + B%Ben0nonCB
  result%Ben1nonCB = A%Ben1nonCB + B%Ben1nonCB
  result%BenZnonCB = A%BenZnonCB + B%BenZnonCB
  result%ABenNonCB = A%ABenNonCB + B%ABenNonCB

!// Specs
  If (A%ProjBenCB + A%ProjBenNonCB > B%ProjBenCB + B%ProjBenNonCB - 0.0001d0) Then
    If (result%PieceType == 0) Then
      result%PieceType = A%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = A%BenNum
    End If

    result%CBBenNum = A%CBBenNum
    result%CashBalERFmethod = A%CashBalERFmethod
    result%CBbalLS = A%CBbalLS
    result%BALICR = A%BALICR
    result%SvcAmt = A%SvcAmt
  Else
    If (result%PieceType == 0) Then
      result%PieceType = B%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = B%BenNum
    End If

    result%CBBenNum = B%CBBenNum
    result%CashBalERFmethod = B%CashBalERFmethod
    result%CBbalLS = B%CBbalLS
    result%BALICR = B%BALICR
    result%SvcAmt = B%SvcAmt
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from AddBenInfo')
  End If

End Function AddBenInfo

!/----------------------------------------------------------------------------------------

Function CombosDIFF(A, B) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, result

!//  Local variables


  If (DebugCombos) Then
    Call debug ('** Entry into CombosDIFF')
  End If

  Call InitBenInfo(result, .true.)

!// Cash Balance benefits
  result%CRETcb = DIM(A%CRETcb, B%CRETcb)
  result%HBENcb = DIM(A%HBENcb, B%HBENcb)
  result%ARETcb = DIM(A%ARETcb, B%ARETcb)
  result%ProjBenCB = DIM(A%ProjBenCB, B%ProjBenCB)
  result%Ben0CB = DIM(A%Ben0CB, B%Ben0CB)
  result%Ben1CB = DIM(A%Ben1CB, B%Ben1CB)
  result%BenZCB = DIM(A%BenZCB, B%BenZCB)
  result%ABenCB = DIM(A%ABenCB, B%ABenCB)

!// Non-Cash Balance benefits
  result%CRETnoncb = DIM(A%CRETnoncb, B%CRETnoncb)
  result%HBENnoncb = DIM(A%HBENnoncb, B%HBENnoncb)
  result%ARETnoncb = DIM(A%ARETnoncb, B%ARETnoncb)
  result%ProjBenNonCB = DIM(A%ProjBenNonCB, B%ProjBenNonCB)
  result%Ben0nonCB = DIM(A%Ben0nonCB, B%Ben0nonCB)
  result%Ben1nonCB = DIM(A%Ben1nonCB, B%Ben1nonCB)
  result%BenZnonCB = DIM(A%BenZnonCB, B%BenZnonCB)
  result%ABennonCB = DIM(A%ABennonCB, B%ABennonCB)

!// Specs
  If (A%ProjBenCB + A%ProjBenNonCB > B%ProjBenCB + B%ProjBenNonCB - 0.0001d0) Then
    If (result%PieceType == 0) Then
      result%PieceType = A%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = A%BenNum
    End If

    result%CBBenNum = A%CBBenNum
    result%CashBalERFmethod = A%CashBalERFmethod
    result%CBbalLS = A%CBbalLS
    result%BALICR = A%BALICR
    result%SvcAmt = A%SvcAmt
  Else
    If (result%PieceType == 0) Then
      result%PieceType = B%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = B%BenNum
    End If

    result%CBBenNum = B%CBBenNum
    result%CashBalERFmethod = B%CashBalERFmethod
    result%CBbalLS = B%CBbalLS
    result%BALICR = B%BALICR
    result%SvcAmt = B%SvcAmt
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from CombosDIFF')
  End If

End Function CombosDIFF

!/----------------------------------------------------------------------------------------

Function CombosMAX(A, B, C, D) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, C, D, result
  Optional :: B, C, D

!// Local variables
  Character (Len=1):: StartWith


  If (DebugCombos) Then
    Call debug ('** Entry into CombosMAX')
  End If

  StartWith = ''
  Call InitBenInfo(result, .true.)

  If (A%BenNum /= 0) Then
    Call SetBenEqual(A, result)
    StartWith = 'A'
  End If

  If (Present (B)) Then
    If (B%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(B, result)
        StartWith = 'B'
      Else
        result = MaxBenInfo(result, B)
      End If
    End If
  End If

  If (Present (C)) Then
    If (C%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(C, result)
        StartWith = 'C'
      Else
        result = MaxBenInfo(result, C)
      End If
    End If
  End If

  If (Present (D)) Then
    If (D%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(D, result)
        StartWith = 'D'
      Else
        result = MaxBenInfo(result, D)
      End If
    End If
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from CombosMAX')
  End If

End Function CombosMAX

!/----------------------------------------------------------------------------------------

Function MaxBenInfo(A, B) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, result

!//  Local variables


  If (DebugCombos) Then
    Call debug ('** Entry into MaxBenInfo')
  End If

  Call InitBenInfo(result, .true.)

  If (A%ARETcb + A%ARETNonCB > B%ARETcb + B%ARETNonCB - 0.0001d0) Then
    result%ARETcb = A%ARETcb
    result%ARETNoncb = A%ARETNoncb
  Else
    result%ARETcb = B%ARETcb
    result%ARETNoncb = B%ARETNoncb
  End If

  If (A%CRETcb + A%CRETNonCB > B%CRETcb + B%CRETNonCB - 0.0001d0) Then
    result%CRETcb = A%CRETcb
    result%CRETNoncb = A%CRETNoncb
  Else
    result%CRETcb = B%CRETcb
    result%CRETNoncb = B%CRETNoncb
  End If

  If (A%HBENcb + A%HBENNonCB > B%HBENcb + B%HBENNonCB - 0.0001d0) Then
    result%HBENcb = A%HBENcb
    result%HBENNoncb = A%HBENNoncb
  Else
    result%HBENcb = B%HBENcb
    result%HBENNoncb = B%HBENNoncb
  End If

  If (A%ProjBencb + A%ProjBenNonCB > B%ProjBencb + B%ProjBenNonCB - 0.0001d0) Then
    result%ProjBencb = A%ProjBencb
    result%ProjBenNoncb = A%ProjBenNoncb
  Else
    result%ProjBencb = B%ProjBencb
    result%ProjBenNoncb = B%ProjBenNoncb
  End If

  If (A%Ben0cb + A%Ben0NonCB > B%Ben0cb + B%Ben0NonCB - 0.0001d0) Then
    result%Ben0cb = A%Ben0cb
    result%Ben0Noncb = A%Ben0Noncb
  Else
    result%Ben0cb = B%Ben0cb
    result%Ben0Noncb = B%Ben0Noncb
  End If

  If (A%Ben1cb + A%Ben1NonCB > B%Ben1cb + B%Ben1NonCB - 0.0001d0) Then
    result%Ben1cb = A%Ben1cb
    result%Ben1Noncb = A%Ben1Noncb
  Else
    result%Ben1cb = B%Ben1cb
    result%Ben1Noncb = B%Ben1Noncb
  End If

  If (A%ABencb + A%ABenNonCB > B%ABencb + B%ABenNonCB - 0.0001d0) Then
    result%ABencb = A%ABencb
    result%ABenNoncb = A%ABenNoncb
  Else
    result%ABencb = B%ABencb
    result%ABenNoncb = B%ABenNoncb
  End If

  If (A%BenZcb + A%BenZNonCB > B%BenZcb + B%BenZNonCB - 0.0001d0) Then
    result%BenZcb = A%BenZcb
    result%BenZNoncb = A%BenZNoncb
  Else
    result%BenZcb = B%BenZcb
    result%BenZNoncb = B%BenZNoncb
  End If

!// Specs
  If (A%ProjBenCB + A%ProjBenNonCB > B%ProjBenCB + B%ProjBenNonCB - 0.0001d0) Then
    If (result%PieceType == 0) Then
      result%PieceType = A%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = A%BenNum
    End If

    result%CBBenNum = A%CBBenNum
    result%CashBalERFmethod = A%CashBalERFmethod
    result%CBbalLS = A%CBbalLS
    result%BALICR = A%BALICR
    result%SvcAmt = A%SvcAmt
  Else
    If (result%PieceType == 0) Then
      result%PieceType = B%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = B%BenNum
    End If

    result%CBBenNum = B%CBBenNum
    result%CashBalERFmethod = B%CashBalERFmethod
    result%CBbalLS = B%CBbalLS
    result%BALICR = B%BALICR
    result%SvcAmt = B%SvcAmt
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from MaxBenInfo')
  End If

End Function MaxBenInfo

!/----------------------------------------------------------------------------------------

Function CombosMIN(A, B, C, D) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, C, D, result
  Optional :: B, C, D

!//  Local variables
  Character (Len=1):: StartWith


  If (DebugCombos) Then
    Call debug ('** Entry into CombosMIN')
  End If

  StartWith = ''
  Call InitBenInfo(result, .true.)

  If (A%BenNum /= 0) Then
    Call SetBenEqual(A, result)
    StartWith = 'A'
  End If

  If (Present (B)) Then
    If (B%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(B, result)
        StartWith = 'B'
      Else
        result = MinBenInfo(result, B)
      End If
    End If
  End If

  If (Present (C)) Then
    If (C%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(C, result)
        StartWith = 'C'
      Else
        result = MinBenInfo(result, C)
      End If
    End If
  End If

  If (Present (D)) Then
    If (D%BenNum /= 0) Then
      If (StartWith == '') Then
        Call SetBenEqual(D, result)
        StartWith = 'D'
      Else
        result = MinBenInfo(result, D)
      End If
    End If
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from CombosMIN')
  End If

 End Function CombosMIN

!/----------------------------------------------------------------------------------------

Function MinBenInfo(A, B) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, result

!// Local variables


  If (DebugCombos) Then
    Call debug ('** Entry into MinBenInfo')
  End If

  Call InitBenInfo(result, .true.)

  If (A%ARETcb + A%ARETNonCB < B%ARETcb + B%ARETNonCB - 0.0001d0) Then
    result%ARETcb = A%ARETcb
    result%ARETNoncb = A%ARETNoncb
  Else
    result%ARETcb = B%ARETcb
    result%ARETNoncb = B%ARETNoncb
  End If

  If (A%CRETcb + A%CRETNonCB < B%CRETcb + B%CRETNonCB - 0.0001d0) Then
    result%CRETcb = A%CRETcb
    result%CRETNoncb = A%CRETNoncb
  Else
    result%CRETcb = B%CRETcb
    result%CRETNoncb = B%CRETNoncb
  End If

  If (A%HBENcb + A%HBENNonCB < B%HBENcb + B%HBENNonCB - 0.0001d0) Then
    result%HBENcb = A%HBENcb
    result%HBENNoncb = A%HBENNoncb
  Else
    result%HBENcb = B%HBENcb
    result%HBENNoncb = B%HBENNoncb
  End If

  If (A%ProjBencb + A%ProjBenNonCB < B%ProjBencb + B%ProjBenNonCB - 0.0001d0) Then
    result%ProjBencb = A%ProjBencb
    result%ProjBenNoncb = A%ProjBenNoncb
  Else
    result%ProjBencb = B%ProjBencb
    result%ProjBenNoncb = B%ProjBenNoncb
  End If

  If (A%Ben0cb + A%Ben0NonCB < B%Ben0cb + B%Ben0NonCB - 0.0001d0) Then
    result%Ben0cb = A%Ben0cb
    result%Ben0Noncb = A%Ben0Noncb
  Else
    result%Ben0cb = B%Ben0cb
    result%Ben0Noncb = B%Ben0Noncb
  End If

  If (A%Ben1cb + A%Ben1NonCB < B%Ben1cb + B%Ben1NonCB - 0.0001d0) Then
    result%Ben1cb = A%Ben1cb
    result%Ben1Noncb = A%Ben1Noncb
  Else
    result%Ben1cb = B%Ben1cb
    result%Ben1Noncb = B%Ben1Noncb
  End If

  If (A%ABencb + A%ABenNonCB < B%ABencb + B%ABenNonCB - 0.0001d0) Then
    result%ABencb = A%ABencb
    result%ABenNoncb = A%ABenNoncb
  Else
    result%ABencb = B%ABencb
    result%ABenNoncb = B%ABenNoncb
  End If

  If (A%BenZcb + A%BenZNonCB < B%BenZcb + B%BenZNonCB - 0.0001d0) Then
    result%BenZcb = A%BenZcb
    result%BenZNoncb = A%BenZNoncb
  Else
    result%BenZcb = B%BenZcb
    result%BenZNoncb = B%BenZNoncb
  End If

!// Specs
  If (A%ProjBenCB + A%ProjBenNonCB < B%ProjBenCB + B%ProjBenNonCB - 0.0001d0) Then
    If (result%PieceType == 0) Then
      result%PieceType = A%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = A%BenNum
    End If

    result%CBBenNum = A%CBBenNum
    result%CashBalERFmethod = A%CashBalERFmethod
    result%CBbalLS = A%CBbalLS
    result%BALICR = A%BALICR
    result%SvcAmt = A%SvcAmt
  Else
    If (result%PieceType == 0) Then
      result%PieceType = B%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = B%BenNum
    End If

    result%CBBenNum = B%CBBenNum
    result%CashBalERFmethod = B%CashBalERFmethod
    result%CBbalLS = B%CBbalLS
    result%BALICR = B%BALICR
    result%SvcAmt = B%SvcAmt
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from MinBenInfo')
  End If

End Function MinBenInfo

!/----------------------------------------------------------------------------------------

Function CombosMULT(A, B, C, D) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, C, D, result
  Optional :: B, C, D

!// Local variables
  Logical:: ResultInitialized


  If (DebugCombos) Then
    Call debug ('** Entry into CombosMULT')
  End If

!// Need to set the result to be equal to the first non-zero benefit component.  Use
!// the ResultInitialized flag to indicate when that has happened.  The result is then
!// multiplied by all subsequent benefit components.
!// Initializing the result to all ones Doubles the results due to the addition of the
!// cash balance and non cash balance amounts.
  Call InitBenInfo(result, .true.)
  ResultInitialized = .false.

  If (A%BenNum /= 0) Then
    Call SetBenEqual(A, result)
    ResultInitialized = .true.
  End If

  If (Present (B)) Then
    If (B%BenNum /= 0) Then
      If (ResultInitialized) Then
        result = MultBenInfo(result,B)
      Else
        Call SetBenEqual(B, result)
        ResultInitialized = .true.
      End If
    End If
  End If

  If (Present (C)) Then
    If (C%BenNum /= 0) Then
      If (ResultInitialized) Then
        result = MultBenInfo(result,C)
      Else
        Call SetBenEqual(C, result)
        ResultInitialized = .true.
      End If
    End If
  End If

  If (Present (D)) Then
    If (D%BenNum /= 0) Then
      If (ResultInitialized) Then
        result = MultBenInfo(result,D)
      Else
        Call SetBenEqual(D, result)
        ResultInitialized = .true.
      End If
    End If
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from CombosMULT')
  End If

End Function CombosMULT

!/----------------------------------------------------------------------------------------

Function MultBenInfo(A, B) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, result

!// Local variables
  Real (Double):: Recip


  If (DebugCombos) Then
    Call debug ('** Entry into MultBenInfo')
  End If

  Call InitBenInfo(result, .true.)

!// Multiplying two benefits together does not make any sense.  Usually, one of these values
!// will be a factor.  In this case, the system will not know which one is the factor, so this
!// has to be an underlying assumption.  With that said, both Cash Balance and regular defined
!// benefits must be considered in all benefit equations.

!// Cash Balance benefits
  result%CRETcb = ((A%CRETcb + A%CRETnoncb) * (B%CRETcb + B%CRETnoncb)) *                 &
    Max (A%CRETcb, B%CRETcb) * Recip (Max (A%CRETcb, B%CRETcb) + Max (A%CRETnoncb, B%CRETnoncb))
  result%HBENcb = ((A%HBENcb + A%HBENnoncb) * (B%HBENcb + B%HBENnoncb)) *                 &
    Max (A%HBENcb, B%HBENcb) * Recip (Max (A%HBENcb, B%HBENcb) + Max (A%HBENnoncb, B%HBENnoncb))
  result%ARETcb = ((A%ARETcb + A%ARETnoncb) * (B%ARETcb + B%ARETnoncb)) *                 &
    Max (A%ARETcb, B%ARETcb) * Recip (Max (A%ARETcb, B%ARETcb) + Max (A%ARETnoncb, B%ARETnoncb))
  result%ProjBenCB = ((A%ProjBencb + A%ProjBenNonCB) * (B%ProjBencb + B%ProjBenNonCB)) *  &
    Max (A%ProjBencb, B%ProjBencb) * Recip (Max (A%ProjBencb, B%ProjBencb) + Max (A%ProjBenNonCB, B%ProjBenNonCB))
  result%Ben0CB = ((A%Ben0cb + A%Ben0noncb) * (B%Ben0cb + B%Ben0noncb)) *                 &
    Max (A%Ben0cb, B%Ben0cb) * Recip (Max (A%Ben0cb, B%Ben0cb) + Max (A%Ben0noncb, B%Ben0noncb))
  result%Ben1CB = ((A%Ben1cb + A%Ben1noncb) * (B%Ben1cb + B%Ben1noncb)) *                 &
    Max (A%Ben1cb, B%Ben1cb) * Recip (Max (A%Ben1cb, B%Ben1cb) + Max (A%Ben1noncb, B%Ben1noncb))
  result%BenZCB = ((A%BenZcb + A%BenZnoncb) * (B%BenZcb + B%BenZnoncb)) *                 &
    Max (A%BenZcb, B%BenZcb) * Recip (Max (A%BenZcb, B%BenZcb) + Max (A%BenZnoncb, B%BenZnoncb))
  result%ABenCB = ((A%Abencb + A%Abennoncb) * (B%Abencb + B%Abennoncb)) *                 &
    Max (A%Abencb, B%Abencb) * Recip (Max (A%Abencb, B%Abencb) + Max (A%Abennoncb, B%Abennoncb))

!// Non-Cash Balance benefits
  result%CRETnoncb =  ((A%CRETcb + A%CRETnoncb) * (B%CRETcb + B%CRETnoncb)) *              &
    Max (A%CRETnoncb, B%CRETnoncb) * Recip (Max (A%CRETcb, B%CRETcb) + Max (A%CRETnoncb, B%CRETnoncb))
  result%HBENnoncb = ((A%HBENcb + A%HBENnoncb) * (B%HBENcb + B%HBENnoncb)) *              &
    Max (A%HBENnoncb, B%HBENnoncb) * Recip (Max (A%HBENcb, B%HBENcb) + Max (A%HBENnoncb, B%HBENnoncb))
  result%ARETnoncb = ((A%ARETcb + A%ARETnoncb) * (B%ARETcb + B%ARETnoncb)) *              &
    Max (A%ARETnoncb, B%ARETnoncb) * Recip (Max (A%ARETcb, B%ARETcb) + Max (A%ARETnoncb, B%ARETnoncb))
  result%ProjBenNonCB = ((A%ProjBencb + A%ProjBenNonCB) * (B%ProjBencb + B%ProjBenNonCB)) * &
    Max (A%ProjBenNonCB, B%ProjBenNonCB) * Recip (Max (A%ProjBencb, B%ProjBencb) + Max (A%ProjBenNonCB, B%ProjBenNonCB))
  result%Ben0nonCB = ((A%Ben0cb + A%Ben0noncb) * (B%Ben0cb + B%Ben0noncb)) *              &
    Max (A%Ben0noncb, B%Ben0noncb) * Recip (Max (A%Ben0cb, B%Ben0cb) + Max (A%Ben0noncb, B%Ben0noncb))
  result%Ben1nonCB = ((A%Ben1cb + A%Ben1noncb) * (B%Ben1cb + B%Ben1noncb)) *              &
    Max (A%Ben1noncb, B%Ben1noncb) * Recip (Max (A%Ben1cb, B%Ben1cb) + Max (A%Ben1noncb, B%Ben1noncb))
  result%BenZnonCB = ((A%BenZcb + A%BenZnoncb) * (B%BenZcb + B%BenZnoncb)) *              &
    Max (A%BenZnoncb, B%BenZnoncb) * Recip (Max (A%BenZcb, B%BenZcb) + Max (A%BenZnoncb, B%BenZnoncb))
  result%ABenNonCB = ((A%ABencb + A%ABennoncb) * (B%ABencb + B%ABennoncb)) *              &
    Max (A%ABennoncb, B%ABennoncb) * Recip (Max (A%ABencb, B%ABencb) + Max (A%ABennoncb, B%ABennoncb))

!// Specs
  If (A%ProjBenCB + A%ProjBenNonCB > B%ProjBenCB + B%ProjBenNonCB - 0.0001d0) Then
    If (result%PieceType == 0) Then
      result%PieceType = A%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = A%BenNum
    End If

    result%CBBenNum = A%CBBenNum
    result%CashBalERFmethod = A%CashBalERFmethod
    result%CBbalLS = A%CBbalLS
    result%BALICR = A%BALICR
    result%SvcAmt = A%SvcAmt
  Else
    If (result%PieceType == 0) Then
      result%PieceType = B%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = B%BenNum
    End If

    result%CBBenNum = B%CBBenNum
    result%CashBalERFmethod = B%CashBalERFmethod
    result%CBbalLS = B%CBbalLS
    result%BALICR = B%BALICR
    result%SvcAmt = B%SvcAmt
  End If


  If (DebugCombos) Then
    Call debug ('** Exit from MultBenInfo')
  End If

End Function MultBenInfo

!/----------------------------------------------------------------------------------------

Function CombosDIV(A, B) Result (result)

  Implicit None

!// Dummy arguments
  Type (tBenInfo):: A, B, result

!//  Local variables
  Real (Double):: Recip


  If (DebugCombos) Then
    Call debug ('** Entry into CombosDIV')
  End If

  Call InitBenInfo(result, .true.)

!// Dividing one benefit by another together does not make any sense unless the user is attempting
!// to develop a factor.  We have to make an assumption here that they are dividing a benefit by
!// a factor, so that Cash Balance benefit processing works properly.  In this case, the factor
!// will always be the denominator, being the second parameter in the CombosDIV Function.  With
!// that said, the denominator must include both Cash Balance and non-Cash Balance values.

!// Cash Balance benefits
  result%CRETcb = A%CRETcb * Recip (B%CRETcb + B%CRETnonCB)
  result%ARETcb = A%ARETcb * Recip (B%ARETcb + B%ARETnonCB)
  result%HBENcb = A%HBENcb * Recip (B%HBENcb + B%HBENnonCB)
  result%ProjBenCB = A%ProjBenCB * Recip (B%ProjBenCB + B%ProjBenNonCB)
  result%Ben0CB = A%Ben0CB * Recip (B%Ben0CB + B%Ben0nonCB)
  result%Ben1CB = A%Ben1CB * Recip (B%Ben1CB + B%Ben1nonCB)
  result%BenZCB = A%BenZCB * Recip (B%BenZCB + B%BenZnonCB)
  result%ABenCB = A%ABenCB * Recip (B%ABenCB + B%ABennonCB)

!// Non-Cash Balance benefits
  result%CRETnoncb = A%CRETnoncb * Recip (B%CRETcb + B%CRETnonCB)
  result%ARETnoncb = A%ARETnoncb * Recip (B%ARETcb + B%ARETnonCB)
  result%HBENnoncb = A%HBENnoncb * Recip (B%HBENcb + B%HBENnonCB)
  result%ProjBenNonCB = A%ProjBenNonCB * Recip (B%ProjBenCB + B%ProjBenNonCB)
  result%Ben0nonCB = A%Ben0nonCB * Recip (B%Ben0CB + B%Ben0nonCB)
  result%Ben1nonCB = A%Ben1nonCB * Recip (B%Ben1CB + B%Ben1nonCB)
  result%BenZnonCB = A%BenZnonCB * Recip (B%BenZCB + B%BenZnonCB)
  result%ABenNonCB = A%ABenNonCB * Recip (B%ABenCB + B%ABennonCB)

!// Specs
  If (A%ProjBenCB + A%ProjBenNonCB > B%ProjBenCB + B%ProjBenNonCB - 0.0001d0) Then
    If (result%PieceType == 0) Then
      result%PieceType = A%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = A%BenNum
    End If

    result%CBBenNum = A%CBBenNum
    result%CashBalERFmethod = A%CashBalERFmethod
    result%CBbalLS = A%CBbalLS
    result%BALICR = A%BALICR
    result%SvcAmt = A%SvcAmt
  Else
    If (result%PieceType == 0) Then
      result%PieceType = B%PieceType
    End If

    If (result%BenNum == 0) Then
      result%BenNum = B%BenNum
    End If

    result%CBBenNum = B%CBBenNum
    result%CashBalERFmethod = B%CashBalERFmethod
    result%CBbalLS = B%CBbalLS
    result%BALICR = B%BALICR
    result%SvcAmt = B%SvcAmt
  End If

  If (DebugCombos) Then
    Call debug ('** Exit from CombosDIV')
  End If

End Function CombosDIV

!/----------------------------------------------------------------------------------------
! Other Exception handling
!/----------------------------------------------------------------------------------------
Subroutine Exception (ErrorCode, ErrorMessage)

  Implicit None

  Integer, Intent(In):: ErrorCode
  Character (Len=*), Intent(In):: ErrorMessage

  Call lverror('Combos',1,ErrorCode,ErrorMessage,'')

End Subroutine Exception

!/----------------------------------------------------------------------------------------

End Module Combos

