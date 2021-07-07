for( k  = 1; k  < Ubound (BenefitStruct,2); k  = k  + 1) {! Combs, ValBens(Ret,Dth,Dis,Wth)
 
    for( i  = 1; i  < Ubound (BenefitStruct,1); i  = i  + 1) {
 
       InitBenInfo(BenefitStruct(i,k)->Ans, AllValues=true); 
      for( j  = 1; j  < 4; j  = j  + 1) {
 
         InitBenInfo(BenefitStruct(i,k)->Piece(j), AllValues=true); 
      } //end do loop  ! j
 
 
  lverror('AllocateComboStructures', 1, iastat,'Error allocating BenefitStruct array.','');
 
          Switch (Trim (bentype)){ 
          Case ('BENEFIT'): 
            ibentype = 1; 
break; 
          Case ('FORMULA'): 
            ibentype = 2; 
break; 
          Case ('COMBINE'): 
            ibentype = 3; 
break; 
          Case ('RETIREMENT'): 
            ibentype = 4; 
break; 
          Case ('DEATH'): 
            ibentype = 5; 
break; 
          Case ('DISABILITY'): 
            ibentype = 6; 
break; 
          Case ('WITHDRAWAL'): 
            ibentype = 7; 
break; 
          Case Default: 
            ibentype = 0; 
break; 
          } 
      } //end do loop  ! j
 
Module Combos; 
 
Use Globals; 
Use gblbens; 
Use f90kinds; 
Use Lvutility; 
Use nfsvcvr; 
Use cbuamod; 
Use GlobErr, Only: iErrCode; 
 
Implicit None; 
 
!// All declarations in this module are private unless explicity declared as public. 
Private; 
 
Public :: Combination,                                                                    //Not converted 
          ValBenefit,                                                                     &; 
          CombSet,                                                                        &; 
          AllocateComboStructures,                                                        &; 
          DeallocateComboStructures,                                                      &; 
          ReadMenuVarComb,                                                                &; 
          CombSetFAS,                                                                     &; 
          SaveNonCBBenInfo,                                                               &; 
          SaveCBBenInfo,                                                                  &; 
          SaveBenInfo,                                                                    &; 
          SaveFormInfo,                                                                   &; 
          AgeLoopAdj,                                                                     &; 
          CombFinal,                                                                      &; 
          CombFinalFAS,                                                                   &; 
          InitializeCombArrays,                                                           &; 
          DebugCombos,                                                                    &; 
          Combs, Rets, Dths, Disbs, Wths, Ret0s, Ret1s, ARets, RetZs, Fld35; 
 
Include 'Params.INC'; 
Include 'COMM1.INC'; 
Include 'COMM3.INC'; 
Include 'COMM4.INC'; 
Include 'product.inc'; 
 
Type(tCombMenu), Pointer:: Combination(:) => null()//Not converted 
Type(tCombMenu), Pointer:: ValBenefit(:,:) => null()//Not converted 
Real (Double), Dimension(:, :), Allocatable:: Fld35//Not converted 
bool  DebugCombos; 
 
!// Constants 
int  Combs=1, Rets=2, Dths=3, Disbs=4, Wths=5, Ret0s=2, Ret1s=3, ARets=4, RetZs=5; //Parameter 
 
!// remainder of variables are private to the module 
Type (tCombMenu), Dimension(:, :), Allocatable, Target:: BenefitStruct//Not converted 
Type (tBenInfo), Dimension(:), Allocatable:: BenVars, FormVars//Not converted 
Real (Double), Dimension(:, :), Allocatable:: ARETXnonCB, CRETXnonCB, HBENnonCB//Not converted 
Integer, Dimension(:, :), Allocatable:: flag//Not converted 
int  iastat, lcMaxBenBlks, lcMaxForms, lcMaxCombs; 
bool  UseFas35Overrides; 
Logical, Dimension(:, :), Allocatable:: UseFld35//Not converted 
char  ErrMessage[80]; 
 
!/---------------------------------------------------------------------------------------- 
Contains; 
!/---------------------------------------------------------------------------------------- 
 
  Function AllocateComboStructures() Result (result) {  
 
    Implicit None; 
 
!// Dummy arguments 
    bool  result; 
 
!//  Local variables 
    int  MenFSOvr; 
 
 
    If (DebugCombos) { 
       debug ('** Entry into AllocateComboStructures'); 
    } 
 
    result = .False.; 
    iastat = 0; 
    If (! Allocated(BenefitStruct)) { 
      Allocate (BenefitStruct(Max (Max (9,MaxBens),MaxCombs),5),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating BenefitStruct array.','');
 
        Return; 
      } 
 
      ValBenefit  => BenefitStruct(:, 2:); 
      Combination => BenefitStruct(:, 1); 
    } 
 
    If (! Allocated(BenVars)) { 
      lcMaxBenBlks = Ubound (ben,1); 
      Allocate (BenVars(lcMaxBenBlks),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating BenVars array.','');
 
        Return; 
      } 
    } 
 
    If (! Allocated(FormVars)) { 
      lcMaxForms = Ubound (form,1); 
      Allocate (FormVars(lcMaxForms),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating FormVars array.','');
 
        Return; 
      } 
    } 
 
    lcMaxCombs = Ubound (comb,1); 
 
    If (! Allocated(ARETXnonCB)) { 
      Allocate (ARETXnonCB(Max (9,MaxBens), 4),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating ARETXnonCB array.','');
 
        Return; 
      } 
    } 
 
    If (! Allocated(CRETXnonCB)) { 
      Allocate (CRETXnonCB(Max (9,MaxBens), 4),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating CRETXnonCB array.','');
 
        Return; 
      } 
    } 
 
    If (! Allocated(HBENnonCB)) { 
      Allocate (HBENnonCB(Max (9,MaxBens), 4),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating HBENnonCB array.','');
 
        Return; 
      } 
    } 
 
    If (! Allocated(flag)) { 
      Allocate (flag(Max (9, MaxBens, lcMaxCombs), 5),stat=iastat); 
      If (iastat /= 0) { 
         lverror('AllocateComboStructures', 1, iastat,'Error allocating flag array.','');
 
        Return; 
      } 
    } 
 
    If (MenuLoaded) { 
      If (! GetMenuItem('menfsovr', MenFSOvr)) { 
         exception(999,Trim (LVUtilityErrMsg)); 
        Return; 
      } 
    } 
 
    UseFas35Overrides = false; 
    If (MenFSOvr == 2) { 
      UseFas35Overrides = true; 
 
      If (! Allocated(Fld35)) { 
        Allocate (Fld35(4, Max (9,MaxBens)),stat=iastat); 
        If (iastat /= 0) { 
           lverror('AllocateComboStructures', 1, iastat,'Error allocating Fld35 array.','');
 
          Return; 
        } 
      } 
 
      If (! Allocated(UseFld35)) { 
        Allocate (UseFld35(4, Max (9,MaxBens)),stat=iastat); 
        If (iastat /= 0) { 
           lverror('AllocateComboStructures', 1, iastat,'Error allocating UseFld35 array.','');
 
          Return; 
        } 
      } 
    } 
 
 
    result = true; 
 
    If (DebugCombos) { 
       debug ('** Exit from AllocateComboStructures'); 
    } 
 
  } // end function ALLOCATECOMBOSTRUCTURES 
 
!/---------------------------------------------------------------------------------------- 
!/ Function ReadMenuVarComb reads menu items that relate to the benefit combinations 
!/ This was previously done in LVcode, but is now called from ReadMenuVariables in modules 
!/---------------------------------------------------------------------------------------- 
 
  Function ReadMenuVarComb() Result (result) {  
 
  Implicit None; 
 
!//  Local variables 
 
 
 
!// Local variables 
  int  i, j, k, iBenType, BenNum, ServNum, PiecesUsed; 
  char  bentype[20], CharVar[20], Switch[20]; 
  char  SwitchLabel[80]; 
  int  TempBound, CombineEquiv, CombType, CombSubType, ServRed; 
   double  benpct, svcdim, svclim, fxyrsc ; 
  bool  result; 
 
 
  If (DebugCombos) { 
     debug ('** Entry into ReadMenuVarComb'); 
  } 
  TempBound = 0; 
  i = 0; 
  j = 0; 
 
!// WARNING: If all the variables aren't here, lv3lib will silently produce no output! 
  If (UseFas35Overrides) { 
    UseFld35 = false; 
  } 
 
  BenefitStruct->Use = .False.; 
  BenefitStruct->CombType = 0; 
  BenefitStruct->CombSubType = 0; 
  BenefitStruct->CombineEquiv = 0; 
  BenefitStruct->BenEquiv = 0; 
  BenefitStruct->ServRed = 0; 
  BenefitStruct->svcdim = 0d0; 
  BenefitStruct->svclim = 0d0; 
  BenefitStruct->fxyrsc = 0d0; 
  BenefitStruct->ServNum = 0; 
  BenefitStruct->BenEquivNum = 0; 
  BenefitStruct->bentype = 0; 
  BenefitStruct->ibencomb = 0; 
  BenefitStruct->BenPct = 1d0; 
  BenefitStruct->CBadjust = 1d0; 
 
  for( k  = 1; k  < Ubound (BenefitStruct,2); k  = k  + 1) {
 
    for( i  = 1; i  < Ubound (BenefitStruct,1); i  = i  + 1) {
 
       InitBenInfo(BenefitStruct(i,k)->Ans, AllValues=true); 
      for( j  = 1; j  < 4; j  = j  + 1) {
 
         InitBenInfo(BenefitStruct(i,k)->Piece(j), AllValues=true); 
      } //end do loop  ! j
 
 
      BenefitStruct(i,k)->ServRedPct = 1d0; 
    } //end do loop  ! i
 
  } //end do loop  ! k
 
 
  for( k  = 1; k  < Ubound (BenVars,1); k  = k  + 1) {
 
     InitBenInfo(BenVars(k), AllValues=true); 
  } //end do loop  ! k
 
 
  for( k  = 1; k  < Ubound (FormVars,1); k  = k  + 1) {
 
     InitBenInfo(FormVars(k), AllValues=true); 
  } //end do loop  ! k
 
 
  for( k  = 1; k  < Ubound (Bft,2) + 1; k  = k  + 1) {! Combs, ValBens(Ret,Dth,Dis,Wth)
 
 
    If (k == 1) { 
      TempBound = lcMaxCombs; 
    } Else { 
      TempBound = MaxBens; 
    } 
 
    for( i  = 1; i  < TempBound; i  = i  + 1) {! array size of switch
 
 
      iBenType = 0; 
      BenNum = 0; 
      ServNum = 0; 
      CombineEquiv = 0; 
      CombType = 0; 
      CombSubType = 0; 
      ServRed = 0; 
      benpct = 0d0; 
      svcdim = 0d0; 
      svclim = 0d0; 
      fxyrsc = 0d0; 
 
!// Need the switch label, too. 
      Switch = Trim (mendescrip(k,1)) // '(' // Trim (IntToStr(i, 3)) // ')'; 
      If (! GetMenuItem(Switch, BenefitStruct(i,k)->Use, SwitchLabel)) { 
        If (LVutilityErrMsg(:48) == 'Switch setting not specified as either ON or OFF') { 
          BenefitStruct(i,k)->Use = false; 
        } Else { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
      } 
 
!// Ignore benefits that are switched on but the label is empty. 
      If (len_Trim (SwitchLabel) == 0) { 
        BenefitStruct(i,k)->Use = false; 
      } 
 
      If (! BenefitStruct(i,k)->Use) { 
        Cycle; 
      } 
 
      If (k == 1 && Debug_On) { 
        If (Combination(i)->Use) { 
           debug('   Combination Use = True "' //mendescrip(k,1)//'"(' //inttostr(i,2) //')'); 
        } Else { 
           debug('   Combination Use = False "' //mendescrip(k,1)//'"(' //inttostr(i,2) //')'); 
        } 
      } 
 
      If (! GetMenuItem('benpct',BenPct,k,i)) { 
         exception(999,Trim (LVUtilityErrMsg)); 
        Return; 
      } 
 
!// If user leaves BenPct blank, it is assumed BenPct is 100% 
      If (abs (BenPct) < 0.0011d0) { 
        BenPct = 100d0; 
      } 
 
      BenefitStruct(i,k)->BenPct = BenPct / 100d0; 
 
      If (k > 1) { 
        If (!  GetMenuItem(MenDescrip(k,10),CombineEquiv,i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
      } Else { 
        CombineEquiv = 1  !// always combine if processing COMB 
      } 
 
      BenefitStruct(i,k)->CombineEquiv = CombineEquiv; 
 
      If (CombineEquiv == 1) { 
!// read the combination related menu items 
 
        If (! GetMenuItem(MenDescrip(k,2),CombType,i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
 
        If (! GetMenuItem(MenDescrip(k,CombType+2),CombSubType,i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
 
        PiecesUsed = 0; 
        for( j  = 1; j  < 4; j  = j  + 1) {! A,B,C,D
 
          If (! GetMenuItem('bentype',bentype,k,j,i)) { 
             exception(999,Trim (LVUtilityErrMsg)); 
            Return; 
          } 
 
          If (! GetMenuItem('ibencomb',BenNum,k,j,i)) { 
             exception(999,Trim (LVUtilityErrMsg)); 
            Return; 
          } 
 
          If (BenNum == 0) { 
            Cycle; 
          } 
 
!// assign correct benefit type value from benefit block drop down boxes 
           capall(bentype); 
 
          If (Debug_On) { 
             Debug('  bentype = ' //bentype); 
          } 
 
          Switch (Trim (bentype)){ 
          Case ('BENEFIT'): 
            ibentype = 1; 
break; 
          Case ('FORMULA'): 
            ibentype = 2; 
break; 
          Case ('COMBINE'): 
            ibentype = 3; 
break; 
          Case ('RETIREMENT'): 
            ibentype = 4; 
break; 
          Case ('DEATH'): 
            ibentype = 5; 
break; 
          Case ('DISABILITY'): 
            ibentype = 6; 
break; 
          Case ('WITHDRAWAL'): 
            ibentype = 7; 
break; 
          Case Default: 
            ibentype = 0; 
break; 
          } 
 
          BenefitStruct(i,k)->Piece(j)->PieceType = iBenType; 
          BenefitStruct(i,k)->Piece(j)->BenNum = BenNum; 
          PiecesUsed = PiecesUsed + 1; 
        } //end do loop  ! j
 
 
!// only read service reduction variables if that menu option is chosen 
 
        If (!  GetMenuItem(MenDescrip(k,9),ServRed,i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
 
        If (ServRed > 1) { 
          If (! GetMenuItem('isvc',ServNum,k,i)) { 
             exception(999,Trim (LVUtilityErrMsg)); 
            Return; 
          } 
 
          If (! GetMenuItem('svcdim',svcdim,k,i)) { 
             exception(999,Trim (LVUtilityErrMsg)); 
            Return; 
          } 
 
          If (! GetMenuItem('svclim',svclim,k,i)) { 
             exception(999,Trim (LVUtilityErrMsg)); 
            Return; 
          } 
 
          If (! GetMenuItem('fxyrsc',fxyrsc,k,i)) { 
             exception(999,Trim (LVUtilityErrMsg)); 
            Return; 
          } 
        } 
 
        BenefitStruct(i,k)->CombType = CombType; 
        BenefitStruct(i,k)->CombSubType = CombSubType; 
        BenefitStruct(i,k)->ServRed = ServRed; 
        BenefitStruct(i,k)->svcdim = svcdim; 
        BenefitStruct(i,k)->ServNum = ServNum; 
        BenefitStruct(i,k)->svclim = svclim; 
        BenefitStruct(i,k)->fxyrsc = fxyrsc; 
 
!// Ignore benefits that are switched on but no benefit pieces are defined. 
        If (PiecesUsed == 0) { 
          BenefitStruct(i,k)->Use = false; 
        } 
 
      } Else { ! CombineEquiv != 1, so not a combination 
 
!// Ignore benefits that are switched on but not included in a plan.  Applies only 
!// to benefits defined as equivalent to another. 
        If (jpln(i, k - 1) == 0) { 
          BenefitStruct(i,k)->Use = false; 
        } 
 
        If (! GetMenuItem(MenDescrip(k,11),BenefitStruct(i,k)->BenEquiv,i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
 
        If (! GetMenuItem('ibeneqiv',BenefitStruct(i,k)->BenEquivNum,k,i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } 
 
      } 
 
      If (UseFas35Overrides) { 
        If (! GetMenuItem('fld35', CharVar, k-1, i)) { 
           exception(999,Trim (LVUtilityErrMsg)); 
          Return; 
        } Else { 
          If (len_Trim (CharVar) > 0) { 
            UseFld35(k-1, i) = true; 
          } 
        } 
      } 
 
    } //end do loop  ! i
 
  } //end do loop  ! k
 
 
  result = .True.; 
 
  If (DebugCombos) { 
     debug ('** Exit from ReadMenuVarComb'); 
  } 
 
} // end function READMENUVARCOMB 
 
!/---------------------------------------------------------------------------------------- 
 
  Subroutine DeallocateComboStructures (); 
 
    Implicit None; 
 
!// Dummy arguments 
 
!//  Local variables 
 
 
    If (DebugCombos) { 
       debug ('** Entry into DeallocateComboStructures'); 
    } 
 
!// First, disassociate the array pointers 
    Nullify (ValBenefit, Combination); 
 
    If (Allocated(BenefitStruct)) { 
      Deallocate (BenefitStruct, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating BenefitStruct array.',''); 
      } 
    } 
 
    If (Allocated(BenVars)) { 
      Deallocate (BenVars, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating BenVars array.',''); 
      } 
    } 
 
    If (Allocated(FormVars)) { 
      Deallocate (FormVars, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating FormVars array.',''); 
      } 
    } 
 
    If (Allocated(ARETXnonCB)) { 
      Deallocate (ARETXnonCB, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating ARETXnonCB array.',''); 
      } 
    } 
 
    If (Allocated(CRETXnonCB)) { 
      Deallocate (CRETXnonCB, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating CRETXnonCB array.',''); 
      } 
    } 
 
    If (Allocated(HBENnonCB)) { 
      Deallocate (HBENnonCB, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating HBENnonCB array.',''); 
      } 
    } 
 
    If (Allocated(flag)) { 
      Deallocate (flag, stat=iastat); 
      If (iastat /= 0) { 
         lverror('DeallocateComboStructures', 1, iastat,'Error deallocating flag array.',''); 
      } 
    } 
 
    If (DebugCombos) { 
       debug ('** Exit from DeallocateComboStructures'); 
    } 
 
  } // end subroutine DEALLOCATECOMBOSTRUCTURES 
 
!/---------------------------------------------------------------------------------------- 
 
  Subroutine InitializeCombArrays (); 
 
    Implicit None; 
 
!// Dummy arguments 
 
!//  Local variables 
    int  i, j, k; 
 
 
    If (DebugCombos) { 
       debug ('** Entry into InitializeCombArrays'); 
    } 
 
    for( k  = 1; k  < Ubound (BenefitStruct,2); k  = k  + 1) {
 
      for( i  = 1; i  < Ubound (BenefitStruct,1); i  = i  + 1) {
 
         InitBenInfo(BenefitStruct(i,k)->Ans); 
        for( j  = 1; j  < 4; j  = j  + 1) {
 
           InitBenInfo(BenefitStruct(i,k)->Piece(j)); 
        } //end do loop  ! j
 
 
        BenefitStruct(i,k)->ServRedPct = 1d0; 
      } //end do loop  ! i
 
    } //end do loop  ! k
 
 
    for( k  = 1; k  < Ubound (BenVars,1); k  = k  + 1) {
 
       InitBenInfo(BenVars(k)); 
    } //end do loop  ! k
 
 
