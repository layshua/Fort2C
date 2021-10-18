import pandas as pd

data = [['character', 'char'],
        ['integer', 'int'],
        ['logical', 'bool'],
        ['real', 'double']]

df_dtmap = pd.DataFrame(data, columns=['Fortran', 'C'])
# LV_print(df_dtmap)


# read the source file line by line
# sFilNam = r"test.f90"
sFilNam = r"test.f90"
sFil = open(sFilNam, "r")
sFil_c = sFilNam.replace(".f90", ".c")
soutfile = open(sFil_c, "w")
soutfile = open(sFil_c, "a")

is_prev_case_Statement = False


def LV_print(C_Synt):
    print(C_Synt)
    soutfile.write(C_Synt + " \n")


def fort2c(fortline):
    # LV_print("Begin-----------")
    fortline = fortline.replace("\n", "")
    # LV_print("//", fortline)


def isdeclarationline(fortline):
    if ("::" in fortline) or (("DOUBLE") in fortline.upper() and ("PRECISION") in fortline.upper()):
        return True
    else:
        return False


def amendintentstring(fortline):
    result = ''
    if fortline.upper().find("INTENT(OUT)") > 0:
        result = result + " //output Parameter"
    if fortline.upper().find("INTENT(IN)") > 0:
        result = result + " //input parameter"
    if fortline.upper().find("INTENT(INOUT)") > 0:
        result = result + " //input&Output parameter"
    return result

def proc_DoublePrecision(fortline):
    parts = fortline.split('Precision')
    parts[1] = parts[1].replace('\n', '')
    idx_Start = fortline.index("Double")
    lpad = fortline[0:idx_Start]

    Result = '{} {} {} {};'.format(lpad, 'double', parts[1], '//Not Converted')
    LV_print(Result)


def proc_realdimension(fortline):
    parts = fortline.split('::')
    parts[1] = parts[1].replace('\n', '')
    parts_0 = parts[0].split(',')
    part1 = parts[1].split(',')
    ar2D = False
    noDimension = False

    if len(parts_0) > 1:
        parts_0[1] = parts_0[1].lower().replace('dimension', '')
        parts_0[1] = parts_0[1].replace('(', '').replace(')', '')
        if len(parts_0) == 3:
            parts_0[2] = parts_0[2].replace(')', '')
            arLength1 = parts_0[1]
            arLength2 = parts_0[2]
            ar2D  =  True
        else:
          ar_Length =  parts_0[1];
          if (type(ar_Length) == str):
            arLength = (parts_0[1])
          else:
            arLength = int(parts_0[1])
          ar2D = False
    else:
        noDimension = True
        arDim = "Double"

    for _part1 in part1:
      if ar2D == True:
    #    arDim = '[' + arLength1, str(arLength2) + '] '
        arDim = arLength1 + ','+ arLength2;
        arDim = '[' + arDim +']'
        parts[1] = parts[1].replace(_part1, _part1 + arDim)
      elif  noDimension == True:
        arDim = 'double'
      else:
        arDim = '[' + str(arLength) + '] '
        parts[1] = parts[1].replace(_part1, _part1 + arDim)
    idx_start = fortline.lower().index("real")

    lpad = fortline[0:idx_start]
    result = '{} {} {} ;'.format(lpad, 'double', parts[1])
    LV_print(result)

def proc_realDim(fortline):
    parts = fortline.split('::')
    # LV_print(parts)
    parts[0] = parts[0].replace('Real (8)', 'double')
    parts[0] = parts[0].replace('Real(8)', 'double')
    parts[1] = parts[1].replace('\n', '')

    result = parts[0] + ' ' + parts[1] + ';'
    LV_print(result)

    return result


def proc_realkindintent(fortline):
    parts = fortline.split('::')
    # LV_print(parts)
    parts[1] = parts[1].replace('\n', '')
    PARTS = parts[0].lower().replace(df_dtmap.iloc[3]['Fortran'], df_dtmap.iloc[3]['C'])
    iDtype = PARTS.find(df_dtmap.iloc[3]['C'])
    PARTS = PARTS[0:iDtype + 6]

    result = PARTS + ' ' + parts[1] + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        result = result + amendintentstring(fortline)
        result = result.replace("\n", "")
    LV_print(result)
    # LV_print("end-----------")
    return result


def proc_intintent(fortline):
    parts = fortline.split('::')
    # LV_print(parts)
    parts[1] = parts[1].replace('\n', '')
    _parts = parts[0].lower().replace(df_dtmap.iloc[1]['Fortran'], df_dtmap.iloc[1]['C'])
    iDtype = _parts.find(df_dtmap.iloc[1]['C'])
    _parts = _parts[0:iDtype + 3]

    result = _parts + ' ' + parts[1] + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        if fortline.find('Parameter') > -1:
            result = result + amendintentstring(fortline) + " //Parameter"
        else:
            result = result + amendintentstring(fortline)

    LV_print(result)
    # LV_print('end---------------')
    return result


def proc_boolintent(fortline):
    parts = fortline.split('::')
    # LV_print(parts)
    parts[1] = parts[1].replace('\n', '')
    PARTS = parts[0].replace('Logical', 'logical')
    print(df_dtmap.iloc[2]['Fortran'])
    print(df_dtmap.iloc[2]['C'])
    PARTS = PARTS.replace(df_dtmap.iloc[2]['Fortran'], df_dtmap.iloc[2]['C'])
    iDtype = PARTS.find(df_dtmap.iloc[2]['C'])
    PARTS = PARTS[0:iDtype + 4]
    result = PARTS + ' ' + parts[1] + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        result = result + amendintentstring(fortline)
    LV_print(result)
    # LV_print('end---------------')
    return result


def proc_chararray(fortline):
    fortline = fortline.replace('Character', 'character')
    parts = fortline.split('::')
    # Right hand side values
    parts_right = parts[1].split("=")
    var_nam = parts_right[0].rstrip().lstrip()
    var_val = parts_right[1].replace("(/", "").replace("/)", "}")
    var_val = var_val.replace("\n", "")
    # LV_print()
    parts_left = parts[0].split(",")
    var_type = parts_left[0].replace(parts_left[0], df_dtmap.iloc[0]['C'])
    var_dim = parts_left[1].lower().lstrip().rstrip();
    var_dim_value_text = var_dim.replace("dimension", "").replace("(", "").replace(")", "")
    var_dim_values = var_dim_value_text.split(":")
    var_dim_lengt = int(var_dim_values[1]) - int(var_dim_values[0]) + 1
    var_is_param = ""
    if len(parts_left) == 3:
        if ("parameter" in parts_left[2].lstrip().rstrip().lower()):
            var_is_param = parts_left[2].lstrip().rstrip().lower()
            var_is_param = "//" + var_is_param
            var_type = "const " + var_type
    istartpos = len(fortline) - len(fortline.lstrip());
    left_pad = fortline[0:istartpos]
    var_type = left_pad + var_type
    LV_print(var_type + " " + var_nam + "[" + str(var_dim_lengt) + "] = " + var_val + "; " + var_is_param)


def proc_charintent(fortline):
    fortline = fortline.replace('Character', 'character')
    parts = fortline.split('::')
    # LV_print(parts)
    parts[1] = parts[1].replace('\n', '')
    parts_0 = parts[0].split(',')
    # LV_print(parts_0)
    iDtype = parts_0[0].upper().find('CHARACTER') + 9
    slen = parts_0[0][iDtype: len(parts_0[0])]
    # LV_print(slen)
    slen = slen.rstrip().lstrip()
    slen = slen[1:len(slen) - 1]
    # LV_print('*CHARAC')
    # LV_print(slen)
    slen = slen.replace('Len=', '')
    slen = slen.replace('len=', '')
    slen = slen.replace('*', '')
    slen = '[' + slen + ']'
    # LV_print(slen)
    # PARTS  = parts[0].replace(df_dtmap.iloc[0]['Fortran'], df_dtmap.iloc[0]['C'])
    PARTS = parts[0].lower().replace(df_dtmap.iloc[0]['Fortran'], df_dtmap.iloc[0]['C'])
    iDtype = PARTS.find(df_dtmap.iloc[0]['C'])
    PARTS = PARTS[0:iDtype + 4]
    sarVal = parts[1].split(',')
    sVal = ''
    for z in sarVal:
        sVal = sVal + z + slen + ','

    sVal = sVal[0: len(sVal) - 1]
    # LV_print(sVal)
    result = PARTS + ' ' + sVal + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        result = result + amendintentstring(fortline)
    LV_print(result)
    # LV_print('end---------------')
    return result


def proc_mathOperators(fortline):
    result = fortline
    if (fortline.find(" aint(") > -1):
        result = fortline.replace("aint", "trunc")
    if (fortline.find(" ** ") > -1):
        result = fortline.replace("**", "pow(")
    if fortline.find('Abs') > -1:
        result = fortline.replace("Abs", "abs")

    return result


def proc_LogicalOperators(fortline):
    # fortline = fortline.lower()
    fortline = fortline.replace('.not.', '!')
    fortline = fortline.replace('.and.', '&&')
    fortline = fortline.replace('.or.', '||')
    fortline = fortline.replace('.eq.', '==')
    fortline = fortline.replace('.True.', 'true')
    fortline = fortline.replace('.true.', 'true')
    fortline = fortline.replace('.false.', 'false')
    fortline = fortline.replace('.False.', 'false')
    fortline = fortline.replace(' if ', ' if ')
    fortline = fortline.replace('subroutine', 'void')
    fortline = fortline.replace('then', '{')
    fortline = fortline.replace('Then', '{')
    fortline = fortline.replace('else', '} else {')
    fortline = fortline.replace('Else', '} else {')
    fortline = fortline.replace('endif', '}')
    fortline = fortline.replace('End If', '}')
    fortline = fortline.replace('call ', '')
    fortline = fortline.replace('Call', '')
    fortline = fortline.replace('%', '->')
    fortline = fortline.replace('/=', '!=')
    fortline = fortline.replace('debug', 'Debug')
    fortline = proc_mathOperators(fortline)

    if fortline.find('if ') > -1:
        fortline = fortline.replace('\n', '')
    elif fortline.find('If ') > -1:
        fortline = fortline.replace('If', 'if')
        fortline = fortline.replace('\n', '')
    elif fortline.find('else') > -1:
        fortline = fortline.replace('\n', '')
    elif fortline.find('Else') > -1:
        fortline = fortline.replace('\n', '')
    elif (fortline.lstrip() == '}\n'):
        fortline = fortline.replace('\n', '')
    elif (fortline.upper().find('FUNCTION ') > -1):
        fortline = fortline.replace('\n', ' { ')
    elif (fortline.upper().find('ENDDO') > -1):
        fortline = fortline.upper().replace('ENDDO', '} //end do loop')
    elif (fortline.find('End Do') > -1):
        fortline = fortline.replace('End Do', '} //end do loop')
    elif (fortline.find('END DO') > -1):
        fortline = fortline.upper().replace('END DO', '} //end do loop')
    elif (fortline.find('&') == len(fortline) - 2):
        fortline = fortline.replace('&', '').replace('\n', '')
    elif (fortline.find('0d0') > -1):
        fortline = fortline.replace('0d0', '0').replace('\n', ';')

    else:
        if fortline.find(';') < 0:
            fortline = fortline.replace('\n', ';')
    LV_print(fortline)
    return fortline


def proc_doloop(fortline):
    comment_idx = fortline.find('!')
    fortline_minus_comment = fortline[0: comment_idx]
    forloopvariables = fortline_minus_comment.split("=")
    forloopvariables[1] = forloopvariables[1].rstrip()
    forloopvariables[1] = forloopvariables[1].replace('\n', '')
    forlooplimits = forloopvariables[1].split(',')
    i_var = forloopvariables[0].replace("do", '')
    i_var = forloopvariables[0].replace("Do", '')
    i_var = i_var.lstrip()
    i_equal = forlooplimits[0]
    i_range = forlooplimits[1]
    if (len(forlooplimits) > 2):
        i_incr = forlooplimits[2]
    else:
        i_incr = '+'
    # LV_print(i_equal, i_range, i_incr)
    istartIdx = fortline_minus_comment.find('Do')
    if istartIdx == -1:
        istartIdx = fortline_minus_comment.find('do')
    lpad = fortline[0: istartIdx]
    result = (
                lpad + "for( " + i_var + " =" + i_equal + "; " + i_var + " < " + i_range + "; " + i_var + " = " + i_var + " + " + i_incr + ") {  //")
    result = result.replace('!', '')
    result = fortline.replace(fortline_minus_comment, result)
    LV_print(result)


def proc_typedim_allocatable(fortline):
    fortline_shadow = fortline.split('::')
    typevalue = ''
    dimvalue = ''
    allocatvalue = ''
    targetvalue = ''

    fortline_shadow[0] = fortline_shadow[0].replace(':, :', ':_ :')
    fortline_shadow[0] = fortline_shadow[0].replace(':, :, :', ':_ :_ :')
    fortline_prefix_list = fortline_shadow[0].split(',')
    for fortline_prefix in fortline_prefix_list:
        if fortline_prefix.upper().find('TYPE') > -1:
            fortline_prefix = fortline_prefix.replace('Type', '')
            fortline_prefix = fortline_prefix.replace('(', '').replace(')', '')
            typevalue = fortline_prefix
        if fortline_prefix.upper().find('REAL') > -1:
            fortline_prefix = fortline_prefix.replace('Real', '')
            fortline_prefix = fortline_prefix.replace('(', '').replace(')', '')
            typevalue = fortline_prefix
        if fortline_prefix.upper().find('INTEGER') > -1:
            fortline_prefix = fortline_prefix.replace('Integer', 'int')
            typevalue = fortline_prefix
        if fortline_prefix.upper().find('LOGICAL') > -1:
            fortline_prefix = fortline_prefix.replace('Logical', 'bool')
            typevalue = fortline_prefix
        if fortline_prefix.upper().find('DIMENSION') > -1:
            fortline_prefix = fortline_prefix.replace('_', ',')
            dimvalue = fortline_prefix
        if fortline_prefix.upper().find('ALLOCATABLE') > -1:
            allocatvalue = 'Allocatable'
        if fortline_prefix.upper().find('TARGET') > -1:
            targetvalue = 'Target'
        result = typevalue
    fortline_suffix_list = fortline_shadow[1].split(',')
    strfortline_suffixes = ''
    for fortline_suffixes in fortline_suffix_list:
        strfortline_suffixes = strfortline_suffixes + '*' + fortline_suffixes.lstrip().replace('\n', '') + ','
    strfortline_suffixes = strfortline_suffixes[0: len(strfortline_suffixes) - 1]
    strfortline_comment = '//' + allocatvalue + ',' + dimvalue + ',' + targetvalue
    sResult = typevalue + ' ' + strfortline_suffixes + ' ;' + strfortline_comment
    LV_print(sResult)


def proc_dim_allocatable(fortline):
    print('Hello world')


# create an empty list to store the line from the files
lstLines = []

for declline in sFil:

    if isdeclarationline(declline):
        lstLines.append(declline)
        fort2c(declline)
        if (('TYPE' in declline.upper()) and ('DIMENSION(' in declline.upper()) and (
                'ALLOCATABLE' in declline.upper())):
            proc_typedim_allocatable(declline)
        elif (('DIMENSION(' in declline.upper()) and ('ALLOCATABLE' in declline.upper()) and not (
                'TYPE' in declline.upper())):
            proc_typedim_allocatable(declline)
        elif (('Double') in declline and ('Precision' in declline)):
            proc_DoublePrecision(declline)
        elif (('Real' in declline) and ('(Double)' in declline) and ('Dimension' in declline)) and not (
                ('Allocatable') in declline):
            proc_realdimension(declline)
        elif (('Real' in declline) and ('(Double)' in declline) and not ('Allocatable') in declline):
            proc_realdimension(declline)
        elif (('Real' in declline) and ('intent(' in declline)) or ('(Kind=' in declline):
            proc_realkindintent(declline)
        elif (('Real' in declline) and '(8)' in declline):
            proc_realDim(declline)
        elif (('Integer' in declline) and ('intent' in declline)) or (
                ('Integer' in declline) and not ('Allocatable') in declline):
            proc_intintent(declline)
        elif (('logical' in declline) and ('intent' in declline)) or (
                ('Logical' in declline) and not ('Allocatable') in declline):
            proc_boolintent(declline)
        elif ('CHARACTER' in declline.upper()) and ('dimension' in declline.lower()) and ('(/' in declline):
            proc_chararray(declline)
        elif (('CHARACTER' in declline.upper()) and ('intent' in declline)) or ('CHARACTER' in declline.upper()):
            proc_charintent(declline)
        else:
            declline = declline.replace('\n', '')
            if declline.find("::") > -1:
                if declline.find('&') > -1:
                    declline = declline.replace('&', '')
            else:
                declline = declline.replace('&', '')
            LV_print(declline + '//Not converted')
    else:
        a = (declline.rstrip())
        if len(a) > 0:
            if a[0] != "!":
                if a.upper().find('END SUBROUTINE') > -1:
                    LV_print(a.upper().replace('END SUBROUTINE', '} // end subroutine'))
                elif a.upper().find('END FUNCTION') > -1:
                    LV_print(a.upper().replace('END FUNCTION', '} // end function'))
                elif a.upper().find('CASE') > - 1 and a.upper().find('SELECT') > - 1:
                    a = a.replace('\n', '')
                    result = a.replace('Select Case', 'Switch')
                    result = result + '{'
                    LV_print(result)
                elif a.upper().find('CASE') > - 1:
                    a = a.replace('\n', '')
                    idx_end = a.find('!')
                    if idx_end > 0:
                        result = a.replace('!', ': //')
                    else:
                        result = a + ':'
                    if is_prev_case_Statement == True:
                        LV_print('break;')
                    is_prev_case_Statement = True
                    LV_print(result)

                elif a.upper().find('SELECT') > - 1 and a.upper().find('END') > - 1:
                    a = a.replace('\n', '')
                    LV_print('break;')
                    result = a.replace('End Select', '}')
                    is_prev_case_Statement = False
                    LV_print(result)
                elif len(a.lstrip()) > 0:
                    if a.find("while") < 0:
                        if a.lstrip()[0:2] in ['do', 'Do', 'DO']:
                            proc_doloop(declline)
                        else:
                            proc_LogicalOperators(declline)
                    else:
                        proc_LogicalOperators(declline)
            else:
                b = a.replace('\n', '')
                b = b.replace('!', '//')
                LV_print(b[0:len(declline) - 1])
                # proc_LogicalOperators(declline)
        else:
            LV_print(a)
sFil.close()
soutfile.close()
