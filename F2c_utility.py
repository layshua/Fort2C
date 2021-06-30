import csv
import pandas as pd

data = [['character', 'char'],
        ['integer', 'int'],
        ['logical', 'bool'],
        ['real', 'double']]

df_dtmap = pd.DataFrame(data, columns=['Fortran', 'C'])
#print(df_dtmap)

# read the source file line by line
sFilNam = r"Gutils.f90"
sFil = open(sFilNam, "r")


def fort2c(fortline):
    #print("Begin-----------")
    fortline = fortline.replace("\n", "")
    #print("//", fortline)


def isdeclarationline(fortline):
    if "::" in fortline:
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


def proc_realkindintent(fortline):
    parts = fortline.split('::')
    # print(parts)
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
    print(result)
    #print("end-----------")
    return result


def proc_intintent(fortline):
    parts = fortline.split('::')
    # print(parts)
    parts[1] = parts[1].replace('\n', '')
    _parts = parts[0].lower().replace(df_dtmap.iloc[1]['Fortran'], df_dtmap.iloc[1]['C'])
    iDtype = _parts.find(df_dtmap.iloc[1]['C'])
    _parts = _parts[0:iDtype + 3]

    result = _parts + ' ' + parts[1] + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        result = result + amendintentstring(fortline)
    print(result)
    #print('end---------------')
    return result


def proc_boolintent(fortline):
    parts = fortline.split('::')
    # print(parts)
    parts[1] = parts[1].replace('\n', '')
    PARTS = parts[0].replace('Logical', 'logical')
    PARTS = parts[0].replace(df_dtmap.iloc[2]['Fortran'], df_dtmap.iloc[2]['C'])
    iDtype = PARTS.find(df_dtmap.iloc[2]['C'])
    PARTS = PARTS[0:iDtype + 4]

    result = PARTS + ' ' + parts[1] + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        result = result + amendintentstring(fortline)

    print(result)
    #print('end---------------')
    return result

def proc_chararray(fortline):
    fortline = fortline.replace('Character', 'character')
    parts = fortline.split('::')
    #Right hand side values
    parts_right = parts[1].split("=")
    var_nam = parts_right[0].rstrip().lstrip()
    var_val = parts_right[1].replace("(/", "{").replace("/)", "}")
    var_val = var_val.replace("\n", "")
    print()
    parts_left = parts[0].split(",")
    var_type = parts_left[0].replace(parts_left[0], df_dtmap.iloc[0]['C'])
    var_dim  = parts_left[1].lower().lstrip().rstrip();
    var_dim_value_text = var_dim.replace("dimension", "").replace("(", "").replace(")", "")
    var_dim_values = var_dim_value_text.split(":")

    var_dim_lengt  = int(var_dim_values[1])-int(var_dim_values[0])+1

    var_is_param = ""
    if len(parts_left) == 3:
        if ("parameter" in parts_left[2].lstrip().rstrip().lower()):
            var_is_param = parts_left[2].lstrip().rstrip().lower()
            var_is_param = "//" +var_is_param
            var_type  = "const "+var_type
    istartpos = len(fortline)-len(fortline.lstrip());
    left_pad  = fortline[0:istartpos]
    var_type = left_pad+var_type
    print(var_type + " " + var_nam + "[" + str(var_dim_lengt) + "] = " + var_val + "; "+var_is_param)

def proc_charintent(fortline):
    fortline = fortline.replace('Character', 'character')
    parts = fortline.split('::')
    # print(parts)
    parts[1] = parts[1].replace('\n', '')
    parts_0 = parts[0].split(',')

    # print(parts_0)
    iDtype = parts_0[0].upper().find('CHARACTER') + 9

    slen = parts_0[0][iDtype: len(parts_0[0])]
    # print(slen)
    slen = slen.rstrip().lstrip()
    slen = slen[1:len(slen) - 1]
    # print('*CHARAC')
    # print(slen)
    slen = slen.replace('Len=', '')
    slen = slen.replace('len=', '')
    slen = slen.replace('*', '')
    slen = '[' + slen + ']'
    # print(slen)
    # PARTS  = parts[0].replace(df_dtmap.iloc[0]['Fortran'], df_dtmap.iloc[0]['C'])
    PARTS = parts[0].lower().replace(df_dtmap.iloc[0]['Fortran'], df_dtmap.iloc[0]['C'])
    iDtype = PARTS.find(df_dtmap.iloc[0]['C'])
    PARTS = PARTS[0:iDtype + 4]

    sarVal = parts[1].split(',')
    sVal = ''
    for z in sarVal:
        sVal = sVal + z + slen + ','

    sVal = sVal[0: len(sVal) - 1]
    # print(sVal)
    result = PARTS + ' ' + sVal + ';'
    if result.rstrip().lstrip() == '':
        result = fortline
    else:
        result = result + amendintentstring(fortline)
    print(result)
    #print('end---------------')
    return result

def proc_mathOperators(fortline):
    result = fortline
    if(fortline.find(" aint(")> -1):
        result = fortline.replace("aint", "trunc")
    if(fortline.find(" ** ")> -1):
        result = fortline.replace("**", "pow(")
    return result

def proc_LogicalOperators(fortline):
    fortline = fortline.lower()
    fortline = fortline.replace('.not.', '!')
    fortline = fortline.replace('.and.', '&&')
    fortline = fortline.replace('.or.', '||')
    fortline = fortline.replace('.eq.', '==')
    fortline = fortline.replace('.true.', 'true')
    fortline = fortline.replace('.false.', 'false')
    fortline = fortline.replace(' if ', ' if ')
    fortline = fortline.replace('subroutine', 'void')
    fortline = fortline.replace('then', '{')
    fortline = fortline.replace('endif', '}')
    fortline = fortline.replace(' call ', '')
    fortline = fortline.replace('%', '->')
    fortline = proc_mathOperators(fortline)
    if fortline.find('if ') > -1:
        fortline = fortline.replace('\n', '')
    elif (fortline.lstrip() == '}\n'):
        fortline = fortline.replace('\n', '')
    elif (fortline.upper().find('FUNCTION ') > -1):
        fortline = fortline.replace('\n', ' {')
    elif (fortline.upper().find('ENDDO') > -1):
        fortline = fortline.upper().replace('ENDDO\n', '} //end do loop')
    else:
        fortline = fortline.replace('\n', ';')
    print(fortline)
    return fortline


def proc_doloop(fortline):
    forloopvariables = fortline.split("=")
    forloopvariables[1] = forloopvariables[1].replace('\n', '')
    forlooplimits = forloopvariables[1].split(',')
    i_var = forloopvariables[0].replace("do", '');
    i_var = i_var.lstrip()
    i_equal = forlooplimits[0]
    i_range = forlooplimits[1]
    i_incr = forlooplimits[2]
    #print(i_equal, i_range, i_incr)
    print("   for( ", i_var, " =", i_equal, "; ", i_var, " < ", i_range, "; ", i_var, " = ", i_var, " + ", i_incr, "){")



# create an empty list to store the line from the files
lstLines = []

for declline in sFil:
    if isdeclarationline(declline):
        lstLines.append(declline)
        fort2c(declline)
        #if (('REAL' in declline.upper()) and ('DIMENSION(' in declline.upper()) and ('ALLOCATABLE(' in declline.upper()) ):
        #    proc_realkindintent(declline)
        if (('Real' in declline) and ('intent(' in declline)) or ('(Kind=' in declline):
            proc_realkindintent(declline)
        elif (('Integer' in declline) and ('intent' in declline)) or ('Integer' in declline):
            proc_intintent(declline)
        elif (('logical' in declline) and ('intent' in declline)) or ('logical' in declline):
            proc_boolintent(declline)
        elif ('CHARACTER' in declline.upper()) and ('dimension' in declline.lower()) and ('(/' in declline):
            proc_chararray(declline)
        elif (('CHARACTER' in declline.upper()) and ('intent' in declline)) or ('CHARACTER' in declline.upper()):
            proc_charintent(declline)
    else:
        a = (declline.lstrip())
        if len(a) > 0:
            if a[0] !=  "!" :
                if a.upper().find('END SUBROUTINE') > -1:
                    print(a.upper().replace('END SUBROUTINE', '} // end subroutine'))
                elif a.upper().find('END FUNCTION') > -1:
                    print(a.upper().replace('END FUNCTION', '} // end function'))
                elif len(a.lstrip()) > 0:
                    if a.find("while") < 0:
                        if a.lstrip()[0:2] == 'do':
                            proc_doloop(declline)
                        else:
                            proc_LogicalOperators(declline)
                    else:
                        proc_LogicalOperators(declline)
            else:
                proc_LogicalOperators(declline)
        else:
            print(a)
sFil.close()
