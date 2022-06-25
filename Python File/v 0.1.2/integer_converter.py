# Integer Converter
# Hao Jiang
# v. 0.1.2







# check functions

def integer_check(x, radix):
    # x is a list or tuple
    # the 0-th element in x is bool, True = positive integer
    #                                False = negative integer
    # for n from 1 to x_len-1, the integer of x is
    # x = x[1] + x[2]*(radix^1) + x[3]*(radix^2) + ... + x[x_len-1]*(radix^(x_len-2))
    res = False
    if isinstance(x, list) | isinstance(x, tuple):
        if isinstance(radix, int):
            if radix > 1:
                x_len = len(x)
                if x_len > 1:
                    if isinstance(x[0], bool):
                        res = True
                        for n in range(1, x_len):
                            temp_bool = check_less_nonneg_int_int(x[n], radix)
                            if temp_bool is None:
                                res = False
                                break
                            elif temp_bool is False:
                                res = False
                                break
                        if x_len > 2:
                            if x[-1] < 1:
                                res = False
    return res
    
def check_less_nonneg_int_int(x, y):
    res = None
    if isinstance(x, int) & isinstance(y, int):
        if (x >= 0) & (y >= 0):
            res = x < y
    return res

def check_lesseq_nonneg_int_int(x, y):    
    res = None
    if isinstance(x, int) & isinstance(y, int):
        if (x >= 0) & (y >= 0):
            res = x < y
    return res

def check_more_nonneg_int_int(x, y):    
    res = None
    if isinstance(x, int) & isinstance(y, int):
        if (x >= 0) & (y >= 0):
            res = x < y
    return res

def check_moreeq_nonneg_int_int(x, y):    
    res = None
    if isinstance(x, int) & isinstance(y, int):
        if (x >= 0) & (y >= 0):
            res = x < y
    return res

def check_eq_nonneg_int_int(x, y):    
    res = None
    if isinstance(x, int) & isinstance(y, int):
        if (x >= 0) & (y >= 0):
            res = x < y
    return res

def check_noteq_nonneg_int_int(x, y):    
    res = None
    if isinstance(x, int) & isinstance(y, int):
        if (x >= 0) & (y >= 0):
            res = x < y
    return res

def integer_less_tuple_tuple(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: bool, x < y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:         
        x_len = len(x)
        y_len = len(y)
        if x[0] is True:
            if y[0] is True:
                if x_len < y_len:
                    res = True
                elif x_len > y_len:
                    res = False
                else:
                    res = False
                    for n in range(x_len-1, 0, -1):
                        if x[n] < y[n]:
                            res = True
                            break
                        elif x[n] > y[n]:
                            break
            else:
                res = False
        else:
            if y[0] is True:
                if (x_len == 2) & (y_len == 2):
                    if (x[1] == 0) & (y[1] == 0):
                        res = False
                    else:
                        res = True
                else:
                    res = True
            else:
                if x_len < y_len:
                    res = False
                elif x_len > y_len:
                    res = True
                else:
                    res = False
                    for n in range(x_len-1, 0, -1):
                        if x[n] > y[n]:
                            res = True
                            break
                        elif x[n] < y[n]:
                            break
    return res

def integer_lesseq_tuple_tuple(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: bool, x <= y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:        
        x_len = len(x)
        y_len = len(y)
        if x[0] is True:
            if y[0] is True:
                if x_len < y_len:
                    res = True
                elif x_len > y_len:
                    res = False
                else:
                    res = True
                    for n in range(x_len-1, 0, -1):
                        if x[n] > y[n]:
                            res = False
                            break
                        elif x[n] < y[n]:
                            break
            else:
                if (x_len == 2) & (y_len == 2):
                    if (x[1] == 0) & (y[1] == 0):
                        res = True
                    else:
                        res = False
                else:
                    res = False
        else:
            if y[0] is True:
                res = True
            else:
                if x_len < y_len:
                    res = False
                elif x_len > y_len:
                    res = True
                else:
                    res = True
                    for n in range(x_len-1, 0, -1):
                        if x[n] < y[n]:
                            res = False
                            break
                        elif x[n] > y[n]:
                            break
    return res

def integer_more_tuple_tuple(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: bool, x > y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:       
        x_len = len(x)
        y_len = len(y)
        if x[0] is True:
            if y[0] is True:
                if x_len < y_len:
                    res = False
                elif x_len > y_len:
                    res = True
                else:
                    res = False
                    for n in range(x_len-1, 0, -1):
                        if x[n] > y[n]:
                            res = True
                            break
                        elif x[n] < y[n]:
                            break
            else:
                if (x_len == 2) & (y_len == 2):
                    if (x[1] == 0) & (y[1] == 0):
                        res = False
                    else:
                        res = True
                else:
                    res = True      
        else:
            if y[0] is True:
                res = False
            else:
                if x_len < y_len:
                    res = True
                elif x_len > y_len:
                    res = False
                else:
                    res = False
                    for n in range(x_len-1, 0, -1):
                        if x[n] < y[n]:
                            res = True
                            break
                        elif x[n] > y[n]:
                            break
    return res

def integer_moreeq_tuple_tuple(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: bool, x >= y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:        
        x_len = len(x)
        y_len = len(y)
        if x[0] is True:
            if y[0] is True:
                if x_len < y_len:
                    res = False
                elif x_len > y_len:
                    res = True
                else:
                    res = True
                    for n in range(x_len-1, 0, -1):
                        if x[n] < y[n]:
                            res = False
                            break
                        elif x[n] > y[n]:
                            break
            else:
                res = True                
        else:
            if y[0] is True:
                if (x_len == 2) & (y_len == 2):
                    if (x[1] == 0) & (y[1] == 0):
                        res = True
                    else:
                        res = False
                else:
                    res = False
            else:
                if x_len < y_len:
                    res = True
                elif x_len > y_len:
                    res = False
                else:
                    res = True
                    for n in range(x_len-1, 0, -1):
                        if x[n] > y[n]:
                            res = False
                            break
                        elif x[n] < y[n]:
                            break
    return res

def integer_eq_tuple_tuple(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: bool, x == y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:      
        x_len = len(x)
        y_len = len(y)
        if x_len != y_len:
            res = False        
        else:            
            if x[0] is y[0]:
                res = True
                for n in range(x_len-1, 0, -1):
                    if x[n] != y[n]:
                        res = False
                        break
            elif (x_len == 2) & (y_len == 2):
                if (x[1] == 0) & (y[1] == 0):
                    res = True
                else:
                    res = False
            else:
                res = False
    return res

def integer_noteq_tuple_tuple(x, y, radix, check = True):    
    # input: integer vector, x, y
    # output: bool, x != y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:        
        x_len = len(x)
        y_len = len(y)
        if x_len != y_len:
            res = True        
        else:            
            if x[0] is y[0]:
                res = False
                for n in range(x_len-1, 0, -1):
                    if x[n] != y[n]:
                        res = True
                        break
            elif (x_len == 2) & (y_len == 2):
                if (x[1] == 0) & (y[1] == 0):
                    res = False
                else:
                    res = True
            else:
                res = True
    return res

def integer_is_even(x, radix, check = True):    
    # input: integer vector, x
    # output: bool, True if x is an even integer
    #               False if x is an odd integer
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:
        temp_num_0 = radix/2
        if_radix_even = int(temp_num_0) == temp_num_0
        if if_radix_even:
            temp_num_0 = x[1]/2
            res = int(temp_num_0) == temp_num_0
        else:
            res = True
            x_len = len(x)
            for n in range(1, x_len):
                temp_num_0 = x[n]/2
                if int(temp_num_0) != temp_num_0:
                    res = not res
    return res



# operation functions

def integer_inverse(x, radix, check = True):
    # input: integer vector, x
    # output: integer vector, -x
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None: 
        res = []
        x_len = len(x)
        res.append(not x[0])
        for n in range(1, x_len):
            res.append(x[n])
        res = tuple(res)
    return res

def integer_absolute(x, radix, check = True):
    # input: integer vector, x
    # output: integer vector, |x|
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None: 
        res = []
        x_len = len(x)
        res.append(True)
        for n in range(1, x_len):
            res.append(x[n])
        res = tuple(res)
    return res

def integer_plus(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: integer vector, x+y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:         
        if integer_moreeq_tuple_tuple(integer_absolute(x, radix, check), 
                                      integer_absolute(y, radix, check), 
                                      radix, check = False):
            x1 = x
            x_len = len(x1)
            y1 = y
            y_len = len(y1)
        else:
            x1 = y
            x_len = len(x1)
            y1 = x
            y_len = len(y1)
        res = []
        for n in range(x_len):
            res.append(x1[n])
        res.append(0)
        if x1[0] is y1[0]: 
            for n in range(1, y_len):
                temp_num = res[n]+y1[n]
                if temp_num < radix:
                    res[n] = temp_num
                else:
                    res[n] = temp_num-radix
                    res[n+1] += 1
            for n in range(y_len, x_len):
                if res[n] >= radix:
                    res[n] -= radix
                    res[n+1] += 1
                else:
                    break
        else:
            for n in range(1, y_len):
                temp_num = res[n]-y1[n]
                if temp_num >= 0:
                    res[n] = temp_num
                else:
                    res[n] = temp_num+radix
                    res[n+1] -= 1
            for n in range(y_len, x_len):
                if res[n] < 0:
                    res[n] += radix
                    res[n+1] -= 1
                else:
                    break
        res = trim_initial_zeros(res, radix, check = False)
    return res

def integer_minus(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: integer vector, x-y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:
        res = integer_plus(x, 
                           integer_inverse(y, radix, check = False), 
                           radix, check = False)
    return res

def integer_multiply(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: integer vector, x*y
    if check:
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:
        if ((integer_eq_tuple_tuple(x, [True, 0], radix, check = False)) | 
            (integer_eq_tuple_tuple(y, [True, 0], radix, check = False))):
            res = (True, 0)
        elif integer_eq_tuple_tuple(x, [True, 1], radix, check = False):
            res = tuple(y)
        elif integer_eq_tuple_tuple(x, [False, 1], radix, check = False):
            res = integer_inverse(y, radix, check = False)
        elif integer_eq_tuple_tuple(y, [True, 1], radix, check = False):
            res = tuple(x)
        elif integer_eq_tuple_tuple(y, [False, 1], radix, check = False):
            res = integer_inverse(x, radix, check = False)
        else: 
            x_len = len(x)
            y_len = len(y)
            if x[0] is y[0]:
                res = [True]
            else:
                res = [False]
            temp_vec_0 = []
            total_len = x_len+y_len-1
            for n in range(total_len):
                temp_vec_0.append(0)
            for n0 in range(1, x_len):
                for n1 in range(1, y_len):
                    temp_num = n0+n1-2            
                    temp_vec = multiply_digits(x[n0], y[n1], radix, 
                                               check = False)
                    temp_len = len(temp_vec)
                    if temp_len == 1:
                        temp_num_1 = temp_vec_0[temp_num] + temp_vec[0]
                        if temp_num_1 < radix:
                            temp_vec_0[temp_num] = temp_num_1
                        else:
                            temp_vec_0[temp_num] = temp_num_1-radix
                            temp_vec_0[temp_num+1] += 1
                        temp_num += 1
                    else:
                        temp_num_1 = temp_vec_0[temp_num] + temp_vec[0]
                        if temp_num_1 < radix:
                            temp_vec_0[temp_num] = temp_num_1
                        else:
                            temp_vec_0[temp_num] = temp_num_1-radix
                            temp_vec_0[temp_num+1] += 1
                        temp_num_1 = temp_vec_0[temp_num+1] + temp_vec[1]
                        if temp_num_1 < radix:
                            temp_vec_0[temp_num+1] = temp_num_1
                        else:
                            temp_vec_0[temp_num+1] = temp_num_1-radix
                            temp_vec_0[temp_num+2] += 1
                        temp_num += 2
                    for n2 in range(temp_num, total_len-1):
                        if temp_vec_0[n2] >= radix:
                            temp_vec_0[n2] -= radix
                            temp_vec_0[n2+1] += 1
                        else:
                            break
            res.extend(temp_vec_0)
            res = trim_initial_zeros(res, radix, check = False)
    return res

def integer_modulo(x, y, radix, check = True):
    # input: integer vector, x, y
    #                        where |y| > 1
    # output: dictionary, {"quo": q, "rem": r}
    #                     where r+qy = x
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix): 
            if len(y) > 2:
                res = True
            elif y[1] > 1:
                res = True
    else:
        res = True
    if not res is None:
        x_sign = x[0]
        x = integer_absolute(x, radix, check = False)
        y_sign = y[0]
        y = integer_absolute(y, radix, check = False)
        if integer_less_tuple_tuple(x, y, radix, check = False):
            temp_vec_0 = x
            temp_vec_1 = [True, 0]
        else:
            x_len = len(x)
            y_len = len(y)
            dif_len = x_len-y_len+1
            temp_upper_vec = [True]
            for n in range(dif_len):
                temp_upper_vec.append(0)
            temp_upper_vec.append(1)
            temp_lower_vec = [True, 0]
            temp_lower_num_vec = [True, 0]
            temp_diff_vec = integer_minus(temp_upper_vec, 
                                          temp_lower_vec, 
                                          radix, check = False)            
            temp_bool_0 = True
            temp_bool_1 = True
            while temp_bool_0:
                temp_half_vec = floor_half_value(temp_diff_vec, 
                                                 radix, check = False)["half"] 
                temp_mid_vec = integer_plus(temp_lower_vec, 
                                            temp_half_vec, 
                                            radix, check = False)                
                temp_mid_num_vec = integer_multiply(temp_mid_vec, 
                                                    y, radix, 
                                                    check = False)
                if integer_less_tuple_tuple(x, temp_mid_num_vec, 
                                            radix, check = False):
                    temp_upper_vec = temp_mid_vec
                elif integer_more_tuple_tuple(x, temp_mid_num_vec, 
                                              radix, check = False):
                    temp_lower_vec = temp_mid_vec
                    temp_lower_num_vec = temp_mid_num_vec
                else:
                    temp_lower_vec = temp_mid_vec
                    temp_lower_num_vec = temp_mid_num_vec
                    temp_bool_1 = False  
                if temp_bool_1:
                    temp_diff_vec = integer_minus(temp_upper_vec, 
                                                  temp_lower_vec, 
                                                  radix, check = False)
                    if (len(temp_diff_vec) == 2) & (temp_diff_vec[1] <= 1):
                        temp_bool_0 = False
                else:
                    temp_bool_0 = False
            if temp_bool_1:
                temp_vec_0 = integer_minus(x, temp_lower_num_vec, 
                                           radix, check = False)
            else:
                temp_vec_0 = tuple([True, 0])
            temp_vec_1 = temp_lower_vec
        if x_sign:
            if y_sign is False:
                temp_vec_1 = integer_inverse(temp_vec_1, 
                                             radix, check = False)
        else: 
            if y_sign:
                temp_vec_1 = integer_inverse(temp_vec_1, 
                                             radix, check = False)
                temp_vec_1 = minus_one(temp_vec_1, radix, check = False)
                temp_vec_0 = integer_inverse(temp_vec_0, 
                                             radix, check = False)
                temp_vec_0 = integer_plus(temp_vec_0, y, 
                                          radix, check = False)
            else:
                temp_vec_1 = plus_one(temp_vec_1, radix, check = False)
                temp_vec_0 = integer_inverse(temp_vec_0, 
                                             radix, check = False)
                temp_vec_0 = integer_plus(temp_vec_0, y, 
                                          radix, check = False)
        res = {"quo": temp_vec_1, 
               "rem": temp_vec_0}
    return res

def integer_power(x, y, radix, check = True):
    # input: integer vector, x, y
    #                        where y >= 0
    # output: integer vector, x^y
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix): 
            if integer_moreeq_tuple_tuple(y, [True, 0], radix, check = False):
                res = True 
    else:
        res = True
    if not res is None:        
        if integer_more_tuple_tuple(y, [True, 1], radix, check = False):
            if integer_eq_tuple_tuple(x, [True, 0], radix, check = False):
                res = tuple([True, 0])
            elif integer_eq_tuple_tuple(x, [True, 1], radix, check = False):
                res = tuple([True, 1])
            elif integer_eq_tuple_tuple(x, [False, 1], radix, check = False):                
                temp_sign = integer_is_even(y, radix, check = False)                
                res = tuple([temp_sign, 1])
            else:
                res = [True, 1]
                temp_num_vec_0 = integer_multiply([True, 1], x, 
                                                  radix, check = False)
                temp_num_vec_1 = integer_tuple_change_radix(y, radix, 
                                                            2, check = False)
                y_len = len(temp_num_vec_1)
                for n in range(1, y_len):
                    if temp_num_vec_1[n] > 0:
                        res = integer_multiply(res, temp_num_vec_0, 
                                               radix, check = False)
                    if n < y_len-1:
                        temp_num_vec_0 = integer_multiply(temp_num_vec_0, temp_num_vec_0, 
                                                          radix, check = False)
        elif integer_more_tuple_tuple(y, [True, 0], radix, check = False):
            res = tuple(x)
        else:
            res = tuple([True, 1])
    return res

def integer_factorial(x, radix, check = True):
    # input: integer vector, x
    #                        where x >= 0
    # output: integer vector, x!
    if check:
        res = None
        if integer_check(x, radix): 
            if integer_moreeq_tuple_tuple(x, [True, 0], radix, check = False):
                res = True 
    else:
        res = True
    if not res is None: 
        if integer_more_tuple_tuple(x, [True, 1], radix, check = False):
            res = [True, 1]
            temp_bool = True
            temp_num = [True, 1]
            while temp_bool:
                temp_num = plus_one(temp_num, radix, check = False)
                res = integer_multiply(res, temp_num, 
                                       radix, check = False)
                if integer_moreeq_tuple_tuple(temp_num, x, 
                                              radix, check = False):
                    temp_bool = False
        else:
            res = tuple([True, 1])
    return res

def integer_permute(m, n, radix, check = True):
    # input: integer vector, m, n
    #                        where 0 <= n <= m, m > 0
    # output: integer vector, m permute n
    if check:
        res = None
        if integer_check(m, radix) & integer_check(n, radix): 
            if integer_moreeq_tuple_tuple(n, [True, 0], radix, check = False):
                if integer_moreeq_tuple_tuple(m, n, radix, check = False):
                    if integer_more_tuple_tuple(m, [True, 0], radix, check = False):
                        res = True 
    else:
        res = True
    if not res is None:
        if integer_lesseq_tuple_tuple(n, [True, 0], radix, check = False):
            res = tuple([True, 1])
        else:
            res = [True, 1]
            temp_bool = True
            while temp_bool:
                res = integer_multiply(res, m, radix, check = False)
                m = minus_one(m, radix, check = False)
                n = minus_one(n, radix, check = False)
                if integer_lesseq_tuple_tuple(n, [True, 0], radix, check = False):
                    temp_bool = False
    return res

def integer_choose(m, n, radix, check = True):
    # input: integer vector, m, n
    #                        where 0 <= n <= m, m > 0
    # output: integer vector, m choose n
    if check:
        res = None
        if integer_check(m, radix) & integer_check(n, radix): 
            if integer_moreeq_tuple_tuple(n, [True, 0], radix, check = False):
                if integer_moreeq_tuple_tuple(m, n, radix, check = False):
                    if integer_more_tuple_tuple(m, [True, 0], radix, check = False):
                        res = True 
    else:
        res = True
    if not res is None:
        complement_n = integer_minus(m, n, radix, check = False)
        if integer_less_tuple_tuple(complement_n, n, radix, check = False):
            n = complement_n
        if integer_eq_tuple_tuple(n, [True, 0], radix, check = False):
            res = tuple([True, 1])
        else:
            res = integer_multiply([True, 1], m, radix)
            if integer_more_tuple_tuple(n, [True, 1], radix, check = False):
                temp_bool = True
                temp_num = [True, 1]
                while temp_bool:
                    m = minus_one(m, radix, check = False)
                    res = integer_multiply(res, m, radix)
                    n = minus_one(n, radix, check = False)
                    temp_num = plus_one(temp_num, radix, check = False)
                    res = integer_modulo(res, temp_num, 
                                         radix, check = False)["quo"]
                    if integer_lesseq_tuple_tuple(n, [True, 1], radix, check = False):
                        temp_bool = False
    return res

def integer_mod_inverse(x, m, radix, check = True):
    # input: integer vector, x, m
    #                        where x != 0, m !in {-1, 0, 1}, 
    #                              x and m are relatively primes
    # output: integer vector, inverse(x) (mod m)    
    if check:
        res = None
        if integer_check(x, radix) & integer_check(m, radix): 
            if integer_noteq_tuple_tuple(x, [True, 0], radix, check = False): 
                if (integer_less_tuple_tuple(m, [False, 1], radix, check = False) | 
                    integer_more_tuple_tuple(m, [True, 1], radix, check = False)):
                    if integer_is_relatively_primes(x, m, radix, check = False):
                        res = True
    else:
        res = True
    if not res is None:
        temp_num_0_0 = integer_absolute(m, radix, check = False)
        temp_num_0_1 = integer_modulo(x, temp_num_0_0, radix, check = False)["rem"]
        temp_num_1_0 = [True, 1]
        temp_num_1_1 = [True, 0]
        temp_num_2_0 = [True, 0]
        temp_num_2_1 = [True, 1]
        temp_bool = True
        while temp_bool:
            temp_modulo = integer_modulo(temp_num_0_0, temp_num_0_1, radix, check = False)
            if integer_more_tuple_tuple(temp_modulo["rem"], [True, 0], 
                                        radix, check = False):
                temp_num_0_0 = temp_num_0_1
                temp_num_0_1 = temp_modulo["rem"]
                temp_num_1_2 = integer_multiply(temp_num_1_1, 
                                                temp_modulo["quo"], 
                                                radix, check = False)
                temp_num_1_2 = integer_minus(temp_num_1_0, 
                                             temp_num_1_2, 
                                             radix, check = False)
                temp_num_1_0 = temp_num_1_1
                temp_num_1_1 = temp_num_1_2
                temp_num_2_2 = integer_multiply(temp_num_2_1, 
                                                temp_modulo["quo"], 
                                                radix, check = False)
                temp_num_2_2 = integer_minus(temp_num_2_0, 
                                             temp_num_2_2, 
                                             radix, check = False)
                temp_num_2_0 = temp_num_2_1
                temp_num_2_1 = temp_num_2_2
            else:
                temp_bool = False
        res = integer_modulo(temp_num_2_1, m, radix, check = False)["rem"]
    return res

def integer_crt(r, m, radix, check = True):
    # input: vector of integer vector, r, m
    #                          where r is the vector of objective remainders
    #                          where m is the vector of divisors
    #                                & all the elements in m are pairwise relatively primes 
    #                                  (excluding -1, 0, 1)
    # output: integer vector, the integer of Chinese remainder theorem
    if check:
        res = None
        if isinstance(r, list) | isinstance(r, tuple):
            r_len = len(r)
            if isinstance(m, list) | isinstance(m, tuple):
                m_len = len(m)
                if (r_len == m_len) & (r_len > 1):
                    temp_bool_0 = True
                    for n in range(r_len):
                        if integer_check(r[n], radix) & integer_check(m[n], radix): 
                            if (integer_less_tuple_tuple(m[n], [False, 1], 
                                                         radix, check = False) |
                                integer_more_tuple_tuple(m[n], [True, 1], 
                                                         radix, check = False)):
                                for n1 in range(n):
                                    temp_bool_0 = integer_is_relatively_primes(m[n], m[n1], 
                                                                               radix, check = False)
                                    if not temp_bool_0:
                                        break
                            else:
                                temp_bool_0 = False
                        else:
                            temp_bool_0 = False
                        if temp_bool_0 is False:
                            break
                    if temp_bool_0:
                        res = True
    else:
        res = True
    if not res is None:
        r_len = len(r)
        M = [True, 1]
        for n in range(r_len):
            temp_num = integer_absolute(m[n], radix, check = False)
            M = integer_multiply(M, temp_num, radix, check = False)
        res = [True, 0]
        for n in range(r_len):
            temp_num = integer_modulo(M, m[n], 
                                      radix, check = False)["quo"]
            temp_num_1 = integer_mod_inverse(temp_num, m[n], 
                                             radix, check = False)
            temp_num = multiply_modulo(temp_num, temp_num_1, M, 
                                       radix, check = False)
            temp_num = multiply_modulo(temp_num, r[n], M, 
                                       radix, check = False)
            res = plus_modulo(res, temp_num, M, 
                              radix, check = False)
    return res

def multiply_digits(x, y, radix, check = True):
    if check:
        res = None
        if isinstance(x, int) & isinstance(y, int) & isinstance(radix, int):
            if radix > 1:                
                if (x >= 0) & (x < radix) & (y >= 0) & (x < radix):
                    res = True
    else:
        res = True
    if not res is None:
        if (x == 0) | (y == 0):
            res = tuple([0])
        elif x == 1:
            res = tuple([y])
        elif y == 1:
            res = tuple([x])
        else:            
            prod_xy = x*y
            if prod_xy < radix:
                res = tuple([prod_xy])
            else:
                temp_upper = radix
                temp_lower = 0
                temp_diff = temp_upper-temp_lower
                temp_lower_num = 0
                while (temp_diff > 1):
                    temp_mid = int(temp_diff/2)+temp_lower
                    temp_mid_num = radix*temp_mid
                    if prod_xy > temp_mid_num:
                        temp_lower = temp_mid
                        temp_lower_num = temp_mid_num
                        temp_diff = temp_upper-temp_lower
                    elif prod_xy < temp_mid_num:
                        temp_upper = temp_mid
                        temp_diff = temp_upper-temp_lower
                    else:
                        temp_lower = temp_mid
                        temp_lower_num = temp_mid_num
                        break
                res = (prod_xy-temp_lower_num, temp_lower)
    return res    

def floor_half_value(x, radix, check = True):
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:
        res = [x[0]]
        x_len = len(x)
        for n in range(x_len-1):
            res.append(0)
        temp_num_0 = radix/2
        temp_num_1 = int(temp_num_0)
        if temp_num_0 > temp_num_1:
            half_radix = temp_num_1
            odd_radix = True
        else:
            half_radix = temp_num_1
            odd_radix = False   
        if_remainder = False
        for n in range(x_len-1, 0, -1):
            temp_num_0 = x[n]/2
            temp_num_1 = int(temp_num_0)
            if temp_num_0 > temp_num_1:
                if if_remainder:
                    if odd_radix:
                        res[n] = half_radix+temp_num_1+1
                        if_remainder = False
                    else:
                        res[n] = half_radix+temp_num_1
                else:
                    res[n] = temp_num_1
                    if_remainder = True
            else:
                if if_remainder:
                    if odd_radix:
                        res[n] = half_radix+temp_num_1                       
                    else:
                        res[n] = half_radix+temp_num_1
                        if_remainder = False
                else:
                    res[n] = temp_num_1
        res = {"half": trim_initial_zeros(res, radix, check = False), 
               "if_rem": if_remainder}
    return res

def floor_sqrt_value(x, radix, check = True):
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:
        x_sign = x[0]
        x = integer_absolute(x, radix, check = False)
        if integer_more_tuple_tuple(x, 
                                    integer_int_2_tuple(8, radix, check = False), 
                                    radix, check = False):            
            temp_upper_vec = floor_half_value(x, radix, check = False)["half"]
            temp_lower_vec = [True, 1]
            temp_lower_num_vec = [True, 1]
            temp_diff_vec = integer_minus(temp_upper_vec, 
                                          temp_lower_vec, 
                                          radix, check = False)   
            temp_bool_0 = True
            temp_bool_1 = True
            while temp_bool_0:
                temp_half_vec = floor_half_value(temp_diff_vec, 
                                                 radix, check = False)["half"]             
                temp_mid_vec = integer_plus(temp_lower_vec, 
                                            temp_half_vec, 
                                            radix, check = False)                
                temp_mid_num_vec = integer_multiply(temp_mid_vec, 
                                                    temp_mid_vec, radix, 
                                                    check = False)
                if integer_less_tuple_tuple(x, temp_mid_num_vec, 
                                            radix, check = False):
                    temp_upper_vec = temp_mid_vec
                elif integer_more_tuple_tuple(x, temp_mid_num_vec, 
                                              radix, check = False):
                    temp_lower_vec = temp_mid_vec
                    temp_lower_num_vec = temp_mid_num_vec
                else:
                    temp_lower_vec = temp_mid_vec
                    temp_lower_num_vec = temp_mid_num_vec
                    temp_bool_1 = False
                if temp_bool_1:
                    temp_diff_vec = integer_minus(temp_upper_vec, 
                                                  temp_lower_vec, 
                                                  radix, check = False)
                    if (len(temp_diff_vec) == 2) & (temp_diff_vec[1] <= 1):
                        temp_bool_0 = False
                else:
                    temp_bool_0 = False
            temp_vec_0 = temp_lower_vec
            temp_vec_1 = temp_lower_num_vec
        elif integer_more_tuple_tuple(x, 
                                      integer_int_2_tuple(3, radix, check = False), 
                                      radix, check = False):
            temp_vec_0 = integer_int_2_tuple(2, radix, check = False)
            temp_vec_1 = integer_int_2_tuple(4, radix, check = False)
        elif integer_more_tuple_tuple(x, 
                                      [True, 0], 
                                      radix, check = False):
            temp_vec_0 = tuple([True, 1])
            temp_vec_1 = tuple([True, 1])
        else:
            temp_vec_0 = tuple([True, 0])
            temp_vec_1 = tuple([True, 0])
            x_sign = True
        res = {"root": temp_vec_0, 
               "root_rem": integer_minus(x, temp_vec_1, radix, check = False), 
               "real": x_sign}
    return res

def plus_one(x, radix, check = True):   
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:        
        x_len = len(x)
        res = x[0]
        temp_vec_0 = []
        for n in range(1, x_len):
            temp_vec_0.append(x[n])
        if res:
            temp_vec_0.append(0)
            temp_vec_0[0] += 1
            for n in range(x_len-1):
                if temp_vec_0[n] >= radix:
                    temp_vec_0[n] -= radix
                    temp_vec_0[n+1] += 1
                else:
                    break
            if temp_vec_0[-1] < 1:
                del(temp_vec_0[-1])
        else:
            if x_len > 2:
                temp_vec_0[0] -= 1
                for n in range(x_len-1):
                    if temp_vec_0[n] < 0:
                        temp_vec_0[n] += radix
                        temp_vec_0[n+1] -= 1
                    else:
                        break
                if temp_vec_0[-1] < 1:
                    del(temp_vec_0[-1])
            else:
                if temp_vec_0[0] > 0:
                    temp_vec_0[0] -= 1
                    if temp_vec_0[0] < 1:
                        res = True
                else:
                    res = True
                    temp_vec_0[0] += 1
        res = [res]
        res.extend(temp_vec_0)
        res = tuple(res)
    return res

def minus_one(x, radix, check = True):   
    if check:
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:        
        x_len = len(x)
        res = x[0]
        temp_vec_0 = []
        for n in range(1, x_len):
            temp_vec_0.append(x[n])
        if res:
            if x_len > 2:
                temp_vec_0[0] -= 1
                for n in range(x_len-1):
                    if temp_vec_0[n] < 0:
                        temp_vec_0[n] += radix
                        temp_vec_0[n+1] -= 1
                    else:
                        break
                if temp_vec_0[-1] < 1:
                    del(temp_vec_0[-1])
            else:
                if temp_vec_0[0] > 0:
                    temp_vec_0[0] -= 1
                else:
                    res = False
                    temp_vec_0[0] += 1
        else:
            temp_vec_0.append(0)
            temp_vec_0[0] += 1
            for n in range(x_len-1):
                if temp_vec_0[n] >= radix:
                    temp_vec_0[n] -= radix
                    temp_vec_0[n+1] += 1
                else:
                    break
            if temp_vec_0[-1] < 1:
                del(temp_vec_0[-1])   
        res = [res]
        res.extend(temp_vec_0)
        res = tuple(res)
    return res

def plus_modulo(x, y, m, radix, check = True):
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix) & integer_check(m, radix): 
            if len(m) > 2:
                res = True
            elif m[1] > 1:
                res = True
    else:
        res = True
    if not res is None:     
        temp_vec_0 = integer_plus(x, y, radix, check = False)
        res = integer_modulo(temp_vec_0, m, 
                             radix, check = False)["rem"]
    return res

def multiply_modulo(x, y, m, radix, check = True):
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix) & integer_check(m, radix): 
            if len(m) > 2:
                res = True
            elif m[1] > 1:
                res = True
    else:
        res = True
    if not res is None:     
        temp_vec_0 = integer_multiply(x, y, radix, check = False)
        res = integer_modulo(temp_vec_0, m, 
                             radix, check = False)["rem"]
    return res   

def power_modulo(x, y, m, radix, check = True):
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix) & integer_check(m, radix): 
            if integer_more_tuple_tuple(y, [True, 0], radix, check = False):
                if len(m) > 2:
                    res = True
                elif m[1] > 1:
                    res = True
    else:
        res = True
    if not res is None: 
        res = [True, 1]
        temp_vec_y = integer_tuple_change_radix(y, radix, 2, check = False)
        y_len = len(temp_vec_y)
        temp_vec_0 = integer_modulo(x, m, 
                                    radix, check = False)["rem"]
        for n in range(1, y_len):
            if temp_vec_y[n] > 0:
                res = integer_multiply(res, temp_vec_0, radix, 
                                       check = False)
                res = integer_modulo(res, m, 
                                     radix, check = False)["rem"]
            if n < y_len-1:
                temp_vec_0 = integer_multiply(temp_vec_0, temp_vec_0, radix, 
                                              check = False)
                temp_vec_0 = integer_modulo(temp_vec_0, m, 
                                            radix, check = False)["rem"]
    return res   

def trim_initial_zeros(x, radix, check = True):   
    if check:
        res = None
        if isinstance(x, list) | isinstance(x, tuple):
            if isinstance(radix, int):
                if radix > 1:
                    x_len = len(x)
                    if x_len > 1:
                        if isinstance(x[0], bool):
                            res = True
                            for n in range(1, x_len):
                                temp_bool = check_less_nonneg_int_int(x[n], radix)
                                if temp_bool is None:
                                    res = None
                                    break
                                elif temp_bool is False:
                                    res = None
                                    break
    else:
        res = True
    if not res is None:    
        res = []
        x_len = len(x)   
        x_len_1 = 2
        if x_len > 2:            
            for n in range(x_len-1, 1, -1):
                if x[n] > 0:
                    x_len_1 = n+1
                    break
        if x_len_1 == 2:
            if x[1] == 0:
                res = [True, 0]
            else:
                res = [x[0], x[1]]
        else:
            for n in range(x_len_1):
                res.append(x[n])
        res = tuple(res)
    return res



# conversion functions

def integer_int_2_tuple(x, radix, check = True):
    # input: int, x
    # output: integer vector, x
    if check:
        res = None
        if isinstance(x, int) | isinstance(radix, int):
            if radix > 1:
                res = True
    else:
        res = True
    if not res is None:
        res = [x >= 0]
        x = abs(x)        
        if x > 0:
            temp_bool_0 = True
            temp_bool_1 = True
            temp_num_0 = 0
            temp_num_1 = 1
            temp_num_list_0 = []
            while temp_bool_0:
                if x > temp_num_1:
                    temp_num_0 += 1
                    temp_num_list_0.append(temp_num_1)
                    temp_num_1 *= radix
                elif x == temp_num_1:
                    temp_vec_0 = []
                    for n in range(temp_num_0):
                        temp_vec_0.append(0)
                    temp_vec_0.append(1)
                    temp_bool_0 = False
                    temp_bool_1 = False
                else:
                    temp_bool_0 = False            
            if temp_bool_1:
                temp_vec_0 = []
                for n in range(temp_num_0):
                    temp_vec_0.append(0)                
                for n in range(temp_num_0-1, -1, -1):
                    temp_upper = radix
                    temp_lower = 0
                    temp_diff = temp_upper-temp_lower
                    temp_lower_num = 0
                    while (temp_diff > 1):
                        temp_mid = int(temp_diff/2)+temp_lower
                        temp_mid_num = temp_num_list_0[n]*temp_mid
                        if x > temp_mid_num:
                            temp_lower = temp_mid
                            temp_lower_num = temp_mid_num
                            temp_diff = temp_upper-temp_lower
                        elif x < temp_mid_num:
                            temp_upper = temp_mid
                            temp_diff = temp_upper-temp_lower
                        else:
                            temp_lower = temp_mid
                            temp_lower_num = temp_mid_num
                            temp_bool_1 = False
                            break
                    temp_vec_0[n] = temp_lower
                    if temp_bool_1:
                        x -= temp_lower_num
                    else:
                        break
        else:
            temp_vec_0 = [0]
        res.extend(temp_vec_0)    
        res = tuple(res)
    return res

def integer_tuple_2_int(x, radix, check = True):
    # input: integer vector, x
    # output: int, x
    if check:        
        if integer_check(x, radix): 
            res = True
        else:
            res = None
    else:
        res = True
    if not res is None:
        res = 0
        x_len = len(x)
        temp_num_0 = 1
        for n in range(1, x_len):
            res += temp_num_0*x[n]
            temp_num_0 *= radix
        if x[0] is False:
            res *= -1
    return res

def integer_tuple_change_radix(x, origin_radix, new_radix, check = True):
    # input: integer vector, x with radix origin_radix
    # output: integer vector, x with radix new_radix
    if check:
        res = None
        if integer_check(x, origin_radix): 
            if isinstance(new_radix, int):
                if new_radix > 1:
                    res = True        
    else:
        res = True
    if not res is None:        
        res = [x[0]]
        x_len = len(x)
        temp_vec_0 = [0]
        temp_vec_2 = [1]
        for n0 in range(1, x_len):
            temp_vec_1 = [0]
            for n1 in range(x[n0]):
                temp_vec_1 = iter_add_pos(temp_vec_1, temp_vec_2, 
                                          new_radix)
            temp_vec_0 = iter_add_pos(temp_vec_1, temp_vec_0, 
                                      new_radix)
            if n0 < x_len-1:
                for n1 in range(x[n0], origin_radix):
                    temp_vec_1 = iter_add_pos(temp_vec_1, temp_vec_2, 
                                              new_radix)
                temp_vec_2 = temp_vec_1   
        res.extend(temp_vec_0)
        res = tuple(res)
    return res

def iter_add_pos(x, y, radix):
    x_len = len(x)
    if (x_len == 1) & (x[0] == 0):
        res = tuple(y)
    else:
        y_len = len(y)
        res = []
        for n in range(x_len):
            res.append(x[n])
        res.append(0)
        for n in range(y_len):
            temp_num = res[n]-radix+y[n]
            if temp_num < 0:
                temp_num += radix
                res[n] = temp_num
            else:
                res[n] = temp_num
                res[n+1] += 1
        for n in range(y_len, x_len):
            if res[n] >= radix:
                res[n] -= radix
                res[n+1] += 1
            else:
                break
        if res[-1] == 0:
            del(res[-1])
        res = tuple(res)
    return res



# prime-related, composite-related functions

def integer_is_prime(x, radix, check = True):
    # input: integer vector, x
    # output: bool, if x is prime, True
    #               else if x is composite, False
    #               else if x is 0 or 1 or -1, None
    if check:
        res = None
        if integer_check(x, radix): 
            res = True        
    else:
        res = True
    if not res is None:  
        x = integer_absolute(x, radix, check = False)
        if integer_more_tuple_tuple(x, 
                                    integer_int_2_tuple(16, radix, check = False), 
                                    radix, check = False):
            res = True
            sqrt_num = floor_sqrt_value(x, radix, check = False)["root"]
            half_sqrt_num = floor_half_value(sqrt_num, radix, check = False)["half"]
            temp_vec_0 = [True, 1]
            temp_bool_0 = True
            temp_prime_list = [0, [], [], []]      
            while temp_bool_0:
                temp_vec_0 = plus_one(temp_vec_0, radix, check = False)
                if not True in temp_prime_list[3]:
                    if integer_lesseq_tuple_tuple(temp_vec_0, 
                                                  half_sqrt_num, 
                                                  radix, check = False):
                        temp_prime_list[0] += 1
                        temp_prime_list[1].append(temp_vec_0)
                        temp_prime_list[2].append([0])
                        temp_prime_list[3].append(True)     
                    temp_modulo_rem = integer_modulo(x, temp_vec_0, 
                                                     radix, check = False)["rem"]
                    if (len(temp_modulo_rem) == 2) & (temp_modulo_rem[1] < 1):
                        res = False
                if res:
                    if integer_moreeq_tuple_tuple(temp_vec_0, 
                                                  sqrt_num, 
                                                  radix, check = False):
                        temp_bool_0 = False
                    else:
                        temp_prime_list = factorization_next(temp_prime_list, radix)
                else:
                    temp_bool_0 = False
        else:
            temp_num = integer_tuple_2_int(x, radix, check = False)
            if temp_num < 2:
                res = None
            elif temp_num in (4, 6, 8, 9, 10, 12, 14, 15, 16):
                res = False
            else:
                res = True
    return res

def integer_factorization(x, radix, check = True):
    # input: integer vector, x
    # output: prime-product of x
    if check:
        res = None
        if integer_check(x, radix): 
            res = True        
    else:
        res = True
    if not res is None:  
        x = integer_absolute(x, radix, check = False)
        if integer_more_tuple_tuple(x, 
                                    integer_int_2_tuple(16, radix, check = False), 
                                    radix, check = False):
            sqrt_num = floor_sqrt_value(x, radix, check = False)["root"]
            half_sqrt_num = floor_half_value(sqrt_num, radix, check = False)["half"]
            temp_vec_0 = [True, 1]
            temp_bool_0 = True
            temp_factor_list = [[], []]
            temp_factror_num = -1
            temp_prime_list = [0, [], [], []]            
            while temp_bool_0:
                temp_vec_0 = plus_one(temp_vec_0, radix, check = False)
                if not True in temp_prime_list[3]:
                    if integer_lesseq_tuple_tuple(temp_vec_0, 
                                                  half_sqrt_num, 
                                                  radix, check = False):
                        temp_prime_list[0] += 1
                        temp_prime_list[1].append(temp_vec_0)
                        temp_prime_list[2].append([0])
                        temp_prime_list[3].append(True)                        
                    temp_bool_1 = True
                    temp_bool_2 = True
                    while temp_bool_1:
                        if integer_more_tuple_tuple(x, temp_vec_0, 
                                                    radix, check = False):
                            temp_modulo_res = integer_modulo(x, temp_vec_0, 
                                                             radix, check = False)
                            temp_modulo_rem = temp_modulo_res["rem"]
                            if len(temp_modulo_rem) == 2:
                                if temp_modulo_rem[1] < 1:
                                    if temp_bool_2:
                                        temp_factror_num += 1
                                        temp_factor_list[0].append(temp_vec_0)
                                        temp_factor_list[1].append(tuple([True, 1]))
                                        x = temp_modulo_res["quo"]
                                        temp_bool_2 = False
                                    else:
                                        temp_factor_list[1][temp_factror_num] = plus_one(temp_factor_list[1][temp_factror_num], 
                                                                                         radix, check = False)
                                        x = temp_modulo_res["quo"]
                                else:
                                    temp_bool_1 = False
                            else:
                                temp_bool_1 = False
                        else:
                            if temp_bool_2:
                                temp_factor_list[0].append(temp_vec_0)
                                temp_factor_list[1].append(tuple([True, 1]))
                                x = [True, 1]
                            else:
                                temp_factor_list[1][temp_factror_num] = plus_one(temp_factor_list[1][temp_factror_num], 
                                                                                 radix, check = False)
                                x = [True, 1]
                            temp_bool_1 = False
                if (len(x) == 2) & (x[1] < 2):
                    temp_bool_0 = False
                elif integer_moreeq_tuple_tuple(temp_vec_0, 
                                                sqrt_num, 
                                                radix, check = False):
                    temp_factor_list[0].append(tuple(x))
                    temp_factor_list[1].append(tuple([True, 1]))
                    temp_bool_0 = False
                else:                    
                    temp_prime_list = factorization_next(temp_prime_list, radix)  
            res = {"prime": tuple(temp_factor_list[0]), 
                   "power": tuple(temp_factor_list[1])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(16, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False)]), 
                   "power": tuple([integer_int_2_tuple(4, radix, check = False)])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(15, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(3, radix, check = False), 
                                   integer_int_2_tuple(5, radix, check = False)]), 
                   "power": tuple([tuple([True, 1]), tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(14, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False), 
                                   integer_int_2_tuple(7, radix, check = False)]), 
                   "power": tuple([tuple([True, 1]), tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(13, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(13, radix, check = False)]), 
                   "power": tuple([tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(12, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False), 
                          integer_int_2_tuple(3, radix, check = False)]), 
                   "power": tuple([integer_int_2_tuple(2, radix, check = False), tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                      integer_int_2_tuple(11, radix, check = False), 
                                      radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(11, radix, check = False)]), 
                   "power": tuple([tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                      integer_int_2_tuple(10, radix, check = False), 
                                      radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False), 
                                   integer_int_2_tuple(5, radix, check = False)]), 
                   "power": tuple([tuple([True, 1]), tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(9, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(3, radix, check = False)]), 
                   "power": tuple([integer_int_2_tuple(2, radix, check = False)])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(8, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False)]), 
                   "power": tuple([integer_int_2_tuple(3, radix, check = False)])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(7, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(7, radix, check = False)]), 
                   "power": tuple([tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(6, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False), 
                                   integer_int_2_tuple(3, radix, check = False)]), 
                   "power": tuple([tuple([True, 1]), tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(5, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(5, radix, check = False)]), 
                   "power": tuple([tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(4, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False)]), 
                   "power": tuple([integer_int_2_tuple(2, radix, check = False)])}
        elif integer_moreeq_tuple_tuple(x, 
                                      integer_int_2_tuple(3, radix, check = False), 
                                      radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(3, radix, check = False)]), 
                   "power": tuple([tuple([True, 1])])}
        elif integer_moreeq_tuple_tuple(x, 
                                        integer_int_2_tuple(2, radix, check = False), 
                                        radix, check = False):
            res = {"prime": tuple([integer_int_2_tuple(2, radix, check = False)]), 
                   "power": tuple([tuple([True, 1])])}
        else:
            res = None
    return res

def integer_divisors(x, radix, check = True):
    # input: integer vector, x
    # output: divisors of x
    if check:
        res = None
        if integer_check(x, radix): 
            res = True        
    else:
        res = True
    if not res is None:  
        x = integer_absolute(x, radix, check = False)
        if integer_more_tuple_tuple(x, [True, 1], radix, check = False):
            temp_factor_dict = integer_factorization(x, radix, check = False)
            temp_len = len(temp_factor_dict["prime"])
            temp_factor_power = []
            for n in range(temp_len):
                temp_num_0 = [True, 0]
                temp_num_1 = (True, 1)
                temp_vec_0 = [temp_num_1]
                temp_bool_0 = True
                while temp_bool_0:
                    temp_num_0 = plus_one(temp_num_0, radix, check = False)
                    temp_num_1 = integer_multiply(temp_num_1, 
                                                  temp_factor_dict["prime"][n], 
                                                  radix, check = False)
                    temp_vec_0.append(temp_num_1)
                    if integer_moreeq_tuple_tuple(temp_num_0, 
                                                  temp_factor_dict["power"][n], 
                                                  radix, check = False):
                        temp_bool_0 = False
                temp_factor_power.append(tuple(temp_vec_0))
            res = []
            temp_cur_0 = 0
            temp_index_vec = []
            temp_index_max_vec = []
            for n in range(temp_len):
                temp_index_vec.append(0)
                temp_index_max_vec.append(integer_tuple_2_int(temp_factor_dict["power"][n], 
                                                              radix, check = False))
            temp_bool_0 = True
            while temp_bool_0:
                temp_num_0 = (True, 1)
                for n in range(temp_len):
                    temp_num_0 = integer_multiply(temp_num_0, 
                                                  temp_factor_power[n][temp_index_vec[n]], 
                                                  radix, check = False)
                temp_cur_1 = temp_cur_0
                for n in range(temp_cur_0):
                    if integer_less_tuple_tuple(temp_num_0, 
                                                res[n], 
                                                radix, check = False):
                        temp_cur_1 = n
                        break
                res.insert(temp_cur_1, temp_num_0)
                temp_cur_0 += 1
                temp_index_vec[0] += 1
                for n in range(temp_len-1):
                    if temp_index_vec[n] > temp_index_max_vec[n]:
                        temp_index_vec[n] = 0
                        temp_index_vec[n+1] += 1
                    else:
                        break
                if temp_index_vec[temp_len-1] > temp_index_max_vec[temp_len-1]:
                    temp_bool_0 = False
            res = tuple(res)
        elif integer_moreeq_tuple_tuple(x, [True, 1], radix, check = False):
            res = tuple([tuple([True, 1])])
        else:
            res = None
    return res

def integer_Euler_phi(x, radix, check = True):
    # input: integer vector, x
    # output: Euler's phi of x
    if check:
        res = None
        if integer_check(x, radix): 
            res = True        
    else:
        res = True
    if not res is None:  
        x = integer_absolute(x, radix, check = False)
        if integer_more_tuple_tuple(x, [True, 1], radix, check = False):
            temp_fac = integer_factorization(x, radix, check = False)
            res = [True, 1]
            fac_len = len(temp_fac["prime"])
            for n in range(fac_len):
                temp_num = integer_power(temp_fac["prime"][n], 
                                         minus_one(temp_fac["power"][n], radix, check = False), 
                                         radix, check = False)
                temp_num = integer_multiply(temp_num, 
                                            minus_one(temp_fac["prime"][n], radix, check = False), 
                                            radix, check = False)
                res = integer_multiply(temp_num, res, 
                                       radix, check = False)
        elif integer_more_tuple_tuple(x, [True, 0], radix, check = False):
            res = tuple([True, 1])
        else:
            res = None
    return res

def integer_gcd(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: greatest common divisor of x, y
    #         where both of x and y are not 0
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True        
    else:
        res = True
    if not res is None:
        x = integer_absolute(x, radix, check = False)
        y = integer_absolute(y, radix, check = False)
        if (len(x) == 2) & (x[1] < 1) & (len(y) == 2) & (y[1] < 1):
            res = None
        elif (len(x) == 2) & (x[1] < 1):
            res = tuple(y)
        elif (len(y) == 2) & (y[1] < 1):
            res = tuple(x)
        else:
            if (len(x) == 2) & (x[1] < 2):
                res = tuple([True, 1])
            elif (len(y) == 2) & (y[1] < 2):
                res = tuple([True, 1])
            else:
                if integer_eq_tuple_tuple(x, y, radix, 
                                          check = False):
                    res = tuple(x)
                    temp_bool_0 = False                
                elif integer_more_tuple_tuple(x, y, radix, 
                                              check = False):
                    x_1 = x
                    y_1 = y
                    temp_bool_0 = True
                else:
                    x_1 = y
                    y_1 = x
                    temp_bool_0 = True                
                while temp_bool_0:
                    r_1 = integer_modulo(x_1, y_1, radix, 
                                         check = False)["rem"]                    
                    if (len(r_1) == 2) & (r_1[1] < 1):
                        res = y_1
                        temp_bool_0 = False
                    elif (len(r_1) == 2) & (r_1[1] < 2):
                        res = tuple([True, 1])
                        temp_bool_0 = False
                    else:
                        x_1 = y_1
                        y_1 = r_1
    return res

def integer_is_relatively_primes(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: if x and y are relatively primes, True
    #         if x and y are not relatively primes, False
    #         if either of x and y is 0, None
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True        
    else:
        res = True
    if not res is None:
        if ((len(x) == 2) & (x[1] < 1)) | ((len(y) == 2) & (y[1] < 1)):
            res = None
        else:
            temp_gcd = integer_gcd(x, y, radix, check = False)
            res = (len(temp_gcd) == 2) & (temp_gcd[1] < 2)
    return res

def integer_lcm(x, y, radix, check = True):
    # input: integer vector, x, y
    # output: least common multiple of x, y
    #         where either of x and y is not 0
    if check:
        res = None
        if integer_check(x, radix) & integer_check(y, radix): 
            res = True        
    else:
        res = True
    if not res is None:
        x = integer_absolute(x, radix, check = False)
        y = integer_absolute(y, radix, check = False)
        if (len(x) == 2) & (x[1] < 1):
            res = None
        elif (len(y) == 2) & (y[1] < 1):
            res = None
        else:
            if (len(x) == 2) & (x[1] < 2):
                res = tuple(y)
            elif (len(y) == 2) & (y[1] < 2):
                res = tuple(x)
            else:
                temp_gcd = integer_gcd(x, y, radix, check = False)
                res = integer_multiply(x, y, radix, check = False)
                res = integer_modulo(res, temp_gcd, radix, 
                                     check = False)["quo"]
    return res

def integer_Carmichael_lambda(x, radix, check = True):
    # input: integer vector, x
    # output: Carmichael's lambda of x
    if check:
        res = None
        if integer_check(x, radix): 
            res = True        
    else:
        res = True
    if not res is None: 
        x = integer_absolute(x, radix, check = False)
        if (len(x) == 2) & (x[1] < 2):
            res = None
        else:
            fact_dict = integer_factorization(x, radix, check = False)
            temp_len = len(fact_dict["prime"])
            temp_2_vec = integer_int_2_tuple(2, radix, check = False)
            if integer_eq_tuple_tuple(fact_dict["prime"][0], 
                                      temp_2_vec, 
                                      radix, check = False):                
                if integer_more_tuple_tuple(fact_dict["power"][0], 
                                            temp_2_vec, 
                                            radix, check = False):
                    res = integer_power(temp_2_vec, 
                                        integer_minus(fact_dict["power"][0], temp_2_vec, radix, check = False), 
                                        radix, check = False)
                else:
                    res = integer_power(temp_2_vec, 
                                        minus_one(fact_dict["power"][0], radix, check = False), 
                                        radix, check = False)
            else:
                res = integer_power(fact_dict["prime"][0], 
                                    minus_one(fact_dict["power"][0], radix, check = False), 
                                    radix, check = False)
                res = integer_multiply(res, 
                                       minus_one(fact_dict["prime"][0], radix, check = False), 
                                       radix, check = False)
            if temp_len > 1:
                for n in range(1, temp_len):
                    temp_num = integer_power(fact_dict["prime"][n], 
                                             minus_one(fact_dict["power"][n], radix, check = False), 
                                             radix, check = False)
                    temp_num = integer_multiply(temp_num, 
                                                minus_one(fact_dict["prime"][n], radix, check = False), 
                                                radix, check = False)
                    res = integer_lcm(res, temp_num, radix, check = False)
    return res

def factorization_next(fac_list, radix):
    for n in range(fac_list[0]):
        temp_num_0 = len(fac_list[2][n])
        if temp_num_0 < 2:
            if fac_list[2][n][0] < 1:
                temp_vec = []
                temp_num_1 = len(fac_list[1][n])-1
                for n1 in range(1, (temp_num_1+1)):
                    temp_vec.append(fac_list[1][n][n1])
                temp_vec[0] -= 1
                for n1 in range(temp_num_1):
                    if temp_vec[n1] < 0:
                        temp_vec[n1] += radix
                        temp_vec[n1+1] -= 1
                    else:
                        break
                if temp_vec[-1] < 1:
                    del(temp_vec[-1])
                fac_list[2][n] = temp_vec
                fac_list[3][n] = False
            else:
                fac_list[2][n][0] -= 1
                if fac_list[2][n][0] < 1:
                    fac_list[3][n] = True
        else:
            fac_list[2][n][0] -= 1
            for n1 in range(temp_num_0):
                if fac_list[2][n][n1] < 0:
                    fac_list[2][n][n1] += radix
                    fac_list[2][n][n1+1] -= 1
                else:
                    break
            if fac_list[2][n][-1] < 1:
                del(fac_list[2][n][-1])
    return fac_list



# modular sequence converter

def integer_am_seq_complete_root_condition(c, a, m, radix):
    # condition checker of arithmetic-modulo sequence
    # input: integer vector, c, a, m
    # output: dictionary of encode, decode, for each including c, a, m
    #         where in encode, convert = c (origin + a) mod m
    #         where in decode, origin = c (convert + a) mod m    
    res = None
    if integer_check(c, radix) & integer_check(a, radix) & integer_check(m, radix): 
        m = integer_absolute(m, radix, check = False)
        if integer_more_tuple_tuple(m, [True, 1], radix, 
                                    check = False):
            c = integer_modulo(c, m, radix, check = False)["rem"]    
            if integer_is_relatively_primes(c, m, radix, check = False):
                inv_c = integer_mod_inverse(c, m, radix, check = False)
                a = integer_modulo(a, m, radix, check = False)["rem"]
                inv_a = integer_multiply(c, a, radix, check = False)
                inv_a = integer_inverse(inv_a, radix, check = False)
                inv_a = integer_modulo(inv_a, m, radix, check = False)["rem"]
                res = {"encode": {"type": "am_seq", 
                                  "c": c, 
                                  "a": a, 
                                  "m": m}, 
                       "decode": {"type": "am_seq", 
                                  "c": inv_c, 
                                  "a": inv_a, 
                                  "m": m}}
    return res

def integer_am_seq_convert(x, am_seq_dict, radix):
    # converter of arithmetic-modulo sequence
    # input: integer vector, x
    #        condition dictionary, am_seq_dict
    #                              which is from encode or decode in integer_am_seq_complete_root_condition    
    # output: value of AM sequence with index x
    res = False
    if integer_check(x, radix) & isinstance(am_seq_dict, dict):
        temp_str_list = am_seq_dict.keys()
        if "type" in temp_str_list :
            if am_seq_dict["type"] == "am_seq":
                res = True
    if res:
        res = integer_plus(x, am_seq_dict["a"], radix, check = False)
        res = integer_multiply(res, am_seq_dict["c"], radix, check = False)
        res = integer_modulo(res, am_seq_dict["m"], radix, check = False)["rem"]
    else:
        res = None
    return res

def integer_t1gm_seq_primitive_root_condition(c, a, p, p_power, radix, double = False):
    # condition checker of type-1 geometric-modulo sequence
    # input: integer vector, c, a, p, p_power
    #                              where if double == True, m = 2 p^(p_power), 
    #                                    if double == False, m = p^(p_power) 
    # output: dictionary including c, a, m 
    #                              where a is the primitive root of the GM sequence modulo m
    res = True
    if isinstance(double, bool):
        if (integer_check(c, radix) & integer_check(a, radix) & 
            integer_check(p, radix) & integer_check(p_power, radix)): 
            if (integer_more_tuple_tuple(p_power, [True, 0], radix, check = False) &
                integer_noteq_tuple_tuple(c, [True, 0], radix, check = False) & 
                integer_noteq_tuple_tuple(a, [True, 0], radix, check = False)):
                p = integer_absolute(p, radix, check = False)
                temp_2_vec = integer_int_2_tuple(2, radix, check = False)
                if integer_eq_tuple_tuple(p, temp_2_vec, 
                                          radix, check = False):
                    if (double):
                        if (integer_eq_tuple_tuple(p_power, [True, 1], radix, check = False)):
                            m = integer_int_2_tuple(4, radix, check = False)
                            factor_vec = tuple([tuple(temp_2_vec)])
                            factor_power_vec = tuple([tuple(temp_2_vec)])
                            phi_m = tuple(temp_2_vec)
                        else:
                            res = False
                    else:
                        if (integer_eq_tuple_tuple(p_power, [True, 1], radix, check = False)):
                            m = tuple(temp_2_vec)
                            factor_vec = tuple([tuple(temp_2_vec)])
                            factor_power_vec = tuple([tuple([True, 1])])
                            phi_m = tuple([True, 1])
                        elif (integer_eq_tuple_tuple(p_power, temp_2_vec, radix, check = False)):
                            m = integer_int_2_tuple(4, radix, check = False)
                            factor_vec = tuple([tuple(temp_2_vec)])
                            factor_power_vec = tuple([tuple(temp_2_vec)])
                            phi_m = tuple(temp_2_vec)
                        else:
                            res = False
                    if res:
                        c = integer_modulo(c, m, radix, check = False)["rem"]
                        temp_c = integer_tuple_2_int(c, radix, check = False)
                        if (temp_c != 1) & (temp_c != 3):
                            res = False
                elif integer_more_tuple_tuple(p, temp_2_vec, 
                                              radix, check = False):
                    if integer_is_prime(p, radix, check = False):
                        if (integer_is_relatively_primes(c, p, radix, check = False) & 
                            integer_is_relatively_primes(a, p, radix, check = False)):
                            if double:
                                if (integer_is_even(c, radix, check = False) | 
                                    integer_is_even(a, radix, check = False)):
                                    res = False
                                else:
                                    phi_m = integer_power(p, 
                                                          minus_one(p_power, radix, check = False), 
                                                          radix, check = False)
                                    m = integer_multiply(phi_m, p, radix, check = False)
                                    phi_m = integer_multiply(phi_m, 
                                                             minus_one(p, radix, check = False), 
                                                             radix, check = False)
                                    m = integer_multiply(m, temp_2_vec, radix, check = False)
                                    factor_vec = tuple([tuple(temp_2_vec), tuple(p)])
                                    factor_power_vec = tuple([tuple([True, 1]), tuple(p_power)])
                                    c = integer_modulo(c, m, radix, check = False)["rem"]
                                    a = integer_modulo(a, m, radix, check = False)["rem"]
                            else:
                                phi_m = integer_power(p, 
                                                      minus_one(p_power, radix, check = False), 
                                                      radix, check = False)
                                m = integer_multiply(phi_m, p, radix, check = False)
                                phi_m = integer_multiply(phi_m, 
                                                         minus_one(p, radix, check = False), 
                                                         radix, check = False)
                                factor_vec = tuple([tuple(p)])
                                factor_power_vec = tuple([tuple(p_power)])
                                c = integer_modulo(c, m, radix, check = False)["rem"]
                                a = integer_modulo(a, m, radix, check = False)["rem"]
                        else:
                            res = False
                    else:
                        res = False
                else:
                    res = False
            else:
                res = False              
        else:
            res = False           
    else:
        res = False           
    if res:
        if integer_more_tuple_tuple(phi_m, [True, 1], 
                                    radix, check = False):            
            half_phi_m = floor_half_value(phi_m, radix, check = False)["half"]
            m_minus_one = minus_one(m, radix, check = False)
            temp_bool = True
            temp_vec_0 = [True, 0]
            temp_vec_1 = [True, 1]
            while temp_bool:
                temp_vec_0 = plus_one(temp_vec_0, radix, check = False)
                temp_vec_1 = multiply_modulo(temp_vec_1, a, m, 
                                             radix, check = False)
                if (len(temp_vec_1) == 2) & (temp_vec_1[1] < 2):
                    res = False
                    temp_bool = False
                elif integer_less_tuple_tuple(temp_vec_0, half_phi_m, 
                                              radix, check = False):
                    if integer_eq_tuple_tuple(temp_vec_1, m_minus_one, 
                                              radix, check = False):
                        res = False
                        temp_bool = False
                else:
                    temp_bool = False
    if res:        
        phi_phi_m = integer_Euler_phi(phi_m, radix, check = False)
        res = {"type": "t1gm_seq_primitive_root", 
               "c": c, 
               "a": a, 
               "m": m, 
               "m_prime": factor_vec, 
               "m_power": factor_power_vec, 
               "phi_m": phi_m, 
               "phi_phi_m": phi_phi_m}
    else:
        res = None
    return res

def integer_t1gm_seq_convert(x, t1pm_seq_pr_dict, radix):
    # converter of type-1 geometric-modulo sequence
    # input: integer vector, x
    #        condition dictionary, t1pm_seq_pr_dict
    #                              which is from integer_t1gm_seq_primitive_root_condition
    # output: value of T1GM sequence with index x
    res = False
    if integer_check(x, radix) & isinstance(t1pm_seq_pr_dict, dict):
        temp_str_list = t1pm_seq_pr_dict.keys()
        if "type" in temp_str_list:
            if t1pm_seq_pr_dict["type"] == "t1gm_seq_primitive_root":
                res = True
    if res:
        x = integer_modulo(x, t1pm_seq_pr_dict["phi_m"], radix, check = False)["rem"]
        if integer_more_tuple_tuple(x, [True, 0], 
                                    radix, check = False):   
            res = power_modulo(t1pm_seq_pr_dict["a"], x,
                               t1pm_seq_pr_dict["m"], radix, 
                               check = False)
            res = multiply_modulo(res, t1pm_seq_pr_dict["c"],
                                  t1pm_seq_pr_dict["m"], radix, 
                                  check = False)
        else:
            res = t1pm_seq_pr_dict["c"]
    else:
        res = None
    return res

def integer_t1gm_seq_index(x, t1pm_seq_pr_dict, radix):
    # index of type-1 geometric-modulo sequence
    # input: integer vector, x, which should be mutually prime with m
    #        condition dictionary, t1pm_seq_pr_dict
    #                              which is from integer_t1gm_seq_primitive_root_condition
    # output: index of T1GM sequence with value x
    res = False
    if integer_check(x, radix) & isinstance(t1pm_seq_pr_dict, dict):
        if integer_noteq_tuple_tuple(x, [True, 0], radix, check = False):
            temp_str_list = t1pm_seq_pr_dict.keys()
            if ("type" in temp_str_list) & ("m" in temp_str_list):
                if t1pm_seq_pr_dict["type"] == "t1gm_seq_primitive_root":
                    if integer_more_tuple_tuple(t1pm_seq_pr_dict["m"], 
                                                [True, 1], radix): 
                        if integer_is_relatively_primes(x, t1pm_seq_pr_dict["m"], 
                                                        radix, check = False):
                            res = True
    if res:
        temp_bool = True
        temp_vec_0 = [True, 0]
        temp_vec_1 = t1pm_seq_pr_dict["c"]
        while temp_bool:
            temp_vec_0 = plus_one(temp_vec_0, radix, check = False)
            temp_vec_1 = multiply_modulo(temp_vec_1, t1pm_seq_pr_dict["a"], 
                                         t1pm_seq_pr_dict["m"], radix, 
                                         check = False)
            if integer_eq_tuple_tuple(temp_vec_1, x, radix, 
                                      check = False):
                res = temp_vec_0
                temp_bool = False
    else:
        res = None
    return res

def integer_pm_seq_complete_root_condition(c, b, p_vec, radix, inverse = True):
    # condition checker of power-modulo sequence
    # input: integer vector, c, b, 
    #        integer vector, or vector of integer vector, p_vec
    # output: dictionary including c, b, m 
    res = False
    if isinstance(inverse, bool):
        if integer_check(c, radix) & integer_check(b, radix): 
            if isinstance(p_vec, list) | isinstance(p_vec, tuple):
                p_len = len(p_vec)
                if p_len > 0:
                    if isinstance(p_vec[0], bool):
                        if integer_check(p_vec, radix):
                            m = integer_absolute(p_vec, radix, check = False)
                            if integer_more_tuple_tuple(m, [True, 1], radix, 
                                                        check = False):                        
                                if integer_is_prime(m, radix, check = False):
                                    phi_m = minus_one(m, radix, check = False)
                                    lambda_m = minus_one(m, radix, check = False)
                                    m_factor = tuple([m])
                                    res = True
                    else:
                        temp_bool_0 = True
                        m_factor = []
                        m = [True, 1]
                        phi_m = [True, 1]
                        lambda_m = [True, 1]
                        for n in range(p_len):
                            if integer_check(p_vec[n], radix):
                                temp_vec = integer_absolute(p_vec[n], radix, check = False)
                                if integer_more_tuple_tuple(temp_vec, [True, 1], radix, 
                                                            check = False): 
                                    if integer_is_prime(temp_vec, radix, check = False):
                                        temp_bool_1 = True
                                        temp_num_0 = 0
                                        for n1 in range(n):
                                            if integer_less_tuple_tuple(temp_vec, m_factor[n1], 
                                                                        radix, check = False):
                                                temp_bool_1 = False
                                                temp_num_0 = n1
                                                break
                                            elif integer_eq_tuple_tuple(temp_vec, m_factor[n1], 
                                                                        radix, check = False):
                                                temp_bool_0 = False
                                                break
                                        if temp_bool_0:
                                            if temp_bool_1:
                                                m_factor.append(temp_vec)
                                            else:
                                                m_factor.insert(temp_num_0, temp_vec)
                                            m = integer_multiply(m, temp_vec, 
                                                                 radix, check = False)
                                            temp_num_1 = minus_one(temp_vec, radix, check = False)
                                            phi_m = integer_multiply(phi_m, temp_num_1,
                                                                     radix, check = False)
                                            lambda_m = integer_lcm(lambda_m, temp_num_1, 
                                                                   radix, check = False)
                                    else:
                                        temp_bool_0 = False
                                else:
                                    temp_bool_0 = False
                            else:
                                temp_bool_0 = False
                            if not temp_bool_0:
                                break
                        res = temp_bool_0
    if res:
        c = integer_modulo(c, m, radix, check = False)["rem"]
        b = integer_modulo(b, lambda_m, radix, check = False)["rem"]
        if integer_is_relatively_primes(c, m, radix, check = False):
            if integer_is_relatively_primes(b, lambda_m, radix, check = False):
                if inverse:
                    inv_c = integer_mod_inverse(c, m, radix, check = False)
                    inv_b = integer_mod_inverse(b, lambda_m, radix, check = False)
            else:
                res = False
        else:
            res = False
    if res:
        phi_phi_m = integer_Euler_phi(phi_m, radix, check = False)
        if inverse:            
            res = {"type": "pm_seq_complete_root", 
                   "c": c, 
                   "b": b, 
                   "m": m, 
                   "m_prime": m_factor, 
                   "phi_m": phi_m, 
                   "phi_phi_m": phi_phi_m, 
                   "lambda_m": lambda_m, 
                   "mod_inverse_c": inv_c, 
                   "mod_inverse_b": inv_b}   
        else:
            res = {"type": "pm_seq_complete_root", 
                   "c": c, 
                   "b": b, 
                   "m": m, 
                   "m_prime": m_factor, 
                   "phi_m": phi_m, 
                   "phi_phi_m": phi_phi_m, 
                   "lambda_m": lambda_m}    
    else:
        res = None
    return res

def integer_pm_seq_convert(x, pm_seq_cr_dict, radix):
    # converter of power-modulo sequence
    # input: integer vector, x
    #        condition dictionary, pm_seq_cr_dict
    #                              which is from integer_pm_seq_complete_root_condition
    # output: value of PM sequence with index x
    res = False
    if integer_check(x, radix) & isinstance(pm_seq_cr_dict, dict):
        temp_str_list = pm_seq_cr_dict.keys()
        if "type" in temp_str_list:
            if pm_seq_cr_dict["type"] == "pm_seq_complete_root":
                res = True
    if res:
        res = power_modulo(x, pm_seq_cr_dict["b"], 
                           pm_seq_cr_dict["m"], 
                           radix, check = False)
        res = multiply_modulo(res, pm_seq_cr_dict["c"], 
                              pm_seq_cr_dict["m"], 
                              radix, check = False)
    else:
        res = None
    return res

def integer_pm_seq_index(x, pm_seq_cr_dict, radix):
    # index of power-modulo sequence
    # input: integer vector, x
    #        condition dictionary, pm_seq_cr_dict
    #                              which is from integer_pm_seq_complete_root_condition
    #                              modular inverse of b and c must be existing
    # output: index of PM sequence with value x
    res = False
    if integer_check(x, radix) & isinstance(pm_seq_cr_dict, dict):
        temp_str_list = pm_seq_cr_dict.keys()
        if (("type" in temp_str_list) & 
            ("mod_inverse_c" in temp_str_list) & 
            ("mod_inverse_b" in temp_str_list)):
            res = True
    if res:
        res = multiply_modulo(x, pm_seq_cr_dict["mod_inverse_c"], 
                              pm_seq_cr_dict["m"], 
                              radix, check = False)
        res = power_modulo(res, pm_seq_cr_dict["mod_inverse_b"], 
                           pm_seq_cr_dict["m"], 
                           radix, check = False)
    else:
        res = None
    return res


