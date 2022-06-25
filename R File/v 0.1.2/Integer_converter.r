# Integer Converter
# Hao Jiang
# v. 0.1.2







# check functions

integer_check = function(x, radix) {
  # x is a vector
  # the 1-st element in x is -1, negative integer
  #                           0, zero
  #                           1, positive integer
  # for n from 2 to x_len, the integer of x is
  # x = x[2] + x[3]*(radix^1) + ... + x[x_len]*(radix^(x_len-2))
  res = FALSE
  if (is.numeric(x)) {
    if (is.numeric(radix)) {
      if (length(radix) == 1) {
        if (round(radix) == radix) {
          if (radix > 1) {
            x_len = length(x)
            if (x_len >= 2) {
              if ((x[1] == 1) | (x[1] == -1)) {
                res = TRUE
                for (n in 2:x_len) {
                  if (round(x[n]) == x[n]) {
                    if ((x[n] < 0) | (x[n] >= radix)) {
                      res = FALSE
                      break
                    }
                  } else {
                    res = FALSE
                    break
                  }
                }
                if (res) {
                  if (x[x_len] == 0) {
                    res = FALSE
                  }
                }
              } else if (x[1] == 0) {
                if (x_len == 2) {
                  if (x[2] == 0) {
                    res = TRUE
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return (res)
}

check_less_nonneg_int_int = function(x, y) {
  res = NULL
  if (is.numeric(x) & is.numeric(y)) {
    if ((length(x) == 1) & (length(y) == 1)) {
      if ((x >= 0) & (y >= 0)) {
        res = (x < y)
      }
    }
  }
  return(res)
}

check_lesseq_nonneg_int_int = function(x, y) {
  res = NULL
  if (is.numeric(x) & is.numeric(y)) {
    if ((length(x) == 1) & (length(y) == 1)) {
      if ((x >= 0) & (y >= 0)) {
        res = (x <= y)
      }
    }
  }
  return(res)
}

check_more_nonneg_int_int = function(x, y) {
  res = NULL
  if (is.numeric(x) & is.numeric(y)) {
    if ((length(x) == 1) & (length(y) == 1)) {
      if ((x >= 0) & (y >= 0)) {
        res = (x > y)
      }
    }
  }
  return(res)
}

check_moreeq_nonneg_int_int = function(x, y) {
  res = NULL
  if (is.numeric(x) & is.numeric(y)) {
    if ((length(x) == 1) & (length(y) == 1)) {
      if ((x >= 0) & (y >= 0)) {
        res = (x >= y)
      }
    }
  }
  return(res)
}

check_eq_nonneg_int_int = function(x, y) {
  res = NULL
  if (is.numeric(x) & is.numeric(y)) {
    if ((length(x) == 1) & (length(y) == 1)) {
      if ((x >= 0) & (y >= 0)) {
        res = (x == y)
      }
    }
  }
  return(res)
}

check_noteq_nonneg_int_int = function(x, y) {
  res = NULL
  if (is.numeric(x) & is.numeric(y)) {
    if ((length(x) == 1) & (length(y) == 1)) {
      if ((x >= 0) & (y >= 0)) {
        res = (x != y)
      }
    }
  }
  return(res)
}

integer_less_vec_vec = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: logical, x < y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == 1) {
      if (y[1] == 1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = TRUE
        } else if (x_len > y_len) {
          res = FALSE
        } else {
          res = FALSE
          for (n in x_len:2) {
            if (x[n] < y[n]) {
              res = TRUE
              break
            } else if (x[n] > y[n]) {
              break
            }
          }
        }
      } else {
        res = FALSE
      }
    } else if (x[1] == -1) {
      if (y[1] == -1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = FALSE
        } else if (x_len > y_len) {
          res = TRUE
        } else {
          res = FALSE
          for (n in x_len:2) {
            if (x[n] > y[n]) {
              res = TRUE
              break
            } else if (x[n] < y[n]) {
              break
            }
          }
        }
      } else {
        res = TRUE
      }
    } else if (x[1] == 0) {
      if (y[1] == 1) {
        res = TRUE
      } else {
        res = FALSE
      }
    }
  }
  return(res)
}

integer_lesseq_vec_vec = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: logical, x <= y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == 1) {
      if (y[1] == 1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = TRUE
        } else if (x_len > y_len) {
          res = FALSE
        } else {
          res = TRUE
          for (n in x_len:2) {
            if (x[n] > y[n]) {
              res = FALSE
              break
            } else if (x[n] < y[n]) {
              break
            }
          }
        }
      } else {
        res = FALSE
      }
    } else if (x[1] == -1) {
      if (y[1] == -1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = FALSE
        } else if (x_len > y_len) {
          res = TRUE
        } else {
          res = TRUE
          for (n in x_len:2) {
            if (x[n] < y[n]) {
              res = FALSE
              break
            } else if (x[n] > y[n]) {
              break
            }
          }
        }
      } else {
        res = TRUE
      }
    } else if (x[1] == 0) {
      if ((y[1] == 1) | (y[1] == 0)) {
        res = TRUE
      } else {
        res = FALSE
      }
    }
  }
  return(res)
}

integer_more_vec_vec = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: logical, x > y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == 1) {
      if (y[1] == 1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = FALSE
        } else if (x_len > y_len) {
          res = TRUE
        } else {
          res = FALSE
          for (n in x_len:2) {
            if (x[n] > y[n]) {
              res = TRUE
              break
            } else if (x[n] < y[n]) {
              break
            }
          }
        }
      } else {
        res = TRUE
      }
    } else if (x[1] == -1) {
      if (y[1] == -1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = TRUE
        } else if (x_len > y_len) {
          res = FALSE
        } else {
          res = FALSE
          for (n in x_len:2) {
            if (x[n] < y[n]) {
              res = TRUE
              break
            } else if (x[n] > y[n]) {
              break
            }
          }
        }
      } else {
        res = FALSE
      }
    } else if (x[1] == 0) {
      if (y[1] == -1) {
        res = TRUE
      } else {
        res = FALSE
      }
    }
  }
  return(res)
}

integer_moreeq_vec_vec = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: logical, x >= y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == 1) {
      if (y[1] == 1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = FALSE
        } else if (x_len > y_len) {
          res = TRUE
        } else {
          res = TRUE
          for (n in x_len:2) {
            if (x[n] < y[n]) {
              res = FALSE
              break
            } else if (x[n] > y[n]) {
              break
            }
          }
        }
      } else {
        res = TRUE
      }
    } else if (x[1] == -1) {
      if (y[1] == -1) {
        x_len = length(x)
        y_len = length(y)
        if (x_len < y_len) {
          res = TRUE
        } else if (x_len > y_len) {
          res = FALSE
        } else {
          res = TRUE
          for (n in x_len:2) {
            if (x[n] > y[n]) {
              res = FALSE
              break
            } else if (x[n] < y[n]) {
              break
            }
          }
        }
      } else {
        res = FALSE
      }
    } else if (x[1] == 0) {
      if ((y[1] == -1) | (y[1] == 0)) {
        res = TRUE
      } else {
        res = FALSE
      }
    }
  }
  return(res)
}

integer_eq_vec_vec = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: logical, x == y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == y[1]) {
      x_len = length(x)
      y_len = length(y)
      if (x_len == y_len) {
        res = TRUE
        for (n in x_len:2) {
          if (x[n] != y[n]) {
            res = FALSE
            break
          } 
        }
      } else {
        res = FALSE
      }
    } else {
      res = FALSE
    }
  }
  return(res)
}

integer_noteq_vec_vec = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: logical, x != y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == y[1]) {
      x_len = length(x)
      y_len = length(y)
      if (x_len == y_len) {
        res = FALSE
        for (n in x_len:2) {
          if (x[n] != y[n]) {
            res = TRUE
            break
          } 
        }
      } else {
        res = TRUE
      }
    } else {
      res = TRUE
    }
  }
  return(res)
}

integer_is_even = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: logical, TRUE if x is an even integer
  #                  FALSE if x is an odd integer
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == 0) {
      res = TRUE
    } else {
      temp_num_1 = radix/2
      if_radix_even = (floor(temp_num_1) == temp_num_1)
      if (if_radix_even) {
        temp_num_1 = x[2]/2
        res = (floor(temp_num_1) == temp_num_1)
      } else {
        res = TRUE
        x_len = length(x)
        for (n in 2:x_len){
          temp_num_1 = x[n]/2
          if (floor(temp_num_1) != temp_num_1){
            res = !res
          }
        }
      }
    }
  }
  return(res)
}



# operation functions

integer_inverse = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: integer vector, -x
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    res = x
    if (res[1] != 0) {
      res[1] = -res[1]
    }
  }
  return(res)
}

integer_absolute = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: integer vector, |x|
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    res = x
    if (res[1] < 0) {
      res[1] = -res[1]
    }
  }
  return(res)
}

integer_plus = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: integer vector, x+y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (integer_moreeq_vec_vec(integer_absolute(x, radix, check = FALSE), 
                               integer_absolute(y, radix, check = FALSE), 
                               radix, check = FALSE)) {
      x1 = x
      x_len = length(x1)
      y1 = y
      y_len = length(y1)
    } else {
      x1 = y
      x_len = length(x1)
      y1 = x
      y_len = length(y1)
    }
    if (y1[1] == 0) {
      res = x1
    } else if (y1[1] == x1[1]) {
      x1 = append(x1, 0)
      x_len = x_len+1
      for (n in 2:y_len) {
        temp_num = x1[n]+y1[n]
        if (temp_num < radix) {
          x1[n] = temp_num
        } else {
          x1[n] = temp_num-radix
          x1[n+1] = x1[n+1]+1
        }
      }
      for (n in (y_len+1):x_len) {
        if (x1[n] >= radix) {
          x1[n] = x1[n]-radix
          x1[n+1] = x1[n+1]+1
        } else {
          break
        }
      }
      res = trim_initial_zeros(x1, radix, check = FALSE)
    } else if (y1[1] == -x1[1]) {
      x1 = append(x1, 0)
      x_len = x_len+1
      for (n in 2:y_len) {
        temp_num = x1[n]-y1[n]
        if (temp_num >= 0) {
          x1[n] = temp_num
        } else {
          x1[n] = temp_num+radix
          x1[n+1] = x1[n+1]-1
        }
      }
      for (n in (y_len+1):x_len) {
        if (x1[n] < 0) {
          x1[n] = x1[n]+radix
          x1[n+1] = x1[n+1]-1
        } else {
          break
        }
      }
      res = trim_initial_zeros(x1, radix, check = FALSE)
    } else {
      res = NULL
    }
  }
  return(res)
}

integer_minus = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: integer vector, x-y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)){
    res = integer_plus(x, integer_inverse(y, radix, check = FALSE), 
                       radix, check = FALSE)
  }
  return(res)
}

integer_multiply = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: integer vector, x*y
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if ((integer_eq_vec_vec(x, c(0, 0), radix, check = FALSE)) | 
        (integer_eq_vec_vec(y, c(0, 0), radix, check = FALSE))) {
      res = c(0, 0)
    } else if (integer_eq_vec_vec(x, c(1, 1), radix, check = FALSE)) {
      res = y
    } else if (integer_eq_vec_vec(x, c(-1, 1), radix, check = FALSE)) {
      res = integer_inverse(y, radix, check = FALSE)
    } else if (integer_eq_vec_vec(y, c(1, 1), radix, check = FALSE)) {
      res = x
    } else if (integer_eq_vec_vec(y, c(-1, 1), radix, check = FALSE)) {
      res = integer_inverse(x, radix, check = FALSE)
    } else {
      x_len = length(x)
      y_len = length(y)
      if (x[1] == y[1]) {
        res = 1
      } else {
        res = -1
      }
      total_len = x_len+y_len-1
      temp_vec_0 = rep(0, total_len)
      for (n1 in 2:x_len) {
        for (n2 in 2:y_len){
          temp_num = n1+n2-3
          temp_vec = multiply_digits(x[n1], y[n2], radix, check = FALSE)
          temp_len = length(temp_vec)
          if (temp_len == 1) {
            temp_num_1 = temp_vec_0[temp_num]+temp_vec
            if (temp_num_1 < radix) {
              temp_vec_0[temp_num] = temp_num_1
            } else {
              temp_vec_0[temp_num] = temp_num_1-radix
              temp_vec_0[temp_num+1] = temp_vec_0[temp_num+1]+1
            }
            temp_num = temp_num+1
          } else {
            temp_num_1 = temp_vec_0[temp_num]+temp_vec[1]
            if (temp_num_1 < radix) {
              temp_vec_0[temp_num] = temp_num_1
            } else {
              temp_vec_0[temp_num] = temp_num_1-radix
              temp_vec_0[temp_num+1] = temp_vec_0[temp_num+1]+1
            }
            temp_num_1 = temp_vec_0[temp_num+1]+temp_vec[2]
            if (temp_num_1 < radix) {
              temp_vec_0[temp_num+1] = temp_num_1
            } else {
              temp_vec_0[temp_num+1] = temp_num_1-radix
              temp_vec_0[temp_num+2] = temp_vec_0[temp_num+2]+1
            }
            temp_num = temp_num+2
          }
          for (n3 in temp_num:(total_len-1)) {
            if (temp_vec_0[n3] >= radix) {
              temp_vec_0[n3] = temp_vec_0[n3]-radix
              temp_vec_0[n3+1] = temp_vec_0[n3+1]+1
            } else {
              break
            }
          }
        }
      }
      res = c(res, temp_vec_0)
      res = trim_initial_zeros(res, radix, check = FALSE)
    }
  }
  return(res)
}

integer_modulo = function(x, y, radix, check = TRUE){
  # input: integer vector, x, y
  #                        where |y| > 1
  # output: list, ("quo"= q, "rem"= r)
  #               where r+qy = x
  if (check) {
    res = NULL
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      if (length(y) > 2) {
        res = TRUE
      } else if (y[2] > 1){
        res = TRUE
      }
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (integer_eq_vec_vec(x, c(0, 0), radix, check = FALSE)){
      res = list("quo" = c(0, 0), 
                 "rem" = c(0, 0))
    } else {
      x_sign = x[1]
      x = integer_absolute(x, radix, check = FALSE)
      y_sign = y[1]
      y = integer_absolute(y, radix, check = FALSE)
      if (integer_less_vec_vec(x, y, radix, check = FALSE)){
        temp_vec_1 = c(0, 0)
        temp_vec_2 = x
      } else {
        x_len = length(x)
        y_len = length(y)
        dif_len = x_len-y_len+1
        temp_upper_vec = c(1, rep(0, dif_len), 1)
        temp_lower_vec = c(0, 0)
        temp_lower_num_vec = c(0, 0)
        temp_diff_vec = integer_minus(temp_upper_vec, 
                                      temp_lower_vec, 
                                      radix, check = FALSE)            
        temp_bool_1 = TRUE
        temp_bool_2 = TRUE
        while(temp_bool_1) {
          temp_half_vec = floor_half_value(temp_diff_vec, 
                                           radix, check = FALSE) 
          temp_mid_vec = integer_plus(temp_lower_vec, 
                                      temp_half_vec[["half"]], 
                                      radix, check = FALSE)                
          temp_mid_num_vec = integer_multiply(temp_mid_vec, 
                                              y, radix, 
                                              check = FALSE)
          if (integer_less_vec_vec(x, temp_mid_num_vec, radix, 
                                   check = FALSE)){
            temp_upper_vec = temp_mid_vec
          } else if (integer_more_vec_vec(x, temp_mid_num_vec, radix, 
                                          check = FALSE)){
            temp_lower_vec = temp_mid_vec
            temp_lower_num_vec = temp_mid_num_vec
          } else {
            temp_lower_vec = temp_mid_vec
            temp_lower_num_vec = temp_mid_num_vec
            temp_bool_2 = FALSE
          }
          if (temp_bool_2) {
            temp_diff_vec = integer_minus(temp_upper_vec, 
                                          temp_lower_vec, 
                                          radix, check = FALSE)      
            if ((length(temp_diff_vec) == 2) & (temp_diff_vec[2] <= 1)) {
              temp_bool_1 = FALSE
            }
          } else {
            temp_bool_1 = FALSE
          }
        }
        temp_vec_1 = temp_lower_vec
        if (temp_bool_2){
          temp_vec_2 = integer_minus(x, temp_lower_num_vec, radix, 
                                     check = FALSE)
        } else {
          temp_vec_2 = c(0, 0)
        }
      }
      if (x_sign > 0) {
        if (y_sign < 0) {
          temp_vec_1 = integer_inverse(temp_vec_1, radix, check = FALSE)
        }
      } else {
        if (y_sign > 0) {
          temp_vec_1 = integer_inverse(temp_vec_1, radix, check = FALSE)
          temp_vec_1 = minus_one(temp_vec_1, radix, check = FALSE)
          temp_vec_2 = integer_inverse(temp_vec_2, radix, check = FALSE)
          temp_vec_2 = integer_plus(temp_vec_2, y, radix, check = FALSE)
        } else {
          temp_vec_1 = plus_one(temp_vec_1, radix, check = FALSE)
          temp_vec_2 = integer_inverse(temp_vec_2, radix, check = FALSE)
          temp_vec_2 = integer_plus(temp_vec_2, y, radix, check = FALSE)
        }
      }
      res = list("quo" = temp_vec_1, 
                 "rem" = temp_vec_2)
    }
  }
  return(res)
}

integer_power = function(x, y, radix, check = TRUE){
  # input: integer vector, x, y
  #                        where y >= 0
  # output: integer vector, x^y
  if (check) {
    res = NULL
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      if (integer_moreeq_vec_vec(y, c(0, 0), check = FALSE)) {
        res = TRUE
      } 
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (integer_more_vec_vec(y, c(1, 1), check = FALSE)) {
      if (integer_eq_vec_vec(x, c(0, 0), check = FALSE)) {
        res = c(0, 0)
      } else if (integer_eq_vec_vec(x, c(1, 1), check = FALSE)) {
        res = c(1, 1)
      } else if (integer_eq_vec_vec(x, c(-1, 1), check = FALSE)) {
        if (integer_is_even(y, radix, check = FALSE)){
          res = c(1, 1)
        } else {
          res = c(-1, 1)
        }
      } else {
        res = c(1, 1)
        temp_num_vec_1 = x
        temp_num_vec_2 = integer_vec_change_radix(y, radix, 2, check = FALSE)
        y_len = length(temp_num_vec_2)
        for (n in 2:y_len){
          if (temp_num_vec_2[n] > 0) {
            res = integer_multiply(res, temp_num_vec_1, radix, check = FALSE)
          }
          if (n < y_len){
            temp_num_vec_1 = integer_multiply(temp_num_vec_1, temp_num_vec_1, 
                                              radix, check = FALSE)
          }
        }
      }
    } else if (integer_more_vec_vec(y, c(0, 0), check = FALSE)) {
      res = x
    } else {
      res = c(1, 1)
    }
  }
  return(res)
}

integer_factorial = function(x, radix, check = TRUE){
  # input: integer vector, x
  #                        where x >= 0
  # output: integer vector, x!
  if (check) {
    res = NULL
    if (integer_check(x, radix)) {
      if (integer_moreeq_vec_vec(x, c(0, 0), radix, check = FALSE)) {
        res = TRUE
      } 
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (integer_more_vec_vec(x, c(1, 1), radix, check = FALSE)) {
      res = c(1, 1)
      temp_bool = TRUE
      temp_num = c(1, 1)
      while (temp_bool) {
        temp_num = plus_one(temp_num, radix, check = FALSE)
        res = integer_multiply(res, temp_num, radix, check = FALSE)
        if (integer_moreeq_vec_vec(temp_num, x, radix, check = FALSE)) {
          temp_bool = FALSE
        }
      }
    } else {
      res = c(1, 1)
    }
  }
  return(res)
}

integer_permute = function(m, n, radix, check = TRUE){
  # input: integer vector, m, n
  #                        where 0 <= n <= m, m > 0
  # output: integer vector, m permute n
  if (check) {
    res = NULL
    if ((integer_check(m, radix)) & (integer_check(n, radix))) {
      if (integer_moreeq_vec_vec(n, c(0, 0), radix, check = FALSE)) {
        if (integer_moreeq_vec_vec(m, n, radix, check = FALSE)) {
          if (integer_more_vec_vec(m, c(0, 0), radix, check = FALSE)) {
            res = TRUE
          }
        }
      } 
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)){
    res = c(1, 1)
    if (integer_more_vec_vec(n, c(0, 0), radix, check = FALSE)) {
      temp_bool = TRUE
      while (temp_bool) {
        res = integer_multiply(res, m, radix, check = FALSE)
        m = minus_one(m, radix, check = FALSE)
        n = minus_one(n, radix, check = FALSE)
        if (integer_lesseq_vec_vec(n, c(0, 0), radix, check = FALSE)){
          temp_bool = FALSE
        }
      }
    }
  }
  return(res)
}

integer_choose = function(m, n, radix, check = TRUE){
  # input: integer vector, m, n
  #                        where 0 <= n <= m, m > 0
  # output: integer vector, m choose n
  if (check) {
    res = NULL
    if ((integer_check(m, radix)) & (integer_check(n, radix))) {
      if (integer_moreeq_vec_vec(n, c(0, 0), radix, check = FALSE)) {
        if (integer_moreeq_vec_vec(m, n, radix, check = FALSE)) {
          if (integer_more_vec_vec(m, c(0, 0), radix, check = FALSE)) {
            res = TRUE
          }
        }
      } 
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)){
    complement_n = integer_minus(m, n, radix, check = FALSE)
    if (integer_less_vec_vec(complement_n, n, radix, check = FALSE)){
      n = complement_n
    }
    res = c(1, 1)
    if (integer_more_vec_vec(n, c(0, 0), radix, check = FALSE)) {
      res = m
      if (integer_more_vec_vec(n, c(1, 1), radix, check = FALSE)) {
        temp_bool = TRUE
        temp_num = c(1, 1)
        while (temp_bool) {
          m = minus_one(m, radix, check = FALSE)
          res = integer_multiply(res, m, radix, check = FALSE)
          n = minus_one(n, radix, check = FALSE)
          temp_num = plus_one(temp_num, radix, check = FALSE)
          res = integer_modulo(res, temp_num, radix, check = FALSE)
          res = res[["quo"]]
          if (integer_lesseq_vec_vec(n, c(1, 1), radix, check = FALSE)){
            temp_bool = FALSE
          }
        }
      }
    }
  }
  return(res)
}

integer_mod_inverse = function(x, m, radix, check = TRUE) {
  # input: integer vector, x, m
  #                        where x != 0, m !in {-1, 0, 1}, 
  #                              x and m are relatively primes
  # output: integer vector, inverse(x) (mod m)    
  if (check) {
    res = NULL
    if ((integer_check(x, radix)) & (integer_check(m, radix))) {
      if (integer_noteq_vec_vec(x, c(0, 0), radix, check = FALSE)) {
        if ((integer_less_vec_vec(m, c(-1, 1), radix, check = FALSE)) | 
            (integer_more_vec_vec(m, c(1, 1), radix, check = FALSE))) {
          if (integer_is_relatively_primes(x, m, radix, check = FALSE)) {
            res = TRUE
          }
        }
      }
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    temp_num_1_1 = integer_absolute(m, radix, check = FALSE)
    temp_num_1_2 = integer_modulo(x, temp_num_1_1, radix, check = FALSE)
    temp_num_1_2 = temp_num_1_2[["rem"]]
    temp_num_2_1 = c(1, 1)
    temp_num_2_2 = c(0, 0)
    temp_num_3_1 = c(0, 0)
    temp_num_3_2 = c(1, 1)
    temp_bool_1 = TRUE
    while (temp_bool_1) {
      temp_modulo = integer_modulo(temp_num_1_1, temp_num_1_2, radix, check = FALSE)
      if (integer_more_vec_vec(temp_modulo[["rem"]], c(0, 0), radix, check = FALSE)) {
        temp_num_1_1 = temp_num_1_2
        temp_num_1_2 = temp_modulo[["rem"]]
        temp_num_2_3 = integer_multiply(temp_num_2_2, temp_modulo[["quo"]], radix, check = FALSE)
        temp_num_2_3 = integer_minus(temp_num_2_1, temp_num_2_3, radix, check = FALSE)
        temp_num_2_1 = temp_num_2_2
        temp_num_2_2 = temp_num_2_3
        temp_num_3_3 = integer_multiply(temp_num_3_2, temp_modulo[["quo"]], radix, check = FALSE)
        temp_num_3_3 = integer_minus(temp_num_3_1, temp_num_3_3, radix, check = FALSE)
        temp_num_3_1 = temp_num_3_2
        temp_num_3_2 = temp_num_3_3
      } else {
        temp_bool_1 = FALSE
      }
    }
    res = integer_modulo(temp_num_3_2, m, radix, check = FALSE)
    res = res[["rem"]]
  }
  return(res)
}

integer_crt = function(r, m, radix, check = TRUE) {
  # input: vector of integer vector, r, m
  #                          where r is the vector of objective remainders
  #                          where m is the vector of divisors
  #                                & all the elements in m are pairwise relatively primes 
  #                                  (excluding -1, 0, 1)
  # output: integer vector, the integer of Chinese remainder theorem
  if (check) {
    res = NULL
    if ((is.list(r)) & (is.list(m))) {
      r_len = length(r)
      m_len = length(m)
      if ((r_len == m_len) & (r_len > 1)) {
        temp_bool_0 = TRUE
        for (n1 in 1:r_len) {
          if ((integer_check(r[[n1]], radix)) & (integer_check(m[[n1]], radix))) {
            if ((integer_less_vec_vec(m[[n1]], c(-1, 1), radix, check = FALSE)) | 
                (integer_more_vec_vec(m[[n1]], c(1, 1), radix, check = FALSE))) {
              temp_num_1 = n1-1
              if (temp_num_1 > 0) {
                for (n2 in 1:temp_num_1) {
                  temp_bool_0 = integer_is_relatively_primes(m[[n1]], m[[n2]], radix, check = FALSE)
                  if (!temp_bool_0) {
                    break
                  }
                }
              }
            }
          } else {
            temp_bool_0 = FALSE
          }
          if (!temp_bool_0) {
            break
          }
        }
        if (temp_bool_0) {
          res = TRUE
        }
      }
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    r_len = length(r)
    M = c(1, 1)
    for (n in 1:r_len) {
      temp_num = integer_absolute(m[[n]], radix, check = FALSE)
      M = integer_multiply(M, temp_num, radix, check = FALSE)
    }
    res = c(0, 0)
    for (n in 1:r_len) {
      temp_num = integer_modulo(M, m[[n]], radix, check = FALSE)
      temp_num = temp_num[["quo"]]
      temp_num_1 = integer_mod_inverse(temp_num, m[[n]], 
                                       radix, check = FALSE)
      temp_num = multiply_modulo(temp_num, temp_num_1,M, 
                                 radix, check = FALSE)
      temp_num = multiply_modulo(temp_num, r[[n]], M, 
                                 radix, check = FALSE)
      res = plus_modulo(res, temp_num, M, 
                        radix, check = FALSE)
    }
  }
  return(res)
}

multiply_digits = function(x, y, radix, check = TRUE) {
  if (check) {
    res = NULL
    if ((is.numeric(x)) & (is.numeric(y))) {
      if ((length(x) == 1) & (length(y) == 1)) {
        if ((round(x) == x) & (round(y) == y)) {
          if ((x >= 0) & (x < radix) & (y >= 0) & (y < radix)) {
            res = TRUE
          }
        }
      }
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if ((x == 0) | (y == 0)) {
      res = 0
    } else if (x == 1) {
      res = y
    } else if (y == 1) {
      res = x
    } else {
      prod_xy = x*y
      if (prod_xy < radix) {
        res = prod_xy
      } else {
        temp_upper = radix
        temp_lower = 0
        temp_diff = temp_upper-temp_lower
        temp_lower_num = 0
        while (temp_diff > 1) {
          temp_mid = floor(temp_diff/2)+temp_lower
          temp_mid_num = radix*temp_mid
          if (prod_xy > temp_mid_num) {
            temp_lower = temp_mid
            temp_lower_num = temp_mid_num
            temp_diff = temp_upper-temp_lower
          } else if (prod_xy < temp_mid_num) {
            temp_upper = temp_mid
            temp_diff = temp_upper-temp_lower
          } else {
            temp_lower = temp_mid
            temp_lower_num = temp_mid_num
            break
          }
        }
        res = c(prod_xy-temp_lower_num, temp_lower)
      }
    }
  }
  return(res)
}

floor_half_value = function(x, radix, check = TRUE) {
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] == 0) {
      res = c(0, 0)
    } else {
      x_len = length(x)
      res = c(x[1], rep(0, (x_len-1)))
      temp_num_0 = radix/2
      temp_num_1 = floor(temp_num_0)
      if (temp_num_0 > temp_num_1){
        half_radix = temp_num_1
        odd_radix = TRUE
      } else {
        half_radix = temp_num_1
        odd_radix = FALSE
      } 
      if_remainder = FALSE
      for (n in x_len:2) {
        temp_num_0 = x[n]/2
        temp_num_1 = floor(temp_num_0)
        if (temp_num_0 > temp_num_1) {
          if (if_remainder) {
            if (odd_radix) {
              res[n] = half_radix+temp_num_1+1
              if_remainder = FALSE
            } else {
              res[n] = half_radix+temp_num_1
            }
          } else {
            res[n] = temp_num_1
            if_remainder = TRUE
          }
        } else {
          if (if_remainder) {
            if (odd_radix) {
              res[n] = half_radix+temp_num_1
            } else {
              res[n] = half_radix+temp_num_1
              if_remainder = FALSE
            }
          } else {
            res[n] = temp_num_1
          }
        }
      }
      res = list("half" = trim_initial_zeros(res, radix, check = FALSE), 
                 "if_rem" = if_remainder)
    }
  }
  return(res)
}

floor_sqrt_value = function(x, radix, check = TRUE) {
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)){
    x_sign = x[1]
    if (x_sign == 0) {
      res = list("root" = c(0, 0), 
                 "root_rem" = c(0, 0), 
                 "real" = TRUE)
    } else {
      x = integer_absolute(x, radix, check = FALSE)
      if (integer_more_vec_vec(x, integer_num_2_vec(8, radix, check = FALSE), 
                               radix, check = FALSE)) {
        temp_upper_vec = floor_half_value(x, radix, check = FALSE)
        temp_upper_vec = temp_upper_vec[["half"]]
        temp_lower_vec = c(1, 1)
        temp_lower_num_vec = c(1, 1)
        temp_diff_vec = integer_minus(temp_upper_vec, temp_lower_vec, radix, 
                                      check = FALSE)
        temp_bool_1 = TRUE
        temp_bool_2 = TRUE
        while (temp_bool_1) {
          temp_half_vec = floor_half_value(temp_diff_vec, 
                                           radix, check = FALSE) 
          temp_mid_vec = integer_plus(temp_lower_vec, 
                                      temp_half_vec[["half"]], 
                                      radix, check = FALSE)                
          temp_mid_num_vec = integer_multiply(temp_mid_vec, 
                                              temp_mid_vec, radix, 
                                              check = FALSE)
          if (integer_less_vec_vec(x, temp_mid_num_vec, radix, 
                                   check = FALSE)) {
            temp_upper_vec = temp_mid_vec
          } else if (integer_more_vec_vec(x, temp_mid_num_vec, radix, 
                                          check = FALSE)) {
            temp_lower_vec = temp_mid_vec
            temp_lower_num_vec = temp_mid_num_vec
          } else {
            temp_lower_vec = temp_mid_vec
            temp_lower_num_vec = temp_mid_num_vec
            temp_bool_2 = FALSE
          }
          if (temp_bool_2) {
            temp_diff_vec = integer_minus(temp_upper_vec, temp_lower_vec, radix, 
                                          check = FALSE)
            if ((length(temp_diff_vec) == 2) & (temp_diff_vec[2] <= 1)) {
              temp_bool_1 = FALSE
            }
          } else {
            temp_bool_1 = FALSE
          }
        }
        temp_vec_1 = temp_lower_vec
        temp_vec_2 = temp_lower_num_vec
      } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(4, radix, check = FALSE), 
                                        radix, check = FALSE)) {
        temp_vec_1 = integer_num_2_vec(2, radix, check = FALSE)
        temp_vec_2 = integer_num_2_vec(4, radix, check = FALSE)
      } else {
        temp_vec_1 = integer_num_2_vec(1, radix, check = FALSE)
        temp_vec_2 = integer_num_2_vec(1, radix, check = FALSE)
      }
      res = list("root" = temp_vec_1, 
                 "root_rem" = integer_minus(x, temp_vec_2, radix, 
                                            check = FALSE), 
                 "real" = (x_sign > 0))
    }
  }
  return(res)
}

plus_one = function(x, radix, check = TRUE) {
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] > 0){
      x_len = length(x)
      x = c(x, 0)
      x[2] = x[2]+1
      for (n in 2:x_len) {
        if (x[n] >= radix){
          x[n] = x[n]-radix
          x[n+1] = x[n+1]+1
        } else {
          break
        }
      }
      if (x[x_len+1] < 1){
        res = x[1:x_len]
      } else {
        res = x
      }
    } else if (x[1] < 0) {
      x_len = length(x)
      x[2] = x[2]-1
      for (n in 2:x_len) {
        if (x[n] < 0){
          x[n] = x[n]+radix
          x[n+1] = x[n+1]-1
        } else {
          break
        }
      }
      if (x[x_len] < 1) {
        if (x_len > 2){
          res = x[1:(x_len-1)]
        } else {
          res = c(0, 0)
        }
      } else {
        res = x
      }
    } else {
      res = c(1, 1)
    }
  }
  return(res)
}

minus_one = function(x, radix, check = TRUE) {
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x[1] > 0){
      x_len = length(x)
      x[2] = x[2]-1
      for (n in 2:x_len) {
        if (x[n] < 0){
          x[n] = x[n]+radix
          x[n+1] = x[n+1]-1
        } else {
          break
        }
      }
      if (x[x_len] < 1){
        if (x_len > 2){
          res = x[1:(x_len-1)]
        } else {
          res = c(0, 0)
        }
      } else {
        res = x
      }
    } else if (x[1] < 0) {
      x_len = length(x)
      x = c(x, 0)
      x[2] = x[2]+1
      for (n in 2:x_len) {
        if (x[n] >= radix){
          x[n] = x[n]-radix
          x[n+1] = x[n+1]+1
        } else {
          break
        }
      }
      if (x[x_len+1] < 1) {
        res = x[1:x_len]
      } else {
        res = x
      }
    } else {
      res = c(-1, 1)
    }
  }
  return(res)
}

plus_modulo = function(x, y, m, radix, check = TRUE){
  if (check) {
    res = NULL
    if ((integer_check(x, radix)) & (integer_check(y, radix)) &
        (integer_check(m, radix))) {
      if (length(m) > 2) {
        res = TRUE
      } else if (m[2] > 1) {
        res = TRUE
      }
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    temp_vec_0 = integer_plus(x, y, radix, check = FALSE)
    res = integer_modulo(temp_vec_0, m, radix, check = FALSE)
    res = res[["rem"]]
  }
  return(res)
}

multiply_modulo = function(x, y, m, radix, check = TRUE){
  if (check) {
    res = NULL
    if ((integer_check(x, radix)) & (integer_check(y, radix)) &
        (integer_check(m, radix))) {
      if (length(m) > 2) {
        res = TRUE
      } else if (m[2] > 1) {
        res = TRUE
      }
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    temp_vec_0 = integer_multiply(x, y, radix, check = FALSE)
    res = integer_modulo(temp_vec_0, m, radix, check = FALSE)
    res = res[["rem"]]
  }
  return(res)
}

power_modulo = function(x, y, m, radix, check = TRUE){
  if (check) {
    res = NULL
    if ((integer_check(x, radix)) & (integer_check(y, radix)) &
        (integer_check(m, radix))) {
      if (integer_more_vec_vec(y, c(0, 0), check = FALSE)) {
        if (length(m) > 2) {
          res = TRUE
        } else if (m[2] > 1) {
          res = TRUE
        }
      } 
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    res = c(1, 1)
    temp_vec_y = integer_vec_change_radix(y, radix, 2, check = FALSE)
    y_len = length(temp_vec_y)
    temp_vec_1 = integer_modulo(x, m, radix, check = FALSE)
    temp_vec_1 = temp_vec_1[["rem"]]
    for (n in 2:y_len){
      if (temp_vec_y[n] > 0){
        res = integer_multiply(res, temp_vec_1, radix, check = FALSE)
        res = integer_modulo(res, m, radix, check = FALSE)
        res = res[["rem"]]
      }
      if (n < y_len){
        temp_vec_1 = integer_multiply(temp_vec_1, temp_vec_1, radix, 
                                      check = FALSE)
        temp_vec_1 = integer_modulo(temp_vec_1, m, radix, check = FALSE)
        temp_vec_1 = temp_vec_1[["rem"]]
      }
    }
  }
  return(res)
}

trim_initial_zeros = function(x, radix, check = TRUE) {
  if (check) {
    res = NULL
    if (is.numeric(x)) {
      if (is.numeric(radix)) {
        if (length(radix) == 1) {
          if (round(radix) == radix) {
            if (radix >= 2) {
              x_len = length(x)
              if (x_len >= 2) {
                if (x[1] %in% c(-1, 1)) {
                  res = TRUE
                  for (n in 2:x_len) {
                    if (round(x[n]) == x[n]) {
                      if ((x[n] < 0) | (x[n] >= radix)) {
                        res = NULL
                        break
                      }
                    } else {
                      res = NULL
                      break
                    }
                  }
                } else if (x[1] == 0) {
                  if ((x_len == 2) & (x[2] == 0)) {
                    res = TRUE
                  }
                }
              }
            }
          }
        }
      }
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x_len = length(x)
    x_len_0 = 2
    for (n in x_len:2) {
      if (x[n] != 0) {
        x_len_0 = n
        break
      }
    }
    if (x_len_0 > 2) {
      res = x[1:x_len_0]
    } else {
      if (x[2] > 0) {
        res = x[1:x_len_0]
      } else {
        res = c(0, 0)
      }
    }
  } 
  return(res)
}



# conversion functions

integer_num_2_vec = function(x, radix, check = TRUE) {
  # input: numeric, x
  # output: integer vector, x
  if (check) {
    res = NULL
    if ((is.numeric(x)) & (is.numeric(radix))) {
      if ((length(x) == 1) & (length(radix) == 1)) {
        if ((round(x) == x) & (round(radix) == radix)) {
          if (radix >= 2) {
            res = TRUE
          }
        }
      }
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (x == 0) {
      res = c(0, 0)
    } else {
      if (x > 0) {
        res = 1
      } else {
        res = -1
      }
      x = abs(x)
      temp_bool_0 = TRUE
      temp_bool_1 = TRUE
      temp_num_0 = 0
      temp_num_1 = 1
      temp_num_vec_0 = numeric(0)
      while (temp_bool_0) {
        if (x > temp_num_1) {
          temp_num_0 = temp_num_0+1
          temp_num_vec_0 = append(temp_num_vec_0, temp_num_1)
          temp_num_1 = temp_num_1*radix
        } else if (x == temp_num_1) {
          temp_num_0 = temp_num_0+1
          res = c(res, rep(0, temp_num_0))
          res[temp_num_0+1] = 1
          temp_bool_0 = FALSE
          temp_bool_1 = FALSE
        } else {
          temp_bool_0 = FALSE
        }
      }
      if (temp_bool_1) {
        res = c(res, rep(0, temp_num_0))
        for (n in temp_num_0:1) {
          temp_upper = radix
          temp_lower = 0
          temp_diff = temp_upper-temp_lower
          temp_lower_num = 0
          while (temp_diff > 1) {
            temp_mid = floor(temp_diff/2)+temp_lower
            temp_mid_num = temp_num_vec_0[n]*temp_mid
            if (x > temp_mid_num) {
              temp_lower = temp_mid
              temp_lower_num = temp_mid_num
              temp_diff = temp_upper-temp_lower
            } else if (x < temp_mid_num) {
              temp_upper = temp_mid
              temp_diff = temp_upper-temp_lower
            } else {
              temp_lower = temp_mid
              temp_lower_num = temp_mid_num
              temp_bool_1 = FALSE
              break
            }
          }
          res[n+1] = temp_lower
          if (temp_bool_1) {
            x = x-temp_lower_num
          } else {
            break
          }
        }
      }
    }
  }
  return(res)
}

integer_vec_2_num = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: numeric, x
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)){
    if (x[1] == 0) {
      res = 0
    } else {
      res = 0
      x_len = length(x)
      temp_num_0 = 1
      for (n in 2:x_len) {
        res = res+x[n]*temp_num_0
        temp_num_0 = temp_num_0*radix
      }
      if (x[1] < 0) {
        res = res*(-1)
      }
    }
  }
  return(res)
}

integer_vec_change_radix = function(x, origin_radix, new_radix, check = TRUE) {
  # input: integer vector, x with radix origin_radix
  # output: integer vector, x with radix new_radix
  if (check) {
    res = NULL
    if (integer_check(x, origin_radix)) {
      if (is.numeric(new_radix)){
        if (length(new_radix) == 1) {
          if (round(new_radix) == new_radix){
            if (new_radix >= 2) {
              res = TRUE
            }
          }
        }
      }
    } 
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    if (integer_eq_vec_vec(x, c(0, 0), origin_radix, check = FALSE)) {
      res = c(0, 0)
    } else {
      if (x[1] > 0) {
        res = 1
      } else {
        res = -1
      }
      x_len = length(x)
      temp_vec_1 = 0
      temp_vec_3 = 1
      for (n1 in 2:x_len) {
        temp_vec_2 = 0
        if (x[n1] > 0){
          for (n2 in 1:(x[n1])) {
            temp_vec_2 = iter_add_pos(temp_vec_2, temp_vec_3, new_radix)
          }
        }
        temp_vec_1 = iter_add_pos(temp_vec_2, temp_vec_1, new_radix)
        if (n1 < x_len) {
          for (n2 in (x[n1]+1):origin_radix){
            temp_vec_2 = iter_add_pos(temp_vec_2, temp_vec_3, new_radix)
          }
          temp_vec_3 = temp_vec_2
        }
      }
      res = c(res, temp_vec_1)
    }
  }
  return(res)
}

iter_add_pos = function(x, y, radix) {
  x_len = length(x)
  if ((x_len == 1) & (x[1] == 0)) {
    res = y
  } else {
    y_len = length(y)
    if ((y_len == 1) & (y[1] == 0)) {
      res = x
    } else {
      res = c(x, 0)
      for (n in 1:y_len){
        temp_num_n = res[n]-radix+y[n]
        if (temp_num_n < 0) {
          temp_num_n = temp_num_n+radix
          res[n] = temp_num_n
        } else {
          res[n] = temp_num_n
          res[n+1] =res[n+1]+1
        }
      }
      for (n in (y_len+1):x_len) {
        if (res[n] >= radix) {
          res[n] = res[n]-radix
          res[n+1] =  res[n+1]+1
        } else {
          break
        }
      }
      if (res[x_len+1] == 0){
        res = res[1:x_len]
      }
    }
  }
  return(res)
}



# prime-related, composite-related functions

integer_is_prime = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: bool, if x is prime, True
  #               else if x is composite, False
  #               else if x is 0 or 1 or -1, None
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    if (integer_more_vec_vec(x, integer_num_2_vec(16, radix, check = FALSE), 
                             radix, check = FALSE)) {
      res = TRUE
      sqrt_num = floor_sqrt_value(x, radix, check = FALSE)
      sqrt_num = sqrt_num[["root"]]
      half_sqrt_num = floor_half_value(sqrt_num, radix, check = FALSE)
      half_sqrt_num = half_sqrt_num[["half"]]
      factor_whole_list = list("num" = 0, 
                               "prime" = list(),  
                               "bool" = logical(0), 
                               "count" = list())
      temp_bool_1 = TRUE
      temp_num_0 = c(1, 1)
      while (temp_bool_1) {
        temp_num_0 = plus_one(temp_num_0, radix, check = FALSE)
        if (!(TRUE %in% factor_whole_list[["bool"]])) {
          if (integer_lesseq_vec_vec(temp_num_0, half_sqrt_num, 
                                     radix, check = FALSE)){
            factor_whole_list[["num"]] = factor_whole_list[["num"]]+1
            factor_whole_list[["prime"]][[factor_whole_list[["num"]]]] = temp_num_0
            factor_whole_list[["bool"]][factor_whole_list[["num"]]] = TRUE
            factor_whole_list[["count"]][[factor_whole_list[["num"]]]] = c(0, 0)
          }
          temp_modulo_rem = integer_modulo(x, temp_num_0, 
                                           radix, check = FALSE)
          temp_modulo_rem = temp_modulo_rem[["rem"]]
          if ((length(temp_modulo_rem) == 2) & (temp_modulo_rem[1] < 1)){
            res = FALSE
          }
        }
        if (res) {
          if (integer_moreeq_vec_vec(temp_num_0, sqrt_num, radix, check = FALSE)) {
            temp_bool_1 = FALSE
          } else {
            factor_whole_list = factorization_next(factor_whole_list, radix)
          }
        } else {
          temp_bool_1 = FALSE
        }
      }
    } else {
      temp_num = integer_vec_2_num(x, radix, check = FALSE)
      if (temp_num < 2) {
        res = NULL
      } else if (temp_num %in% c(4, 6, 8, 9, 10, 12, 14, 15, 16)) {
        res = FALSE
      } else {
        res = TRUE
      }
    } 
  }
  return(res)
}

integer_factorization = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: prime-product of x
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    if (integer_more_vec_vec(x, integer_num_2_vec(16, radix, check = FALSE), 
                             radix, check = FALSE)) {
      sqrt_num = floor_sqrt_value(x, radix, check = FALSE)
      sqrt_num = sqrt_num[["root"]]
      half_sqrt_num = floor_half_value(sqrt_num, radix, check = FALSE)
      half_sqrt_num = half_sqrt_num[["half"]]
      factor_whole_list = list("num" = 0, 
                               "prime" = list(),  
                               "bool" = logical(0), 
                               "count" = list())
      prime_list = list()
      power_list = list()
      prime_num = 0
      temp_bool_1 = TRUE
      temp_num_0 = c(1, 1)
      while (temp_bool_1) {
        temp_num_0 = plus_one(temp_num_0, radix, check = FALSE)
        if (!(TRUE %in% factor_whole_list[["bool"]])) {
          if (integer_lesseq_vec_vec(temp_num_0, half_sqrt_num, 
                                     radix, check = FALSE)){
            factor_whole_list[["num"]] = factor_whole_list[["num"]]+1
            factor_whole_list[["prime"]][[factor_whole_list[["num"]]]] = temp_num_0
            factor_whole_list[["bool"]][factor_whole_list[["num"]]] = TRUE
            factor_whole_list[["count"]][[factor_whole_list[["num"]]]] = c(0, 0)
          }
          temp_bool_2 = TRUE
          temp_bool_3 = TRUE
          while (temp_bool_2) {
            if (integer_more_vec_vec(x, temp_num_0, radix, check = FALSE)) {
              temp_modulo_res = integer_modulo(x, temp_num_0, 
                                               radix, check = FALSE)
              temp_modulo_rem = temp_modulo_res[["rem"]]
              if (temp_modulo_rem[1] == 0) {
                if (temp_bool_3) {
                  prime_num = prime_num+1
                  prime_list[[prime_num]] = temp_num_0
                  power_list[[prime_num]] = c(1, 1)
                  x = temp_modulo_res[["quo"]]
                  temp_bool_3 = FALSE
                } else {
                  power_list[[prime_num]] = plus_one(power_list[[prime_num]], 
                                                     radix, check = FALSE)
                  x = temp_modulo_res[["quo"]]
                }
              } else {
                temp_bool_2 = FALSE
              }
            } else {
              if (temp_bool_3){
                prime_num = prime_num+1
                prime_list[[prime_num]] = temp_num_0
                power_list[[prime_num]] = c(1, 1)
                x = c(1, 1)
              } else {
                power_list[[prime_num]] = plus_one(power_list[[prime_num]], 
                                                   radix, check = FALSE)
                x = c(1, 1)
              }
              temp_bool_2 = FALSE
            }
          }
        }
        if ((length(x) == 2) & (x[2] < 2)) {
          temp_bool_1 = FALSE
        } else if (integer_moreeq_vec_vec(temp_num_0, sqrt_num, 
                                          radix, check = FALSE)){
          prime_num = prime_num+1
          prime_list[[prime_num]] = x
          power_list[[prime_num]] = c(1, 1)
          temp_bool_1 = FALSE
        } else {
          factor_whole_list = factorization_next(factor_whole_list, radix)
        }
      }
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(16, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE))
      power_list = list(integer_num_2_vec(4, radix, check = FALSE))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(15, radix, check = FALSE), 
                                     radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(3, radix, check = FALSE), 
                        integer_num_2_vec(5, radix, check = FALSE))
      power_list = list(c(1, 1), 
                        c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(14, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE), 
                        integer_num_2_vec(7, radix, check = FALSE))
      power_list = list(c(1, 1), 
                        c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(13, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(13, radix, check = FALSE))
      power_list = list(c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(12, radix, check = FALSE), 
                                     radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE), 
                        integer_num_2_vec(3, radix, check = FALSE))
      power_list = list(integer_num_2_vec(2, radix, check = FALSE), 
                        c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(11, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(11, radix, check = FALSE))
      power_list = list(c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(10, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE), 
                        integer_num_2_vec(5, radix, check = FALSE))
      power_list = list(c(1, 1), 
                        c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(9, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(3, radix, check = FALSE))
      power_list = list(integer_num_2_vec(2, radix, check = FALSE))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(8, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE))
      power_list = list(integer_num_2_vec(3, radix, check = FALSE))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(7, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(7, radix, check = FALSE))
      power_list = list(c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(6, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE), 
                        integer_num_2_vec(3, radix, check = FALSE))
      power_list = list(c(1, 1), 
                        c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(5, radix, check = FALSE), 
                                     radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(5, radix, check = FALSE))
      power_list = list(c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(4, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE))
      power_list = list(integer_num_2_vec(2, radix, check = FALSE))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(3, radix, check = FALSE), 
                                     radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(3, radix, check = FALSE))
      power_list = list(c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else if (integer_moreeq_vec_vec(x, integer_num_2_vec(2, radix, check = FALSE), 
                                      radix, check = FALSE)) {
      prime_list = list(integer_num_2_vec(2, radix, check = FALSE))
      power_list = list(c(1, 1))
      res = list("prime" = prime_list, "power" = power_list)
    } else {
      res = NULL
    }
  }
  return(res)
}

integer_divisors = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: divisors of x
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    if (integer_more_vec_vec(x, c(1, 1), radix, check = FALSE)) {
      temp_factor_list = integer_factorization(x, radix, check = FALSE)
      temp_len = length(temp_factor_list[["prime"]])
      temp_factor_power = list()
      for (n in 1:temp_len) {
        temp_list_1 = list()
        temp_num_1 = c(0, 0)
        temp_num_2 = c(1, 1)
        temp_num_3 = 1
        temp_list_1[[temp_num_3]] = temp_num_2
        temp_bool_1 = TRUE
        while (temp_bool_1) {
          temp_num_3 = temp_num_3+1
          temp_num_2 = integer_multiply(temp_num_2, temp_factor_list[["prime"]][[n]], 
                                        radix, check = FALSE)
          temp_num_1 = plus_one(temp_num_1, radix, check = FALSE)
          temp_list_1[[temp_num_3]] = temp_num_2
          if (integer_moreeq_vec_vec(temp_num_1, temp_factor_list[["power"]][[n]], 
                                     radix, check = FALSE)) {
            temp_bool_1 = FALSE
          }
        }
        temp_factor_power[[n]] = temp_list_1
      }
      temp_index_vec = numeric(0)
      temp_index_max_vec = numeric(0)
      for (n in 1:temp_len) {
        temp_index_vec = append(temp_index_vec, 1)
        temp_index_max_vec = append(temp_index_max_vec, 
                                    integer_vec_2_num(temp_factor_list[["power"]][[n]], 
                                                       radix, check = FALSE))
      }
      temp_factor_list = list()
      temp_index_loc_vec = numeric(0)
      temp_cur_1 = 0
      temp_bool_1 = TRUE
      while (temp_bool_1) {
        temp_num_1 = c(1, 1)
        for (n in 1:temp_len) {
          temp_num_1 = integer_multiply(temp_num_1, 
                                        temp_factor_power[[n]][[temp_index_vec[n]]], 
                                        radix, check = FALSE)
        }
        if (temp_cur_1 > 0) {
          temp_cur_2 = temp_cur_1
          for (n in 1:temp_cur_1) {
            if (integer_less_vec_vec(temp_num_1, temp_factor_list[[temp_index_loc_vec[n]]], 
                                     radix, check = FALSE)) {
              temp_cur_2 = n-1
              break
            }
          }
          temp_cur_1 = temp_cur_1+1
          temp_factor_list[[temp_cur_1]] = temp_num_1
          temp_index_loc_vec = append(temp_index_loc_vec, temp_cur_1, temp_cur_2)
        } else {
          temp_cur_1 = temp_cur_1+1
          temp_factor_list[[temp_cur_1]] = temp_num_1
          temp_index_loc_vec = append(temp_index_loc_vec, temp_cur_1)
        }
        temp_index_vec[1] = temp_index_vec[1]+1
        if (temp_len > 1) {
          for (n in 1:(temp_len-1)) {
            if (temp_index_vec[n] > temp_index_max_vec[n]+1) {
              temp_index_vec[n] = 1
              temp_index_vec[n+1] = temp_index_vec[n+1]+1
            } else {
              break
            }
          }
        }
        if (temp_index_vec[temp_len] > temp_index_max_vec[temp_len]+1) {
          res = list()
          for (n in 1:temp_cur_1) {
            res[[n]] = temp_factor_list[[temp_index_loc_vec[n]]]
          }
          temp_bool_1 = FALSE
        }
      }
    } else if (integer_moreeq_vec_vec(x, c(1, 1), radix, check = FALSE)) {
      res = list()
      res[[1]] = c(1, 1)
    } else {
      res = NULL
    }
  }
  return(res)
}

integer_Euler_phi = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: Euler's phi of x
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    if (integer_less_vec_vec(x, c(1, 1), radix, check = FALSE)) {
      res = NULL
    } else if (integer_lesseq_vec_vec(x, c(1, 1), radix, check = FALSE)) {
      res = c(1, 1)
    } else {
      temp_fact = integer_factorization(x, radix, check = FALSE)
      fact_len = length(temp_fact[["prime"]])
      res = c(1, 1)
      for (n in 1:fact_len){
        temp_num = integer_power(temp_fact[["prime"]][[n]], 
                                 minus_one(temp_fact[["power"]][[n]], radix, check = FALSE), 
                                 radix, check = FALSE)
        temp_num = integer_multiply(temp_num, 
                                    minus_one(temp_fact[["prime"]][[n]], radix, check = FALSE), 
                                    radix, check = FALSE)
        res = integer_multiply(res, temp_num, radix, check = FALSE)
      }
    }
  }
  return(res)
}

integer_gcd = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: greatest common divisor of x, y
  #         where both of x and y are not 0
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    y = integer_absolute(y, radix, check = FALSE)
    if ((length(x) == 2) & (x[2] < 1) & 
        (length(y) == 2) & (y[2] < 1)) {
      res = NULL
    } else if ((length(x) == 2) & (x[2] < 1)) {
      res = y
    } else if ((length(y) == 2) & (y[2] < 1)) {
      res = x
    } else {
      if ((length(x) == 2) & (x[2] < 2)) {
        res = c(1, 1)
      } else if ((length(y) == 2) & (y[2] < 2)) {
        res = c(1, 1)
      } else {
        if (integer_eq_vec_vec(x, y, radix, check = FALSE)) {
          res = x
          temp_bool_1 = FALSE   
        } else if (integer_more_vec_vec(x, y, radix, check = FALSE)) {
          x_1 = x
          y_1 = y
          temp_bool_1 = TRUE
        } else {
          x_1 = y
          y_1 = x
          temp_bool_1 = TRUE
        }
        while (temp_bool_1) {
          r_1 = integer_modulo(x_1, y_1, radix, check = FALSE)
          r_1 = r_1[["rem"]]
          if ((length(r_1) == 2) & (r_1[2] < 1)) {
            res = y_1
            temp_bool_1 = FALSE  
          } else if ((length(r_1) == 2) & (r_1[2] < 2)) {
            res = c(1, 1)
            temp_bool_1 = FALSE  
          } else {
            x_1 = y_1
            y_1 = r_1
          }
        }
      }
    }
  }
  return(res)
}

integer_is_relatively_primes = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: if x and y are relatively primes, True
  #         if x and y are not relatively primes, False
  #         if either of x and y is 0, Null
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    y = integer_absolute(y, radix, check = FALSE)
    if (((length(x) == 2) & (x[2] < 1)) |
        ((length(y) == 2) & (y[2] < 1))) {
      res = NULL
    } else {
      temp_gcd = integer_gcd(x, y, radix, check = FALSE)
      res = ((length(temp_gcd) == 2) & (temp_gcd[2] < 2))
    }
  }
  return(res)
}

integer_lcm = function(x, y, radix, check = TRUE) {
  # input: integer vector, x, y
  # output: least common multiple of x, y
  #         where either of x and y is not 0
  if (check) {
    if ((integer_check(x, radix)) & (integer_check(y, radix))) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    y = integer_absolute(y, radix, check = FALSE)
    if ((length(x) == 2) & (x[2] < 1)) {
      res = NULL
    } else if ((length(y) == 2) & (y[2] < 1)) {
      res = NULL
    } else {
      temp_gcd = integer_gcd(x, y, radix, check = FALSE)
      res = integer_multiply(x, y, radix, check = FALSE)
      res = integer_modulo(res, temp_gcd, radix, check = FALSE)
      res = res[["quo"]]
    }
  }
  return(res)
}

integer_Carmichael_lambda = function(x, radix, check = TRUE) {
  # input: integer vector, x
  # output: Carmichael's lambda of x
  if (check) {
    if (integer_check(x, radix)) {
      res = TRUE
    } else {
      res = NULL
    }
  } else {
    res = TRUE
  }
  if (!is.null(res)) {
    x = integer_absolute(x, radix, check = FALSE)
    if ((length(x) == 2) & (x[2] < 2)) {
      res = NULL
    } else {
      fact_list = integer_factorization(x, radix, check = FALSE)
      temp_len = length(fact_list[["prime"]])
      temp_2_vec = integer_num_2_vec(2, radix, check = FALSE)
      if (integer_eq_vec_vec(fact_list[["prime"]][[1]], 
                             temp_2_vec, radix, check = FALSE)) {
        if (integer_more_vec_vec(fact_list[["power"]][[1]], 
                                 temp_2_vec, radix, check = FALSE)) {
          res = integer_power(temp_2_vec, 
                              integer_minus(fact_list[["power"]][[1]], temp_2_vec, radix, check = FALSE), 
                              radix, check = FALSE)
        } else {
          res = integer_power(temp_2_vec, 
                              minus_one(fact_list[["power"]][[1]], radix, check = FALSE), 
                              radix, check = FALSE)
        }
      } else {
        res = integer_power(fact_list[["prime"]][[1]], 
                            minus_one(fact_list[["power"]][[1]], radix, check = FALSE), 
                            radix, check = FALSE)
        res = integer_multiply(res, 
                               minus_one(fact_list[["prime"]][[1]], radix, check = FALSE), 
                               radix, check = FALSE)
      }
      if (temp_len > 1) {
        for (n in 2:temp_len) {
          temp_num = integer_power(fact_list[["prime"]][[n]], 
                                   minus_one(fact_list[["power"]][[n]], radix, check = FALSE), 
                                   radix, check = FALSE)
          temp_num = integer_multiply(temp_num, 
                                      minus_one(fact_list[["prime"]][[n]], radix, check = FALSE), 
                                      radix, check = FALSE)
          res = integer_lcm(res, temp_num, radix, check = FALSE)
        }
      }
    }
  }
  return(res)
}

factorization_next = function(fac_list, radix) {
  for (n in 1:fac_list[["num"]]){
    if (fac_list[["bool"]][n]){
      fac_list[["count"]][[n]] = minus_one(fac_list[["prime"]][[n]], radix, 
                                           check = FALSE)
      fac_list[["bool"]][n] = FALSE
    } else {
      fac_list[["count"]][[n]] = minus_one(fac_list[["count"]][[n]], radix, 
                                           check = FALSE)
      if (fac_list[["count"]][[n]][1] == 0) {
        fac_list[["bool"]][n] = TRUE
      }
    }
  }
  return(fac_list)
}



# modular sequence converter

integer_am_seq_complete_root_condition = function(c, a, m, radix) {
  # condition checker of arithmetic-modulo sequence
  # input: integer vector, c, a, m
  # output: dictionary of encode, decode, for each including c, a, m
  #         where in encode, convert = c (origin + a) mod m
  #         where in decode, origin = c (convert + a) mod m    
  res = NULL
  if ((integer_check(c, radix)) & (integer_check(a, radix)) & 
      (integer_check(m, radix))) {
    m = integer_absolute(m, radix, check = FALSE)
    if (integer_more_vec_vec(m, c(1, 1), radix, check = FALSE)) {
      c = integer_modulo(c, m, radix, check = FALSE)
      c = c[["rem"]]
      if (integer_more_vec_vec(c, c(0, 0), radix, check = FALSE)) {
        if (integer_is_relatively_primes(c, m, radix, check = FALSE)) {
          inv_c = integer_mod_inverse(c, m, radix, check = FALSE)
          a = integer_modulo(a, m, radix, check = FALSE)
          a = a[["rem"]]
          inv_a = integer_multiply(c, a, radix, check = FALSE)
          inv_a = integer_inverse(inv_a, radix, check = FALSE)
          inv_a = integer_modulo(inv_a, m, radix, check = FALSE)
          inv_a = inv_a[["rem"]]
          encode_list = list("type" = "am_seq", 
                             "c" = c, 
                             "a" = a, 
                             "m" = m)
          decode_list = list("type" = "am_seq", 
                             "c" = inv_c, 
                             "a" = inv_a, 
                             "m" = m)
          res = list("encode" = encode_list, 
                     "decode" = decode_list)
        }
      }
    }
  }
  return(res)
}

integer_am_seq_convert = function(x, am_seq_list, radix) {
  # converter of arithmetic-modulo sequence
  # input: integer vector, x
  #        condition tuple, am_seq_dict
  #                         where from encode or decode in integer_am_seq_complete_root_condition    
  # output: value of AM sequence with index x
  res = FALSE
  if ((integer_check(x, radix)) & (is.list(am_seq_list))) {
    if (!(is.null(am_seq_list[["type"]]))) {
      if (am_seq_list[["type"]] == "am_seq") {
        res = TRUE
      }
    }
  }
  if (res) {
    res = integer_plus(x, am_seq_list[["a"]], radix, check = FALSE)
    res = integer_multiply(res, am_seq_list[["c"]], radix, check = FALSE)
    res = integer_modulo(res, am_seq_list[["m"]], radix, check = FALSE)
    res = res[["rem"]]
  } else {
    res = NULL
  }
  return(res)
}

integer_t1gm_seq_primitive_root_condition = function(c, a, p, p_power, radix, double = FALSE) {
  # condition checker of type-1 geometric-modulo sequence
  # input: integer vector, c, a, p, p_power
  #                              where if double == True, m = 2 p^(p_power), 
  #                                    if double == False, m = p^(p_power) 
  # output: dictionary including c, a, m 
  #                              where a is the primitive root of the GM sequence modulo m
  res = TRUE
  if (is.logical(double)) {
    if ((integer_check(c, radix)) & (integer_check(a, radix)) & 
        (integer_check(p, radix)) & (integer_check(p_power, radix))) {
      if ((integer_more_vec_vec(p_power, c(0, 0), radix, check = FALSE)) & 
          (integer_moreeq_vec_vec(c, c(0, 0), radix, check = FALSE)) & 
          (integer_moreeq_vec_vec(a, c(0, 0), radix, check = FALSE))) {
        p = integer_absolute(p, radix, check = FALSE)
        temp_2_vec = integer_num_2_vec(2, radix, check = FALSE)
        if (integer_eq_vec_vec(p, temp_2_vec, radix, check = FALSE)) {
          if (double) {
            if (integer_eq_vec_vec(p_power, c(1, 1), radix, check = FALSE)) {
              m = integer_num_2_vec(4, radix, check = FALSE)
              factor_vec = list(temp_2_vec)
              factor_power_vec = list(temp_2_vec)
              phi_m = temp_2_vec
            } else {
              res = FALSE
            }
          } else {
            if (integer_eq_vec_vec(p_power, c(1, 1), radix, check = FALSE)) {
              m = temp_2_vec
              factor_vec = list(temp_2_vec)
              factor_power_vec = list(c(1, 1))
              phi_m = c(1, 1)
            } else if (integer_eq_vec_vec(p_power, temp_2_vec, radix, check = FALSE)) {
              m = integer_num_2_vec(4, radix, check = FALSE)
              factor_vec = list(temp_2_vec)
              factor_power_vec = list(temp_2_vec)
              phi_m = temp_2_vec
            } else {
              res = FALSE
            }
          }
          if (res) {
            c = integer_modulo(c, m, radix, check = FALSE)
            c = c[["rem"]]
            temp_c = integer_vec_2_num(c, radix, check = FALSE)
            if ((temp_c != 1) & (temp_c != 3)) {
              res = FALSE
            }
          }
        } else if (integer_more_vec_vec(p, temp_2_vec, radix, check = FALSE)) {
          if (integer_is_prime(p, radix, check = FALSE)){
            if ((integer_is_relatively_primes(c, p, radix, check = FALSE)) & 
                (integer_is_relatively_primes(a, p, radix, check = FALSE))) {
              if (double) {
                if ((integer_is_even(c, radix, check = FALSE)) | 
                    (integer_is_even(a, radix, check = FALSE))) {
                  res = FALSE
                } else{
                  phi_m = integer_power(p, 
                                        minus_one(p_power, radix, check = FALSE), 
                                        radix, check = FALSE)
                  m = integer_multiply(phi_m, p, radix, check = FALSE)
                  phi_m = integer_multiply(phi_m, 
                                           minus_one(p, radix, check = FALSE), 
                                           radix, check = FALSE)
                  m = integer_multiply(m, temp_2_vec, radix, check = FALSE)
                  factor_vec = list(temp_2_vec, p)
                  factor_power_vec = list(c(1, 1), p_power)
                  c = integer_modulo(c, m, radix, check = FALSE)
                  c = c[["rem"]]
                  a = integer_modulo(a, m, radix, check = FALSE)
                  a = a[["rem"]]
                } 
              } else {
                phi_m = integer_power(p, 
                                      minus_one(p_power, radix, check = FALSE), 
                                      radix, check = FALSE)
                m = integer_multiply(phi_m, p, radix, check = FALSE)
                phi_m = integer_multiply(phi_m, 
                                         minus_one(p, radix, check = FALSE), 
                                         radix, check = FALSE)
                factor_vec = list(p)
                factor_power_vec = list(p_power)
                c = integer_modulo(c, m, radix, check = FALSE)
                c = c[["rem"]]
                a = integer_modulo(a, m, radix, check = FALSE)
                a = a[["rem"]]
              }
            } else {
              res = FALSE
            }
          } else {
            res = FALSE
          }
        } else {
          res = FALSE
        }
      } else {
        res = FALSE
      }
    } else {
      res = FALSE
    }
  } else {
    res = FALSE
  }
  if (res) {
    if (integer_more_vec_vec(phi_m, c(1, 1), radix, check = FALSE)) {
      half_phi_m = floor_half_value(phi_m, radix, check = FALSE)
      half_phi_m = half_phi_m[["half"]]
      m_minus_one = minus_one(m, radix, check = FALSE)
      temp_bool_1 = TRUE
      temp_vec_1 = c(0, 0)
      temp_vec_2 = c(1, 1)
      while (temp_bool_1) {
        temp_vec_1 = plus_one(temp_vec_1, radix, check = FALSE)
        temp_vec_2 = multiply_modulo(temp_vec_2, a, m, 
                                     radix, check = FALSE)
        if ((length(temp_vec_2) == 2) & (temp_vec_2[2] < 2)) {
          res = FALSE
          temp_bool_1 = FALSE
        } else if (integer_less_vec_vec(temp_vec_1, half_phi_m, radix, check = FALSE)){
          if (integer_eq_vec_vec(temp_vec_2, m_minus_one, radix, check = FALSE)) {
            res = FALSE
            temp_bool_1 = FALSE
          }
        } else {
          temp_bool_1 = FALSE
        }
      }
    } 
  }
  if (res) {
    phi_phi_m = integer_Euler_phi(phi_m, radix, check = FALSE)
    res = list("type" = "t1gm_seq_primitive_root", 
               "c" = c, 
               "a" = a, 
               "m" = m, 
               "m_prime" = factor_vec, 
               "m_power" = factor_power_vec, 
               "phi_m" = phi_m, 
               "phi_phi_m" = phi_phi_m)
  } else {
    res = NULL
  }
  return(res)
}


integer_t1gm_seq_convert = function(x, t1pm_seq_pr_list, radix) {
  # converter of type-1 geometric-modulo sequence
  # input: integer vector, x
  #        condition list, t1pm_seq_pr_dict
  #                        which is from integer_t1gm_seq_primitive_root_condition
  # output: value of T1GM sequence with index x
  res = FALSE
  if ((integer_check(x, radix)) & (is.list(t1pm_seq_pr_list))) {
    if (!(is.null(t1pm_seq_pr_list[["type"]]))) {
      if (t1pm_seq_pr_list[["type"]] == "t1gm_seq_primitive_root") {
        res = TRUE
      }
    }
  }
  if (res) {
    x = integer_modulo(x, t1pm_seq_pr_list[["phi_m"]], radix, check = FALSE)
    x = x[["rem"]]
    if (integer_more_vec_vec(x, c(0, 0), radix, check = FALSE)) {
      res = power_modulo(t1pm_seq_pr_list[["a"]], x, 
                         t1pm_seq_pr_list[["m"]], radix, check = FALSE)
      res = multiply_modulo(res, t1pm_seq_pr_list[["c"]], 
                            t1pm_seq_pr_list[["m"]], radix, check = FALSE)
    } else {
      res = t1pm_seq_pr_list[["c"]]
    }
  } else {
    res = NULL
  }
  return(res)
}

integer_t1gm_seq_index = function(x, t1pm_seq_pr_list, radix) {
  # index of type-1 geometric-modulo sequence
  # input: integer vector, x, which should be mutually prime with m
  #        condition list, t1pm_seq_pr_list
  #                        which is from integer_t1gm_seq_primitive_root_condition
  # output: index of T1GM sequence with value x
  res = FALSE
  if ((integer_check(x, radix)) & (is.list(t1pm_seq_pr_list))) {
    if (integer_noteq_vec_vec(x, c(0, 0), radix, check = FALSE)) {
      if (!(is.null(t1pm_seq_pr_list[["type"]]))) {
        if (t1pm_seq_pr_list[["type"]] == "t1gm_seq_primitive_root") {
          if (!(is.null(t1pm_seq_pr_list[["m"]]))) {
            if (integer_more_vec_vec(t1pm_seq_pr_list[["m"]], c(1, 1), radix)) {
              if (integer_is_relatively_primes(x, t1pm_seq_pr_list[["m"]], 
                                               radix, check = FALSE)) {
                res = TRUE
              }
            }
          }
        }
      }
    }
  }
  if (res) {
    temp_bool = TRUE
    temp_vec_1 = c(0, 0)
    temp_vec_2 = t1pm_seq_pr_list[["c"]]
    while(temp_bool) {
      temp_vec_1 = plus_one(temp_vec_1, radix, check = FALSE)
      temp_vec_2 = multiply_modulo(temp_vec_2, t1pm_seq_pr_list[["a"]], 
                                   t1pm_seq_pr_list[["m"]], 
                                   radix, check = FALSE)
      if (integer_eq_vec_vec(temp_vec_2, x, radix, check = FALSE)) {
        res = temp_vec_1
        temp_bool = FALSE
      }
    }
  } else {
    res = NULL
  }
  return(res)
}

integer_pm_seq_complete_root_condition = function(c, b, p_list, radix, inverse = TRUE) {
  # condition checker of power-modulo sequence
  # input: integer vector, c, b, 
  #        list of integer vector, p_list
  # output: dictionary including c, b, m 
  res = FALSE
  if (is.logical(inverse)) {
    if (length(inverse) == 1) {
      if ((integer_check(c, radix)) & (integer_check(b, radix)) & 
          (is.list(p_list))) {
        p_len = length(p_list)
        if (p_len >= 1) {
          temp_vec_1 = numeric(0)
          temp_num_1 = 0
          temp_bool_1 = TRUE
          res = TRUE
          while(temp_bool_1) {
            temp_num_1 = temp_num_1+1
            if (integer_check(p_list[[temp_num_1]], radix)) {
              p_list[[temp_num_1]] = integer_absolute(p_list[[temp_num_1]], radix,
                                                      check = FALSE)
              if (integer_more_vec_vec(p_list[[temp_num_1]], 
                                       c(1, 1), radix, check = FALSE)) {
                if (integer_is_prime(p_list[[temp_num_1]], radix, check = FALSE)) {
                  temp_num_2 = temp_num_1
                  if (temp_num_1 > 1) {
                    for (n in 1:(temp_num_1-1)) {
                      if (integer_less_vec_vec(p_list[[temp_num_1]], 
                                               p_list[[temp_vec_1[n]]], 
                                               radix, check = FALSE)) {
                        temp_num_2 = n
                        break
                      } else if (integer_eq_vec_vec(p_list[[temp_num_1]], 
                                                    p_list[[temp_vec_1[n]]], 
                                                    radix, check = FALSE)) {
                        res = FALSE
                        break
                      }
                    }
                  } 
                  if (res) {
                    temp_vec_1 = append(temp_vec_1, temp_num_1, temp_num_2-1)
                  }
                } else {
                  res = FALSE
                }
              } else {
                res = FALSE
              }
            } else {
              res = FALSE
            }
            if (res) {
              if (temp_num_1 >= p_len) {
                temp_bool_1 = FALSE
              }
            } else {
              temp_bool_1 = FALSE
            }
          }
        }
      }
    }
  }
  if (res) {
    m = c(1, 1)
    phi_m = c(1, 1)
    lambda_m = c(1, 1)
    m_factor = list()
    for (n in 1:p_len) {
      m_factor[[n]] = p_list[[temp_vec_1[n]]]
      m = integer_multiply(m, p_list[[temp_vec_1[n]]], 
                           radix, check = FALSE)
      temp_num_2 =  minus_one(p_list[[temp_vec_1[n]]], 
                              radix, check = FALSE)
      phi_m = integer_multiply(phi_m, temp_num_2, 
                               radix, check = FALSE)
      lambda_m = integer_lcm(lambda_m, temp_num_2, 
                             radix, check = FALSE)
    }
    c = integer_modulo(c, m, radix, check = FALSE)
    c = c[["rem"]]
    b = integer_modulo(b, lambda_m, radix, check = FALSE)
    b = b[["rem"]]
    res = FALSE
    if (integer_more_vec_vec(c, c(0, 0), radix, check = FALSE)) {
      if (integer_more_vec_vec(b, c(0, 0), radix, check = FALSE)) {
        if (integer_is_relatively_primes(c, m, radix, check = FALSE)) {
          if (integer_is_relatively_primes(b, lambda_m, radix, check = FALSE)) {
            res = TRUE
            if (inverse) {
              inv_c = integer_mod_inverse(c, m, radix, check = FALSE)
              inv_b = integer_mod_inverse(b, lambda_m, radix, check = FALSE)
            }
          } 
        } 
      }
    }
  } 
  if (res) {
    phi_phi_m = integer_Euler_phi(phi_m, radix, check = FALSE)
    if (inverse) {
      res = list("type" = "pm_seq_complete_root", 
                 "c" = c, 
                 "b" = b, 
                 "m" = m, 
                 "m_prime" = m_factor, 
                 "phi_m" = phi_m, 
                 "phi_phi_m" = phi_phi_m, 
                 "lambda_m" = lambda_m, 
                 "mod_inverse_c" = inv_c, 
                 "mod_inverse_b" = inv_b)
    } else {
      res = list("type" = "pm_seq_complete_root", 
                 "c" = c, 
                 "b" = b, 
                 "m" = m, 
                 "m_prime" = m_factor, 
                 "phi_m" = phi_m, 
                 "phi_phi_m" = phi_phi_m, 
                 "lambda_m" = lambda_m)
    }
  } else {
    res = NULL
  }
  return(res)
}

integer_pm_seq_convert = function(x, pm_seq_cr_list, radix) {
  # converter of power-modulo sequence
  # input: integer vector, x
  #        condition list, pm_seq_cr_list
  #                        which is from integer_pm_seq_complete_root_condition
  # output: value of PM sequence with index x
  res = FALSE
  if ((integer_check(x, radix)) & (is.list(pm_seq_cr_list))) {
    if (!(is.null(pm_seq_cr_list[["type"]]))) {
      if (pm_seq_cr_list[["type"]] == "pm_seq_complete_root") {
        res = TRUE
      }
    }
  }
  if (res) {
    res = power_modulo(x, pm_seq_cr_list[["b"]], 
                       pm_seq_cr_list[["m"]], radix, check = FALSE)
    res = multiply_modulo(res, pm_seq_cr_list[["c"]], 
                          pm_seq_cr_list[["m"]], radix, check = FALSE)
  } else {
    res = NULL
  }
  return(res)
}

integer_pm_seq_index = function(x, pm_seq_cr_list, radix) {
  # index of power-modulo sequence
  # input: integer vector, x
  #        condition list, pm_seq_cr_list
  #                        which is from integer_pm_seq_complete_root_condition
  #                        modular inverse of b and c must be existing
  # output: index of PM sequence with value x
  res = FALSE
  if ((integer_check(x, radix)) & (is.list(pm_seq_cr_list))) {
    if ((!(is.null(pm_seq_cr_list[["type"]]))) & 
        (!(is.null(pm_seq_cr_list[["mod_inverse_c"]]))) & 
        (!(is.null(pm_seq_cr_list[["mod_inverse_b"]])))) {
      if (pm_seq_cr_list[["type"]] == "pm_seq_complete_root") {
        res = TRUE
      }
    }
  }
  if (res) {
    res = multiply_modulo(x, pm_seq_cr_list[["mod_inverse_c"]], 
                          pm_seq_cr_list[["m"]], radix, check = FALSE)
    res = power_modulo(res, pm_seq_cr_list[["mod_inverse_b"]], 
                       pm_seq_cr_list[["m"]], radix, check = FALSE)
  } else {
    res = NULL
  }
  return(res)
}


