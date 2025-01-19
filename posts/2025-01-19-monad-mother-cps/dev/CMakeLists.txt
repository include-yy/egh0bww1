cmake_minimum_required(VERSION 3.30)

project(math)

function (Add a b result)
  math(EXPR res "${a} + ${b}")
  set(${result} ${res} PARENT_SCOPE)
endfunction()

function (Sub a b result)
  math(EXPR res "${a} - ${b}")
  set(${result} ${res} PARENT_SCOPE)
endfunction()

function (Mul a b result)
  math(EXPR res "${a} * ${b}")
  set(${result} ${res} PARENT_SCOPE)
endfunction()

function (Div a b result)
  math(EXPR res "${a} / ${b}")
  set(${result} ${res} PARENT_SCOPE)
endfunction()

set(res1 0)
set(res2 0)

Add(1 2 res1)
Mul(${res1} 3 res1)
Div(6 3 res2)
Sub(${res2} 1 res2)
Add(${res1} ${res2} res1)

message("The result is : ${res1}")

# -- Selecting Windows SDK version 10.0.22621.0 to target Windows 10.0.26100.
# The result is : 10
# -- Configuring done (0.0s)
# -- Generating done (0.0s)
# -- Build files have been written to: ...
