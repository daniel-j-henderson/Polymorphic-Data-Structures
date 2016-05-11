# Polymorphic Data Structures
Polymorphic data structures implemented in Fortran

In the mylist directory, there's a linked list structure. The list and node elements can be left as-is. 
Each node contains a 'key', which has an integer id. Type extensions of the key can add other data. 
My 'key' extensions are int_key (adds member 'value' of type integer), and string_key (adds member
'value' of type character(100)). You can extend the key to add whatever type of value you want. 

Builds with: gfortran 4.7.1+, pgfortran 12.5+, ifort 13.1.2+

make [gnu, intel, pgi] 
