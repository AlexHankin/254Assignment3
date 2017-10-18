Xuan Thuong Le
Alex Hankin

CSC 254
Assignment 3: Translation

Files:

Assignment3.ml: Includes portion of code for creating an Abstraact Syntax tree. All functions take the parse tree as a parameter, match the node of given parse tree to a production, and if we reach a terminal - create an AST node. If we reach an operator, the function takes left subtree as an additional parameter to create an AST_bin node. There was a weird error, that we could still not figure out. At expr_tail and reln_tail passed in node wouldn't match to productions with operators (ao, mo, or ro), but would match to empty tail (e.g. FT -> []), in which case we would return left subtree without operations. Thus there are no operators to be seen in the C program, other than that, all the AST creation is done correctly. 

translator.ml: This was definitely an interesting portion to work on, neither of us had ever created a program that would translate a portion of code
into another language entirely. This code has been separated into its own file from Assignment3.ml since it was able to compile with any error messages. 
We wanted the code to be able to stand on its own, as we were not able to conduct a lot of testing on the program. Our main feature is the lack of any sort of user input
without going into the code itself to alter anything for translator.ml. If you want to perform a test using translator.ml replace the empty list on line 115 with ur AST you would
like to test with. Currently the program will return the proper C function, if you would like to print out the list of unused variables, change the 'snd' on line 116 to 'fst'.

In order to run both programs use either:
ocaml Assignment3.ml
ocaml translator.ml

