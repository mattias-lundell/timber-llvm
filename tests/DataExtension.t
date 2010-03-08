module DataExtension where

data A = A
data B = B
data C > A,B

f A = True
f B = False
