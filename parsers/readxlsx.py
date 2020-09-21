import pandas as pd
import sys
import xlrd

orig_stdout = sys.stdout
f = open('carreira.pl', 'w')
sys.stdout = f

array = []
workbook = xlrd.open_workbook('carreiras.xlsx')
for sheet in workbook.sheets():
    for column in range(sheet.nrows): 
        array.append(sheet.cell_value(column, 0))
    array.pop(0) 
    print (f'carreira({sheet.name},{str(array):s}).'.replace("'", ""))
    array = []

sys.stdout = orig_stdout
f.close()
