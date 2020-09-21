import csv
import sys

orig_stdout = sys.stdout
f = open('paragem.pl', 'w')
sys.stdout = f

with open('paragens.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=';')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            line_count += 1
        else:
            if (row[9].find('\'') != -1): 
                row[9].replace('\'', ' ')
            print(f'paragem({row[0]}, {row[1]}, {row[2]}, \'{row[3]}\', \'{row[4]}\', \'{row[5]}\', \'{row[6]}\', [{row[7]}], {row[8]}, \'{row[9]}\', [\'{row[10]}\']).')
            line_count += 1


sys.stdout = orig_stdout
f.close()
