#!/usr/bin/python3
import re
import sys

for path in sys.argv[1:]:
    with open(path) as file:
        s = file.read()
        s = re.sub(r'\bNULL\b', '0', s)
        s = re.sub(r'\bva_list ([^;]*)', 'va_list *\\1', s)
        s = re.sub(r'\bva_start\(([^)]*),([^)]*)\)', '\\1 = __va_area__', s)
        s = re.sub(r'\bstdin\b', '__stdinp', s)
        s = re.sub(r'\bstdout\b', '__stdoutp', s)
        s = re.sub(r'\bstderr\b', '__stderrp', s)
        print(s)
