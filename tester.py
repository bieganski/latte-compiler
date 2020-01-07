#!/usr/bin/env python3

from os import system, listdir
from sys import argv
import subprocess


yellow = "\u001b[33m"
green = "\u001b[32m"
red = "\u001b[31m"
blue = "\u001b[34m"
cyan = "\u001b[36m"
reset = "\u001b[0m"
magenta = "\u001b[35m"


if len(argv) < 3:
    print("Arguments: <dir with tests> ./latc_x86")
    exit(1)
else:
    cmd = argv[2].split(" ")

dirr = argv[1]
while dirr.endswith("/"):
    dirr = dirr[:-1]

if system("make") != 0:
    exit()

def runFile(f):
    print(magenta + "FILE " + yellow + f + cyan)

    system("cat " + f)
    print("\n" + magenta + "FILE END")
    res = subprocess.run(cmd + [f], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if res.returncode == 0:
        res_file_name =f[:-3] + "res"
        res_file = open(res_file_name, "w")
        output_file = f[:-3] + "output"
        res1 = subprocess.run(f[:-4], stdout=res_file, stderr=subprocess.PIPE)
        print(green)
        system("cat " + res_file_name)
        print(reset)
        print("\n------ CORRECT ------")
        system("cat " + output_file)
        print("\n------ DIFF ------")
        system("diff " + res_file_name + " " + output_file)
    else:
        print(red)
        print(res.stderr.decode("utf-8"))
        print(res.stdout.decode("utf-8"))
    
    print(reset)
    a = input("Press Enter to continue, q to quit...\n")
    if 'q' in a:
        return True
    print("")
    return False

print(reset)

for f in listdir(dirr):
    if f.endswith(".lat") and runFile(dirr + "/" + f):
        break

print(reset)
