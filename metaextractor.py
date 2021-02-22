##Script to parse JSON data from  html files in a directory. Uses extruct package.
##Overwrites files, so be careful

import os
import sys
import extruct
from pprint import pprint
import string
import json
from json import loads

def extractJson(path: string ):

    targets = []
    contents = os.listdir(path)
    for i in contents:
        if i.endswith('.html'):
            targets.append(i) 
            
    targets.sort()

    for i in targets:
        f = open(path + str(i), "r")
        fr = f.read()
        data = extruct.extract(fr, uniform = True)
        fw = open(path + os.path.splitext(str(i))[0] + ".json","w")
        json.dump(data, fw)
        fw.close()
        f.close()
        print(str(i) + " file ok")


def main():
    
    #path = "/Users/patez/OneDrive - Oulun yliopisto/gradu/csv/html/"
    path = str(sys.argv[1])

    extractJson(path) #path to files
    
main()
