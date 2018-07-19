import os 
import os.path
dir = "H:/My Documents/"
flist = os.listdir(dir)
for f in dir:
	print(f," DIR:", os.path.isdir(dir+f), " FILE: ", os.path.isfile(dir+f))