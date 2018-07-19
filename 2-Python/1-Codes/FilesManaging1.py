import os 
import os.path

def listDir(path):
	listdirfile = os.listdir(path)
	i = 1
	for fd in listdirfile:
		if os.path.isfile(os.path.join(path,fd)):
			i += 1
			print(i*'\t','F: ',df)
		elif os.path.isdir(os.path.join(path,fd)):
			listDir(os.path.join(path,fd)
	return None

a = listDir('H:\My Documents\MyFiles')