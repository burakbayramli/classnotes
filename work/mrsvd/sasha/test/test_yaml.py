import yaml

stram = open("/home/burak/.sasha.conf", "r")
conf =  yaml.load(stram)
print conf

print conf['sasha.dir']
