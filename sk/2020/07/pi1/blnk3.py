import RPi.GPIO as gpio  
gpio.setmode(gpio.BCM)  
gpio.setup(18, gpio.OUT)  
gpio.output(18, False)
