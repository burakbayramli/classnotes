import RPi.GPIO as GPIO, time
GPIO.setmode(GPIO.BOARD)
GPIO.setup(12, GPIO.OUT)

p = GPIO.PWM(12, 0.5)
time.sleep(10)
GPIO.cleanup()


