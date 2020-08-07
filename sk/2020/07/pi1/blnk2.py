import time
import RPi.GPIO as GPIO

GPIO.setmode(GPIO.BOARD)
pin = 18
GPIO.setup(pin, GPIO.OUT)

try:
        while True:
            GPIO.output(pin, GPIO.HIGH)
            time.sleep(1.0)
            GPIO.output(pin, GPIO.LOW)
            time.sleep(1.0)
finally:
        print("Cleaning up")
        GPIO.cleanup()

