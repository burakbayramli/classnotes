import RPi.GPIO as GPIO
import time
GPIO.cleanup() 
GPIO.setmode(GPIO.BCM)
led_pin = 17
GPIO.setup(led_pin, GPIO.OUT)
try:
        while True:
                GPIO.output(led_pin, True)
                time.sleep(0.5)
                GPIO.output(led_pin, False)
                time.sleep(0.5)
finally:
        print("Cleaning up")
        GPIO.cleanup()
