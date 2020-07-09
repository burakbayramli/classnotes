# Simbad, Sensor, Python


Simbad, Sensor, Python



Simbad kodlari icinde az sayida Python ornegi var; sensor ekleyen bir ornek yoktu. Java kodlarini Jython uzerinden Python'a aktarirken biraz takla atmak gerekiyor, mesela bir class'in static metotlarini tum class'i import edip fonksiyonel cagri yaparak isletiyorsunuz.Alttaki kod bir Robot'a 12 tane sonar sensoru ekleyerek sifirinci sensor'un degerini belli araliklarla ekrana basmanin ornegini gosteriyor.from simbad.gui import *from simbad.sim import *from simbad.sim.RobotFactory import addSonarBeltSensorfrom javax.vecmath import *# a very simple robot controllerclass MyRobot(Agent): def __init__(self, vector, name):  Agent.__init__(self, vector, name)  self.sonars = addSonarBeltSensor(self, 12)   def initBehavior(self):  # nothing to do  pass  def performBehavior(self):  if self.collisionDetected():   self.setTranslationalVelocity(0)  else:   self.setTranslationalVelocity(0.2)  if self.getCounter() % 100 == 0:   print "Sonar num 0  = " + str(self.sonars.getMeasurement(0))   # description of the environmentclass MyEnv(EnvironmentDescription): def __init__(self):  # put a  robot  self.add(MyRobot(Vector3d(0,0,0),"robot with python"))  # put a box  self.add(Box(Vector3d(3, 0, 0), Vector3f(1, 1, 1),self))# launch simbadsimbad = Simbad(MyEnv(),0)




