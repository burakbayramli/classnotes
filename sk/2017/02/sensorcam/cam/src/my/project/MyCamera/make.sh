export CP=:../../../:/home/burak/Downloads/android-sdk-linux/platforms/android-17/android.jar
javac -classpath $CP Test.java 
java -classpath $CP my.project.MyCamera.Test
