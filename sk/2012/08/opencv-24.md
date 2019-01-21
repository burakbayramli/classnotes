# OpenCV 2.4


OpenCV 2.4




Ubuntu 12.04 LTS icin en son OpenCV kurulusu. 

http://www.samontab.com/web/2012/06/installing-opencv-2-4-1-ubuntu-12-04-lts/

Basit "resim cekmek" icin (t tusuna basilinca ekran goruntusu diske jpg olarak yazilir),
import cvclass Capture:        def save(self, pic):        cv.SaveImage('deneme' + str(self.i) + '.jpg', pic)        def __init__(self):        self.i = 1        self.capture = cv.CaptureFromCAM(0)        cv.NamedWindow( "CamShiftDemo", 1 )        print( "Keys:\n"            "    ESC - quit the program\n"            "    t - take picture\n"            "To initialize tracking, drag across the object with the mouse\n" )    def run(self):        while True:            frame = cv.QueryFrame( self.capture )            cv.ShowImage( "CamShiftDemo", frame )            c = cv.WaitKey(7)            if c == 27:                break            elif c == ord("t"):                frame = cv.QueryFrame( self.capture )                self.save(frame)                self.i += 1if __name__=="__main__":    demo = Capture()    demo.run()

Video kaydetmek icin

import cvimport cv2class Capture:        def __init__(self):        self.capture = cv.CaptureFromCAM(0)        cv.NamedWindow( "CamShiftDemo", 1 )        print( "Keys:\n"            "    ESC - quit the program\n"            "    t - take picture\n"            "To initialize tracking, drag across the object with the mouse\n" )    def run(self):        fps =  30        frame = cv.QueryFrame( self.capture )        frame_size = cv.GetSize (frame)        writer=cv.CreateVideoWriter('movie.avi',cv2.cv.CV_FOURCC('F', 'M', 'P', '4'),fps,frame_size)        while True:            frame = cv.QueryFrame( self.capture )            cv.ShowImage( "CamShiftDemo", frame )            c = cv.WaitKey(7)            cv.WriteFrame(writer,frame)            if c == 27:                break                    cv.ReleaseVideoWriter (writer)  if __name__=="__main__":    demo = Capture()    demo.run()




