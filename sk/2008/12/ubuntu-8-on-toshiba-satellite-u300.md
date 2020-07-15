# Ubuntu 8 on Toshiba Satellite u300

This post is in English hoping it can help out a wider audience on
Linux compatibility issues. Also see Linux on Laptops page.I have
purchased a sweet little Toshiba Satellite u300 laptop and am running
Ubuntu 8 on it; Sound, video worked out of the box.Wireless caused
problems, to get around it, you need to download and install
compat-wireless package found here. Before installing this package,
the network icon on the upper right corner will not detect any
wireless networks, and don't fiddle with anything in the system to get
wireless "working" - it won't. Until you install compat-wireless. Get
the sources, then "make" "sudo make install" and "sudo make
load". After this last step, driver will be loaded, and network icon
in upper right corner will change, it will detect and connect to a
wireless network. From now on, if you want to load the drivers, you
need to run "sudo make load" (I am still looking for a way to make
this part of startup without changing startup scripts).There is a
problem with the volume control on the left side of the machine. When
you roll the wheel up or down, it gets stuck on changing the volume
action and will not yield control to your keyboard. At this point you
are screwed, the only thing you can do is rebooting the machine.My
solution is to disable the wheel volume up or down event. Go to System
| Preferences | Keyboard Shortcuts and assign "any" key combination to
Sound | Volume up and down events. Anything except the X86blabla
event. Then you will be set. Make sure you have turned volume all the
way up before you do this.By the way; To shut off the annoying drum,
tamtam sound when you login, go to System | Administration | Login
Window and to Accesibility tab. There uncheck the sound option for
question.wav.Out of the box, you won't have external display
capability. In order to enable TVOUT so you can watch movies,
presentation on an external display, you need to enable TwinView.From
cmd prompt, sudo vi /etc/X11/xorg.conf then add the following two
lines to your "Section" block in your xorg.conf.Section "Screen" ...
Option "TwinView" Option "TVOutFormat" "SVIDEO" SubSection "Display"
Virtual 1280 800 EndSubSectionEndSectionYou need to reboot for
external display to take effect everytime you use your external
display. When you reconnect, same way, reboot so you get back to your
normal display. I am sure there are easier ways to do this, this was
easiest way for me with least amount of trial and error.I will update
this post with any other u300 + Linux related problems and solutions.




