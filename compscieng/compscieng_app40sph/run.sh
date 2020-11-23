g++ simsph2.cpp -std=c++1z  -g -O2 -o /tmp/a.exe; /tmp/a.exe
python -u simsph2.py
convert -scale 70% -delay 30 /tmp/simsph/*.png ~/Downloads/simsph.gif
eog ~/Downloads/simsph.gif

