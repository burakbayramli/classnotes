# Simple version to start servers, it pops up xterms, good for debugging
echo $1
if [ $1 == "1" ]; then
    xterm -fn 10x20 -hold -e "cd $HOME/Documents/sasha/sasha; python sasha.py local localhost 5000" &
fi
if [ $1 == "2" ]; then
    xterm -fn 10x20  -hold -e "cd $HOME/Documents/sasha/sasha; python sasha.py local2 localhost 5000" &
    xterm -fn 10x20  -hold -e "cd $HOME/Documents/sasha/sasha; python sasha.py local2 localhost 5001" &
fi
