#include <iostream>
#include <string>
#include "cat.h"

using std::cout;using std::endl;using std::string;
int main()
{
    string name = "Ali";
    cout<< "Kedim burada, " << name << "!" <<endl;
    Cat kitty(name);
    kitty.speak();
    return 0;
}


