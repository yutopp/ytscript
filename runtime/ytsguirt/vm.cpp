#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include <ytsrtcore/engine.hpp>


int main()
{
    ytscript::engine<ytscript::graphics_runtime> engine( "out.ytx" );

    engine.main_loop();
}
