#include <cmath>

double my_cos(double d){
    return std::cos(d);
}

// TODO : add args
extern "C" int64_t func_for_function(int64_t (*a)()){
    return a();
}