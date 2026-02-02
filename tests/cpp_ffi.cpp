#include <cmath>
#include <cstdint>

// TODO : add __declspec(dllexport) for windows ?
double my_cos(double d){
    return std::cos(d);
}

// TODO : add args
extern "C" int64_t func_for_function(int64_t (*a)()){
    return a();
}

extern "C" int64_t func_for_function_args(int64_t (*a)(int64_t, int), int64_t arg1, int arg2){
    return a(arg1, arg2);
}